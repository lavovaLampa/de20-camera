library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use std.textio.all;

use work.sdram_model_pkg.all;
use work.sdram_pkg.all;

library osvvm;
context osvvm.OsvvmContext;

-- SDRAM must support CONCURRENT AUTO PRECHARGE
entity sdram_model is
    generic(
        -- model simulation settings
        LOAD_FROM_FILE  : boolean := false; -- whether to load memory content from file
        DUMP_TO_FILE    : boolean := false; -- whether to store memory content to a file
        INPUT_FILENAME  : string  := "input_fjel.txt"; -- name of a file to be loaded
        OUTPUT_FILENAME : string  := "output_fjel.txt" -- name of a file to be dumped
    );
    port(
        clkIn                                : in    std_logic;
        addrIn                               : in    Addr_T;
        dataIo                               : inout Data_T  := (others => 'Z');
        bankSelectIn                         : in    Bank_Addr_T;
        clkEnableIn                          : in    std_logic;
        chipSelectNegIn, rowAddrStrobeNegIn  : in    std_logic;
        colAddrStrobeNegIn, writeEnableNegIn : in    std_logic;
        dqmIn                                : in    std_logic_vector(1 downto 0);
        -- debug signals
        isInitializedOut                     : out   boolean := false;
        simEndedIn                           : in    boolean
    );
    constant SDRAM_ALERT_ID        : AlertLogIDType := GetAlertLogID("SDRAM", ALERTLOG_BASE_ID);
    constant MEMORY_MODEL_ALERT_ID : AlertLogIDType := GetAlertLogID("Memory Model", SDRAM_ALERT_ID);
end entity sdram_model;

architecture model of sdram_model is
    -- mode register (reg)
    signal modeReg : std_logic_vector(DATA_WIDTH - 1 downto 0) := encode_mode_reg(1, Sequential, 2, ProgrammedLength);

    -- decoded current command on input (wire) (non-latched)
    signal currCmd : Cmd_T := NoOp;

    -- decoded state from mode register (wire)
    signal burstLength    : Burst_Length_T     := 1;
    signal burstType      : Burst_Type_T       := Sequential;
    signal latencyMode    : Latency_Mode_T     := 2;
    signal writeBurstMode : Write_Burst_Mode_T := ProgrammedLength;

    -- internal signals (reg)
    signal clkInternal     : std_logic           := '0';
    signal dataOutPipeline : Data_Out_Pipeline_T := (others => (others => 'Z'));
    signal dataInPipeline  : Data_In_Pipeline_T  := (others => (data => (others => 'Z'), dqm => (others => '0')));

    -- input cmd/addr latch (register)
    signal inputReg : Input_Latch_R;
begin
    -- initialize memory model in shared variable
    memoryModel.MemInit(ADDR_WIDTH, DATA_WIDTH);
    memoryModel.SetAlertLogID(MEMORY_MODEL_ALERT_ID);

    -- mask data according to dqm
    -- if mask bit is low, allow data to be read from target byte
    dataIo <= mask_data(dataOutPipeline(0), dataInPipeline(tDQZ - 1).dqm);

    decodeBlock : block
        signal clkEnabled : std_logic := '0';
    begin
        -- clk enable is sampled on rising edge of clkIn
        clkProc : process(clkIn)
        begin
            if rising_edge(clkIn) then
                -- clock enable signal latched on rising edge
                clkEnabled <= clkEnableIn;
            end if;
        end process clkProc;

        latchProc : process(clkInternal)
        begin
            if rising_edge(clkInternal) then
                -- latch signals on rising edge of clk
                inputReg               <= (
                    addr => addrIn,
                    cmd  => currCmd,
                    bank => bankSelectIn
                );
                dataInPipeline(0)      <= (
                    data => dataIo,
                    dqm  => dqmIn
                );
                dataInPipeline(1 to 3) <= dataInPipeline(0 to 2);
            end if;
        end process latchProc;

        -- only enable clock if clkEnableIn is high
        with clkEnabled select clkInternal <=
            clkIn when '1',
            '0' when others;

        -- decode cmd signals to internal representation
        currCmd <= decode_cmd(chipSelectNegIn, rowAddrStrobeNegIn, colAddrStrobeNegIn, writeEnableNegIn);

        -- decode mode reg
        burstLength    <= decode_mode_reg(modeReg).burstLength;
        burstType      <= decode_mode_reg(modeReg).burstType;
        latencyMode    <= decode_mode_reg(modeReg).latencyMode;
        writeBurstMode <= decode_mode_reg(modeReg).writeBurstMode;
    end block decodeBlock;

    ctrlBlock : block
        -- TODO: is the upper counter limit OK?
        type Bank_State_Helper_R is record
            counting           : boolean;
            counter            : natural range 0 to (tRCCycles + 1);
            scheduledPrecharge : boolean;
        end record Bank_State_Helper_R;
        type Bank_Helpers_T is array (0 to BANK_COUNT - 1) of Bank_State_Helper_R;

        -- populate bank state counter on state transition
        pure function bank_schedule_transition(currState : Bank_State_T; nextState : Bank_State_T) return Bank_State_Helper_R is
        begin
            return (
                counting           => true,
                counter            => bank_transition_delay(currState, nextState),
                scheduledPrecharge => false
            );
        end function bank_schedule_transition;

        -- ctrl/banks state representation (reg)
        signal banks : Bank_Array_T := (others => (state => Idle, row => 0));
        signal ctrl  : Ctrl_State_R;

        -- debug signals
        signal bankCountersDbg : Bank_Helpers_T;
        signal burstCounterDbg : natural;

    begin
        bankCtrl : process(clkInternal)
            -- state regs
            -- times bank state changes
            variable bankCounters  : Bank_Helpers_T := (others => (counting => false, counter => 0, scheduledPrecharge => false));
            -- times subsequent active commands
            variable activeCounter : natural range 0 to tRRDCycles + 1;

            -- helper variables
            variable bankPtr : Bank_Ptr_T := 0;
            variable addrPtr : Row_Ptr_T  := 0;
            variable a10Flag : boolean    := false;
        begin
            bankCountersDbg <= bankCounters;

            if rising_edge(clkInternal) then
                -- helper variables
                bankPtr := to_safe_natural(inputReg.bank);
                addrPtr := to_safe_natural(inputReg.addr);
                a10Flag := logic_to_bool(inputReg.addr(10));

                -- resolve scheduled tasks
                for i in 0 to BANK_COUNT - 1 loop
                    if bankCounters(i).scheduledPrecharge and banks(i).state = ActiveIdle then
                        banks(i).state          <= Precharging;
                        bankCounters(i).counter := bank_transition_delay(Precharging, bank_next_state(Precharging));

                        Log(SDRAM_ALERT_ID, "Bank " & to_string(i) & " state change scheduled: " & Bank_State_T'image(banks(i).state) & " --> " & Bank_State_T'image(Precharging), DEBUG);
                    end if;

                    if bankCounters(i).counting then
                        if bankCounters(i).counter = 0 then
                            banks(i).state <= bank_next_state(banks(i).state);

                            Log(SDRAM_ALERT_ID, "Bank " & to_string(i) & " state change scheduled: " & Bank_State_T'image(banks(i).state) & " --> " & Bank_State_T'image(bank_next_state(banks(i).state)), DEBUG);

                            -- if bank is Active schedule transition from ActiveIdle to ActiveRecharging
                            if banks(i).state = Activating then
                                bankCounters(i).counter := bank_transition_delay(ActiveRecharging, ActiveIdle);
                            else
                                bankCounters(i).counting := false;
                            end if;
                        else
                            bankCounters(i).counter := bankCounters(i).counter - 1;
                        end if;
                    end if;
                end loop;

                -- decrement Active counter
                if activeCounter > 0 then
                    activeCounter := activeCounter - 1;
                end if;

                -- handle current command on input latch
                case inputReg.cmd is
                    -- open row for reading/writing
                    -- active(row_addr, bank_addr)
                    when Active =>
                        -- Cannot issue any command during loading of Mode Register
                        assert ctrl.state /= AccessingModeReg
                        report "Controller is not in valid state to Activate row"
                        severity error;

                        -- Bank to be activated must be Idle
                        assert banks(bankPtr).state = Idle
                        report "Bank " & natural'image(bankPtr) & " is not in Idle state"
                        severity error;

                        -- There is a delay between Active commands
                        assert activeCounter = 0
                        report "Cannot issue another Active command right after last one (not enough time passed)"
                        severity error;

                        banks(bankPtr).state  <= Activating;
                        banks(bankPtr).row    <= addrPtr;
                        bankCounters(bankPtr) := bank_schedule_transition(Activating, bank_next_state(Activating));
                        activeCounter         := tRRDCycles;

                        Log(SDRAM_ALERT_ID, "Bank " & to_string(bankPtr) & " state change scheduled: " & Bank_State_T'image(banks(bankPtr).state) & " --> " & Bank_State_T'image(Activating), DEBUG);

                    -- close activated row (if idle does nothing)
                    -- a10 flags selects whether to Precharge all banks (a10 HIGH -> Precharge All)
                    -- precharge(bank_addr, a10)
                    when Precharge =>
                        assert ctrl.state /= AccessingModeReg
                        report "Cannot issue any command other than NoOp or Command Inhibit during Load Mode Register operation"
                        severity error;

                        if a10Flag then -- precharge all banks (all must be in a valid state to Precharge)
                            for i in 0 to BANK_COUNT - 1 loop
                                assert banks(i).state = ActiveIdle or banks(i).state = Idle
                                report "All banks are not in Idle or Active state with capacitors recharged"
                                severity error;

                                -- cannot terminate Read/Write Burst with Auto Precharge using a Precharge command
                                assert (not ctrl.autoPrecharge) or (ctrl.state /= WriteBurst and ctrl.state /= ReadBurst)
                                report "Cannot Precharge all banks while doing a Burst with Auto Precharge enabled"
                                severity error;

                                -- cannot Precharge a bank that has a Precharge command already pending
                                -- as a result of a short Write/Read burst
                                assert not bankCounters(i).scheduledPrecharge
                                report "Bank " & natural'image(i) & " has a Precharge scheduled after a Write/Read burst with Auto Precharge"
                                severity error;

                                -- if banks is idle, Precharge acts as NOP
                                if (banks(i).state = ActiveIdle) then
                                    banks(i).state  <= Precharging;
                                    bankCounters(i) := bank_schedule_transition(Precharging, bank_next_state(Precharging));

                                    Log(SDRAM_ALERT_ID, "Bank " & to_string(i) & " state change scheduled: " & Bank_State_T'image(banks(i).state) & " --> " & Bank_State_T'image(Precharging), DEBUG);
                                end if;
                            end loop;
                        else            -- precharge only selected bank
                            assert banks(bankPtr).state = ActiveIdle or banks(bankPtr).state = Idle
                            report "Bank " & natural'image(bankPtr) & " is not in Idle or Active state with capacitors recharged"
                            severity error;

                            -- cannot terminate Read/Write Burst with Auto Precharge using a Precharge command
                            assert (not ctrl.autoPrecharge) or (ctrl.state /= WriteBurst and ctrl.state /= ReadBurst) or ctrl.currBank /= bankPtr
                            report "Cannot Precharge bank " & natural'image(bankPtr) & " while doing a Burst with Auto Precharge enabled"
                            severity error;

                            -- cannot Precharge a bank that has a Precharge command already pending
                            -- as a result of a short Write/Read burst
                            assert not bankCounters(bankPtr).scheduledPrecharge
                            report "Bank " & natural'image(bankPtr) & " has a Precharge scheduled after a Write/Read burst with Auto Precharge"
                            severity error;

                            -- if bank is Idle, Precharge acts as NOP
                            if (banks(bankPtr).state = ActiveIdle) then
                                banks(bankPtr).state  <= Precharging;
                                bankCounters(bankPtr) := bank_schedule_transition(Precharging, bank_next_state(Precharging));

                                Log(SDRAM_ALERT_ID, "Bank " & to_string(bankPtr) & " state change scheduled: " & Bank_State_T'image(banks(bankPtr).state) & " --> " & Bank_State_T'image(Precharging), DEBUG);
                            end if;
                        end if;

                    -- refresh(void)
                    -- must be run 4096 at least every 64 ms
                    when Refresh =>
                        assert ctrl.state = Idle
                        report "Controller not in Idle state, cannot start Auto Refresh"
                        severity error;

                        for i in 0 to BANK_COUNT - 1 loop
                            assert banks(i).state = Idle
                            report "Bank " & natural'image(i) & " not in Idle state, cannot start Auto Refresh"
                            severity error;

                            banks(i).state  <= Refreshing;
                            bankCounters(i) := bank_schedule_transition(Refreshing, bank_next_state(Refreshing));

                            Log(SDRAM_ALERT_ID, "Bank " & to_string(i) & " state change scheduled: " & Bank_State_T'image(banks(i).state) & " --> " & Bank_State_T'image(Refreshing), DEBUG);
                        end loop;
                        null;

                    -- of importance when autoPrecharge is true
                    when Read | Write =>
                        if ctrl.autoPrecharge and (ctrl.state = ReadBurst or ctrl.state = WriteBurst) then
                            -- sanity checks
                            assert banks(ctrl.currBank).state = ActiveRecharging or banks(ctrl.currBank).state = ActiveIdle
                            report "Bank " & natural'image(ctrl.currBank) & "is not currently in Active state, cannot start Auto Precharge"
                            severity error;

                            assert bankPtr /= ctrl.currBank
                            report "Cannot truncate a Burst with Auto Precharge enabled"
                            severity error;

                            if banks(ctrl.currBank).state = ActiveIdle then
                                banks(ctrl.currBank).state  <= Precharging;
                                bankCounters(ctrl.currBank) := bank_schedule_transition(Precharging, bank_next_state(Precharging));

                                Log(SDRAM_ALERT_ID, "Bank " & to_string(ctrl.currBank) & " state change scheduled: " & Bank_State_T'image(banks(ctrl.currBank).state) & " --> " & Bank_State_T'image(Precharging), DEBUG);
                            elsif banks(ctrl.currBank).state = ActiveRecharging then
                                bankCounters(ctrl.currBank).scheduledPrecharge := true;
                            else
                                report "Inconsistent banks state detected... (probably a source error)"
                                severity error;
                            end if;
                        end if;

                    -- ingnore NoOp, CmdInhibit, LoadModeReg, BurstTerminate
                    when NoOp | CmdInhibit | LoadModeReg | BurstTerminate =>
                        null;
                end case;
            end if;
        end process bankCtrl;

        -- main controlling process
        mainCtrl : process
            procedure write_mem(bank : in Bank_Ptr_T; row : in Row_Ptr_T; col : in Col_Ptr_T; data : in Data_T; dqm : in std_logic_vector(1 downto 0)) is
                variable fullAddr : Full_Addr_T := addr_ptr_to_addr(bank, row, col);
                variable currData : Data_T      := memoryModel.MemRead(fullAddr);
            begin
                Log(SDRAM_ALERT_ID, "Writing to Bank: " & to_string(bank) & ", Row: " & to_string(row) & ", Col: " & to_string(col) & ", Dqm: " & to_bstring(dqm) & ", Data: " & to_hstring(data), DEBUG);

                for i in dqm'range loop
                    -- mask data according to dqm
                    -- if mask bit is low, allow writing to target byte
                    if dqm(i) = '0' then
                        currData(((i + 1) * 8) - 1 downto i * 8) := data(((i + 1) * 8) - 1 downto i * 8);
                    end if;
                end loop;

                memoryModel.MemWrite(fullAddr, currData);
            end procedure write_mem;

            impure function read_mem(bank : in Bank_Ptr_T; row : in Row_Ptr_T; col : in Col_Ptr_T) return Data_T is
                variable fullAddr : Full_Addr_T := addr_ptr_to_addr(bank, row, col);
            begin
                Log(SDRAM_ALERT_ID, "Reading from Bank: " & to_string(bank) & ", Row: " & to_string(row) & ", Col: " & to_string(col), DEBUG);

                return memoryModel.MemRead(fullAddr);
            end function read_mem;

            -- state variables
            variable currCol      : Col_Ptr_T                            := 0;
            variable burstCounter : natural range 0 to 2**COL_ADDR_WIDTH := 0;

            impure function get_curr_col return Col_Ptr_T is
                constant COL_END : natural := 2**COL_ADDR_WIDTH - 1;
                variable tmp     : natural;
            begin
                tmp := currCol + burstCounter;
                -- wrap the burst around column end
                while tmp > COL_END loop
                    tmp := tmp mod COL_END;
                end loop;
                assert tmp >= 0;
                return tmp;
            end function get_curr_col;

            -- helper variables
            variable bankPtr                        : Bank_Ptr_T := 0;
            variable addrPtr                        : Row_Ptr_T  := 0;
            variable a10Flag                        : boolean    := false;
            variable data                           : Data_T     := (others => 'Z');
            variable isPrechargingAll, isRefreshing : boolean    := false;
        begin
            if LOAD_FROM_FILE then
                report "Reading (sparse) memory from file, please wait..." severity note;
                memoryModel.FileReadH(INPUT_FILENAME);
            end if;

            while not simEndedIn loop
                wait until rising_edge(clkInternal);
                if rising_edge(clkInternal) then
                    -- debug signals
                    burstCounterDbg <= burstCounter;

                    -- helper variables
                    bankPtr := to_safe_natural(inputReg.bank);
                    addrPtr := to_safe_natural(inputReg.addr);
                    a10Flag := logic_to_bool(inputReg.addr(10));
                    data    := dataInPipeline(0).data;

                    -- set additional state flags
                    isPrechargingAll := true;
                    isRefreshing     := true;
                    for i in 0 to BANK_COUNT - 1 loop
                        isPrechargingAll := isPrechargingAll and banks(i).state = Precharging;
                        isRefreshing     := isRefreshing and banks(i).state = Refreshing;
                    end loop;

                    -- increment dataOutPipeline shift register
                    dataOutPipeline(0 to 2) <= dataOutPipeline(1 to 3);
                    dataOutPipeline(3)      <= (others => 'Z');

                    -- TODO: implement single location Write option + Auto Precharge + tRDL
                    -- TODO: might be best to re-architecture
                    -- resolve scheduled Read(s)/Write(s)
                    if ctrl.state = ReadBurst or ctrl.state = WriteBurst then
                        if ctrl.state = ReadBurst and inputReg.cmd /= BurstTerminate then
                            -- FIXME: add hold and setup time requirements
                            dataOutPipeline(latencyMode - 2) <= read_mem(ctrl.currBank, banks(ctrl.currBank).row, get_curr_col);
                        elsif ctrl.state = WriteBurst then
                            write_mem(ctrl.currBank, banks(ctrl.currBank).row, get_curr_col, dataInPipeline(1).data, dataInPipeline(1).dqm);
                        end if;
                    end if;

                    -- tMRD = 2 cycles
                    if ctrl.state = AccessingModeReg then
                        ctrl.state <= Idle;

                        Log(SDRAM_ALERT_ID, "Ctrl State Change scheduled: " & Sdram_State_T'image(ctrl.state) & " --> " & Sdram_State_T'image(Idle), DEBUG);
                    end if;

                    -- increment burst counter
                    if ctrl.state = WriteBurst or ctrl.state = ReadBurst then
                        -- if write burst is disabled, only write to one location
                        if ctrl.state = WriteBurst and writeBurstMode = SingleLocation then
                            ctrl.state <= Idle;
                        elsif burstCounter = burstLength then
                            -- FullPage burst mode wraps around (only ends after user cancellation)
                            if burstLength = PAGE_LEN then
                                burstCounter := 0;
                            else
                                ctrl.state <= Idle;
                            end if;
                        else
                            if burstType = Sequential then
                                burstCounter := burstCounter + 1;
                            else
                                -- FIXME: implement interleaved burst
                                null;
                            end if;
                        end if;
                    end if;

                    -- debug log
                    if inputReg.cmd /= NoOp and inputReg.cmd /= CmdInhibit then
                        Log(SDRAM_ALERT_ID, "Received command: " & Cmd_T'image(inputReg.cmd), INFO);
                    end if;

                    case inputReg.cmd is
                        when CmdInhibit | NoOp =>
                            null;

                        -- Start a Read burst beginning from selected column
                        -- a10Flag = Auto Precharge
                        -- read(col_addr, bank_addr, a10Flag)
                        when Read =>
                            assert ctrl.state /= AccessingModeReg
                            report "Cannot Read during Load Mode Register operation"
                            severity warning;

                            assert banks(bankPtr).state = ActiveRecharging or banks(bankPtr).state = ActiveIdle
                            report "Selected bank not Active, cannot start Read burst"
                            severity error;

                            -- cannot start a new read burst in the same bank if Auto Precharge is enabled
                            assert (not ctrl.autoPrecharge) or ctrl.currBank /= bankPtr
                            report "Cannot initiate new Read burst to bank doing Burst with Auto Precharge"
                            severity error;

                            ctrl.state         <= ReadBurst;
                            ctrl.currBank      <= bankPtr;
                            ctrl.autoPrecharge <= a10Flag;

                            Log(SDRAM_ALERT_ID, "Ctrl State Change scheduled: " & Sdram_State_T'image(ctrl.state) & " --> " & Sdram_State_T'image(ReadBurst), INFO);

                            -- setup burst counter
                            currCol      := addrPtr;
                            burstCounter := 0;

                            -- immediately output read data
                            dataOutPipeline(latencyMode - 2) <= read_mem(bankPtr, banks(bankPtr).row, get_curr_col);

                            burstCounter := 1;

                        when Write =>
                            assert ctrl.state /= AccessingModeReg
                            report "Cannot Read during Load Mode Register operation"
                            severity error;

                            assert banks(bankPtr).state = ActiveRecharging or banks(bankPtr).state = ActiveIdle
                            report "Selected bank not Active, cannot start Write burst"
                            severity error;

                            -- cannot start a new write burst in the same bank if Auto Precharge is enabled
                            assert (not ctrl.autoPrecharge) or ctrl.currBank /= bankPtr
                            report "Cannot initiate new Read burst to bank doing Burst with Auto Precharge"
                            severity error;

                            ctrl.state         <= WriteBurst;
                            ctrl.currBank      <= bankPtr;
                            ctrl.autoPrecharge <= a10Flag;

                            Log(SDRAM_ALERT_ID, "Ctrl State Change scheduled: " & Sdram_State_T'image(ctrl.state) & " --> " & Sdram_State_T'image(WriteBurst), INFO);

                            currCol      := addrPtr;
                            burstCounter := 0;

                        -- immediately write input data
                        --                            write_mem(bankPtr, banks(bankPtr).row, get_curr_col, inputReg.data, inputReg.dqm);

                        when BurstTerminate =>
                            assert ctrl.state = WriteBurst or ctrl.state = ReadBurst
                            report "Controller currently not doing a Write/Read burst"
                            severity warning;

                            assert not ctrl.autoPrecharge
                            report "Cannot truncate a burst with Auto Precharge enabled"
                            severity error;

                            ctrl.state <= Idle;

                            Log(SDRAM_ALERT_ID, "Ctrl State Change scheduled: " & Sdram_State_T'image(ctrl.state) & " --> " & Sdram_State_T'image(Idle), INFO);

                            currCol      := 0;
                            burstCounter := 0;

                        when Precharge =>
                            assert (ctrl.state /= WriteBurst and ctrl.state /= ReadBurst) or (not ctrl.autoPrecharge) or (not a10Flag and ctrl.currBank /= bankPtr)
                            report "Cannot truncate a Read/Write burst with Auto Precharge enabled using Precharge command"
                            severity error;

                            -- if Precharge all or Precharge current bank
                            if (ctrl.state = WriteBurst or ctrl.state = ReadBurst) and not ctrl.autoPrecharge then
                                if a10Flag or bankPtr = ctrl.currBank then
                                    -- truncate Read/Write burst without Auto Precharge
                                    ctrl.state <= Idle;

                                    Log(SDRAM_ALERT_ID, "Ctrl State Change scheduled: " & Sdram_State_T'image(ctrl.state) & " --> " & Sdram_State_T'image(Idle), INFO);
                                end if;
                            end if;

                        when LoadModeReg =>
                            assert ctrl.state = Idle
                            report "Controller not in Idle state, cannot Load Mode Register"
                            severity error;

                            for i in 0 to BANK_COUNT - 1 loop
                                assert banks(i).state = Idle
                                report "Bank " & natural'image(i) & " not in Idle state, cannot Load Mode Register"
                                severity error;
                            end loop;

                            assert validate_mode_reg(data)
                            report "Invalid Mode Register value on data input, cannot Load Mode Register"
                            severity error;

                            Log(SDRAM_ALERT_ID, "Received Load Mode Register command with payload: 0x" & to_hstring(data), INFO);

                            ctrl.state <= AccessingModeReg;
                            modeReg    <= data;

                            Log(SDRAM_ALERT_ID, "Ctrl State Change scheduled: " & Sdram_State_T'image(ctrl.state) & " --> " & Sdram_State_T'image(AccessingModeReg), INFO);

                        -- Active does not terminate read/write burst
                        -- Refresh handled by bank controller
                        when Refresh | Active =>
                            null;

                            -- TODO: implement command error handling
                            --                        when CmdError =>
                            --                            report "Cannot decode command"
                            --                            severity error;

                    end case;
                end if;
            end loop;

            if DUMP_TO_FILE and simEndedIn then
                Log(SDRAM_ALERT_ID, "Dumping (sparse) memory to file: """ & OUTPUT_FILENAME & """, please wait ...", INFO);
                memoryModel.FileWriteH(OUTPUT_FILENAME);
            end if;

            wait;
        end process mainCtrl;

        -- check if sdram is being properly refreshed at least every tREF
        refreshCheckProc : process(clkInternal)
            variable lastRefreshCycle : time                                 := NOW;
            variable refreshCounter   : natural range 0 to 2**ROW_ADDR_WIDTH := 2**ROW_ADDR_WIDTH;
        begin
            assert NOW - lastRefreshCycle < tREF
            report "Didn't refresh all the rows in time!"
            severity error;

            if rising_edge(clkInternal) then
                if currCmd = Refresh then
                    if refreshCounter = 0 then
                        lastRefreshCycle := NOW;
                        refreshCounter   := 2**ROW_ADDR_WIDTH;
                    else
                        refreshCounter := refreshCounter - 1;
                    end if;
                end if;
            end if;
        end process refreshCheckProc;

        -- we should check for proper sdram initialization after power-on
        initCheckProc : process(clkInternal)
            type Internal_State_T is (InitialWait, PrechargeAll, Refresh, Done);

            variable currState   : Internal_State_T := InitialWait;
            variable waitCounter : natural          := 200 us / CLK_PERIOD;
        begin
            if rising_edge(clkInternal) then
                case currState is
                    when InitialWait =>
                        if waitCounter = 0 then
                            currState := PrechargeAll;
                        else
                            waitCounter := waitCounter - 1;
                        end if;

                    when PrechargeAll =>
                        -- received PrechargeAll command
                        if inputReg.cmd = Precharge and inputReg.addr(10) = '1' then
                            currState   := Refresh;
                            waitCounter := 2;
                        elsif inputReg.cmd /= NoOp and inputReg.cmd /= CmdInhibit then
                            report "Didn't receive a PrechargeAll command after stable clock & power during initialization"
                            severity error;
                        end if;

                    when Refresh =>
                        if waitCounter = 0 then
                            if inputReg.cmd = LoadModeReg then
                                currState := Done;
                            elsif inputReg.cmd /= NoOp and inputReg.cmd /= CmdInhibit and inputReg.cmd /= Refresh then
                                report "Before issuing any other command, mode register must first be initialized"
                                severity error;
                            end if;
                        else
                            if inputReg.cmd = Refresh then
                                waitCounter := waitCounter - 1;
                            elsif inputReg.cmd /= NoOp and inputReg.cmd /= CmdInhibit then
                                report "Didn't receive a Refresh command at least 2 times after Precharge all during initialization"
                                severity error;
                            end if;
                        end if;

                    when Done =>
                        isInitializedOut <= true;

                end case;
            end if;
        end process initCheckProc;
    end block ctrlBlock;
end architecture model;
