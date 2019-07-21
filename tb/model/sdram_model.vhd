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
        -- simulation settings
        LOAD_FROM_FILE   : boolean := false; -- whether to load memory content from file
        DUMP_TO_FILE     : boolean := false; -- whether to store memory content to a file
        INPUT_FILE_NAME  : string  := "input_fjel.txt"; -- name of a file to be loaded
        OUTPUT_FILE_NAME : string  := "output_fjel.txt" -- name of a file to be dumped
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
        isInitialized                        : out   boolean := false;
        simEnded                             : in    boolean
    );
end entity sdram_model;

architecture model of sdram_model is
    -- mode register (register)
    signal modeReg : std_logic_vector(DATA_WIDTH - 1 downto 0) := encode_mode_reg(1, Sequential, 2, ProgrammedLength);

    -- decoded current command on input (wire) (non-latched)
    signal currCmd : Cmd_T := NoOp;

    -- decoded state from mode register (wire)
    signal burstLength    : Burst_Length_T     := 1;
    signal burstType      : Burst_Type_T       := Sequential;
    signal latencyMode    : Latency_Mode_T     := 2;
    signal writeBurstMode : Write_Burst_Mode_T := ProgrammedLength;

    -- internal signals (reg)
    signal clkInternal : std_logic := '0';

    -- input data latch (register)
    signal inputReg : Input_Latch_R;
begin

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
                inputReg <= (
                    addr => addrIn,
                    dqm  => dqmIn,
                    cmd  => currCmd,
                    bank => bankSelectIn,
                    data => dataIo
                );
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
        type Data_Out_Pipeline_T is array (0 to 9) of Data_T;

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
        signal banks           : Bank_Array_T        := (others => (state => Idle, row => 0));
        signal ctrl            : Ctrl_State_R;
        signal dataOutPipeline : Data_Out_Pipeline_T := (others => (others => 'Z'));

        -- debug signals
        signal bankCountersDbg : Bank_Helpers_T;

    begin
        bankCtrl : process(clkInternal)
            -- store timings

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

                        Log("Bank " & to_string(i) & " state change scheduled: " & Bank_State_T'image(banks(i).state) & " --> " & Bank_State_T'image(Precharging), DEBUG);
                    end if;

                    if bankCounters(i).counting then
                        if bankCounters(i).counter = 0 then
                            banks(i).state <= bank_next_state(banks(i).state);

                            Log("Bank " & to_string(i) & " state change scheduled: " & Bank_State_T'image(banks(i).state) & " --> " & Bank_State_T'image(bank_next_state(banks(i).state)), DEBUG);

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

                -- debug log
                if inputReg.cmd /= NoOp and inputReg.cmd /= CmdInhibit then
                    Log("Received command: " & Cmd_T'image(inputReg.cmd), INFO);
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

                        Log("Bank " & to_string(bankPtr) & " state change scheduled: " & Bank_State_T'image(banks(bankPtr).state) & " --> " & Bank_State_T'image(Activating), DEBUG);

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

                                    Log("Bank " & to_string(i) & " state change scheduled: " & Bank_State_T'image(banks(i).state) & " --> " & Bank_State_T'image(Precharging), DEBUG);
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

                                Log("Bank " & to_string(bankPtr) & " state change scheduled: " & Bank_State_T'image(banks(bankPtr).state) & " --> " & Bank_State_T'image(Precharging), DEBUG);
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

                            Log("Bank " & to_string(i) & " state change scheduled: " & Bank_State_T'image(banks(i).state) & " --> " & Bank_State_T'image(Refreshing), DEBUG);
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

                                Log("Bank " & to_string(ctrl.currBank) & " state change scheduled: " & Bank_State_T'image(banks(ctrl.currBank).state) & " --> " & Bank_State_T'image(Precharging), DEBUG);
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
            type Mem_Row_T is array (0 to 2**COL_ADDR_WIDTH - 1) of bit_vector(DATA_WIDTH - 1 downto 0);
            type Mem_Row_Ptr_T is access Mem_Row_T;
            type Mem_Bank_T is array (0 to 2**ROW_ADDR_WIDTH - 1) of Mem_Row_Ptr_T;
            type Mem_Bank_Array_T is array (Bank_Ptr_T) of Mem_Bank_T;

            -- memory data storage
            variable bankData : Mem_Bank_Array_T;

            -- helper memory procedures
            procedure init_mem(bank : in Bank_Ptr_T; row : in Row_Ptr_T) is
            begin
                if bankData(bank)(row) = NULL then
                    bankData(bank)(row) := new Mem_Row_T;
                    for col in Col_Ptr_T loop
                        bankData(bank)(row)(col) := (others => '0');
                    end loop;
                end if;
            end procedure init_mem;

            procedure write_mem(bank : in Bank_Ptr_T; row : in Row_Ptr_T; col : in Col_Ptr_T; data : in Data_T; dqm : in std_logic_vector(1 downto 0)) is
            begin
                -- if data = all zeroes, we don't have to do nothing, that's the default state of mem
                if data /= (data'range => '0') and dqm /= (dqm'range => '1') then
                    init_mem(bank, row);
                    for i in dqm'range loop
                        -- mask data according to dqm
                        -- if mask bit is low, allow writing to target byte
                        if dqm(i) = '0' then
                            bankData(bank)(row)(col)(((i + 1) * 8) - 1 downto i * 8) := to_bitvector(data(((i + 1) * 8) - 1 downto i * 8));
                        end if;
                    end loop;
                end if;
            end procedure write_mem;

            impure function read_mem(bank : in Bank_Ptr_T; row : in Row_Ptr_T; col : in Col_Ptr_T; dqm : in std_logic_vector(1 downto 0)) return Data_T is
                variable tmpData : Data_T := (others => '0');
            begin
                if bankData(bank)(row) = NULL then
                    tmpData := (others => '0');
                else
                    tmpData := to_slv(bankData(bank)(row)(col));
                end if;

                for i in dqm'range loop
                    -- mask data according to dqm
                    -- if mask bit is low, allow writing to target byte
                    if dqm(i) = '0' then
                        tmpData(((i + 1) * 8) - 1 downto i * 8) := tmpData(((i + 1) * 8) - 1 downto i * 8);
                    end if;
                end loop;

                return tmpData;
            end function read_mem;

            -- tmp
            variable loadDone                                 : boolean := false;
            file inputFile, outputFile                        : text;
            variable inputLine, outputLine                    : line;
            variable bankCount, rowCount, colCount, dataWidth : natural;
            variable rowUsed                                  : boolean;
            variable tmpCol                                   : bit_vector(DATA_WIDTH - 1 downto 0);

            -- state variables
            variable currCol      : Col_Ptr_T                                := 0;
            variable burstCounter : natural range 0 to 2**COL_ADDR_WIDTH - 1 := 0;

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
            if LOAD_FROM_FILE and not loadDone then
                -- run only once
                loadDone := true;

                report "Reading (sparse) memory from file, please wait..." severity note;
                file_open(inputFile, INPUT_FILE_NAME, read_mode);

                while not endfile(inputFile) loop
                    readline(inputFile, inputLine);

                    read(inputLine, bankCount);
                    assert bankCount = BANK_COUNT;
                    read(inputLine, rowCount);
                    assert rowCount = 2**ROW_ADDR_WIDTH;
                    read(inputLine, colCount);
                    assert colCount = 2**COL_ADDR_WIDTH;
                    read(inputLine, dataWidth);
                    assert dataWidth = DATA_WIDTH;

                    for bank in 0 to BANK_COUNT - 1 loop
                        for row in Row_Ptr_T loop
                            readline(inputFile, inputLine);
                            read(inputLine, rowUsed);

                            if rowUsed then
                                init_mem(bank, row);
                                for col in Col_Ptr_T loop
                                    read(inputLine, tmpCol);
                                    bankData(bank)(row)(col) := tmpCol;
                                end loop;
                            end if;
                        end loop;
                    end loop;
                end loop;
                file_close(inputFile);
            end if;

            while not simEnded loop
                wait until rising_edge(clkInternal);
                if rising_edge(clkInternal) then
                    -- helper variables
                    bankPtr := to_safe_natural(inputReg.bank);
                    addrPtr := to_safe_natural(inputReg.addr);
                    a10Flag := logic_to_bool(inputReg.addr(10));
                    data    := inputReg.data;

                    -- set additional state flags
                    isPrechargingAll := true;
                    isRefreshing     := true;
                    for i in 0 to BANK_COUNT - 1 loop
                        isPrechargingAll := isPrechargingAll and banks(i).state = Precharging;
                        isRefreshing     := isRefreshing and banks(i).state = Refreshing;
                    end loop;

                    -- TODO: implement single location Write option + Auto Precharge + tRDL
                    -- TODO: might be best to re-architecture
                    -- resolve scheduled Read(s)/Write(s)
                    if ctrl.state = ReadBurst or ctrl.state = WriteBurst then
                        if ctrl.state = ReadBurst then
                            -- FIXME: CL latency
                            -- FIXME: add hold and setup time requirements
                            dataOutPipeline(latencyMode - 2) <= read_mem(ctrl.currBank, banks(ctrl.currBank).row, get_curr_col, inputReg.dqm);
                        else
                            write_mem(ctrl.currBank, banks(ctrl.currBank).row, get_curr_col, inputReg.data, inputReg.dqm);
                        end if;
                    end if;

                    -- tMRD = 2 cycles
                    if ctrl.state = AccessingModeReg then
                        ctrl.state <= Idle;

                        Log("Ctrl State Change scheduled: " & Sdram_State_T'image(ctrl.state) & " --> " & Sdram_State_T'image(Idle), DEBUG);
                    end if;

                    -- increment burst counter
                    if ctrl.state = WriteBurst or ctrl.state = ReadBurst then
                        -- if write burst is disabled, only write to one location
                        if ctrl.state = WriteBurst and writeBurstMode = SingleLocation then
                            ctrl.state <= Idle;
                        elsif burstCounter = burstLength then
                            -- FullPage burst mode wraps around (only ends after user cmd)
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

                            Log("Ctrl State Change scheduled: " & Sdram_State_T'image(ctrl.state) & " --> " & Sdram_State_T'image(ReadBurst), DEBUG);

                            currCol      := addrPtr;
                            burstCounter := 0;

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

                            Log("Ctrl State Change scheduled: " & Sdram_State_T'image(ctrl.state) & " --> " & Sdram_State_T'image(WriteBurst), DEBUG);

                            currCol      := addrPtr;
                            burstCounter := 0;

                        when BurstTerminate =>
                            assert ctrl.state = WriteBurst or ctrl.state = ReadBurst
                            report "Controller currently not doing a Write/Read burst"
                            severity warning;

                            assert not ctrl.autoPrecharge
                            report "Cannot truncate a burst with Auto Precharge enabled"
                            severity error;

                            ctrl.state <= Idle;

                            Log("Ctrl State Change scheduled: " & Sdram_State_T'image(ctrl.state) & " --> " & Sdram_State_T'image(Idle), DEBUG);

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

                                    Log("Ctrl State Change scheduled: " & Sdram_State_T'image(ctrl.state) & " --> " & Sdram_State_T'image(Idle), DEBUG);
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

                            Log("Received Load Mode Register command with payload: 0x" & to_hstring(inputReg.data), DEBUG);

                            ctrl.state <= AccessingModeReg;
                            modeReg    <= data;

                            Log("Ctrl State Change scheduled: " & Sdram_State_T'image(ctrl.state) & " --> " & Sdram_State_T'image(AccessingModeReg), DEBUG);

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

            if DUMP_TO_FILE and simEnded then
                report "Dumping (sparse) memory to file, please wait..." severity note;
                file_open(outputFile, OUTPUT_FILE_NAME, write_mode);

                write(outputLine, BANK_COUNT);
                write(outputLine, 2**ROW_ADDR_WIDTH);
                write(outputLine, 2**COL_ADDR_WIDTH);
                write(outputLine, DATA_WIDTH);
                writeline(outputFile, outputLine);

                for bank in Bank_Ptr_T loop
                    for row in Row_Ptr_T loop
                        if bankData(bank)(row) = NULL then
                            write(outputLine, false);
                        else
                            write(outputLine, true);
                            for col in Col_Ptr_T loop
                                write(outputLine, bankData(bank)(row)(col));
                            end loop;
                        end if;
                        writeline(outputFile, outputLine);
                    end loop;
                end loop;
                file_close(outputFile);
            end if;

            wait;
        end process mainCtrl;

        -- check if sdram is being properly refreshed at least every tREF
        refreshCheckProc : process(clkInternal)
            variable lastRefreshCycle : time                                 := NOW;
            variable refreshCounter   : natural range 0 to 2**ROW_ADDR_WIDTH := 2**ROW_ADDR_WIDTH;
        begin
            assert NOW - lastRefreshCycle < tREF
            report "Didn't refresh all rows in time!"
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
            type Internal_State_T is (InitialWait, PrechargeAll, Refresh, SetModeReg, Done);

            variable currState : Internal_State_T := InitialWait;
            variable counter   : natural          := 200 us / CLK_PERIOD;
        begin
            if rising_edge(clkInternal) then
                case currState is
                    when InitialWait =>
                        if counter = 0 then
                            currState := PrechargeAll;
                        else
                            counter := counter - 1;
                        end if;

                    when PrechargeAll =>
                        if inputReg.cmd = Precharge and inputReg.addr(10) = '1' then
                            currState := Refresh;
                            counter   := 2**(ROW_ADDR_WIDTH + 1);
                        elsif inputReg.cmd /= NoOp and inputReg.cmd /= CmdInhibit then
                            report "Didn't receive a PrechargeAll command after stable clock & power during initialization"
                            severity error;
                        end if;

                    when Refresh =>
                        if inputReg.cmd = Refresh then
                            if counter = 0 then
                                currState := SetModeReg;
                            else
                                counter := counter - 1;
                            end if;
                        elsif inputReg.cmd /= NoOp and inputReg.cmd /= CmdInhibit then
                            report "Didn't receive a Refresh command 4096*2 times after Precharge all during initialization"
                            severity error;
                        end if;

                    when SetModeReg =>
                        if inputReg.cmd = LoadModeReg then
                            currState := Done;
                        elsif inputReg.cmd /= NoOp and inputReg.cmd /= CmdInhibit then
                            report "Didn't receive a Load Mode Register command after refreshing all rows during initialization"
                            severity error;
                        end if;

                    when Done =>
                        isInitialized <= true;

                end case;
            end if;
        end process initCheckProc;
    end block ctrlBlock;
end architecture model;
