library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use work.sdram_pkg_new.all;

entity sdram_model_new is
    generic(
        -- timings
        -- FIXME: fix this
        tCAS           : time    := 0 ns;
        tAC            : time    := 6.0 ns; -- access time from clk
        tHZ            : time    := 7.0 ns; -- output high impedance time
        tOH            : time    := 2.7 ns; -- output data hold time
        tMRD           : natural := 2;  -- clock cycles between write to mode register and next command
        tRAS           : time    := 44.0 ns; -- row address strobe (active to precharge)
        tRC            : time    := 66.0 ns; -- row cycle (ref to ref / activate to activate)
        tRCD           : time    := 20.0 ns; -- RAS to CAS delay (active command to read/write command delay time)
        tRP            : time    := 20.0 ns; -- row precharge time (min. time between precharging row and activating new one)
        tRRD           : time    := 15.0 ns; -- bank to bank delay time (min. time between successive ACTIVE commands to different banks)
        tWRa           : time    := 7.5 ns; -- A2 Version - Auto precharge mode only (1 Clk + 7.5 ns)
        tWRp           : time    := 15.0 ns; -- A2 Version - Precharge mode only (15 ns)
        -- FIXME: implement 
        tCKA           : time    := 0 ns; -- CKE to CLK recovery delay time

        tAH            : time    := 0.8 ns; -- address hold time
        tAS            : time    := 1.5 ns; -- address setup time
        tCH            : time    := 2.5 ns; -- clk high level width
        tCL            : time    := 2.5 ns; -- clk low level width
        tCK            : time    := 10.0 ns; -- clk cycle time
        tDH            : time    := 0.8 ns; -- input data hold time
        tDS            : time    := 1.5 ns; -- input data setup time
        tCKH           : time    := 0.8 ns; -- CKE hold time (clk enable?)
        tCKS           : time    := 1.5 ns; -- CKE setup time
        tCMH           : time    := 0.8 ns; -- command hold time
        tCMS           : time    := 1.5 ns; -- command setup time

        tREF           : time    := 64 ms; -- refresh cycle time (4096)

        -- period of clkIn
        CLK_PERIOD     : time    := 8 ns;
        -- ram parameters
        ADDR_WIDTH     : natural := 12;
        COL_WIDTH      : natural := 8;
        DATA_WIDTH     : natural := 16;
        -- simulation settings
        LOAD_FROM_FILE : boolean := false;
        DUMP_TO_FILE   : boolean := false
    );
    port(
        clkIn                                : in    std_logic;
        addrIn                               : in    unsigned(ADDR_WIDTH - 1 downto 0);
        dataIn                               : inout std_logic_vector(DATA_WIDTH - 1 downto 0);
        bankSelectIn                         : in    unsigned(1 downto 0);
        clkEnableIn                          : in    std_logic;
        chipSelectNegIn, rowAddrStrobeNegIn  : in    std_logic;
        colAddrStrobeNegIn, writeEnableNegIn : in    std_logic;
        dqmIn                                : in    std_logic_vector(1 downto 0);
        simEnded                             : in    boolean
    );
    subtype Mem_Data_T is std_logic_vector(DATA_WIDTH - 1 downto 0);
    subtype Mem_Addr_T is unsigned(ADDR_WIDTH - 1 downto 0);

    -- TODO: fix duplicate types
    subtype Bank_T is natural range 0 to BANK_COUNT - 1;
    subtype Row_T is natural range 0 to 2**ADDR_WIDTH - 1;
    subtype Col_T is natural range 0 to 2**COL_WIDTH - 1;

    type Bank_State_R is record
        state : Bank_State_T;
        row   : Row_T;
    end record Bank_State_R;
    type Bank_Array_T is array (BANK_COUNT - 1 downto 0) of Bank_State_R;

    type Ctrl_State_R is record
        state         : Ctrl_State_T;
        autoPrecharge : boolean;
        currBank      : Bank_T;
        burstDone     : boolean;
    end record Ctrl_State_R;

    type Input_Latch_R is record
        cmd  : Cmd_T;
        dqm  : std_logic_vector(1 downto 0);
        addr : Mem_Addr_T;
        bank : unsigned(1 downto 0);
        data : Mem_Data_T;
    end record Input_Latch_R;

    subtype Latency_Mode_Range_T is natural range 2 to 3;
    subtype Burst_Length_Range_T is natural range 1 to 2**COL_WIDTH - 1;
    constant PAGE_LEN : natural := 2**COL_WIDTH;
    subtype Bank_Range_T is natural range 0 to BANK_COUNT - 1;
    subtype Row_Range_T is natural range 0 to 2**ADDR_WIDTH - 1;
    subtype Col_Range_T is natural range 0 to 2**COL_WIDTH - 1;
end entity sdram_model_new;

architecture model of sdram_model_new is
    -- mode register (register)
    signal modeReg : std_logic_vector(DATA_WIDTH - 1 downto 0) := (others => '0');

    -- decoded current command on input (wire) (non-latched)
    signal currCmd : Cmd_T := NoOp;

    -- decoded state from mode register (wire)
    signal burstLength    : Burst_Length_Range_T := -1;
    signal burstType      : Burst_Type_T         := Sequential;
    signal latencyMode    : Latency_Mode_Range_T := 2;
    signal writeBurstMode : Write_Burst_Mode_T   := ProgrammedLength;

    -- internal signals (reg)
    signal clkInternal : std_logic := '0';

    -- input data latch (register)
    signal inputReg : Input_Latch_R;
begin

    decodeBlock : block
        signal clkEnabled         : std_logic := '0';
        signal cmdSelectAggregate : std_logic_vector(3 downto 0);
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
                    data => dataIn
                );
            end if;
        end process latchProc;

        -- only enable clock if clkEnableIn is high
        with clkEnabled select clkInternal <=
            clkIn when '1',
            '0' when others;

        -- decode signals to internal representation
        cmdSelectAggregate <= chipSelectNegIn & rowAddrStrobeNegIn & colAddrStrobeNegIn & writeEnableNegIn;

        with cmdSelectAggregate select currCmd <=
            CmdInhibit when "1---",
            NoOp when "0111",
            Active when "0011",
            Read when "0101",
            Write when "0100",
            BurstTerminate when "0110",
            Precharge when "0010",
            Refresh when "0001",
            LoadModeReg when "0000",
            CmdError when others;

        with modeReg(2 downto 0) select burstLength <=
            1 when "000",
            2 when "001",
            4 when "010",
            8 when "011",
            PAGE_LEN when "111",
            1 when others;

        with modeReg(3) select burstType <=
            Sequential when '0',
            Interleaved when '1',
            Sequential when others;

        with modeReg(6 downto 4) select latencyMode <=
            2 when "010",
            3 when "011",
            2 when others;

        with modeReg(9) select writeBurstMode <=
            ProgrammedLength when '0',
            SingleLocation when '1',
            ProgrammedLength when others;
    end block decodeBlock;

    ctrlBlock : block
        type Data_Out_Pipeline_T is array (0 to 9) of Mem_Data_T;

        -- ctrl/banks state representation (reg)
        signal banks           : Bank_Array_T        := (others => (state => Idle, row => 0));
        signal ctrl            : Ctrl_State_R;
        signal dataOutPipeline : Data_Out_Pipeline_T := (others => (others => 'Z'));
    begin

        bankCtrl : process(clkInternal)
            -- FIXME: generify
            constant PRE_TO_IDLE                      : natural := 2;
            constant IDLE_TO_ACTIVE_RECHARGING        : natural := 2;
            constant ACTIVE_RECHARGING_TO_ACTIVE_IDLE : natural := 3;
            constant ACTIVE_TO_ACTIVE                 : natural := 2;
            constant REFRESH_TO_IDLE                  : natural := IDLE_TO_ACTIVE_RECHARGING + ACTIVE_RECHARGING_TO_ACTIVE_IDLE + PRE_TO_IDLE;

            -- TODO: change upper counter limit
            type Bank_Counter_R is record
                active  : boolean;
                counter : natural range 0 to 10;
            end record Bank_Counter_R;
            type Bank_Counters_T is array (0 to BANK_COUNT - 1) of Bank_Counter_R;

            pure function get_delay(currState : Bank_State_T; nextState : Bank_State_T) return natural is
                type State_Delay_Map_T is array (Bank_State_T, Bank_State_T) of natural;
                constant transitionDelay : State_Delay_Map_T := (
                    Idle             => (
                        Refreshing => 0,
                        Activating => 0,
                        others     => 0),
                    Refreshing       => (
                        Idle   => REFRESH_TO_IDLE,
                        others => 0
                    ),
                    Activating       => (
                        ActiveRecharging => IDLE_TO_ACTIVE_RECHARGING,
                        others           => 0
                    ),
                    ActiveRecharging => (
                        ActiveIdle => ACTIVE_RECHARGING_TO_ACTIVE_IDLE,
                        others     => 0
                    ),
                    ActiveIdle       => (
                        Precharging => 0,
                        others      => 0
                    ),
                    Precharging      => (
                        Idle   => PRE_TO_IDLE,
                        others => 0
                    )
                );
            begin
                if bank_transition_valid(currState, nextState) then
                    return transitionDelay(currState, nextState);
                else
                    report "Invalid state transition!" & LF &
                    "Cannot go from state " & Bank_State_T'image(currState) & " to state " & Bank_State_T'image(nextState)
                    severity error;
                end if;
            end function get_delay;

            -- store timings
            variable bankCounters  : Bank_Counters_T := (others => (active => false, counter => 0));
            variable activeCounter : natural range 0 to 10; -- TODO: generic upper limit

            -- helper variables
            variable currBank : Bank_T  := 0;
            variable currAddr : Row_T   := 0;
            variable a10Flag  : boolean := false;
        begin
            if rising_edge(clkInternal) then
                -- helper variables
                currBank := to_integer(inputReg.bank);
                currAddr := to_integer(inputReg.addr);
                a10Flag  := logic_to_bool(inputReg.addr(10));

                -- decrement counters and traverse states
                for i in 0 to BANK_COUNT - 1 loop
                    if bankCounters(i).active then
                        if bankCounters(i).counter = 0 then
                            banks(i).state <= bank_next_state(banks(i).state);

                            if banks(i).state = ActiveIdle then
                                bankCounters(i).counter := get_delay(ActiveIdle, bank_next_state(banks(i).state));
                            else
                                bankCounters(i).active := false;
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
                        assert banks(currBank).state = Idle
                        report "Bank " & natural'image(currBank) & " is not in Idle state"
                        severity error;

                        -- There is a delay between Active commands
                        assert activeCounter = 0
                        report "Cannot issue another Active command right after last one (not enough time passed)"
                        severity error;

                        banks(currBank).state          <= Activating;
                        banks(currBank).row            <= currAddr;
                        bankCounters(currBank).counter := get_delay(Activating, bank_next_state(Activating));
                        activeCounter                  := ACTIVE_TO_ACTIVE;

                    -- close activated row (if idle does nothing)
                    -- precharge(bank_addr, a10)
                    when Precharge =>
                        assert ctrl.state /= AccessingModeReg
                        report "Controller is not in valid state to Activate row"
                        severity error;

                        if a10Flag then -- precharge all banks
                            for i in 0 to BANK_COUNT - 1 loop
                                assert banks(i).state = ActiveIdle or banks(i).state = Idle
                                report "All banks are not in Idle or Active state"
                                severity error;

                                assert (not ctrl.autoPrecharge) or (ctrl.state /= WriteBurst and ctrl.state /= ReadBurst)
                                report "Cannot Precharge all banks while doing a Burst with Auto Precharge enabled"
                                severity error;

                                -- if banks is idle Precharge acts as NOP
                                if (banks(i).state = ActiveIdle) then
                                    banks(i).state          <= Precharging;
                                    bankCounters(i).counter := get_delay(Precharging, bank_next_state(Precharging));
                                end if;
                            end loop;
                        else            -- precharge only selected bank
                            assert banks(currBank).state = ActiveIdle or banks(currBank).state = Idle
                            report "Bank " & natural'image(currBank) & " is not in Idle or Active state"
                            severity error;

                            assert (not ctrl.autoPrecharge) or (ctrl.state /= WriteBurst and ctrl.state /= ReadBurst) or ctrl.currBank /= currBank
                            report "Cannot Precharge bank " & natural'image(currBank) & " while doing a Burst with Auto Precharge enabled"
                            severity error;

                            -- if banks is idle Precharge acts as NOP
                            if (banks(currBank).state = ActiveIdle) then
                                banks(currBank).state          <= Precharging;
                                bankCounters(currBank).counter := get_delay(Precharging, bank_next_state(Precharging));
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

                            banks(i).state          <= Refreshing;
                            bankCounters(i).counter := get_delay(Refreshing, bank_next_state(Refreshing));
                        end loop;
                        null;

                    -- of importance when autoPrecharge is true
                    when Read | Write =>
                        if ctrl.autoPrecharge and (ctrl.state = ReadBurst or ctrl.state = WriteBurst) then
                            -- sanity checks
                            assert banks(ctrl.currBank).state = ActiveRecharging or banks(ctrl.currBank).state = ActiveIdle
                            report "Bank " & natural'image(ctrl.currBank) & "is not currently in Active state, cannot start Auto Precharge"
                            severity error;

                            assert currBank /= ctrl.currBank
                            report "Cannot truncate a Burst with Auto Precharge enabled"
                            severity error;

                            banks(ctrl.currBank).state          <= Precharging;
                            bankCounters(ctrl.currBank).counter := get_delay(Precharging, bank_next_state(Precharging));
                        end if;

                    when BurstTerminate =>
                        assert not ctrl.autoPrecharge
                        report "Cannot truncate a Burst with Auto Precharge enabled"
                        severity error;

                    -- ingnore NoOp, CmdInhibit, LoadModeReg, CmdError
                    when NoOp | CmdInhibit | LoadModeReg | CmdError =>
                        null;
                end case;

            end if;
        end process bankCtrl;

        -- main controlling process
        mainCtrl : process
            type Mem_Row_T is array (0 to 2**COL_WIDTH - 1) of bit_vector(DATA_WIDTH - 1 downto 0);
            type Mem_Row_Ptr_T is access Mem_Row_T;
            type Mem_Bank_T is array (0 to 2**ADDR_WIDTH - 1) of Mem_Row_Ptr_T;
            type Mem_Bank_Array_T is array (Bank_Range_T) of Mem_Bank_T;

            -- memory data storage
            variable bankData : Mem_Bank_Array_T;

            procedure init_mem(bank : in Bank_Range_T; row : in Row_Range_T) is
            begin
                if bankData(bank)(row) = NULL then
                    bankData(bank)(row) := new Mem_Row_T;
                    for col in Col_Range_T loop
                        bankData(bank)(row)(col) := (others => '0');
                    end loop;
                end if;
            end procedure init_mem;

            procedure write_mem(bank : in Bank_Range_T; row : in Row_Range_T; col : in Col_Range_T; data : in Mem_Data_T; dqm : in std_logic_vector(1 downto 0)) is
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

            impure function read_mem(bank : in Bank_Range_T; row : in Row_Range_T; col : in Col_Range_T; dqm : in std_logic_vector(1 downto 0)) return Mem_Data_T is
                variable tmpData : Mem_Data_T := (others => '0');
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

            pure function validate_mode_reg(val : std_logic_vector(DATA_WIDTH - 1 downto 0)) return boolean is
                variable burstLength    : std_logic_vector(2 downto 0) := val(2 downto 0);
                variable burstType      : std_logic                    := val(3);
                variable latencyMode    : std_logic_vector(2 downto 0) := val(6 downto 4);
                variable operatingMode  : std_logic_vector(1 downto 0) := val(8 downto 7);
                variable writeBurstMode : std_logic                    := val(9);
                variable reserved       : std_logic_vector(1 downto 0) := val(11 downto 10);
                variable isValid        : boolean                      := true;
            begin
                isValid := isValid and (burstLength = "000" or burstLength = "001" or burstLength = "010" or burstLength = "011" or burstLength = "111");
                isValid := isValid and (burstType = '1' or burstType = '0');
                isValid := isValid and not (burstLength = "111" and burstType = '1');
                isValid := isValid and (latencyMode = "010" or latencyMode = "011");
                isValid := isValid and operatingMode = "00";
                isValid := isValid and (writeBurstMode = '1' or writeBurstMode = '0');
                isValid := isValid and reserved = "00";
                return isValid;
            end function validate_mode_reg;

            -- tmp
            variable loadDone : boolean := false;

            -- state variables
            variable currCol      : Col_T                               := 0;
            variable burstCounter : natural range 0 to 2**COL_WIDTH - 1 := 0;

            impure function get_curr_col return Col_Range_T is
                constant COL_END : natural := 2**COL_WIDTH - 1;
                variable tmp     : natural;
            begin
                tmp := currCol + burstCounter;
                -- wrap the burst around column end
                while tmp > COL_END loop
                    tmp := tmp - COL_END;
                end loop;
                assert tmp >= 0;
                return tmp;
            end function get_curr_col;

            -- helper variables
            variable bankPtr                        : Bank_T     := 0;
            variable addrPtr                        : Row_T      := 0;
            variable a10Flag                        : boolean    := false;
            variable data                           : Mem_Data_T := (others => 'Z');
            variable isPrechargingAll, isRefreshing : boolean    := false;
        begin
            if LOAD_FROM_FILE and not loadDone then
                report "Reading memory array from file. This operation may take several minutes. Please wait..." severity note;
                while not endfile(file_load) loop
                    readline(file_load, l);
                    read(l, ch);
                    if (ch /= 'S') or (ch /= 's') then
                        hread(l, rectype);
                        hread(l, reclen);
                        recaddr := (others => '0');

                        case rectype is
                            when "0001" =>
                                hread(l, recaddr(15 downto 0));
                            when "0010" =>
                                hread(l, recaddr(23 downto 0));
                            when "0011" =>
                                hread(l, recaddr);
                                recaddr(31 downto 24) := (others => '0');
                            when others => next;
                        end case;

                        if L.all'length * 4 < recdata'length then
                            hread(l, recdata(0 to L.all'length * 4 - 1));
                        else
                            hread(l, recdata);
                        end if;

                        if index < 32 then
                            currBank := to_integer(unsigned(recaddr(25 downto 24)));
                            currRow  := to_integer(unsigned(recaddr(23 downto 11)));
                            currCol  := to_integer(unsigned(recaddr(10 downto 2)));
                            initMem(currBank, currRow);
                            for i in 0 to 3 loop
                                bankData(currBank + i)(currRow)(currCol) := (inUse => true, data => recdata(i * 32 + index to i * 32 + index + 15));
                            end loop;
                        else
                            currBank := to_integer(unsigned(recaddr(26 downto 25)));
                            currRow  := to_integer(unsigned(recaddr(24 downto 12)));
                            currCol  := to_integer(unsigned(recaddr(11 downto 3)));
                            initMem(currBank, currRow);
                            for i in 0 to 3 loop
                                bankData(currBank)(currRow)(currCol + i) := (inUse => true, data => recdata(i * 64 + index - 32 to i * 64 + index - 32 + 15));
                            end loop;
                        end if;
                    end if;
                end loop;
            end if;

            if DUMP_TO_FILE and simEnded then
                for bank in Bank_Range_T loop
                    for row in Row_Range_T loop
                        if bankData(bank)(row) = NULL then

                        else
                            for col in Col_Range_T loop

                            end loop;
                        end if;
                    end loop;
                end loop;
            end if;

            wait until rising_edge(clkInternal);
            if rising_edge(clkInternal) then
                -- helper variables
                bankPtr := to_integer(inputReg.bank);
                addrPtr := to_integer(inputReg.addr);
                a10Flag := logic_to_bool(inputReg.addr(10));
                data    := inputReg.data;

                -- set additional state flags
                isPrechargingAll := true;
                isRefreshing     := true;
                for i in 0 to BANK_COUNT - 1 loop
                    isPrechargingAll := isPrechargingAll and banks(i).state = Precharging;
                    isRefreshing     := isRefreshing and banks(i).state = Refreshing;
                end loop;

                -- schedule waiting operations
                if ctrl.state = AccessingModeReg then
                    ctrl.state <= Idle;
                end if;

                -- read/write operations
                if ctrl.state = ReadBurst or ctrl.state = WriteBurst then
                    if ctrl.state = ReadBurst then
                        -- FIXME: CL latency
                        -- FIXME: add hold and setup time requirements
                        dataOutPipeline(latencyMode - 2) <= read_mem(ctrl.currBank, banks(ctrl.currBank).row, get_curr_col, inputReg.dqm);
                    else
                        write_mem(ctrl.currBank, banks(ctrl.currBank).row, get_curr_col, inputReg.data, inputReg.dqm);
                    end if;
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

                        currCol      := addrPtr;
                        burstCounter := 0;

                    when BurstTerminate =>
                        assert ctrl.state = WriteBurst or ctrl.state = ReadBurst
                        report "Controller currently not doing a burst write/read"
                        severity warning;

                        assert not ctrl.autoPrecharge
                        report "Cannot terminate Burst with Auto Precharge enabled"
                        severity error;

                        ctrl.state <= Idle;

                        currCol      := 0;
                        burstCounter := 0;

                    when Precharge =>
                        assert (not ctrl.autoPrecharge) or ctrl.currBank /= bankPtr
                        report "Cannot truncate burst with Auto Precharge enabled using Precharge"
                        severity error;

                        -- if Precharge all or Precharge current bank
                        if ctrl.state = WriteBurst or ctrl.state = ReadBurst then
                            if a10Flag or bankPtr = ctrl.currBank then
                                -- end write/read burst
                                ctrl.state   <= Idle;
                                burstCounter := 0;
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

                        ctrl.state <= AccessingModeReg;
                        modeReg    <= data;

                    -- Active does not terminate read/write burst
                    -- Refresh handled by bank controller
                    when Refresh | Active =>
                        null;

                    when CmdError =>
                        report "Cannot decode command"
                        severity error;

                end case;

            end if;
        end process mainCtrl;
    end block ctrlBlock;
end architecture model;
