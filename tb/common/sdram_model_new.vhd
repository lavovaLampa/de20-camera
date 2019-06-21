library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.sdram_pkg_new.all;

entity sdram_model_new is
    generic(
        -- timings
        -- FIXME: fix this
        tCAS       : time    := 0 ns;
        tAC        : time    := 6.0 ns; -- access time from clk
        tHZ        : time    := 7.0 ns; -- output high impedance time
        tOH        : time    := 2.7 ns; -- output data hold time
        tMRD       : natural := 2;      -- clock cycles between write to mode register and next command
        tRAS       : time    := 44.0 ns; -- row address strobe (active to precharge)
        tRC        : time    := 66.0 ns; -- row cycle (ref to ref / activate to activate)
        tRCD       : time    := 20.0 ns; -- RAS to CAS delay (active command to read/write command delay time)
        tRP        : time    := 20.0 ns; -- row precharge time (min. time between precharging row and activating new one)
        tRRD       : time    := 15.0 ns; -- bank to bank delay time (min. time between successive ACTIVE commands to different banks)
        tWRa       : time    := 7.5 ns; -- A2 Version - Auto precharge mode only (1 Clk + 7.5 ns)
        tWRp       : time    := 15.0 ns; -- A2 Version - Precharge mode only (15 ns)
        -- FIXME: implement 
        tCKA       : time    := 0 ns;   -- CKE to CLK recovery delay time

        tAH        : time    := 0.8 ns; -- address hold time
        tAS        : time    := 1.5 ns; -- address setup time
        tCH        : time    := 2.5 ns; -- clk high level width
        tCL        : time    := 2.5 ns; -- clk low level width
        tCK        : time    := 10.0 ns; -- clk cycle time
        tDH        : time    := 0.8 ns; -- input data hold time
        tDS        : time    := 1.5 ns; -- input data setup time
        tCKH       : time    := 0.8 ns; -- CKE hold time (clk enable?)
        tCKS       : time    := 1.5 ns; -- CKE setup time
        tCMH       : time    := 0.8 ns; -- command hold time
        tCMS       : time    := 1.5 ns; -- command setup time

        tREF       : time    := 64 ms;  -- refresh cycle time (4096)

        -- period of clkIn
        CLK_PERIOD : time    := 8 ns;
        -- ram parameters
        ADDR_WIDTH : natural := 12;
        COL_WIDTH  : natural := 8;
        DATA_WIDTH : natural := 16
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
        -- ctrl/banks state representation (reg)
        signal banks : Bank_Array_T := (others => (state => Idle, row => 0));
        signal ctrl  : Ctrl_State_R;
    begin

        bankCtrl : process(clkInternal)
            -- FIXME: generify
            constant PRE_TO_IDLE                      : natural := 2;
            constant IDLE_TO_ACTIVE_RECHARGING        : natural := 2;
            constant ACTIVE_RECHARGING_TO_ACTIVE_IDLE : natural := 3;
            constant REFRESH_TO_IDLE                  : natural := IDLE_TO_ACTIVE_RECHARGING + ACTIVE_RECHARGING_TO_ACTIVE_IDLE + PRE_TO_IDLE;

            -- store timings
            type Bank_Counters_T is array (0 to BANK_COUNT - 1) of integer;
            variable bankCounter : Bank_Counters_T;

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

                case inputReg.cmd is
                    -- open row for reading/writing
                    -- active(row_addr, bank_addr)
                    when Active =>
                        assert ctrl.state /= AccessingModeReg
                        report "Controller is not in valid state to Activate row"
                        severity error;

                        assert banks(currBank).state = Idle
                        report "Bank " & natural'image(currBank) & " is not in Idle state"
                        severity error;

                        banks(currBank).state <= Activating;
                        banks(currBank).row   <= currAddr;
                        bankCounter(currBank) := IDLE_TO_ACTIVE_RECHARGING;

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

                                -- if banks is idle Precharge acts as NOP
                                if (banks(i).state = ActiveIdle) then
                                    banks(i).state <= Precharging;
                                    bankCounter(i) := PRE_TO_IDLE;
                                end if;
                            end loop;
                        else            -- precharge only selected bank
                            assert banks(currBank).state = ActiveIdle or banks(currBank).state = Idle
                            report "Bank " & natural'image(currBank) & " is not in Idle or Active state"
                            severity error;

                            -- if banks is idle Precharge acts as NOP
                            if (banks(currBank).state = ActiveIdle) then
                                banks(currBank).state <= Precharging;
                                bankCounter(currBank) := PRE_TO_IDLE;
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

                            banks(i).state <= Refreshing;
                            bankCounter(i) := REFRESH_TO_IDLE;
                        end loop;
                        null;

                    -- of importance when autoPrecharge
                    when Read | Write | BurstTerminate =>
                        if ctrl.autoPrecharge then
                            -- sanity checks
                            assert ctrl.state = ReadBurst or ctrl.state = WriteBurst
                            report "Controller is currently not doing any Read/Write burst, cannot start Auto Precharge"
                            severity error;

                            assert banks(ctrl.currBank).state = ActiveRecharging or banks(ctrl.currBank).state = ActiveIdle
                            report "Bank " & natural'image(ctrl.currBank) & "is not currently in Active state, cannot start Auto Precharge"
                            severity error;

                            banks(ctrl.currBank).state <= Precharging;
                            bankCounter(ctrl.currBank) := PRE_TO_IDLE;
                        end if;

                    -- ingnore NoOp, CmdInhibit, LoadModeReg, CmdError
                    when others =>
                        null;
                end case;

                -- run scheduler (change state after defined cycle count)
                for i in 0 to BANK_COUNT - 1 loop
                    if bankCounter(i) = 0 then
                        bankCounter(i) := -1;
                        banks(i).state <= next_bank_state(banks(i).state);
                    else
                        bankCounter(i) := bankCounter(i) - 1;
                    end if;
                end loop;

            end if;
        end process bankCtrl;

        -- main controlling process
        mainCtrl : process
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

            -- state variables
            variable currCol      : Col_T                               := 0;
            variable burstCounter : natural range 0 to 2**COL_WIDTH - 1 := 0;

            -- helper variables
            variable bankPtr : Bank_T     := 0;
            variable addrPtr : Row_T      := 0;
            variable a10Flag : boolean    := false;
            variable data    : Mem_Data_T := (others => 'Z');
        begin
            if rising_edge(clkInternal) then
                -- helper variables
                bankPtr := to_integer(inputReg.bank);
                addrPtr := to_integer(inputReg.addr);
                a10Flag := logic_to_bool(inputReg.addr(10));
                data    := inputReg.data;

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

                        currCol      := addrPtr;
                        burstCounter := burstLength;

                    when Write =>
                        assert ctrl.state /= AccessingModeReg
                        report "Cannot Read during Load Mode Register operation"
                        severity warning;

                        assert banks(bankPtr).state = ActiveRecharging or banks(bankPtr).state = ActiveIdle
                        report "Selected bank not Active, cannot start Read burst"
                        severity error;

                    when BurstTerminate =>
                        assert ctrl.state = WriteBurst or ctrl.state = ReadBurst
                        report "Controller currently not doint a burst write/read"
                        severity warning;

                        ctrl.state   <= Idle;
                        burstCounter := 0;

                    when Precharge =>
                        null;

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

                    -- TODO: of no interest?
                    when Refresh | Active =>
                        null;

                    when CmdError =>
                        null;

                end case;
            end if;
        end process mainCtrl;

    end block ctrlBlock;
end architecture model;
