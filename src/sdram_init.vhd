library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use work.sdram_pkg_new.all;

entity sdram_init is
    generic(
        -- FIXME: set sensible defaults
        CLK_PERIOD  : time                          := 7.5 ns;
        INIT_DELAY  : time                          := 200 us;
        tRP         : time                          := 15 ns;
        tRC         : time                          := 60 ns;
        tMRD_CYCLES : natural                       := 2;
        MODE_REG    : std_logic_vector(15 downto 0) := (others => '0')
    );
    port(
        clkIn, rstAsyncIn   : in  std_logic;
        clkStableIn         : in  std_logic;
        addrOut             : out unsigned(11 downto 0);
        dataOut             : out std_logic_vector(15 downto 0);
        --        bankSelectOut       : out unsigned(1 downto 0);
        chipSelectNegOut    : out std_logic := '0';
        rowAddrStrobeNegOut : out std_logic := '1';
        colAddrStrobeNegOut : out std_logic := '1';
        writeEnableNegOut   : out std_logic := '1';
        clkEnableOut        : out std_logic := '0';
        doneOut             : out boolean   := false
    );
    constant tRP_CYCLES          : natural := (tRP / CLK_PERIOD) + 1;
    constant tRC_CYCLES          : natural := (tRC / CLK_PERIOD) + 1;
    constant STABLE_POWER_CYCLES : natural := 200;
    constant INIT_WAIT_CYCLES    : natural := (INIT_DELAY / CLK_PERIOD) + 1;
    constant COUNTER_LIMIT       : natural := maximum(INIT_WAIT_CYCLES, 2**12);
    type Init_State_T is (WaitForStablePower, WaitForStableClock, InitDelay, PrechargeAll, AutoRefresh, LoadModeReg, Done);
end entity sdram_init;

architecture RTL of sdram_init is
    -- reg
    signal currState : Init_State_T := InitDelay;
    signal currCmd   : Cmd_T        := NoOp;

    -- wire
    signal cmdSelectAggregate : std_logic_vector(3 downto 0);
begin
    cmdSelectAggregate <= encode_cmd(currCmd);

    -- decode internal representation to control signals
    chipSelectNegOut    <= cmdSelectAggregate(3);
    rowAddrStrobeNegOut <= cmdSelectAggregate(2);
    colAddrStrobeNegOut <= cmdSelectAggregate(1);
    writeEnableNegOut   <= cmdSelectAggregate(0);

    initProc : process(clkIn, rstAsyncIn)
        variable counter             : natural range 0 to COUNTER_LIMIT := 0;
        variable auxCounter          : natural range 0 to 10            := 0;
        variable refreshCycleCounter : natural range 0 to tRC_CYCLES    := 0;
        variable cmdSent             : boolean                          := false;

        procedure next_state(nextState : in Init_State_T) is
        begin
            currState  <= nextState;
            counter    := 0;
            auxCounter := 0;
        end procedure next_state;

    begin
        if rstAsyncIn = '1' then
            addrOut             <= (others => '0');
            --            bankSelectOut       <= (others => '0');
            chipSelectNegOut    <= '0';
            rowAddrStrobeNegOut <= '1';
            colAddrStrobeNegOut <= '1';
            writeEnableNegOut   <= '1';
            doneOut             <= false;
            clkEnableOut        <= '0';
            dataOut             <= (others => 'Z');

            -- internal signals/variables
            currState  <= InitDelay;
            counter    := 0;
            auxCounter := 0;
            cmdSent    := false;
        elsif rising_edge(clkIn) then
            case currState is
                when WaitForStablePower =>
                    if counter >= STABLE_POWER_CYCLES then
                        next_state(WaitForStableClock);
                    end if;

                when WaitForStableClock =>
                    if clkStableIn = '1' then
                        next_state(WaitForStablePower);
                        clkEnableOut <= '1';
                    end if;

                when InitDelay =>
                    if counter >= INIT_WAIT_CYCLES then
                        next_state(PrechargeAll);
                    end if;

                when PrechargeAll =>
                    if cmdSent then
                        currCmd <= NoOp;
                        if counter > tRP_CYCLES then
                            next_state(AutoRefresh);
                        end if;
                    else
                        currCmd <= Precharge;
                        -- enable a10Flag -> Precharge All banks
                        addrOut <= b"0000_0000_0010";
                        cmdSent := true;
                    end if;

                when AutoRefresh =>
                    if cmdSent then
                        currCmd <= NoOp;

                        if refreshCycleCounter > tRC_CYCLES then
                            cmdSent             := false;
                            refreshCycleCounter := 0;
                        else
                            refreshCycleCounter := refreshCycleCounter + 1;
                        end if;
                    else
                        if auxCounter > 1 then
                            next_state(LoadModeReg);
                        else
                            currCmd <= Refresh;
                            cmdSent := true;

                            if counter > 2**12 then
                                counter    := 0;
                                auxCounter := auxCounter + 1;
                            else
                                counter := counter + 1;
                            end if;
                        end if;
                    end if;

                when LoadModeReg =>
                    if cmdSent then
                        currCmd <= NoOp;
                        if counter > tMRD_CYCLES + 2 then
                            next_state(Done);
                        end if;
                    else
                        currCmd <= LoadModeReg;
                        dataOut <= MODE_REG;
                        cmdSent := true;
                    end if;

                when Done =>
                    doneOut <= true;
            end case;

            counter := counter + 1;
        end if;
    end process initProc;
end architecture RTL;
