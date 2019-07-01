library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity sdram_init is
    generic(
        CLK_PERIOD : time := 7.5 ns
    );
    port(
        clkIn, rstAsyncIn   : in  std_logic;
        addrOut             : out unsigned(11 downto 0);
        dataOut             : out std_logic_vector(15 downto 0);
        --        bankSelectOut       : out unsigned(1 downto 0);
        chipSelectNegOut    : out std_logic := '0';
        rowAddrStrobeNegOut : out std_logic := '1';
        colAddrStrobeNegOut : out std_logic := '1';
        writeEnableNegOut   : out std_logic := '1';
        initDoneOut         : out boolean   := false
    );
    constant INIT_COUNTER_HIGH : natural := 100 us / CLK_PERIOD;
    type Init_State_T is (InitDelay, PrechargeAll, AutoRefresh, LoadModeReg, FinalNop, Done);
    type Cmd_T is (CmdInhibit, NoOp, Active, Read, Write, BurstTerminate, Precharge, Refresh, LoadModeReg);
end entity sdram_init;

architecture RTL of sdram_init is
    -- reg
    signal currState : Init_State_T := InitDelay;
    signal currCmd   : Cmd_T        := NoOp;

    -- wire
    signal cmdSelectAggregate : std_logic_vector(3 downto 0);
begin
    with currCmd select cmdSelectAggregate <=
        "1---" when CmdInhibit,
        "0111" when NoOp,
        "0011" when Active,
        "0101" when Read,
        "0100" when Write,
        "0110" when BurstTerminate,
        "0010" when Precharge,
        "0001" when Refresh,
        "0000" when LoadModeReg;

    -- decode internal representation to control signals
    chipSelectNegOut    <= cmdSelectAggregate(3);
    rowAddrStrobeNegOut <= cmdSelectAggregate(2);
    colAddrStrobeNegOut <= cmdSelectAggregate(1);
    writeEnableNegOut   <= cmdSelectAggregate(0);

    initProc : process(clkIn, rstAsyncIn)
        variable initCounter    : natural range 0 to INIT_COUNTER_HIGH + 100 := 0;
        variable refreshCounter : natural range 0 to 2**12;

        procedure mem_precharge is
        begin
            currCmd <= Precharge;
            -- precharge all flag
            addrOut <= b"0000_0000_0010";
        end procedure mem_precharge;

        procedure mem_refresh is
        begin
            currCmd <= Refresh;
        end procedure mem_refresh;
    begin
        if rstAsyncIn = '1' then
            addrOut             <= (others => '0');
            --            bankSelectOut       <= (others => '0');
            chipSelectNegOut    <= '0';
            rowAddrStrobeNegOut <= '1';
            colAddrStrobeNegOut <= '1';
            writeEnableNegOut   <= '1';
            initDoneOut         <= false;
            dataOut             <= (others => 'Z');

            -- internal signals/variables
            currState      <= InitDelay;
            initCounter    := 0;
            refreshCounter := 0;
        elsif rising_edge(clkIn) then
            case currState is
                when InitDelay =>
                    if initCounter < INIT_COUNTER_HIGH + 100 then
                        currState <= PrechargeAll;
                    else
                        initCounter := initCounter + 1;
                    end if;

                when PrechargeAll =>
                    mem_precharge;
                    currState <= 

                when AutoRefresh =>
                    null;
                when LoadModeReg =>
                    null;
                when FinalNop =>
                    null;
                when Done =>
                    null;

            end case;
        end if;
    end process initProc;
end architecture RTL;
