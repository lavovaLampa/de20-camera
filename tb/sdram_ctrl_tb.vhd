-- Testbench automatically generated online
-- at http://vhdl.lapinoo.net
-- Generation date : 22.7.2019 12:25:07 GMT

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.sdram_pkg.all;
use work.sdram_ctrl_pkg.all;

entity sdram_ctrl_tb is
    constant CLK_PERIOD : time := 7.5 ns;

    constant ROW_MAX         : natural := 1400;
    constant READ_BURST_LEN  : natural := 4;
    constant WRITE_BURST_LEN : natural := 4;
end sdram_ctrl_tb;

architecture tb of sdram_ctrl_tb is
    -- common signals
    signal clkIn      : std_logic;
    signal rstAsyncIn : std_logic;

    -- ctrl I/O
    signal addrIn       : Ctrl_Addr_T;
    signal cmdIn        : Ctrl_Cmd_T;
    signal cmdReadyOut  : boolean;
    signal dataReadyOut : boolean;
    signal dataIn       : Data_T;
    signal dataOut      : Data_T;

    -- SDRAM I/O
    signal sdramClkOut : std_logic;
    signal sdramDataIo : Data_T;
    signal sdramOut    : Mem_IO_R;

    -- SDRAM model debug signals
    signal isInitialized : boolean;
    signal simEnded      : boolean;

    -- testbench signals
    signal tbClk      : std_logic := '0';
    signal tbSimEnded : std_logic := '0';

begin
    dut : entity work.sdram_ctrl_top
        generic map(
            ROW_MAX         => ROW_MAX,
            READ_BURST_LEN  => READ_BURST_LEN,
            WRITE_BURST_LEN => WRITE_BURST_LEN
        )
        port map(
            clkIn        => clkIn,
            rstAsyncIn   => rstAsyncIn,
            addrIn       => addrIn,
            cmdIn        => cmdIn,
            cmdReadyOut  => cmdReadyOut,
            dataReadyOut => dataReadyOut,
            dataIn       => dataIn,
            dataOut      => dataOut,
            sdramClkOut  => sdramClkOut,
            sdramDataIo  => sdramDataIo,
            sdramOut     => sdramOut
        );

    sdramModel : entity work.sdram_model
        generic map(
            LOAD_FROM_FILE => false,
            DUMP_TO_FILE   => false
        )
        port map(
            clkIn              => clkIn,
            addrIn             => sdramOut.addr,
            dataIo             => sdramDataIo,
            bankSelectIn       => sdramOut.bankSelect,
            clkEnableIn        => sdramOut.clkEnable,
            chipSelectNegIn    => sdramOut.cmdAggregate.chipSelectNeg,
            rowAddrStrobeNegIn => sdramOut.cmdAggregate.rowAddrStrobeNeg,
            colAddrStrobeNegIn => sdramOut.cmdAggregate.colAddrStrobeNeg,
            writeEnableNegIn   => sdramOut.cmdAggregate.writeEnableNeg,
            dqmIn              => sdramOut.dqm,
            -- debug signals
            isInitializedOut   => isInitialized,
            simEndedIn         => simEnded
        );

    -- Clock generation
    tbClk <= not tbClk after CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';

    -- EDIT: Check that clkIn is really your main clock signal
    clkIn <= tbClk;

    stimuli : process
    begin
        -- EDIT Adapt initialization as needed
        addrIn <= (others => '0');
        cmdIn  <= NoOp;
        dataIn <= (others => '0');

        -- Reset generation
        -- EDIT: Check that rstAsyncIn is really your reset signal
        rstAsyncIn <= '1';
        wait for 10 ns;
        rstAsyncIn <= '0';
        wait for 10 ns;

        wait until isInitialized;

        -- Stop the clock and hence terminate the simulation
        tbSimEnded <= '1';
        wait;
    end process;

    readProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then

        elsif rising_edge(clkIn) then

        end if;
    end process readProc;

    writeProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then

        elsif rising_edge(clkIn) then

        end if;
    end process writeProc;
end tb;
