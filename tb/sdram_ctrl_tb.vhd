-- Testbench automatically generated online
-- at http://vhdl.lapinoo.net
-- Generation date : 15.7.2019 01:11:37 GMT

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.sdram_pkg.all;
use work.sdram_ctrl_pkg.all;

entity sdram_ctrl_tb is
    constant CLK_PERIOD : time := 7.5 ns;
end sdram_ctrl_tb;

architecture tb of sdram_ctrl_tb is

    -- entity signals
    signal clkIn        : std_logic;
    signal rstAsyncIn   : std_logic;
    signal addrIn       : Ctrl_Addr_T;
    signal cmdIn        : Ctrl_Cmd_T;
    signal cmdReadyOut  : boolean;
    signal dataReadyOut : boolean;
    signal dataIn       : Data_T;
    signal dataOut      : Data_T;
    signal clkStable    : std_logic;

    -- testbench internal signals
    signal tbClk      : std_logic := '0';
    signal tbSimEnded : std_logic := '0';

begin

    sdramCtrl : entity work.sdram_ctrl
        generic map(
            ROW_MAX         => 1400,
            READ_BURST_LEN  => 5,
            WRITE_BURST_LEN => 4
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
            clkStableIn  => clkStable
        );

    -- Clock generation
    tbClk <= not tbClk after CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';

    -- EDIT: Check that clkIn is really your main clock signal
    clkIn <= tbClk;

    stimuli : process
    begin
        -- EDIT Adapt initialization as needed
        --        addrIn <= '0';
        --        cmdIn  <= '0';
        --        dataIn <= '0';

        -- Reset generation
        -- EDIT: Check that rstAsyncIn is really your reset signal

        clkStable <= '0';
        dataIn    <= (others => '1');

        rstAsyncIn <= '1';
        wait for 20 ns;
        rstAsyncIn <= '0';
        wait for 20 ns;

        clkStable <= '1' after 100 ns;

        wait until cmdReadyOut;
        cmdIn  <= Read;
        addrIn <= B"0000_0000_0011_01";

        wait for 200 ns;

        -- Stop the clock and hence terminate the simulation
        tbSimEnded <= '1';
        wait;
    end process;

end tb;
