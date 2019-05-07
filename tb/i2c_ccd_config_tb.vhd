library ieee;
use ieee.std_logic_1164.all;
use work.i2c_pkg.all;
use work.common_pkg.all;

entity tb_i2c_ccd_config is
    constant CLK_PERIOD : time := 20 ns; -- 50MHz clock
end tb_i2c_ccd_config;

architecture tb of tb_i2c_ccd_config is
    signal clkIn, rstAsyncIn : std_logic;
    signal enableIn          : boolean;
    signal doneOut           : boolean;

    -- i2c signals
    signal sClkOut : std_logic;
    signal sDataIO : std_logic;

    -- TB signals
    signal tbClk      : std_logic := '0';
    signal tbSimEnded : std_logic := '0';

begin

    dut : entity work.i2c_ccd_config
        port map(
            clkIn      => clkIn,
            rstAsyncIn => rstAsyncIn,
            enableIn   => enableIn,
            doneOut    => doneOut,
            sClkOut    => sClkOut,
            sDataIO    => sDataIO
        );

    -- Clock generation
    tbClk <= not tbClk after CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';
    clkIn <= tbClk;

    stimuli : process
    begin
        enableIn <= false;

        -- Reset generation
        rstAsyncIn <= '1';
        wait for 10 * CLK_PERIOD;
        rstAsyncIn <= '0';
        wait for 10 * CLK_PERIOD;

        -- EDIT Add stimuli here
        wait for 100 * CLK_PERIOD;

        -- Stop the clock and hence terminate the simulation
        tbSimEnded <= '1';
        wait;
    end process;

    checkProc : process(sClkOut, sDataIO)
    begin

    end process checkProc;

end tb;
