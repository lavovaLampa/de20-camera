library ieee;
use ieee.std_logic_1164.all;

entity reset_ctrl_tb is
    constant CLK_PERIOD : time := 20 ns; -- 50 MHz
end reset_ctrl_tb;

architecture tb of reset_ctrl_tb is
    -- clock & reset
    signal clkIn      : std_logic;
    signal rstAsyncIn : std_logic;

    -- reset in
    signal memInitialized       : boolean;
    signal ccdConfigurationDone : boolean;

    -- reset out
    signal dataCtrlResetOut         : std_logic;
    signal ccdResetOut              : std_logic;
    signal ccdConfigurationResetOut : std_logic;
    signal resetOut                 : std_logic;

    -- testbench signals
    signal tbClk      : std_logic := '0';
    signal tbSimEnded : std_logic := '0';
begin

    dut : entity work.reset_ctrl
        port map(
            clkIn                    => clkIn,
            rstAsyncIn               => rstAsyncIn,
            memInitialized           => memInitialized,
            ccdConfigurationDone     => ccdConfigurationDone,
            dataCtrlResetOut         => dataCtrlResetOut,
            ccdResetOut              => ccdResetOut,
            ccdConfigurationResetOut => ccdConfigurationResetOut,
            resetOut                 => resetOut
        );

    -- Clock generation
    tbClk <= not tbClk after CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';
    clkIn <= tbClk;

    stimuli : process
    begin
        -- EDIT Adapt initialization as needed
        memInitialized       <= false;
        ccdConfigurationDone <= false;

        -- Reset generation
        -- EDIT: Check that rstAsyncIn is really your reset signal
        rstAsyncIn <= '1';
        wait for 5 * CLK_PERIOD;
        rstAsyncIn <= '0';
        wait for 5 * CLK_PERIOD;

        memInitialized       <= true;
        ccdConfigurationDone <= true;

        wait for 5000 * CLK_PERIOD;

        -- Stop the clock and hence terminate the simulation
        tbSimEnded <= '1';
        wait;
    end process;

end tb;
