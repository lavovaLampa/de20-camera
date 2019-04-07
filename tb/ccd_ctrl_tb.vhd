library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ccd_pkg.all;

entity ccd_ctrl_tb is
    constant CLK_PERIOD : time := 20 ns; -- 50 MHz
end entity ccd_ctrl_tb;

architecture TB of ccd_ctrl_tb is
    signal clk           : std_logic      := '0';
    signal rst           : std_logic      := '1';
    signal frameValid    : std_logic      := '0';
    signal lineValid     : std_logic      := '0';
    signal pixelData     : Ccd_Pixel_Data := X"000";
    signal redOut        : Pixel_Data;
    signal greenOut      : Pixel_Data;
    signal blueOut       : Pixel_Data;
    signal currXOut      : Img_Width_Range;
    signal currYOut      : Img_Height_Range;
    signal pixelValidOut : boolean;
begin
    clk <= not clk after CLK_PERIOD;
    rst <= '0' after CLK_PERIOD;

    uut : entity work.ccd_ctrl
        port map(
            clkIn         => clk,
            rstAsyncIn    => rst,
            frameValidIn  => frameValid,
            lineValidIn   => lineValid,
            pixelDataIn   => pixelData,
            redOut        => redOut,
            greenOut      => greenOut,
            blueOut       => blueOut,
            currXOut      => currXOut,
            currYOut      => currYOut,
            pixelValidOut => pixelValidOut
        );

    testProc : process(clk)
    begin
        if falling_edge(clk) then

        end if;
    end process testProc;

end architecture TB;
