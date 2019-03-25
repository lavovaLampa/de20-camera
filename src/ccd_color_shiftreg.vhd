library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ccd_pkg.all;

entity ccd_color_shiftreg is
    port(
        clkIn, rstAsyncIn : in  std_logic;
        dataIn            : in  Pixel_Data;
        pixelsOut         : out Pixel_Matrix
    );

    type Color_ShiftReg_Array is array (((2 * IMG_CONSTS.width) + 2) downto 0) of Pixel_Data;
end entity ccd_color_shiftreg;

architecture RTL of ccd_color_shiftreg is
    signal shiftReg : Color_ShiftReg_Array := (others => (others => '0'));

begin
    shiftProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            shiftReg <= (others => (others => '0'));
        elsif rising_edge(clkIn) then
            shiftReg(shiftReg'high - 1  downto 0) <= shiftReg(shiftReg'high downto 1);
            shiftReg(shiftReg'high) <= dataIn;
        end if;
    end process shiftProc;

    -- TODO: nutne premysliet
    pixelsOut(0, 2 downto 0) <= shiftReg();
    pixelsOut(1, 2 downto 0) <= shiftReg();
    pixelsOut(2, 2 downto 0) <= shiftReg();
end architecture RTL;
