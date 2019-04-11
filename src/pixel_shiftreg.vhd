library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ccd_pkg.all;

entity pixel_shiftreg is
    port(
        clkIn, rstAsyncIn : in  std_logic;
        dataIn            : in  Pixel_Data;
        enableIn          : in  boolean;
        pixelsOut         : out Pixel_Matrix
    );

    type Color_ShiftReg_Array is array (SHIFT_LEN - 1 downto 0) of Pixel_Data;
    alias WIDTH is IMG_CONSTS.width;
end entity pixel_shiftreg;

architecture RTL of pixel_shiftreg is
    signal shiftReg : Color_ShiftReg_Array := (others => (others => '0'));

begin
    shiftProc : process(clkIn, rstAsyncIn, enableIn)
    begin
        if rstAsyncIn = '1' then
            shiftReg <= (others => (others => '0'));
        elsif rising_edge(clkIn) and enableIn then
            shiftReg(shiftReg'high - 1 downto 0) <= shiftReg(shiftReg'high downto 1);
            shiftReg(shiftReg'high)              <= dataIn;
        end if;
    end process shiftProc;

    -- TODO: nutne premysliet
    pixelsOut(0, 0) <= shiftReg(2);
    pixelsOut(0, 1) <= shiftReg(1);
    pixelsOut(0, 2) <= shiftReg(0);
    pixelsOut(1, 0) <= shiftReg(WIDTH + 2);
    pixelsOut(1, 1) <= shiftReg(WIDTH + 1);
    pixelsOut(1, 2) <= shiftReg(WIDTH);
    pixelsOut(2, 0) <= shiftReg((2 * WIDTH) + 2);
    pixelsOut(2, 1) <= shiftReg((2 * WIDTH) + 1);
    pixelsOut(2, 2) <= shiftReg(2 * WIDTH);
end architecture RTL;
