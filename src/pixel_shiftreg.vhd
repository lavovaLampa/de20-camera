library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.img_pkg.Pixel_Data_T;
use work.img_pkg.Pixel_Matrix_T;
use work.ccd_pkg.CCD_CONFIGURATION;

entity pixel_shiftreg is
    generic(
        constant SHIFT_LEN  : natural := (CCD_CONFIGURATION.width * 2) + 3;
        constant LINE_WIDTH : natural := CCD_CONFIGURATION.width
    );
    port(
        clkIn, rstAsyncIn : in  std_logic;
        dataIn            : in  Pixel_Data_T;
        enableIn          : in  boolean;
        pixelsOut         : out Pixel_Matrix_T
    );

    type Pixel_Shift_Array_T is array (SHIFT_LEN - 1 downto 0) of Pixel_Data_T;
end entity pixel_shiftreg;

architecture RTL of pixel_shiftreg is
    signal shiftReg : Pixel_Shift_Array_T := (others => (others => '0'));

begin
    shiftProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            shiftReg <= (others => (others => '0'));
        elsif rising_edge(clkIn) then
            if enableIn then
                shiftReg(shiftReg'high - 1 downto 0) <= shiftReg(shiftReg'high downto 1);
                shiftReg(shiftReg'high)              <= dataIn;
            end if;
        end if;
    end process shiftProc;

    pixelsOut(0, 0) <= shiftReg(2);
    pixelsOut(0, 1) <= shiftReg(1);
    pixelsOut(0, 2) <= shiftReg(0);
    pixelsOut(1, 0) <= shiftReg(LINE_WIDTH + 2);
    pixelsOut(1, 1) <= shiftReg(LINE_WIDTH + 1);
    pixelsOut(1, 2) <= shiftReg(LINE_WIDTH);
    pixelsOut(2, 0) <= shiftReg((2 * LINE_WIDTH) + 2);
    pixelsOut(2, 1) <= shiftReg((2 * LINE_WIDTH) + 1);
    pixelsOut(2, 2) <= shiftReg(2 * LINE_WIDTH);
end architecture RTL;
