library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ccd_pkg.all;

entity ccd_color_shiftreg is
    generic(
        regColor : Pixel_Color := Red
    );
    port(
        clkIn, rstAsyncIn : in  std_logic;
        dataIn            : in  Ccd_Pixel_Data;
        enableIn          : in  boolean;
        dataOut           : out Ccd_Color_Matrix := (others => (others => '0'))
    );
    type Color_ShiftReg_Array is array (((2 * IMG_PARAMS.width) + KERNEL_PARAMS.dim) downto 0) of Ccd_Pixel_Data;
end entity ccd_color_shiftreg;

architecture RTL of ccd_color_shiftreg is
    signal shiftReg : Color_ShiftReg_Array := (others => (others => '0'));

begin
    shiftProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            shiftReg <= (others => (others => '0'));
        elsif rising_edge(clkIn) then
            if enableIn then
                shiftReg(shiftReg'high downto 1) <= shiftReg(shiftReg'high - 1 downto 0);
                shiftReg(0) <= dataIn;
            end if;
        end if;
    end process shiftProc;

    dataOut(8 downto 6) <= (shiftReg(2), shiftReg(1), shiftReg(0));
    dataOut(5 downto 3) <= (shiftReg(IMG_PARAMS.width + 2), shiftReg(IMG_PARAMS.width + 1), shiftReg(IMG_PARAMS.width));
    dataOut(2 downto 0) <= (shiftReg(shiftReg'high), shiftReg(shiftReg'high - 1), shiftReg(shiftReg'high - 2));

end architecture RTL;
