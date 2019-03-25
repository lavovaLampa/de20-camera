library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ccd_pkg.all;

entity ccd_bayer_to_rgb_shiftreg is
    port(
        clkIn, rstAsyncIn : in  std_logic;
        enableIn          : in  boolean;
        dataIn            : in  Pixel_Data;
        dataOut           : out Pixel_Data := X"000"
    );
    type Green_ShiftReg_Array is array ((IMG_PARAMS.width + 1) downto 0) of Pixel_Data;
end entity ccd_bayer_to_rgb_shiftreg;

architecture RTL of ccd_bayer_to_rgb_shiftreg is
    signal shiftReg : Green_ShiftReg_Array := (others => (others => '0'));
begin
    shiftProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            shiftReg <= (others => (others => '0'));
        elsif rising_edge(clkIn) then
            if enableIn then
                shiftReg(shiftReg'high downto 1) <= shiftReg(shiftReg'high - 1 downto 0);
                shiftReg(0)                      <= dataIn;
            end if;
        end if;
    end process shiftProc;

    dataOut <= resize((widen(shiftReg(0)) + widen(shiftReg(shiftReg'high))) / 2, dataOut'length);
end architecture RTL;
