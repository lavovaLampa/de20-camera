library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ccd_pkg.all;

entity ccd_color_demux is
    port(
        dataIn  : in  Color_Data_Mux;
        colorIn : in  Pixel_Color;
        dataOut : out Ccd_Pixel_Data := X"000"
    );
end entity ccd_color_demux;

architecture RTL of ccd_color_demux is
begin
    with colorIn select dataOut <=
        dataIn.red when Red,
        dataIn.green when Green,
        dataIn.blue when Blue;
end architecture RTL;
