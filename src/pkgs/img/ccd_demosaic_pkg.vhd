library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.img_pkg.PIXEL_WIDTH;

package ccd_demosaic_pkg is
    -- pipeline stage has to be wide enough not to overflow during addition
    constant PIPELINE_SIZE : natural := PIXEL_WIDTH + 2;

    subtype Pixel_Range_T is natural range PIXEL_WIDTH - 1 downto 0;

    -- PIPELINE TYPES
    subtype Pipeline_Pixel is unsigned(PIPELINE_SIZE - 1 downto 0);
    type Stage_Out is array (1 downto 0) of Pipeline_Pixel;
    type Pipeline_Matrix is array (2 downto 0, 2 downto 0) of Pipeline_Pixel;
end package ccd_demosaic_pkg;
