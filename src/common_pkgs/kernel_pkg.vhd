library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ccd_pkg.IMG_CONSTS;
use work.ccd_pkg.Internal_Pixel_Color;
use work.ccd_pkg.Pixel_Matrix;

package kernel_pkg is
    alias PIXEL_SIZE is IMG_CONSTS.pixel_data_size;

    -- CONSTANTS
    constant PIPELINE_STAGE_AMOUNT : natural := 4;
    -- necessary to prevent overflow during addition
    constant PIPELINE_SIZE         : natural := PIXEL_SIZE + PIPELINE_STAGE_AMOUNT;
    constant IMG_WIDTH             : natural := IMG_CONSTS.width - 2;
    constant IMG_HEIGHT            : natural := IMG_CONSTS.height - 2;

    constant STAGE1_AMOUNT : natural := 5;
    constant STAGE2_AMOUNT : natural := 3;
    constant STAGE3_AMOUNT : natural := 2;

    -- RANGES
    subtype Pipeline_Pixel_Range is natural range PIPELINE_SIZE - 1 downto 0;

    -- TYPES
    subtype Pipeline_Pixel is signed(Pipeline_Pixel_Range);
    type Kernel_Consts is array (2 downto 0, 2 downto 0) of integer;
    -- constant to divide (prescale) kernel with
    subtype Conv_Prescale is integer range -12 to 12;
    type Matrix_Aggregate is array (Internal_Pixel_Color) of Pixel_Matrix;
    type Stage_Color_Out is array (natural range <>) of Pipeline_Pixel;
    type Stage_Out is array (Internal_Pixel_Color) of Stage_Color_Out;

    -- FUNCTIONS

end package kernel_pkg;

package body kernel_pkg is

end package body kernel_pkg;
