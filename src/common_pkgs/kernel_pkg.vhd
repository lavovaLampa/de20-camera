library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.common_pkg.all;

package kernel_pkg is
    -- ALIASES
    alias PIXEL_SIZE is IMG_CONSTS.pixel_size;

    -- CONSTANTS
    constant PIPELINE_STAGE_AMOUNT : natural := 4;
    -- 17 bit data width should be enough to prevent under/overflows
    constant PIPELINE_SIZE         : natural := 17;
    constant IMG_WIDTH             : natural := IMG_CONSTS.width - 2;
    constant IMG_HEIGHT            : natural := IMG_CONSTS.height - 2;

    constant STAGE1_AMOUNT : natural := 5;
    constant STAGE2_AMOUNT : natural := 3;
    constant STAGE3_AMOUNT : natural := 2;

    -- TYPES
    subtype Pipeline_Pixel is signed(PIPELINE_SIZE - 1 downto 0);
    type Stage_Color_Out is array (natural range <>) of Pipeline_Pixel;
    type Stage_Out is array (Pixel_Color) of Stage_Color_Out;

    type Matrix_Aggregate is array (Pixel_Color) of Pixel_Matrix;

    -- 5 bit
    type Convolution_Params is array (2 downto 0, 2 downto 0) of integer range -16 to 15;
    -- constant to divide (prescale) kernel with
    subtype Convolution_Prescale is integer range -6 to 6;

    -- FUNCTIONS
    function toSaturatedUnsigned(val : signed; outLen : natural) return unsigned;

end package kernel_pkg;

package body kernel_pkg is
    pure function toSaturatedUnsigned(val : signed; outLen : natural)
    return unsigned is
        variable temp : unsigned((outLen - 1) downto 0);
    begin
        assert outLen < val'length;
        if val < 0 then
            temp := (others => '0');
        elsif val > to_signed((2**outLen) - 1, val'length) then
            temp := (others => '1');
        else
            temp := unsigned(val((outLen - 1) downto 0));
        end if;
        return temp;
    end function toSaturatedUnsigned;
end package body kernel_pkg;
