library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.ccd_pkg.all;
use work.img_pkg.Pixel_Color_T;
use work.img_pkg.Pixel_Matrix_T;
use work.img_pkg.Pixel_Aggregate_T;

package kernel_pkg is
    -- demosaiced image resolution
    --    constant RGB_IMG_WIDTH  : natural := OUTPUT_WIDTH - 2;
    --    constant RGB_IMG_HEIGHT : natural := OUTPUT_HEIGHT - 2;
    constant RGB_IMG_HEIGHT : natural := 62;
    constant RGB_IMG_WIDTH  : natural := 82;

    subtype Rgb_Img_Width_T is natural range 0 to RGB_IMG_WIDTH - 1;
    subtype Rgb_Img_Height_T is natural range 0 to RGB_IMG_HEIGHT - 1;
    subtype Rgb_Img_Pixel_Ptr_T is natural range 0 to (RGB_IMG_WIDTH * RGB_IMG_HEIGHT);

    constant PIPELINE_STAGES      : natural := 4;
    -- 17 bit data width should be enough to prevent under/overflows
    constant PIPELINE_PIXEL_WIDTH : natural := 17;

    subtype Pipeline_Pixel_T is signed(PIPELINE_PIXEL_WIDTH - 1 downto 0);
    type Stage_Color_Output_T is array (Pixel_Color_T) of Pipeline_Pixel_T;

    -- number of adders required for each pipeline stage
    constant STAGE1_ADDER_COUNT : natural := 5;
    constant STAGE2_ADDER_COUNT : natural := 3;
    constant STAGE3_ADDER_COUNT : natural := 2;

    type Stage1_Output_Array_T is array (0 to STAGE1_ADDER_COUNT - 1) of Stage_Color_Output_T;
    type Stage2_Output_Array_T is array (0 to STAGE2_ADDER_COUNT - 1) of Stage_Color_Output_T;
    type Stage3_Output_Array_T is array (0 to STAGE3_ADDER_COUNT - 1) of Stage_Color_Output_T;

    type Matrix_Aggregate_T is array (Pixel_Color_T) of Pixel_Matrix_T;

    -- 5 bit
    type Convolution_Params_T is array (2 downto 0, 2 downto 0) of integer range -16 to 15;
    -- constant to divide (prescale) kernel with (only prescaling by powers of 2)
    subtype Convolution_Prescale_T is integer range -6 to 6;

    type Rgb_Img_Pixel_Array_T is protected
        procedure setPixel(currHeight : in Rgb_Img_Height_T; currWidth : in Rgb_Img_Width_T; value : in Pixel_Aggregate_T);
        impure function getPixel(currHeight : Rgb_Img_Height_T; currWidth : Rgb_Img_Width_T) return Pixel_Aggregate_T;
    end protected Rgb_Img_Pixel_Array_T;

    function to_saturated_unsigned(val : signed; outLen : natural) return unsigned;
end package kernel_pkg;

package body kernel_pkg is
    type Rgb_Img_Pixel_Array_T is protected body
        type Rgb_Img_Pixel_Matrix_T is array (Rgb_Img_Height_T, Rgb_Img_Width_T) of Pixel_Aggregate_T;

        variable pixelMatrix : Rgb_Img_Pixel_Matrix_T;

        procedure setPixel(currHeight : in Rgb_Img_Height_T; currWidth : in Rgb_Img_Width_T; value : in Pixel_Aggregate_T) is
        begin
            pixelMatrix(currHeight, currWidth) := value;
        end procedure setPixel;

        impure function getPixel(currHeight : Rgb_Img_Height_T; currWidth : Rgb_Img_Width_T) return Pixel_Aggregate_T is
        begin
            return pixelMatrix(currHeight, currWidth);
        end function getPixel;
    end protected body Rgb_Img_Pixel_Array_T;

    pure function to_saturated_unsigned(val : signed; outLen : natural)
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
    end function to_saturated_unsigned;
end package body kernel_pkg;
