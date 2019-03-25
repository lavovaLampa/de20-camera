library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package ccd_pkg is
    subtype CCD_WIDTH is natural range 0 to 2751;
    subtype CCD_HEIGHT is natural range 0 to 2001;

    type Ccd_Properties is record
        -- including boundary region to eliminate errors when convoluting border pixels (size -> 1 row/col on either img side)
        width         : positive;
        height        : positive;
        -- true active image size (excluding boundary region, dark region)
        active_width  : positive;
        active_height : positive;
        -- is chip outputting pixels mirrored
        is_mirrored   : boolean;
        -- length of pixel data vector
        data_len      : positive;
    end record Ccd_Properties;

    type Image_Properties is record
        width_start     : CCD_WIDTH;
        height_start    : CCD_HEIGHT;
        height          : CCD_HEIGHT;
        width           : CCD_WIDTH;
        pixel_data_size : positive;
    end record Image_Properties;

    -- convolution kernel properties
    type Kernel_Properties is record
        dim          : natural;
        data_len     : natural;
        pipeline_len : natural;
    end record Kernel_Properties;

    -- adder stages lengths record
    type Adder_Tree_Props is record
        stage1_len : natural;
        stage2_len : natural;
        stage3_len : natural;
        stage4_len : natural;
    end record Adder_Tree_Props;

    constant CCD_CONSTS : Ccd_Properties := (
        width         => 2752,
        height        => 2002,
        active_width  => 2592,
        active_height => 1944,
        is_mirrored   => true,
        data_len      => 12
    );

    constant IMG_CONSTS : Image_Properties := (
        width_start     => 1055,
        height_start    => 760,
        height          => 480,
        width           => 640,
        pixel_data_size => 8
    );

    constant KERNEL_PARAMS : Kernel_Properties := (
        dim          => 3,
        data_len     => 9,
        pipeline_len => 4
    );

    constant ADDER_TREE_PARAMS : Adder_Tree_Props := (
        stage1_len => ((KERNEL_PARAMS.dim**2) / (2**1)) + ((KERNEL_PARAMS.dim**2) mod 2),
        stage2_len => ((KERNEL_PARAMS.dim**2) / (2**2)) + ((KERNEL_PARAMS.dim**2) mod 2),
        stage3_len => ((KERNEL_PARAMS.dim**2) / (2**3)) + ((KERNEL_PARAMS.dim**2) mod 2),
        stage4_len => ((KERNEL_PARAMS.dim**2) / (2**4)) + ((KERNEL_PARAMS.dim**2) mod 2)
    );

    constant MUL_RESULT_LEN : natural := (IMG_CONSTS.pixel_data_size + 1) + KERNEL_PARAMS.data_len;

    subtype Ccd_Pixel_Data is std_logic_vector((CCD_CONSTS.data_len - 1) downto 0);
    subtype Pixel_Data is unsigned((IMG_CONSTS.pixel_data_size - 1) downto 0);

    type Pixel_Color is (Red, Green, Blue);

    type Pixel_Matrix is array (2 downto 0, 2 downto 0) of Pixel_Data;

    subtype Curr_Height_Range is natural range IMG_CONSTS.height_start to IMG_CONSTS.height_start + IMG_CONSTS.height;
    subtype Curr_Width_Range is natural range IMG_CONSTS.width_start to IMG_CONSTS.width_start + IMG_CONSTS.width;

    subtype Kernel_Const_Data_Range is integer range -(2**(KERNEL_PARAMS.data_len - 1)) to ((2**(KERNEL_PARAMS.data_len - 1)) - 1);

    -- constant to divide (prescale) kernel with
    subtype Prescale_Range is integer range -12 to 12;

    -- register type to store raw (unsigned) pixel values (12b) for convolution kernel computation
    type Ccd_Color_Matrix is array (((KERNEL_PARAMS.dim**2) - 1) downto 0) of Pixel_Data;

    -- type of accumulator to store values after multiplication
    type Mul_Acc is array (((KERNEL_PARAMS.dim**2) - 1) downto 0) of signed((MUL_RESULT_LEN - 1) downto 0);

    -- type of accumulator for first level of addition (size 26b + 1b because of overflow, etc.)
    type Adder_Acc_1 is array ((ADDER_TREE_PARAMS.stage1_len - 1) downto 0) of signed(MUL_RESULT_LEN downto 0);

    -- type of accumulator for second level of addition (size 27b + 1b because of overflow, etc.)
    type Adder_Acc_2 is array ((ADDER_TREE_PARAMS.stage2_len - 1) downto 0) of signed((MUL_RESULT_LEN + 1) downto 0);

    -- type of accumulator for second level of addition (size 27b + 1b because of overflow, etc.)
    type Adder_Acc_3 is array ((ADDER_TREE_PARAMS.stage3_len - 1) downto 0) of signed((MUL_RESULT_LEN + 2) downto 0);

    -- type of accumulator for second level of addition (size 27b + 1b because of overflow, etc.)
    type Adder_Acc_4 is array ((ADDER_TREE_PARAMS.stage4_len - 1) downto 0) of signed((MUL_RESULT_LEN + 3) downto 0);

    -- 9b signed kernel convolution constants
    type Kernel_Params_Arr is array (((KERNEL_PARAMS.dim**2) - 1) downto 0) of Kernel_Const_Data_Range;

    pure function widen(val : signed) return signed;
    pure function widen(val : unsigned) return unsigned;
    pure function toSaturatedUnsigned(val : signed; outLen : natural) return unsigned;
    pure function currColor(currWidth : CCD_WIDTH; currHeight : CCD_HEIGHT; isMirrored : boolean) return Pixel_Color;
end package ccd_pkg;

package body ccd_pkg is
    pure function decodeColor(isEvenRow : boolean; isEvenColumn : boolean)
    return Pixel_Color is
    begin
        if isEvenColumn then
            if isEvenRow then
                return Green;
            else
                return Blue;
            end if;
        else
            if isEvenRow then
                return Red;
            else
                return Green;
            end if;
        end if;
    end function decodeColor;

    pure function currColor(currWidth : CCD_WIDTH; currHeight : CCD_HEIGHT; isMirrored : boolean)
    return Pixel_Color is
        variable isEvenRow    : boolean := currHeight mod 2 = 0;
        variable isEvenColumn : boolean := currWidth mod 2 = 0;
    begin
        if isMirrored then
            return decodeColor(not isEvenRow, not isEvenColumn);
        else
            return decodeColor(isEvenRow, isEvenColumn);
        end if;
    end function currColor;

    pure function widen(val : signed)
    return signed is
    begin
        return resize(val, val'length + 1);
    end function widen;

    pure function widen(val : unsigned)
    return unsigned is
    begin
        return resize(val, val'length + 1);
    end function widen;

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

end package body ccd_pkg;
