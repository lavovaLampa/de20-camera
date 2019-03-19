library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package ccd_pkg is
    subtype IMG_WIDTH_RANGE is natural range 0 to 2751 - 1;
    subtype IMG_HEIGHT_RANGE is natural range 0 to 2001 - 1;

    type Ccd_Img_Properties is record
        -- including boundary region to eliminate errors when convoluting border pixels (size -> 1 row/col on either img side)
        width         : IMG_WIDTH_RANGE;
        height        : IMG_HEIGHT_RANGE;
        -- true active image size (excluding boundary region, dark region)
        active_width  : IMG_WIDTH_RANGE;
        active_height : IMG_HEIGHT_RANGE;
        -- is chip outputting pixels mirrored
        mirrored      : boolean;
        -- length of pixel data vector
        data_len      : natural;
    end record Ccd_Img_Properties;

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

    constant IMG_PARAMS : Ccd_Img_Properties := (
        width         => 2594,
        height        => 1946,
        active_width  => 2592,
        active_height => 1944,
        mirrored      => true,
        data_len      => 12
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

    constant MUL_RESULT_LEN : natural := (IMG_PARAMS.data_len + 1) + KERNEL_PARAMS.data_len;

    subtype Ccd_Pixel_Data is unsigned((IMG_PARAMS.data_len - 1) downto 0);

    type Color_Enable_Mux is record
        red   : boolean;
        green : boolean;
        blue  : boolean;
    end record Color_Enable_Mux;

    type Color_Data_Mux is record
        red   : Ccd_Pixel_Data;
        green : Ccd_Pixel_Data;
        blue  : Ccd_Pixel_Data;
    end record Color_Data_Mux;

    type Pixel_Color is (Red, Green, Blue);

    subtype Curr_Width_Range is natural range 0 to IMG_PARAMS.width - 1;
    subtype Curr_Height_Range is natural range 0 to IMG_PARAMS.height - 1;

    subtype Kernel_Const_Data_Range is integer range -(2**(KERNEL_PARAMS.data_len - 1)) to ((2**(KERNEL_PARAMS.data_len - 1)) - 1);

    -- constant to divide (prescale) kernel with
    subtype Prescale_Range is integer range -12 to 12;

    -- register type to store raw (unsigned) pixel values (12b) for convolution kernel computation
    type Ccd_Color_Matrix is array (((KERNEL_PARAMS.dim**2) - 1) downto 0) of Ccd_Pixel_Data;

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

    function nextColor(currColor : Pixel_Color; firstSubpixelRow : boolean) return Pixel_Color;
    function widen(val : signed) return signed;
    function widen(val : unsigned) return unsigned;
    function toSaturatedUnsigned(val : signed; outLen : natural) return unsigned;
end package ccd_pkg;

package body ccd_pkg is

    function nextColor(currColor : Pixel_Color; firstSubpixelRow : boolean)
    return Pixel_Color is
    begin
        if currColor = Red or currColor = Blue then
            return Green;
        else
            if IMG_PARAMS.mirrored then
                if firstSubpixelRow then
                    return Blue;
                else
                    return Red;
                end if;
            else
                if firstSubpixelRow then
                    return Red;
                else
                    return Blue;
                end if;
            end if;
        end if;
    end function nextColor;

    function widen(val : signed)
    return signed is
    begin
        return resize(val, val'length + 1);
    end function widen;

    function widen(val : unsigned)
    return unsigned is
    begin
        return resize(val, val'length + 1);
    end function widen;

    function toSaturatedUnsigned(val : signed; outLen : natural)
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
