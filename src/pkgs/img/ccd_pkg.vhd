library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.i2c_pkg.all;

package ccd_pkg is
    constant ARRAY_WIDTH  : natural := 2752;
    constant ARRAY_HEIGHT : natural := 2004;

    constant ACTIVE_WIDTH  : natural := 2592;
    constant ACTIVE_HEIGHT : natural := 1944;

    constant OUTPUT_WIDTH  : natural := 84;
    constant OUTPUT_HEIGHT : natural := 64;

    constant DATA_WIDTH : natural := 12;

    subtype Horizontal_Blank_T is natural range 0 to 4095;
    subtype Vertical_Blank_T is natural range 8 to 2047;

    subtype Ccd_Height_Ptr_T is natural range 0 to ARRAY_HEIGHT - 1;
    subtype Ccd_Width_Ptr_T is natural range 0 to ARRAY_WIDTH - 1;
    subtype Ccd_Pixel_Ptr_T is natural range 0 to (ARRAY_HEIGHT * ARRAY_WIDTH) - 1;

    subtype Ccd_Active_Height_Ptr_T is natural range 0 to ACTIVE_HEIGHT - 1;
    subtype Ccd_Active_Width_Ptr_T is natural range 0 to ACTIVE_WIDTH - 1;
    subtype Ccd_Active_Pixel_Ptr_T is natural range 0 to (ACTIVE_HEIGHT * ACTIVE_WIDTH) - 1;

    type Ccd_Configuration_R is record
        -- the X coordinate of the upper-left corner of FOV -> EVEN (rounded down)
        width_start   : positive range 16 to 2607;
        -- the Y coordinate of the upper-left corner of FOV -> EVEN (rounded down)
        height_start  : positive range 54 to 1997;
        -- height of the FOV - 1 -> ODD (rounded up)
        height        : Ccd_Active_Height_Ptr_T;
        -- width of the FOV - 1 -> ODD (rounded up)
        width         : Ccd_Active_Width_Ptr_T;
        -- is chip outputting pixels mirrored
        is_mirrored   : boolean;
        -- number of PIXCLKs
        hblank        : Horizontal_Blank_T;
        -- number of lines
        vblank        : Vertical_Blank_T;
        -- self-explanatory
        shutter_width : natural;
        -- whether to show debug test pattern
        test_pattern  : boolean;
    end record Ccd_Configuration_R;

    constant CCD_CONFIGURATION : Ccd_Configuration_R := (
        width_start   => 990,
        height_start  => 838,
        height        => OUTPUT_HEIGHT,
        width         => OUTPUT_WIDTH,
        is_mirrored   => false,
        hblank        => 1023,
        vblank        => 130,
        -- FIXME: write correct value
        shutter_width => 0,
        test_pattern  => true
    );

    -- configure img dimensions
    subtype Ccd_Img_Height_Ptr_T is natural range 0 to CCD_CONFIGURATION.height - 1;
    subtype Ccd_Img_Width_Ptr_T is natural range 0 to CCD_CONFIGURATION.width - 1;
    subtype Ccd_Img_Pixel_Ptr_T is natural range 0 to (CCD_CONFIGURATION.height * CCD_CONFIGURATION.width) - 1;

    -- ccd pixel data (12 bit)
    subtype Ccd_Pixel_Data_T is std_logic_vector(DATA_WIDTH - 1 downto 0);
    -- ccd has bayer color mask (2 * green pixel)
    type Ccd_Pixel_Color_T is (Red, Green1, Green2, Blue);

    type Ccd_Pixel_Array_T is protected
        procedure setPixel(currHeight : in Ccd_Active_Height_Ptr_T; currWidth : in Ccd_Active_Width_Ptr_T; value : in Ccd_Pixel_Data_T);
        impure function getPixel(currHeight : Ccd_Active_Height_Ptr_T; currWidth : Ccd_Active_Width_Ptr_T) return Ccd_Pixel_Data_T;
    end protected Ccd_Pixel_Array_T;

    pure function get_ccd_pixel_color(absoluteHeight : Ccd_Active_Height_Ptr_T; absoluteWidth : Ccd_Active_Width_Ptr_T; isMirrored : boolean) return Ccd_Pixel_Color_T;
end package ccd_pkg;

package body ccd_pkg is
    type Ccd_Pixel_Array_T is protected body
        type Ccd_Pixel_Matrix_T is array (Ccd_Active_Height_Ptr_T, Ccd_Active_Width_Ptr_T) of Ccd_Pixel_Data_T;

        variable pixelMatrix : Ccd_Pixel_Matrix_T;

        procedure setPixel(currHeight : in Ccd_Active_Height_Ptr_T; currWidth : in Ccd_Active_Width_Ptr_T; value : in Ccd_Pixel_Data_T) is
        begin
            pixelMatrix(currHeight, currWidth) := value;
        end procedure setPixel;

        impure function getPixel(currHeight : Ccd_Active_Height_Ptr_T; currWidth : Ccd_Active_Width_Ptr_T) return Ccd_Pixel_Data_T is
        begin
            return pixelMatrix(currHeight, currWidth);
        end function getPixel;
    end protected body Ccd_Pixel_Array_T;

    pure function decode_color(isEvenRow : boolean; isEvenColumn : boolean) return Ccd_Pixel_Color_T is
    begin
        if isEvenColumn then
            if isEvenRow then
                return Green1;
            else
                return Blue;
            end if;
        else
            if isEvenRow then
                return Red;
            else
                return Green2;
            end if;
        end if;
    end function decode_color;

    pure function get_ccd_pixel_color(absoluteHeight : Ccd_Active_Height_Ptr_T; absoluteWidth : Ccd_Active_Width_Ptr_T; isMirrored : boolean) return Ccd_Pixel_Color_T is
        variable isEvenRow    : boolean := absoluteHeight mod 2 = 0;
        variable isEvenColumn : boolean := absoluteWidth mod 2 = 0;
    begin
        if isMirrored then
            return decode_color(not isEvenRow, not isEvenColumn);
        else
            return decode_color(isEvenRow, isEvenColumn);
        end if;
    end function get_ccd_pixel_color;

    --    pure function getCurrColor(currHeight : Ccd_Active_Height_Ptr_T; currWidth : Ccd_Active_Width_Ptr_T) return Ccd_Pixel_Color_T is
    --        variable absoluteWidth  : Ccd_Active_Width_Ptr_T  := CCD_CONFIGURATION.width_start + currWidth;
    --        variable absoluteHeight : Ccd_Active_Height_Ptr_T := CCD_CONFIGURATION.height_start + currHeight;
    --    begin
    --        return currColorAbsolute(absoluteHeight, absoluteWidth, CCD_CONFIGURATION.is_mirrored);
    --    end function getCurrColor;

end package body ccd_pkg;
