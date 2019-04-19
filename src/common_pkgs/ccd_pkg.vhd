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
        -- length of pixel data vector
        data_len      : positive;
    end record Ccd_Properties;

    type Image_Properties is record
        -- from which column are we starting readout
        width_start     : CCD_WIDTH;
        -- from which row are we starting readout
        height_start    : CCD_HEIGHT;
        -- image height (including pixels only used to avoid fringing)
        height          : CCD_HEIGHT;
        -- image width (including pixels only used to avoid fringing)
        width           : CCD_WIDTH;
        -- is chip outputting pixels mirrored
        is_mirrored     : boolean;
        -- how much pixel data do we really use/need
        pixel_data_size : positive;
    end record Image_Properties;

    constant CCD_CONSTS : Ccd_Properties := (
        width         => 2752,
        height        => 2002,
        active_width  => 2592,
        active_height => 1944,
        data_len      => 12
    );

    constant IMG_CONSTS : Image_Properties := (
        width_start     => 1054,
        height_start    => 759,
        height          => 482,
        width           => 642,
        is_mirrored     => false,
        pixel_data_size => 8
    );

    subtype Ccd_Pixel_Data is std_logic_vector((CCD_CONSTS.data_len - 1) downto 0);
    subtype Pixel_Data is unsigned((IMG_CONSTS.pixel_data_size - 1) downto 0);
    subtype Pixel_Count_Range is natural range 0 to (IMG_CONSTS.width * IMG_CONSTS.height);
    
    type Internal_Pixel_Color is (Red, Green, Blue);
    type Pixel_Aggregate is array (Internal_Pixel_Color) of Pixel_Data;
    
    type Pixel_Color is (Red, Green1, Green2, Blue);

    type Pixel_Matrix is array (2 downto 0, 2 downto 0) of Pixel_Data;

    subtype Img_Height_Range is natural range 0 to IMG_CONSTS.height - 1;
    subtype Img_Width_Range is natural range 0 to IMG_CONSTS.width - 1;

    pure function toSaturatedUnsigned(val : signed; outLen : natural) return unsigned;
    pure function getCurrColor(currWidth : Img_Width_Range; currHeight : Img_Height_Range) return Pixel_Color;
end package ccd_pkg;

package body ccd_pkg is
    pure function decodeColor(isEvenRow : boolean; isEvenColumn : boolean)
    return Pixel_Color is
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
    end function decodeColor;

    pure function currColorAbsolute(currWidth : CCD_WIDTH; currHeight : CCD_HEIGHT; isMirrored : boolean)
    return Pixel_Color is
        variable isEvenRow    : boolean := currHeight mod 2 = 0;
        variable isEvenColumn : boolean := currWidth mod 2 = 0;
    begin
        if isMirrored then
            return decodeColor(not isEvenRow, not isEvenColumn);
        else
            return decodeColor(isEvenRow, isEvenColumn);
        end if;
    end function currColorAbsolute;

    pure function getCurrColor(currWidth : Img_Width_Range; currHeight : Img_Height_Range)
    return Pixel_Color is
        variable absoluteWidth  : CCD_WIDTH  := IMG_CONSTS.width_start + currWidth;
        variable absoluteHeight : CCD_HEIGHT := IMG_CONSTS.height_start + currHeight;
    begin
        return currColorAbsolute(absoluteWidth, absoluteHeight, IMG_CONSTS.is_mirrored);
    end function getCurrColor;

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
