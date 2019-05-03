library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package common_pkg is
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
        width_start  : CCD_WIDTH;
        -- from which row are we starting readout
        height_start : CCD_HEIGHT;
        -- image height (including pixels only used to avoid fringing)
        height       : CCD_HEIGHT;
        -- image width (including pixels only used to avoid fringing)
        width        : CCD_WIDTH;
        -- is chip outputting pixels mirrored
        is_mirrored  : boolean;
        -- how much pixel data do we really use/need
        pixel_size   : positive;
    end record Image_Properties;

    constant CCD_CONSTS : Ccd_Properties := (
        width         => 2752,
        height        => 2002,
        active_width  => 2592,
        active_height => 1944,
        data_len      => 12
    );

    --    constant IMG_CONSTS : Image_Properties := (
    --        width_start     => 1053,
    --        height_start    => 758,
    --        height          => 484,
    --        width           => 644,
    --        is_mirrored     => false,
    --        pixel_data_size => 8
    --    );

    constant IMG_CONSTS : Image_Properties := (
        width_start  => 1053,
        height_start => 758,
        height       => 64,
        width        => 84,
        is_mirrored  => false,
        pixel_size   => 8
    );

    subtype Img_Height_Range is natural range 0 to IMG_CONSTS.height - 1;
    subtype Img_Width_Range is natural range 0 to IMG_CONSTS.width - 1;

    -- INTERNAL TYPES
    subtype Pixel_Data is unsigned((IMG_CONSTS.pixel_size - 1) downto 0);
    subtype Pixel_Count_Range is natural range 0 to (IMG_CONSTS.width * IMG_CONSTS.height);
    type Pixel_Color is (Red, Green, Blue);
    type Pixel_Aggregate is array (Pixel_Color) of Pixel_Data;
    type Pixel_Matrix is array (2 downto 0, 2 downto 0) of Pixel_Data;

end package common_pkg;
