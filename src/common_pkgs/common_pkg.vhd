library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.i2c_pkg.I2C_Addr;
use work.i2c_pkg.I2C_Data;

package common_pkg is
    type CCD_Properties_R is record
        width         : positive;
        height        : positive;
        -- true active image size (excluding boundary region, dark region)
        active_width  : positive;
        active_height : positive;
        -- length of pixel data vector
        data_len      : positive;
    end record CCD_Properties_R;

    constant CCD_CONSTS : CCD_Properties_R := (
        width         => 2752,
        height        => 2002,
        active_width  => 2592,
        active_height => 1944,
        data_len      => 12
    );

    subtype CCD_Width_Range is natural range 0 to CCD_CONSTS.width - 1;
    subtype CCD_Height_Range is natural range 0 to CCD_CONSTS.height - 1;

    type ROM_Data is record
        addr : I2C_Addr;
        data : I2C_Data;
    end record ROM_Data;
    type Configuration_Array_T is array (natural range <>) of ROM_Data;
    type Switch_T is array (boolean) of I2C_Data;

    pure function valToConfig(val : natural) return I2C_Data;

    type Img_Properties_R is record
        -- the X coordinate of the upper-left corner of FOV -> EVEN (rounded down)
        width_start   : CCD_Width_Range;
        -- the Y coordinate of the upper-left corner of FOV -> EVEN (rounded down)
        height_start  : CCD_Height_Range;
        -- height of the FOV - 1 -> ODD (rounded up)
        height        : CCD_Height_Range;
        -- width of the FOV - 1 -> ODD (rounded up)
        width         : CCD_Width_Range;
        -- is chip outputting pixels mirrored
        is_mirrored   : boolean;
        -- how much pixel data do we really use/need
        pixel_size    : positive;
        -- number of PIXCLKs
        hblank        : natural;
        -- number of lines
        vblank        : natural;
        -- self-explanatory
        shutter_width : natural;
        -- whether to show debug test pattern
        test_pattern  : boolean;
    end record Img_Properties_R;

    --    constant IMG_CONSTS : Image_Properties := (
    --        width_start     => 1053,
    --        height_start    => 758,
    --        height          => 484,
    --        width           => 644,
    --        is_mirrored     => false,
    --        pixel_data_size => 8
    --    );

    constant IMG_CONSTS : Img_Properties_R := (
        width_start   => 1052,
        height_start  => 758,
        height        => 64,
        width         => 84,
        is_mirrored   => false,
        pixel_size    => 8,
        hblank        => 768,
        vblank        => 8,
        -- FIXME: nespravna hodnota (0x7970)
        shutter_width => 0,
        test_pattern  => true
    );

    constant MIRROR_SWITCH       : Switch_T := (true => X"C040", false => X"0040");
    constant TEST_PATTERN_SWITCH : Switch_T := (true => X"0001", false => X"0000");

    -- data to send to ccd chip in format (reg_addr, reg_data) [8b, 16b]
    constant CCD_CONFIG : Configuration_Array_T := (
        -- row start (Y coordinate of upper left FOV corner)
        (X"01", valToConfig(IMG_CONSTS.height_start)),
        -- column start (X coordinate of upper left FOV corner)
        (X"02", valToConfig(IMG_CONSTS.width_start)),
        -- height of FOV
        (X"03", valToConfig(IMG_CONSTS.height)),
        -- width of FOV
        (X"04", valToConfig(IMG_CONSTS.width)),
        -- hblank (num. of PIXCLKs)
        (X"05", valToConfig(IMG_CONSTS.hblank)),
        -- vblank (num. of PIXCLKs)
        (X"06", valToConfig(IMG_CONSTS.vblank)),
        -- set shutter width
        (X"09", valToConfig(IMG_CONSTS.shutter_width)),
        -- invert pixel clock
        (X"0A", X"8000"),
        -- enable row & column mirroring
        (X"20", MIRROR_SWITCH(IMG_CONSTS.is_mirrored)),
        -- row address mode (binning, skipping) [NO BIN, NO SKIP]
        (X"22", X"0000"),
        -- column address mode (binning, skipping) [NO BIN, NO SKIP]
        (X"23", X"0000"),
        -- Green1 gain
        (X"2B", X"000B"),
        -- Blue gain
        (X"2C", X"000F"),
        -- Red gain
        (X"2D", X"000F"),
        -- Green2 gain
        (X"2E", X"000B"),
        -- TEST PATTERNS
        -- test pattern control (on/off + type of pattern)
        (X"A0", TEST_PATTERN_SWITCH(IMG_CONSTS.test_pattern)),
        -- test pattern green intensity
        (X"A1", X"00FF"),
        -- test pattern red intensity
        (X"A2", X"00FF"),
        -- test pattern blue intensity
        (X"A3", X"00FF"),
        -- test pattern bar width (for patterns 6, 7)
        (X"A4", X"0000")
    );

    subtype Img_Height_Range is natural range 0 to IMG_CONSTS.height - 1;
    subtype Img_Width_Range is natural range 0 to IMG_CONSTS.width - 1;

    -- CCD TYPES
    subtype CCD_Pixel_Data_T is std_logic_vector((CCD_CONSTS.data_len - 1) downto 0);
    -- ccd has bayer color mask (2 * green pixel)
    type CCD_Pixel_Color_T is (Red, Green1, Green2, Blue);
    type CCD_Matrix_T is array (CCD_Height_Range, CCD_Width_Range) of CCD_Pixel_Data_T;

    -- COMMON INTERNAL TYPES
    subtype Pixel_Data is unsigned((IMG_CONSTS.pixel_size - 1) downto 0);
    subtype Pixel_Count_Range is natural range 0 to (IMG_CONSTS.width * IMG_CONSTS.height);
    type Pixel_Color is (Red, Green, Blue);
    type Pixel_Aggregate is array (Pixel_Color) of Pixel_Data;
    type Pixel_Matrix is array (2 downto 0, 2 downto 0) of Pixel_Data;

end package common_pkg;

package body common_pkg is

    pure function valToConfig(val : natural) return I2C_Data is
    begin
        return std_logic_vector(to_unsigned(val, I2C_Data'length));
    end function valToConfig;

end package body common_pkg;
