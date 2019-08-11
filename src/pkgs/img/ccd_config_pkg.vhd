library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.i2c_pkg.all;
use work.ccd_pkg.all;

package ccd_config_pkg is
    type ROM_Data_T is record
        addr : I2c_Addr_T;
        data : I2c_Data_T;
    end record ROM_Data_T;
    type Configuration_Array_T is array (natural range <>) of ROM_Data_T;
    type Switch_T is array (boolean) of I2c_Data_T;

    constant MIRROR_SWITCH       : Switch_T := (true => X"C040", false => X"0040");
    constant TEST_PATTERN_SWITCH : Switch_T := (true => X"0001", false => X"0000");

    pure function valToConfig(val : natural) return I2c_Data_T;

    -- data to send to ccd chip in format (reg_addr, reg_data) [8b, 16b]
    constant CCD_CONFIG_ARRAY : Configuration_Array_T := (
        -- row start (Y coordinate of upper left FOV corner)
        (X"01", valToConfig(CCD_CONFIGURATION.height_start)),
        -- column start (X coordinate of upper left FOV corner)
        (X"02", valToConfig(CCD_CONFIGURATION.width_start)),
        -- height of FOV
        (X"03", valToConfig(CCD_CONFIGURATION.height - 1)),
        -- width of FOV
        (X"04", valToConfig(CCD_CONFIGURATION.width - 1)),
        -- hblank (num. of PIXCLKs)
        (X"05", valToConfig(CCD_CONFIGURATION.hblank)),
        -- vblank (num. of PIXCLKs)
        (X"06", valToConfig(CCD_CONFIGURATION.vblank)),
        -- set shutter width
        (X"09", valToConfig(CCD_CONFIGURATION.shutter_width)),
        -- invert pixel clock
        (X"0A", X"8000"),
        -- enable row & column mirroring
        (X"20", MIRROR_SWITCH(CCD_CONFIGURATION.is_mirrored)),
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
        (X"A0", TEST_PATTERN_SWITCH(CCD_CONFIGURATION.test_pattern)),
        -- test pattern green intensity
        (X"A1", X"00FF"),
        -- test pattern red intensity
        (X"A2", X"00FF"),
        -- test pattern blue intensity
        (X"A3", X"00FF"),
        -- test pattern bar width (for patterns 6, 7)
        (X"A4", X"0000")
    );

    subtype Config_Ptr_T is natural range CCD_CONFIG_ARRAY'range;
end package ccd_config_pkg;

package body ccd_config_pkg is
    pure function valToConfig(val : natural)
    return I2c_Data_T is
    begin
        return std_logic_vector(to_unsigned(val, I2c_Data_T'length));
    end function valToConfig;
end package body ccd_config_pkg;
