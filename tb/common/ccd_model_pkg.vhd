library ieee;
use ieee.std_logic_1164.all;
use work.common_pkg.all;
use work.i2c_pkg.all;

package ccd_model_pkg is
    type CCD_Reg_Addr_R is record
        rowStart, colStart : I2C_Addr;
        rowSize, colSize   : I2C_Addr;
        hblank, vblank     : I2C_Addr;
        -- mirror row/column, show dark rows/columns, etc.
        readMode2          : I2C_Addr;
    end record CCD_Reg_Addr_R;

    subtype HBlank_Range is natural range 0 to 4095;
    subtype VBlank_Range is natural range 8 to 2047;

    type CCD_Params_R is record
        rowStart  : CCD_Height_Range;
        colStart  : CCD_Width_Range;
        rowSize   : CCD_Height_Range;
        colSize   : CCD_Width_Range;
        -- in PIXCLKs
        hblank    : HBlank_Range;
        -- in image rows
        vblank    : VBlank_Range;
        rowMirror : boolean;
        colMirror : boolean;
    end record CCD_Params_R;

    -- depends on binning mode (this is for no bin [row, col])
    constant HBLANK_MIN        : HBlank_Range     := 782;
    constant VBLANK_MIN        : VBlank_Range     := 8;
    constant HBLANK_DEFAULT    : HBlank_Range     := 782;
    constant VBLANK_DEFAULT    : VBlank_Range     := 8;
    constant ROW_START_DEFAULT : CCD_Height_Range := 54;
    constant COL_START_DEFAULT : CCD_Width_Range  := 16;
    constant ROW_SIZE_DEFAULT  : CCD_Height_Range := 1943;
    constant COL_SIZE_DEFAULT  : CCD_Width_Range  := 2591;
    constant REG_ADDR          : CCD_Reg_Addr_R   := (
        rowStart  => X"01",
        colStart  => X"02",
        rowSize   => X"03",
        colSize   => X"04",
        hblank    => X"05",
        vblank    => X"06",
        readMode2 => X"20"
    );

    pure function logicToBool(val : std_logic) return boolean;
end package ccd_model_pkg;

package body ccd_model_pkg is

    pure function logicToBool(val : std_logic) return boolean is
    begin
        if val = '1' then
            return true;
        else
            return false;
        end if;
    end function logicToBool;

end package body ccd_model_pkg;
