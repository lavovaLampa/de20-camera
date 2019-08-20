library ieee;
use ieee.std_logic_1164.all;

use work.ccd_pkg.all;
use work.i2c_pkg.all;

package ccd_model_pkg is
    -- pixel array to be used in simulation
    shared variable pixelArray : Ccd_Pixel_Array_T;

    type CCD_Reg_Addr_R is record
        rowStart, colStart : I2c_Addr_T;
        rowSize, colSize   : I2c_Addr_T;
        hblank, vblank     : I2c_Addr_T;
        -- mirror row/column, show dark rows/columns, etc.
        readMode2          : I2c_Addr_T;
    end record CCD_Reg_Addr_R;

    type Ccd_Params_R is record
        rowStart  : Ccd_Active_Height_Ptr_T;
        colStart  : Ccd_Active_Width_Ptr_T;
        rowSize   : Ccd_Active_Height_Ptr_T;
        colSize   : Ccd_Active_Width_Ptr_T;
        -- in PIXCLKs
        hblank    : Horizontal_Blank_T;
        -- in image rows
        vblank    : Vertical_Blank_T;
        rowMirror : boolean;
        colMirror : boolean;
    end record Ccd_Params_R;

    -- depends on binning mode (this is for no bin [row, col])
    constant HBLANK_MIN        : Horizontal_Blank_T      := 312;
    constant VBLANK_MIN        : Vertical_Blank_T        := 8;
    constant HBLANK_DEFAULT    : Horizontal_Blank_T      := 782;
    constant VBLANK_DEFAULT    : Vertical_Blank_T        := 8;
    constant ROW_START_DEFAULT : Ccd_Active_Height_Ptr_T := 54;
    constant COL_START_DEFAULT : Ccd_Active_Width_Ptr_T  := 16;
    constant ROW_SIZE_DEFAULT  : Ccd_Active_Height_Ptr_T := 1943;
    constant COL_SIZE_DEFAULT  : Ccd_Active_Width_Ptr_T  := 2591;
    constant REG_ADDR          : CCD_Reg_Addr_R          := (
        rowStart  => X"01",
        colStart  => X"02",
        rowSize   => X"03",
        colSize   => X"04",
        hblank    => X"05",
        vblank    => X"06",
        readMode2 => X"20"
    );

    type Ccd_Pixel_T is (Dark, Boundary, Active, HorizontalBlank, VerticalBlank);

    type Region_Bounds_R is record
        low  : natural;
        -- end is reserved word :(
        high : natural;
    end record Region_Bounds_R;

    type CCD_Region_R is record
        dark1     : Region_Bounds_R;
        boundary1 : Region_Bounds_R;
        active    : Region_Bounds_R;
        boundary2 : Region_Bounds_R;
        dark2     : Region_Bounds_R;
    end record CCD_Region_R;

    pure function get_ccd_pixel_type(absoluteHeight : natural; absoluteWidth : natural) return Ccd_Pixel_T;
end package ccd_model_pkg;

package body ccd_model_pkg is
    constant COL_REGIONS : CCD_Region_R := (
        dark1     => (0, 9),
        boundary1 => (10, 15),
        active    => (16, 2607),
        boundary2 => (2608, 2617),
        dark2     => (2618, 2751)
    );

    constant ROW_REGIONS : CCD_Region_R := (
        dark1     => (0, 49),
        boundary1 => (50, 53),
        active    => (54, 1997),
        boundary2 => (1998, 2000),
        dark2     => (2001, 2001)
    );

    type Ccd_Pixel_Type_Map_T is array (Ccd_Pixel_T, Ccd_Pixel_T) of Ccd_Pixel_T;
    -- pixelTypeMap(height, width)
    constant pixelTypeMap : Ccd_Pixel_Type_Map_T := (
        Dark            => (
            HorizontalBlank => HorizontalBlank,
            others          => Dark
        ),
        Boundary        => (
            Dark            => Dark,
            HorizontalBlank => HorizontalBlank,
            others          => Boundary
        ),
        Active          => (
            Dark            => Dark,
            Boundary        => Boundary,
            HorizontalBlank => HorizontalBlank,
            others          => Active
        ),
        VerticalBlank   => (
            others => VerticalBlank
        ),
        HorizontalBlank => (
            others => HorizontalBlank
        )
    );

    pure function getRowType(absoluteHeight : natural) return Ccd_Pixel_T is
    begin
        case absoluteHeight is
            when ROW_REGIONS.dark1.low to ROW_REGIONS.dark1.high | ROW_REGIONS.dark2.low to ROW_REGIONS.dark2.high =>
                return Dark;

            when ROW_REGIONS.boundary1.low to ROW_REGIONS.boundary1.high | ROW_REGIONS.boundary2.low to ROW_REGIONS.boundary2.high =>
                return Boundary;

            when ROW_REGIONS.active.low to ROW_REGIONS.active.high =>
                return Active;

            when others =>
                return VerticalBlank;
        end case;
    end function getRowType;

    pure function getColType(absoluteWidth : natural) return Ccd_Pixel_T is
    begin
        case absoluteWidth is
            when COL_REGIONS.dark1.low to COL_REGIONS.dark1.high | COL_REGIONS.dark2.low to COL_REGIONS.dark2.high =>
                return Dark;

            when COL_REGIONS.boundary1.low to COL_REGIONS.boundary1.high | COL_REGIONS.boundary2.low to COL_REGIONS.boundary2.high =>
                return Boundary;

            when COL_REGIONS.active.low to COL_REGIONS.active.high =>
                return Active;

            when others =>
                return HorizontalBlank;
        end case;
    end function getColType;

    pure function get_ccd_pixel_type(absoluteHeight : natural; absoluteWidth : natural) return Ccd_Pixel_T is
        constant colType : Ccd_Pixel_T := getColType(absoluteWidth);
        constant rowType : Ccd_Pixel_T := getRowType(absoluteHeight);
    begin
        return pixelTypeMap(rowType, colType);
    end function get_ccd_pixel_type;

end package body ccd_model_pkg;
