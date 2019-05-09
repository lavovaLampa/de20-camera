library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.common_pkg.all;

package ccd_ctrl_pkg is
    alias IMG_WIDTH is IMG_CONSTS.width;
    alias IMG_HEIGHT is IMG_CONSTS.height;

    -- pipeline stage has to be wide enough not to overflow during addition
    constant PIPELINE_SIZE : natural := IMG_CONSTS.pixel_size + 2;

    subtype Pixel_Range is natural range IMG_CONSTS.pixel_size - 1 downto 0;

    -- PIPELINE TYPES
    subtype Pipeline_Pixel is unsigned(PIPELINE_SIZE - 1 downto 0);
    type Stage_Out is array (1 downto 0) of Pipeline_Pixel;
    type Pipeline_Matrix is array (2 downto 0, 2 downto 0) of Pipeline_Pixel;

    pure function getCurrColor(currWidth : Img_Width_Range; currHeight : Img_Height_Range) return CCD_Pixel_Color_T;
end package ccd_ctrl_pkg;

package body ccd_ctrl_pkg is
    pure function decodeColor(isEvenRow : boolean; isEvenColumn : boolean)
    return CCD_Pixel_Color_T is
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

    pure function currColorAbsolute(currWidth : CCD_Width_Range; currHeight : CCD_Height_Range; isMirrored : boolean)
    return CCD_Pixel_Color_T is
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
    return CCD_Pixel_Color_T is
        variable absoluteWidth  : CCD_Width_Range  := IMG_CONSTS.width_start + currWidth;
        variable absoluteHeight : CCD_Height_Range := IMG_CONSTS.height_start + currHeight;
    begin
        return currColorAbsolute(absoluteWidth, absoluteHeight, IMG_CONSTS.is_mirrored);
    end function getCurrColor;
end package body ccd_ctrl_pkg;
