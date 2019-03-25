library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ccd_pkg.all;

entity ccd_ctrl is
    port(
        clkIn, rstAsyncIn                   : in  std_logic;
        frameValidIn, lineValidIn, strobeIn : in  std_logic;
        pixelDataIn                         : in  Ccd_Pixel_Data;
        redOut, greenOut, blueOut           : out Pixel_Data;
        currXOut                            : out Curr_Width_Range;
        currYOut                            : out Curr_Height_Range;
        pixelValidOut                       : out boolean
    );
end entity ccd_ctrl;

architecture RTL of ccd_ctrl is
    signal currWidth         : Curr_Width_Range     := 0;
    signal currHeight        : Curr_Height_Range    := 0;
    signal currColor         : Pixel_Color          := Green;
begin
    controlProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            currWidth        <= 0;
            currHeight       <= 0;
            currColor        <= Green;
        elsif rising_edge(clkIn) then
            assert currWidth < IMG_PARAMS.width and currHeight < IMG_PARAMS.height;
            if frameValidIn = '1' and lineValidIn = '1' then
                -- assigns next subpixel color (hopefully)
                currColor <= nextColor(currColor, firstSubpixelRow);

                -- column counter
                if (currWidth < IMG_PARAMS.width - 1) then
                    currWidth <= currWidth + 1;
                elsif (currWidth >= IMG_PARAMS.width - 1) then
                    -- end of row
                    currWidth        <= 0;
                    -- row counter, if we are on second subpixel row, advance to the next pixel line
                    currHeight       <= currHeight + 1;
                    firstSubpixelRow <= not firstSubpixelRow;
                end if;

            -- reset state on frame end/begin
            elsif frameValidIn = '0' and lineValidIn = '0' then
                -- TODO: write assert
                currWidth        <= 0;
                currHeight       <= 0;
                currColor        <= Green;
                firstSubpixelRow <= true;
            end if;
        end if;
    end process controlProc;
end architecture RTL;
