library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ccd_pkg.all;

entity ccd_ctrl is
    port(
        clkIn, rstAsyncIn         : in  std_logic;
        frameValidIn, lineValidIn : in  std_logic;
        triggerOut                : out std_logic        := '1';
        shiftEnableOut            : out Color_Enable_Mux := (others => false);
        bayerShiftEnableOut       : out boolean          := false;
        pixelColorOut             : out Pixel_Color      := Green;
        kernelEnableOut           : out boolean
    );
end entity ccd_ctrl;

architecture RTL of ccd_ctrl is
    signal currWidth        : Curr_Width_Range  := 0;
    signal currHeight       : Curr_Height_Range := 0;
    signal currColor        : Pixel_Color       := Green;
    signal firstSubpixelRow : boolean           := true;
begin
    controlProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            currWidth        <= 0;
            currHeight       <= 0;
            currColor        <= Green;
            firstSubpixelRow <= true;
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
                    -- row counter
                    currHeight       <= currHeight + 1;
                    firstSubpixelRow <= not firstSubpixelRow;
                end if;

            -- color shift register selection (mux)
            elsif frameValidIn = '0' and lineValidIn = '0' then
                -- TODO: write assert
                currWidth        <= 0;
                currHeight       <= 0;
                currColor        <= Green;
                firstSubpixelRow <= true;
            end if;
        end if;
    end process controlProc;

    shiftEnableOut.red   <= currColor = Red;
    shiftEnableOut.green <= currColor = Green and not firstSubpixelRow;
    shiftEnableOut.blue  <= currColor = Blue;
    bayerShiftEnableOut  <= currColor = Green;
    -- TODO: kernelEnableOut
    kernelEnableOut      <= (currHeight > 1) and (currWidth > KERNEL_PARAMS.dim - 2);
    pixelColorOut        <= currColor;
    triggerOut           <= '1';

end architecture RTL;
