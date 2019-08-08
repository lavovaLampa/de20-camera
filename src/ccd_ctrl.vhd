library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.ccd_pkg.all;
use work.img_pkg.Pixel_Data_T;

entity ccd_ctrl is
    port(
        clkIn, rstAsyncIn                                      : in  std_logic;
        -- ccd i/o
        frameValidIn, lineValidIn                              : in  std_logic;
        ccdPixelIn                                             : in  Ccd_Pixel_Data_T;
        -- output signals
        pixelValidOut, frameEndStrobeOut, hBlankOut, vBlankOut : out boolean;
        heightOut                                              : out Ccd_Img_Height_Ptr_T; -- zero based
        widthOut                                               : out Ccd_Img_Width_Ptr_T; -- zero based
        pixelCounterOut                                        : out Ccd_Img_Pixel_Ptr_T; -- zero based
        pixelOut                                               : out Pixel_Data_T
    );
end entity ccd_ctrl;

architecture RTL of ccd_ctrl is
    -- ccd state
    signal currPixelValid : boolean;
    signal hBlank, vBlank : boolean;

    -- state registers
    signal currWidth    : Ccd_Img_Width_Ptr_T  := 0; -- width of pixel at the center of matrix
    signal currHeight   : Ccd_Img_Height_Ptr_T := 0; -- height of pixel at the center of matrix
    signal pixelCounter : Ccd_Img_Pixel_Ptr_T  := 0;
begin
    -- self-explanatory, is current pixel valid?
    currPixelValid <= frameValidIn = '1' and lineValidIn = '1';
    hBlank         <= lineValidIn = '0';
    vBlank         <= frameValidIn = '0';

    assert (pixelValidOut xor (hBlankOut or vBlankOut)) or (not pixelValidOut and not hBlankOut and not vBlankOut)
    report "Invalid state, ccd sensors cannot simulatenously be blanking and outputting valid pixel"
    severity error;

    ctrlProc : process(clkIn, rstAsyncIn)
        variable frameEndStrobe : boolean := false;
    begin
        if rstAsyncIn = '1' then
            currWidth    <= 0;
            currHeight   <= 0;
            pixelCounter <= 0;

            pixelValidOut     <= false;
            frameEndStrobeOut <= false;
            hBlankOut         <= false;
            heightOut         <= 0;
            widthOut          <= 0;

            frameEndStrobe := false;
        elsif rising_edge(clkIn) then
            pixelValidOut     <= currPixelValid;
            hBlankOut         <= hBlank;
            vBlankOut         <= vBlank;
            -- truncate pixel to high 8 bits
            pixelOut          <= unsigned(ccdPixelIn(ccdPixelIn'high downto 4));
            frameEndStrobeOut <= false;
            pixelCounterOut   <= pixelCounter;
            heightOut         <= currHeight;
            widthOut          <= currWidth;

            -- counter logic
            if currPixelValid then
                if currWidth >= OUTPUT_WIDTH - 1 then
                    currWidth <= 0;
                    if currHeight < OUTPUT_HEIGHT - 1 then
                        currHeight <= currHeight + 1;
                    else
                        currHeight <= 0;
                    end if;
                else
                    currWidth <= currWidth + 1;
                end if;

                if pixelCounter < Ccd_Img_Pixel_Ptr_T'high then
                    pixelCounter <= pixelCounter + 1;
                else
                    pixelCounter <= 0;
                end if;

            -- reset counters on start of new frame
            elsif vBlank then
                if not frameEndStrobe then
                    frameEndStrobeOut <= true;
                end if;

                currWidth    <= 0;
                currHeight   <= 0;
                pixelCounter <= 0;
            end if;
        end if;
    end process ctrlProc;
end architecture RTL;
