library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.ccd_pkg.all;
use work.img_pkg.Pixel_Data_T;

entity ccd_ctrl is
    port(
        clkIn, rstAsyncIn                     : in  std_logic;
        -- ccd i/o
        frameValidIn, lineValidIn             : in  std_logic;
        ccdPixelIn                            : in  CCD_Pixel_Data_T;
        -- output signals
        pixelValidOut, frameEndOut, hBlankOut : out boolean;
        heightOut                             : out Ccd_Height_Ptr_T;
        widthOut                              : out Ccd_Width_Ptr_T;
        pixelCounterOut                       : out Ccd_Pixel_Ptr_T;
        pixelOut                              : out Pixel_Data_T
    );
end entity ccd_ctrl;

architecture RTL of ccd_ctrl is
    -- ccd state
    signal currPixelValid : boolean;
    signal hBlank, vBlank : boolean;

    -- state registers
    signal currWidth    : Ccd_Width_Ptr_T  := 0; -- width of pixel at the center of matrix
    signal currHeight   : Ccd_Height_Ptr_T := 0; -- height of pixel at the center of matrix
    signal pixelCounter : Ccd_Pixel_Ptr_T  := 0;
begin
    -- assign output signals
    heightOut       <= currHeight;
    widthOut        <= currWidth;
    pixelCounterOut <= pixelCounter;

    -- self-explanatory, is current pixel valid?
    currPixelValid <= frameValidIn = '1' and lineValidIn = '1';
    hBlank         <= frameValidIn = '1' and lineValidIn = '0';
    vBlank         <= frameValidIn = '0' and lineValidIn = '0';

    ctrlProc : process(clkIn, rstAsyncIn)
        variable frameEndStrobe : boolean := false;
    begin
        if rstAsyncIn = '1' then
            currWidth    <= 0;
            currHeight   <= 0;
            pixelCounter <= 0;

            pixelValidOut <= false;
            frameEndOut   <= true;
            hBlankOut     <= false;

            frameEndStrobe := false;
        elsif rising_edge(clkIn) then
            pixelValidOut <= currPixelValid;
            frameEndOut   <= vBlank;
            hBlankOut     <= hBlank;
            -- truncate pixel to high 8 bits
            pixelOut      <= unsigned(ccdPixelIn(ccdPixelIn'high downto 4));

            -- counter logic
            if currPixelValid then
                -- account for shift register delay
                if currWidth >= CCD_WIDTH - 1 then
                    currWidth  <= 0;
                    currHeight <= currHeight + 1;
                else
                    currWidth <= currWidth + 1;
                end if;

                pixelCounter <= pixelCounter + 1;

            -- reset counters on start of new frame
            elsif vBlank then
                currWidth    <= 0;
                currHeight   <= 0;
                pixelCounter <= 0;
            end if;
        end if;
    end process ctrlProc;
end architecture RTL;
