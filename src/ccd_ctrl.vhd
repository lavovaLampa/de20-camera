library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ccd_pkg.all;

entity ccd_ctrl is
    port(
        clkIn, rstAsyncIn         : in  std_logic;
        frameValidIn, lineValidIn : in  std_logic;
        pixelDataIn               : in  Ccd_Pixel_Data;
        redOut, greenOut, blueOut : out Pixel_Data;
        currXOut                  : out Img_Width_Range;
        currYOut                  : out Img_Height_Range;
        pixelValidOut             : out boolean
    );
    alias IMG_WIDTH is IMG_CONSTS.width;
    alias IMG_HEIGHT is IMG_CONSTS.height;
end entity ccd_ctrl;

architecture RTL of ccd_ctrl is
    -- pixel shift register OUTPUT
    signal currPixelMatrix : Pixel_Matrix := (others => (others => X"00"));

    signal currPixelValid  : boolean           := true;
    signal pixelCounter    : Pixel_Count_Range := 0;
    signal currShiftWidth  : Img_Width_Range   := 0;
    signal currShiftHeight : Img_Height_Range  := 0;

    signal widthOut    : Img_Width_Range  := 0;
    signal heightOut   : Img_Height_Range := 0;
    signal hasNewPixel : boolean          := false;
begin

    -- self-explanatory, is current pixel valid?
    currPixelValid <= frameValidIn = '1' and lineValidIn = '1';

    pixelShiftReg : entity work.pixel_shiftreg
        port map(
            clkIn      => clkIn,
            rstAsyncIn => rstAsyncIn,
            -- TODO: parametrize
            dataIn     => unsigned(pixelDataIn(pixelDataIn'high downto 4)),
            enableIn   => currPixelValid,
            pixelsOut  => currPixelMatrix
        );

    shiftProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            currShiftWidth  <= 0;
            currShiftHeight <= 0;
            pixelCounter    <= 0;
        elsif rising_edge(clkIn) then

            -- counter logic
            if currPixelValid then
                -- counts received valid pixels in current frame
                pixelCounter <= pixelCounter + 1;

                hasNewPixel <= true;

                -- current width & height of pixel in shift register
                if pixelCounter > IMG_WIDTH + 1 then
                    if currShiftWidth >= IMG_WIDTH - 1 then
                        currShiftWidth  <= 0;
                        currShiftHeight <= currShiftHeight + 1;
                    else
                        currShiftWidth <= currShiftWidth + 1;
                    end if;
                end if;

            elsif frameValidIn = '0' and lineValidIn = '0' then
                -- reset counters on start of new frame
                pixelCounter    <= 0;
                currShiftWidth  <= 0;
                currShiftHeight <= 0;
                hasNewPixel     <= false;
            else
                hasNewPixel <= false;
            end if;

        end if;
    end process shiftProc;

    demosaicProc : process(clkIn, rstAsyncIn)
        variable currColor : Pixel_Color := getCurrColor(currShiftWidth, currShiftHeight);
    begin
        if rstAsyncIn = '1' then
            redOut        <= X"00";
            greenOut      <= X"00";
            blueOut       <= X"00";
            pixelValidOut <= false;
            widthOut      <= 0;
            heightOut     <= 0;
        elsif rising_edge(clkIn) then
            currColor     := getCurrColor(currShiftWidth, currShiftHeight);
            pixelValidOut <= hasNewPixel and currShiftHeight > 0 and currShiftHeight < IMG_WIDTH - 1 and currShiftWidth > 0 and currShiftWidth < IMG_WIDTH - 1;

            -- ignore image fringes
            if not (currShiftHeight = 0 or currShiftHeight = IMG_HEIGHT - 1 or currShiftWidth = 0 or currShiftWidth = IMG_WIDTH - 1) then
                if hasNewPixel then
                    if widthOut >= IMG_WIDTH - 3 then
                        widthOut  <= 0;
                        heightOut <= heightOut + 1;
                    elsif currShiftHeight = 1 and currShiftWidth = 1 then
                        widthOut <= 0;
                    else
                        widthOut <= widthOut + 1;
                    end if;
                end if;
            elsif pixelCounter = 0 then
                heightOut <= 0;
                widthOut  <= 0;
            end if;

            -- demosaicing
            if currColor = Red then
                redOut   <= currPixelMatrix(1, 1);
                greenOut <= (currPixelMatrix(0, 1) + currPixelMatrix(1, 0) + currPixelMatrix(1, 2) + currPixelMatrix(2, 1)) / 4;
                blueOut  <= (currPixelMatrix(0, 0) + currPixelMatrix(0, 2) + currPixelMatrix(2, 0) + currPixelMatrix(2, 2)) / 4;
            elsif currColor = Blue then
                redOut   <= (currPixelMatrix(0, 0) + currPixelMatrix(0, 2) + currPixelMatrix(2, 0) + currPixelMatrix(2, 2)) / 4;
                greenOut <= (currPixelMatrix(0, 1) + currPixelMatrix(1, 0) + currPixelMatrix(1, 2) + currPixelMatrix(2, 1)) / 4;
                blueOut  <= currPixelMatrix(1, 1);
            elsif currColor = Green1 then
                redOut   <= (currPixelMatrix(1, 0) + currPixelMatrix(1, 2)) / 2;
                greenOut <= currPixelMatrix(1, 1);
                blueOut  <= (currPixelMatrix(0, 1) + currPixelMatrix(2, 1)) / 2;
            elsif currColor = Green2 then
                redOut   <= (currPixelMatrix(0, 1) + currPixelMatrix(2, 1)) / 2;
                greenOut <= currPixelMatrix(1, 1);
                blueOut  <= (currPixelMatrix(1, 0) + currPixelMatrix(1, 2)) / 2;
            else
                redOut   <= X"00";
                greenOut <= X"00";
                blueOut  <= X"00";
            end if;
        end if;
    end process demosaicProc;

    -- assign out signals
    -- skip image fringes (upper, lower, left, right)
    currXOut <= widthOut;
    currYOut <= heightOut;

end architecture RTL;
