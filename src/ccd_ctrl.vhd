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

    signal currPixelValid : boolean           := true;
    signal pixelCounter   : Pixel_Count_Range := 0;
    signal currWidth      : Img_Width_Range   := 0;
    signal currHeight     : Img_Height_Range  := 0;
    signal currColorDbg   : Pixel_Color       := Green1;
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

    controlProc : process(clkIn, rstAsyncIn)
        variable currColor : Pixel_Color := getCurrColor(currWidth, currHeight);
    begin
        if rstAsyncIn = '1' then
            currWidth    <= 0;
            currHeight   <= 0;
            pixelCounter <= 0;
            redOut       <= X"00";
            greenOut     <= X"00";
            blueOut      <= X"00";
        elsif rising_edge(clkIn) then
            currColor := getCurrColor(currWidth, currHeight);
            -- debug statement
            currColorDbg <= currColor;

            -- counter logic
            if currPixelValid then
                pixelCounter <= pixelCounter + 1;

                -- current width & height of pixel in shift register
                if pixelCounter > IMG_WIDTH + 1 then
                    if currWidth >= IMG_WIDTH - 1 then
                        currWidth  <= 0;
                        currHeight <= currHeight + 1;
                    else
                        currWidth <= currWidth + 1;
                    end if;
                end if;
            elsif frameValidIn = '0' and lineValidIn = '0' then
                -- reset counters on start of new frame
                pixelCounter <= 0;
                currWidth    <= 0;
                currHeight   <= 0;
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
    end process controlProc;

    -- asign out signals
    -- skip image fringes (upper, lower, left, right)
    pixelValidOut <= currPixelValid and (currHeight > 0 and currHeight < IMG_HEIGHT - 1) and (currWidth > 0 and currWidth < IMG_WIDTH - 1);
    currXOut      <= currWidth;
    currYOut      <= currHeight;

end architecture RTL;
