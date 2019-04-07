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
    -- set in controlProc
    signal currPixelValid : boolean := true;
    signal pixelCounter   : natural := 0;

    -- set in demosaicProc
    signal currWidth  : Img_Width_Range  := 0;
    signal currHeight : Img_Height_Range := 0;
    signal currColor  : Pixel_Color      := getCurrColor(currWidth, currHeight);

    -- pixel shift register OUTPUT
    signal currPixelMatrix : Pixel_Matrix := (others => (others => X"00"));
begin

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
    begin
        if rstAsyncIn = '1' then
            currPixelValid <= true;
            pixelCounter   <= 0;
        elsif rising_edge(clkIn) then
            -- self-explanatory, is current pixel valid?
            currPixelValid <= frameValidIn = '1' and lineValidIn = '1';

            if frameValidIn = '1' and lineValidIn = '1' then
                -- increment pixel count
                pixelCounter <= pixelCounter + 1;
            elsif frameValidIn = '0' and lineValidIn = '0' then
                pixelCounter <= 0;
            end if;
        end if;
    end process controlProc;

    demosaicProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            currWidth     <= 0;
            currHeight    <= 0;
            currColor     <= getCurrColor(0, 0);
            pixelValidOut <= false;
        elsif rising_edge(clkIn) then
            currColor <= getCurrColor(currWidth, currHeight);

            -- reset counter on start of new frame
            if pixelCounter = 0 then
                currWidth     <= 0;
                currHeight    <= 0;
            -- if we have enough data in shift register, we can start demosaicing
            elsif currPixelValid and pixelCounter >= SHIFT_AMOUNT then
                if currWidth >= IMG_WIDTH - 1 then
                    currWidth  <= 0;
                    currHeight <= currHeight + 1;
                else
                    currWidth <= currWidth + 1;
                end if;
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
            end if;
        end if;
    end process demosaicProc;

    -- asign out signals
    pixelValidOut <= currPixelValid and pixelCounter >= SHIFT_AMOUNT;
    currXOut <= currWidth;
    currYOut <= currHeight;

end architecture RTL;
