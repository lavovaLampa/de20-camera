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

    -- shiftProc (exposes all information)
    signal currPixelValid  : boolean           := true;
    signal pixelCounter    : Pixel_Count_Range := 0;
    signal currShiftWidth  : Img_Width_Range   := 0;
    signal currShiftHeight : Img_Height_Range  := 0;
    signal hasNewPixel     : boolean           := false;

    -- demosaicStage1 (internal)
    signal currWidth  : Img_Width_Range  := 0;
    signal currHeight : Img_Height_Range := 0;

    -- demosaicStage1 (API)
    signal pipelineWidth              : Img_Width_Range      := 0;
    signal pipelineHeight             : Img_Height_Range     := 0;
    signal color11stage, color12stage : unsigned(8 downto 0) := B"0000_0000_0";
    signal color21stage, color22stage : unsigned(8 downto 0) := B"0000_0000_0";
    signal color3stage                : Pixel_Data           := B"0000_0000";
    signal stageColor                 : Pixel_Color          := Green1;
    signal pipelineReady              : boolean              := false;

    -- demosaicStage2

    impure function isImageFringe return boolean is
    begin
        return currShiftHeight = 0 or currShiftHeight = IMG_WIDTH - 1 or currShiftWidth = 0 or currShiftWidth = IMG_WIDTH - 1;
    end function isImageFringe;
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
            hasNewPixel     <= false;
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

    demosaicStage1 : process(clkIn, rstAsyncIn)
        variable currColor : Pixel_Color := getCurrColor(currShiftWidth, currShiftHeight);
    begin
        if rstAsyncIn = '1' then
            currWidth  <= 0;
            currHeight <= 0;
        elsif rising_edge(clkIn) then
            pipelineWidth  <= currWidth;
            pipelineHeight <= currHeight;
            pipelineReady  <= hasNewPixel and not isImageFringe;
            currColor      := getCurrColor(currShiftWidth, currShiftHeight);

            -- ignore image fringes
            if not isImageFringe then
                if hasNewPixel then
                    if currWidth >= IMG_WIDTH - 3 then
                        currWidth  <= 0;
                        currHeight <= currHeight + 1;
                    -- ugly hack, but what can you do :)
                    elsif currShiftHeight = 1 and currShiftWidth = 1 then
                        currWidth <= 0;
                    else
                        currWidth <= currWidth + 1;
                    end if;
                end if;
            elsif pixelCounter = 0 then
                currHeight <= 0;
                currWidth  <= 0;
            end if;

            -- demosaicing (first pipeline stage)
            if currColor = Red then
                stageColor   <= Red;
                -- red
                color3stage  <= currPixelMatrix(1, 1);
                -- green
                color11stage <= widen(currPixelMatrix(0, 1)) + widen(currPixelMatrix(1, 0));
                color12stage <= widen(currPixelMatrix(1, 2)) + widen(currPixelMatrix(2, 1));
                -- blue
                color21stage <= widen(currPixelMatrix(0, 0)) + widen(currPixelMatrix(0, 2));
                color22stage <= widen(currPixelMatrix(2, 0)) + widen(currPixelMatrix(2, 2));
            elsif currColor = Blue then
                stageColor   <= Blue;
                -- blue
                color3stage  <= currPixelMatrix(1, 1);
                -- green
                color11stage <= widen(currPixelMatrix(0, 1)) + widen(currPixelMatrix(1, 0));
                color12stage <= widen(currPixelMatrix(1, 2)) + widen(currPixelMatrix(2, 1));
                -- red
                color21stage <= widen(currPixelMatrix(0, 0)) + widen(currPixelMatrix(0, 2));
                color22stage <= widen(currPixelMatrix(2, 0)) + widen(currPixelMatrix(2, 2));
            elsif currColor = Green1 then
                stageColor   <= Green1;
                -- green
                color3stage  <= currPixelMatrix(1, 1);
                -- red
                color11stage <= widen(currPixelMatrix(1, 0)) + widen(currPixelMatrix(1, 2));
                -- blue
                color21stage <= widen(currPixelMatrix(0, 1)) + widen(currPixelMatrix(2, 1));
            elsif currColor = Green2 then
                stageColor   <= Green2;
                -- green
                color3stage  <= currPixelMatrix(1, 1);
                -- blue
                color11stage <= widen(currPixelMatrix(1, 0)) + widen(currPixelMatrix(1, 2));
                -- red
                color21stage <= widen(currPixelMatrix(0, 1)) + widen(currPixelMatrix(2, 1));
            else
                report "this should not happen" severity failure;
            end if;

        end if;
    end process demosaicStage1;

    demosaicStage2 : process(clkIn, rstAsyncIn)
        variable pipelineAcc : unsigned(9 downto 0) := B"0000_0000_00";
    begin
        if rstAsyncIn = '1' then
            redOut        <= X"00";
            greenOut      <= X"00";
            blueOut       <= X"00";
            pixelValidOut <= false;
        elsif rising_edge(clkIn) then
            pixelValidOut <= pipelineReady;

            if pipelineReady then
                if stageColor = Red then
                    redOut <= color3stage(7 downto 0);

                    pipelineAcc := ((widen(color11stage) + widen(color12stage)) / 4);
                    greenOut    <= pipelineAcc(7 downto 0);

                    pipelineAcc := ((widen(color21stage) + widen(color22stage)) / 4);
                    blueOut     <= pipelineAcc(7 downto 0);
                elsif stageColor = Blue then
                    blueOut <= color3stage(7 downto 0);

                    pipelineAcc := ((widen(color11stage) + widen(color12stage)) / 4);
                    greenOut    <= pipelineAcc(7 downto 0);

                    pipelineAcc := ((widen(color21stage) + widen(color22stage)) / 4);
                    redOut      <= pipelineAcc(7 downto 0);
                elsif stageColor = Green1 then
                    greenOut <= color3stage(7 downto 0);

                    pipelineAcc := widen(color11stage) / 2;
                    redOut      <= pipelineAcc(7 downto 0);

                    pipelineAcc := widen(color21stage) / 2;
                    blueOut     <= pipelineAcc(7 downto 0);
                elsif stageColor = Green2 then
                    greenOut <= color3stage(7 downto 0);

                    pipelineAcc := widen(color11stage) / 2;
                    blueOut     <= pipelineAcc(7 downto 0);

                    pipelineAcc := widen(color21stage) / 2;
                    redOut      <= pipelineAcc(7 downto 0);
                end if;
            end if;
        end if;
    end process demosaicStage2;

    currXOut <= pipelineWidth;
    currYOut <= pipelineHeight;

end architecture RTL;
