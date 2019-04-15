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
    alias PIXEL_SIZE is IMG_CONSTS.pixel_data_size;
    subtype Pixel_Range is natural range PIXEL_SIZE - 1 downto 0;
    constant PIPELINE_SIZE : natural := PIXEL_SIZE + 2;
    -- pipeline stage has to be wide enough not to overflow during addition
    subtype Pipeline_Pixel is unsigned(PIPELINE_SIZE - 1 downto 0);
end entity ccd_ctrl;

architecture RTL of ccd_ctrl is
    -- pixel shift register OUTPUT
    signal pixelMatrix : Pixel_Matrix := (others => (others => X"00"));

    -- shiftProc (exposes all information)
    signal currPixelValid : boolean           := true;
    signal pixelCounter   : Pixel_Count_Range := 0;
    signal matrixWidth    : Img_Width_Range   := 0; -- width of pixel at the center of matrix
    signal matrixHeight   : Img_Height_Range  := 0; -- height of pixel at the center of matrix
    signal hasNewPixel    : boolean           := false;

    -- demosaicStage1 (internal)
    signal pipelineWidth  : Img_Width_Range  := 0; -- width of pixel in 1st pipeline stage
    signal pipelineHeight : Img_Height_Range := 0; -- height of pixel in 1st pipeline stage

    -- demosaicStage1 (API)
    signal outWidth                   : Img_Width_Range  := 0; -- width of pixel at the pipeline output
    signal outHeight                  : Img_Height_Range := 0; -- height of pixel at the pipeline output
    signal color11stage, color12stage : Pipeline_Pixel   := B"0000_0000_00";
    signal color21stage, color22stage : Pipeline_Pixel   := B"0000_0000_00";
    signal color3stage                : Pixel_Data       := B"0000_0000";
    signal stageColor                 : Pixel_Color      := Green1;
    signal pipelineReady              : boolean          := false;

    impure function isImageFringe return boolean is
    begin
        return matrixHeight = 0 or matrixHeight = IMG_WIDTH - 1 or matrixWidth = 0 or matrixWidth = IMG_WIDTH - 1;
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
            pixelsOut  => pixelMatrix
        );

    shiftProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            matrixWidth  <= 0;
            matrixHeight <= 0;
            pixelCounter <= 0;
            hasNewPixel  <= false;
        elsif rising_edge(clkIn) then

            -- counter logic
            if currPixelValid then
                -- counts received valid pixels in current frame
                pixelCounter <= pixelCounter + 1;

                hasNewPixel <= true;

                -- current width & height of pixel in shift register
                if pixelCounter > IMG_WIDTH + 1 then
                    if matrixWidth >= IMG_WIDTH - 1 then
                        matrixWidth  <= 0;
                        matrixHeight <= matrixHeight + 1;
                    else
                        matrixWidth <= matrixWidth + 1;
                    end if;
                end if;

            elsif frameValidIn = '0' and lineValidIn = '0' then
                -- reset counters on start of new frame
                pixelCounter <= 0;
                matrixWidth  <= 0;
                matrixHeight <= 0;
                hasNewPixel  <= false;
            else
                hasNewPixel <= false;
            end if;

        end if;
    end process shiftProc;

    demosaicStage1 : process(clkIn, rstAsyncIn)
        variable currColor : Pixel_Color := getCurrColor(matrixWidth, matrixHeight);
    begin
        if rstAsyncIn = '1' then
            pipelineWidth  <= 0;
            pipelineHeight <= 0;
        elsif rising_edge(clkIn) then
            outWidth      <= pipelineWidth;
            outHeight     <= pipelineHeight;
            pipelineReady <= hasNewPixel and not isImageFringe;
            currColor     := getCurrColor(matrixWidth, matrixHeight);

            -- ignore image fringes
            if not isImageFringe then
                if hasNewPixel then
                    if pipelineWidth >= IMG_WIDTH - 3 then
                        pipelineWidth  <= 0;
                        pipelineHeight <= pipelineHeight + 1;
                    -- ugly hack, but what can you do :)
                    elsif matrixHeight = 1 and matrixWidth = 1 then
                        pipelineWidth <= 0;
                    else
                        pipelineWidth <= pipelineWidth + 1;
                    end if;
                end if;
            elsif pixelCounter = 0 then
                pipelineHeight <= 0;
                pipelineWidth  <= 0;
            end if;

            -- demosaicing (first pipeline stage)
            if currColor = Red then
                stageColor   <= Red;
                -- red
                color3stage  <= pixelMatrix(1, 1);
                -- green
                color11stage <= resize(pixelMatrix(0, 1), PIPELINE_SIZE) + resize(pixelMatrix(1, 0), PIPELINE_SIZE);
                color12stage <= resize(pixelMatrix(1, 2), PIPELINE_SIZE) + resize(pixelMatrix(2, 1), PIPELINE_SIZE);
                -- blue
                color21stage <= resize(pixelMatrix(0, 0), PIPELINE_SIZE) + resize(pixelMatrix(0, 2), PIPELINE_SIZE);
                color22stage <= resize(pixelMatrix(2, 0), PIPELINE_SIZE) + resize(pixelMatrix(2, 2), PIPELINE_SIZE);
            elsif currColor = Blue then
                stageColor   <= Blue;
                -- blue
                color3stage  <= pixelMatrix(1, 1);
                -- green
                color11stage <= resize(pixelMatrix(0, 1), PIPELINE_SIZE) + resize(pixelMatrix(1, 0), PIPELINE_SIZE);
                color12stage <= resize(pixelMatrix(1, 2), PIPELINE_SIZE) + resize(pixelMatrix(2, 1), PIPELINE_SIZE);
                -- red
                color21stage <= resize(pixelMatrix(0, 0), PIPELINE_SIZE) + resize(pixelMatrix(0, 2), PIPELINE_SIZE);
                color22stage <= resize(pixelMatrix(2, 0), PIPELINE_SIZE) + resize(pixelMatrix(2, 2), PIPELINE_SIZE);
            elsif currColor = Green1 then
                stageColor   <= Green1;
                -- green
                color3stage  <= pixelMatrix(1, 1);
                -- red
                color11stage <= resize(pixelMatrix(1, 0), PIPELINE_SIZE) + resize(pixelMatrix(1, 2), PIPELINE_SIZE);
                -- blue
                color21stage <= resize(pixelMatrix(0, 1), PIPELINE_SIZE) + resize(pixelMatrix(2, 1), PIPELINE_SIZE);
            elsif currColor = Green2 then
                stageColor   <= Green2;
                -- green
                color3stage  <= pixelMatrix(1, 1);
                -- blue
                color11stage <= resize(pixelMatrix(1, 0), PIPELINE_SIZE) + resize(pixelMatrix(1, 2), PIPELINE_SIZE);
                -- red
                color21stage <= resize(pixelMatrix(0, 1), PIPELINE_SIZE) + resize(pixelMatrix(2, 1), PIPELINE_SIZE);
            else
                report "this should not happen" severity failure;
            end if;

        end if;
    end process demosaicStage1;

    demosaicStage2 : process(clkIn, rstAsyncIn)
        variable pipelineAcc : Pipeline_Pixel := B"0000_0000_00";
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
                    redOut <= color3stage;

                    pipelineAcc := (color11stage + color12stage) / 4;
                    greenOut    <= pipelineAcc(Pixel_Range);

                    pipelineAcc := (color21stage + color22stage) / 4;
                    blueOut     <= pipelineAcc(Pixel_Range);
                elsif stageColor = Blue then
                    blueOut <= color3stage;

                    pipelineAcc := (color11stage + color12stage) / 4;
                    greenOut    <= pipelineAcc(Pixel_Range);

                    pipelineAcc := (color21stage + color22stage) / 4;
                    redOut      <= pipelineAcc(Pixel_Range);
                elsif stageColor = Green1 then
                    greenOut <= color3stage;

                    pipelineAcc := color11stage / 2;
                    redOut      <= pipelineAcc(Pixel_Range);

                    pipelineAcc := color21stage / 2;
                    blueOut     <= pipelineAcc(Pixel_Range);
                elsif stageColor = Green2 then
                    greenOut <= color3stage;

                    pipelineAcc := color11stage / 2;
                    blueOut     <= pipelineAcc(Pixel_Range);

                    pipelineAcc := color21stage / 2;
                    redOut      <= pipelineAcc(Pixel_Range);
                end if;
            end if;
        end if;
    end process demosaicStage2;

    currXOut <= outWidth;
    currYOut <= outHeight;

end architecture RTL;
