library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.ccd_demosaic_pkg.all;
use work.ccd_pkg.all;
use work.img_pkg.Pixel_Data_T;
use work.img_pkg.Pixel_Matrix_T;
use work.img_pkg.Pixel_Color_T;
use work.img_pkg.Pixel_Aggregate_T;

entity ccd_demosaic is
    port(
        clkIn, rstAsyncIn              : in  std_logic;
        -- ccd ctrl in
        pixelIn                        : in  Pixel_Data_T;
        pixelValidIn, frameEndStrobeIn : in  boolean;
        pixelCounterIn                 : in  Ccd_Img_Pixel_Ptr_T;
        -- data out
        pixelOut                       : out Pixel_Aggregate_T;
        newPixelOut, frameEndOut       : out boolean
    );
end entity ccd_demosaic;

architecture RTL of ccd_demosaic is
    -- state registers
    signal currShiftHeight              : Ccd_Active_Height_Ptr_T := 0;
    signal currShiftWidth               : Ccd_Active_Width_Ptr_T  := 0;
    -- shift signals for one clock to account for shift register delay
    signal lastPixelValid, lastFrameEnd : boolean                 := false;

    -- helper state signals
    signal isImageEdge : boolean;
begin
    isImageEdge <= currShiftHeight = 0 or currShiftHeight = OUTPUT_HEIGHT - 1 or currShiftWidth = 0 or currShiftWidth = OUTPUT_WIDTH - 1;

    counterProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            currShiftHeight <= 0;
            currShiftWidth  <= 0;
            lastPixelValid  <= false;
            lastFrameEnd    <= false;
        elsif rising_edge(clkIn) then
            lastPixelValid <= pixelValidIn;
            lastFrameEnd   <= frameEndStrobeIn;

            if pixelValidIn then
                -- current width & height of pixel in shift register
                if pixelCounterIn > OUTPUT_WIDTH + 1 then
                    if currShiftWidth >= OUTPUT_WIDTH - 1 then
                        currShiftWidth <= 0;

                        if currShiftHeight >= OUTPUT_HEIGHT - 1 then
                            currShiftHeight <= 0;
                        else
                            currShiftHeight <= currShiftHeight + 1;
                        end if;
                    else
                        currShiftWidth <= currShiftWidth + 1;
                    end if;
                end if;

            elsif frameEndStrobeIn then
                -- reset counters on start of new frame
                currShiftWidth  <= 0;
                currShiftHeight <= 0;
            end if;
        end if;
    end process counterProc;

    demosaicPipeline : block
        -- pixel shift register output
        signal pixelMatrix : Pixel_Matrix_T := (others => (others => X"00"));

        -- demosaicStage1 output
        signal color1Out, color2Out : Stage_Out         := (others => (others => '0'));
        signal color3Out            : Pixel_Data_T      := B"0000_0000";
        signal stageFrameEnd        : boolean           := false;
        signal stageColor           : Ccd_Pixel_Color_T := Green1;
        signal pipelineReady        : boolean           := false;
    begin
        stage1Proc : process(clkIn, rstAsyncIn)
            variable currColor     : Ccd_Pixel_Color_T := Green1;
            variable resizedMatrix : Pipeline_Matrix   := (others => (others => (others => '0')));
        begin
            if rstAsyncIn = '1' then
                pipelineReady <= false;
                stageColor    <= Green1;
                color1Out     <= (others => (others => '0'));
                color2Out     <= (others => (others => '0'));
                color3Out     <= (others => '0');
                stageFrameEnd <= false;
            elsif rising_edge(clkIn) then
                currColor := get_ccd_pixel_color(currShiftHeight + CCD_CONFIGURATION.height_start, currShiftWidth + CCD_CONFIGURATION.width_start, CCD_CONFIGURATION.is_mirrored);

                -- ignore image edges
                pipelineReady <= lastPixelValid and not isImageEdge;
                stageColor    <= currColor;
                stageFrameEnd <= lastFrameEnd;

                for y in 0 to 2 loop
                    for x in 0 to 2 loop
                        resizedMatrix(y, x) := resize(pixelMatrix(y, x), PIPELINE_SIZE);
                    end loop;
                end loop;

                -- demosaicing (first pipeline stage)
                case currColor is
                    when Red =>
                        -- green
                        color1Out(0) <= resizedMatrix(0, 1) + resizedMatrix(1, 0);
                        color1Out(1) <= resizedMatrix(1, 2) + resizedMatrix(2, 1);
                        -- blue
                        color2Out(0) <= resizedMatrix(0, 0) + resizedMatrix(0, 2);
                        color2Out(1) <= resizedMatrix(2, 0) + resizedMatrix(2, 2);
                        -- red
                        color3Out    <= pixelMatrix(1, 1);

                    when Blue =>
                        -- green
                        color1Out(0) <= resizedMatrix(0, 1) + resizedMatrix(1, 0);
                        color1Out(1) <= resizedMatrix(1, 2) + resizedMatrix(2, 1);
                        -- red
                        color2Out(0) <= resizedMatrix(0, 0) + resizedMatrix(0, 2);
                        color2Out(1) <= resizedMatrix(2, 0) + resizedMatrix(2, 2);
                        -- blue
                        color3Out    <= pixelMatrix(1, 1);

                    when Green1 =>
                        -- red
                        color1Out(0) <= resizedMatrix(1, 0) + resizedMatrix(1, 2);
                        -- blue
                        color2Out(0) <= resizedMatrix(0, 1) + resizedMatrix(2, 1);
                        -- green
                        color3Out    <= pixelMatrix(1, 1);

                    when Green2 =>
                        -- blue
                        color1Out(0) <= resizedMatrix(1, 0) + resizedMatrix(1, 2);
                        -- red
                        color2Out(0) <= resizedMatrix(0, 1) + resizedMatrix(2, 1);
                        -- green
                        color3Out    <= pixelMatrix(1, 1);

                end case;
            end if;
        end process stage1Proc;

        stage2Proc : process(clkIn, rstAsyncIn)
            variable pipelineAcc : Pipeline_Pixel := B"0000_0000_00";
        begin
            if rstAsyncIn = '1' then
                pixelOut(Red)   <= X"00";
                pixelOut(Green) <= X"00";
                pixelOut(Blue)  <= X"00";
                newPixelOut     <= false;
                frameEndOut     <= false;
            elsif rising_edge(clkIn) then
                newPixelOut <= pipelineReady;
                frameEndOut <= stageFrameEnd;

                assert (newPixelOut xor frameEndOut) or (not newPixelOut and not frameEndOut);

                if pipelineReady then
                    case stageColor is
                        when Red =>
                            pixelOut(Red) <= color3Out;

                            pipelineAcc     := (color1Out(0) + color1Out(1)) / 4;
                            pixelOut(Green) <= pipelineAcc(Pixel_Range_T);

                            pipelineAcc    := (color2Out(0) + color2Out(1)) / 4;
                            pixelOut(Blue) <= pipelineAcc(Pixel_Range_T);

                        when Blue =>
                            pixelOut(Blue) <= color3Out;

                            pipelineAcc     := (color1Out(0) + color1Out(1)) / 4;
                            pixelOut(Green) <= pipelineAcc(Pixel_Range_T);

                            pipelineAcc   := (color2Out(0) + color2Out(1)) / 4;
                            pixelOut(Red) <= pipelineAcc(Pixel_Range_T);

                        when Green1 =>
                            pixelOut(Green) <= color3Out;

                            pipelineAcc   := color1Out(0) / 2;
                            pixelOut(Red) <= pipelineAcc(Pixel_Range_T);

                            pipelineAcc    := color2Out(0) / 2;
                            pixelOut(Blue) <= pipelineAcc(Pixel_Range_T);

                        when Green2 =>
                            pixelOut(Green) <= color3Out;

                            pipelineAcc    := color1Out(0) / 2;
                            pixelOut(Blue) <= pipelineAcc(Pixel_Range_T);

                            pipelineAcc   := color2Out(0) / 2;
                            pixelOut(Red) <= pipelineAcc(Pixel_Range_T);
                    end case;
                end if;
            end if;
        end process stage2Proc;

        pixelShiftReg : entity work.pixel_shiftreg
            generic map(
                SHIFT_LEN  => (2 * OUTPUT_WIDTH) + 3,
                LINE_WIDTH => OUTPUT_WIDTH
            )
            port map(
                clkIn      => clkIn,
                rstAsyncIn => rstAsyncIn,
                dataIn     => pixelIn,
                enableIn   => pixelValidIn,
                pixelsOut  => pixelMatrix
            );

    end block demosaicPipeline;
end architecture RTL;
