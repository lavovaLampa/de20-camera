library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ccd_ctrl_pkg.all;
use work.common_pkg.all;

entity ccd_ctrl is
    port(
        clkIn, rstAsyncIn          : in  std_logic;
        frameValidIn, lineValidIn  : in  std_logic;
        pixelDataIn                : in  CCD_Pixel_Data_T;
        pixelOut                   : out Pixel_Aggregate;
        pixelValidOut, frameEndOut : out boolean
    );
end entity ccd_ctrl;

architecture RTL of ccd_ctrl is
    -- ccd state (not registers)
    signal currPixelValid : boolean := true;
    signal hSync, vSync   : boolean := false;

    -- pixel shift register OUTPUT
    signal pixelMatrix : Pixel_Matrix := (others => (others => X"00"));

    -- shiftProc (exposes all information)
    signal pixelCounter          : Pixel_Count_Range := 0;
    signal currShiftWidth        : Img_Width_Range   := 0; -- width of pixel at the center of matrix
    signal currShiftHeight       : Img_Height_Range  := 0; -- height of pixel at the center of matrix
    signal hasNewPixel, frameEnd : boolean           := false;

    -- demosaicStage1 (API)
    signal color1Out, color2Out : Stage_Out       := (others => (others => '0'));
    signal color3Out            : Pixel_Data      := B"0000_0000";
    signal stageFrameEnd        : boolean         := false;
    signal stageColor           : CCD_Pixel_Color_T := Green1;
    signal pipelineReady        : boolean         := false;

    impure function isImageEdge return boolean is
    begin
        return currShiftHeight = 0 or currShiftHeight = IMG_WIDTH - 1 or currShiftWidth = 0 or currShiftWidth = IMG_WIDTH - 1;
    end function isImageEdge;
begin

    -- self-explanatory, is current pixel valid?
    currPixelValid <= frameValidIn = '1' and lineValidIn = '1';
    hSync          <= frameValidIn = '1' and lineValidIn = '0';
    vSync          <= frameValidIn = '0' and lineValidIn = '0';

    shiftProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            currShiftWidth  <= 0;
            currShiftHeight <= 0;
            pixelCounter    <= 0;
            hasNewPixel     <= false;
        elsif rising_edge(clkIn) then
            hasNewPixel <= currPixelValid;
            frameEnd    <= vSync;

            -- counter logic
            if currPixelValid then
                -- counts received valid pixels in current frame
                pixelCounter <= pixelCounter + 1;

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
            end if;

        end if;
    end process shiftProc;

    demosaicStage1 : process(clkIn, rstAsyncIn)
        variable currColor     : CCD_Pixel_Color_T := getCurrColor(currShiftWidth, currShiftHeight);
        variable resizedMatrix : Pipeline_Matrix := (others => (others => (others => '0')));
    begin
        if rstAsyncIn = '1' then
            pipelineReady <= false;
            stageColor    <= Green1;
            color1Out     <= (others => (others => '0'));
            color2Out     <= (others => (others => '0'));
            color3Out     <= (others => '0');
            stageFrameEnd <= false;
        elsif rising_edge(clkIn) then
            currColor := getCurrColor(currShiftWidth, currShiftHeight);

            -- ignore image edges
            pipelineReady <= hasNewPixel and not isImageEdge;
            stageColor    <= currColor;
            stageFrameEnd <= frameEnd;

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
    end process demosaicStage1;

    demosaicStage2 : process(clkIn, rstAsyncIn)
        variable pipelineAcc : Pipeline_Pixel := B"0000_0000_00";
    begin
        if rstAsyncIn = '1' then
            pixelOut(Red)   <= X"00";
            pixelOut(Green) <= X"00";
            pixelOut(Blue)  <= X"00";
            pixelValidOut   <= false;
            frameEndOut     <= false;
        elsif rising_edge(clkIn) then
            pixelValidOut <= pipelineReady;
            frameEndOut   <= frameEnd;

            if pipelineReady then
                case stageColor is
                    when Red =>
                        pixelOut(Red) <= color3Out;

                        pipelineAcc     := (color1Out(0) + color1Out(1)) / 4;
                        pixelOut(Green) <= pipelineAcc(Pixel_Range);

                        pipelineAcc    := (color2Out(0) + color2Out(1)) / 4;
                        pixelOut(Blue) <= pipelineAcc(Pixel_Range);

                    when Blue =>
                        pixelOut(Blue) <= color3Out;

                        pipelineAcc     := (color1Out(0) + color1Out(1)) / 4;
                        pixelOut(Green) <= pipelineAcc(Pixel_Range);

                        pipelineAcc   := (color2Out(0) + color2Out(1)) / 4;
                        pixelOut(Red) <= pipelineAcc(Pixel_Range);

                    when Green1 =>
                        pixelOut(Green) <= color3Out;

                        pipelineAcc   := color1Out(0) / 2;
                        pixelOut(Red) <= pipelineAcc(Pixel_Range);

                        pipelineAcc    := color2Out(0) / 2;
                        pixelOut(Blue) <= pipelineAcc(Pixel_Range);

                    when Green2 =>
                        pixelOut(Green) <= color3Out;

                        pipelineAcc    := color1Out(0) / 2;
                        pixelOut(Blue) <= pipelineAcc(Pixel_Range);

                        pipelineAcc   := color2Out(0) / 2;
                        pixelOut(Red) <= pipelineAcc(Pixel_Range);
                end case;
            end if;
        end if;
    end process demosaicStage2;

    pixelShiftReg : entity work.pixel_shiftreg
        generic map(
            SHIFT_LEN  => (2 * IMG_WIDTH) + 3,
            LINE_WIDTH => IMG_WIDTH
        )
        port map(
            clkIn      => clkIn,
            rstAsyncIn => rstAsyncIn,
            -- TODO: parametrize
            dataIn     => unsigned(pixelDataIn(pixelDataIn'high downto 4)),
            enableIn   => currPixelValid,
            pixelsOut  => pixelMatrix
        );

end architecture RTL;
