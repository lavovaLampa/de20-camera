library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.common_pkg.all;
use work.kernel_pkg.all;

entity color_kernel is
    generic(
        -- edge detection kernel
        constant kernelParams   : Convolution_Params   := (
            (-1, -1, -1),
            (-1, 8, -1),
            (-1, -1, -1)
        );
        constant prescaleAmount : Convolution_Prescale := 0
    );
    port(
        clkIn, rstAsyncIn        : in  std_logic;
        pixelIn                  : in  Pixel_Aggregate;
        newPixelIn, frameEndIn   : in  boolean;
        pixelOut                 : out Pixel_Aggregate;
        newPixelOut, frameEndOut : out boolean
    );
end entity color_kernel;

architecture RTL of color_kernel is
    -- pixel shift register OUTPUT
    signal pixelMatrix : Matrix_Aggregate := (others => (others => (others => X"00"))); -- that's a lOOOOng initialization

    signal pixelCounter    : Pixel_Count_Range := 0;
    signal currShiftWidth  : Img_Width_Range   := 0;
    signal currShiftHeight : Img_Height_Range  := 0;

    signal pixelInBuffer : boolean := false;

    signal stage1Out                                      : Stage_Out(open)(STAGE1_AMOUNT - 1 downto 0);
    signal stage2Out                                      : Stage_Out(open)(STAGE2_AMOUNT - 1 downto 0);
    signal stage3Out                                      : Stage_Out(open)(STAGE3_AMOUNT - 1 downto 0);
    signal stage1Ready, stage2Ready, stage3Ready          : boolean := false;
    signal stage1FrameEnd, stage2FrameEnd, stage3FrameEnd : boolean := false;

    impure function isImageEdge return boolean is
    begin
        return currShiftHeight = 0 or currShiftHeight = IMG_WIDTH - 1 or currShiftWidth = 0 or currShiftWidth = IMG_WIDTH - 1;
    end function isImageEdge;

    impure function isFrameEnd return boolean is
    begin
        return frameEndIn or pixelCounter >= IMG_WIDTH * IMG_HEIGHT;
    end function isFrameEnd;
begin
    shiftProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            currShiftWidth  <= 0;
            currShiftHeight <= 0;
            pixelCounter    <= 0;
        elsif rising_edge(clkIn) then
            pixelInBuffer <= newPixelIn;

            -- counter logic
            if newPixelIn then
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
            end if;

            -- reset counters on start of new frame
            if isFrameEnd then
                pixelCounter    <= 0;
                currShiftWidth  <= 0;
                currShiftHeight <= 0;
            end if;
        end if;
    end process shiftProc;

    convStage1 : process(clkIn, rstAsyncIn)
        variable leftX, leftY, rightX, rightY    : natural;
        variable mulAcc, leftMulAcc, rightMulAcc : signed(13 downto 0);
    begin
        if rstAsyncIn = '1' then
            for currColor in Pixel_Color loop
                for i in 0 to STAGE1_AMOUNT - 1 loop
                    stage1Out(currColor)(i) <= (others => '0');
                end loop;
            end loop;
            stage1Ready    <= false;
            stage1FrameEnd <= false;
        elsif rising_edge(clkIn) then
            -- propagate new pixel
            stage1Ready    <= not isImageEdge and pixelInBuffer;
            stage1FrameEnd <= isFrameEnd;

            if not isImageEdge and newPixelIn then
                for currColor in Pixel_Color loop
                    --                    report "Current color: " & Pixel_Color'image(currColor);
                    for i in 0 to STAGE1_AMOUNT - 2 loop
                        leftY  := i / 3;
                        rightY := (8 - i) / 3;
                        leftX  := i mod 3;
                        rightX := (8 - i) mod 3;

                        leftMulAcc  := signed(resize(pixelMatrix(currColor)(leftY, leftX), PIXEL_SIZE + 1)) * to_signed(kernelParams(leftY, leftX), 5);
                        rightMulAcc := signed(resize(pixelMatrix(currColor)(rightY, rightX), PIXEL_SIZE + 1)) * to_signed(kernelParams(rightY, rightX), 5);

                        stage1Out(currColor)(i) <= resize(leftMulAcc, PIPELINE_SIZE) + resize(rightMulAcc, PIPELINE_SIZE);
                    end loop;
                    mulAcc                  := signed(resize(pixelMatrix(currColor)(1, 1), 9)) * to_signed(kernelParams(1, 1), 5);
                    stage1Out(currColor)(4) <= resize(mulAcc, PIPELINE_SIZE);
                end loop;
            end if;
        end if;

    end process convStage1;

    convStage2 : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            for currColor in Pixel_Color loop
                for i in 0 to STAGE2_AMOUNT - 1 loop
                    stage2Out(currColor)(i) <= (others => '0');
                end loop;
            end loop;
            stage2Ready    <= false;
            stage2FrameEnd <= false;
        elsif rising_edge(clkIn) then
            -- propagate new pixel
            stage2Ready    <= stage1Ready;
            stage2FrameEnd <= stage1FrameEnd;

            if stage1Ready then
                for currColor in Pixel_Color loop
                    for i in 0 to STAGE2_AMOUNT - 2 loop
                        stage2Out(currColor)(i) <= stage1Out(currColor)(i) + stage1Out(currColor)(STAGE1_AMOUNT - 1 - i);
                    end loop;
                    stage2Out(currColor)(2) <= stage1Out(currColor)(2);
                end loop;
            end if;
        end if;
    end process convStage2;

    convStage3 : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            for currColor in Pixel_Color loop
                for i in 0 to STAGE3_AMOUNT - 1 loop
                    stage3Out(currColor)(i) <= (others => '0');
                end loop;
            end loop;
            stage3Ready    <= false;
            stage3FrameEnd <= false;
        elsif rising_edge(clkIn) then
            -- propagate new pixel
            stage3Ready    <= stage2Ready;
            stage3FrameEnd <= stage2FrameEnd;

            if stage2Ready then
                for currColor in Pixel_Color loop
                    for i in 0 to STAGE3_AMOUNT - 2 loop
                        stage3Out(currColor)(i) <= stage2Out(currColor)(i) + stage2Out(currColor)(STAGE2_AMOUNT - 1 - i);
                    end loop;
                    stage3Out(currColor)(1) <= stage2Out(currColor)(1);
                end loop;
            end if;
        end if;
    end process convStage3;

    convStage4 : process(clkIn, rstAsyncIn)
        variable tmp         : Pipeline_Pixel := (others => '0');
        variable tmpUnsigned : Pixel_Data     := X"00";
    begin
        if rstAsyncIn = '1' then
            for currColor in Pixel_Color loop
                pixelOut(currColor) <= X"00";
            end loop;
            newPixelOut <= false;
            frameEndOut <= false;
        elsif rising_edge(clkIn) then
            -- propagate new pixel
            newPixelOut <= stage3Ready;
            frameEndOut <= stage3FrameEnd;

            if stage3Ready then
                for currColor in Pixel_Color loop
                    tmp         := stage3Out(currColor)(0) + stage3Out(currColor)(1);
                    tmp         := tmp / (2 ** prescaleAmount);
                    tmpUnsigned := toSaturatedUnsigned(tmp, IMG_CONSTS.pixel_size);

                    -- convert back to unsigned, we can be sure the number is correct unsigned value because of IF block
                    pixelOut(currColor) <= tmpUnsigned;
                end loop;
            end if;
        end if;
    end process convStage4;

    redShiftReg : entity work.pixel_shiftreg
        generic map(
            SHIFT_LEN  => (2 * IMG_WIDTH) + 3,
            LINE_WIDTH => IMG_WIDTH
        )
        port map(
            clkIn      => clkIn,
            rstAsyncIn => rstAsyncIn,
            -- TODO: parametrize
            dataIn     => pixelIn(Red),
            enableIn   => newPixelIn,
            pixelsOut  => pixelMatrix(Red)
        );

    greenShiftReg : entity work.pixel_shiftreg
        generic map(
            SHIFT_LEN  => (2 * IMG_WIDTH) + 3,
            LINE_WIDTH => IMG_WIDTH
        )
        port map(
            clkIn      => clkIn,
            rstAsyncIn => rstAsyncIn,
            -- TODO: parametrize
            dataIn     => pixelIn(Green),
            enableIn   => newPixelIn,
            pixelsOut  => pixelMatrix(Green)
        );

    blueShiftReg : entity work.pixel_shiftreg
        generic map(
            SHIFT_LEN  => (2 * IMG_WIDTH) + 3,
            LINE_WIDTH => IMG_WIDTH
        )
        port map(
            clkIn      => clkIn,
            rstAsyncIn => rstAsyncIn,
            -- TODO: parametrize
            dataIn     => pixelIn(Blue),
            enableIn   => newPixelIn,
            pixelsOut  => pixelMatrix(Blue)
        );

end architecture RTL;
