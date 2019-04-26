library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ccd_pkg.all;
use work.kernel_pkg.all;

entity color_kernel is
    generic(
        -- edge detection kernel
        kernelParams   : Convolution_Params   := (
            (-1, -1, -1),
            (-1, 8, -1),
            (-1, -1, -1)
        );
        prescaleAmount : Convolution_Prescale := 0
    );
    port(
        clkIn, rstAsyncIn        : in  std_logic;
        pixelIn                  : in  Pixel_Aggregate;
        -- true if new pixel available
        newPixelIn               : in  boolean;
        -- true if frame ended
        frameEndIn               : in  boolean;
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

    signal stage1Out                                         : Stage_Out(open)(STAGE1_AMOUNT - 1 downto 0);
    signal stage2Out                                         : Stage_Out(open)(STAGE2_AMOUNT - 1 downto 0);
    signal stage3Out                                         : Stage_Out(open)(STAGE3_AMOUNT - 1 downto 0);
    signal stage1Ready, stage2Ready, stage3Ready             : boolean := false;
    signal stage1LastPixel, stage2LastPixel, stage3LastPixel : boolean := false;

    impure function isImageFringe return boolean is
    begin
        return currShiftHeight = 0 or currShiftHeight = IMG_WIDTH - 1 or currShiftWidth = 0 or currShiftWidth = IMG_WIDTH - 1;
    end function isImageFringe;
begin
    shiftProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            currShiftWidth  <= 0;
            currShiftHeight <= 0;
            pixelCounter    <= 0;
        elsif rising_edge(clkIn) then

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

            elsif frameEndIn then
                -- reset counters on start of new frame
                pixelCounter    <= 0;
                currShiftWidth  <= 0;
                currShiftHeight <= 0;
            end if;

        end if;
    end process shiftProc;

    convStage1 : process(clkIn, rstAsyncIn)
        variable leftX, leftY, rightX, rightY : natural;
    begin
        if rstAsyncIn = '1' then
            for currColor in Pixel_Color loop
                for i in 0 to STAGE1_AMOUNT - 1 loop
                    stage1Out(currColor)(i) <= X"000";
                end loop;
            end loop;
        elsif rising_edge(clkIn) then
            -- propagate new pixel
            stage1Ready     <= not isImageFringe and newPixelIn;
            stage1LastPixel <= currShiftHeight = IMG_WIDTH - 2 and currShiftHeight = IMG_HEIGHT - 2;

            if not isImageFringe then
                if newPixelIn then
                    for currColor in Pixel_Color loop
                        for i in 0 to STAGE1_AMOUNT - 2 loop
                            leftY  := i / 3;
                            rightY := (8 - i) / 3;
                            leftX  := i mod 3;
                            rightX := (8 - i) mod 3;

                            stage1Out(currColor)(i) <= (signed(resize(pixelMatrix(currColor)(leftY, leftX), PIPELINE_SIZE)) * kernelParams(leftY, leftX)) + (signed(resize(pixelMatrix(currColor)(rightY, rightX), PIPELINE_SIZE)) * kernelParams(rightY, rightX));
                        end loop;
                        stage1Out(currColor)(4) <= (signed(resize(pixelMatrix(currColor)(1, 1), PIPELINE_SIZE)) * kernelParams(1, 1));
                    end loop;
                end if;
            end if;
        end if;

    end process convStage1;

    convStage2 : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            for currColor in Pixel_Color loop
                for i in 0 to STAGE2_AMOUNT - 1 loop
                    stage2Out(currColor)(i) <= X"000";
                end loop;
            end loop;
        elsif rising_edge(clkIn) then
            -- propagate new pixel
            stage2Ready     <= stage1Ready;
            stage2LastPixel <= stage1LastPixel;

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
                    stage3Out(currColor)(i) <= X"000";
                end loop;
            end loop;
        elsif rising_edge(clkIn) then
            -- propagate new pixel
            stage3Ready     <= stage2Ready;
            stage3LastPixel <= stage2LastPixel;

            if stage2Ready then
                for currColor in Pixel_Color loop
                    for i in 0 to STAGE3_AMOUNT - 2 loop
                        stage3Out(currColor)(i) <= stage2Out(currColor)(i) + stage2Out(currColor)(STAGE2_AMOUNT - 1 - i);
                    end loop;
                    stage3Out(currColor)(1) <= stage2Out(currColor)(2);
                end loop;
            end if;
        end if;
    end process convStage3;

    convStage4 : process(clkIn, rstAsyncIn)
        variable tmp         : Pipeline_Pixel := X"000";
        variable tmpUnsigned : Pixel_Data     := X"00";
    begin
        if rstAsyncIn = '1' then
            for currColor in Pixel_Color loop
                pixelOut(currColor) <= X"00";
            end loop;
        elsif rising_edge(clkIn) then
            -- propagate new pixel
            newPixelOut <= stage3Ready;
            frameEndOut <= stage3LastPixel;

            if stage3Ready then
                for currColor in Pixel_Color loop
                    tmp         := stage3Out(currColor)(0) + stage3Out(currColor)(1);
                    tmp         := tmp / (2 ** prescaleAmount);
                    tmpUnsigned := toSaturatedUnsigned(tmp, IMG_CONSTS.pixel_data_size);

                    -- convert back to unsigned, we can be sure the number is correct unsigned value because of IF block
                    pixelOut(currColor) <= tmpUnsigned;
                end loop;
            end if;
        end if;
    end process convStage4;

    redShiftReg : entity work.pixel_shiftreg
        generic map(
            SHIFT_LEN => (2 * IMG_WIDTH) + 3,
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
            SHIFT_LEN => (2 * IMG_WIDTH) + 3,
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
            SHIFT_LEN => (2 * IMG_WIDTH) + 3,
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
