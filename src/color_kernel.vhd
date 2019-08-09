library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.kernel_pkg.all;
use work.img_pkg.Pixel_Aggregate_T;
use work.img_pkg.Pixel_Color_T;
use work.img_pkg.PIXEL_WIDTH;
use work.img_pkg.Pixel_Data_T;

entity color_kernel is
    generic(
        -- edge detection kernel
        constant KERNEL_PARAMS_MATRIX : Convolution_Params_T   := (
            (-1, -1, -1),
            (-1, 8, -1),
            (-1, -1, -1)
        );
        constant PRESCALE_AMOUNT      : Convolution_Prescale_T := 0
    );
    port(
        clkIn, rstAsyncIn              : in  std_logic;
        -- input
        pixelDataIn                    : in  Pixel_Aggregate_T;
        newPixelIn, frameEndStrobeIn   : in  boolean;
        -- output
        pixelDataOut                   : out Pixel_Aggregate_T;
        newPixelOut, frameEndStrobeOut : out boolean
    );
end entity color_kernel;

architecture RTL of color_kernel is
    -- shift register pixel data output
    signal pixelMatrix : Matrix_Aggregate_T := (others => (others => (others => X"00"))); -- that's a lOOOOng initialization

    -- state registers
    signal pixelCounter    : Rgb_Img_Pixel_Ptr_T := 0;
    signal currShiftWidth  : Rgb_Img_Width_T     := 0;
    signal currShiftHeight : Rgb_Img_Height_T    := 0;

    -- TODO: trash kod, kys martin
    signal stage1Out                                      : Stage1_Output_Array_T;
    signal stage2Out                                      : Stage2_Output_Array_T;
    signal stage3Out                                      : Stage3_Output_Array_T;
    signal stage1NewPixel, stage2NewPixel, stage3NewPixel : boolean := false;
    signal stage1FrameEnd, stage2FrameEnd, stage3FrameEnd : boolean := false;

    impure function isImageEdge return boolean is
    begin
        return currShiftHeight = 0 or currShiftHeight = RGB_IMG_WIDTH - 1 or currShiftWidth = 0 or currShiftWidth = RGB_IMG_WIDTH - 1;
    end function isImageEdge;

    impure function isFrameEnd return boolean is
    begin
        return frameEndStrobeIn or pixelCounter > RGB_IMG_WIDTH * RGB_IMG_HEIGHT;
    end function isFrameEnd;
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
                if pixelCounter > RGB_IMG_WIDTH + 1 then
                    if currShiftWidth >= RGB_IMG_WIDTH - 1 then
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
            for color in Pixel_Color_T loop
                for i in 0 to STAGE1_ADDER_COUNT - 1 loop
                    stage1Out(i)(color) <= (others => '0');
                end loop;
            end loop;
            stage1NewPixel <= false;
            stage1FrameEnd <= false;
        elsif rising_edge(clkIn) then
            -- propagate new pixel
            stage1NewPixel <= not isImageEdge and newPixelIn;
            stage1FrameEnd <= isFrameEnd;

            if not isImageEdge and newPixelIn then
                for color in Pixel_Color_T loop
                    --                    report "Current color: " & Pixel_Color'image(currColor);
                    for i in 0 to STAGE1_ADDER_COUNT - 2 loop
                        leftY  := i / 3;
                        rightY := (8 - i) / 3;
                        leftX  := i mod 3;
                        rightX := (8 - i) mod 3;

                        leftMulAcc  := signed(resize(pixelMatrix(color)(leftY, leftX), PIXEL_WIDTH + 1)) * to_signed(KERNEL_PARAMS_MATRIX(leftY, leftX), 5);
                        rightMulAcc := signed(resize(pixelMatrix(color)(rightY, rightX), PIXEL_WIDTH + 1)) * to_signed(KERNEL_PARAMS_MATRIX(rightY, rightX), 5);

                        stage1Out(i)(color) <= resize(leftMulAcc, PIPELINE_PIXEL_WIDTH) + resize(rightMulAcc, PIPELINE_PIXEL_WIDTH);
                    end loop;
                    mulAcc              := signed(resize(pixelMatrix(color)(1, 1), 9)) * to_signed(KERNEL_PARAMS_MATRIX(1, 1), 5);
                    stage1Out(4)(color) <= resize(mulAcc, PIPELINE_PIXEL_WIDTH);
                end loop;
            end if;
        end if;

    end process convStage1;

    convStage2 : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            for color in Pixel_Color_T loop
                for i in 0 to STAGE2_ADDER_COUNT - 1 loop
                    stage2Out(i)(color) <= (others => '0');
                end loop;
            end loop;
            stage2NewPixel <= false;
            stage2FrameEnd <= false;
        elsif rising_edge(clkIn) then
            -- propagate new pixel
            stage2NewPixel <= stage1NewPixel;
            stage2FrameEnd <= stage1FrameEnd;

            if stage1NewPixel then
                for color in Pixel_Color_T loop
                    for i in 0 to STAGE2_ADDER_COUNT - 2 loop
                        stage2Out(i)(color) <= stage1Out(i)(color) + stage1Out(STAGE1_ADDER_COUNT - 1 - i)(color);
                    end loop;
                    stage2Out(2)(color) <= stage1Out(2)(color);
                end loop;
            end if;
        end if;
    end process convStage2;

    convStage3 : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            for color in Pixel_Color_T loop
                for i in 0 to STAGE3_ADDER_COUNT - 1 loop
                    stage3Out(i)(color) <= (others => '0');
                end loop;
            end loop;
            stage3NewPixel <= false;
            stage3FrameEnd <= false;
        elsif rising_edge(clkIn) then
            -- propagate new pixel
            stage3NewPixel <= stage2NewPixel;
            stage3FrameEnd <= stage2FrameEnd;

            if stage2NewPixel then
                for color in Pixel_Color_T loop
                    for i in 0 to STAGE3_ADDER_COUNT - 2 loop
                        stage3Out(i)(color) <= stage2Out(i)(color) + stage2Out(STAGE2_ADDER_COUNT - 1 - i)(color);
                    end loop;
                    stage3Out(1)(color) <= stage2Out(1)(color);
                end loop;
            end if;
        end if;
    end process convStage3;

    convStage4 : process(clkIn, rstAsyncIn)
        variable tmp         : Pipeline_Pixel_T := (others => '0');
        variable tmpUnsigned : Pixel_Data_T     := X"00";
    begin
        if rstAsyncIn = '1' then
            for currColor in Pixel_Color_T loop
                pixelDataOut(currColor) <= X"00";
            end loop;
            newPixelOut       <= false;
            frameEndStrobeOut <= false;
        elsif rising_edge(clkIn) then
            -- propagate new pixel
            newPixelOut       <= stage3NewPixel;
            frameEndStrobeOut <= stage3FrameEnd;

            if stage3NewPixel then
                for color in Pixel_Color_T loop
                    tmp         := stage3Out(0)(color) + stage3Out(1)(color);
                    tmp         := tmp / (2 ** PRESCALE_AMOUNT);
                    tmpUnsigned := to_saturated_unsigned(tmp, PIXEL_WIDTH);

                    -- convert back to unsigned, we can be sure the number is correct unsigned value because of IF block
                    pixelDataOut(color) <= tmpUnsigned;
                end loop;
            end if;
        end if;
    end process convStage4;

    tmp : for color in Pixel_Color_T generate
        pixelShiftReg : entity work.pixel_shiftreg
            generic map(
                SHIFT_LEN  => (2 * RGB_IMG_WIDTH) + 3,
                LINE_WIDTH => RGB_IMG_WIDTH
            )
            port map(
                clkIn      => clkIn,
                rstAsyncIn => rstAsyncIn,
                dataIn     => pixelDataIn(color),
                enableIn   => newPixelIn,
                pixelsOut  => pixelMatrix(color)
            );
    end generate tmp;
end architecture RTL;
