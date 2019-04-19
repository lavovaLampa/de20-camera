library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ccd_pkg.all;
use work.kernel_pkg.all;

entity color_kernel is
    generic(
        -- edge detection kernel
        kernelParams   : Kernel_Consts := (
            (-1, -1, -1),
            (-1, 8, -1),
            (-1, -1, -1)
        );
        prescaleAmount : Conv_Prescale := 0
    );
    port(
        clkIn, rstAsyncIn      : in  std_logic;
        pixelIn                : in  Pixel_Aggregate;
        newPixelIn, frameEndIn : in  boolean;
        currWidthIn            : in  Img_Width_Range;
        currHeightIn           : in  Img_Height_Range;
        pixelOut               : out Pixel_Aggregate;
        currWidthOut           : out Img_Width_Range;
        currHeightOut          : out Img_Height_Range;
        newPixelOut            : out boolean
    );
end entity color_kernel;

architecture RTL of color_kernel is
    -- pixel shift register OUTPUT
    signal pixelMatrix : Matrix_Aggregate := (others => (others => (others => X"00"))); -- that's a lOOOOng initialization

    signal pixelCounter    : Pixel_Count_Range := 0;
    signal currShiftWidth  : Img_Width_Range   := 0;
    signal currShiftHeight : Img_Height_Range  := 0;

    signal stage1Out : Stage_Out(open)(STAGE1_AMOUNT - 1 downto 0);
    signal stage2Out : Stage_Out(open)(STAGE2_AMOUNT - 1 downto 0);
    signal stage3Out : Stage_Out(open)(STAGE3_AMOUNT - 1 downto 0);

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
        variable tmpX, tmpY, tmpXA, tmpYA : natural;
    begin
        if rstAsyncIn = '1' then

        elsif rising_edge(clkIn) then
            if not isImageFringe then
                if newPixelIn then
                    for currColor in Internal_Pixel_Color loop
                        for i in 0 to STAGE1_AMOUNT - 2 loop
                            tmpY  := i / 3;
                            tmpYA := (9 - i) / 3;
                            tmpX  := i mod 3;
                            tmpXA := (9 - i) mod 3;

                            stage1Out(currColor)(i) <= (signed(pixelMatrix(currColor)(tmpY, tmpX)) * kernelParams(tmpY, tmpX)) + (signed(pixelMatrix(currColor)(tmpYA, tmpXA)) * kernelParams(tmpYA, tmpXA));
                        end loop;
                        stage1Out(currColor)(4) <= (signed(pixelMatrix(currColor)(1, 1) * kernelParams(1, 1)));
                    end loop;
                end if;
            end if;
        end if;

    end process convStage1;

    convStage2 : process(clkIn, rstAsyncIn)
    begin
    end process convStage2;

    convStage3 : process(clkIn, rstAsyncIn)
    begin
    end process convStage3;

    convStage4 : process(clkIn, rstAsyncIn)
    begin
    end process convStage4;

    redShiftReg : entity work.pixel_shiftreg
        port map(
            clkIn      => clkIn,
            rstAsyncIn => rstAsyncIn,
            -- TODO: parametrize
            dataIn     => pixelIn(Red),
            enableIn   => newPixelIn,
            pixelsOut  => pixelMatrix(Red)
        );

    greenShiftReg : entity work.pixel_shiftreg
        port map(
            clkIn      => clkIn,
            rstAsyncIn => rstAsyncIn,
            -- TODO: parametrize
            dataIn     => pixelIn(Green),
            enableIn   => newPixelIn,
            pixelsOut  => pixelMatrix(Green)
        );

    blueShiftReg : entity work.pixel_shiftreg
        port map(
            clkIn      => clkIn,
            rstAsyncIn => rstAsyncIn,
            -- TODO: parametrize
            dataIn     => pixelIn(Blue),
            enableIn   => newPixelIn,
            pixelsOut  => pixelMatrix(Blue)
        );

end architecture RTL;
