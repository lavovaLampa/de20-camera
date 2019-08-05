library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.ccd_pkg.all;
use work.kernel_pkg.all;
use work.color_kernel;
use work.img_pkg.Pixel_Aggregate_T;
use work.img_pkg.Pixel_Data_T;
use work.img_pkg.Pixel_Color_T;
use work.img_pkg.PIXEL_WIDTH;

entity color_kernel_tb is
    constant TEST_KERNEL      : Convolution_Params   := (
        (-1, -1, -1),
        (-1, 8, -1),
        (-1, -1, -1)
    );
    constant TEST_PRESCALE    : Convolution_Prescale := 0;
    constant TEST_FRAME_COUNT : natural              := 2;

    -- 50 MHz
    constant CLK_PERIOD : time := 20 ns; -- EDIT Put right period here
    type Image_Accumulator is array (0 to IMG_HEIGHT - 1, 0 to IMG_WIDTH - 1) of Pixel_Aggregate_T;
end color_kernel_tb;

architecture tb of color_kernel_tb is
    -- WRITE-ONLY
    signal clkIn, rstAsyncIn      : std_logic;
    signal pixelIn                : Pixel_Aggregate_T;
    signal newPixelIn, frameEndIn : boolean;

    -- READ-ONLY
    signal pixelOut    : Pixel_Aggregate_T;
    signal newPixelOut : boolean;
    signal frameEndOut : boolean;

    -- INTERNAL
    signal TbClock    : std_logic := '0';
    signal TbSimEnded : std_logic := '0';

    signal imgArray : Image_Accumulator := (others => (others => (others => X"00")));
begin

    -- Clock generation
    TbClock <= not TbClock after CLK_PERIOD / 2 when TbSimEnded /= '1' else '0';

    -- EDIT: Check that clkIn is really your main clock signal
    clkIn <= TbClock;

    dut : entity work.color_kernel
        generic map(
            kernelParams   => TEST_KERNEL,
            prescaleAmount => TEST_PRESCALE
        )
        port map(
            clkIn       => clkIn,
            rstAsyncIn  => rstAsyncIn,
            pixelIn     => pixelIn,
            newPixelIn  => newPixelIn,
            frameEndIn  => frameEndIn,
            pixelOut    => pixelOut,
            newPixelOut => newPixelOut,
            frameEndOut => frameEndOut
        );

    stimuli : process
        variable tmpPixel : Pixel_Data_T := X"00";
    begin
        pixelIn    <= (others => X"00");
        newPixelIn <= false;
        frameEndIn <= false;

        -- Reset generation
        rstAsyncIn <= '1';
        wait for 2 * CLK_PERIOD;
        rstAsyncIn <= '0';
        wait for 2 * CLK_PERIOD;

        for n in 0 to TEST_FRAME_COUNT - 1 loop
            frameEndIn <= false;
            for y in 0 to IMG_HEIGHT - 1 loop
                for x in 0 to IMG_WIDTH - 1 loop
                    wait until rising_edge(clkIn);
                    newPixelIn <= true;
                    tmpPixel   := to_unsigned(x, 8);
                    for currColor in Pixel_Color_T loop
                        imgArray(y, x)(currColor) <= tmpPixel;
                        pixelIn(currColor)        <= tmpPixel;
                    end loop;
                end loop;
                wait until rising_edge(clkIn);
                newPixelIn <= false;
                wait for 5 * CLK_PERIOD; -- horizontal blank
            end loop;
            frameEndIn <= true;
            wait for 10 * CLK_PERIOD;   -- vertical blank
        end loop;

        --        wait until frameEndOut;

        wait for 10 * CLK_PERIOD;
        -- Stop the clock and hence terminate the simulation
        TbSimEnded <= '1';
        wait;
    end process;

    outputCheck : process(clkIn, rstAsyncIn)
        constant OUTPUT_WIDTH  : natural := IMG_WIDTH - 2;
        constant OUTPUT_HEIGHT : natural := IMG_HEIGHT - 2;

        variable currWidth, arrayWidth   : Img_Width_Ptr_T     := 0;
        variable currHeight, arrayHeight : Img_Height_Ptr_T    := 0;
        variable pixelAcc                : Pipeline_Pixel      := (others => '0');
        variable mulAcc                  : signed(13 downto 0) := (others => '0');
        variable pixelCounter            : natural             := 0;
        variable tmpPixel                : Pixel_Data_T        := X"00";
    begin
        if rstAsyncIn = '1' then
            pixelCounter := 0;
            currWidth    := 0;
            currHeight   := 0;
        elsif rising_edge(clkIn) then
            if newPixelOut then
                pixelCounter := pixelCounter + 1;
                arrayWidth   := currWidth + 1;
                arrayHeight  := currHeight + 1;

                --                report "Array Height, Width: " & natural'image(arrayHeight) & ", " & natural'image(arrayWidth);
                for currColor in Pixel_Color_T loop
                    pixelAcc := (others => '0');
                    for y in 0 to 2 loop
                        for x in 0 to 2 loop
                            mulAcc   := signed(resize(imgArray(arrayHeight + y - 1, arrayWidth + x - 1)(currColor), PIXEL_WIDTH + 1)) * to_signed(TEST_KERNEL(y, x), 5);
                            pixelAcc := pixelAcc + resize(mulAcc, PIPELINE_SIZE);
                        end loop;
                    end loop;
                    pixelAcc := pixelAcc / (2 ** TEST_PRESCALE);
                    tmpPixel := toSaturatedUnsigned(pixelAcc, PIXEL_WIDTH);

                    assert tmpPixel = pixelOut(currColor) report "Received wrong pixel value" & LF &
                    "Expected: " & integer'image(to_integer(tmpPixel)) & LF &
                    "Received: " & integer'image(to_integer(pixelOut(currColor))) severity failure;
                end loop;

                if currWidth >= OUTPUT_WIDTH - 1 then
                    currWidth  := 0;
                    currHeight := currHeight + 1;
                else
                    currWidth := currWidth + 1;
                end if;
            end if;

            if frameEndOut and (currWidth /= 0 or currHeight /= 0) then
                report "Frame End received";
                report "currWidth: " & natural'image(currWidth);
                report "currHeight: " & natural'image(currHeight);
                assert pixelCounter = OUTPUT_WIDTH * OUTPUT_HEIGHT report "didn't receive all pixels or received more" & LF &
                    "Expected: " & natural'image(OUTPUT_WIDTH * OUTPUT_HEIGHT) & " pixels" & LF &
                    "Received: " & natural'image(pixelCounter) & " pixels" severity failure;
                currWidth    := 0;
                currHeight   := 0;
                pixelCounter := 0;
            end if;
        end if;
    end process;

end tb;
