library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use work.ccd_pkg.all;
use work.common_pkg.all;
use work.ccd_ctrl;

entity ccd_ctrl_tb is
    alias PIXEL_SIZE is IMG_CONSTS.pixel_size;
    constant PIPELINE_SIZE : natural := PIXEL_SIZE + 2;
    -- pipeline stage has to be wide enough not to overflow during addition
    subtype Pipeline_Pixel is unsigned(PIPELINE_SIZE - 1 downto 0);
    subtype Pixel_Range is natural range PIXEL_SIZE - 1 downto 0;
    -- image value accumulator (we have to remember to check if pixels were computed successfully)
    type Ccd_Image_Acc is array (0 to IMG_HEIGHT - 1, 0 to IMG_WIDTH - 1) of Ccd_Pixel_Data;

    constant CLK_PERIOD       : time     := 20 ns; -- 50 MHz clock
    constant TEST_FRAME_COUNT : natural  := 2;
    -- real constants are way higher
    -- TODO: consult documentation
    constant TEST_HBLANK_CLKS : positive := 5;
    constant TEST_VBLANK_CLKS : positive := 10;

end ccd_ctrl_tb;

architecture test of ccd_ctrl_tb is

    -- dut interfacing signals
    signal clkIn, rstAsyncIn          : std_logic      := '0';
    signal frameValidIn, lineValidIn  : std_logic      := '0';
    signal pixelDataIn                : Ccd_Pixel_Data := X"000";
    signal pixelOut                   : Pixel_Aggregate;
    signal pixelValidOut, frameEndOut : boolean;

    -- testbench signals
    signal tbClock    : std_logic := '0';
    signal tbSimEnded : std_logic := '0';

    -- internal testbench signals
    signal pixelMatrix : Ccd_Image_Acc := (others => (others => X"000"));
begin
    -- Clock generation
    tbClock <= not tbClock after CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';
    clkIn   <= tbClock;

    dut : entity ccd_ctrl
        port map(clkIn         => clkIn,
                 rstAsyncIn    => rstAsyncIn,
                 frameValidIn  => frameValidIn,
                 lineValidIn   => lineValidIn,
                 pixelDataIn   => pixelDataIn,
                 pixelOut      => pixelOut,
                 frameEndOut   => frameEndOut,
                 pixelValidOut => pixelValidOut);

    stimuli : process
        variable pixelDataAcc : Ccd_Pixel_Data;
    begin
        -- generate reset
        rstAsyncIn <= '1';
        wait for 2 * CLK_PERIOD;
        rstAsyncIn <= '0';
        wait for 2 * CLK_PERIOD;

        wait until falling_edge(clkIn);

        for currFrame in 0 to TEST_FRAME_COUNT - 1 loop
            report "Frame num.: " & to_string(currFrame);
            frameValidIn <= '1';
            for y in 0 to IMG_HEIGHT - 1 loop
                lineValidIn <= '1';
                for x in 0 to IMG_WIDTH - 1 loop
                    pixelDataAcc      := std_logic_vector(to_unsigned(x, Ccd_Pixel_Data'length));
                    pixelMatrix(y, x) <= pixelDataAcc;
                    pixelDataIn       <= pixelDataAcc;
                    wait until falling_edge(clkIn);
                end loop;
                -- HBLANK
                lineValidIn <= '0';
                wait for TEST_HBLANK_CLKS * CLK_PERIOD;
                wait until falling_edge(clkIn);
            end loop;
            -- VBLANK + HBLANK
            frameValidIn <= '0';
            wait for TEST_VBLANK_CLKS * CLK_PERIOD;
            wait until falling_edge(clkIn);
        end loop;

        -- Stop the clock and hence terminate the simulation
        tbSimEnded <= '1';
        wait;
    end process;

    checkProc : process(clkIn, rstAsyncIn)
        type Integer_Pixel_Matrix is array (2 downto 0, 2 downto 0) of natural;
        constant NEW_HEIGHT                      : Img_Height_Range := IMG_HEIGHT - 2;
        constant NEW_WIDTH                       : Img_Width_Range  := IMG_WIDTH - 2;
        variable currColor                       : Ccd_Pixel_Color  := Green1;
        variable redColor, greenColor, blueColor : natural;
        variable arrayX, arrayY, pixelCount      : natural          := 0;
        variable tmpArray                        : Integer_Pixel_Matrix;
    begin
        if rstAsyncIn = '1' then
            arrayY     := 1;
            arrayX     := 1;
            pixelCount := 0;
        elsif rising_edge(clkIn) then
            if pixelValidOut then
                currColor := getCurrColor(arrayX, arrayY);

                for y in 0 to 2 loop
                    for x in 0 to 2 loop
                        -- TODO: uprav ma
                        tmpArray(y, x) := to_integer(unsigned(pixelMatrix(arrayY + y - 1, arrayX + x - 1)(11 downto 4)));
                        --                            report "tmpArray (" & natural'image(y) & ", " & natural'image(x) & "): " & integer'image(tmpArray(y, x));
                    end loop;
                end loop;

                -- demosaicing
                case currColor is
                    when Red =>
                        redColor   := tmpArray(1, 1);
                        greenColor := (tmpArray(0, 1) + tmpArray(1, 0) + tmpArray(1, 2) + tmpArray(2, 1)) / 4;
                        blueColor  := (tmpArray(0, 0) + tmpArray(0, 2) + tmpArray(2, 0) + tmpArray(2, 2)) / 4;

                    when Blue =>
                        blueColor  := tmpArray(1, 1);
                        greenColor := (tmpArray(0, 1) + tmpArray(1, 0) + tmpArray(1, 2) + tmpArray(2, 1)) / 4;
                        redColor   := (tmpArray(0, 0) + tmpArray(0, 2) + tmpArray(2, 0) + tmpArray(2, 2)) / 4;

                    when Green1 =>
                        greenColor := tmpArray(1, 1);
                        redColor   := (tmpArray(1, 0) + tmpArray(1, 2)) / 2;
                        blueColor  := (tmpArray(0, 1) + tmpArray(2, 1)) / 2;

                    when Green2 =>
                        greenColor := tmpArray(1, 1);
                        blueColor  := (tmpArray(1, 0) + tmpArray(1, 2)) / 2;
                        redColor   := (tmpArray(0, 1) + tmpArray(2, 1)) / 2;

                end case;

                -- computed colors should be equal
                assert redColor = to_integer(pixelOut(Red))
                report "Wrong red color value received at (height, width): " & integer'image(arrayY) & " x " & integer'image(arrayX) & LF &
                "Expected: " & natural'image(redColor) & LF &
                "Received: " & natural'image(to_integer(pixelOut(Red))) severity failure;

                assert greenColor = to_integer(pixelOut(Green))
                report "Wrong green color value received at (height, width): " & integer'image(arrayY) & " x " & integer'image(arrayX) & LF &
                "Expected: " & natural'image(greenColor) & LF &
                "Received: " & natural'image(to_integer(pixelOut(Green))) severity failure;

                assert blueColor = to_integer(pixelOut(Blue))
                report "Wrong blue color value received at (height, width): " & integer'image(arrayY) & " x " & integer'image(arrayX) & LF &
                "Expected: " & natural'image(blueColor) & LF &
                "Received: " & natural'image(to_integer(pixelOut(Blue))) severity failure;

                pixelCount := pixelCount + 1;
                if (arrayX >= NEW_WIDTH) then
                    arrayX := 1;
                    arrayY := arrayY + 1;
                else
                    arrayX := arrayX + 1;
                end if;

            elsif frameEndOut and pixelCount /= 0 then
                assert pixelCount = NEW_HEIGHT * NEW_WIDTH report "Wrong number of pixels recived" & LF &
                "Expected: " & positive'image(NEW_HEIGHT * NEW_WIDTH) & LF &
                "Received: " & positive'image(pixelCount) severity failure;

                pixelCount := 0;
                arrayY     := 1;
                arrayX     := 1;
            end if;
        end if;
    end process checkProc;
end test;
