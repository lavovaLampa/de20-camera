-- Testbench automatically generated online
-- at http://vhdl.lapinoo.net
-- Generation date : 7.4.2019 23:05:20 GMT

library ieee;
use ieee.std_logic_1164.all;
use work.ccd_pkg.all;
use std.textio.all;
use work.ccd_ctrl;
use ieee.numeric_std.all;

entity ccd_ctrl_tb is
    alias IMG_WIDTH is IMG_CONSTS.width;
    alias IMG_HEIGHT is IMG_CONSTS.height;
    alias PIXEL_SIZE is IMG_CONSTS.pixel_data_size;
    constant PIPELINE_SIZE : natural := PIXEL_SIZE + 2;
    -- pipeline stage has to be wide enough not to overflow during addition
    subtype Pipeline_Pixel is unsigned(PIPELINE_SIZE - 1 downto 0);
    subtype Pixel_Range is natural range PIXEL_SIZE - 1 downto 0;
    -- image value accumulator (we have to remember to check if pixels were computed successfully)
    type Ccd_Image_Acc is array (0 to IMG_HEIGHT - 1, 0 to IMG_WIDTH - 1) of Ccd_Pixel_Data;

    constant CLK_PERIOD : time := 20 ns; -- 50 MHz clock

    -- real constants are way higher
    -- TODO: consult documentation
    constant HBLANK_CLKS : positive := 5;
    constant VBLANK_CLKS : positive := 10;
end ccd_ctrl_tb;

architecture test of ccd_ctrl_tb is

    -- dut interfacing signals
    signal clkIn         : std_logic      := '0';
    signal rstAsyncIn    : std_logic      := '0';
    signal frameValidIn  : std_logic      := '0';
    signal lineValidIn   : std_logic      := '0';
    signal pixelDataIn   : ccd_pixel_data := X"000";
    signal redOut        : pixel_data;
    signal greenOut      : pixel_data;
    signal blueOut       : pixel_data;
    signal currXOut      : img_width_range;
    signal currYOut      : img_height_range;
    signal pixelValidOut : boolean;

    -- testbench signals
    signal tbClock    : std_logic := '0';
    signal tbSimEnded : std_logic := '0';

    -- internal testbench signals
    signal pixelMatrix : Ccd_Image_Acc := (others => (others => X"000"));

    pure function matrixToPixel(logicPixel : Ccd_Pixel_Data) return Pixel_Data is
    begin
        return resize(unsigned(logicPixel), PIPELINE_SIZE);
    end function matrixToPixel;

begin

    dut : entity ccd_ctrl
        port map(clkIn         => clkIn,
                 rstAsyncIn    => rstAsyncIn,
                 frameValidIn  => frameValidIn,
                 lineValidIn   => lineValidIn,
                 pixelDataIn   => pixelDataIn,
                 redOut        => redOut,
                 greenOut      => greenOut,
                 blueOut       => blueOut,
                 currXOut      => currXOut,
                 currYOut      => currYOut,
                 pixelValidOut => pixelValidOut);

    -- Clock generation
    tbClock <= not tbClock after CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';
    clkIn   <= tbClock;

    stimuli : process
        variable pixelDataAcc : Ccd_Pixel_Data;
    begin
        -- generate reset
        rstAsyncIn <= '1';
        wait for 2 * CLK_PERIOD;
        rstAsyncIn <= '0';
        wait for 2 * CLK_PERIOD;
        wait until falling_edge(clkIn);

        -- start sending pixel data
        for frameCount in 0 to 1 loop
            frameValidIn <= '1';
            wait for 2 * CLK_PERIOD;
            for y in 0 to IMG_HEIGHT - 1 loop
                lineValidIn <= '1';
                for x in 0 to IMG_WIDTH - 1 loop
                    wait until falling_edge(clkIn);

                    pixelDataAcc      := std_logic_vector(to_unsigned(x, pixelDataAcc'length));
                    pixelDataIn       <= pixelDataAcc;
                    pixelMatrix(y, x) <= pixelDataAcc;

                end loop;
                wait until falling_edge(clkIn);
                lineValidIn <= '0';
                pixelDataIn <= X"000";
                wait for HBLANK_CLKS * CLK_PERIOD;
            end loop;
            frameValidIn <= '0';
            wait for VBLANK_CLKS * CLK_PERIOD;
        end loop;

        -- Stop the clock and hence terminate the simulation
        tbSimEnded <= '1';
        wait;
    end process;

    checkProc : process(clkIn, pixelValidOut)
        variable currColor                       : Pixel_Color    := Green1;
        variable redColor, greenColor, blueColor : Pixel_Data;
        variable x, y                            : natural        := 0;
        variable tempPixel                       : Ccd_Pixel_Data := X"000";
    begin
        if rising_edge(clkIn) and pixelValidOut then
            x         := currXOut + 1;
            y         := currYOut + 1;
            currColor := getCurrColor(x, y);

            -- demosaicing
            if currColor = Red then
                tempPixel := pixelMatrix(y, x);
                redColor  := unsigned(tempPixel(Pixel_Range));

                greenColor := (matrixToPixel(pixelMatrix(y - 1, x)) + matrixToPixel(pixelMatrix(y, x - 1)) + matrixToPixel(pixelMatrix(y, x + 1)) + matrixToPixel(pixelMatrix(y + 1, x))) / 4;
                blueColor  := (matrixToPixel(pixelMatrix(y - 1, x - 1)) + matrixToPixel(pixelMatrix(y - 1, x + 1)) + matrixToPixel(pixelMatrix(y + 1, x - 1)) + matrixToPixel(pixelMatrix(y + 1, x + 1))) / 4;
            elsif currColor = Blue then
                tempPixel := pixelMatrix(y, x);
                blueColor := unsigned(tempPixel(Pixel_Range));

                greenColor := (matrixToPixel(pixelMatrix(y - 1, x)) + matrixToPixel(pixelMatrix(y, x - 1)) + matrixToPixel(pixelMatrix(y, x + 1)) + matrixToPixel(pixelMatrix(y + 1, x))) / 4;
                redColor   := (matrixToPixel(pixelMatrix(y - 1, x - 1)) + matrixToPixel(pixelMatrix(y - 1, x + 1)) + matrixToPixel(pixelMatrix(y + 1, x - 1)) + matrixToPixel(pixelMatrix(y + 1, x + 1))) / 4;
            elsif currColor = Green1 then
                tempPixel  := pixelMatrix(y, x);
                greenColor := unsigned(tempPixel(Pixel_Range));

                redColor  := (matrixToPixel(pixelMatrix(y, x - 1)) + matrixToPixel(pixelMatrix(y, x + 1))) / 2;
                blueColor := (matrixToPixel(pixelMatrix(y - 1, x)) + matrixToPixel(pixelMatrix(y + 1, x))) / 2;
            elsif currColor = Green2 then
                tempPixel  := pixelMatrix(y, x);
                greenColor := unsigned(tempPixel(Pixel_Range));

                blueColor := (matrixToPixel(pixelMatrix(y, x - 1)) + matrixToPixel(pixelMatrix(y, x + 1))) / 2;
                redColor  := (matrixToPixel(pixelMatrix(y - 1, x)) + matrixToPixel(pixelMatrix(y + 1, x))) / 2;
            end if;

            -- computed colors should be equal
            assert redColor = redOut report "Wrong red color value on output\n(height, width): (" &
                integer'image(currYOut) & ", " & integer'image(currXOut) & ")\n" severity failure;

            assert greenColor = greenOut report "Wrong green color value on output\n(height, width): (" &
                integer'image(currYOut) & ", " & integer'image(currXOut) & ")\n" severity failure;

            assert blueColor = blueOut report "Wrong blue color value on output\n(height, width): (" &
                integer'image(currYOut) & ", " & integer'image(currXOut) & ")\n" severity failure;
        end if;
    end process checkProc;
end test;
