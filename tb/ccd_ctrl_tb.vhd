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

    type Ccd_Image_Acc is array (0 to IMG_HEIGHT - 1, 0 to IMG_WIDTH - 1) of Pixel_Data;

    constant CLK_PERIOD : time := 20 ns; -- EDIT Put right period here

    -- real constants are way higher
    -- TODO: consult documentation
    constant HBLANK_CLKS : positive := 5;
    constant VBLANK_CLKS : positive := 10;
end ccd_ctrl_tb;

architecture test of ccd_ctrl_tb is

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
    signal TbClock    : std_logic := '0';
    signal TbSimEnded : std_logic := '0';
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
    TbClock <= not TbClock after CLK_PERIOD / 2 when TbSimEnded /= '1' else '0';

    -- EDIT: Check that clkIn is really your main clock signal
    clkIn <= TbClock;

    stimuli : process
    begin
        frameValidIn <= '0';
        lineValidIn  <= '0';
        pixelDataIn  <= X"000";

        -- Reset generation
        -- EDIT: Check that rstAsyncIn is really your reset signal
        rstAsyncIn <= '1';
        wait for 2 * CLK_PERIOD;
        rstAsyncIn <= '0';
        wait for 2 * CLK_PERIOD;
        wait until falling_edge(clkIn);

        for frameCount in 0 to 1 loop
            frameValidIn <= '1';
            wait for 2 * CLK_PERIOD;

            for y in 0 to IMG_HEIGHT - 1 loop
                lineValidIn <= '1';

                for x in 0 to IMG_WIDTH - 1 loop
                    wait until falling_edge(clkIn);

                    pixelDataIn(11 downto 4) <= std_logic_vector(to_unsigned(x, 8));
--                    pixelArray(y, x)         <= temp;
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
        TbSimEnded <= '1';
        wait;
    end process;

--    checkProc : process(clkIn, pixelValidOut)
--        variable currColor                       : Ccd_Pixel_Color := Green1;
--        variable redColor, greenColor, blueColor : Pixel_Data;
--        variable x, y                            : natural     := 0;
--    begin
--        if rising_edge(clkIn) and pixelValidOut then
--            currColor := getCurrColor(currXOut, currYOut);
--            x         := currXOut + 1;
--            y         := currYOut + 1;
--
--            -- demosaicing
--            if currColor = Red then
--                redColor   := pixelArray(y, x);
--                greenColor := (pixelArray(y - 1, x) + pixelArray(y, x - 1) + pixelArray(y, x + 1) + pixelArray(y + 1, x)) / 4;
--                blueColor  := (pixelArray(y - 1, x - 1) + pixelArray(y - 1, x + 1) + pixelArray(y + 1, x - 1) + pixelArray(y + 1, x + 1)) / 4;
--            elsif currColor = Blue then
--                redColor   := (pixelArray(y - 1, x - 1) + pixelArray(y - 1, x + 1) + pixelArray(y + 1, x - 1) + pixelArray(y + 1, x + 1)) / 4;
--                greenColor := (pixelArray(y - 1, x + 1) + pixelArray(y, x - 1) + pixelArray(y, x + 1) + pixelArray(y + 1, x)) / 4;
--                blueColor  := pixelArray(y, x);
--            elsif currColor = Green1 then
--                redColor   := (pixelArray(y, x - 1) + pixelArray(y, x + 1)) / 2;
--                greenColor := pixelArray(y, x);
--                blueColor  := (pixelArray(y - 1, x) + pixelArray(y + 1, x)) / 2;
--            elsif currColor = Green2 then
--                redColor   := (pixelArray(y - 1, x) + pixelArray(y + 1, x)) / 2;
--                greenColor := pixelArray(y, x);
--                blueColor  := (pixelArray(y, x - 1) + pixelArray(y, x + 1)) / 2;
--            end if;
--
--            assert redColor = redOut report "Wrong red color value on output\n(height, width): (" &
--            integer'image(currYOut) & ", " & integer'image(currXOut) & ")\n" severity failure;
--
--            assert greenColor = greenOut report "Wrong green color value on output\n(height, width): (" &
--            integer'image(currYOut) & ", " & integer'image(currXOut) & ")\n" severity failure;
--
--            assert blueColor = blueOut report "Wrong blue color value on output\n(height, width): (" &
--            integer'image(currYOut) & ", " & integer'image(currXOut) & ")\n" severity failure;
--        end if;
--    end process checkProc;

end test;
