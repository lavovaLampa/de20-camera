-- Testbench automatically generated online
-- at http://vhdl.lapinoo.net
-- Generation date : 23.4.2019 14:42:07 GMT

library ieee;
use ieee.std_logic_1164.all;
use work.kernel_pkg.all;

entity tb_color_kernel is
    constant CLK_PERIOD : time := 20 ns; -- EDIT Put right period here
end tb_color_kernel;

architecture tb of tb_color_kernel is

    signal clkIn         : std_logic;
    signal rstAsyncIn    : std_logic;
    signal pixelIn       : pixel_aggregate;
    signal newPixelIn    : boolean;
    signal frameEndIn    : boolean;
    signal currWidthIn   : img_width_range;
    signal currHeightIn  : img_height_range;
    signal pixelOut      : pixel_aggregate;
    signal currWidthOut  : img_width_range;
    signal currHeightOut : img_height_range;
    signal newPixelOut   : boolean;

    signal TbClock    : std_logic := '0';
    signal TbSimEnded : std_logic := '0';

begin

    dut : entity work.color_kernel
        port map(clkIn         => clkIn,
                 rstAsyncIn    => rstAsyncIn,
                 pixelIn       => pixelIn,
                 newPixelIn    => newPixelIn,
                 frameEndIn    => frameEndIn,
                 currWidthIn   => currWidthIn,
                 currHeightIn  => currHeightIn,
                 pixelOut      => pixelOut,
                 currWidthOut  => currWidthOut,
                 currHeightOut => currHeightOut,
                 newPixelOut   => newPixelOut);

    -- Clock generation
    TbClock <= not TbClock after CLK_PERIOD / 2 when TbSimEnded /= '1' else '0';

    -- EDIT: Check that clkIn is really your main clock signal
    clkIn <= TbClock;

    stimuli : process
    begin
        -- EDIT Adapt initialization as needed
        pixelIn      <= '0';
        newPixelIn   <= '0';
        frameEndIn   <= '0';
        currWidthIn  <= '0';
        currHeightIn <= '0';

        -- Reset generation
        rstAsyncIn <= '1';
        wait for 2 * CLK_PERIOD;
        rstAsyncIn <= '0';
        wait for 2 * CLK_PERIOD;

        for y in 0 to IMG_HEIGHT - 1 loop
            for x in 0 to IMG_WIDTH - 1 loop

            end loop;
        end loop;

        -- Stop the clock and hence terminate the simulation
        TbSimEnded <= '1';
        wait;
    end process;

end tb;
