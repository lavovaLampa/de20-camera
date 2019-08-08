-- Testbench automatically generated online
-- at http://vhdl.lapinoo.net
-- Generation date : 7.8.2019 22:32:45 GMT

library ieee;
use ieee.std_logic_1164.all;

use work.ccd_model_pkg.all;
use work.img_pkg.Pixel_Aggregate_T;
use work.img_pkg.Pixel_Data_T;

entity ccd_demosaic_tb is
end ccd_demosaic_tb;

architecture tb of ccd_demosaic_tb is

    signal clkIn        : std_logic;
    signal rstAsyncIn   : std_logic;
    signal pixelIn      : pixel_data_t;
    signal pixelValidIn : boolean;
    signal frameEndIn   : boolean;
    signal pixelOut     : pixel_aggregate_t;
    signal newPixelOut  : boolean;
    signal frameEndOut  : boolean;

    constant TbPeriod : time      := 1000 ns; -- EDIT Put right period here
    signal TbClock    : std_logic := '0';
    signal TbSimEnded : std_logic := '0';

begin

    dut : entity work.ccd_demosaic
        port map(
            clkIn        => clkIn,
            rstAsyncIn   => rstAsyncIn,
            pixelIn      => pixelIn,
            pixelValidIn => pixelValidIn,
            frameEndIn   => frameEndIn,
            pixelOut     => pixelOut,
            newPixelOut  => newPixelOut,
            frameEndOut  => frameEndOut
        );

    -- Clock generation
    TbClock <= not TbClock after TbPeriod / 2 when TbSimEnded /= '1' else '0';

    -- EDIT: Check that clkIn is really your main clock signal
    clkIn <= TbClock;

    stimuli : process
    begin
        -- EDIT Adapt initialization as needed
        pixelIn      <= '0';
        pixelValidIn <= '0';
        frameEndIn   <= '0';

        -- Reset generation
        -- EDIT: Check that rstAsyncIn is really your reset signal
        rstAsyncIn <= '1';
        wait for 100 ns;
        rstAsyncIn <= '0';
        wait for 100 ns;

        -- EDIT Add stimuli here
        wait for 100 * TbPeriod;

        -- Stop the clock and hence terminate the simulation
        TbSimEnded <= '1';
        wait;
    end process;

    checkProc : process(clkIn, rstAsyncIn)
        type Integer_Pixel_Matrix is array (2 downto 0, 2 downto 0) of natural;
        constant NEW_HEIGHT                      : Ccd_Active_Height_Ptr_T := CCD_WIDTH - 2;
        constant NEW_WIDTH                       : Ccd_Active_Width_Ptr_T  := CCD_HEIGHT - 2;
        variable currColor                       : Ccd_Pixel_Color_T       := Green1;
        variable redColor, greenColor, blueColor : natural;
        variable arrayX, arrayY, pixelCount      : natural                 := 0;
        variable tmpArray                        : Integer_Pixel_Matrix;
    begin
        if rstAsyncIn = '1' then
            arrayY     := 1;
            arrayX     := 1;
            pixelCount := 0;
        elsif rising_edge(clkIn) then
            if pixelValidOut then
                currColor := getCurrColor(arrayY, arrayX);

                --                report "Current (relative) pixel coords (y, x): " & natural'image(arrayY) & ", " & natural'image(arrayX);
                for y in 0 to 2 loop
                    for x in 0 to 2 loop
                        tmpArray(y, x) := to_integer(unsigned(ccdArray(arrayY + y - 1, arrayX + x - 1)(11 downto 4)));
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
end tb;
