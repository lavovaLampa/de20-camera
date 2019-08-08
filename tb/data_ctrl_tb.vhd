-- Testbench automatically generated online
-- at http://vhdl.lapinoo.net
-- Generation date : 7.8.2019 22:25:44 GMT

library ieee;
use ieee.std_logic_1164.all;

entity data_ctrl_tb is
end data_ctrl_tb;

architecture tb of data_ctrl_tb is

    component data_ctrl
        port(ccdClkIn       : in  std_logic;
             vgaClkIn       : in  std_logic;
             rstAsyncIn     : in  std_logic;
             ccdPixelDataIn : in  pixel_aggregate_t;
             newPixelIn     : in  boolean;
             ccdHBlankIn    : in  boolean;
             ccdVBlankIn    : in  boolean;
             vgaNextPixelIn : in  boolean;
             vgaPixelOut    : out pixel_aggregate_t);
    end component;

    signal ccdClkIn       : std_logic;
    signal vgaClkIn       : std_logic;
    signal rstAsyncIn     : std_logic;
    signal ccdPixelDataIn : pixel_aggregate_t;
    signal newPixelIn     : boolean;
    signal ccdHBlankIn    : boolean;
    signal ccdVBlankIn    : boolean;
    signal vgaNextPixelIn : boolean;
    signal vgaPixelOut    : pixel_aggregate_t;

    constant TbPeriod : time      := 1000 ns; -- EDIT Put right period here
    signal TbClock    : std_logic := '0';
    signal TbSimEnded : std_logic := '0';

begin

    dut : data_ctrl
        port map(ccdClkIn       => ccdClkIn,
                 vgaClkIn       => vgaClkIn,
                 rstAsyncIn     => rstAsyncIn,
                 ccdPixelDataIn => ccdPixelDataIn,
                 newPixelIn     => newPixelIn,
                 ccdHBlankIn    => ccdHBlankIn,
                 ccdVBlankIn    => ccdVBlankIn,
                 vgaNextPixelIn => vgaNextPixelIn,
                 vgaPixelOut    => vgaPixelOut);

    -- Clock generation
    TbClock <= not TbClock after TbPeriod / 2 when TbSimEnded /= '1' else '0';

    -- EDIT: Check that ccdClkIn is really your main clock signal
    ccdClkIn <= TbClock;

    stimuli : process
    begin
        -- EDIT Adapt initialization as needed
        vgaClkIn       <= '0';
        ccdPixelDataIn <= '0';
        newPixelIn     <= '0';
        ccdHBlankIn    <= '0';
        ccdVBlankIn    <= '0';
        vgaNextPixelIn <= '0';

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
