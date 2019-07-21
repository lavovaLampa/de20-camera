library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;
use work.ccd_ctrl_pkg.all;
use work.common_pkg.all;
use work.ccd_ctrl;

entity ccd_ctrl_tb is
    constant CLK_PERIOD       : time    := 20 ns; -- 50 MHz clock
    constant TEST_FRAME_COUNT : natural := 2;
    -- real constants are way higher
end ccd_ctrl_tb;

architecture test of ccd_ctrl_tb is
    -- dut interfacing signals
    signal clkIn, rstAsyncIn          : std_logic        := '0';
    signal frameValid, lineValid      : std_logic        := '0';
    signal pixelData                  : CCD_Pixel_Data_T := X"000";
    signal pixelOut                   : Pixel_Aggregate;
    signal pixelValidOut, frameEndOut : boolean;

    signal nRstAsync, pixClk : std_logic;
    signal frameDone         : boolean;

    -- mocked ccd array data
    signal ccdArray : CCD_Matrix_T := (others => (others => X"000"));

    -- testbench signals
    signal tbClock    : std_logic := '0';
    signal tbSimEnded : std_logic := '0';
begin
    -- Clock generation
    tbClock <= not tbClock after CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';
    clkIn   <= tbClock;

    nRstAsync <= not rstAsyncIn;

    dut : entity ccd_ctrl
        port map(clkIn         => pixClk,
                 rstAsyncIn    => rstAsyncIn,
                 frameValidIn  => frameValid,
                 lineValidIn   => lineValid,
                 pixelDataIn   => pixelData,
                 pixelOut      => pixelOut,
                 frameEndOut   => frameEndOut,
                 pixelValidOut => pixelValidOut);

    ccdModel : entity work.ccd_model
        generic map(
            INIT_HEIGHT       => IMG_HEIGHT,
            INIT_WIDTH        => IMG_WIDTH,
            INIT_HEIGHT_START => IMG_CONSTS.height_start,
            INIT_WIDTH_START  => IMG_CONSTS.width_start,
            DEBUG             => true
        )
        port map(
            clkIn           => clkIn,
            nRstAsyncIn     => nRstAsync,
            pixClkOut       => pixClk,
            lineValidOut    => lineValid,
            frameValidOut   => frameValid,
            strobeOut       => open,
            dataOut         => pixelData,
            sClkIn          => 'Z',
            sDataIO         => open,
            ccdArrayIn      => ccdArray,
            frameDoneOut    => frameDone,
            configUpdateOut => open
        );

    stimuli : process
    begin
        -- initialize frame pixel values
        for y in ccdArray'range(1) loop
            for x in ccdArray'range(2) loop
                ccdArray(y, x) <= std_logic_vector(to_unsigned(x, CCD_Pixel_Data_T'length));
            end loop;
        end loop;

        -- generate reset
        rstAsyncIn <= '1';
        wait for 2 * CLK_PERIOD;
        rstAsyncIn <= '0';
        wait for 2 * CLK_PERIOD;

        wait until falling_edge(clkIn);

        for i in 0 to TEST_FRAME_COUNT - 1 loop
            wait until frameDone;
        end loop;

        -- Stop the clock and hence terminate the simulation
        tbSimEnded <= '1';
        wait;
    end process;

    checkProc : process(clkIn, rstAsyncIn)
        type Integer_Pixel_Matrix is array (2 downto 0, 2 downto 0) of natural;
        constant NEW_HEIGHT                      : Img_Height_Range  := IMG_HEIGHT - 2;
        constant NEW_WIDTH                       : Img_Width_Range   := IMG_WIDTH - 2;
        variable currColor                       : CCD_Pixel_Color_T := Green1;
        variable redColor, greenColor, blueColor : natural;
        variable arrayX, arrayY, pixelCount      : natural           := 0;
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
end test;
