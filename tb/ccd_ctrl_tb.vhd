library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use std.textio.all;

use work.ccd_pkg.all;
use work.ccd_model_pkg.all;
use work.img_pkg.Pixel_Aggregate_T;
use work.img_pkg.Pixel_Color_T;
use work.img_pkg.Pixel_Data_T;

library osvvm;
context osvvm.OsvvmContext;

entity ccd_ctrl_tb is
    constant CLK_PERIOD : time := 20 ns; -- 50 MHz clock

    constant TEST_HEIGHT      : natural := 64;
    constant TEST_WIDTH       : natural := 84;
    constant TEST_FRAME_COUNT : natural := 20;

    constant CCD_CTRL_TB_ALERT_ID : AlertLogIDType := GetAlertLogID("Ccd Ctrl TestBench", ALERTLOG_BASE_ID);
end ccd_ctrl_tb;

architecture test of ccd_ctrl_tb is
    -- dut interfacing signals
    signal clkIn, rstAsyncIn          : std_logic        := '0';
    signal frameValid, lineValid      : std_logic        := '0';
    signal pixelData                  : Ccd_Pixel_Data_T := X"000";
    signal pixelOut                   : Pixel_Data_T;
    signal pixelValid, frameEndStrobe : boolean;
    signal hBlank, vBlank             : boolean;
    signal currHeight                 : Ccd_Img_Height_Ptr_T;
    signal currWidth                  : Ccd_Img_Width_Ptr_T;
    signal pixelCounter               : Ccd_Img_Pixel_Ptr_T;

    signal nRstAsync, pixClk : std_logic;
    signal frameDone         : boolean;
    signal configUpdate      : boolean;

    -- testbench signals
    signal tbClock    : std_logic := '0';
    signal tbSimEnded : std_logic := '0';
begin
    -- Clock generation
    tbClock <= not tbClock after CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';
    clkIn   <= tbClock;

    nRstAsync <= not rstAsyncIn;

    dut : entity work.ccd_ctrl
        port map(
            clkIn             => pixClk,
            rstAsyncIn        => rstAsyncIn,
            frameValidIn      => frameValid,
            lineValidIn       => lineValid,
            ccdPixelIn        => pixelData,
            -- output
            pixelValidOut     => pixelValid,
            frameEndStrobeOut => frameEndStrobe,
            hBlankOut         => hBlank,
            vBlankOut         => vBlank,
            heightOut         => currHeight,
            widthOut          => currWidth,
            pixelCounterOut   => pixelCounter,
            pixelOut          => pixelOut
        );

    ccdModel : entity work.ccd_model
        generic map(
            INIT_HEIGHT => TEST_HEIGHT,
            INIT_WIDTH  => TEST_WIDTH
        )
        port map(
            clkIn           => clkIn,
            rstAsyncNegIn   => nRstAsync,
            pixClkOut       => pixClk,
            lineValidOut    => lineValid,
            frameValidOut   => frameValid,
            strobeOut       => open,
            dataOut         => pixelData,
            sClkIn          => 'Z',
            sDataIO         => open,
            frameDoneOut    => frameDone,
            configUpdateOut => configUpdate
        );

    stimuli : process
        variable randomGen : RandomPType;
    begin
        randomGen.InitSeed(randomGen'instance_name);
        -- initialize frame pixel values
        for y in 0 to TEST_HEIGHT - 1 loop
            for x in 0 to TEST_WIDTH - 1 loop
                pixelArray.setPixel(y, x, randomGen.randSlv(0, 2**12 - 1, 12));
            end loop;
        end loop;

        -- generate reset
        rstAsyncIn <= '1';
        wait for 10 * CLK_PERIOD;
        rstAsyncIn <= '0';
        wait for 10 * CLK_PERIOD;

        wait until falling_edge(clkIn);

        for i in 0 to TEST_FRAME_COUNT - 1 loop
            wait until frameDone;
        end loop;

        -- Stop the clock and hence terminate the simulation
        tbSimEnded <= '1';
        wait;
    end process;

    sanityCheckProc : process(pixClk, rstAsyncIn)
        variable strobeCheck : boolean := false;
    begin
        if rstAsyncIn = '1' then
            strobeCheck := false;
        elsif rising_edge(pixClk) then
            AlertIfNot(CCD_CTRL_TB_ALERT_ID, (pixelValid xor frameEndStrobe) or (not pixelValid and not frameEndStrobe), "Invalid frame end/new pixel signals state");

            if frameEndStrobe then
                AlertIf(CCD_CTRL_TB_ALERT_ID, strobeCheck, "Frame end signal active for more than 1 clock cycle (not strobe)");

                strobeCheck := true;
            else
                strobeCheck := false;
            end if;
        end if;
    end process sanityCheckProc;

    blankingCheckProc : process(pixClk, nRstAsync)
    begin
        if falling_edge(pixClk) and nRstAsync /= '0' then
            AlertIf(CCD_CTRL_TB_ALERT_ID, pixelValid and (hBlank or vBlank), "Controller cannot output valid pixel during a blanking period");
            AlertIfNot(CCD_CTRL_TB_ALERT_ID, hBlank xor lineValid = '1', "Controller doesn't output correct sensor horizontal blanking state");
            AlertIfNot(CCD_CTRL_TB_ALERT_ID, vBlank xor frameValid = '1', "Controller doesn't output correct sensor horizontal blanking state");
        end if;
    end process blankingCheckProc;

    pixelCountCheckProc : process(pixClk, nRstAsync)
        variable counter : natural := 0;
    begin
        if nRstAsync = '0' then
            counter := 0;
        elsif rising_edge(pixClk) then
            AlertIfNot(CCD_CTRL_TB_ALERT_ID, pixelCounter = (currHeight * TEST_WIDTH) + currWidth, "Incorrect pixel count reported" & LF & "Expected: " & to_string((currHeight * TEST_WIDTH) + currWidth) & LF & "Got: " & to_string(pixelCounter));

            if pixelValid then
                counter := counter + 1;
            elsif frameEndStrobe then
                AlertIfNot(CCD_CTRL_TB_ALERT_ID, counter = TEST_HEIGHT * TEST_WIDTH or counter = 0, "Incorrect number of pixels received" & LF & "Expected: " & to_string(TEST_HEIGHT * TEST_WIDTH) & LF & "Got: " & to_string(counter));
                counter := 0;
            end if;
        end if;
    end process pixelCountCheckProc;

    pixelDataCheck : process(currHeight, currWidth, pixelOut, pixelValid)
    begin
        if pixelValid then
            AlertIfNot(CCD_CTRL_TB_ALERT_ID, pixelArray.getPixel(currHeight, currWidth)(11 downto 4) = std_logic_vector(pixelOut),
                       "Incorrect pixel data received at position (height x width): " & to_string(currHeight) & " x " & to_string(currWidth) & LF & "Expected: " & to_hstring(pixelArray.getPixel(currHeight, currWidth)(11 downto 4)) & LF & "Got: " & to_hstring(pixelOut));
        end if;
    end process pixelDataCheck;

end test;
