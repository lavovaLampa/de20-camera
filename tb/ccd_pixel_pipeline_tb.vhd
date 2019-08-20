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

entity ccd_pixel_pipeline_tb is
    constant CLK_PERIOD : time := 20 ns; -- 50 MHz clock

    constant TEST_HEIGHT      : natural := 64;
    constant TEST_WIDTH       : natural := 84;
    constant TEST_FRAME_COUNT : natural := 3;

    constant PIXEL_PIPELINE_ALERT_ID : AlertLogIDType := GetAlertLogID("Pixel pipeline testbench", ALERTLOG_BASE_ID);
end ccd_pixel_pipeline_tb;

architecture test of ccd_pixel_pipeline_tb is
    -- clock & reset
    signal clkIn, rstAsyncIn : std_logic := '0';
    signal nRstAsync, pixClk : std_logic;

    -- ccd model output
    signal modelFrameValidOut, modelLineValidOut : std_logic;
    signal modelPixelDataOut                     : Ccd_Pixel_Data_T;
    -- debug signals
    signal modelFrameDoneDbg                     : boolean;
    signal modelConfigUpdateDbg                  : boolean;

    -- ccd ctrl output
    signal ccdPixelDataOut                        : Pixel_Data_T;
    signal ccdPixelValidOut, ccdFrameEndStrobeOut : boolean;
    signal ccdHBlankOut, ccdVBlankOut             : boolean;
    signal ccdHeightOut                           : Ccd_Img_Height_Ptr_T;
    signal ccdWidthOut                            : Ccd_Img_Width_Ptr_T;
    signal ccdPixelCounterOut                     : Ccd_Img_Pixel_Ptr_T;

    -- ccd demosaic output
    signal demosaicPixelDataOut                           : Pixel_Aggregate_T;
    signal demosaicNewPixelOut, demosaicFrameEndStrobeOut : boolean;

    -- img convolution output
    signal convPixelDataOut                       : Pixel_Aggregate_T;
    signal convNewPixelOut, convFrameEndStrobeOut : boolean;

    -- testbench signals
    signal tbClock    : std_logic := '0';
    signal tbSimEnded : std_logic := '0';
begin
    -- Clock generation
    tbClock <= not tbClock after CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';
    clkIn   <= tbClock;

    nRstAsync <= not rstAsyncIn;

    imgConvolution : entity work.img_convolution
            --        generic map(
            --            CONVOLUTION_KERNEL => CONVOLUTION_KERNEL,
            --            PRESCALE_AMOUNT    => PRESCALE_AMOUNT
            --        )
        port map(
            clkIn             => clkIn,
            rstAsyncIn        => rstAsyncIn,
            --
            pixelDataIn       => demosaicPixelDataOut,
            newPixelIn        => demosaicNewPixelOut,
            frameEndStrobeIn  => demosaicFrameEndStrobeOut,
            --
            pixelDataOut      => convPixelDataOut,
            newPixelOut       => convNewPixelOut,
            frameEndStrobeOut => convFrameEndStrobeOut
        );

    ccdDemosaic : entity work.ccd_demosaic
        port map(
            clkIn             => clkIn,
            rstAsyncIn        => rstAsyncIn,
            --
            pixelIn           => ccdPixelDataOut,
            pixelValidIn      => ccdPixelValidOut,
            frameEndStrobeIn  => ccdFrameEndStrobeOut,
            pixelCounterIn    => ccdPixelCounterOut,
            --
            pixelOut          => demosaicPixelDataOut,
            newPixelOut       => demosaicNewPixelOut,
            frameEndStrobeOut => demosaicFrameEndStrobeOut
        );

    ccdCtrl : entity work.ccd_ctrl
        port map(
            clkIn             => pixClk,
            rstAsyncIn        => rstAsyncIn,
            --
            frameValidIn      => modelFrameValidOut,
            lineValidIn       => modelLineValidOut,
            ccdPixelIn        => modelPixelDataOut,
            -- output
            pixelValidOut     => ccdPixelValidOut,
            frameEndStrobeOut => ccdFrameEndStrobeOut,
            hBlankOut         => ccdHBlankOut,
            vBlankOut         => ccdVBlankOut,
            heightOut         => ccdHeightOut,
            widthOut          => ccdWidthOut,
            pixelCounterOut   => ccdPixelCounterOut,
            pixelOut          => ccdPixelDataOut
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
            -- model output
            lineValidOut    => modelLineValidOut,
            frameValidOut   => modelFrameValidOut,
            strobeOut       => open,
            dataOut         => modelPixelDataOut,
            -- i2c i/o
            sClkIn          => 'Z',
            sDataIO         => open,
            -- debug signals
            frameDoneOut    => modelFrameDoneDbg,
            configUpdateOut => modelConfigUpdateDbg
        );

    stimuli : process
    begin
        -- generate reset
        rstAsyncIn <= '1';
        wait for 10 * CLK_PERIOD;
        rstAsyncIn <= '0';
        wait for 10 * CLK_PERIOD;

        wait until falling_edge(clkIn);

        for i in 0 to TEST_FRAME_COUNT - 1 loop
            Log(PIXEL_PIPELINE_ALERT_ID, "Current frame: " & to_string(i));
            wait until modelFrameDoneDbg;
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
            AlertIf(PIXEL_PIPELINE_ALERT_ID, convNewPixelOut and convFrameEndStrobeOut, "Invalid frame end/new pixel signals state");

            if convFrameEndStrobeOut then
                AlertIf(PIXEL_PIPELINE_ALERT_ID, strobeCheck, "Frame end signal active for more than 1 clock cycle (not strobe)");

                strobeCheck := true;
            else
                strobeCheck := false;
            end if;
        end if;
    end process sanityCheckProc;

    pixelCountCheckProc : process(pixClk, rstAsyncIn)
        variable pixelCounter : natural := 0;
    begin
        if rstAsyncIn = '1' then
            pixelCounter := 0;
        elsif rising_edge(pixClk) then
            if convNewPixelOut then
                pixelCounter := pixelCounter + 1;
            elsif convFrameEndStrobeOut then
                AlertIfNot(PIXEL_PIPELINE_ALERT_ID, pixelCounter = (TEST_HEIGHT - 4) * (TEST_WIDTH - 4),
                           "Incorrect number of pixels received in a frame" & LF & "Expected: " & to_string((TEST_HEIGHT - 4) * (TEST_WIDTH - 4)) & LF & "Got: " & to_string(pixelCounter));

                pixelCounter := 0;
            end if;
        end if;
    end process pixelCountCheckProc;
end test;
