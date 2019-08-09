library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.ccd_pkg.all;
use work.ccd_model_pkg.all;
use work.img_pkg.Pixel_Aggregate_T;
use work.img_pkg.Pixel_Data_T;
use work.img_pkg.Pixel_Color_T;

library osvvm;
context osvvm.OsvvmContext;

entity ccd_demosaic_tb is
    constant CLK_PERIOD : time := 20 ns; -- 50 MHz

    constant TEST_HEIGHT      : positive := 64;
    constant TEST_WIDTH       : positive := 84;
    constant TEST_FRAME_COUNT : positive := 3;

    constant DEMOSAIC_TB_ALERT_ID : AlertLogIDType := GetAlertLogID("CCD demosaic testbench", ALERTLOG_BASE_ID);
end ccd_demosaic_tb;

architecture tb of ccd_demosaic_tb is
    -- common signals
    signal mainClk, pixClk       : std_logic;
    signal rstAsync, rstAsyncNeg : std_logic;

    -- ccd model signals
    signal lineValid, frameValid         : std_logic;
    signal ccdPixelData                  : Ccd_Pixel_Data_T;
    signal ccdFrameDone, ccdConfigUpdate : boolean;

    -- ccd ctrl signals
    signal pixelValid                     : boolean;
    signal frameEndStrobe, hBlank, vBlank : boolean;
    signal currHeight                     : Ccd_Img_Height_Ptr_T;
    signal currWidth                      : Ccd_Img_Width_Ptr_T;
    signal pixelCounter                   : Ccd_Img_Pixel_Ptr_T;
    signal pixelData                      : Pixel_Data_T;

    -- ccd demosaic signals
    signal pixelOut    : pixel_aggregate_t;
    signal newPixelOut : boolean;
    signal frameEndOut : boolean;

    -- debug signals
    --    signal currHeightDbg, currWidthDbg : natural;
    --    signal pixelCounterDbg             : natural;

    -- testbench signals
    signal tbClock    : std_logic := '0';
    signal tbSimEnded : std_logic := '0';

begin
    -- negate reset for ccd
    rstAsyncNeg <= not rstAsync;

    ccdModel : entity work.ccd_model
        generic map(
            INIT_HEIGHT => TEST_HEIGHT,
            INIT_WIDTH  => TEST_WIDTH
        )
        port map(
            clkIn           => mainClk,
            rstAsyncNegIn   => rstAsyncNeg,
            -- sensor i/o
            pixClkOut       => pixClk,
            lineValidOut    => lineValid,
            frameValidOut   => frameValid,
            strobeOut       => open,
            dataOut         => ccdPixelData,
            -- serial i/o
            sClkIn          => '0',
            sDataIO         => open,
            -- debug
            frameDoneOut    => ccdFrameDone,
            configUpdateOut => ccdConfigUpdate
        );

    ccdCtrl : entity work.ccd_ctrl
        port map(
            clkIn             => pixClk,
            rstAsyncIn        => rstAsync,
            -- sensor input
            frameValidIn      => frameValid,
            lineValidIn       => lineValid,
            ccdPixelIn        => ccdPixelData,
            -- output
            pixelValidOut     => pixelValid,
            frameEndStrobeOut => frameEndStrobe,
            hBlankOut         => hBlank,
            vBlankOut         => vBlank,
            heightOut         => currHeight,
            widthOut          => currWidth,
            pixelCounterOut   => pixelCounter,
            pixelOut          => pixelData
        );

    dut : entity work.ccd_demosaic
        port map(
            clkIn            => pixClk,
            rstAsyncIn       => rstAsync,
            -- input
            pixelIn          => pixelData,
            pixelValidIn     => pixelValid,
            frameEndStrobeIn => frameEndStrobe,
            pixelCounterIn   => pixelCounter,
            -- output
            pixelOut         => pixelOut,
            newPixelOut      => newPixelOut,
            frameEndOut      => frameEndOut
        );

    -- Clock generation
    tbClock <= not tbClock after CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';
    mainClk <= tbClock;

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

        -- Reset generation
        rstAsync <= '1';
        wait for 10 * CLK_PERIOD;
        rstAsync <= '0';
        wait for 10 * CLK_PERIOD;

        for i in 0 to TEST_FRAME_COUNT loop
            wait until frameEndOut;
        end loop;

        -- Stop the clock and hence terminate the simulation
        tbSimEnded <= '1';
        wait;
    end process;

    sanityCheckProc : process(pixClk, rstAsync)
        variable strobeCheck : boolean := false;
    begin
        if rstAsync = '1' then
            strobeCheck := false;
        elsif rising_edge(pixClk) then
            AlertIfNot(DEMOSAIC_TB_ALERT_ID, (newPixelOut xor frameEndOut) or (not newPixelOut and not frameEndOut), "Invalid frame end/new pixel signals state");

            if frameEndOut then
                AlertIf(DEMOSAIC_TB_ALERT_ID, strobeCheck, "Frame end signal active for more than 1 clock cycle (not strobe)");

                strobeCheck := true;
            else
                strobeCheck := false;
            end if;
        end if;
    end process sanityCheckProc;

    demosaicCheckProc : process(pixClk, rstAsync)
        type Pixel_Matrix_Int_T is array (2 downto 0, 2 downto 0) of natural;
        type Pixel_Value_T is array (Pixel_Color_T) of natural;

        -- state registers
        constant NEW_HEIGHT : Ccd_Active_Height_Ptr_T := OUTPUT_HEIGHT - 2;
        constant NEW_WIDTH  : Ccd_Active_Width_Ptr_T  := OUTPUT_WIDTH - 2;

        -- helper variables
        variable currColor                            : Ccd_Pixel_Color_T;
        variable computedPixel                        : Pixel_Value_T;
        variable outWidth, outHeight, outPixelCounter : natural := 0;
        variable pixelCheckArray                      : Pixel_Matrix_Int_T;
    begin
        -- debug signals
        --        currHeightDbg   <= outHeight;
        --        currWidthDbg    <= outWidth;
        --        pixelCounterDbg <= outPixelCounter;

        if rstAsync = '1' then
            outWidth        := 1;
            outHeight       := 1;
            outPixelCounter := 0;
        elsif rising_edge(pixClk) then
            if newPixelOut then
                currColor := get_ccd_pixel_color(outHeight + CCD_CONFIGURATION.height_start, outWidth + CCD_CONFIGURATION.width_start, CCD_CONFIGURATION.is_mirrored);

                Log(DEMOSAIC_TB_ALERT_ID, "Current (relative) pixel position (height, width): " & to_string(outHeight) & " x " & to_string(outWidth), DEBUG);

                for y in 0 to 2 loop
                    for x in 0 to 2 loop
                        pixelCheckArray(y, x) := to_integer(unsigned(pixelArray.getPixel(outHeight + y - 1, outWidth + x - 1)(11 downto 4)));

                        Log(DEMOSAIC_TB_ALERT_ID, "Currently fetching pixel (height, width): " & to_string(outHeight + y - 1) & " x " & to_string(outWidth + x - 1), DEBUG);
                        Log(DEMOSAIC_TB_ALERT_ID, "Data: " & to_hstring(pixelArray.getPixel(outHeight + y - 1, outWidth + x - 1)) & " truncated to: " & to_hstring(to_unsigned(pixelCheckArray(y, x), 8)), DEBUG);
                    end loop;
                end loop;

                -- demosaicing
                case currColor is
                    when Red =>
                        computedPixel(Red)   := pixelCheckArray(1, 1);
                        computedPixel(Green) := (pixelCheckArray(0, 1) + pixelCheckArray(1, 0) + pixelCheckArray(1, 2) + pixelCheckArray(2, 1)) / 4;
                        computedPixel(Blue)  := (pixelCheckArray(0, 0) + pixelCheckArray(0, 2) + pixelCheckArray(2, 0) + pixelCheckArray(2, 2)) / 4;

                    when Blue =>
                        computedPixel(Blue)  := pixelCheckArray(1, 1);
                        computedPixel(Green) := (pixelCheckArray(0, 1) + pixelCheckArray(1, 0) + pixelCheckArray(1, 2) + pixelCheckArray(2, 1)) / 4;
                        computedPixel(Red)   := (pixelCheckArray(0, 0) + pixelCheckArray(0, 2) + pixelCheckArray(2, 0) + pixelCheckArray(2, 2)) / 4;

                    when Green1 =>
                        computedPixel(Green) := pixelCheckArray(1, 1);
                        computedPixel(Red)   := (pixelCheckArray(1, 0) + pixelCheckArray(1, 2)) / 2;
                        computedPixel(Blue)  := (pixelCheckArray(0, 1) + pixelCheckArray(2, 1)) / 2;

                    when Green2 =>
                        computedPixel(Green) := pixelCheckArray(1, 1);
                        computedPixel(Blue)  := (pixelCheckArray(1, 0) + pixelCheckArray(1, 2)) / 2;
                        computedPixel(Red)   := (pixelCheckArray(0, 1) + pixelCheckArray(2, 1)) / 2;

                end case;

                -- computed colors should be equal
                for color in Pixel_Color_T loop
                    AlertIfNot(DEMOSAIC_TB_ALERT_ID, computedPixel(color) = to_integer(pixelOut(color)),
                               "Wrong " & to_string(color) & " pixel value received at (height, width): " & to_string(outHeight) & " x " & to_string(outWidth) & LF & "Expected: " & to_hstring(to_unsigned(computedPixel(color), 8)) & LF & "Received: " & to_hstring(pixelOut(color)));
                end loop;

                outPixelCounter := outPixelCounter + 1;

                if (outWidth >= NEW_WIDTH) then
                    outWidth := 1;

                    if outHeight >= NEW_HEIGHT then
                        outHeight := 1;
                    else
                        outHeight := outHeight + 1;
                    end if;

                    Log(DEMOSAIC_TB_ALERT_ID, "New height: " & to_string(outHeight));
                else
                    outWidth := outWidth + 1;
                end if;

            elsif frameEndOut and outPixelCounter /= 0 then
                AlertIfNot(DEMOSAIC_TB_ALERT_ID, outPixelCounter = NEW_HEIGHT * NEW_WIDTH,
                           "Received incorrect number of pixels" & LF & "Expected: " & to_string(NEW_HEIGHT * NEW_WIDTH) & LF & "Received: " & to_string(outPixelCounter));

                outWidth        := 1;
                outHeight       := 1;
                outPixelCounter := 0;
            end if;
        end if;
    end process demosaicCheckProc;
end tb;
