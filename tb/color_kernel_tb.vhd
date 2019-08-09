library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.ccd_pkg.all;
use work.kernel_pkg.all;
use work.img_pkg.Pixel_Aggregate_T;
use work.img_pkg.Pixel_Data_T;
use work.img_pkg.Pixel_Color_T;
use work.img_pkg.PIXEL_WIDTH;

library osvvm;
context osvvm.OsvvmContext;

entity color_kernel_tb is
    constant CLK_PERIOD    : time     := 20 ns; -- 50 MHz
    constant TEST_HEIGHT   : positive := 62;
    constant TEST_WIDTH    : positive := 82;
    constant HBLANK_CYCLES : positive := 150;
    constant VBLANK_CYCLES : positive := 10;

    constant TEST_KERNEL      : Convolution_Params_T   := (
        (-1, -1, -1),
        (-1, 8, -1),
        (-1, -1, -1)
    );
    constant TEST_PRESCALE    : Convolution_Prescale_T := 0;
    constant TEST_FRAME_COUNT : natural                := 2;

    constant KERNEL_TB_ALERT_ID : AlertLogIDType := GetAlertLogID("Color kernel testbench", ALERTLOG_BASE_ID);
end color_kernel_tb;

architecture tb of color_kernel_tb is
    -- common signals
    signal clkIn, rstAsyncIn : std_logic;

    -- color kernel in
    signal pixelDataIn            : Pixel_Aggregate_T := (others => (others => '0'));
    signal newPixelIn, frameEndIn : boolean           := false;

    -- color kernel out
    signal pixelDataOut             : Pixel_Aggregate_T;
    signal newPixelOut, frameEndOut : boolean;

    -- testbench signals
    signal tbClk      : std_logic := '0';
    signal tbSimEnded : std_logic := '0';

    -- debug signals
    signal currHeightDbg, currWidthDbg, pixelCounterDbg : natural;

    shared variable pixelArray : Rgb_Img_Pixel_Array_T;
begin
    -- clock generation
    tbClk <= not tbClk after CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';
    clkIn <= tbClk;

    dut : entity work.color_kernel
        generic map(
            KERNEL_PARAMS_MATRIX => TEST_KERNEL,
            PRESCALE_AMOUNT      => TEST_PRESCALE
        )
        port map(
            clkIn             => clkIn,
            rstAsyncIn        => rstAsyncIn,
            -- input
            pixelDataIn       => pixelDataIn,
            newPixelIn        => newPixelIn,
            frameEndStrobeIn  => frameEndIn,
            -- output
            pixelDataOut      => pixelDataOut,
            newPixelOut       => newPixelOut,
            frameEndStrobeOut => frameEndOut
        );

    stimuli : process
        variable randomGen : RandomPType;
    begin
        --        SetLogEnable(DEBUG, true);
        randomGen.InitSeed(randomGen'instance_name);
        -- initialize frame pixel values
        for y in 0 to TEST_HEIGHT - 1 loop
            for x in 0 to TEST_WIDTH - 1 loop
                for color in Pixel_Color_T loop
                    pixelArray.setPixel(y, x, (
                        Red   => randomGen.RandUnsigned(0, 2**8 - 1, 8),
                        Green => randomGen.RandUnsigned(0, 2**8 - 1, 8),
                        Blue  => randomGen.RandUnsigned(0, 2**8 - 1, 8)
                    ));
                end loop;
            end loop;
        end loop;

        -- reset generation
        rstAsyncIn <= '1';
        wait for 10 * CLK_PERIOD;
        rstAsyncIn <= '0';
        wait for 10 * CLK_PERIOD;

        for currFrame in 0 to TEST_FRAME_COUNT loop
            Log(KERNEL_TB_ALERT_ID, "Current frame: " & to_string(currFrame));

            wait until frameEndIn;
            for y in 0 to TEST_HEIGHT - 1 loop
                for x in 0 to TEST_WIDTH - 1 loop
                    for color in Pixel_Color_T loop
                        pixelArray.setPixel(y, x, (
                            Red   => randomGen.RandUnsigned(0, 2**8 - 1, 8),
                            Green => randomGen.RandUnsigned(0, 2**8 - 1, 8),
                            Blue  => randomGen.RandUnsigned(0, 2**8 - 1, 8)
                        ));
                    end loop;
                end loop;
            end loop;
        end loop;

        wait for 10 * CLK_PERIOD;
        -- Stop the clock and hence terminate the simulation
        tbSimEnded <= '1';
        wait;
    end process;

    inputMockProc : process(clkIn, rstAsyncIn)
        -- state registers
        variable currWidth    : natural             := 0;
        variable currHeight   : natural             := 0;
        variable pixelCounter : Rgb_Img_Pixel_Ptr_T := 0;
    begin
        -- debug signals
        currHeightDbg   <= currHeight;
        currWidthDbg    <= currWidth;
        pixelCounterDbg <= pixelCounter;

        if rstAsyncIn = '1' then
            frameEndIn  <= false;
            newPixelIn  <= false;
            pixelDataIn <= (others => (others => '0'));

            currWidth    := 0;
            currHeight   := 0;
            pixelCounter := 0;
        elsif rising_edge(clkIn) then
            newPixelIn <= false;
            frameEndIn <= false;        -- strobe

            if currWidth < TEST_WIDTH and currHeight < TEST_HEIGHT then
                pixelDataIn <= pixelArray.getPixel(currHeight, currWidth);
                newPixelIn  <= true;

                AlertIfNot(KERNEL_TB_ALERT_ID, pixelCounter = (currHeight * TEST_WIDTH) + currWidth, "Invalid pixelCounter count at (height, width): " & to_string(currHeight) & " x " & to_string(currWidth));

                pixelCounter := pixelCounter + 1;
            else                        -- hblank or vblank
                pixelDataIn <= (others => (others => '0'));
            end if;

            if currWidth >= TEST_WIDTH + HBLANK_CYCLES then
                currWidth := 0;

                if currHeight >= TEST_HEIGHT + VBLANK_CYCLES then
                    currHeight   := 0;
                    frameEndIn   <= true;
                    pixelCounter := 0;
                else
                    currHeight := currHeight + 1;
                end if;
            else
                currWidth := currWidth + 1;
            end if;
        end if;
    end process inputMockProc;

    sanityCheckProc : process(clkIn, rstAsyncIn)
        variable strobeCheck : boolean := false;
    begin
        if rstAsyncIn = '1' then
            strobeCheck := false;
        elsif rising_edge(clkIn) then
            AlertIfNot(KERNEL_TB_ALERT_ID, (newPixelOut xor frameEndOut) or (not newPixelOut and not frameEndOut), "Invalid frame end/new pixel signals state");

            if frameEndOut then
                AlertIf(KERNEL_TB_ALERT_ID, strobeCheck, "Frame end signal active for more than 1 clock cycle (not strobe)");

                strobeCheck := true;
            else
                strobeCheck := false;
            end if;
        end if;
    end process sanityCheckProc;

    outputCheckProc : process(clkIn, rstAsyncIn)
        constant OUTPUT_WIDTH  : natural := TEST_WIDTH - 2;
        constant OUTPUT_HEIGHT : natural := TEST_HEIGHT - 2;

        -- state registers
        variable currHeight   : Rgb_Img_Height_T := 0;
        variable currWidth    : Rgb_Img_Width_T  := 0;
        variable pixelCounter : natural          := 0;

        -- helper wire variables
        variable multiplyAccu  : integer;
        variable currPixelAccu : integer;
        variable tmpPixel      : Pixel_Data_T;

        pure function saturate_integer(value : integer) return natural is
        begin
            if value < 0 then
                return 0;
            elsif value >= 2**8 then
                return 2**8 - 1;
            else
                return value;
            end if;
        end function saturate_integer;
    begin
        -- debug signals
        --        currHeightDbg   <= currHeight;
        --        currWidthDbg    <= currWidth;
        --        pixelCounterDbg <= pixelCounter;

        if rstAsyncIn = '1' then
            currHeight   := 1;
            currWidth    := 1;
            pixelCounter := 0;
        elsif rising_edge(clkIn) then

            if newPixelOut then
                Log(KERNEL_TB_ALERT_ID, "Current location (height, width): " & to_string(currHeight) & " x " & to_string(currWidth), DEBUG);

                for color in Pixel_Color_T loop
                    currPixelAccu := 0;
                    for y in 0 to 2 loop
                        for x in 0 to 2 loop
                            multiplyAccu  := to_integer(pixelArray.getPixel(currHeight + y - 1, currWidth + x - 1)(color)) * TEST_KERNEL(y, x);
                            currPixelAccu := currPixelAccu + multiplyAccu;
                        end loop;
                    end loop;
                    currPixelAccu := currPixelAccu / (2**TEST_PRESCALE);
                    tmpPixel      := to_unsigned(saturate_integer(currPixelAccu), Pixel_Data_T'length);

                    AlertIfNot(KERNEL_TB_ALERT_ID, tmpPixel = pixelDataOut(color),
                               "Received incorrect pixel value for color " & to_string(color) & " at location (height, width): " & to_string(currHeight) & " x " & to_string(currWidth) & LF & "Expected: " & to_hstring(tmpPixel) & LF & "Received: " & to_hstring(pixelDataOut(color)));
                end loop;

                pixelCounter := pixelCounter + 1;

                if currWidth >= OUTPUT_WIDTH then
                    currWidth := 1;

                    if currHeight >= OUTPUT_HEIGHT then
                        currHeight := 1;
                    else
                        currHeight := currHeight + 1;
                    end if;

                    Log(KERNEL_TB_ALERT_ID, "New height: " & to_string(currHeight));
                else
                    currWidth := currWidth + 1;
                end if;

            elsif frameEndOut then
                Log(KERNEL_TB_ALERT_ID, "Frame end received", DEBUG);
                Log(KERNEL_TB_ALERT_ID, "Current height: " & to_string(currHeight), DEBUG);
                Log(KERNEL_TB_ALERT_ID, "Current width: " & to_string(currWidth), DEBUG);

                AlertIfNot(KERNEL_TB_ALERT_ID, currWidth = OUTPUT_WIDTH and currHeight = OUTPUT_HEIGHT,
                           "Invalid height or width at the end of the frame" & LF & "Current location (height, width): " & to_string(currHeight) & " x " & to_string(currWidth));
                AlertIfNot(KERNEL_TB_ALERT_ID, pixelCounter = (OUTPUT_HEIGHT * OUTPUT_WIDTH),
                           "Incorrect number of pixels received" & LF & "Expected: " & to_string(OUTPUT_HEIGHT * OUTPUT_WIDTH) & LF & "Received: " & to_string(pixelCounter));

                currWidth    := 1;
                currHeight   := 1;
                pixelCounter := 0;
            end if;
        end if;
    end process;
end tb;
