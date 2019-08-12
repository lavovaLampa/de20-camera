library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.img_pkg.Pixel_Aggregate_T;
use work.img_pkg.Pixel_Color_T;
use work.img_pkg.Pixel_Data_T;

use work.vga_pkg.all;

library osvvm;
context osvvm.OsvvmContext;

entity vga_ctrl_tb is
    constant CLK_PERIOD       : time    := 40 ns; -- 25 MHz
    constant TEST_FRAME_COUNT : natural := 3;

    constant VGA_TB_ALERT_ID : AlertLogIDType := GetAlertLogID("VGA ctrl testbench", ALERTLOG_BASE_ID);
end vga_ctrl_tb;

architecture tb of vga_ctrl_tb is
    -- clk & reset
    signal clkIn      : std_logic;
    signal rstAsyncIn : std_logic;

    -- vga ctrl i/o
    signal vgaPixelDataIn  : Pixel_Aggregate_T;
    signal vgaNextPixelOut : boolean;

    -- vga i/o
    signal vSyncNegOut : std_logic;
    signal hSyncNegOut : std_logic;

    -- dac i/o
    signal blankNegOut          : std_logic;
    signal syncNegOut           : std_logic;
    signal vgaPixelDataOut      : Pixel_Aggregate_T;
    -- simulated dac output
    signal dacRedPixelDataOut   : Pixel_Data_T;
    signal dacGreenPixelDataOut : Pixel_Data_T;
    signal dacBluePixelDataOut  : Pixel_Data_T;

    -- tesbench signals
    signal tbClk      : std_logic := '0';
    signal tbSimEnded : std_logic := '0';
begin

    dut : entity work.vga_ctrl
        port map(
            clkIn        => clkIn,
            rstAsyncIn   => rstAsyncIn,
            -- i/o
            pixelDataIn  => vgaPixelDataIn,
            nextPixelOut => vgaNextPixelOut,
            blankNegOut  => blankNegOut,
            syncNegOut   => syncNegOut,
            pixelDataOut => vgaPixelDataOut,
            vSyncNegOut  => vSyncNegOut,
            hSyncNegOut  => hSyncNegOut
        );

    -- Clock generation
    tbClk <= not tbClk after CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';
    clkIn <= tbClk;

    stimuli : process
    begin
        -- Reset generation
        -- EDIT: Check that rstAsyncIn is really your reset signal
        rstAsyncIn <= '1';
        wait for 5 * CLK_PERIOD;
        rstAsyncIn <= '0';
        wait for 5 * CLK_PERIOD;

        for i in 0 to TEST_FRAME_COUNT - 1 loop
            Log(VGA_TB_ALERT_ID, "Current frame: " & to_string(i));

            wait until vSyncNegOut = '0';
            wait until vSyncNegOut = '1';
        end loop;

        -- Stop the clock and hence terminate the simulation
        tbSimEnded <= '1';
        wait;
    end process;

    dacDelayProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            dacRedPixelDataOut   <= (others => '0');
            dacGreenPixelDataOut <= (others => '0');
            dacBluePixelDataOut  <= (others => '0');
        elsif rising_edge(clkIn) then
            dacRedPixelDataOut   <= vgaPixelDataOut(Red);
            dacGreenPixelDataOut <= vgaPixelDataOut(Green);
            dacBluePixelDataOut  <= vgaPixelDataOut(Blue);
        end if;
    end process dacDelayProc;

    inputMock : process(clkIn, rstAsyncIn)
        variable currHeight, currWidth    : natural := 0;
        variable pixelCounter             : natural := 0;
        variable randomGen                : RandomPType;
        variable hSyncStrobe, vSyncStrobe : boolean := false;
    begin
        if rstAsyncIn = '1' then
            randomGen.InitSeed("jalfj20481230 v04cv12048");

            currHeight   := 0;
            currWidth    := 0;
            pixelCounter := 0;
            hSyncStrobe  := false;
            vSyncStrobe  := false;

            vgaPixelDataIn <= (
                Red   => randomGen.RandUnsigned(8),
                Green => randomGen.RandUnsigned(8),
                Blue  => randomGen.RandUnsigned(8)
            );
        elsif rising_edge(clkIn) then
            if vgaNextPixelOut then
                vgaPixelDataIn <= (
                    Red   => randomGen.RandUnsigned(8),
                    Green => randomGen.RandUnsigned(8),
                    Blue  => randomGen.RandUnsigned(8)
                );
                pixelCounter   := pixelCounter + 1;
                hSyncStrobe    := false;
                vSyncStrobe    := false;

                if currWidth = IMG_WIDTH - 1 then
                    currWidth := 0;

                    if currHeight = IMG_HEIGHT - 1 then
                        currHeight := 0;
                    else
                        currHeight := currHeight + 1;
                    end if;
                else
                    currWidth := currWidth + 1;
                end if;
            elsif hSyncNegOut = '0' then
                if not hSyncStrobe then
                    AlertIfNot(
                        VGA_TB_ALERT_ID, pixelCounter = IMG_WIDTH,
                        "Incorrect number of pixels output in current line" & LF & "Expected: " & to_string(IMG_WIDTH) & LF & "Received: " & to_string(pixelCounter),
                        FAILURE
                    );

                    pixelCounter := 0;
                    hSyncStrobe  := true;
                end if;
            elsif vSyncNegOut = '0' then
                if not vSyncStrobe then
                    --                    Log(VGA_TB_ALERT_ID, "Debug: " & to_string(currHeight) & " x " & to_string(currWidth));
                    AlertIfNot(
                        VGA_TB_ALERT_ID, currHeight = 0 and currWidth = 0,
                        "Incorrect number of pixels received in frame" & LF & "Current height x width: " & to_string(currHeight) & " x " & to_string(currWidth),
                        FAILURE
                    );

                    currHeight  := 0;
                    currWidth   := 0;
                    vSyncStrobe := true;
                end if;
            end if;
        end if;
    end process inputMock;

    syncCheckProc : process(clkIn, rstAsyncIn)
        variable hCounter, vCounter         : natural   := 1;
        variable hSynced, vSynced           : boolean   := false;
        variable hSyncLastVal, vSyncLastVal : std_logic := '1';
    begin
        if rstAsyncIn = '1' then
            hCounter := 1;
            vCounter := 1;

            hSynced := false;
            vSynced := false;

            hSyncLastVal := '1';
            vSyncLastVal := '1';
        elsif rising_edge(clkIn) then

            -- check if pixels are black during horizontal/vertical sync
            if hSyncNegOut = '0' or vSyncNegOut = '0' then
                AlertIfNot(
                    VGA_TB_ALERT_ID,
                    dacRedPixelDataOut = (Pixel_Data_T'range => '0') and dacGreenPixelDataOut = (Pixel_Data_T'range => '0') and dacBluePixelDataOut = (Pixel_Data_T'range => '0'),
                    "Pixels not black during horizontal/vertical sync"
                );
            end if;

            -- check if horizontal timing is met
            if hSynced then
                if hSyncLastVal = '1' and hSyncNegOut = '0' then
                    AlertIfNot(
                        VGA_TB_ALERT_ID,
                        hCounter = (IMG_WIDTH + HORIZONTAL_BLANK_CYCLES - LINE_SYNC_PULSE),
                        "Incorrect timing between horizontal blanks" & LF & "Expected: " & to_string((IMG_WIDTH + HORIZONTAL_BLANK_CYCLES - LINE_SYNC_PULSE)) & LF & "Received: " & to_string(hCounter)
                    );

                    hCounter := 0;
                elsif hSyncNegOut = '1' then
                    if hCounter < LINE_BACK_PORCH or hCounter >= LINE_BACK_PORCH + IMG_WIDTH then
                        AlertIfNot(
                            VGA_TB_ALERT_ID,
                            dacRedPixelDataOut = (Pixel_Data_T'range => '0') and dacGreenPixelDataOut = (Pixel_Data_T'range => '0') and dacBluePixelDataOut = (Pixel_Data_T'range => '0'),
                            "Pixels not black during horizontal blanks' front/back porch"
                        );
                    end if;

                    hCounter := hCounter + 1;
                end if;
            else
                if hSyncLastVal = '0' and hSyncNegOut = '1' then
                    hSynced  := true;
                    hCounter := 1;
                end if;
            end if;

            -- check if vertical timing is met
            if vSynced then
                if vSyncLastVal = '1' and vSyncNegOut = '0' then
                    AlertIfNot(
                        VGA_TB_ALERT_ID,
                        vCounter = ((IMG_WIDTH + HORIZONTAL_BLANK_CYCLES) * (IMG_HEIGHT + VERTICAL_BLANK_CYCLES - FRAME_SYNC_PULSE)),
                        "Incorrect timing between vertical blanks" & LF & "Expected: " & to_string(((IMG_WIDTH + HORIZONTAL_BLANK_CYCLES) * (IMG_HEIGHT + VERTICAL_BLANK_CYCLES - FRAME_SYNC_PULSE))) & LF & "Received: " & to_string(vCounter),
                        ERROR
                    );

                    vCounter := 0;
                elsif vSyncNegOut = '1' then
                    if vCounter < (IMG_WIDTH + HORIZONTAL_BLANK_CYCLES) * FRAME_BACK_PORCH or vCounter >= (IMG_WIDTH + HORIZONTAL_BLANK_CYCLES) * (FRAME_BACK_PORCH + IMG_HEIGHT) then
                        AlertIfNot(
                            VGA_TB_ALERT_ID,
                            dacRedPixelDataOut = (Pixel_Data_T'range => '0') and dacGreenPixelDataOut = (Pixel_Data_T'range => '0') and dacBluePixelDataOut = (Pixel_Data_T'range => '0'),
                            "Pixels not black during vertical blanks' front/back porch"
                        );
                    end if;

                    vCounter := vCounter + 1;
                end if;
            elsif vSyncLastVal = '0' and vSyncNegOut = '1' then
                vSynced  := true;
                vCounter := 1;
            end if;

            hSyncLastVal := hSyncNegOut;
            vSyncLastVal := vSyncNegOut;
        end if;
    end process syncCheckProc;
end tb;
