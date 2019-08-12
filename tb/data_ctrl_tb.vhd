library ieee;
use ieee.std_logic_1164.all;

use work.img_pkg.Pixel_Aggregate_T;
use work.img_pkg.Pixel_Data_T;
use work.img_pkg.Pixel_Color_T;

use work.sdram_pkg.all;

use work.ccd_pkg.all;

use work.vga_pkg.all;

library osvvm;
context osvvm.OsvvmContext;

entity data_ctrl_tb is
    constant CCD_CLK_PERIOD : time := 20 ns;
    constant MEM_CLK_PERIOD : time := 10 ns;
    constant VGA_CLK_PERIOD : time := 40 ns;

    constant TEST_CCD_HEIGHT        : natural := 480;
    constant TEST_CCD_WIDTH         : natural := 640;
    constant TEST_CCD_HBLANK_CYCLES : natural := 1024;
    constant TEST_CCD_VBLANK_CYCLES : natural := 131;

    constant TEST_FRAME_COUNT : natural := 3;

    constant DATA_CTRL_TB_ALERT_ID : AlertLogIDType := GetAlertLogID("Data ctrl testbench", ALERTLOG_BASE_ID);
end data_ctrl_tb;

architecture tb of data_ctrl_tb is
    -- clock & reset signals
    signal ccdClk, vgaClk, memClk : std_logic := '0';
    signal rstAsync, dataRstAsync : std_logic;

    -- ccd-side signals
    signal ccdPixelDataOut                      : Pixel_Aggregate_T;
    signal ccdNewPixelOut, ccdFrameEndStrobeOut : boolean;
    signal ccdHBlankOut, ccdVBlankOut           : boolean;

    -- vga-side signals
    signal vgaNextPixelOut, vgaVBlankOut : boolean;
    signal vgaPixelIn                    : Pixel_Aggregate_T;

    -- mem signals
    signal memDataIo                   : Data_T;
    signal memOut                      : Mem_IO_R;
    signal memInitialized, memSimEnded : boolean;
    signal memClkStable                : std_logic := '0';

    -- testbench signals
    signal tbSimEnded : std_logic := '0';
begin
    dut : entity work.data_ctrl
        port map(
            ccdClkIn         => ccdClk,
            vgaClkIn         => vgaClk,
            memClkIn         => memClk,
            rstAsyncIn       => dataRstAsync,
            -- ccd i/o
            ccdPixelDataIn   => ccdPixelDataOut,
            ccdNewPixelIn       => ccdNewPixelOut,
            ccdFrameEndStrobeIn => ccdFrameEndStrobeOut,
            ccdHBlankIn      => ccdHBlankOut,
            ccdVBlankIn      => ccdVBlankOut,
            -- vga i/o
            vgaNextPixelIn   => vgaNextPixelOut,
            vgaVBlankIn      => vgaVBlankOut,
            vgaPixelOut      => vgaPixelIn,
            -- mem i/o
            memDataIo        => memDataIo,
            memOut           => memOut,
            memClkStableIn   => memClkStable
        );

    sdramModel : entity work.sdram_model
        generic map(
            LOAD_FROM_FILE => false,
            DUMP_TO_FILE   => false
        )
        port map(
            clkIn              => memClk,
            addrIn             => memOut.addr,
            dataIo             => memDataIo,
            bankSelectIn       => memOut.bankSelect,
            clkEnableIn        => memOut.clkEnable,
            chipSelectNegIn    => memOut.cmdAggregate.chipSelectNeg,
            rowAddrStrobeNegIn => memOut.cmdAggregate.rowAddrStrobeNeg,
            colAddrStrobeNegIn => memOut.cmdAggregate.colAddrStrobeNeg,
            writeEnableNegIn   => memOut.cmdAggregate.writeEnableNeg,
            dqmIn              => memOut.dqm,
            -- debug signals
            isInitializedOut   => memInitialized,
            simEndedIn         => memSimEnded
        );

    -- clock generation
    ccdClk <= not ccdClk after CCD_CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';
    vgaClk <= not vgaClk after VGA_CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';
    memClk <= not memClk after MEM_CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';

    stimuli : process
    begin
        SetLogEnable(DEBUG, true);
        SetLogEnable(INFO, true);

        memClkStable <= '0';

        -- reset generation
        rstAsync     <= '1';
        dataRstAsync <= '1';
        wait for 10 * MEM_CLK_PERIOD;

        dataRstAsync <= '0';
        wait for 10 * MEM_CLK_PERIOD;

        memClkStable <= '1';

        wait until memInitialized;

        rstAsync <= '0';
        wait for 10 * MEM_CLK_PERIOD;

        for i in 0 to TEST_FRAME_COUNT - 1 loop
            wait until ccdFrameEndStrobeOut;
        end loop;

        -- Stop the clock and hence terminate the simulation
        tbSimEnded <= '1';
        wait;
    end process;

    ccdBlock : block
        signal currHeight, currWidth : natural;
    begin
        inputMockProc : process(ccdClk, rstAsync)
            variable randomGen : RandomPType;
        begin
            -- debug signals
            --        currHeightDbg   <= currHeight;
            --        currWidthDbg    <= currWidth;
            --        pixelCounterDbg <= pixelCounter;

            if rstAsync = '1' then
                ccdFrameEndStrobeOut <= false;
                ccdNewPixelOut       <= false;
                ccdPixelDataOut      <= (others => (others => '0'));

                currWidth  <= 0;
                currHeight <= 0;

                randomGen.InitSeed("ajfljsfowenvzoiru98423");
            elsif rising_edge(ccdClk) then
                ccdNewPixelOut       <= false;
                ccdFrameEndStrobeOut <= false; -- strobe

                ccdHBlankOut <= currWidth >= TEST_CCD_WIDTH;
                ccdVBlankOut <= currHeight >= TEST_CCD_HEIGHT;

                if currWidth < TEST_CCD_WIDTH and currHeight < TEST_CCD_HEIGHT then
                    ccdPixelDataOut <= (
                        Red   => randomGen.RandUnsigned(Pixel_Data_T'length),
                        Green => randomGen.RandUnsigned(Pixel_Data_T'length),
                        Blue  => randomGen.RandUnsigned(Pixel_Data_T'length)
                    );
                    ccdNewPixelOut  <= true;
                else                    -- hblank or vblank
                    ccdPixelDataOut <= (others => (others => '-'));
                    ccdNewPixelOut  <= false;
                end if;

                if currHeight = TEST_CCD_HEIGHT and currWidth = 0 then
                    ccdFrameEndStrobeOut <= true;
                end if;

                if currWidth >= TEST_CCD_WIDTH + TEST_CCD_HBLANK_CYCLES - 1 then
                    currWidth <= 0;

                    Log(DATA_CTRL_TB_ALERT_ID, "Current height: " & to_string(currHeight));

                    if currHeight >= TEST_CCD_HEIGHT + TEST_CCD_VBLANK_CYCLES - 1 then
                        currHeight <= 0;
                    else
                        currHeight <= currHeight + 1;
                    end if;
                else
                    currWidth <= currWidth + 1;
                end if;
            end if;
        end process inputMockProc;
    end block ccdBlock;

    vgaBlock : block
        signal currHeight, currWidth : natural;
    begin
        vgaMockProc : process(vgaClk, rstAsync, currHeight, currWidth)
        begin
            vgaVBlankOut    <= currHeight >= IMG_HEIGHT;
            vgaNextPixelOut <= currHeight < IMG_HEIGHT and currWidth < IMG_WIDTH;

            if rstAsync = '1' then
                currHeight <= 0;
                currWidth  <= 0;
            elsif rising_edge(vgaClk) then
                if currWidth > IMG_WIDTH + HORIZONTAL_BLANK_CYCLES then
                    currWidth <= 0;

                    if currHeight > IMG_HEIGHT + VERTICAL_BLANK_CYCLES then
                        currHeight <= 0;
                    else
                        currHeight <= currHeight + 1;
                    end if;
                else
                    currWidth <= currWidth + 1;
                end if;
            end if;
        end process vgaMockProc;
    end block vgaBlock;
end tb;
