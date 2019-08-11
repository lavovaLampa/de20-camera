library ieee;
use ieee.std_logic_1164.all;

use work.img_pkg.Pixel_Aggregate_T;
use work.img_pkg.Pixel_Data_T;

use work.sdram_pkg.all;

use work.ccd_pkg.all;

library osvvm;
context osvvm.OsvvmContext;

entity data_ctrl_tb is
    constant CCD_CLK_PERIOD : time := 20 ns;
    constant MEM_CLK_PERIOD : time := 10 ns;
    constant VGA_CLK_PERIOD : time := 40 ns;

    constant TEST_CCD_HEIGHT : natural := 10;
    constant TEST_CCD_WIDTH  : natural := 10;

    constant TEST_VGA_HEIGHT : natural := 480;
    constant TEST_VGA_WIDTH  : natural := 640;

    constant DATA_CTRL_TB_ALERT_ID : AlertLogIDType := GetAlertLogID("Data ctrl testbench", ALERTLOG_BASE_ID);
end data_ctrl_tb;

architecture tb of data_ctrl_tb is
    -- clock & reset signals
    signal ccdClk, vgaClk, memClk, pixClk : std_logic := '0';
    signal rstAsync                       : std_logic;

    -- ccd-side signals
    signal imgPixelDataOut      : Pixel_Aggregate_T;
    signal imgNewPixelOut       : boolean;
    signal imgFrameEndStrobeOut : boolean;
    signal ccdHBlankIn          : boolean;
    signal ccdVBlankIn          : boolean;

    -- vga-side signals
    signal vgaNextPixelIn : boolean;
    signal vgaPixelOut    : Pixel_Aggregate_T;

    -- mem signals
    signal memDataIo                   : Data_T;
    signal memOut                      : Mem_IO_R;
    signal memInitialized, memSimEnded : boolean;
    signal memClkStable                : std_logic := '0';

    -- testbench signals
    signal tbSimEnded : std_logic := '0';
begin
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
    
    dut : entity work.data_ctrl
        port map(
            ccdClkIn         => ccdClk,
            vgaClkIn         => vgaClk,
            memClkIn         => memClk,
            rstAsyncIn       => rstAsync,
            -- ccd i/o
            ccdPixelDataIn   => imgPixelDataOut,
            newPixelIn       => imgNewPixelOut,
            frameEndStrobeIn => imgFrameEndStrobeOut,
            ccdHBlankIn      => ccdHBlankIn,
            ccdVBlankIn      => ccdVBlankIn,
            -- vga i/o
            vgaNextPixelIn   => vgaNextPixelIn,
            vgaPixelOut      => vgaPixelOut,
            -- mem i/o
            memDataIo        => memDataIo,
            memOut           => memOut,
            memClkStableIn   => memClkStable
        );

--    ccdBlock : block
--        signal ccdPixelValidOut, demosaicNewPixelOut : boolean;
--        signal ccdFrameEndOut, demosaicFrameEndOut   : boolean;
--        signal demosaicPixelDataOut                  : Pixel_Aggregate_T;
--        signal ccdPixelDataOut                       : Pixel_Data_T;
--        signal ccdPixelCounterOut                    : Ccd_Img_Pixel_Ptr_T;
--    begin
--        imgConvolution : entity work.img_convolution
--                --            generic map(
--                --                CONVOLUTION_KERNEL => CONVOLUTION_KERNEL,
--                --                PRESCALE_AMOUNT    => PRESCALE_AMOUNT
--                --            )
--            port map(
--                clkIn             => ccdClk,
--                rstAsyncIn        => rstAsync,
--                -- img input
--                pixelDataIn       => demosaicPixelDataOut,
--                newPixelIn        => demosaicNewPixelOut,
--                frameEndStrobeIn  => demosaicFrameEndOut,
--                -- img output
--                pixelDataOut      => imgPixelDataOut,
--                newPixelOut       => imgNewPixelOut,
--                frameEndStrobeOut => imgFrameEndStrobeOut
--            );
--
--        ccdDemosaic : entity work.ccd_demosaic
--            port map(
--                clkIn            => pixClk,
--                rstAsyncIn       => rstAsync,
--                -- pixel in
--                pixelIn          => ccdPixelDataOut,
--                pixelValidIn     => ccdPixelValidOut,
--                frameEndStrobeIn => ccdFrameEndOut,
--                pixelCounterIn   => ccdPixelCounterOut,
--                -- pixel out
--                pixelOut         => demosaicPixelDataOut,
--                newPixelOut      => demosaicNewPixelOut,
--                frameEndOut      => demosaicFrameEndOut
--            );
--
--        tmpBlock : block
--            signal modelLineValid, modelFrameValid : std_logic;
--            signal modelPixelDataOut               : Ccd_Pixel_Data_T;
--
--            -- ccd model debug signals
--            signal modelFrameDoneDbg, modelConfigUpdateDbg : boolean;
--        begin
--
--            ccdCtrl : entity work.ccd_ctrl
--                port map(
--                    clkIn             => pixClk,
--                    rstAsyncIn        => rstAsync,
--                    -- ccd input
--                    frameValidIn      => modelFrameValid,
--                    lineValidIn       => modelLineValid,
--                    ccdPixelIn        => modelPixelDataOut,
--                    -- state output
--                    pixelValidOut     => ccdPixelValidOut,
--                    frameEndStrobeOut => ccdFrameEndOut,
--                    hBlankOut         => ccdHBlankIn,
--                    vBlankOut         => ccdVBlankIn,
--                    heightOut         => open,
--                    widthOut          => open,
--                    pixelCounterOut   => ccdPixelCounterOut,
--                    pixelOut          => ccdPixelDataOut
--                );
--
--            ccdModel : entity work.ccd_model
--                generic map(
--                    INIT_HEIGHT => TEST_CCD_HEIGHT,
--                    INIT_WIDTH  => TEST_CCD_WIDTH
--                    --            INIT_HEIGHT_START => INIT_HEIGHT_START,
--                    --            INIT_WIDTH_START  => INIT_WIDTH_START
--                )
--                port map(
--                    clkIn           => ccdClk,
--                    rstAsyncNegIn   => rstAsync,
--                    pixClkOut       => pixClk,
--                    -- state signals output
--                    lineValidOut    => modelLineValid,
--                    frameValidOut   => modelFrameValid,
--                    strobeOut       => open,
--                    -- pixel data out
--                    dataOut         => modelPixelDataOut,
--                    -- i2c i/o
--                    sClkIn          => '1',
--                    sDataIO         => open,
--                    -- debug signals
--                    frameDoneOut    => modelFrameDoneDbg,
--                    configUpdateOut => modelConfigUpdateDbg
--                );
--        end block tmpBlock;
--
--    end block ccdBlock;

    -- clock generation
    ccdClk <= not ccdClk after CCD_CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';
    vgaClk <= not vgaClk after VGA_CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';
    memClk <= not memClk after MEM_CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';

    stimuli : process
    begin
        vgaNextPixelIn <= false;
        memClkStable   <= '0';

        -- reset generation
        rstAsync <= '1';
        wait for 10 * MEM_CLK_PERIOD;
        rstAsync <= '0';
        wait for 10 * MEM_CLK_PERIOD;

        wait for 10 * MEM_CLK_PERIOD;
        memClkStable <= '1';

        wait for 5000 * CCD_CLK_PERIOD;

        -- Stop the clock and hence terminate the simulation
        tbSimEnded <= '1';
        wait;
    end process;

    --    vgaMockProc : process(vgaClk, rstAsync)
    --    begin
    --        if rstAsync = '1' then
    --
    --        elsif rising_edge(vgaClk) then
    --
    --        end if;
    --    end process vgaMockProc;

end tb;
