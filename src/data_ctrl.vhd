library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.data_ctrl_pkg.all;
use work.sdram_ctrl_pkg.all;
use work.img_pkg.Pixel_Aggregate_T;
use work.img_pkg.Pixel_Data_T;
use work.img_pkg.Pixel_Color_T;
use work.sdram_pkg.Data_T;

library PoC;

entity data_ctrl is
    port(
        ccdClkIn, vgaClkIn     : in  std_logic;
        rstAsyncIn             : in  std_logic;
        -- write side
        ccdPixelIn             : in  Pixel_Aggregate_T;
        newPixelIn, frameEndIn : in  boolean;
        -- read side
        vgaPixelOut            : out Pixel_Aggregate_T
    );
    constant MAX_ROW        : natural        := 1400;
    constant FRAME_END_FLAG : Packed_Pixel_T := X"01_01_01";
end entity data_ctrl;

architecture RTL of data_ctrl is
    -- sdram-related declarations
    signal memClk              : std_logic;
    signal currAddr            : Ctrl_Addr_T;
    signal currCmd             : Ctrl_Cmd_T;
    signal cmdReady            : boolean;
    signal dataReady, dataNext : boolean;
    signal dataRead, dataWrite : Data_T;

    -- write side declarations
    signal ccdFull               : std_logic;
    signal ccdDataIn, ccdDataOut : Packed_Pixel_T;
    signal ccdPut, ccdGot        : std_logic;
    signal ccdValid              : std_logic;
    signal ccdState              : Fifo_State_R;

    -- read side declarations
    signal vgaFull               : std_logic;
    signal vgaDataIn, vgaDataOut : Packed_Pixel_T;
    signal vgaPut, vgaGot        : std_logic;
    signal vgaValid              : std_logic;
    signal vgaState              : Fifo_State_R;
begin

    ccdWriteProc : process(newPixelIn, ccdPixelIn, ccdFull, frameEndIn)
        variable packedPixel : Packed_Pixel_T;
    begin
        packedPixel := std_logic_vector(ccdPixelIn(Red)) & std_logic_vector(ccdPixelIn(Green)) & std_logic_vector(ccdPixelIn(Blue));

        -- in-band frame end signalling
        if packedPixel = FRAME_END_FLAG then
            ccdDataIn <= X"02_02_02";
        elsif frameEndIn then
            ccdDataIn <= FRAME_END_FLAG;
        else
            ccdDataIn <= packedPixel;
        end if;

        if newPixelIn then
            if not ccdFull then
                ccdPut <= '1';
            else
                report "Ccd write pipeline is full"
                severity error;

                ccdPut <= '0';
            end if;
        else
            ccdPut <= '0';
        end if;
    end process ccdWriteProc;

    vgaReadProc : process(vgaClkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then

        elsif rising_edge(vgaClkIn) then

        end if;
    end process vgaReadProc;

    memProc : process(memClk, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then

        elsif rising_edge(memClk) then

        end if;
    end process memProc;

    ccdFifo : entity PoC.fifo_ic_got
        generic map(
            D_BITS         => 24,
            MIN_DEPTH      => 1280,
            DATA_REG       => true,
            OUTPUT_REG     => false,
            ESTATE_WR_BITS => STATE_BITS,
            FSTATE_RD_BITS => STATE_BITS
        )
        port map(
            clk_wr    => ccdClkIn,
            rst_wr    => rstAsyncIn,
            put       => ccdPut,
            dataIn    => ccdDataIn,
            full      => ccdFull,
            estate_wr => ccdState.emptyState,
            clk_rd    => memClk,
            rst_rd    => rstAsyncIn,
            got       => ccdGot,
            valid     => ccdValid,
            dataOut   => ccdDataOut,
            fstate_rd => ccdState.fullState
        );

    vgaFifo : entity PoC.fifo_ic_got
        generic map(
            D_BITS         => 24,
            MIN_DEPTH      => 1280,
            DATA_REG       => true,
            OUTPUT_REG     => false,
            ESTATE_WR_BITS => STATE_BITS,
            FSTATE_RD_BITS => STATE_BITS
        )
        port map(
            clk_wr    => memClk,
            rst_wr    => rstAsyncIn,
            put       => vgaPut,
            dataIn    => vgaDataIn,
            full      => vgaFull,
            estate_wr => vgaState.emptyState,
            clk_rd    => vgaClkIn,
            rst_rd    => rstAsyncIn,
            got       => vgaGot,
            valid     => vgaValid,
            dataOut   => vgaDataOut,
            fstate_rd => vgaState.fullState
        );

    sdramCtrl : entity work.sdram_ctrl_top
        generic map(
            -- FIXME: write in correct values
            ROW_MAX         => MAX_ROW,
            READ_BURST_LEN  => 4,
            WRITE_BURST_LEN => 4
        )
        port map(
            clkIn        => memClk,
            rstAsyncIn   => rstAsyncIn,
            addrIn       => currAddr,
            cmdIn        => currCmd,
            cmdReadyOut  => cmdReady,
            dataReadyOut => dataReady,
            nextDataOut  => dataNext,
            dataIn       => dataWrite,
            dataOut      => dataRead,
            -- sdram i/o
            sdramDataIo  => sdramDataIo,
            sdramOut     => sdramOut,
            clkStableIn  => clkStableIn
        );
end architecture RTL;
