library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

use work.ccd_pkg.CCD_CONFIGURATION;
use work.data_ctrl_pkg.all;
use work.sdram_ctrl_pkg.all;

use work.img_pkg.Pixel_Aggregate_T;
use work.img_pkg.Pixel_Data_T;
use work.img_pkg.Pixel_Color_T;
use work.img_pkg.IMG_HEIGHT;
use work.img_pkg.IMG_WIDTH;

use work.sdram_pkg.Data_T;
use work.sdram_pkg.Col_Ptr_T;
use work.sdram_pkg.PAGE_LEN;
use work.sdram_pkg.Mem_IO_R;
use work.sdram_pkg.BANK_COUNT;

library PoC;

entity data_ctrl is
    port(
        ccdClkIn, vgaClkIn, memClkIn       : in    std_logic;
        rstAsyncIn                         : in    std_logic;
        -- write side
        ccdPixelDataIn                     : in    Pixel_Aggregate_T;
        ccdNewPixelIn, ccdFrameEndStrobeIn : in    boolean; -- from convolution kernel
        ccdHBlankIn, ccdVBlankIn           : in    boolean; -- from ccd ctrl
        -- read side
        vgaNextPixelIn                     : in    boolean;
        vgaVBlankIn                        : in    boolean;
        vgaPixelOut                        : out   Pixel_Aggregate_T;
        -- sdram i/o
        memDataIo                          : inout Data_T;
        memOut                             : out   Mem_IO_R;
        memClkStableIn                     : in    std_logic
    );
    -- 1 col = 16 b
    constant MEM_COLS_REQUIRED  : natural := natural(ceil(real(IMG_HEIGHT * IMG_WIDTH * 3) / 2.0));
    -- 1 page = 256 cols ~~ 170 pixels (170.666 pixels)
    constant MEM_PAGES_REQUIRED : natural := natural(ceil(real(MEM_COLS_REQUIRED) / real(PAGE_LEN)));
    -- 1 row = 4 pages (4 banks, each has the same row)
    constant ROWS_REQUIRED      : natural := natural(ceil(real(MEM_PAGES_REQUIRED) / real(BANK_COUNT)));

    constant MAX_ADDR      : Ctrl_Addr_T := to_unsigned(MEM_PAGES_REQUIRED, Ctrl_Addr_T'length) - 1;
    constant COL_REMAINDER : Col_Ptr_T   := MEM_COLS_REQUIRED mod PAGE_LEN;

    -- frame end in-band flag
    constant FRAME_END_FLAG : Packed_Pixel_T := X"01_01_01";
end entity data_ctrl;

architecture RTL of data_ctrl is
    -- sdram i/o
    --    signal sdramDataIo    : Data_T;
    --    signal sdramOut       : Mem_IO_R;
    --    signal sdramClkStable : std_logic;

    -- sdram-related declarations
    signal currAddr                    : Ctrl_Addr_T;
    signal currCmd                     : Ctrl_Cmd_T;
    signal cmdReady                    : boolean;
    signal dataReady, dataNextRequired : boolean;
    signal memRead, memWrite           : Data_T;

    -- ccd side declarations
    signal ccdFifo : Fifo_State_R;

    -- vga side declarations
    signal vgaFifo : Fifo_State_R;
begin
    -- ccd-side fifo write controller
    ccdWriteProc : process(ccdFifo, ccdNewPixelIn, ccdPixelDataIn, ccdFrameEndStrobeIn)
        variable packedPixel : Packed_Pixel_T; -- helper variable
    begin
        -- pack pixel data to be suitable for fifo
        packedPixel := std_logic_vector(ccdPixelDataIn(Red)) & std_logic_vector(ccdPixelDataIn(Green)) & std_logic_vector(ccdPixelDataIn(Blue));

        -- set pixel data to be input to the write port of ccd FIFO
        if ccdFifo.writePort.data = FRAME_END_FLAG then
            ccdFifo.writePort.data <= X"02_02_02";
        elsif ccdFrameEndStrobeIn then
            ccdFifo.writePort.data <= FRAME_END_FLAG;
        else
            ccdFifo.writePort.data <= packedPixel;
        end if;

        -- control whether to advance fifo write data pointer
        if ccdNewPixelIn or ccdFrameEndStrobeIn then
            assert not ccdFifo.writePort.full
            report "CCD write fifo is full"
            severity error;

            ccdFifo.writePort.put <= true;
        else
            ccdFifo.writePort.put <= false;
        end if;
    end process ccdWriteProc;

    vgaReadProc : process(vgaClkIn, rstAsyncIn, vgaFifo, vgaNextPixelIn)
        variable initialSync : boolean := false;
    begin
        vgaPixelOut <= (
            Red   => unsigned(vgaFifo.readPort.data(23 downto 16)),
            Green => unsigned(vgaFifo.readPort.data(15 downto 8)),
            Blue  => unsigned(vgaFifo.readPort.data(7 downto 0))
        );

        if vgaNextPixelIn and initialSync then
            assert vgaFifo.readPort.valid
            report "Cannot read from vga fifo, pixel not valid"
            severity error;

            vgaFifo.readPort.got <= true;
        else
            vgaFifo.readPort.got <= false;
        end if;

        if rstAsyncIn = '1' then
            initialSync := false;
        elsif rising_edge(vgaClkIn) then

            -- initially wait for vga vblank to synchronize reading from fifo
            if vgaVBlankIn then
                initialSync := true;
            end if;
        end if;
    end process vgaReadProc;

    -- mem arbiter process
    memProc : process(memClkIn, rstAsyncIn, ccdFifo, vgaFifo, cmdReady, ccdHBlankIn, ccdVBlankIn, dataNextRequired, dataReady, memRead)
        impure function next_addr(addr : Ctrl_Addr_T) return Ctrl_Addr_T is
        begin
            if addr = MAX_ADDR then
                return (others => '0');
            else
                return addr + 1;
            end if;
        end function next_addr;

        -- mem clk ~~ 2x faster than ccd clk
        constant CCD_HBLANK_CYCLES : natural := CCD_CONFIGURATION.hblank * 2;
        constant CCD_VBLANK_CYCLES : natural := CCD_HBLANK_CYCLES * CCD_CONFIGURATION.vblank;

        -- mem clk ~~ 4x faster than vga clk
        constant VGA_HBLANK_CYCLES : natural := 160 * 4;
        constant VGA_VBLANK_CYCLES : natural := VGA_HBLANK_CYCLES * 44;

        -- port state registers
        variable readPort        : Mem_Port_State_R := (currAddr => (others => '0'), pixelPacking => RedGreen, dataHold => (others => '0'));
        variable writePort       : Mem_Port_State_R := (currAddr => (others => '0'), pixelPacking => RedGreen, dataHold => (others => '0'));
        variable currBurstAddr   : Ctrl_Addr_T      := (others => '0');
        variable vgaCounter      : Col_Ptr_T        := 0;
        variable vgaFrameEndSent : boolean          := true;
        variable tmpFlag         : boolean          := false;
    begin
        tmpFlag := false;

        -- mem access scheduling/arbitration
        if cmdReady then
            -- write if ccd is in horizontal blank and fifo has enough pixels to fill memory page
            -- or ccd is in a vertical blank and we empty the fifo completely
            if (ccdFifo.readPort.fillState >= B"0010" and ccdHBlankIn) or ((ccdVBlankIn and ccdHBlankIn) and ccdFifo.readPort.valid and ccdFifo.readPort.data /= FRAME_END_FLAG) then
                currCmd  <= Write;
                currAddr <= writePort.currAddr;

            -- if ccdh is in a verical and horizontal blank, we refresh the memory
            elsif ccdHBlankIn and ccdVBlankIn then
                currCmd  <= Refresh;
                currAddr <= (others => '-');

            -- otherwise, if vga fifo has enough space for one mem page of pixels,
            -- we start read burst
            elsif vgaFifo.writePort.emptyState >= B"0010" and vgaFrameEndSent then
                currCmd  <= Read;
                currAddr <= readPort.currAddr;

            -- do nothing
            else
                currCmd  <= NoOp;
                currAddr <= (others => '-');
            end if;
        else
            currCmd  <= NoOp;
            currAddr <= (others => '-');
        end if;

        -- handle memory writes
        if dataNextRequired then
            assert ccdFifo.readPort.valid or ccdVBlankIn
            report "Cannot read data from ccd fifo because it's not valid yet";

            if ccdFifo.readPort.valid and ccdFifo.readPort.data /= FRAME_END_FLAG then
                case writePort.pixelPacking is
                    when RedGreen =>
                        memWrite             <= ccdFifo.readPort.data(23 downto 8);
                        ccdFifo.readPort.got <= true;

                    when BlueRed =>
                        memWrite             <= writePort.dataHold(15 downto 8) & ccdFifo.readPort.data(23 downto 16);
                        ccdFifo.readPort.got <= false;

                    when GreenBlue =>
                        memWrite             <= ccdFifo.readPort.data(15 downto 0);
                        ccdFifo.readPort.got <= true;
                end case;

            -- if writing last pixels during ccd vertical blank, the page
            -- doesn't have to be fully used, so we fill unused columns with zeros
            else
                memWrite             <= (others => '0');
                ccdFifo.readPort.got <= false;
            end if;

        -- if last pixel in the ccd fifo is frame end flag, we flush it so as not
        -- to desynchronize
        elsif ccdFifo.readPort.valid and ccdFifo.readPort.data = FRAME_END_FLAG then
            memWrite             <= (others => '-');
            ccdFifo.readPort.got <= true;

        -- else do nothing (do not advance fifo read ptr)
        else
            ccdFifo.readPort.got <= false;
            memWrite             <= (others => '-');
        end if;

        -- handle memory reads
        if dataReady then
            if currBurstAddr < MAX_ADDR or COL_REMAINDER = 0 or vgaCounter < COL_REMAINDER then
                assert not vgaFifo.writePort.full
                report "Cannot write data to vga fifo because it's full";

                case readPort.pixelPacking is
                    when RedGreen =>
                        vgaFifo.writePort.data <= (others => '-');
                        vgaFifo.writePort.put  <= false;

                    when BlueRed =>
                        vgaFifo.writePort.data <= readPort.dataHold & memRead(15 downto 8);
                        vgaFifo.writePort.put  <= true;

                    when GreenBlue =>
                        vgaFifo.writePort.data <= readPort.dataHold(7 downto 0) & memRead;
                        vgaFifo.writePort.put  <= true;
                end case;
            else
                vgaFifo.writePort.put  <= false;
                vgaFifo.writePort.data <= (others => '-');
            end if;
        elsif not vgaFrameEndSent then
            vgaFifo.writePort.put  <= true;
            vgaFifo.writePort.data <= FRAME_END_FLAG;
            tmpFlag                := true;
        else
            vgaFifo.writePort.put  <= false;
            vgafifo.writePort.data <= (others => '-');
        end if;

        if rstAsyncIn = '1' then
            readPort      := (currAddr => (others => '0'), pixelPacking => RedGreen, dataHold => (others => '0'));
            writePort     := (currAddr => (others => '0'), pixelPacking => RedGreen, dataHold => (others => '0'));
            currBurstAddr := (others => '0');

            vgaCounter      := 0;
            vgaFrameEndSent := true;
        elsif rising_edge(memClkIn) then
            -- advance address if used
            if cmdReady then
                if (ccdFifo.readPort.fillState > B"0010" and ccdHBlankIn) or ((ccdVBlankIn and ccdHBlankIn) and ccdFifo.readPort.valid and ccdFifo.readPort.data /= FRAME_END_FLAG) then
                    currBurstAddr      := writePort.currAddr;
                    writePort.currAddr := writePort.currAddr + 1;
                elsif vgaFifo.writePort.emptyState >= B"0010" then
                    currBurstAddr := readPort.currAddr;

                    if currBurstAddr = MAX_ADDR then
                        vgaFrameEndSent := false;
                    end if;

                    readPort.currAddr := next_addr(readPort.currAddr);
                end if;
            end if;

            -- ccd fifo read/mem write side
            if dataNextRequired then
                case writePort.pixelPacking is
                    when RedGreen  => writePort.dataHold(15 downto 8) := ccdFifo.readPort.data(7 downto 0);
                    when BlueRed   => writePort.dataHold := ccdFifo.readPort.data(15 downto 0);
                    when GreenBlue => null;
                end case;

                writePort.pixelPacking := next_packing(writePort.pixelPacking);
            elsif ccdFifo.readPort.valid and ccdFifo.readPort.data = FRAME_END_FLAG then
                writePort.pixelPacking := RedGreen;
                writePort.currAddr     := (others => '0');
            end if;

            -- mem read/vga fifo write side
            if dataReady then
                case readPort.pixelPacking is
                    when RedGreen  => readPort.dataHold := memRead;
                    when BlueRed   => readPort.dataHold(7 downto 0) := memRead(7 downto 0);
                    when GreenBlue => null;
                end case;

                readPort.pixelPacking := next_packing(readPort.pixelPacking);

                if vgaCounter >= PAGE_LEN - 1 then
                    vgaCounter := 0;
                else
                    vgaCounter := vgaCounter + 1;
                end if;
            end if;

            if tmpFlag then
                vgaFrameEndSent := true;
            end if;
        end if;
    end process memProc;

    ccdFifoEntity : entity PoC.fifo_ic_got
        generic map(
            DATA_WIDTH     => 24,
            MIN_DEPTH      => 1280,     -- gets rounded to 2048 = 2^11
            DATA_REG       => true,
            OUTPUT_REG     => false,
            ESTATE_WR_BITS => STATE_BITS,
            FSTATE_RD_BITS => STATE_BITS
        )
        port map(
            -- ccd side
            clkWriteIn => ccdClkIn,
            -- FIXME: synchronous reset
            rstWriteIn => rstAsyncIn,
            put        => ccdFifo.writePort.put,
            dataIn     => ccdFifo.writePort.data,
            full       => ccdFifo.writePort.full,
            estate_wr  => ccdFifo.writePort.emptyState,
            -- mem side
            clkReadIn  => memClkIn,
            -- FIXME: synchronous reset
            rstReadIn  => rstAsyncIn,
            got        => ccdFifo.readPort.got,
            valid      => ccdFifo.readPort.valid,
            dataOut    => ccdFifo.readPort.data,
            fstate_rd  => ccdFifo.readPort.fillState
        );

    vgaFifoEntity : entity PoC.fifo_ic_got
        generic map(
            DATA_WIDTH     => 24,
            MIN_DEPTH      => 1280,     --gets rounded to 2048 = 2^11
            DATA_REG       => true,
            OUTPUT_REG     => false,
            ESTATE_WR_BITS => STATE_BITS,
            FSTATE_RD_BITS => STATE_BITS
        )
        port map(
            -- mem side
            clkWriteIn => memClkIn,
            -- FIXME: synchronous reset
            rstWriteIn => rstAsyncIn,
            put        => vgaFifo.writePort.put,
            dataIn     => vgaFifo.writePort.data,
            full       => vgaFifo.writePort.full,
            estate_wr  => vgaFifo.writePort.emptyState,
            -- vga side
            clkReadIn  => vgaClkIn,
            -- FIXME: synchronous reset
            rstReadIn  => rstAsyncIn,
            got        => vgaFifo.readPort.got,
            valid      => vgaFifo.readPort.valid,
            dataOut    => vgaFifo.readPort.data,
            fstate_rd  => vgaFifo.readPort.fillState
        );

    sdramCtrl : entity work.sdram_ctrl_top
        generic map(
            -- FIXME: write in correct values
            PAGES_REQUIRED  => MEM_PAGES_REQUIRED,
            READ_BURST_LEN  => 4,
            WRITE_BURST_LEN => 4
        )
        port map(
            clkIn             => memClkIn,
            rstAsyncIn        => rstAsyncIn,
            clkStableIn       => memClkStableIn,
            -- input
            addrIn            => currAddr,
            cmdIn             => currCmd,
            dataIn            => memWrite,
            -- output
            cmdReadyOut       => cmdReady,
            provideNewDataOut => dataNextRequired,
            newDataOut        => dataReady,
            dataOut           => memRead,
            -- sdram i/o
            sdramDataIo       => memDataIo,
            sdramOut          => memOut
        );
end architecture RTL;
