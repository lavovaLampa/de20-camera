library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

use work.ccd_pkg.CCD_PROPERTIES;
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

library PoC;

entity data_ctrl is
    port(
        ccdClkIn, vgaClkIn       : in  std_logic;
        rstAsyncIn               : in  std_logic;
        -- write side
        ccdPixelIn               : in  Pixel_Aggregate_T;
        newPixelIn               : in  boolean;
        ccdHBlankIn, ccdVBlankIn : in  boolean;
        -- read side
        vgaNextPixelIn           : in  boolean;
        vgaPixelOut              : out Pixel_Aggregate_T
    );
    constant MEM_CELLS_REQUIRED : natural := natural(ceil(real(IMG_HEIGHT * IMG_WIDTH * 3) / 2.0));
    constant PAGES_REQUIRED     : natural := natural(ceil(real(MEM_CELLS_REQUIRED) / real(PAGE_LEN)));
    constant ROWS_REQUIRED      : natural := natural(ceil(real(PAGES_REQUIRED) / 4.0));

    constant MAX_ADDR      : Ctrl_Addr_T := to_unsigned(PAGES_REQUIRED, Ctrl_Addr_T'length) - 1;
    constant COL_REMAINDER : Col_Ptr_T   := MEM_CELLS_REQUIRED mod PAGE_LEN;
end entity data_ctrl;

architecture RTL of data_ctrl is
    -- sdram-related declarations
    signal memClk              : std_logic;
    signal currAddr            : Ctrl_Addr_T;
    signal currCmd             : Ctrl_Cmd_T;
    signal cmdReady            : boolean;
    signal dataReady, dataNext : boolean;
    signal memRead, memWrite   : Data_T;

    -- ccd side declarations
    signal ccdFifo : Fifo_State_R;

    -- vga side declarations
    signal vgaFifo : Fifo_State_R;
begin

    ccdWriteProc : process(newPixelIn, ccdPixelIn, ccdFifo)
        variable packedPixel : Packed_Pixel_T;
    begin
        packedPixel            := std_logic_vector(ccdPixelIn(Red)) & std_logic_vector(ccdPixelIn(Green)) & std_logic_vector(ccdPixelIn(Blue));
        ccdFifo.writePort.data <= packedPixel;

        if newPixelIn then
            if not ccdFifo.writePort.full then
                ccdFifo.writePort.put <= true;
            else
                report "Ccd write pipeline is full"
                severity error;

                ccdFifo.writePort.put <= false;
            end if;
        else
            ccdFifo.writePort.put <= false;
        end if;
    end process ccdWriteProc;

    vgaReadProc : process(vgaClkIn, rstAsyncIn, vgaFifo)
    begin
        if rstAsyncIn = '1' then

        elsif rising_edge(vgaClkIn) then

        end if;
    end process vgaReadProc;

    memProc : process(memClk, rstAsyncIn, cmdReady, ccdHBlankIn, dataNext, ccdFifo, vgaFifo, dataReady, memRead)
        impure function next_addr(currAddr : Ctrl_Addr_T) return Ctrl_Addr_T is
        begin
            if currAddr = MAX_ADDR then
                return (others => '0');
            else
                return currAddr + 1;
            end if;
        end function next_addr;

        -- mem clk ~~ 2x faster than ccd clk
        constant CCD_HBLANK_CYCLES : natural := CCD_PROPERTIES.hblank * 2;
        constant CCD_VBLANK_CYCLES : natural := CCD_HBLANK_CYCLES * CCD_PROPERTIES.vblank;

        -- mem clk ~~ 4x faster than vga clk
        constant VGA_HBLANK_CYCLES : natural := 160 * 4;
        constant VGA_VBLANK_CYCLES : natural := VGA_HBLANK_CYCLES * 44;

        -- port state registers
        variable readPort      : Mem_Port_State_R                     := (nextAddr => (others => '0'), dataPacking => RedGreen, dataHold => (others => '0'));
        variable writePort     : Mem_Port_State_R                     := (nextAddr => (others => '0'), dataPacking => RedGreen, dataHold => (others => '0'));
        variable currBurstAddr : Ctrl_Addr_T                          := (others => '0');
        variable ccdCounter    : natural range 0 to CCD_VBLANK_CYCLES := 0;
        variable vgaCounter    : natural range 0 to VGA_VBLANK_CYCLES := 0;
    begin
    if cmdReady then
        if (ccdFifo.readPort.fillState > B"001" and ccdHBlankIn) or (ccdVBlankIn and ccdFifo.readPort.valid) then
            currCmd  <= Write;
            currAddr <= writePort.nextAddr;
        elsif vgaFifo.writePort.emptyState < 010 then
            currCmd  <= Read;
            currAddr <= readPort.nextAddr;
        elsif ccdVBlankIn then
            currCmd  <= Refresh;
            currAddr <= (others => '-');
        else
            currCmd  <= NoOp;
            currAddr <= (others => '-');
        end if;
    else
        currCmd  <= NoOp;
        currAddr <= (others => '-');
    end if;

    if dataNext then
        assert ccdFifo.readPort.valid or ccdVBlankIn
        report "Cannot read data from ccd fifo because it's not valid yet";

        if ccdFifo.readPort.valid then
            case writePort.dataPacking is
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
            assert writePort.dataPacking = RedGreen
            report "Last pixel ended on invalid boundary";

            memWrite             <= (others => '0');
            ccdFifo.readPort.got <= false;
        end if;
    else
        ccdFifo.readPort.got <= false;
        memWrite             <= (others => '-');
    end if;

    if dataReady then
        if readPort.nextAddr
            assert not vgaFifo.writePort.full
            report "Cannot write data to vga fifo because it's full";

            case readPort.dataPacking is
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
            vgafifo.writePort.data <= (others => '-');
        end if;

        if rstAsyncIn = '1' then
            readPort   := (nextAddr => (others => '0'), dataPacking => RedGreen, dataHold => (others => '0'));
            writePort  := (nextAddr => (others => '0'), dataPacking => RedGreen, dataHold => (others => '0'));
            ccdCounter := 0;
            vgaCounter := 0;
        elsif rising_edge(memClk) then
            if cmdReady then
                if ccdFifo.readPort.fillState > B"001" and ccdHBlankIn then
                    writePort.nextAddr := next_addr(writePort.nextAddr);
                elsif vgaFifo.writePort.emptyState < 111 then
                    readPort.nextAddr := next_addr(readPort.nextAddr);
                end if;
            end if;

            -- ccd fifo read/mem write side
            if dataNext then
                case writePort.dataPacking is
                    when RedGreen  => writePort.dataHold(15 downto 8) := ccdFifo.readPort.data(7 downto 0);
                    when BlueRed   => writePort.dataHold := ccdFifo.readPort.data(15 downto 0);
                    when GreenBlue => null;
                end case;

                writePort.dataPacking := next_packing(writePort.dataPacking);
            end if;

            -- mem read/vga fifo write side
            if dataReady then
                case readPort.dataPacking is
                    when RedGreen  => readPort.dataHold := memRead;
                    when BlueRed   => readPort.dataHold(7 downto 0) := memRead(7 downto 0);
                    when GreenBlue => null;
                end case;

                readPort.dataPacking := next_packing(readPort.dataPacking);
            end if;
        end if;
    end process memProc;

    ccdFifoEntity : entity PoC.fifo_ic_got
        generic map(
            D_BITS         => 24,
            MIN_DEPTH      => 1280,
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
            clkReadIn  => memClk,
            -- FIXME: synchronous reset
            rstReadIn  => rstAsyncIn,
            got        => ccdFifo.readPort.got,
            valid      => ccdFifo.readPort.valid,
            dataOut    => ccdFifo.readPort.data,
            fstate_rd  => ccdFifo.readPort.fillState
        );

    vgaFifoEntity : entity PoC.fifo_ic_got
        generic map(
            D_BITS         => 24,
            MIN_DEPTH      => 1280,
            DATA_REG       => true,
            OUTPUT_REG     => false,
            ESTATE_WR_BITS => STATE_BITS,
            FSTATE_RD_BITS => STATE_BITS
        )
        port map(
            -- mem side
            clkWriteIn => memClk,
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
            PAGES_REQUIRED  => PAGES_REQUIRED,
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
            dataIn       => memWrite,
            dataOut      => memRead,
            -- sdram i/o
            sdramDataIo  => sdramDataIo,
            sdramOut     => sdramOut,
            clkStableIn  => clkStableIn
        );
end architecture RTL;
