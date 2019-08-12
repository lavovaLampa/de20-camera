library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.img_pkg.Pixel_Aggregate_T;

use work.vga_pkg.all;

library poc;
use poc.utils.all;

entity vga_ctrl is
    port(
        clkIn, rstAsyncIn        : in  std_logic; -- 25 MHz (40 ns period) [640x480 @ 60Hz]
        -- input
        pixelDataIn              : in  Pixel_Aggregate_T;
        -- if true, provide new pixel on next clk cycle
        nextPixelOut             : out boolean;
        -- dac (ADV7123) i/o
        -- output data latched in DAC at rising_edge of clk (1 clk delay?)
        blankNegOut              : out std_logic; -- whether to blank on green?
        syncNegOut               : out std_logic; -- whether to sync on green
        pixelDataOut             : out Pixel_Aggregate_T;
        -- output directly to d-sub connector
        vSyncNegOut, hSyncNegOut : out std_logic
    );
end entity vga_ctrl;

architecture RTL of vga_ctrl is
begin
    -- we handle blanking & synchronization
    syncNegOut  <= '0';                 -- no sync on green
    blankNegOut <= '1';                 -- no direct blanking

    mainProc : process(clkIn, rstAsyncIn, pixelDataIn)
        variable currHeight : Vga_Height_Ptr_T := 0;
        variable currWidth  : Vga_Width_Ptr_t  := 0;
    begin
        -- if not blanking, provide new pixel
        if currWidth < IMG_WIDTH and currHeight < IMG_HEIGHT then
            nextPixelOut <= true;
            pixelDataOut <= pixelDataIn;
        else
            nextPixelOut <= false;
            pixelDataOut <= (others => (others => '0'));
        end if;

        if rstAsyncIn = '1' then
            currHeight := 0;
            currWidth  := 0;

            hSyncNegOut <= '1';
            vSyncNegOut <= '1';
        elsif rising_edge(clkIn) then
            -- delay signals by 1 clk to account for latching of signals in DAC
            hSyncNegOut <= to_sl(not (currWidth >= IMG_WIDTH + LINE_FRONT_PORCH and currWidth < IMG_WIDTH + LINE_FRONT_PORCH + LINE_SYNC_PULSE));
            vSyncNegOut <= to_sl(not (currHeight >= IMG_HEIGHT + FRAME_FRONT_PORCH and currHeight < IMG_HEIGHT + FRAME_FRONT_PORCH + FRAME_SYNC_PULSE));

            if currWidth >= Vga_Width_Ptr_T'high then
                currWidth := 0;

                if currHeight >= Vga_Height_Ptr_T'high then
                    currHeight := 0;
                else
                    currHeight := currHeight + 1;
                end if;
            else
                currWidth := currWidth + 1;
            end if;
        end if;
    end process mainProc;
end architecture RTL;
