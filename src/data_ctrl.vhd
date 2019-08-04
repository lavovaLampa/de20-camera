library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library PoC;

entity data_ctrl is
    port(
        ccdClkIn, vgaClkIn : in std_logic;
        rstAsyncIn         : in std_logic
    );
end entity data_ctrl;

architecture RTL of data_ctrl is
    -- sdram-related declarations
    signal memClk : std_logic;
    
    
    -- write side declarations
    signal writeFull                 : std_logic;
    signal writeDataIn, writeDataOut : std_logic_vector(2 downto 0);
    signal writePut, writeGot        : std_logic;
    signal writeValid : std_logic;

    -- read side declarations

begin

    writeFifo : entity PoC.fifo_ic_got
        generic map(
            D_BITS         => 24,
            MIN_DEPTH      => MIN_DEPTH,
            DATA_REG       => DATA_REG,
            OUTPUT_REG     => OUTPUT_REG,
            ESTATE_WR_BITS => ESTATE_WR_BITS,
            FSTATE_RD_BITS => FSTATE_RD_BITS
        )
        port map(
            clk_wr    => ccdClkIn,
            rst_wr    => rstAsyncIn,
            put       => writePut,
            din       => writeDataIn,
            full      => writeFull,
            estate_wr => estate_wr,
            clk_rd    => memClk,
            rst_rd    => rst_rd,
            got       => writeGot,
            valid     => writeValid,
            dout      => writeDataOut,
            fstate_rd => fstate_rd
        );

end architecture RTL;
