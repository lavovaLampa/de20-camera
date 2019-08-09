-- Testbench automatically generated online
-- at http://vhdl.lapinoo.net
-- Generation date : 7.8.2019 22:25:44 GMT

library ieee;
use ieee.std_logic_1164.all;

entity data_ctrl_tb is
end data_ctrl_tb;

architecture tb of data_ctrl_tb is

    component data_ctrl
        port(ccdClkIn       : in  std_logic;
             vgaClkIn       : in  std_logic;
             rstAsyncIn     : in  std_logic;
             ccdPixelDataIn : in  pixel_aggregate_t;
             newPixelIn     : in  boolean;
             ccdHBlankIn    : in  boolean;
             ccdVBlankIn    : in  boolean;
             vgaNextPixelIn : in  boolean;
             vgaPixelOut    : out pixel_aggregate_t);
    end component;

    signal ccdClkIn       : std_logic;
    signal vgaClkIn       : std_logic;
    signal rstAsyncIn     : std_logic;
    signal ccdPixelDataIn : pixel_aggregate_t;
    signal newPixelIn     : boolean;
    signal ccdHBlankIn    : boolean;
    signal ccdVBlankIn    : boolean;
    signal vgaNextPixelIn : boolean;
    signal vgaPixelOut    : pixel_aggregate_t;

    constant TbPeriod : time      := 1000 ns; -- EDIT Put right period here
    signal TbClock    : std_logic := '0';
    signal TbSimEnded : std_logic := '0';

begin

    dut : data_ctrl
        port map(ccdClkIn       => ccdClkIn,
                 vgaClkIn       => vgaClkIn,
                 rstAsyncIn     => rstAsyncIn,
                 ccdPixelDataIn => ccdPixelDataIn,
                 newPixelIn     => newPixelIn,
                 ccdHBlankIn    => ccdHBlankIn,
                 ccdVBlankIn    => ccdVBlankIn,
                 vgaNextPixelIn => vgaNextPixelIn,
                 vgaPixelOut    => vgaPixelOut);

    -- Clock generation
    TbClock <= not TbClock after TbPeriod / 2 when TbSimEnded /= '1' else '0';

    -- EDIT: Check that ccdClkIn is really your main clock signal
    ccdClkIn <= TbClock;

    stimuli : process
    begin
        -- EDIT Adapt initialization as needed
        vgaClkIn       <= '0';
        ccdPixelDataIn <= '0';
        newPixelIn     <= '0';
        ccdHBlankIn    <= '0';
        ccdVBlankIn    <= '0';
        vgaNextPixelIn <= '0';

        -- Reset generation
        -- EDIT: Check that rstAsyncIn is really your reset signal
        rstAsyncIn <= '1';
        wait for 100 ns;
        rstAsyncIn <= '0';
        wait for 100 ns;

        -- EDIT Add stimuli here
        wait for 100 * TbPeriod;

        -- Stop the clock and hence terminate the simulation
        TbSimEnded <= '1';
        wait;
    end process;

end tb;
