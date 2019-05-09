library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.common_pkg.all;
use work.ccd_ctrl_pkg.getCurrColor;

entity ccd_model is
    generic(
        INIT_HEIGHT : CCD_Height_Range := 1943;
        INIT_WIDTH  : CCD_Width_Range  := 2591
    );
    port(
        -- MODEL
        clkIn, nRstAsyncIn       : in    std_logic;
        pixClkOut, lineValidOut  : out   std_logic;
        frameValidOut, strobeOut : out   std_logic;
        dataOut                  : out   CCD_Pixel_Data_T;
        --- serial (i2c) communication
        sClkIn                   : in    std_logic;
        sDataIO                  : inout std_logic;
        -- DEBUG
        ccdArrayIn               : in    CCD_Matrix_T;
        frameDone                : out   boolean
    );
    subtype HBlank_Range is natural range 0 to 4095;
    subtype VBlank_Range is natural range 8 to 2047;
    -- depends on binning mode (this is for no bin [row, col])
    constant HBLANK_MIN        : HBlank_Range     := 782;
    constant VBLANK_MIN        : VBlank_Range     := 8;
    constant HBLANK_DEFAULT    : HBlank_Range     := 782;
    constant VBLANK_DEFAULT    : VBlank_Range     := 8;
    constant ROW_START_DEFAULT : CCD_Height_Range := 54;
    constant COL_START_DEFAULT : CCD_Width_Range  := 16;
    constant ROW_SIZE_DEFAULT  : CCD_Height_Range := 1943;
    constant COL_SIZE_DEFAULT  : CCD_Width_Range  := 2591;
end entity ccd_model;

-- TODO: write assert for rowStart, colStart, rowSize, colSize
architecture RTL of ccd_model is
    signal rowStart             : CCD_Height_Range := ROW_START_DEFAULT;
    signal colStart             : CCD_Width_Range  := COL_START_DEFAULT;
    subtype FOV_Height_Range is natural range 0 to CCD_Height_Range'high - rowStart;
    subtype FOV_Width_Range is natural range 0 to CCD_Width_Range'high - colStart;
    signal rowSize              : FOV_Height_Range := INIT_HEIGHT;
    signal colSize              : FOV_Width_Range  := INIT_WIDTH;
    -- in PIXCLKs
    signal hBlankCount          : natural          := 0;
    -- in image rows
    signal vBlankCount          : natural          := 0;
    signal rowMirror, colMirror : boolean          := false;

    signal isHBlank, isVBlank : boolean := false;
begin

    -- internal PLL currently not simulated
    pixClkOut <= clkIn;

    imageOutProc : process(clkIn, nRstAsyncIn)
        type CCD_State is (ReadLine, HBlankOut, VBlankOut);
        variable currState : CCD_State := ReadLine;

        -- absolute width
        variable currWidth    : CCD_Width_Range  := 0;
        -- absolute height
        variable currHeight   : CCD_Height_Range := 0;
        variable pixelCounter : natural          := 0;
        variable lineCounter  : natural          := 0;
    begin
        if nRstAsyncIn = '0' then
            currWidth    := colStart;
            currHeight   := rowStart;
            pixelCounter := 0;
            lineCounter  := 0;
            isHBlank     <= true;
            isVBlank     <= true;
        elsif falling_edge(clkIn) then
            case currState is
                when ReadLine =>
                    dataOut <= ccdArrayIn(currHeight, currWidth);

                when HBlankOut =>
                    if pixelCounter >= hBlankCount then
                        currState    := ReadLine;
                        pixelCounter := 0;
                    end if;
                    pixelCounter := pixelCounter + 1;

                when VBlankOut =>
                    if lineCounter >= vBlankCount then
                        lineCounter := 0;
                        currState   := ReadLine;
                    end if;

                    if pixelCounter >= hBlankCount then
                        pixelCounter := 0;
                        lineCounter  := lineCounter + 1;
                    end if;
                    pixelCounter := pixelCounter + 1;

            end case;
        end if;
    end process imageOutProc;

    serialCommsProc : process(clkIn, nRstAsyncIn)
    begin
        rowStart    <= ROW_START_DEFAULT;
        colStart    <= COL_START_DEFAULT;
        rowSize     <= INIT_HEIGHT;
        colSize     <= INIT_WIDTH;
        hBlankCount <= HBLANK_DEFAULT;
        vBlankCount <= VBLANK_DEFAULT;
        rowMirror   <= false;
        colMirror   <= false;
    end process serialCommsProc;
end architecture RTL;
