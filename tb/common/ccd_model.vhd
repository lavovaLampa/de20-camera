library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ccd_model_pkg.all;
use work.common_pkg.all;
use work.i2c_pkg.all;
use work.ccd_ctrl_pkg.getCurrColor;

entity ccd_model is
    generic(
        INIT_HEIGHT : CCD_Height_Range := 1943;
        INIT_WIDTH  : CCD_Width_Range  := 2591;
        DEBUG       : boolean          := false
    );
    port(
        -- MODEL
        clkIn, nRstAsyncIn         : in    std_logic;
        pixClkOut, lineValidOut    : out   std_logic;
        frameValidOut, strobeOut   : out   std_logic;
        dataOut                    : out   CCD_Pixel_Data_T;
        --- serial (i2c) communication
        sClkIn                     : in    std_logic;
        sDataIO                    : inout std_logic;
        -- DEBUG
        ccdArrayIn                 : in    CCD_Matrix_T;
        frameDone, configUpdateOut : out   boolean
    );

    procedure debugPrint(val : string) is
    begin
        if DEBUG then
            report val;
        end if;
    end procedure debugPrint;

end entity ccd_model;

-- TODO: write assert for rowStart, colStart, rowSize, colSize
architecture RTL of ccd_model is
    --    subtype FOV_Height_Range is natural range 0 to CCD_Height_Range'high - currOptions.rowStart;
    --    subtype FOV_Width_Range is natural range 0 to CCD_Width_Range'high - currOptions.colStart;
    signal paramsReg : CCD_Params_R := (
        rowStart  => ROW_START_DEFAULT,
        colStart  => COL_START_DEFAULT,
        rowSize   => INIT_HEIGHT,
        colSize   => INIT_WIDTH,
        hblank    => HBLANK_DEFAULT,
        vblank    => VBLANK_DEFAULT,
        rowMirror => false,
        colMirror => false
    );

    -- TODO: better names
    signal isHBlank, isVBlank : boolean := false;
begin

    -- internal PLL currently not implemented
    pixClkOut <= clkIn;

    imageOutProc : process(clkIn, nRstAsyncIn)
        type CCD_State is (ConfigReadout, MatrixReadout);
        variable currState   : CCD_State := ConfigReadout;
        -- options valid for current frame (options are synchronized to frame boundaries)
        variable currOptions : CCD_Params_R;

        variable currWidth, widthEnd   : CCD_Width_Range  := 0; -- absolute
        variable currHeight, heightEnd : CCD_Height_Range := 0; -- absolute

        impure function getPixelOut(height : CCD_Height_Range; width : CCD_Width_Range)
        return CCD_Pixel_Data_T is
        begin
            -- FIXME: bad bounds calculation!
            if height > currOptions.rowStart and height < currOptions.rowStart + currOptions.rowSize and width > currOptions.colStart and width < currOptions.colStart + currOptions.colSize then
                return ccdArrayIn(height, width);
            else
                -- hblank/vblank occuring
                debugPrint("vblank/hblank pixel query");
                return X"000";
            end if;
        end function getPixelOut;
    begin
        if nRstAsyncIn = '0' then
            currWidth  := 0;
            currHeight := 0;
            isHBlank   <= true;
            isVBlank   <= true;
        elsif falling_edge(clkIn) then
            isHBlank <= currWidth > (currOptions.colStart + currOptions.colSize);
            isVblank <= currHeight > (currOptions.rowStart + currOptions.rowSize);

            case currState is
                when ConfigReadout =>
                    currState   := MatrixReadout;
                    -- update current configuration parameters
                    currOptions := paramsReg;
                    -- reset value to defaults
                    -- FIXME: hodnoty sa pouzivaju inak (val - 1?)
                    currWidth   := currOptions.colStart;
                    widthEnd    := currOptions.colStart + currOptions.colSize;
                    currHeight  := currOptions.rowStart;
                    heightEnd   := currOptions.rowStart + currOptions.rowSize;

                when MatrixReadout =>
                    dataOut <= getPixelOut(currHeight, currWidth);

                    if currWidth < (widthEnd + currOptions.hblank) then
                        currWidth := currWidth + 1;
                    else
                        currWidth  := 0;
                        currHeight := currHeight + 1;
                    end if;

                    if currHeight >= (heightEnd + currOptions.vblank) then
                        currState := ConfigReadout;
                    end if;

            end case;
        end if;
    end process imageOutProc;

    serialConfig : block
        signal newDataArrived : boolean;
        signal dataIn         : I2C_Data;
        signal dataAddrIn     : I2C_Addr;
        signal rstAsyncIn     : std_logic;
    begin

        rstAsyncIn <= not nRstAsyncIn;

        serialCommsProc : process(clkIn, nRstAsyncIn)
            variable tmpData : natural;
        begin
            if nRstAsyncIn = '0' then
                -- initialize defaults
                paramsReg.rowStart  <= ROW_START_DEFAULT;
                paramsReg.colStart  <= COL_START_DEFAULT;
                paramsReg.rowSize   <= INIT_HEIGHT;
                paramsReg.colSize   <= INIT_WIDTH;
                paramsReg.hblank    <= HBLANK_DEFAULT;
                paramsReg.vblank    <= VBLANK_DEFAULT;
                paramsReg.rowMirror <= false;
                paramsReg.colMirror <= false;
            elsif rising_edge(clkIn) then
                if newDataArrived then
                    case dataAddrIn is
                        when REG_ADDR.rowStart =>
                            tmpData := to_integer(unsigned(dataIn));
                            if tmpData mod 2 = 1 then
                                report "Expecting odd value. Next lower even value will be used!" severity warning;
                                tmpData := tmpData - 1;
                            end if;
                            if tmpData > 2004 then
                                report "Invalid value range. Legal range: [0 - 2004], even." severity warning;
                            else
                                paramsReg.rowStart <= tmpData;
                            end if;

                        when REG_ADDR.colStart =>
                            tmpData := to_integer(unsigned(dataIn));
                            if tmpData mod 2 = 1 then
                                report "Expecting odd value. Next lower even value will be used!" severity warning;
                                tmpData := tmpData - 1;
                            end if;
                            if tmpData > 2750 then
                                report "Invalid value range. Legal range: [0 - 2750], even." severity warning;
                            else
                                paramsReg.colStart <= tmpData;
                            end if;

                        when REG_ADDR.rowSize =>
                            tmpData := to_integer(unsigned(dataIn));
                            if tmpData mod 2 = 0 then
                                report "Expecting even value. Next higher odd value will be used!" severity warning;
                                tmpData := tmpData + 1;
                            end if;
                            if tmpData < 1 or tmpData > 2005 then
                                report "Invalid value range. Legal range: [1 - 2005], odd." severity warning;
                            else
                                paramsReg.rowSize <= tmpData;
                            end if;

                        when REG_ADDR.colSize =>
                            tmpData := to_integer(unsigned(dataIn));
                            if tmpData mod 2 = 0 then
                                report "Expecting even value. Next higher odd value will be used!" severity warning;
                                tmpData := tmpData + 1;
                            end if;
                            if tmpData < 1 or tmpData > 2751 then
                                report "Invalid value range. Legal range: [1 - 2751], odd." severity warning;
                            else
                                paramsReg.colSize <= tmpData;
                            end if;

                        when REG_ADDR.hblank =>
                            tmpData := to_integer(unsigned(dataIn));
                            if tmpData > 4095 then
                                report "Invalid value range. Legal range: [0 - 4095]." severity warning;
                            else
                                if tmpData < HBLANK_MIN then
                                    report "Value under minimal Horizontal Blank value. Defaulting to minimum." severity warning;
                                    tmpData := HBLANK_MIN;
                                end if;
                                paramsReg.hblank <= tmpData;
                            end if;

                        when REG_ADDR.vblank =>
                            tmpData := to_integer(unsigned(dataIn));
                            if tmpData < 8 or tmpData > 2047 then
                                report "Invalid value range. Legal range: [8 - 2047]." severity warning;
                            else
                                if tmpData < VBLANK_MIN then
                                    report "Value under minimal Vertical Blank value. Defaulting to minimum." severity warning;
                                    tmpData := VBLANK_MIN;
                                end if;
                                paramsReg.vblank <= tmpData;
                            end if;

                        when REG_ADDR.readMode2 =>
                            -- mirror row
                            paramsReg.rowMirror <= logicToBool(dataIn(15));
                            -- mirror column
                            paramsReg.colMirror <= logicToBool(dataIn(14));

                        when others =>
                            report "Register: 0x" & to_hstring(dataAddrIn) &
                            " currently not implemented" severity warning;
                    end case;
                end if;
            end if;
        end process serialCommsProc;

        iicSlave : entity work.i2c_slave_model
            generic map(
                DEBUG      => false,
                CHECK_DATA => false
            )
            port map(
                testClkIn          => clkIn,
                sClkIn             => sClkIn,
                rstAsyncIn         => rstAsyncIn,
                dataReceivedOut    => newDataArrived,
                sDataIO            => sDataIO,
                recvDevAddrOut     => open,
                recvDataAddrOut    => dataAddrIn,
                recvDataOut        => dataIn,
                expectedDevAddrIn  => (others => '-'),
                expectedDataAddrIn => (others => '-'),
                expectedDataIn     => (others => '-')
            );

    end block serialConfig;
end architecture RTL;
