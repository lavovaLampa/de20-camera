library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ccd_model_pkg.all;
use work.common_pkg.all;
use work.i2c_pkg.all;
use work.ccd_ctrl_pkg.getCurrColor;

entity ccd_model is
    generic(
        INIT_HEIGHT       : CCD_Height_Range := 484;
        INIT_WIDTH        : CCD_Width_Range  := 644;
        INIT_HEIGHT_START : CCD_Height_Range := ROW_START_DEFAULT;
        INIT_WIDTH_START  : CCD_Width_Range  := COL_START_DEFAULT;
        DEBUG             : boolean          := false
    );
    port(
        -- MODEL
        clkIn, nRstAsyncIn            : in    std_logic;
        pixClkOut, lineValidOut       : out   std_logic;
        frameValidOut, strobeOut      : out   std_logic;
        dataOut                       : out   CCD_Pixel_Data_T;
        --- serial (i2c) communication
        sClkIn                        : in    std_logic;
        sDataIO                       : inout std_logic;
        -- DEBUG
        ccdArrayIn                    : in    CCD_Matrix_T;
        frameDoneOut, configUpdateOut : out   boolean
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
    signal hBlank, vBlank : boolean := false;
begin

    -- internal PLL currently not implemented (but we invert the pixclock output)
    pixClkOut     <= not clkIn;
    lineValidOut  <= not boolToLogic(hBlank);
    frameValidOut <= not boolToLogic(vBlank);

    imageOutProc : process(clkIn, nRstAsyncIn)
        type CCD_State is (ConfigReadout, MatrixReadout);
        variable currState   : CCD_State := ConfigReadout;
        -- options valid for current frame (options are synchronized to frame boundaries)
        variable currOptions : CCD_Params_R;
        subtype Temp_Width_Range is natural range 0 to 6846;
        subtype Temp_Height_Range is natural range 0 to 4048;

        variable currWidth  : Temp_Width_Range      := 0; -- absolute
        variable currHeight : Temp_Height_Range     := 0; -- absolute
        variable debugCount : CCD_Pixel_Count_Range := 0;

        impure function getPixelOut(height : Temp_Height_Range; width : Temp_Width_Range) return CCD_Pixel_Data_T is
            constant pixelType : CCD_Pixel_Type := getCcdPixelType(height, width);
        begin
            case pixelType is
                when Dark =>
                    return X"000";

                when Vsync | Hsync =>
                    return X"000";

                when Active | Boundary =>
                    if height >= currOptions.rowStart and height < currOptions.rowStart + currOptions.rowSize and width >= currOptions.colStart and width < currOptions.colStart + currOptions.colSize then
                        debugPrint("CCD arrray access at (height, width): " & natural'image(height) & ", " & natural'image(width));
                        debugPrint("Array coords: " & natural'image(height - currOptions.rowStart) & ", " & natural'image(width - currOptions.colStart));
                        return ccdArrayIn(height - currOptions.rowStart, width - currOptions.colStart);
                    else
                        -- hblank
                        return X"000";
                    end if;
            end case;
        end function getPixelOut;
    begin
        if nRstAsyncIn = '0' then
            debugCount   := (currOptions.rowSize + 1) * (currOptions.colSize + 1);
            currWidth    := 0;
            currHeight   := 0;
            hBlank       <= true;
            vBlank       <= true;
            frameDoneOut <= false;
        elsif rising_edge(clkIn) then

            case currState is
                when ConfigReadout =>
                    if DEBUG then
                        assert debugCount = (currOptions.rowSize + 1) * (currOptions.colSize + 1)
                        report "Pixel count not equal to width * height of frame!"
                        severity failure;
                    end if;
                    currState    := MatrixReadout;
                    -- update current configuration parameters
                    currOptions  := paramsReg;
                    -- reset value to defaults
                    currWidth    := currOptions.colStart;
                    currHeight   := currOptions.rowStart;
                    debugCount   := 0;
                    frameDoneOut <= false;
                    debugPrint(
                        LF & "Row start: " & natural'image(currOptions.rowStart) & LF & "Col start: " & natural'image(currOptions.colStart)
                    );

                    assert getCurrColor(0, 0) = Green1
                    report "First pixel is not correctly aligned to whole Bayer pixel!"
                    severity failure;

                when MatrixReadout =>
                    hBlank     <= currWidth >= (currOptions.colStart + currOptions.colSize);
                    vBlank     <= currHeight >= (currOptions.rowStart + currOptions.rowSize);
                    debugCount := debugCount + 1;
                    dataOut    <= getPixelOut(currHeight, currWidth);

                    if currWidth < (currOptions.colStart + currOptions.colSize + currOptions.hblank) then
                        currWidth := currWidth + 1;
                    else
                        currWidth  := currOptions.colStart;
                        currHeight := currHeight + 1;
                    end if;

                    if currHeight > (currOptions.rowStart + currOptions.rowSize + currOptions.vblank) then
                        currState    := ConfigReadout;
                        frameDoneOut <= true;
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
                paramsReg.rowStart  <= INIT_HEIGHT_START;
                paramsReg.colStart  <= INIT_WIDTH_START;
                paramsReg.rowSize   <= INIT_HEIGHT;
                paramsReg.colSize   <= INIT_WIDTH;
                paramsReg.hblank    <= HBLANK_DEFAULT;
                paramsReg.vblank    <= VBLANK_DEFAULT;
                paramsReg.rowMirror <= false;
                paramsReg.colMirror <= false;
            elsif rising_edge(clkIn) then
                -- TODO: should be asserted only if valid value arrived?
                configUpdateOut <= newDataArrived;

                if newDataArrived then
                    case dataAddrIn is
                        when REG_ADDR.rowStart =>
                            tmpData := to_integer(unsigned(dataIn));
                            if tmpData mod 2 = 1 then
                                report "Expecting even value. Next lower even value will be used!"
                                severity warning;
                                tmpData := tmpData - 1;
                            end if;
                            if tmpData > 2004 then
                                report "Invalid value range. Legal range: [0 - 2004], even."
                                severity warning;
                            else
                                paramsReg.rowStart <= tmpData;
                            end if;

                        when REG_ADDR.colStart =>
                            tmpData := to_integer(unsigned(dataIn));
                            if tmpData mod 2 = 1 then
                                report "Expecting even value. Next lower even value will be used!"
                                severity warning;
                                tmpData := tmpData - 1;
                            end if;
                            if tmpData > 2750 then
                                report "Invalid value range. Legal range: [0 - 2750], even."
                                severity warning;
                            else
                                paramsReg.colStart <= tmpData;
                            end if;

                        when REG_ADDR.rowSize =>
                            tmpData := to_integer(unsigned(dataIn));
                            if tmpData mod 2 = 0 then
                                report "Expecting odd value. Next higher odd value will be used!"
                                severity warning;
                                tmpData := tmpData + 1;
                            end if;
                            if tmpData < 1 or tmpData > 2005 then
                                report "Invalid value range. Legal range: [1 - 2005], odd."
                                severity warning;
                            else
                                paramsReg.rowSize <= tmpData;
                            end if;

                        when REG_ADDR.colSize =>
                            tmpData := to_integer(unsigned(dataIn));
                            if tmpData mod 2 = 0 then
                                report "Expecting odd value. Next higher odd value will be used!"
                                severity warning;
                                tmpData := tmpData + 1;
                            end if;
                            if tmpData < 1 or tmpData > 2751 then
                                report "Invalid value range. Legal range: [1 - 2751], odd."
                                severity warning;
                            else
                                paramsReg.colSize <= tmpData;
                            end if;

                        when REG_ADDR.hblank =>
                            tmpData := to_integer(unsigned(dataIn));
                            if tmpData > 4095 then
                                report "Invalid value range. Legal range: [0 - 4095]."
                                severity warning;
                            else
                                if tmpData < HBLANK_MIN then
                                    report "Value under minimal Horizontal Blank value. Defaulting to minimum."
                                    severity warning;
                                    tmpData := HBLANK_MIN;
                                end if;
                                paramsReg.hblank <= tmpData;
                            end if;

                        when REG_ADDR.vblank =>
                            tmpData := to_integer(unsigned(dataIn));
                            if tmpData < 8 or tmpData > 2047 then
                                report "Invalid value range. Legal range: [8 - 2047]."
                                severity warning;
                            else
                                if tmpData < VBLANK_MIN then
                                    report "Value under minimal Vertical Blank value. Defaulting to minimum."
                                    severity warning;
                                    tmpData := VBLANK_MIN;
                                end if;
                                paramsReg.vblank <= tmpData;
                            end if;

                        when REG_ADDR.readMode2 =>
                            -- mirror row
                            paramsReg.rowMirror <= logicToBool(dataIn(15));
                            -- mirror column
                            paramsReg.colMirror <= logicToBool(dataIn(14));
                            debugPrint("ReadMode2 register updated. Using only upper 2 bits.");

                        when others =>
                            report "Register: 0x" & to_hstring(dataAddrIn) & " currently not implemented"
                            severity warning;
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
