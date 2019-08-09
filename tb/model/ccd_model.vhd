library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.ccd_model_pkg.all;
use work.ccd_pkg.all;
use work.i2c_pkg.all;

library PoC;
use PoC.utils.all;

library osvvm;
context osvvm.OsvvmContext;

entity ccd_model is
    generic(
        INIT_HEIGHT       : Ccd_Active_Height_Ptr_T := 484;
        INIT_WIDTH        : Ccd_Active_Width_Ptr_T  := 644;
        INIT_HEIGHT_START : Ccd_Active_Height_Ptr_T := ROW_START_DEFAULT;
        INIT_WIDTH_START  : Ccd_Active_Width_Ptr_T  := COL_START_DEFAULT
    );
    port(
        clkIn, rstAsyncNegIn          : in    std_logic;
        -- ccd i/o
        pixClkOut, lineValidOut       : out   std_logic;
        frameValidOut, strobeOut      : out   std_logic;
        -- pixel data i/o
        dataOut                       : out   Ccd_Pixel_Data_T;
        --- i2c config i/o
        sClkIn                        : in    std_logic;
        sDataIO                       : inout std_logic;
        -- debug signals
        frameDoneOut, configUpdateOut : out   boolean
    );
    constant CCD_ALERT_ID : AlertLogIDType := GetAlertLogID("CCD MODEL", ALERTLOG_BASE_ID);
end entity ccd_model;

-- TODO: write assert for rowStart, colStart, rowSize, colSize
architecture model of ccd_model is
    signal paramsReg : Ccd_Params_R := (
        rowStart  => ROW_START_DEFAULT,
        colStart  => COL_START_DEFAULT,
        rowSize   => INIT_HEIGHT,
        colSize   => INIT_WIDTH,
        hblank    => HBLANK_DEFAULT,
        vblank    => VBLANK_DEFAULT,
        rowMirror => false,
        colMirror => false
    );

    -- helper state registers
    signal hBlank, vBlank : boolean := true;

    -- debug signals
    signal currHeightDbg   : natural;
    signal currWidthDbg    : natural;
    signal pixelCounterDbg : natural;
    signal currOptionsDbg  : Ccd_Params_R;
begin

    -- internal PLL currently not implemented (but we invert the pixclock output)
    pixClkOut     <= not clkIn;
    lineValidOut  <= not to_sl(hBlank);
    frameValidOut <= not to_sl(vBlank);

    imageOutProc : process(clkIn, rstAsyncNegIn, paramsReg)
        subtype Model_Width_Ptr_T is natural range 0 to ARRAY_WIDTH + Horizontal_Blank_T'high - 1;
        subtype Model_Height_Ptr_T is natural range 0 to ARRAY_HEIGHT + Vertical_Blank_T'high - 1;

        -- state variables
        variable currOptions  : Ccd_Params_R; -- currently used options (writes synchronized to frame boundaries)
        variable currWidth    : Model_Width_Ptr_T      := 0;
        variable currHeight   : Model_Height_Ptr_T     := 0;
        variable pixelCounter : Ccd_Active_Pixel_Ptr_T := 0;

        impure function getPixelOut(height : natural; width : natural) return Ccd_Pixel_Data_T is
            constant absoluteHeight : natural     := height + currOptions.rowStart;
            constant absoluteWidth  : natural     := width + currOptions.colStart;
            constant pixelType      : Ccd_Pixel_T := get_ccd_pixel_type(absoluteHeight, absoluteWidth);
        begin
            case pixelType is
                when Dark =>
                    return (others => '0');

                when VerticalBlank | HorizontalBlank =>
                    return (others => '-');

                when Active =>
                    if height < currOptions.rowSize and width < currOptions.colSize then
                        Log(CCD_ALERT_ID, "CCD arrray access at (height, width): " & natural'image(absoluteHeight) & ", " & natural'image(absoluteWidth), DEBUG);
                        Log(CCD_ALERT_ID, "Array coords: " & natural'image(absoluteHeight - currOptions.rowStart) & ", " & natural'image(absoluteWidth - currOptions.colStart), DEBUG);
                        return pixelArray.getPixel(height, width);
                    else
                        -- hblank and/or vblank
                        return X"000";
                    end if;

                when Boundary =>
                    report "Currently unsupported"
                    severity error;

                    return (others => '0');
            end case;
        end function getPixelOut;
    begin
        if rstAsyncNegIn = '0' then
            pixelCounter := 0;
            currWidth    := 0;
            currHeight   := 0;
            currOptions  := paramsReg;

            frameDoneOut <= false;
            hBlank       <= true;
            vBlank       <= true;
        elsif rising_edge(clkIn) then
            -- debug signals
            currHeightDbg   <= currHeight;
            currWidthDbg    <= currWidth;
            pixelCounterDbg <= pixelCounter;
            currOptionsDbg  <= currOptions;

            -- strobe
            frameDoneOut <= false;

            -- reg signals
            hBlank <= currWidth >= currOptions.colSize;
            vBlank <= currHeight >= currOptions.rowSize;

            dataOut <= getPixelOut(currHeight, currWidth);

            if currWidth < currOptions.colSize and currHeight < currOptions.rowSize then
                pixelCounter := pixelCounter + 1;
            end if;

            if currWidth >= (currOptions.colSize + currOptions.hblank) - 1 then
                if currHeight >= (currOptions.rowSize + currOptions.vblank) - 1 then
                    AlertIfNot(CCD_ALERT_ID, (pixelCounter = (currOptions.rowSize * currOptions.colSize)) or pixelCounter = 0,
                               "Invalid number of pixels has been output" & LF & "Expected: " & to_string(currOptions.rowSize * currOptions.colSize) & "Got: " & to_string(pixelCounter));

                    currHeight   := 0;
                    currWidth    := 0;
                    pixelCounter := 0;

                    currOptions  := paramsReg;
                    frameDoneOut <= true;

                    assert get_ccd_pixel_color(currOptions.rowStart, currOptions.colStart, currOptions.rowMirror and currOptions.colMirror) = Green1
                    report "First pixel is not correctly aligned to whole Bayer pixel!"
                    severity failure;
                else
                    currHeight := currHeight + 1;
                    currWidth  := 0;
                end if;
            else
                currWidth := currWidth + 1;
            end if;
        end if;
    end process imageOutProc;

    serialConfig : block
        signal newDataArrived : boolean;
        signal dataIn         : I2c_Data_T;
        signal dataAddrIn     : I2c_Addr_T;
        signal rstAsyncIn     : std_logic;
    begin

        rstAsyncIn <= not rstAsyncNegIn;

        serialCommsProc : process(clkIn, rstAsyncNegIn)
            variable tmpData : natural;
        begin
            if rstAsyncNegIn = '0' then
                -- initialize defaults
                paramsReg <= (
                    rowStart  => INIT_HEIGHT_START,
                    colStart  => INIT_WIDTH_START,
                    rowSize   => INIT_HEIGHT,
                    colSize   => INIT_WIDTH,
                    hblank    => HBLANK_DEFAULT,
                    vblank    => VBLANK_DEFAULT,
                    rowMirror => false,
                    colMirror => false
                );
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
                            paramsReg.rowMirror <= ?? dataIn(15);
                            -- mirror column
                            paramsReg.colMirror <= ?? dataIn(14);
                            Log(CCD_ALERT_ID, "ReadMode2 register updated. Using only upper 2 bits.", DEBUG);

                        when others =>
                            report "Register: 0x" & to_hstring(dataAddrIn) & " currently not implemented"
                            severity warning;
                    end case;
                end if;
            end if;
        end process serialCommsProc;

        i2cSlave : entity work.i2c_slave_model
            generic map(
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
end architecture model;
