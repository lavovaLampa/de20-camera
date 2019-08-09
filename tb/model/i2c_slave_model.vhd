library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.i2c_pkg.all;

library osvvm;
context osvvm.OsvvmContext;

entity i2c_slave_model is
    generic(
        CHECK_DATA : boolean := false
    );
    port(
        testClkIn                             : in    std_logic;
        rstAsyncIn                            : in    std_logic;
        -- i2c i/o
        sClkIn                                : in    std_logic;
        dataReceivedOut                       : out   boolean   := false;
        sDataIO                               : inout std_logic := 'Z';
        -- test signals
        recvDevAddrOut, recvDataAddrOut       : out   I2c_Addr_T;
        recvDataOut                           : out   I2c_Data_T;
        -- test checking signals
        expectedDevAddrIn, expectedDataAddrIn : in    I2c_Addr_T;
        expectedDataIn                        : in    I2c_Data_T
    );
    constant I2C_SLAVE_ALERT_ID : AlertLogIDType := GetAlertLogID("i2c slave", ALERTLOG_BASE_ID);
end entity i2c_slave_model;

architecture model of i2c_slave_model is
    signal testDataAggregate          : I2c_Aggregate_T;
    signal startReceived, endReceived : boolean := false;
    signal doAck, dataDone            : boolean := false;
    signal batchOK                    : boolean := false;

    signal dataInAcc : I2c_Aggregate_T := X"00000000";
begin

    testDataAggregate <= std_logic_vector(expectedDevAddrIn) & std_logic_vector(expectedDataAddrIn) & expectedDataIn;

    -- synchronized to TB clock
    reportProc : process(testClkIn, rstAsyncIn)
        variable lastVal : boolean := false;
    begin
        if rstAsyncIn = '1' then
            dataReceivedOut <= false;
            lastVal         := false;
        elsif rising_edge(testClkIn) then
            -- value changed
            dataReceivedOut <= batchOK and not lastVal;
            lastVal         := batchOK;
        end if;
    end process reportProc;

    readProc : process(sClkIn, rstAsyncIn)
        type Model_State_T is (Receive, Acknowledge, IgnoreStop);

        -- registers
        variable currState  : Model_State_T       := Receive;
        variable currBitPtr : I2c_Aggregate_Ptr_T := AGGREGATE_WIDTH - 1;

        -- helper wire variable
        variable currBit : std_logic := '0';
    begin
        if rstAsyncIn = '1' then
            currBitPtr := AGGREGATE_WIDTH - 1;
            currState  := Receive;

            doAck <= false;
        elsif rising_edge(sClkIn) then
            currBit := i2c_to_logic(sDataIO);

            case currState is
                when Receive =>
                    dataDone <= false;
                    if startReceived then
                        Log(I2C_SLAVE_ALERT_ID, "Received bit (index): " & to_string(currBitPtr) & " -> Value: " & to_string(currBit), DEBUG);
                        if CHECK_DATA then
                            AlertIfNot(I2C_SLAVE_ALERT_ID, currBit = testDataAggregate(currBitPtr), "Received wrong bit value at index: " & natural'image(currBitPtr) & LF & "Expected: " & std_logic'image(testDataAggregate(currBitPtr)) & LF & "Received: " & std_logic'image(currBit));
                        end if;

                        dataInAcc(currBitPtr) <= currBit;
                        if currBitPtr mod 8 = 0 then
                            currState := Acknowledge;
                            doAck     <= true;
                        end if;
                        if currBitPtr > 0 then
                            currBitPtr := currBitPtr - 1;
                        end if;
                    else
                        report "Clock running without receiving start bit!" severity warning;
                    end if;

                when Acknowledge =>
                    Log(I2C_SLAVE_ALERT_ID, "-------------BYTE SEPARATOR-------------", DEBUG);

                    assert currBit = '0' report "byte wasn't acknowledged or data bus wasn't free" severity failure;
                    doAck <= false;

                    if currBitPtr = 0 then
                        dataDone   <= true;
                        currBitPtr := AGGREGATE_WIDTH - 1;
                        currState  := IgnoreStop;
                    else
                        currState := Receive;
                    end if;

                -- TODO: handle this better
                when IgnoreStop =>
                    currState := Receive;

            end case;
        end if;
    end process readProc;

    writeProc : process(sClkIn, rstAsyncIn)
        variable dataOut : std_logic := '1';
    begin
        if rstAsyncIn = '1' then
            dataOut := '1';
        elsif falling_edge(sClkIn) then
            if doAck then
                dataOut := '0';
            else
                dataOut := '1';
            end if;
        end if;

        sDataIO <= logic_to_i2c(dataOut);
    end process writeProc;

    -- latches & potentially non-synthesizable constructs ahead, BEWARE!
    ctrlProc : process(sDataIO, rstAsyncIn)
        variable tmpDevAddr, tmpDataAddr : I2c_Addr_T := X"00";
        variable tmpData                 : I2c_Data_T := X"0000";
    begin
        if rstAsyncIn = '1' then
            endReceived   <= false;
            startReceived <= false;
            batchOK       <= false;
        elsif sDataIO'event then
            -- if START bit received
            -- HIGH to LOW transition on DATA line while CLK is HIGH
            if sDataIO = '0' and sClkIn = '1' and sClkIn'stable then
                Log(I2C_SLAVE_ALERT_ID, "Received start flag", DEBUG);

                endReceived   <= false;
                startReceived <= true;
                batchOK       <= false;

            -- if STOP bit received
            -- LOW to HIGH transition on DATA line while CLK is HIGH
            elsif sDataIO = 'Z' and sClkIn = '1' and sClkIn'stable then
                Log(I2C_SLAVE_ALERT_ID, "Received stop flag", DEBUG);

                startReceived <= false;
                endReceived   <= true;

                assert dataDone report "Slave didn't receive all data" severity failure;
                tmpDevAddr  := unsigned(dataInAcc(31 downto 24));
                tmpDataAddr := unsigned(dataInAcc(23 downto 16));
                tmpData     := dataInAcc(DATA_WIDTH - 1 downto 0);
                report "Data received OK: 0x" & to_hstring(dataInAcc);

                if CHECK_DATA then
                    assert tmpDevAddr = expectedDevAddrIn
                    report "Wrong device address received" & LF &
                       "Expected: 0x" & to_hstring(expectedDevAddrIn) & LF &
                       "Received: 0x" & to_hstring(tmpDevAddr) severity failure;

                    assert tmpDataAddr = expectedDataAddrIn
                    report "received register address is not equal" & LF &
                       "Expected: 0x" & to_hstring(expectedDataAddrIn) & LF &
                       "Received: 0x" & to_hstring(tmpDataAddr) severity failure;

                    assert tmpData = expectedDataIn
                    report "Received wrong data (not equal to what was sent)" & LF &
                       "Expected: 0x" & to_hstring(expectedDataIn) & LF &
                       "Received: 0x" & to_hstring(tmpData) severity failure;
                end if;

                batchOK <= true;
            end if;
        end if;

        recvDevAddrOut  <= tmpDevAddr;
        recvDataAddrOut <= tmpDataAddr;
        recvDataOut     <= tmpData;
    end process ctrlProc;
end architecture model;
