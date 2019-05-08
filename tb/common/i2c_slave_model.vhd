library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.i2c_pkg.all;

entity i2c_slave_model is
    generic(
        DEBUG : boolean := false
    );
    port(
        testClkIn                             : in    std_logic;
        sClkIn, rstAsyncIn                    : in    std_logic;
        dataReceivedOut                       : out   boolean := false;
        sDataIO                               : inout std_logic;
        expectedDevAddrIn, expectedDataAddrIn : in    I2C_Addr;
        expectedDataIn                        : in    I2C_Data
    );
    type Slave_State is (Receive, Acknowledge, IgnoreStop);
end entity i2c_slave_model;

architecture RTL of i2c_slave_model is
    signal testDataAggregate          : I2C_Aggregate;
    signal startReceived, endReceived : boolean := false;
    signal doAck, dataDone            : boolean := false;
    signal batchOK                    : boolean := false;

    signal dataInAcc : I2C_Aggregate := X"00000000";

    procedure debugPrint(val : string) is
    begin
        if DEBUG then
            report val;
        end if;
    end procedure debugPrint;

begin

    testDataAggregate <= expectedDevAddrIn & expectedDataAddrIn & expectedDataIn;

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
            lastVal         := batchOk;
        end if;
    end process reportProc;

    readProc : process(sClkIn, rstAsyncIn)
        -- registers
        variable currState : Slave_State          := Receive;
        variable bitPtr    : Data_Aggregate_Range := AGGREGATE_WIDTH - 1;

        variable currBit : std_logic := '0';
    begin
        if rstAsyncIn = '1' then
            bitPtr    := AGGREGATE_WIDTH - 1;
            currState := Receive;
            doAck     <= false;
        elsif rising_edge(sClkIn) then
            currBit := i2cBusToLogic(sDataIO);

            case currState is
                when Receive =>
                    dataDone <= false;
                    if startReceived then
                        debugPrint("Received bit (index): " & natural'image(bitPtr) & " -> Value: " & std_logic'image(currBit));
                        assert currBit = testDataAggregate(bitPtr)
                        report "Received wrong bit value at index: " & natural'image(bitPtr) & LF &
                               "Expected: " & std_logic'image(testDataAggregate(bitPtr)) & LF &
                               "Received: " & std_logic'image(currBit) severity warning;

                        dataInAcc(bitPtr) <= currBit;
                        if bitPtr mod 8 = 0 then
                            currState := Acknowledge;
                            doAck     <= true;
                        end if;
                        if bitPtr > 0 then
                            bitPtr := bitPtr - 1;
                        end if;
                    else
                        report "Clock running without receiving start bit!" severity warning;
                    end if;

                when Acknowledge =>
                    debugPrint("-------------BYTE SEPARATOR-------------");
                    assert currBit = '0' report "byte wasn't acknowledged or data bus wasn't free" severity failure;
                    doAck <= false;
                    if bitPtr = 0 then
                        dataDone  <= true;
                        bitPtr    := AGGREGATE_WIDTH - 1;
                        currState := IgnoreStop;
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

        sDataIO <= logicToI2CBus(dataOut);
    end process writeProc;

    -- latches & potentially non-synthesizable constructs ahead, BEWARE!
    ctrlProc : process(sDataIO, rstAsyncIn)
        variable tmpDevAddr, tmpRegAddr : I2C_Addr := X"00";
        variable tmpData                : I2C_Data := X"0000";
    begin
        if rstAsyncIn = '1' then
            endReceived   <= false;
            startReceived <= false;
            batchOK       <= false;
        elsif sDataIO'event then
            -- if START bit received
            -- HIGH to LOW transition on DATA line while CLK is HIGH
            if sDataIO = '0' and sClkIn = '1' and sClkIn'stable then
                debugPrint("received start bit");
                endReceived   <= false;
                startReceived <= true;
                batchOk       <= false;

            -- if STOP bit received
            -- LOW to HIGH transition on DATA line while CLK is HIGH
            elsif sDataIO = 'Z' and sClkIn = '1' and sClkIn'stable then
                debugPrint("received stop bit");
                startReceived <= false;
                endReceived   <= true;

                assert dataDone report "Slave didn't receive all data" severity failure;
                tmpDevAddr := dataInAcc(31 downto 24);
                tmpRegAddr := dataInAcc(23 downto 16);
                tmpData    := dataInAcc(DATA_WIDTH - 1 downto 0);
                report "Data received OK: 0x" & to_hstring(dataInAcc);

                assert tmpDevAddr = expectedDevAddrIn
                report "Wrong device address received" & LF &
                       "Expected: 0x" & to_hstring(expectedDevAddrIn) & LF &
                       "Received: 0x" & to_hstring(tmpDevAddr) severity failure;

                assert tmpRegAddr = expectedDataAddrIn
                report "received register address is not equal" & LF &
                       "Expected: 0x" & to_hstring(expectedDataAddrIn) & LF &
                       "Received: 0x" & to_hstring(tmpRegAddr) severity failure;

                assert tmpData = expectedDataIn
                report "Received wrong data (not equal to what was sent)" & LF &
                       "Expected: 0x" & to_hstring(expectedDataIn) & LF &
                       "Received: 0x" & to_hstring(tmpData) severity failure;

                batchOk <= true;
            end if;
        end if;
    end process ctrlProc;
end architecture RTL;
