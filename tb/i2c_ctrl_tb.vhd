library ieee;
use ieee.std_logic_1164.all;
use work.i2c_pkg.all;
use work.i2c_ctrl;

entity i2c_ctrl_tb is
    constant I2C_PERIOD : time := 10 us;
    -- clock has to be 2 times i2c clock
    constant CLK_PERIOD : time := 2 * I2C_PERIOD;

    constant TEST_DATA           : I2c_Data                      := X"5555";
    constant TEST_DEV_ADDR       : I2c_Addr                      := CCD_WRITE_ADDR;
    constant TEST_REG_ADDR       : I2c_Addr                      := X"55";
    constant TEST_DATA_AGGREGATE : std_logic_vector(31 downto 0) := TEST_DEV_ADDR & TEST_REG_ADDR & TEST_DATA;
end i2c_ctrl_tb;

architecture tb of i2c_ctrl_tb is

    signal clkIn             : std_logic := '0';
    signal rstAsyncIn        : std_logic := '0';
    signal enableIn          : boolean   := false;
    signal dataIn            : i2c_data  := X"0000";
    signal devAddrIn         : i2c_addr  := X"00";
    signal dataAddrIn        : i2c_addr  := X"00";
    signal doneOut, errorOut : boolean;
    signal sClkOut           : std_logic;
    signal sDataIO           : std_logic := 'Z';

    signal tbClock    : std_logic := '0';
    signal tbSimEnded : std_logic := '0';

begin

    dut : entity i2c_ctrl
        port map(clkIn      => clkIn,
                 rstAsyncIn => rstAsyncIn,
                 enableIn   => enableIn,
                 dataIn     => dataIn,
                 devAddrIn  => devAddrIn,
                 dataAddrIn => dataAddrIn,
                 doneOut    => doneOut,
                 errorOut   => errorOut,
                 sClkOut    => sClkOut,
                 sDataIO    => sDataIO);

    -- Clock generation
    tbClock <= not tbClock after CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';

    -- EDIT: Check that clkIn is really your main clock signal
    clkIn <= tbClock;

    stimuli : process
    begin
        -- EDIT Adapt initialization as needed
        enableIn   <= false;
        dataIn     <= X"0000";
        devAddrIn  <= X"00";
        dataAddrIn <= X"00";

        -- Reset generation
        -- EDIT: Check that rstAsyncIn is really your reset signal
        rstAsyncIn <= '1';
        wait for 2 * CLK_PERIOD;
        rstAsyncIn <= '0';
        wait for 2 * CLK_PERIOD;

        devAddrIn  <= TEST_DEV_ADDR;
        dataAddrIn <= TEST_REG_ADDR;
        dataIn     <= TEST_DATA;
        enableIn   <= true;
        wait until rising_edge(clkIn);

        wait until doneOut = true;
        assert not errorOut;
        -- wait for stop bit
        wait for 10 * CLK_PERIOD;
        -- Stop the clock and hence terminate the simulation
        tbSimEnded <= '1';
        wait;
    end process;

    mockSlave : process(sClkOut, sDataIO, rstAsyncIn)
        type Slave_State is (AwaitStart, Receive, Acknowledge, SkipAck, AwaitStop);

        variable dataInAcc              : I2c_Aggregate        := X"00000000";
        variable state                  : Slave_State          := AwaitStart;
        variable bitPointer             : Data_Aggregate_Range := AGGREGATE_WIDTH - 1;
        variable dataOut                : std_logic            := '1';
        variable currBit                : std_logic            := '0';
        variable tmpDevAddr, tmpRegAddr : I2c_Addr             := X"00";
        variable tmpData                : I2c_Data             := X"0000";
    begin
        if rstAsyncIn = '1' then
            state      := AwaitStart;
            bitPointer := AGGREGATE_WIDTH - 1;
            dataOut    := '1';
            dataInAcc  := X"00000000";
        elsif rising_edge(sClkOut) then
            currBit := i2cBusStateToLogic(sDataIO);
            case state is
                when Receive =>
                    report "Received bit (index): " & natural'image(bitPointer) & " -> Value: " & std_logic'image(currBit);
                    assert currBit = TEST_DATA_AGGREGATE(bitPointer) report "Received wrong bit value at index: " & natural'image(bitPointer) & LF &
                    "Expected: " & std_logic'image(TEST_DATA_AGGREGATE(bitPointer)) & LF &
                    "Received: " & std_logic'image(currBit) severity warning;

                    dataInAcc(bitPointer) := currBit;
                    if bitPointer mod 8 = 0 then
                        state := Acknowledge;
                    else
                        bitPointer := bitPointer - 1;
                    end if;

                when SkipAck =>
                    assert currBit = '0' report "byte wasn't acknowledged or data bus wasn't free" severity note;

                when others =>
                    null;
            end case;

        elsif falling_edge(sClkOut) then
            case state is
                when Acknowledge =>
                    dataOut := '0';
                    state   := SkipAck;

                when SkipAck =>
                    -- release line
                    dataOut := '1';
                    if bitPointer > 0 then
                        state      := Receive;
                        bitPointer := bitPointer - 1;
                    else
                        state := AwaitStop;
                    end if;

                when others =>
                    null;
            end case;

        -- if START bit received
        -- HIGH to LOW transition on DATA line while CLK is HIGH
        elsif sClkOut = '1' and (not sClkOut'event) and sDataIO'event and sDataIO'last_value = 'Z' and sDataIO = '0' and state = AwaitStart then
            report "received start bit" severity note;
            state := Receive;

        -- if STOP bit received
        -- LOW to HIGH transition on DATA line while CLK is HIGH
        elsif sClkOut = '1' and (not sClkOut'event) and sDataIO'event and sDataIO'last_value = 'Z' and sDataIO = '0' and state = AwaitStop and bitPointer = 0 then
            report "received stop bit" severity note;
            state      := AwaitStart;
            tmpDevAddr := dataInAcc(31 downto 24);
            tmpRegAddr := dataInAcc(23 downto 16);
            tmpData    := dataInAcc(DATA_WIDTH - 1 downto 0);
            report "Data received OK: 0x" & to_hstring(dataInAcc);

            assert tmpDevAddr = TEST_DEV_ADDR report "Wrong device address received" & LF &
            "Expected: 0x" & to_hstring(TEST_DEV_ADDR) & LF &
            "Received: 0x" & to_hstring(tmpDevAddr) severity failure;
            assert tmpRegAddr = TEST_REG_ADDR report "received register address is not equal" & LF &
            "Expected: 0x" & to_hstring(TEST_REG_ADDR) & LF &
            "Received: 0x" & to_hstring(tmpRegAddr) severity failure;
            assert tmpData = TEST_DATA report "Received wrong data (not equal to what was sent)" & LF &
            "Expected: 0x" & to_hstring(TEST_DATA) & LF &
            "Received: 0x" & to_hstring(tmpData) severity failure;
        end if;

        sDataIO <= logicToI2CBusState(dataOut);

    end process mockSlave;

    -- SDA cannot get to high state
    assert sDataIO = 'Z' or sDataIO = '0' severity failure;

end tb;
