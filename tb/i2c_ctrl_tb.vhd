library ieee;
use ieee.std_logic_1164.all;
use work.i2c_pkg.all;
use work.i2c_ctrl;

entity tb_i2c_ctrl is
    constant I2C_PERIOD : time := 10 us;
    -- clock has to be 2 times i2c clock
    constant CLK_PERIOD : time := 2 * I2C_PERIOD;

    constant TEST_DATA     : I2c_Data := X"00FF";
    constant TEST_DEV_ADDR : I2c_Addr := CCD_WRITE_ADDR;
    constant TEST_REG_ADDR : I2c_Addr := X"03";
end tb_i2c_ctrl;

architecture tb of tb_i2c_ctrl is

    signal clkIn      : std_logic := '0';
    signal rstAsyncIn : std_logic := '0';
    signal enableIn   : boolean   := false;
    signal dataIn     : i2c_data  := X"0000";
    signal devAddrIn  : i2c_addr  := X"00";
    signal dataAddrIn : i2c_addr  := X"00";
    signal doneOut    : boolean;
    signal sClkOut    : std_logic;
    signal sDataIO    : std_logic := 'Z';

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
        -- Stop the clock and hence terminate the simulation
        tbSimEnded <= '1';
        wait;
    end process;

    mockSlave : process(sClkOut, sDataIO, rstAsyncIn)
        type Slave_State is (AwaitStart, Receive, Acknowledge, AwaitStop);

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
            case state is
                when Receive =>
                    currBit := i2cBusStateToLogic(sDataIO);
                    report "Received bit (index): " & natural'image(bitPointer) & " -> Value: " & std_logic'image(currBit);
                    -- release line
                    dataOut := '1';

                    if bitPointer mod 8 = 0 then
                        state := Acknowledge;
                    else
                        bitPointer := bitPointer - 1;
                    end if;

                when Acknowledge =>
                    report "byte acknowledged" severity note;
                    state   := Receive;
                    dataOut := '0';
                    if bitPointer > 0 then
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

            assert tmpDevAddr = TEST_DEV_ADDR report "Device address" & LF & "Expected: " & LF & "Received: " severity failure;
            assert tmpRegAddr = TEST_REG_ADDR report "received register address is not equal" severity failure;
            assert tmpData = TEST_DATA report "received data is not equal" severity failure;
        end if;

        sDataIO <= logicToI2CBusState(dataOut);

    end process mockSlave;

    -- SDA cannot get to high state
    assert sDataIO = 'Z' or sDataIO = '0' severity failure;

end tb;
