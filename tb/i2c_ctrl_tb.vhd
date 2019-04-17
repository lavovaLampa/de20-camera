library ieee;
use ieee.std_logic_1164.all;
use work.i2c_pkg.all;
use work.i2c_ctrl;

entity tb_i2c_ctrl is
    constant I2C_PERIOD : time := 10 us;
    -- clock has to be 2 times i2c clock
    constant CLK_PERIOD : time := 2 * I2C_PERIOD;
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

        devAddrIn  <= CCD_WRITE_ADDR;
        dataAddrIn <= X"03";
        dataIn     <= X"00FF";
        enableIn   <= true;
        wait until rising_edge(clkIn);

        wait until doneOut = true;
        -- Stop the clock and hence terminate the simulation
        tbSimEnded <= '1';
        wait;
    end process;

    mockSlave : process(sClkOut, sDataIO, rstAsyncIn)
        type Slave_State is (Idle, Receive);

        variable dataInAcc  : I2c_Aggregate        := X"00000000";
        variable state      : Slave_State          := Idle;
        variable bitPointer : Data_Aggregate_Range := AGGREGATE_WIDTH - 1;
        variable dataOut    : std_logic            := '1';
        variable currBit    : std_logic            := '0';
    begin
        if rstAsyncIn = '1' then
            state      := Idle;
            bitPointer := AGGREGATE_WIDTH - 1;
            dataOut    := '1';
            dataInAcc  := X"00000000";
        else
            case state is
                when Idle =>
                    -- if START bit received
                    -- HIGH to LOW transition on DATA line while CLK is HIGH
                    if sClkOut = '1' and (not sClkOut'event) and sDataIO'event and sDataIO'last_value = 'Z' and sDataIO = '0' then
                        state := Receive;
                    end if;

                when Receive =>
                    if rising_edge(sClkOut) then
                        currBit    := i2cBusStateToLogic(sDataIO);
                        report "Received bit (num): " & natural'image(bitPointer) & "\nValue: " & std_logic'image(currBit);
                        -- FIXME: will underflow
                        bitPointer := bitPointer - 1;
                        -- release line
                        dataOut    := '1';
                    end if;

                    if falling_edge(sClkOut) and bitPointer mod 8 = 0 then
                        -- acknowledge byte received
                        dataOut := '0';
                    end if;

                    -- if STOP bit received
                    -- LOW to HIGH transition on DATA line while CLK is HIGH
                    if sClkOut = '1' and (not sClkOut'event) and sDataIO'event and sDataIO'last_value = '0' and sDataIO = 'Z' then
                        state := Idle;
                        assert bitPointer = 0 severity warning;
                    end if;
            end case;
        end if;

        sDataIO <= logicToI2CBusState(dataOut);

    end process mockSlave;

    -- SDA cannot get to high state
    assert sDataIO = 'Z' or sDataIO = '0' severity failure;

end tb;
