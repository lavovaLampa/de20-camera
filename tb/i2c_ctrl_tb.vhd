library ieee;
use ieee.std_logic_1164.all;
use work.i2c_pkg.all;
use work.i2c_ctrl;

entity i2c_ctrl_tb is
    constant I2C_PERIOD : time := 10 us;
    -- clock has to be 4 times i2c clock
    -- TODO: parametrize?
    constant CLK_PERIOD : time := 4 * I2C_PERIOD;

    constant TEST_DATA      : I2c_Data_T := X"5555";
    constant TEST_DEV_ADDR  : I2c_Addr_T := CCD_WRITE_ADDR;
    constant TEST_DATA_ADDR : I2c_Addr_T := X"55";
end i2c_ctrl_tb;

architecture tb of i2c_ctrl_tb is
    -- clock & reset
    signal clkIn, rstAsyncIn : std_logic := '0';

    -- i2c ctrl signals
    signal ctrlEnableStrobeIn                    : boolean    := false;
    signal ctrlDataIn                            : I2c_Data_T := X"0000";
    signal ctrlDevAddrIn                         : I2c_Addr_T := X"00";
    signal ctrlDataAddrIn                        : I2c_Addr_T := X"00";
    signal ctrlDoneStrobeOut, ctrlErrorStrobeOut : boolean;

    -- i2c i/o signals
    signal sClkOut : std_logic;
    signal sDataIO : std_logic := 'Z';

    -- i2c slave model signals
    signal slaveDataReceivedOut                              : boolean;
    signal slaveReceivedDevAddrOut, slaveReceivedDataAddrOut : I2c_Addr_T;
    signal slaveReceivedDataOut                              : I2c_Data_T;

    -- testbench signals
    signal tbClock    : std_logic := '0';
    signal tbSimEnded : std_logic := '0';
begin
    dut : entity work.i2c_ctrl
        port map(
            clkIn          => clkIn,
            rstAsyncIn     => rstAsyncIn,
            -- input
            enableInStrobe => ctrlEnableStrobeIn,
            dataIn         => ctrlDataIn,
            devAddrIn      => ctrlDevAddrIn,
            dataAddrIn     => ctrlDataAddrIn,
            -- output
            doneStrobeOut  => ctrlDoneStrobeOut,
            errorStrobeOut => ctrlErrorStrobeOut,
            -- i2c i/o
            sClkOut        => sClkOut,
            sDataIO        => sDataIO
        );

    i2cSlave : entity work.i2c_slave_model
        generic map(
            CHECK_DATA => true
        )
        port map(
            tbClkIn            => clkIn,
            rstAsyncIn         => rstAsyncIn,
            -- i2c i/o
            sClkIn             => sClkOut,
            sDataIO            => sDataIO,
            -- dbg output
            newDataReceivedOut => slaveDataReceivedOut,
            expectedDataIn     => TEST_DATA,
            expectedDataAddrIn => TEST_DATA_ADDR,
            expectedDevAddrIn  => TEST_DEV_ADDR,
            recvDevAddrOut     => slaveReceivedDevAddrOut,
            recvDataAddrOut    => slaveReceivedDataAddrOut,
            recvDataOut        => slaveReceivedDataOut
        );

    -- clock generation
    tbClock <= not tbClock after CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';
    clkIn   <= tbClock;

    stimuli : process
    begin
        ctrlEnableStrobeIn <= false;
        ctrlDataIn         <= X"0000";
        ctrlDevAddrIn      <= X"00";
        ctrlDataAddrIn     <= X"00";

        -- reset generation
        rstAsyncIn <= '1';
        wait for 2 * CLK_PERIOD;
        rstAsyncIn <= '0';
        wait for 2 * CLK_PERIOD;

        ctrlDevAddrIn      <= TEST_DEV_ADDR;
        ctrlDataAddrIn     <= TEST_DATA_ADDR;
        ctrlDataIn         <= TEST_DATA;
        ctrlEnableStrobeIn <= true;
        wait until rising_edge(clkIn);
        ctrlEnableStrobeIn <= false;

        wait until ctrlDoneStrobeOut = true;
        assert not ctrlErrorStrobeOut;
        -- wait for stop bit
        wait for 10 * CLK_PERIOD;
        -- Stop the clock and hence terminate the simulation
        tbSimEnded <= '1';
        wait;
    end process;

    checkProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
        elsif rising_edge(clkIn) then
            if slaveDataReceivedOut then
                report "Data received";
            end if;
        end if;
    end process checkProc;

    -- SDA cannot get to high state
    --    assert sDataIO = 'Z' or sDataIO = '0';
    assert sDataIo /= '1';

end tb;
