library ieee;
use ieee.std_logic_1164.all;
use work.i2c_pkg.all;
use work.i2c_ctrl;

entity i2c_ctrl_tb is
    constant I2C_PERIOD : time := 10 us;
    -- clock has to be 4 times i2c clock
    constant CLK_PERIOD : time := 4 * I2C_PERIOD;

    constant TEST_DATA      : I2c_Data_T := X"5555";
    constant TEST_DEV_ADDR  : I2c_Addr_T := CCD_WRITE_ADDR;
    constant TEST_DATA_ADDR : I2c_Addr_T := X"55";
end i2c_ctrl_tb;

architecture tb of i2c_ctrl_tb is

    signal clkIn, rstAsyncIn : std_logic  := '0';
    signal enableIn          : boolean    := false;
    signal dataIn            : I2c_Data_T := X"0000";
    signal devAddrIn         : I2c_Addr_T := X"00";
    signal dataAddrIn        : I2c_Addr_T := X"00";
    signal doneOut, errorOut : boolean;
    signal sClkOut           : std_logic;
    signal sDataIO           : std_logic  := 'Z';

    signal slaveDataReceived         : boolean;
    signal recvDevAddr, recvDataAddr : I2c_Addr_T;
    signal recvData                  : I2c_Data_T;

    signal tbClock    : std_logic := '0';
    signal tbSimEnded : std_logic := '0';

begin

    dut : entity work.i2c_ctrl
        port map(clkIn          => clkIn,
                 rstAsyncIn     => rstAsyncIn,
                 enableInStrobe => enableIn,
                 dataIn         => dataIn,
                 devAddrIn      => devAddrIn,
                 dataAddrIn     => dataAddrIn,
                 doneOut        => doneOut,
                 errorOut       => errorOut,
                 sClkOut        => sClkOut,
                 sDataIO        => sDataIO);

    i2cSlave : entity work.i2c_slave_model
        generic map(
            CHECK_DATA => true
        )
        port map(
            tbClkIn          => clkIn,
            sClkIn             => sClkOut,
            rstAsyncIn         => rstAsyncIn,
            dataReceivedOut    => slaveDataReceived,
            sDataIO            => sDataIO,
            expectedDataIn     => TEST_DATA,
            expectedDataAddrIn => TEST_DATA_ADDR,
            expectedDevAddrIn  => TEST_DEV_ADDR,
            recvDevAddrOut     => recvDevAddr,
            recvDataAddrOut    => recvDataAddr,
            recvDataOut        => recvData
        );

    -- Clock generation
    tbClock <= not tbClock after CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';
    clkIn   <= tbClock;

    stimuli : process
    begin
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
        dataAddrIn <= TEST_DATA_ADDR;
        dataIn     <= TEST_DATA;
        enableIn   <= true;
        wait until rising_edge(clkIn);
        enableIn   <= false;

        wait until doneOut = true;
        assert not errorOut;
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
            if slaveDataReceived then
                report "Data received";
            end if;
        end if;
    end process checkProc;

    -- SDA cannot get to high state
    assert sDataIO = 'Z' or sDataIO = '0';

end tb;
