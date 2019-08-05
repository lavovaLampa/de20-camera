library ieee;
use ieee.std_logic_1164.all;
use work.i2c_pkg.all;
use work.ccd_pkg.all;

entity i2c_ccd_config_tb is
    constant VERBOSE    : boolean := false;
    constant CLK_PERIOD : time    := 20 ns; -- 50MHz clock
end i2c_ccd_config_tb;

architecture tb of i2c_ccd_config_tb is
    signal clkIn, rstAsyncIn : std_logic;
    signal enableIn          : boolean;
    signal doneOut           : boolean;

    signal expectedData                      : I2C_Data;
    signal expectedDataAddr, expectedDevAddr : I2C_Addr;

    signal slaveDataReceived         : boolean;
    signal dataOutCount              : natural := 0;
    signal recvData                  : I2C_Data;
    signal recvDevAddr, recvDataAddr : I2C_Addr;

    -- i2c signals
    signal sClkOut : std_logic;
    signal sDataIO : std_logic;

    -- TB signals
    signal tbClk      : std_logic := '0';
    signal tbSimEnded : std_logic := '0';
begin

    dut : entity work.i2c_ccd_config
        port map(
            clkIn      => clkIn,
            rstAsyncIn => rstAsyncIn,
            enableIn   => enableIn,
            doneOut    => doneOut,
            sClkOut    => sClkOut,
            sDataIO    => sDataIO
        );

    i2cSlave : entity work.i2c_slave_model
        generic map(
            DEBUG      => false,
            CHECK_DATA => true
        )
        port map(
            testClkIn          => clkIn,
            sClkIn             => sClkOut,
            rstAsyncIn         => rstAsyncIn,
            dataReceivedOut    => slaveDataReceived,
            sDataIO            => sDataIO,
            expectedDataIn     => expectedData,
            expectedDataAddrIn => expectedDataAddr,
            expectedDevAddrIn  => expectedDevAddr,
            recvDevAddrOut     => recvDevAddr,
            recvDataAddrOut    => recvDataAddr,
            recvDataOut        => recvData
        );

    -- Clock generation
    tbClk <= not tbClk after CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';
    clkIn <= tbClk;

    expectedDevAddr <= CCD_WRITE_ADDR;

    stimuli : process
    begin
        enableIn <= false;

        -- Reset generation
        rstAsyncIn <= '1';
        wait for 5 * CLK_PERIOD;
        rstAsyncIn <= '0';
        wait for 5 * CLK_PERIOD;

        enableIn <= true;

        wait until doneOut;
        wait for 5 * CLK_PERIOD;
        assert dataOutCount = CCD_CONFIG'length
        report "Didn't send all configuration data or sent more" & LF &
        "Expected: " & natural'image(CCD_CONFIG'length) & LF &
        "Sent: " & natural'image(dataOutCount);

        -- Stop the clock and hence terminate the simulation
        tbSimEnded <= '1';
        wait;
    end process;

    checkProc : process(clkIn, rstAsyncIn)
        variable configPtr : natural := 0;
    begin
        if rstAsyncIn = '1' then
            configPtr := 0;
        elsif rising_edge(clkIn) then
            expectedData     <= CCD_CONFIG(configPtr).data;
            expectedDataAddr <= CCD_CONFIG(configPtr).addr;
            if slaveDataReceived then
                if VERBOSE then
                    report "Config array pointer: " & natural'image(configPtr);
                    report "Received device address: 0x" & to_hstring(recvDevAddr);
                    report "Received data address: 0x" & to_hstring(recvDataAddr);
                    report "Received data: 0x" & to_hstring(recvData);
                end if;

                dataOutCount <= dataOutCount + 1;
                if configPtr < CCD_CONFIG'length - 1 then
                    configPtr := configPtr + 1;
                end if;
            end if;
        end if;
    end process checkProc;

end tb;
