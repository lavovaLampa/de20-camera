library ieee;
use ieee.std_logic_1164.all;

use work.i2c_pkg.all;
use work.ccd_config_pkg.all;

library osvvm;
context osvvm.OsvvmContext;

entity i2c_ccd_config_tb is
    constant CLK_PERIOD : time := 20 ns; -- 50MHz clock

    constant I2C_CONFIG_ALERT_ID : AlertLogIDType := GetAlertLogID("i2c config ctrl testbench", ALERTLOG_BASE_ID);
end i2c_ccd_config_tb;

architecture tb of i2c_ccd_config_tb is
    -- clk & reset
    signal clkIn, rstAsyncIn : std_logic := '0';

    -- i2c config ctrl signals
    signal configEnableIn      : boolean := false;
    signal configDoneStrobeOut : boolean;

    -- i2c model debug signals
    signal modelNewDataReceivedDbg                           : boolean;
    signal modelExpectedDataIn                               : I2c_Data_T;
    signal modelExpectedDataAddrIn, modelExpectedDevAddrIn   : I2c_Addr_T;
    signal modelReceivedDataAddrOut, modelReceivedDevAddrOut : I2c_Addr_T;
    signal modelReceivedDataOut                              : I2c_Data_T;

    -- i2c i/o
    signal sClkOut : std_logic;
    signal sDataIO : std_logic;

    -- testbench signals
    signal tbClk      : std_logic := '0';
    signal tbSimEnded : std_logic := '0';
begin

    dut : entity work.i2c_ccd_config
        port map(
            clkIn               => clkIn,
            rstAsyncIn          => rstAsyncIn,
            -- state out
            enableStrobeIn      => configEnableIn,
            configDoneStrobeOut => configDoneStrobeOut,
            -- i2c i/o
            sClkOut             => sClkOut,
            sDataIO             => sDataIO
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
            -- debug signals
            newDataReceivedOut => modelNewDataReceivedDbg,
            expectedDataIn     => modelExpectedDataIn,
            expectedDataAddrIn => modelExpectedDataAddrIn,
            expectedDevAddrIn  => modelExpectedDevAddrIn,
            recvDevAddrOut     => modelReceivedDevAddrOut,
            recvDataAddrOut    => modelReceivedDataAddrOut,
            recvDataOut        => modelReceivedDataOut
        );

    -- Clock generation
    tbClk <= not tbClk after CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';
    clkIn <= tbClk;

    modelExpectedDevAddrIn <= CCD_WRITE_ADDR;

    stimuli : process
    begin
        configEnableIn <= false;

        -- Reset generation
        rstAsyncIn <= '1';
        wait for 5 * CLK_PERIOD;
        rstAsyncIn <= '0';
        wait for 5 * CLK_PERIOD;

        configEnableIn <= true;
        wait until rising_edge(clkIn);
        configEnableIn <= false;

        wait until configDoneStrobeOut;
        wait for 5 * CLK_PERIOD;

        -- Stop the clock and hence terminate the simulation
        tbSimEnded <= '1';
        wait;
    end process;

    checkProc : process(clkIn, rstAsyncIn)
        -- state regs
        variable dataCounter : natural := 0;
    begin
        if rstAsyncIn = '1' then
            dataCounter := 0;
        elsif rising_edge(clkIn) then
            if dataCounter < CCD_CONFIG_ARRAY'length then
                modelExpectedDataIn     <= CCD_CONFIG_ARRAY(dataCounter).data;
                modelExpectedDataAddrIn <= CCD_CONFIG_ARRAY(dataCounter).addr;
            end if;

            if modelNewDataReceivedDbg then
                Log(I2C_CONFIG_ALERT_ID,
                    "Config array pointer: " & to_string(dataCounter) & LF & "Received device address: 0x" & to_hstring(std_logic_vector(modelReceivedDevAddrOut)) & LF & "Received data address: 0x" & to_hstring(std_logic_vector(modelReceivedDataAddrOut)) & LF & "Received data: 0x" & to_hstring(modelReceivedDataOut), DEBUG);

                dataCounter := dataCounter + 1;
            elsif configDoneStrobeOut then
                AlertIfNot(I2C_CONFIG_ALERT_ID, dataCounter = CCD_CONFIG_ARRAY'length,
                           "Incorrect number of configuration entries received" & LF & "Expected: " & to_string(CCD_CONFIG_ARRAY'length) & LF & "Received: " & to_string(dataCounter));
            end if;
        end if;
    end process checkProc;

end tb;
