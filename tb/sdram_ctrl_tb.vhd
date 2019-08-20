library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.sdram_pkg.all;
use work.sdram_ctrl_pkg.all;
use work.sdram_model_pkg.all;
use work.sdram_ctrl_tb_pkg.all;

library osvvm;
context osvvm.OsvvmContext;

entity sdram_ctrl_tb is
    constant CLK_PERIOD : time := work.sdram_pkg.CLK_PERIOD; -- 100 MHz

    -- number of required memory pages (1 page = 256 * 16b = 4096b)
    constant PAGES_REQUIRED : natural := 1400;

    -- estimated read burst length
    constant READ_BURST_LEN  : natural := 5;
    -- estimated write burst length
    constant WRITE_BURST_LEN : natural := 4;

    constant SDRAM_CTRL_TB_ALERT_ID : AlertLogIDType := GetAlertLogID("SDRAM ctrl testbench", ALERTLOG_BASE_ID);
end sdram_ctrl_tb;

architecture tb of sdram_ctrl_tb is
    -- TODO: how to generate clock? Should I use some phase shift?
    -- common signals
    signal testClk, rstAsync : std_logic;
    signal clkStable         : std_logic;

    -- mem ctrl signals
    signal ctrlCmd                                   : Ctrl_Cmd_T;
    signal ctrlAddr                                  : Ctrl_Addr_T;
    signal ctrlDataIn, ctrlDataOut                   : Data_T;
    signal ctrlNewData, ctrlProvideNewData, cmdReady : boolean;

    -- sdram i/o
    signal sdramDataIo   : Data_T;
    signal sdramIo       : Mem_IO_R;
    -- sdram testbench debug signals
    signal isInitialized : boolean;
    signal simEnded      : boolean;

    -- testbench signals
    signal tbClk      : std_logic := '0';
    signal tbSimEnded : std_logic := '0';
    signal stimuliEnd : boolean   := false;

begin
    dut : entity work.sdram_ctrl_top
        generic map(
            PAGES_REQUIRED  => 1400,
            READ_BURST_LEN  => READ_BURST_LEN,
            WRITE_BURST_LEN => WRITE_BURST_LEN
        )
        port map(
            clkIn             => testClk,
            rstAsyncIn        => rstAsync,
            clkStableIn       => clkStable,
            -- input
            addrIn            => ctrlAddr,
            cmdIn             => ctrlCmd,
            dataIn            => ctrlDataIn,
            -- output
            cmdReadyOut       => cmdReady,
            provideNewDataOut => ctrlProvideNewData,
            newDataOut        => ctrlNewData,
            dataOut           => ctrlDataOut,
            -- sdram i/o
            sdramDataIo       => sdramDataIo,
            sdramOut          => sdramIo
        );

    sdramModel : entity work.sdram_model
        generic map(
            LOAD_FROM_FILE => false,
            DUMP_TO_FILE   => false
        )
        port map(
            clkIn              => testClk,
            addrIn             => sdramIo.addr,
            dataIo             => sdramDataIo,
            bankSelectIn       => sdramIo.bankSelect,
            clkEnableIn        => sdramIo.clkEnable,
            chipSelectNegIn    => sdramIo.cmdAggregate.chipSelectNeg,
            rowAddrStrobeNegIn => sdramIo.cmdAggregate.rowAddrStrobeNeg,
            colAddrStrobeNegIn => sdramIo.cmdAggregate.colAddrStrobeNeg,
            writeEnableNegIn   => sdramIo.cmdAggregate.writeEnableNeg,
            dqmIn              => sdramIo.dqm,
            -- debug signals
            isInitializedOut   => isInitialized,
            simEndedIn         => simEnded
        );

    -- Clock generation
    tbClk   <= not tbClk after CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';
    testClk <= tbClk;

    stimuli : process
    begin
        clkStable <= '0';

        rstAsync <= '1';
        wait for 10 * CLK_PERIOD;
        rstAsync <= '0';
        wait for 10 * CLK_PERIOD;

        wait for 15 * CLK_PERIOD;
        clkStable <= '1';

        wait until cmdReady;
        AlertIf(not isInitialized, "Controller is ready for command even though the SDRAM is not initialized properly", ERROR);

        --        SetLogEnable(DEBUG, true);
        --        SetLogEnable(INFO, true);
        SetLogEnable(SDRAM_CTRL_TB_ALERT_ID, DEBUG, false);
        SetLogEnable(SDRAM_CTRL_TB_ALERT_ID, INFO, true);

        wait until stimuliEnd;

        Log(SDRAM_CTRL_TB_ALERT_ID, "Stimuli end");
        wait for 100 * CLK_PERIOD;
        simEnded <= true;

        -- Stop the clock and hence terminate the simulation
        tbSimEnded <= '1';
        Log(SDRAM_CTRL_TB_ALERT_ID, "Sim ended");
        wait;
    end process;

    testBlock : block
        signal newCmdToggle : std_logic;
        signal currCmd      : Ctrl_Cmd_T;
        signal currAddr     : Ctrl_Addr_T;
    begin
        inputGenProc : process
            constant PLAN_LENGTH : natural := 10;
            constant MAX_WAIT    : natural := 5;

            type Test_Plan_T is array (0 to PLAN_LENGTH - 1) of Ctrl_Cmd_T;
            type Test_Addr_Plan_T is array (0 to PLAN_LENGTH - 1) of Ctrl_Addr_T;
            type Addr_Generation_T is (SameAddr, ConsecutiveAddr, RandomAddr);

            -- tests all possible command combinations (ref, write, read) x (ref, write, read)
            constant TEST_CMD_PLAN  : Test_Plan_T      := (
                Write, Write, Read, Read, Refresh,
                Refresh, Read, Write, Refresh, Write
            );
            variable TEST_ADDR_PLAN : Test_Addr_Plan_T := (others => (others => '0'));

            variable randomGen   : RandomPType;
            variable tmpAddr     : Ctrl_Addr_T;
            variable tmpFullAddr : Full_Addr_T;
        begin
            randomGen.InitSeed("inputGenProc");

            for addrGen in Addr_Generation_T loop
                for cmdDelay in 0 to MAX_WAIT - 1 loop

                    case addrGen is
                        when SameAddr =>
                            tmpAddr := randomGen.RandUnsigned(PAGES_REQUIRED - 1, Ctrl_Addr_T'length);
                            for i in TEST_ADDR_PLAN'range loop
                                TEST_ADDR_PLAN(i) := tmpAddr;
                            end loop;

                        when ConsecutiveAddr =>
                            tmpAddr := randomGen.RandUnsigned(PAGES_REQUIRED - 100, Ctrl_Addr_T'length);
                            for i in TEST_ADDR_PLAN'range loop
                                TEST_ADDR_PLAN(i) := tmpAddr;
                                tmpAddr           := tmpAddr + 1;
                            end loop;

                        when RandomAddr =>
                            for i in TEST_ADDR_PLAN'range loop
                                TEST_ADDR_PLAN(i) := randomGen.RandUnsigned(PAGES_REQUIRED - 1, Ctrl_Addr_T'length);
                            end loop;
                    end case;

                    -- initialize memory to random values for reading tests
                    for i in TEST_ADDR_PLAN'range loop
                        if TEST_CMD_PLAN(i) = Read then
                            for col in Col_Ptr_T loop
                                tmpFullAddr := addr_ptr_to_addr(to_integer(TEST_ADDR_PLAN(i)(1 downto 0)), to_integer(TEST_ADDR_PLAN(i)(Ctrl_Addr_T'high downto 2)), col);
                                memoryModel.MemWrite(tmpFullAddr, randomGen.RandSlv(Data_T'length));
                            end loop;
                        end if;
                    end loop;

                    for planPtr in TEST_CMD_PLAN'range loop
                        wait until cmdReady;
                        if cmdDelay /= 0 then
                            for i in 0 to cmdDelay - 1 loop
                                wait until rising_edge(testClk);
                            end loop;
                        end if;

                        Log(SDRAM_CTRL_TB_ALERT_ID,
                            "Currently testing with parameters (addrType, delay, cmd): " & to_string(addrGen) & ", " & to_string(cmdDelay) & ", " & to_string(TEST_CMD_PLAN(planPtr)), INFO);
                        Log(SDRAM_CTRL_TB_ALERT_ID,
                            "Current address: 0x" & to_hstring(TEST_ADDR_PLAN(planPtr)), INFO);

                        ctrlCmd  <= TEST_CMD_PLAN(planPtr);
                        ctrlAddr <= TEST_ADDR_PLAN(planPtr);

                        -- save addr + cmd for data test processes
                        currCmd  <= TEST_CMD_PLAN(planPtr);
                        currAddr <= TEST_ADDR_PLAN(planPtr);

                        Toggle(newCmdToggle);

                        wait until rising_edge(testClk) and cmdReady;
                        ctrlCmd <= NoOp;
                    end loop;
                end loop;
            end loop;

            Log(SDRAM_CTRL_TB_ALERT_ID, "Testcases end");

            stimuliEnd <= true;
            Toggle(newCmdToggle);

            wait;
        end process inputGenProc;

        dataReadCheckProc : process
            variable tmpFullAddr : Full_Addr_T;
            variable tmpCmd      : Ctrl_Cmd_T  := NoOp;
            variable tmpAddr     : Ctrl_Addr_T := (others => '0');
        begin
            while not stimuliEnd loop
                WaitForToggle(newCmdToggle);

                tmpCmd  := currCmd;
                tmpAddr := currAddr;

                if tmpCmd = Read then
                    for col in Col_Ptr_T loop
                        wait until rising_edge(testClk) and ctrlNewData for 20 * CLK_PERIOD;
                        AlertIfNot(SDRAM_CTRL_TB_ALERT_ID, ctrlNewData, "Didn't receive ""new data ready"" flag after 20 clock cycles", FAILURE);

                        tmpFullAddr := addr_ptr_to_addr(to_integer(tmpAddr(1 downto 0)), to_integer(tmpAddr(Ctrl_Addr_T'high downto 2)), col);
                        AlertIfNot(SDRAM_CTRL_TB_ALERT_ID, ctrlDataOut = memoryModel.MemRead(tmpFullAddr),
                                   "Invalid data received at address (addr, col): " & to_hstring(tmpAddr) & " x " & to_string(col) & LF & "Expected: " & to_hstring(memoryModel.MemRead(tmpFullAddr)) & LF & "Received: " & to_hstring(ctrlDataOut), FAILURE);
                    end loop;
                end if;
            end loop;

            wait;
        end process dataReadCheckProc;

        -- we have to take care when checking if data were written correctly
        -- because data are written to memory with a delay,
        -- so we have to wait after burst ended for tRDL cycles
        writeBlock : block
            signal checkToggle : std_logic   := '0';
            signal checkAddr   : Ctrl_Addr_T := (others => '0');

            shared variable checkData : Page_Array_T;
        begin
            dataWriteDataGenProc : process
                variable tmpCmd  : Ctrl_Cmd_T  := NoOp;
                variable tmpAddr : Ctrl_Addr_T := (others => '0');

                variable dataArray : Page_Array_T;
            begin
                dataArray.initRandomGen;

                while not stimuliEnd loop
                    WaitForToggle(newCmdToggle);
                    --                Log(SDRAM_CTRL_TB_ALERT_ID, "Toggle toggled", DEBUG);
                    tmpCmd  := currCmd;
                    tmpAddr := currAddr;

                    --                    Log(SDRAM_CTRL_TB_ALERT_ID, "Curr parameters (cmd, addr, col): " & to_string(tmpCmd) & ", 0x" & to_hstring(tmpAddr) & ", " & to_string(currCol), DEBUG);

                    if tmpCmd = Write then
                        -- generate random values for writing to memory
                        dataArray.generateFullPage;
                        ctrlDataIn <= dataArray.getCol(0);

                        for col in 1 to Col_Ptr_T'high loop
                            wait until rising_edge(testClk) and ctrlProvideNewData for 20 * CLK_PERIOD;
                            AlertIfNot(SDRAM_CTRL_TB_ALERT_ID, ctrlProvideNewData, "Didn't receive ""provide new data"" flag after 20 clock cycles", FAILURE);

                            ctrlDataIn <= dataArray.getCol(col);
                        end loop;

                        -- there is a delay before data is written to memory
                        -- so we have to check data only after the delay is over
                        checkData.initFromFullPage(dataArray.getFullPage);
                        checkAddr <= tmpAddr;
                        Toggle(checkToggle);
                    end if;
                end loop;

                Toggle(checkToggle);

                wait;
            end process dataWriteDataGenProc;

            dataWriteDataCheckProc : process
                variable fullAddr : Full_Addr_T;
            begin
                while not stimuliEnd loop
                    WaitForToggle(checkToggle);
                    wait until rising_edge(testClk);
                    -- account for inter-device register delay
                    wait for (tRDL + 2) * CLK_PERIOD;

                    for col in Col_Ptr_T loop
                        fullAddr := addr_ptr_to_addr(to_integer(checkAddr(1 downto 0)), to_integer(checkAddr(Ctrl_Addr_T'high downto 2)), col);
                        AlertIfNot(SDRAM_CTRL_TB_ALERT_ID, memoryModel.MemRead(fullAddr) = checkData.getCol(col),
                                   "Data not correctly written to memory at address (addr, col): 0x" & to_hstring(checkAddr) & " x " & to_string(col) & LF & "Data written: " & to_hstring(checkData.getCol(col)) & LF & "Data read: " & to_hstring(memoryModel.MemRead(fullAddr)), FAILURE);
                    end loop;
                end loop;

                wait;
            end process dataWriteDataCheckProc;
        end block writeBlock;
    end block testBlock;
end tb;
