library ieee;
use ieee.std_logic_1164.all;
use work.sdram_pkg.all;

entity sdram_init_ctrl_tb is
    constant CLK_PERIOD : time   := 7.5 ns;
    constant MODE_REG   : Data_T := encode_mode_reg(PAGE_LEN, Sequential, 2, ProgrammedLength);
end sdram_init_ctrl_tb;

library osvvm;
context osvvm.OsvvmContext;

architecture tb of sdram_init_ctrl_tb is

    -- sdram_init_ctrl entity i/o signals
    signal clkIn, rstAsync : std_logic;
    signal clkStable       : std_logic;
    signal memIo           : Mem_Data_Aggregate_R;
    signal clkEnable       : std_logic;
    signal initDone        : boolean;

    -- sdram model entity i/o signals
    signal encodedCmd     : Cmd_Aggregate_R;
    signal memInitialized : boolean;
    signal simEnded       : boolean := false;

    -- clocking
    signal tbClock    : std_logic := '0';
    signal tbSimEnded : std_logic := '0';

begin
    encodedCmd <= encode_cmd(memIo.cmd);

    initCtrl : entity work.sdram_init_ctrl
        generic map(
            MODE_REG => MODE_REG
        )
        port map(
            clkIn        => clkIn,
            rstAsyncIn   => rstAsync,
            clkStableIn  => clkStable,
            memIoOut     => memIo,
            clkEnableOut => clkEnable,
            doneOut      => initDone
        );

    sdramModel : entity work.sdram_model
        generic map(
            LOAD_FROM_FILE => false,
            DUMP_TO_FILE   => false
        )
        port map(
            clkIn              => clkIn,
            addrIn             => memIo.addr,
            dataIn             => memIo.data,
            bankSelectIn       => memIo.bank,
            clkEnableIn        => clkEnable,
            chipSelectNegIn    => encodedCmd.chipSelectNeg,
            rowAddrStrobeNegIn => encodedCmd.rowAddrStrobeNeg,
            colAddrStrobeNegIn => encodedCmd.colAddrStrobeNeg,
            writeEnableNegIn   => encodedCmd.writeEnableNeg,
            dqmIn              => (others => '0'),
            -- debug signals
            isInitialized      => memInitialized,
            simEnded           => simEnded
        );

    -- Clock generation
    tbClock <= not tbClock after CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';

    -- EDIT: Check that clkIn is really your main clock signal
    clkIn <= tbClock;

    stimuli : process
    begin
--        SetLogEnable(DEBUG, true);
        SetLogEnable(INFO, true);

        -- EDIT Adapt initialization as needed
        clkStable <= '0';
        simEnded  <= false;

        -- Reset generation
        rstAsync <= '1';
        wait for 10 ns;
        rstAsync <= '0';
        wait for 10 ns;

        clkStable <= '1' after 50 ns;

        -- wait until controller reports succesfull initalization and then wait for
        -- one more clock cycle, to let the change propagate
        wait until initDone;
        wait until rising_edge(clkIn);

        assert memInitialized
        report "Memory doesn't report successfull initialization after init controller ends"
        severity error;

        -- Stop the clock and hence terminate the simulation
        simEnded   <= true;
        tbSimEnded <= '1';
        wait;
    end process;
end tb;
