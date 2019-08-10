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
    signal memIo           : Mem_IO_R;
    signal memData         : Data_T;
    signal initDone        : boolean;
    signal outputEnable    : boolean;
    signal memDataIo       : Data_T;

    -- sdram model entity i/o signals
    signal memInitialized : boolean;
    signal simEnded       : boolean := false;

    -- clocking
    signal tbClock    : std_logic := '0';
    signal tbSimEnded : std_logic := '0';

begin
    memDataIo <= memData when outputEnable else (others => 'Z');
    
    tmpCtrl : entity work.sdram_init_ctrl
        generic map(
            MODE_REG => MODE_REG
        )
        port map(
            clkIn               => clkIn,
            rstAsyncIn          => rstAsync,
            clkStableIn         => clkStable,
            memInitializedOut   => initDone,
            memOut              => memIo,
            memDataOut          => memData,
            memDataOutputEnable => outputEnable
        );

    sdramModel : entity work.sdram_model
        generic map(
            LOAD_FROM_FILE => false,
            DUMP_TO_FILE   => false
        )
        port map(
            clkIn              => clkIn,
            addrIn             => memIo.addr,
            bankSelectIn       => memIo.bankSelect,
            clkEnableIn        => memIo.clkEnable,
            chipSelectNegIn    => memIo.cmdAggregate.chipSelectNeg,
            rowAddrStrobeNegIn => memIo.cmdAggregate.rowAddrStrobeNeg,
            colAddrStrobeNegIn => memIo.cmdAggregate.colAddrStrobeNeg,
            writeEnableNegIn   => memIo.cmdAggregate.writeEnableNeg,
            dqmIn              => memIo.dqm,
            dataIo             => memDataIo,
            -- debug signals
            isInitializedOut   => memInitialized,
            simEndedIn         => simEnded
        );

    -- clock generation
    tbClock <= not tbClock after CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';
    clkIn   <= tbClock;

    stimuli : process
    begin
        --        SetLogEnable(DEBUG, true);
        SetLogEnable(INFO, true);

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
        report "Memory didn't report successfull initialization after init controller is done"
        severity error;

        -- Stop the clock and hence terminate the simulation
        simEnded   <= true;
        tbSimEnded <= '1';
        wait;
    end process;
end tb;
