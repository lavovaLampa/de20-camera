-- Testbench automatically generated online
-- at http://vhdl.lapinoo.net
-- Generation date : 11.8.2019 16:30:39 GMT

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library poc;
use poc.utils.all;

library osvvm;
context osvvm.OsvvmContext;

entity fifo_tb is
    constant WRITE_CLK_PERIOD : time := 20 ns;
    constant READ_CLK_PERIOD  : time := 10 ns;

    constant TEST_DATA_WIDTH : positive := 24;
--    constant TEST_MIN_DEPTH  : positive := 640 * 2;
    constant TEST_MIN_DEPTH  : positive := 1025;
    constant STATE_BITS      : natural  := 3;

    constant FIFO_TB_ALERT_ID : AlertLogIDType := GetAlertLogID("Fifo testbench", ALERTLOG_BASE_ID);
end fifo_tb;

architecture tb of fifo_tb is
    -- clock & reset
    signal writeClk, readClk     : std_logic := '0';
    signal writeReset, readReset : std_logic := '0';

    -- fifo write port signals
    signal fifoPut, fifoFull : boolean;
    signal fifoDataIn        : std_logic_vector(TEST_DATA_WIDTH - 1 downto 0);
    signal fifoEmptyState    : unsigned(imax(STATE_BITS - 1, 0) downto 0);

    -- fifo read port signals
    signal fifoGot, fifoValid : boolean;
    signal fifoDataOut        : std_logic_vector(TEST_DATA_WIDTH - 1 downto 0);
    signal fifoFullState      : unsigned(imax(STATE_BITS - 1, 0) downto 0);

    -- testbench signals
    signal tbSimEnded : std_logic := '0';
begin

    dut : entity poc.fifo_ic_got
        generic map(
            DATA_WIDTH     => TEST_DATA_WIDTH,
            MIN_DEPTH      => TEST_MIN_DEPTH,
            DATA_REG       => true,
            OUTPUT_REG     => true,
            ESTATE_WR_BITS => STATE_BITS,
            FSTATE_RD_BITS => STATE_BITS
        )
        port map(
            -- write port
            clkWriteIn => writeClk,
            rstWriteIn => writeReset,
            -- write in
            put        => fifoPut,
            dataIn     => fifoDataIn,
            full       => fifoFull,
            estate_wr  => fifoEmptyState,
            -- read port
            clkReadIn  => readClk,
            rstReadIn  => readReset,
            -- read out
            got        => fifoGot,
            valid      => fifoValid,
            dataOut    => fifoDataOut,
            fstate_rd  => fifoFullState
        );

    -- clock generation
    writeClk <= not writeClk after WRITE_CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';
    readClk  <= not readClk after READ_CLK_PERIOD / 2 when tbSimEnded /= '1' else '0';

    stimuli : process
    begin
        -- reset generation
        writeReset <= '1';
        readReset  <= '1';
        wait for 5 * WRITE_CLK_PERIOD;
        writeReset <= '0';
        readReset  <= '0';
        wait for 5 * WRITE_CLK_PERIOD;

        -- EDIT Add stimuli here
        wait for 2500 * WRITE_CLK_PERIOD;

        -- Stop the clock and hence terminate the simulation
        tbSimEnded <= '1';
        wait;
    end process;

    fifoWriteProc : process(writeClk)
        variable dataCounter : natural := 0;
        variable randomGen   : RandomPType;
    begin
        if rising_edge(writeClk) then
            if writeReset = '1' then
                randomGen.InitSeed("lafjaoewfjnvzldj");
                dataCounter := 0;

                fifoPut    <= false;
                fifoDataIn <= (others => '-');
            else
                fifoPut <= false;

                if not fifoFull then
                    fifoPut    <= true;
                    fifoDataIn <= randomGen.RandSlv(TEST_DATA_WIDTH);
                end if;
            end if;
        end if;
    end process fifoWriteProc;

    fifoReadProc : process(readClk, fifoValid, fifoDataOut)
        variable dataCounter : natural := 0;
        variable waitCounter : natural := 200;
    begin
--        if fifoValid and waitCounter = 0 then
--            Log(FIFO_TB_ALERT_ID, "Got data: 0x" & to_hstring(fifoDataOut));
--            fifoGot <= true;
--        else
--            fifoGot <= false;
--        end if;

        if rising_edge(readClk) then
            if readReset = '1' then
                dataCounter := 0;
                waitCounter := 200;
            else
                dataCounter := dataCounter + 1;
                if waitCounter > 0 then
                    waitCounter := waitCounter - 1;
                end if;
            end if;
        end if;
    end process fifoReadProc;
end tb;
