library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.sdram_pkg.all;

entity sdram_init_ctrl is
    generic(
        MODE_REG          : Data_T  := (others => '0');
        INIT_DELAY_CYCLES : natural := 200 us / CLK_PERIOD
    );
    port(
        clkIn, rstAsyncIn   : in  std_logic;
        clkStableIn         : in  std_logic;
        -- functional outputs
        memInitializedOut   : out boolean := false;
        -- SDRAM I/O
        memOut              : out Mem_IO_R;
        memDataOut          : out Data_T;
        memDataOutputEnable : out boolean
    );
end entity sdram_init_ctrl;

architecture rtl of sdram_init_ctrl is
    type Internal_State_T is (InitDelay, PrechargeAll, Refresh, LoadModeReg, Done);
    signal nextIo       : Mem_IO_Aggregate_R                          := nop;
    signal nextData     : Data_T                                      := (others => '-');
    signal outputEnable : boolean                                     := false;
    signal waitCounter  : integer range -1 to INIT_DELAY_CYCLES + 100 := 0;
    signal dqm          : Dqm_T                                       := (others => '1');

    signal clkEnable : std_logic := '0';

    -- debug signals
    signal dbgInternalState : Internal_State_T;
begin
    -- pack mem i/o
    memOut              <= (
        cmdAggregate => encode_cmd(nextIo.cmd),
        addr         => nextIo.addr,
        bankSelect   => nextIo.bank,
        clkEnable    => clkEnable,
        dqm          => dqm
    );
    memDataOut          <= nextData;
    memDataOutputEnable <= outputEnable;

    initProc : process(clkIn, rstAsyncIn)
        -- reg
        variable currState      : Internal_State_T     := InitDelay;
        -- we have to issue REFRESH command atleast 2 times
        variable refreshCounter : natural range 0 to 3 := 3;
    begin
        if rstAsyncIn = '1' then
            memInitializedOut <= false;

            nextIo       <= nop;
            nextData     <= (others => '-');
            dqm          <= (others => '1');
            waitCounter  <= INIT_DELAY_CYCLES;
            clkEnable    <= '0';
            outputEnable <= false;

            currState      := InitDelay;
            refreshCounter := 3;
        elsif rising_edge(clkIn) then
            -- by default send nop command
            clkEnable    <= '1';
            nextIo       <= nop;
            dqm          <= (others => '1');
            outputEnable <= false;
            nextData     <= (others => '-');

            if clkStableIn then
                waitCounter <= waitCounter - 1 when currState /= Done;

                case currState is
                    when InitDelay =>
                        if waitCounter = 0 then
                            currState   := PrechargeAll;
                            nextIo      <= precharge((others => '-'), true);
                            waitCounter <= cmd_delay(Precharge) - 1;
                        end if;

                    when PrechargeAll =>
                        if waitCounter = 0 then
                            currState   := Refresh;
                            nextIo      <= refresh;
                            waitCounter <= cmd_delay(Refresh) - 1;
                        end if;

                    when Refresh =>
                        if waitCounter = 0 then
                            if refreshCounter = 0 then
                                currState := LoadModeReg;

                                nextIo       <= load_mode_reg(MODE_REG);
                                nextData     <= MODE_REG;
                                dqm          <= (others => '0');
                                outputEnable <= true;

                                waitCounter <= cmd_delay(LoadModeReg) - 1;
                            else
                                refreshCounter := refreshCounter - 1;
                                nextIo         <= refresh;
                                waitCounter    <= cmd_delay(Refresh) - 1;
                            end if;
                        end if;

                    when LoadModeReg =>
                        if waitCounter = 0 then
                            currState := Done;
                        end if;

                    when Done =>
                        memInitializedOut <= true;

                end case;
            end if;
        end if;

        -- debug signals
        dbgInternalState <= currState;
    end process initProc;
end architecture rtl;
