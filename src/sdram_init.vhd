library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.sdram_pkg.all;

entity sdram_init is
    generic(
        INIT_DELAY_CYCLES : natural                       := 200 us / 7.5 ns;
        MODE_REG          : std_logic_vector(15 downto 0) := (others => '0')
    );
    port(
        clkIn, rstAsyncIn : in  std_logic;
        clkStableIn       : in  std_logic;
        memIoOut          : out Mem_IO_Aggregate_R;
        clkEnableOut      : out std_logic;
        doneOut           : out boolean := false
    );
end entity sdram_init;

architecture RTL of sdram_init is
    signal waitCounter : natural range 0 to INIT_DELAY_CYCLES + 100 := 0;
begin
    initProc : process(clkIn, rstAsyncIn)
        type Internal_State_T is (InitDelay, PrechargeAll, Refresh, LoadModeReg, Done);

        -- reg
        variable currState      : Internal_State_T                           := InitDelay;
        -- we have to refresh all rows 2 times -> 2 * 2**ROW_ADDR_WIDTH = 2**(ROW_ADDR_WIDTH + 1)
        variable refreshCounter : natural range 0 to 2**(ROW_ADDR_WIDTH + 1) := 2**(ROW_ADDR_WIDTH + 1);
    begin
        if rstAsyncIn = '1' then
            clkEnableOut <= '0';
            memIoOut     <= nop;
            doneOut      <= false;

            waitCounter <= INIT_DELAY_CYCLES;

            currState      := InitDelay;
            refreshCounter := 2**(ROW_ADDR_WIDTH + 1);
        elsif rising_edge(clkIn) then
            -- by default send nop command
            memIoOut <= nop;

            if clkStableIn then
                waitCounter <= waitCounter - 1;

                case currState is
                    when InitDelay =>
                        if waitCounter = 0 then
                            currState   := PrechargeAll;
                            memIoOut    <= precharge((others => '-'), true);
                            waitCounter <= cmd_delay(Precharge) - 1;
                        end if;

                    when PrechargeAll =>
                        if waitCounter = 0 then
                            currState   := Refresh;
                            memIoOut    <= refresh;
                            waitCounter <= cmd_delay(Refresh) - 1;
                        end if;

                    when Refresh =>
                        if waitCounter = 0 then
                            if refreshCounter = 0 then
                                currState   := LoadModeReg;
                                memIoOut    <= load_mode_reg(MODE_REG);
                                waitCounter <= cmd_delay(LoadModeReg) - 1;
                            else
                                refreshCounter := refreshCounter - 1;
                                memIoOut       <= refresh;
                                waitCounter    <= cmd_delay(Refresh) - 1;
                            end if;
                        end if;

                    when LoadModeReg =>
                        if waitCounter = 0 then
                            currState := Done;
                        end if;

                    when Done =>
                        doneOut <= true;

                end case;
            end if;
        end if;
    end process initProc;
end architecture RTL;
