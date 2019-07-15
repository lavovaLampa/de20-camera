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
        memIoOut          : out Mem_Data_Aggregate_R;
        clkEnableOut      : out std_logic;
        doneOut           : out boolean := false
    );
end entity sdram_init;

architecture RTL of sdram_init is
    signal waitCounter : natural range 0 to INIT_DELAY_CYCLES + 100 := 0;
    signal nextData    : Data_T                                     := (others => 'Z');
    signal nextIo      : Mem_IO_Aggregate_R                         := nop;
begin
    -- pack mem i/o with data
    memIoOut <= (
        cmd  => nextIo.cmd,
        addr => nextIo.addr,
        bank => nextIo.bank,
        data => nextData
    );

    initProc : process(clkIn, rstAsyncIn)
        type Internal_State_T is (InitDelay, PrechargeAll, Refresh, LoadModeReg, Done);

        -- reg
        variable currState      : Internal_State_T                           := InitDelay;
        -- we have to refresh all rows 2 times -> 2 * 2**ROW_ADDR_WIDTH = 2**(ROW_ADDR_WIDTH + 1)
        variable refreshCounter : natural range 0 to 2**(ROW_ADDR_WIDTH + 1) := 2**(ROW_ADDR_WIDTH + 1);
    begin
        if rstAsyncIn = '1' then
            clkEnableOut <= '0';
            doneOut      <= false;

            nextIo      <= nop;
            waitCounter <= INIT_DELAY_CYCLES;

            currState      := InitDelay;
            refreshCounter := 2**(ROW_ADDR_WIDTH + 1);
        elsif rising_edge(clkIn) then
            -- by default send nop command
            nextIo   <= nop;
            nextData <= (others => 'Z');

            if clkStableIn then
                clkEnableOut <= '1';
                waitCounter  <= waitCounter - 1;

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
                                currState   := LoadModeReg;
                                nextIo      <= load_mode_reg(MODE_REG);
                                nextData    <= MODE_REG;
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
                        doneOut <= true;

                end case;
            end if;
        end if;
    end process initProc;
end architecture RTL;
