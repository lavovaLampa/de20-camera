library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity reset_ctrl is
    port(
        clkIn, rstAsyncIn        : in  std_logic;
        -- input
        memInitialized           : in  boolean;
        ccdConfigurationDone     : in  boolean;
        -- reset output
        dataCtrlResetOut         : out std_logic; -- bring up first and wait for mem
        ccdResetOut              : out std_logic; -- bring up second and wait for power up
        ccdConfigurationResetOut : out std_logic; -- bring up third to configure ccd
        resetOut                 : out std_logic -- bring up everything else
    );
end entity reset_ctrl;

architecture RTL of reset_ctrl is
    constant COUNTER_MAX                          : natural := 2**11;
    signal memInitStabilizer0, memInitStabilizer1 : boolean; -- crossing clock domains, avoid metastability
begin
    rstProc : process(clkIn, rstAsyncIn)
        type Ctrl_State_T is (InitMem, InitCcd, WriteConfiguration, InitRest);

        -- state registers
        variable currState    : Ctrl_State_T                       := InitMem;
        variable resetCounter : natural range 0 to COUNTER_MAX - 1 := 0;
    begin
        if rstAsyncIn = '1' then
            currState    := InitMem;
            resetCounter := 0;

            dataCtrlResetOut         <= '1';
            ccdResetOut              <= '1';
            ccdConfigurationResetOut <= '1';
            resetOut                 <= '1';
        elsif rising_edge(clkIn) then
            memInitStabilizer0 <= memInitialized;
            memInitStabilizer1 <= memInitStabilizer0;

            case currState is
                when InitMem =>
                    if resetCounter > 1000 then
                        dataCtrlResetOut <= '0';

                        if memInitStabilizer1 then
                            resetCounter := 0;
                            currState    := InitCcd;
                        end if;
                    else
                        resetCounter := resetCounter + 1;
                    end if;

                when InitCcd =>
                    ccdResetOut <= '0';

                    if resetCounter > 1000 then
                        currState    := WriteConfiguration;
                        resetCounter := 0;
                    else
                        resetCounter := resetCounter + 1;
                    end if;

                when WriteConfiguration =>
                    ccdConfigurationResetOut <= '0';

                    if ccdConfigurationDone then
                        currState := InitRest;
                    end if;

                when InitRest =>
                    resetOut <= '0';

            end case;
        end if;
    end process rstProc;
end architecture RTL;
