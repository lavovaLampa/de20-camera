library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.ccd_pkg.all;
use work.i2c_pkg.all;
use work.ccd_config_pkg.all;

entity i2c_ccd_config is
    port(
        clkIn, rstAsyncIn : in    std_logic;
        enableIn          : in    boolean;
        doneOut           : out   boolean := false;
        -- I2C communication
        sClkOut           : out   std_logic;
        sDataIO           : inout std_logic
    );
    alias romData is CCD_CONFIG;
end entity i2c_ccd_config;

architecture RTL of i2c_ccd_config is
    type Ctrl_State_T is (WaitForEnable, SendCmd, AwaitCompletion, Done);

    -- state registers
    signal fsmState  : Ctrl_State_T := WaitForEnable;
    -- TODO: add bounds
    signal configPtr : natural      := 0;

    -- i2c controller INPUT
    signal ctrlData            : I2c_Data_T := X"0000";
    signal devAddr, dataAddr   : I2c_Addr_T := X"00";
    signal ctrlEnable          : boolean    := false;
    -- i2c controller OUTPUT
    signal ctrlDone, ctrlError : boolean;
begin

    devAddr <= CCD_WRITE_ADDR;

    controlProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            fsmState  <= WaitForEnable;
            configPtr <= 0;
            doneOut   <= false;
        -- TODO: check if falling_edge -> rising_edge change breaks anything
        elsif rising_edge(clkIn) then
            case fsmState is
                when WaitForEnable =>
                    configPtr <= 0;
                    if enableIn then
                        fsmState <= SendCmd;
                    end if;

                when SendCmd =>
                    assert not ctrlDone and not ctrlError report "Controller in invalid state" severity failure;
                    fsmState   <= AwaitCompletion;
                    dataAddr   <= romData(configPtr).addr;
                    ctrlData   <= romData(configPtr).data;
                    ctrlEnable <= true;

                when AwaitCompletion =>
                    ctrlEnable <= false;

                    if ctrlDone then
                        if configPtr < romData'high then
                            fsmState  <= SendCmd;
                            configPtr <= configPtr + 1;
                        else
                            fsmState <= Done;
                        end if;
                    elsif ctrlError then
                        report "Controller reported error" severity warning;
                        fsmState <= SendCmd;
                    end if;

                when Done =>
                    doneOut <= true;
            end case;
        end if;
    end process controlProc;

    i2c_ctrl_inst : entity work.i2c_ctrl
        port map(
            clkIn          => clkIn,
            rstAsyncIn     => rstAsyncIn,
            enableInStrobe => ctrlEnable,
            dataIn         => ctrlData,
            devAddrIn      => devAddr,
            dataAddrIn     => dataAddr,
            doneOut        => ctrlDone,
            errorOut       => ctrlError,
            sClkOut        => sClkOut,
            sDataIO        => sDataIO
        );

end architecture RTL;
