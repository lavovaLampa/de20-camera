library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.ccd_pkg.all;
use work.i2c_pkg.all;
use work.ccd_config_pkg.all;

library osvvm;
context osvvm.OsvvmContext;

entity i2c_ccd_config is
    port(
        clkIn, rstAsyncIn   : in    std_logic;
        enableStrobeIn      : in    boolean;
        configDoneStrobeOut : out   boolean := false;
        -- i2c i/o
        sClkOut             : out   std_logic;
        sDataIO             : inout std_logic
    );
    constant I2C_CONFIG_ALERT_ID : AlertLogIDType := GetAlertLogID("i2c config ctrl", ALERTLOG_BASE_ID);
end entity i2c_ccd_config;

architecture RTL of i2c_ccd_config is
    -- i2c controller INPUT
    signal ctrlData            : I2c_Data_T := (others => '-');
    signal dataAddr            : I2c_Addr_T := (others => '-');
    signal devAddr             : I2c_Addr_T;
    signal ctrlEnable          : boolean    := false;
    -- i2c controller OUTPUT
    signal ctrlDone, ctrlError : boolean;
begin
    -- device address is hard-coded and known at synthesize-time
    devAddr <= CCD_WRITE_ADDR;

    controlProc : process(clkIn, rstAsyncIn)
        type Ctrl_State_T is (WaitForEnable, DataSend, AwaitCompletion);

        -- state registers
        variable currState : Ctrl_State_T := WaitForEnable;
        variable configPtr : Config_Ptr_T := 0;
    begin
        if rstAsyncIn = '1' then
            currState := WaitForEnable;
            configPtr := 0;

            ctrlData   <= (others => '-');
            dataAddr   <= (others => '-');
            ctrlEnable <= false;

            configDoneStrobeOut <= false;
        -- TODO: check if falling_edge -> rising_edge change breaks anything
        elsif rising_edge(clkIn) then
            ctrlEnable          <= false; -- strobe
            configDoneStrobeOut <= false; -- strobe

            case currState is
                when WaitForEnable =>
                    if enableStrobeIn then
                        currState := DataSend;
                        configPtr := 0;
                    end if;

                when DataSend =>
                    AlertIf(ctrlDone or ctrlError, "Controller in invalid state");

                    dataAddr   <= CCD_CONFIG_ARRAY(configPtr).addr;
                    ctrlData   <= CCD_CONFIG_ARRAY(configPtr).data;
                    ctrlEnable <= true;

                    currState := AwaitCompletion;

                when AwaitCompletion =>
                    if ctrlDone then
                        if configPtr < CCD_CONFIG_ARRAY'high then
                            currState := DataSend;
                            configPtr := configPtr + 1;
                        else
                            configDoneStrobeOut <= true;
                            currState           := WaitForEnable;
                        end if;
                    elsif ctrlError then
                        report "Controller reported error" severity warning;

                        currState := DataSend;
                    end if;
            end case;
        end if;
    end process controlProc;

    i2cCtrl : entity work.i2c_ctrl
        port map(
            clkIn          => clkIn,
            rstAsyncIn     => rstAsyncIn,
            enableStrobeIn => ctrlEnable,
            dataIn         => ctrlData,
            devAddrIn      => devAddr,
            dataAddrIn     => dataAddr,
            doneStrobeOut        => ctrlDone,
            errorStrobeOut       => ctrlError,
            sClkOut        => sClkOut,
            sDataIo        => sDataIO
        );

end architecture RTL;
