library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.i2c_pkg.all;

entity i2c_ccd_config is
    port(
        clkIn, rstAsyncIn       : in  std_logic;
        enableIn                : in  boolean;
        errorIn, doneIn         : in  boolean;
        devAddrOut, dataAddrOut : out I2c_Addr;
        dataOut                 : out I2c_Data;
        enableOut               : out boolean;
        doneOut                 : out boolean := false
    );
    type FSM_State is (WaitForEnable, SendCmd, AwaitCompletion, Done);
    type ROM_Data is record
        addr : I2c_Addr;
        data : I2c_Data;
    end record ROM_Data;
    type ROM_Array is array (integer range <>) of ROM_Data;
end entity i2c_ccd_config;

architecture RTL of i2c_ccd_config is
    signal fsmState   : FSM_State := WaitForEnable;
    -- data to send to ccd chip in format (reg_addr, reg_data) [8b, 16b]
    constant romData  : ROM_Array := (
        (X"FF", X"FFFF"),
        (X"FF", X"FFFF"),
        (X"FF", X"FFFF")
    );
    signal romCounter : natural   := 2;
begin
    controlProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            fsmState   <= WaitForEnable;
            romCounter <= romData'high;
            doneOut    <= false;
        elsif falling_edge(clkIn) then
            case fsmState is
                when WaitForEnable =>
                    fsmState   <= SendCmd;
                    romCounter <= romData'high;

                when SendCmd =>
                    fsmState    <= AwaitCompletion;
                    dataAddrOut <= romData(romCounter).addr;
                    dataOut     <= romData(romCounter).data;
                    enableOut   <= true;
                    if romCounter > 0 then
                        romCounter <= romCounter - 1;
                    end if;

                when AwaitCompletion =>
                    enableOut <= false;
                    if doneIn then
                        if romCounter = 0 then
                            fsmState <= Done;
                        else
                            fsmState <= SendCmd;
                        end if;
                    end if;

                when Done =>
                    doneOut <= true;
            end case;
        end if;

        devAddrOut <= CCD_WRITE_ADDR;
    end process controlProc;

end architecture RTL;
