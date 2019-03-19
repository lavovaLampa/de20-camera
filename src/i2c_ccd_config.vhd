library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.i2c_pkg.all;

entity i2c_ccd_config is
    generic(
        ROM_LENGTH : natural range 0 to 39 := 3
    );
    port(
        clkIn, rstAsyncIn       : in  std_logic;
        devAddrOut, dataAddrOut : out std_logic_vector((ADDR_WIDTH - 1) downto 0) := X"00";
        dataOut                 : out std_logic_vector((DATA_WIDTH - 1) downto 0) := X"0000";
        doneIn                  : in  boolean;
        enableOut, doneOut      : out boolean                                     := false
    );
    type FSM_State is (WaitForGo, SendCmd, WaitForCtrl, Done);
    type ROM_Data is record
        addr : std_logic_vector((ADDR_WIDTH - 1) downto 0);
        data : std_logic_vector((DATA_WIDTH - 1) downto 0);
    end record ROM_Data;
    type ROM_Array is array ((ROM_LENGTH - 1) downto 0) of ROM_Data;
end entity i2c_ccd_config;

architecture RTL of i2c_ccd_config is
    signal fsmState   : FSM_State                               := WaitForGo;
    signal romCounter : natural range (ROM_LENGTH - 1) downto 0 := ROM_LENGTH - 1;
    -- data to send to ccd chip in format (reg_addr, reg_data) [8b, 16b]
    constant romData  : ROM_Array                               := (
        (X"FF", X"FFFF"),
        (X"FF", X"FFFF"),
        (X"FF", X"FFFF")
    );
begin
    controlProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            fsmState   <= WaitForGo;
            romCounter <= ROM_LENGTH - 1;
            enableOut  <= false;
            doneOut    <= false;
        elsif falling_edge(clkIn) then
            case fsmState is
                when WaitForGo =>
                    fsmState   <= SendCmd;
                    romCounter <= ROM_LENGTH - 1;
                when SendCmd =>
                    dataAddrOut <= romData(romCounter).addr;
                    dataOut     <= romData(romCounter).data;
                    fsmState    <= WaitForCtrl;
                    enableOut   <= true;
                    if romCounter > 0 then
                        romCounter <= romCounter - 1;
                    end if;
                when WaitForCtrl =>
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
