library ieee;
use ieee.std_logic_1164.all;

package i2c_pkg is
    constant DATA_WIDTH : natural   := 16;
    constant ADDR_WIDTH : natural   := 8;
    constant I2C_WRITE  : std_logic := '0';
    constant I2C_READ   : std_logic := '1';

    subtype Data_Range is natural range DATA_WIDTH - 1 downto 0;
    subtype Addr_Range is natural range ADDR_WIDTH - 1 downto 0;

    subtype I2c_Data is std_logic_vector(Data_Range);
    subtype I2c_Addr is std_logic_vector(Addr_Range);

    constant CCD_READ_ADDR  : I2c_Addr := X"BB";
    constant CCD_WRITE_ADDR : I2c_Addr := X"BA";
    constant FULL_WIDTH     : natural  := (ADDR_WIDTH * 2) + DATA_WIDTH;

    type FSM_State is (Ready, SendData, SendStop);
    type ACK_State is (WaitForAck, ReleaseLine, ReadAck);

    pure function logicToI2CBusState(val : std_logic) return std_logic;
end package i2c_pkg;

package body i2c_pkg is

    pure function logicToI2CBusState(val : std_logic)
    return std_logic is
    begin
        case val is
            when '1'    => return 'Z';
            when others => return '0';
        end case;
    end logicToI2CBusState;

end package body i2c_pkg;
