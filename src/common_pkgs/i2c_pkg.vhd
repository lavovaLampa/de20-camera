library ieee;
use ieee.std_logic_1164.all;

package i2c_pkg is
    constant DATA_WIDTH      : natural   := 16;
    constant ADDR_WIDTH      : natural   := 8;
    constant AGGREGATE_WIDTH : natural   := (2 * ADDR_WIDTH) + DATA_WIDTH;
    constant I2C_WRITE       : std_logic := '0';
    constant I2C_READ        : std_logic := '1';

    subtype Data_Range is natural range DATA_WIDTH - 1 downto 0;
    subtype Addr_Range is natural range ADDR_WIDTH - 1 downto 0;
    subtype Data_Aggregate_Range is natural range AGGREGATE_WIDTH - 1 downto 0;

    subtype I2C_Data is std_logic_vector(Data_Range);
    subtype I2C_Addr is std_logic_vector(Addr_Range);
    subtype I2C_Aggregate is std_logic_vector(Data_Aggregate_Range);

    constant CCD_READ_ADDR  : I2C_Addr := X"BB";
    constant CCD_WRITE_ADDR : I2C_Addr := X"BA";

    type FSM_State is (Ready, SendData, ReleaseLine, WaitForAck, SendStop);

    pure function logicToI2CBusState(val : std_logic) return std_logic;
    pure function i2cBusStateToLogic(val : std_logic) return std_logic;
end package i2c_pkg;

package body i2c_pkg is

    pure function logicToI2CBusState(val : std_logic)
    return std_logic is
    begin
        case val is
            when '1' => return 'Z';
            when '0' => return '0';
            when others =>
                report "invalid logic value" severity failure;
                return 'Z';
        end case;
    end logicToI2CBusState;

    pure function i2cBusStateToLogic(val : std_logic) return std_logic is
    begin
        case val is
            when 'Z' => return '1';
            when '0' => return '0';
            when others =>
                report "invalid i2c bus state" severity failure;
                return 'Z';
        end case;
    end function i2cBusStateToLogic;

end package body i2c_pkg;
