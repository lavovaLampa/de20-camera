library ieee;
use ieee.std_logic_1164.all;

package i2c_pkg is
    constant DATA_WIDTH     : natural                                     := 16;
    constant ADDR_WIDTH     : natural                                     := 8;
    constant I2C_WRITE      : std_logic                                   := '0';
    constant I2C_READ       : std_logic                                   := '1';
    constant CCD_READ_ADDR  : std_logic_vector((ADDR_WIDTH - 1) downto 0) := X"BB";
    constant CCD_WRITE_ADDR : std_logic_vector((ADDR_WIDTH - 1) downto 0) := X"BA";

    function logicToI2CBusState(val : std_logic) return std_logic;
end package i2c_pkg;

package body i2c_pkg is

    function logicToI2CBusState(val : std_logic)
    return std_logic is
    begin
        case val is
            when '1'    => return 'Z';
            when others => return '0';
        end case;
    end logicToI2CBusState;

end package body i2c_pkg;
