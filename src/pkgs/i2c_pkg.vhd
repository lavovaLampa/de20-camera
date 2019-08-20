library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package i2c_pkg is
    constant DATA_WIDTH      : natural := 16;
    constant ADDR_WIDTH      : natural := 8;
    constant AGGREGATE_WIDTH : natural := (2 * ADDR_WIDTH) + DATA_WIDTH;

    -- i2c bus constants
    constant I2C_WRITE_FLAG : std_logic := '0';
    constant I2C_READ_FLAG  : std_logic := '1';

    subtype I2c_Data_T is std_logic_vector(DATA_WIDTH - 1 downto 0);
    subtype I2c_Addr_T is unsigned(ADDR_WIDTH - 1 downto 0);
    subtype I2c_Aggregate_T is std_logic_vector(AGGREGATE_WIDTH - 1 downto 0);

    subtype I2c_Aggregate_Ptr_T is natural range 0 to AGGREGATE_WIDTH - 1;

    constant CCD_READ_ADDR  : I2c_Addr_T := X"BB";
    constant CCD_WRITE_ADDR : I2c_Addr_T := X"BA";

    pure function logic_to_i2c(value : std_logic) return std_logic;
    pure function i2c_to_logic(value : std_logic) return std_logic;
end package i2c_pkg;

package body i2c_pkg is
    pure function logic_to_i2c(value : std_logic)
    return std_logic is
    begin
        case value is
            when '1' => return 'Z';
            when '0' => return '0';
            when others =>
                report "invalid logic value" severity failure;
                return 'Z';
        end case;
    end logic_to_i2c;

    pure function i2c_to_logic(value : std_logic) return std_logic is
    begin
        case value is
            when 'Z' => return '1';
            when '0' => return '0';
            when others =>
                report "invalid i2c bus state" severity failure;
                return 'Z';
        end case;
    end function i2c_to_logic;
end package body i2c_pkg;
