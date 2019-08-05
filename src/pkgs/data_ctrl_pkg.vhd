library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package data_ctrl_pkg is
    constant STATE_BITS : natural := 2;
    
    subtype Packed_Pixel_T is std_logic_vector(23 downto 0);

    subtype Fifo_State_T is unsigned(STATE_BITS - 1 downto 0);
    type Fifo_State_R is record
        fullState  : Fifo_State_T;
        emptyState : Fifo_State_T;
    end record Fifo_State_R;

end package data_ctrl_pkg;

package body data_ctrl_pkg is

end package body data_ctrl_pkg;
