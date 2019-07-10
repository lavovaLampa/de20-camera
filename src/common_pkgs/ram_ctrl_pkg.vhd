library ieee;
use ieee.numeric_std.all;
use work.sdram_pkg.ROW_ADDR_WIDTH;
use work.sdram_pkg.BANK_ADDR_WIDTH;
use work.sdram_pkg.Data_T;

package ram_ctrl_pkg is
    type Ctrl_Cmd_T is (NoOp, Read, Write, Refresh);
    type Data_Op_T is (Read, Write);

    constant ADDR_WIDTH : natural := ROW_ADDR_WIDTH + BANK_ADDR_WIDTH;

    subtype Ctrl_Addr_T is unsigned(ADDR_WIDTH - 1 downto 0);
    alias Ctrl_Data_T is Data_T;

end package ram_ctrl_pkg;
