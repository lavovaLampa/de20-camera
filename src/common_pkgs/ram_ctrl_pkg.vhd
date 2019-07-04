use work.sdram_pkg.COL_ADDR_WIDTH;
use work.sdram_pkg.BANK_ADDR_WIDTH;

package ram_ctrl_pkg is
    type Ctrl_Cmd_T is (NoOp, Read, Write, Refresh);

    constant ADDR_WIDTH : natural := COL_ADDR_WIDTH + BANK_ADDR_WIDTH;

end package ram_ctrl_pkg;
