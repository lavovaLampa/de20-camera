library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.sdram_pkg.all;

package sdram_ctrl_pkg is
    constant ADDR_WIDTH : natural := ROW_ADDR_WIDTH + BANK_ADDR_WIDTH;

    type Ctrl_Cmd_T is (NoOp, Read, Write, Refresh);
    subtype Ctrl_Addr_T is unsigned(ADDR_WIDTH - 1 downto 0);

    type Ctrl_Addr_R is record
        row  : Addr_T;
        bank : Bank_Addr_T;
    end record Ctrl_Addr_R;

    type Bank_State_R is record
        active : boolean;
        row    : Addr_T;
    end record Bank_State_R;
    type Bank_State_Array_T is array (0 to BANK_COUNT - 1) of Bank_State_R;

    type Scheduled_Cmd_R is record
        cmd        : Cmd_T;
        addr       : Ctrl_Addr_R;
        startBurst : boolean;
        done       : boolean;
    end record Scheduled_Cmd_R;

    type Burst_State_R is record
        inBurst : boolean;
        counter : natural range 0 to 2**COL_ADDR_WIDTH + tCAS;
    end record Burst_State_R;

    pure function next_row_addr(currAddr : Ctrl_Addr_T; rowMax : natural) return Ctrl_Addr_T;
    pure function addr_to_record(addr : Ctrl_Addr_T) return Ctrl_Addr_R;
end package sdram_ctrl_pkg;

package body sdram_ctrl_pkg is
    -- prevent addr overflow
    pure function next_row_addr(currAddr : Ctrl_Addr_T; rowMax : natural) return Ctrl_Addr_T is
    begin
        if currAddr < rowMax - 1 then
            return currAddr + 1;
        else
            return (others => '0');
        end if;
    end function next_row_addr;

    pure function addr_to_record(addr : Ctrl_Addr_T) return Ctrl_Addr_R is
    begin
        return (
            row  => addr(addr'high downto BANK_ADDR_WIDTH),
            bank => addr(BANK_ADDR_WIDTH - 1 downto 0)
        );
    end function addr_to_record;

end package body sdram_ctrl_pkg;
