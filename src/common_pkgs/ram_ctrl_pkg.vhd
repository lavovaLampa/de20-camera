library ieee;
use ieee.numeric_std.all;
use ieee.std_logic_1164.all;
use work.sdram_pkg.all;

package ram_ctrl_pkg is
    type Ctrl_Cmd_T is (NoOp, Read, Write, Refresh);
    type Scheduler_Cmd_T is (Active, Precharge, PrechargeAll, Refresh);

    constant ADDR_WIDTH : natural := ROW_ADDR_WIDTH + BANK_ADDR_WIDTH;

    subtype Ctrl_Addr_T is unsigned(ADDR_WIDTH - 1 downto 0);
    alias Ctrl_Data_T is Data_T;
    type Ctrl_Addr_R is record
        row  : Addr_T;
        bank : Bank_Addr_T;
    end record Ctrl_Addr_R;

    type Bank_State_R is record
        active : boolean;
        row    : Addr_T;
    end record Bank_State_R;
    type Bank_State_Array_T is array (BANK_COUNT - 1 downto 0) of Bank_State_R;

    type Scheduler_Cmd_Array_T is array (1 downto 0) of Scheduler_Cmd_T;
    type Scheduler_In_R is record
        cmdArray     : Scheduler_Cmd_Array_T;
        arrayPtr     : natural range Scheduler_Cmd_Array_T'range;
        addr         : Ctrl_Addr_R;
        batchExecute : boolean;
    end record Scheduler_In_R;

    type Scheduler_Out_R is record
        batchDone : boolean;
        memIo     : Mem_IO_Aggregate_R;
    end record Scheduler_Out_R;

    type Burst_State_R is record
        bursting : boolean;
        counter  : natural range 0 to 2**COL_ADDR_WIDTH;
        bank     : Bank_Addr_T;
        reading  : boolean;
    end record Burst_State_R;

    pure function next_row_addr(currAddr : Ctrl_Addr_T; rowMax : natural) return Ctrl_Addr_T;
    pure function addr_to_record(addr : Ctrl_Addr_T) return Ctrl_Addr_R;
end package ram_ctrl_pkg;

library ieee;
use ieee.std_logic_1164.all;
package body ram_ctrl_pkg is

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
end package body ram_ctrl_pkg;
