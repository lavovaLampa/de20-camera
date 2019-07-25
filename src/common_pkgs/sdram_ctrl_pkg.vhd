library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.sdram_pkg.all;

package sdram_ctrl_pkg is
    constant ADDR_WIDTH : natural := ROW_ADDR_WIDTH + BANK_ADDR_WIDTH;

    type Ctrl_Cmd_T is (NoOp, Read, Write, Refresh);
    subtype Ctrl_Addr_T is unsigned(ADDR_WIDTH - 1 downto 0);
    subtype Burst_Counter_Range_T is integer range -tCAS to 2**COL_ADDR_WIDTH;

    subtype Op_T is Ctrl_Cmd_T range Read to Write;
    type Burst_Len_T is array (Op_T) of natural;
    type Next_Op_Map_T is array (Op_T) of Op_T;
    constant next_op : Next_Op_Map_T := (
        Read  => Write,
        Write => Read
    );

    type Ctrl_Addr_R is record
        row  : Addr_T;
        bank : Bank_Addr_T;
    end record Ctrl_Addr_R;

    type Bank_State_R is record
        active : boolean;
        row    : Addr_T;
    end record Bank_State_R;
    type Bank_State_Array_T is array (0 to BANK_COUNT - 1) of Bank_State_R;

    type Scheduler_R is record
        cmd         : Cmd_T;
        addr        : Ctrl_Addr_R;
        startBurst  : boolean;
        isScheduled : boolean;
    end record Scheduler_R;

    type Burst_State_R is record
        inBurst   : boolean;
        counter   : Burst_Counter_Range_T;
        precharge : boolean;
        burstType : Op_T;
    end record Burst_State_R;

    type Prefetch_Data_R is record
        lastAddr     : Ctrl_Addr_R;
        cmdCounter   : natural range 0 to 7;
        isPrefetched : boolean;
    end record Prefetch_Data_R;

    type Prefetch_Array_T is array (Op_T) of Prefetch_Data_R;

    pure function next_row_addr(addrRecord : Ctrl_Addr_R; rowMax : natural) return Ctrl_Addr_R;
    pure function addr_to_record(addr : Ctrl_Addr_T) return Ctrl_Addr_R;
end package sdram_ctrl_pkg;

package body sdram_ctrl_pkg is
    pure function record_to_addr(addr : Ctrl_Addr_R) return Ctrl_Addr_T is
    begin
        return addr.row & addr.bank;
    end function record_to_addr;

    pure function addr_to_record(addr : Ctrl_Addr_T) return Ctrl_Addr_R is
    begin
        return (
            row  => addr(addr'high downto BANK_ADDR_WIDTH),
            bank => addr(BANK_ADDR_WIDTH - 1 downto 0)
        );
    end function addr_to_record;

    -- prevent addr overflow
    pure function next_row_addr(addrRecord : Ctrl_Addr_R; rowMax : natural) return Ctrl_Addr_R is
        variable currAddr : Ctrl_Addr_T := record_to_addr(addrRecord);
    begin
        if currAddr < rowMax - 1 then
            return addr_to_record(currAddr + 1);
        else
            return addr_to_record((others => '0'));
        end if;
    end function next_row_addr;
end package body sdram_ctrl_pkg;
