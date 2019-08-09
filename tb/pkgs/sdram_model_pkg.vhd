library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.sdram_pkg.all;

library osvvm;
use osvvm.MemoryPkg.all;

package sdram_model_pkg is
    -- memory model in package to be available for predictable initialization from testbench
    shared variable memoryModel : MemoryPType;

    subtype Full_Addr_T is std_logic_vector(ADDR_WIDTH - 1 downto 0);

    type Bank_State_R is record
        state : Bank_State_T;
        row   : Row_Ptr_T;
    end record Bank_State_R;
    type Bank_Array_T is array (BANK_COUNT - 1 downto 0) of Bank_State_R;

    type Ctrl_State_R is record
        state         : Sdram_State_T;
        autoPrecharge : boolean;
        currBank      : Bank_Ptr_T;
        burstDone     : boolean;
    end record Ctrl_State_R;

    type Input_Latch_R is record
        cmd  : Cmd_T;
        addr : Addr_T;
        bank : Bank_Addr_T;
    end record Input_Latch_R;

    subtype Burst_Length_Range_T is natural range 1 to 2**COL_ADDR_WIDTH - 1;
    type Data_IO_R is record
        data : Data_T;
        dqm  : Dqm_T;
    end record Data_IO_R;
    type Data_In_Pipeline_T is array (0 to 3) of Data_IO_R;
    type Data_Out_Pipeline_T is array (0 to 3) of Data_T;

    pure function logic_to_bool(val : std_logic) return boolean;
    pure function bank_next_state(currState : Bank_State_T) return Bank_State_T;
    pure function bank_transition_valid(currState : Bank_State_T; nextState : Bank_State_T) return boolean;
    pure function to_safe_natural(val : unsigned) return natural;
    pure function mask_data(data : Data_T; dqm : Dqm_T) return Data_T;
    pure function addr_ptr_to_addr(bank : Bank_Ptr_T; row : Row_Ptr_T; col : Col_Ptr_T) return Full_Addr_T;

    -- return number of cycles required for bank to transition from currState to nextState
    pure function bank_transition_delay(currState : Bank_State_T; nextState : Bank_State_T) return natural;
end package sdram_model_pkg;

package body sdram_model_pkg is
    pure function to_safe_natural(val : unsigned) return natural is
        variable hasMetavalue : boolean := false;
    begin
        for i in val'range loop
            hasMetavalue := hasMetavalue or (val(i) /= '0' and val(i) /= '1');
        end loop;

        if hasMetavalue then
            return 0;
        else
            return to_integer(val);
        end if;
    end function to_safe_natural;

    pure function logic_to_bool(val : std_logic) return boolean is
    begin
        case val is
            when '1'    => return true;
            when others => return false;
        end case;
    end function logic_to_bool;

    -- generate next bank state according to current state
    pure function bank_next_state(currState : Bank_State_T) return Bank_State_T is
    begin
        case currState is
            when Activating =>
                return ActiveRecharging;

            when ActiveRecharging =>
                return ActiveIdle;

            when Precharging =>
                return Idle;

            when Refreshing =>
                return Idle;

            when Idle | ActiveIdle =>
                report "Cannot generate next state, specify manually"
                severity error;
                return currState;

        end case;
    end function bank_next_state;

    pure function bank_transition_valid(currState : Bank_State_T; nextState : Bank_State_T) return boolean is
    begin
        if currState = Idle and nextState = Refreshing then
            return true;
        elsif currState = Refreshing and nextState = Idle then
            return true;
        elsif currState = Idle and nextState = Activating then
            return true;
        elsif currState = Activating and nextState = ActiveRecharging then
            return true;
        elsif currState = ActiveRecharging and nextState = ActiveIdle then
            return true;
        elsif currState = ActiveIdle and nextState = Precharging then
            return true;
        elsif currState = Precharging and nextState = Idle then
            return true;
        else
            return false;
        end if;
    end function bank_transition_valid;

    pure function mask_data(data : Data_T; dqm : Dqm_T) return Data_T is
        variable tmpData : Data_T := data;
    begin
        for i in dqm'range loop
            if dqm(i) = '1' then
                tmpData(((i + 1) * 8) - 1 downto i * 8) := (others => 'Z');
            end if;
        end loop;
        return tmpData;
    end function mask_data;

    pure function bank_transition_delay(currState : Bank_State_T; nextState : Bank_State_T) return natural is
        type State_Delay_Map_T is array (Bank_State_T, Bank_State_T) of natural;
        constant transitionDelay : State_Delay_Map_T := (
            Idle             => (
                Refreshing => 0,
                Activating => 0,
                others     => 0),
            Refreshing       => (
                Idle   => tRCCycles - 2,
                others => 0
            ),
            Activating       => (
                ActiveRecharging => tRCDCycles - 2,
                others           => 0),
            ActiveRecharging => (
                ActiveIdle => tRASminCycles - tRCDCycles - 2,
                others     => 0
            ),
            ActiveIdle       => (
                Precharging => 0,
                others      => 0
            ),
            Precharging      => (
                Idle   => tRPCycles - 2,
                others => 0
            )
        );
    begin
        if bank_transition_valid(currState, nextState) then
            return transitionDelay(currState, nextState);
        else
            report "Invalid state transition!" & LF &
                    "Cannot go from state " & Bank_State_T'image(currState) & " to state " & Bank_State_T'image(nextState)
            severity error;
        end if;
    end function bank_transition_delay;

    pure function addr_ptr_to_addr(bank : Bank_Ptr_T; row : Row_Ptr_T; col : Col_Ptr_T) return Full_Addr_T is
        variable bankAddr : Bank_Addr_T := to_unsigned(bank, Bank_Addr_T'length);
        variable rowAddr  : Row_Addr_T  := to_unsigned(row, Row_Addr_T'length);
        variable colAddr  : Col_Addr_T  := to_unsigned(col, Col_Addr_T'length);
    begin
        return std_logic_vector(bankAddr) & std_logic_vector(rowAddr) & std_logic_vector(colAddr);
    end function addr_ptr_to_addr;
end package body sdram_model_pkg;
