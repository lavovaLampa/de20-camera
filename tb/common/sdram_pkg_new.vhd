library ieee;
use ieee.std_logic_1164.all;

package sdram_pkg_new is
    constant BANK_COUNT : natural := 4;

    -- decoded SDRAM commands
    type Cmd_T is (CmdInhibit, NoOp, Active, Read, Write, BurstTerminate, Precharge, Refresh, LoadModeReg, CmdError);
    -- controller states
    type Ctrl_State_T is (Idle, ReadBurst, WriteBurst, AccessingModeReg);
    -- bank states
    type Bank_State_T is (Idle, Activating, ActiveRecharging, ActiveIdle, Precharging, Refreshing);
    type Burst_Type_T is (Interleaved, Sequential);
    type Write_Burst_Mode_T is (SingleLocation, ProgrammedLength);

    pure function logic_to_bool(val : std_logic) return boolean;
    pure function next_bank_state(currState : Bank_State_T) return Bank_State_T;
end package sdram_pkg_new;

package body sdram_pkg_new is
    pure function logic_to_bool(val : std_logic) return boolean is
    begin
        case val is
            when '1'    => return true;
            when others => return false;
        end case;
    end function logic_to_bool;

    -- generate next bank state according to current state
    pure function next_bank_state(currState : Bank_State_T) return Bank_State_T is
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
                report "Non-transitive state"
                severity error;
                return currState;

        end case;
    end function next_bank_state;
end package body sdram_pkg_new;
