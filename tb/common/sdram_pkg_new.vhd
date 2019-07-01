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
    pure function bank_next_state(currState : Bank_State_T) return Bank_State_T;
    pure function bank_transition_valid(currState : Bank_State_T; nextState : Bank_State_T) return boolean;
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
end package body sdram_pkg_new;
