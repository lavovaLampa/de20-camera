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
    subtype Cmd_Signal_Aggregate_T is std_logic_vector(3 downto 0);

    pure function decode_cmd(chipSelectNeg, rowAddrStrobeNeg, colAddrStrobeNeg, writeEnableNeg : std_logic) return Cmd_T;
    pure function encode_cmd(cmd : Cmd_T) return Cmd_Signal_Aggregate_T;
    pure function logic_to_bool(val : std_logic) return boolean;
    pure function bank_next_state(currState : Bank_State_T) return Bank_State_T;
    pure function bank_transition_valid(currState : Bank_State_T; nextState : Bank_State_T) return boolean;
end package sdram_pkg_new;

package body sdram_pkg_new is
    pure function decode_cmd(chipSelectNeg, rowAddrStrobeNeg, colAddrStrobeNeg, writeEnableNeg : std_logic) return Cmd_T is
        variable cmdSelectAggregate : std_logic_vector(3 downto 0) := chipSelectNeg & rowAddrStrobeNeg & colAddrStrobeNeg & writeEnableNeg;
    begin
        case cmdSelectAggregate is
            when "1---" => return CmdInhibit;
            when "0111" => return NoOp;
            when "0011" => return Active;
            when "0101" => return Read;
            when "0100" => return Write;
            when "0110" => return BurstTerminate;
            when "0010" => return Precharge;
            when "0001" => return Refresh;
            when "0000" => return LoadModeReg;
            when others => return CmdError;
        end case;
    end function decode_cmd;

    pure function encode_cmd(cmd : Cmd_T) return Cmd_Signal_Aggregate_T is
    begin
        case cmd is
            when CmdInhibit     => return "1---";
            when NoOp           => return "0111";
            when Active         => return "0011";
            when Read           => return "0101";
            when Write          => return "0100";
            when BurstTerminate => return "0110";
            when Precharge      => return "0010";
            when Refresh        => return "0001";
            when LoadModeReg    => return "0000";
            when CmdError =>
                report "Invalid command"
                severity error;
        end case;
    end function encode_cmd;

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
