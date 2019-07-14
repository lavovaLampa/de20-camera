library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

package sdram_model_pkg is
    constant BANK_COUNT : natural := 4;

    type State_T is (ACT, A_REF, BST, LMR, NOP, PRECH, READ, READ_A, WRITE, WRITE_A, LOAD_FILE, DUMP_FILE);
    type Command_T is (ActiveEna, ArefEna, BurstTerm, ModeRegEna, PrechargeEna, ReadEna, WriteEna, NoOp, CmdInhibit);
    type Burst_Length_T is ('1', '2', '4', '8', FullPage, InvalidLength);
    type Latency_Mode_T is ('2', '3', InvalidLatency);
    type Array4xBool is array (BANK_COUNT - 1 downto 0) of boolean;
    type Array4xI IS ARRAY (3 DOWNTO 0) OF INTEGER;
    type Array4xT IS ARRAY (3 DOWNTO 0) OF TIME;
    type Array4xB IS ARRAY (3 DOWNTO 0) OF BIT;
    type Array4x2BV IS ARRAY (3 DOWNTO 0) OF BIT_VECTOR(1 DOWNTO 0);
    type Array_state IS ARRAY (4 DOWNTO 0) OF State_T;

    pure function toBool(val : bit) return boolean;
    pure function toBool(val : std_logic) return boolean;
    function To_StdLogic(s : bit) return std_logic;
    function TO_INTEGER(input : std_logic) return integer;
    function TO_INTEGER(input : bit_vector) return integer;
    function TO_INTEGER(input : std_logic_vector) return integer;
    PROCEDURE TO_BITVECTOR(VARIABLE input : IN INTEGER; variable output : out BIT_VECTOR);

END sdram_model_pkg;

PACKAGE BODY sdram_model_pkg IS

    pure function toBool(val : std_logic) return boolean is
    begin
        case val is
            when '1'    => return true;
            when others => return false;
        end case;
    end function toBool;

    function toBool(val : bit) return boolean is
    begin
        case val is
            when '0' => return false;
            when '1' => return true;
        end case;
    end function toBool;

    -- Convert BIT to STD_LOGIC
    FUNCTION To_StdLogic(s : BIT) RETURN STD_LOGIC IS
    BEGIN
        CASE s IS
            WHEN '0'    => RETURN ('0');
            WHEN '1'    => RETURN ('1');
            WHEN OTHERS => RETURN ('0');
        END CASE;
    END;

    -- Convert STD_LOGIC to INTEGER
    FUNCTION TO_INTEGER(input : STD_LOGIC) RETURN INTEGER IS
        VARIABLE result : INTEGER := 0;
        VARIABLE weight : INTEGER := 1;
    BEGIN
        IF input = '1' THEN
            result := weight;
        ELSE
            result := 0;                -- if unknowns, default to logic 0
        END IF;
        RETURN result;
    END TO_INTEGER;

    function to_integer(input : bit_vector) return integer is
        variable tempVal : std_logic_vector(input'range) := to_StdLogicVector(input);
    begin
        return to_integer(unsigned(tempVal));
    end function to_integer;

    -- Convert BIT_VECTOR to INTEGER
    --    FUNCTION TO_INTEGER(input : BIT_VECTOR) RETURN INTEGER IS
    --        VARIABLE result : INTEGER := 0;
    --        VARIABLE weight : INTEGER := 1;
    --    BEGIN
    --        FOR i IN input'LOW TO input'HIGH LOOP
    --            IF input(i) = '1' THEN
    --                result := result + weight;
    --            ELSE
    --                result := result + 0;   -- if unknowns, default to logic 0
    --            END IF;
    --            weight := weight * 2;
    --        END LOOP;
    --        RETURN result;
    --    END TO_INTEGER;

    -- Convert STD_LOGIC_VECTOR to INTEGER
    FUNCTION TO_INTEGER(input : STD_LOGIC_VECTOR) RETURN INTEGER IS
        VARIABLE result : INTEGER := 0;
        VARIABLE weight : INTEGER := 1;
    BEGIN
        FOR i IN input'LOW TO input'HIGH LOOP
            IF input(i) = '1' THEN
                result := result + weight;
            ELSE
                result := result + 0;   -- if unknowns, default to logic 0
            END IF;
            weight := weight * 2;
        END LOOP;
        RETURN result;
    END TO_INTEGER;

    --    function toBitVector(val : integer) return bit_vector 

    -- Conver INTEGER to BIT_VECTOR
    PROCEDURE TO_BITVECTOR(VARIABLE input : IN INTEGER; VARIABLE output : OUT BIT_VECTOR) IS
        VARIABLE work, offset, outputlen, j : INTEGER := 0;
    BEGIN
        --length of vector
        IF output'LENGTH > 32 THEN      --'
            outputlen := 32;
            offset    := output'LENGTH - 32; --'
            IF input >= 0 THEN
                FOR i IN offset - 1 DOWNTO 0 LOOP
                    output(output'HIGH - i) := '0'; --'
                END LOOP;
            ELSE
                FOR i IN offset - 1 DOWNTO 0 LOOP
                    output(output'HIGH - i) := '1'; --'     
                END LOOP;
            END IF;
        ELSE
            outputlen := output'LENGTH; --'
        END IF;
        --positive value
        IF (input >= 0) THEN
            work := input;
            j    := outputlen - 1;
            FOR i IN 1 to 32 LOOP
                IF j >= 0 then
                    IF (work MOD 2) = 0 THEN
                        output(output'HIGH - j - offset) := '0'; --'
                    ELSE
                        output(output'HIGH - j - offset) := '1'; --'
                    END IF;
                END IF;
                work := work / 2;
                j    := j - 1;
            END LOOP;
            IF outputlen = 32 THEN
                output(output'HIGH) := '0'; --'
            END IF;
        --negative value
        ELSE
            work := (-input) - 1;
            j    := outputlen - 1;
            FOR i IN 1 TO 32 LOOP
                IF j >= 0 THEN
                    IF (work MOD 2) = 0 THEN
                        output(output'HIGH - j - offset) := '1'; --'
                    ELSE
                        output(output'HIGH - j - offset) := '0'; --'
                    END IF;
                END IF;
                work := work / 2;
                j    := j - 1;
            END LOOP;
            IF outputlen = 32 THEN
                output(output'HIGH) := '1'; --'
            END IF;
        END IF;
    END TO_BITVECTOR;

END sdram_model_pkg;
