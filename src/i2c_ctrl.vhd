library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.i2c_pkg.all;

-- currently supports only writes
entity i2c_ctrl is
    port(
        -- TODO: really, check timing information?
        -- clkIn has to be in range of 10s of kHz (for i2c communication) -> 20kHz 
        -- clkIn has to have 2 * sClkOut HZ (because we change sClk on each rising edge only)
        clkIn, rstAsyncIn     : in    std_logic;
        -- DATA IN
        enableIn              : in    boolean;
        dataIn                : in    I2c_Data;
        devAddrIn, dataAddrIn : in    I2c_Addr;
        -- DATA OUT
        doneOut, errorOut     : out   boolean   := false;
        -- I2C COMMS
        sClkOut               : out   std_logic := '1';
        sDataIO               : inout std_logic := 'Z' -- 1 == 'Z', 0 == '0' (i2c spec)
    );
end entity i2c_ctrl;

architecture behavioral of i2c_ctrl is
    signal clkDivider    : unsigned(1 downto 0) := B"11";
    signal fsmState      : FSM_State            := Ready;
    -- data/addr array pointer
    signal currBit       : Data_Aggregate_Range := AGGREGATE_WIDTH - 1;
    signal dataAggregate : I2c_Aggregate        := X"00_00_00_00";
    signal error         : boolean              := false;
    signal dbg           : std_logic            := '0';

    impure function canWriteData return boolean is
    begin
        return clkDivider = B"00";
    end function canWriteData;

    impure function canReadData return boolean is
    begin
        return clkDivider = B"11";
    end function canReadData;

    impure function canControlBus return boolean is
    begin
        return clkDivider = B"10";
    end function canControlBus;
begin
    controlProc : process(clkIn, rstAsyncIn)
        variable dataOut : std_logic := '1';
    begin
        if rstAsyncIn = '1' then
            dataAggregate <= X"00_00_00_00";
            fsmState      <= Ready;
            currBit       <= AGGREGATE_WIDTH - 1;
            doneOut       <= false;
            -- i2c idle state (data + clk both HIGH)
            dataOut       := '1';
        elsif rising_edge(clkIn) then
            if fsmState = Ready or fsmState = SendStop then
                clkDivider <= B"10";
            else
                clkDivider <= clkDivider + 1;
            end if;

            case fsmState is
                when Ready =>
                    doneOut <= false;
                    currBit <= AGGREGATE_WIDTH - 1;
                    error   <= false;
                    if enableIn then
                        dataAggregate <= devAddrIn & dataAddrIn & dataIn;
                        fsmState      <= SendData;
                        -- initiate with a start bit (pull SDA low while clock is high)
                        dataOut       := '0';
                    end if;

                when SendData =>
                    -- data can only update while clock is low
                    -- because data are latched they update on next clock cycle (HIGH -> LOW)
                    if canWriteData then
                        dataOut := dataAggregate(currBit);
                        if currBit > 0 then
                            currBit <= currBit - 1;
                        end if;

                        if currBit mod 8 = 0 then
                            fsmState <= ReleaseLine;
                        end if;
                    end if;

                when ReleaseLine =>
                    if canWriteData then
                        -- release SDA
                        dataOut  := '1';
                        fsmState <= WaitForAck;
                    end if;

                when WaitForAck =>
                    if canReadData then
                        if sDataIO = '0' then
                            if currBit = 0 then
                                dataOut  := '0';
                                fsmState <= SendStop;
                            else
                                fsmState <= SendData;
                            end if;
                        else
                            report "slave didn't acknowledge data" severity failure;
                            fsmState <= SendStop;
                            error    <= true;
                        end if;
                    end if;

                when SendStop =>
                    if canControlBus then
                        dataOut  := '1';
                        fsmState <= Ready;
                        doneOut  <= true;
                        error    <= false;
                    end if;
            end case;

            -- i2c data conversion
            dbg <= logicToI2CBusState(dataOut);
            sDataIO <= logicToI2CBusState(dataOut);
        end if;
    end process controlProc;

    errorOut <= error;
    sClkOut  <= clkDivider(1);
end architecture behavioral;

