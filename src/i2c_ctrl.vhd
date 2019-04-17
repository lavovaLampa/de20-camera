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
        enableIn              : in    boolean;
        dataIn                : in    I2c_Data;
        devAddrIn, dataAddrIn : in    I2c_Addr;
        doneOut               : out   boolean   := false;
        sClkOut               : out   std_logic := '1';
        sDataIO               : inout std_logic := 'Z' -- 1 == 'Z', 0 == '0' (i2c spec)
    );
end entity i2c_ctrl;

architecture behavioral of i2c_ctrl is
    signal fsmState      : FSM_State            := Ready;
    -- data/addr array pointer
    signal currBit       : Data_Aggregate_Range := AGGREGATE_WIDTH - 1;
    signal dataAggregate : I2c_Aggregate        := X"00_00_00_00";
    signal clkState      : std_logic            := '1';
    signal retry         : boolean              := false;
begin
    controlProc : process(clkIn, rstAsyncIn)
        variable dataOut : std_logic := '1';
    begin
        if rstAsyncIn = '1' then
            dataAggregate <= X"00_00_00_00";
            fsmState      <= Ready;
            currBit       <= AGGREGATE_WIDTH - 1;
            doneOut       <= false;
            retry         <= false;
            -- i2c idle state (data + clk both HIGH)
            clkState      <= '1';
            dataOut       := '1';
        elsif rising_edge(clkIn) then
            case fsmState is
                when Ready =>
                    doneOut <= false;
                    currBit <= AGGREGATE_WIDTH - 1;
                    retry   <= false;
                    if enableIn or retry then
                        dataAggregate <= devAddrIn & dataAddrIn & dataIn;
                        fsmState      <= SendData;
                        -- initiate with a start bit (pull SDA low while clock is high)
                        clkState      <= '1';
                        dataOut       := '0';
                    end if;

                when SendData =>
                    clkState <= not clkState;

                    -- data can only update while clock is low
                    -- because data are latched they update on next clock cycle (HIGH -> LOW)
                    if clkState = '1' then
                        dataOut := dataAggregate(currBit);
                        currBit <= currBit - 1;

                        if currBit = 24 or currBit = 16 or currBit = 8 or currBit = 0 then
                            fsmState <= ReleaseLine;
                        end if;
                    end if;

                when ReleaseLine =>
                    clkState <= not clkState;

                    if clkState = '1' then
                        -- release SDA
                        dataOut  := '1';
                        fsmState <= WaitForAck;
                    end if;

                when WaitForAck =>
                    clkState <= not clkState;

                    if clkState = '1' then
                        if sDataIO = '0' then
                            if currBit = 0 then
                                dataOut  := '0';
                                fsmState <= SendStop;
                            else
                                -- we have to change data in this clock cycle
                                dataOut  := dataAggregate(currBit);
                                currBit  <= currBit - 1;
                                fsmState <= SendData;
                            end if;
                        else
                            report "slave didn't acknowledge data" severity failure;
                            fsmState <= Ready;
                            retry    <= true;
                        end if;
                    end if;

                when SendStop =>
                    clkState <= '1';

                    if clkState = '1' then
                        dataOut  := '1';
                        fsmState <= Ready;
                        doneOut  <= true;
                    end if;
            end case;

            -- i2c data conversion
            sDataIO <= logicToI2CBusState(dataOut);
        end if;
    end process controlProc;

    sClkOut <= clkState;
end architecture behavioral;

