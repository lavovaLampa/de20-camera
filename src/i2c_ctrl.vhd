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
    signal fsmState      : FSM_State                                 := Ready;
    signal ackState      : ACK_State                                 := WaitForAck;
    -- data/addr array pointer
    signal bitIndex      : natural range FULL_WIDTH - 1 downto 0     := FULL_WIDTH - 1;
    signal byteCounter   : natural range 7 downto 0                  := 7;
    signal dataAggregate : std_logic_vector(FULL_WIDTH - 1 downto 0) := X"00000000";
    signal clkState      : std_logic                                 := '1';
begin
    controlProc : process(clkIn, rstAsyncIn)
        variable dataOut : std_logic := '1';
    begin
        if rstAsyncIn = '1' then
            dataAggregate <= X"00000000";
            fsmState      <= Ready;
            ackState      <= WaitForAck;
            bitIndex      <= ADDR_WIDTH - 1;
            byteCounter   <= 7;
            doneOut       <= false;
            -- i2c is idle if SDA = SCL = '1'
            clkState      <= '1';
            dataOut       := '1';
        elsif rising_edge(clkIn) then
            case fsmState is
                when Ready =>
                    if enableIn then
                        dataAggregate <= devAddrIn & dataAddrIn & dataIn;
                        fsmState      <= SendData;
                        bitIndex      <= ADDR_WIDTH - 1;
                        ackState      <= WaitForAck;
                        byteCounter   <= 7;
                        -- initiate with a start bit (pull SDA low while clock is high)
                        dataOut       := '0';
                    end if;

                when SendData =>
                    dataOut := dataAggregate(bitIndex);
                    if bitIndex > 0 then
                        bitIndex <= bitIndex - 1;
                    end if;

                    if byteCounter > 0 then
                        byteCounter <= byteCounter - 1;
                    elsif byteCounter = 0 then
                        if ackState = WaitForAck then
                            ackState <= ReleaseLine;
                        elsif ackState = ReleaseLine then
                            dataOut  := '1';
                            ackState <= ReadAck;
                        elsif ackState = ReadAck then
                            if bitIndex = 0 and sDataIO = '0' then
                                dataOut  := '0';
                                fsmState <= SendStop;
                            elsif byteCounter = 0 and sDataIO = '0' then
                                ackState    <= WaitForAck;
                                byteCounter <= 7;
                            else
                                -- TODO: implement error recovery
                                fsmState <= Ready;
                            end if;
                        end if;
                    end if;

                when SendStop =>
                    dataOut  := '1';
                    fsmState <= Ready;
                    doneOut  <= true;
            end case;
        end if;

        -- i2c communication
        sDataIO <= logicToI2CBusState(dataOut);
        sClkOut <= clkState;

    end process controlProc;
end architecture behavioral;

