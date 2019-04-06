library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.i2c_pkg.all;

-- currently supports only writes
entity i2c_ctrl is
    port(
        -- TODO: really, check timing information?
        -- clkIn has to be in range of 10s of kHz (for i2c communication) -> 20kHz 
        clkIn, rstAsyncIn     : in    std_logic;
        enableIn              : in    boolean;
        dataIn                : in    std_logic_vector((DATA_WIDTH - 1) downto 0);
        devAddrIn, dataAddrIn : in    std_logic_vector((ADDR_WIDTH - 1) downto 0);
        doneOut               : out   boolean   := false;
        sClkOut               : out   std_logic := '1';
        sDataIO               : inout std_logic := 'Z' -- 1 == 'Z', 0 == '0' (i2c spec)
    );
end entity i2c_ctrl;

architecture behavioral of i2c_ctrl is
    signal fsmState      : FSM_State                                 := WaitForEnable;
    signal ackState      : ACK_State                                 := WaitForAck;
    -- data/addr array pointer
    signal bitIndex      : natural range FULL_WIDTH - 1 downto 0     := FULL_WIDTH - 1;
    signal byteCounter   : natural range 7 downto 0                  := 7;
    signal dataAggregate : std_logic_vector(FULL_WIDTH - 1 downto 0) := X"00000000";
begin
    controlProc : process(clkIn, rstAsyncIn)
        variable dataOut  : std_logic := '1';
        variable forceClk : std_logic := '1';
    begin
        if rstAsyncIn = '1' then
            dataAggregate <= X"00000000";
            fsmState      <= WaitForEnable;
            ackState      <= WaitForAck;
            bitIndex      <= ADDR_WIDTH - 1;
            byteCounter   <= 7;
            doneOut       <= false;
            dataOut       := '1';
            forceClk      := '1';
        elsif falling_edge(clkIn) then
            case fsmState is
                when WaitForEnable =>
                    if enableIn then
                        dataAggregate <= devAddrIn & dataAddrIn & dataIn;
                        fsmState      <= SendData;
                        bitIndex      <= ADDR_WIDTH - 1;
                        ackState      <= WaitForAck;
                        byteCounter   <= 7;
                        dataOut       := '0';
                        forceClk      := '1';
                    end if;
                when SendData =>
                    forceClk := '0';
                    dataOut  := dataAggregate(bitIndex);
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
                                forceClk := '1';
                                dataOut  := '0';
                                fsmState <= SendStop;
                            elsif byteCounter = 0 and sDataIO = '0' then
                                ackState    <= WaitForAck;
                                byteCounter <= 7;
                            else
                                -- TODO: implement error recovery
                                fsmState <= WaitForEnable;
                            end if;
                        end if;
                    end if;
                when SendStop =>
                    dataOut  := '1';
                    fsmState <= WaitForEnable;
                    doneOut  <= true;
            end case;
        end if;

        sDataIO <= logicToI2CBusState(dataOut);

        -- i2c communication
        sClkOut <= clkIn or forceClk;

    end process controlProc;
end architecture behavioral;

