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
        clkIn, rstAsyncIn             : in    std_logic;
        -- ctrl in
        enableInStrobe                : in    boolean;
        dataIn                        : in    I2c_Data_T;
        devAddrIn, dataAddrIn         : in    I2c_Addr_T;
        -- state out
        doneStrobeOut, errorStrobeOut : out   boolean   := false;
        -- i2c i/o
        sClkOut                       : out   std_logic := '1';
        sDataIO                       : inout std_logic := 'Z' -- 1 == 'Z', 0 == '0' (i2c spec)
    );
end entity i2c_ctrl;

architecture rtl of i2c_ctrl is
    -- clock divider counter
    signal clkDivider : unsigned(1 downto 0) := B"11";

    -- i2c i/o state signals
    signal canWriteData, canReadData, canControlBus : boolean;

    -- debug signals
    signal dataBusDbg : std_logic := '0';

    -- i2c data reg
    signal nextData : std_logic := '1';
begin
    -- i2c i/o state
    canWriteData  <= clkDivider = B"00";
    canReadData   <= clkDivider = B"11";
    canControlBus <= clkDivider = B"10";

    -- i2c clk divider out
    sClkOut <= clkDivider(1);

    -- i2c data conversion
    dataBusDbg <= logic_to_i2c(nextData);
    sDataIO    <= logic_to_i2c(nextData);

    ctrlProc : process(clkIn, rstAsyncIn)
        type Ctrl_State_T is (Ready, SendData, ReleaseLine, WaitForAck, SendStop);

        -- ctrl state regs
        variable currState : Ctrl_State_T := Ready;

        -- data state regs
        variable dataAggregate : I2c_Aggregate_T     := (others => '0');
        variable bitPtr        : I2c_Aggregate_Ptr_T := I2c_Aggregate_Ptr_T'high;
    begin
        if rstAsyncIn = '1' then
            currState     := Ready;
            dataAggregate := (others => '0');
            bitPtr        := I2c_Aggregate_Ptr_T'high;

            nextData <= '1';            -- i2c idle state (data + clk both HIGH)

            clkDivider <= (others => '1');

            doneStrobeOut  <= false;
            errorStrobeOut <= false;
        elsif rising_edge(clkIn) then
            -- strobes
            doneStrobeOut  <= false;
            errorStrobeOut <= false;

            -- clk control
            if currState = Ready or currState = SendStop then
                clkDivider <= B"10";
            else
                clkDivider <= clkDivider + 1;
            end if;

            case currState is
                when Ready =>
                    if enableInStrobe then
                        dataAggregate := std_logic_vector(devAddrIn) & std_logic_vector(dataAddrIn) & dataIn;
                        bitPtr        := I2c_Aggregate_Ptr_T'high;

                        -- initiate with a start bit (pull SDA low while clock is high)
                        nextData <= '0';

                        currState := SendData;
                    end if;

                when SendData =>
                    -- data can only update while clock is low
                    -- because data are latched they update on next clock cycle (HIGH -> LOW)
                    if canWriteData then
                        nextData <= dataAggregate(bitPtr);

                        -- after every byte, release line to receive acknowledge bit
                        if bitPtr mod 8 = 0 then
                            currState := ReleaseLine;
                        end if;

                        if bitPtr > 0 then
                            bitPtr := bitPtr - 1;
                        end if;
                    end if;

                when ReleaseLine =>
                    if canWriteData then
                        -- release SDA
                        nextData  <= '1';
                        currState := WaitForAck;
                    end if;

                when WaitForAck =>
                    if canReadData then
                        if sDataIO = '0' then
                            if bitPtr = 0 then
                                nextData  <= '0';
                                currState := SendStop;
                            else
                                currState := SendData;
                            end if;
                        else
                            report "slave didn't acknowledge data" severity error;

                            currState      := SendStop;
                            errorStrobeOut <= true;
                        end if;
                    end if;

                when SendStop =>
                    if canControlBus then
                        nextData <= '1';

                        doneStrobeOut <= true;
                        currState     := Ready;
                    end if;
            end case;
        end if;
    end process ctrlProc;
end architecture rtl;

