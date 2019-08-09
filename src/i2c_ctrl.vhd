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
        -- ctrl in
        enableInStrobe        : in    boolean;
        dataIn                : in    I2c_Data_T;
        devAddrIn, dataAddrIn : in    I2c_Addr_T;
        -- state out
        doneOut, errorOut     : out   boolean   := false;
        -- i2c i/o
        sClkOut               : out   std_logic := '1';
        sDataIO               : inout std_logic := 'Z' -- 1 == 'Z', 0 == '0' (i2c spec)
    );
end entity i2c_ctrl;

architecture behavioral of i2c_ctrl is
    type Ctrl_State_T is (Ready, SendData, ReleaseLine, WaitForAck, SendStop);

    -- clock divider counter
    signal clkDivider : unsigned(1 downto 0) := B"11";

    -- state registers
    signal currState     : Ctrl_State_T        := Ready;
    signal currBit       : I2c_Aggregate_Ptr_T := AGGREGATE_WIDTH - 1; -- array current bit pointer
    -- FIXME: migrate to variable
    signal dataAggregate : I2c_Aggregate_T     := X"00_00_00_00";
    signal error         : boolean             := false;

    -- debug signals
    signal dataBusDbg : std_logic := '0';

    -- TODO: rewrite using signals?
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
            currState     <= Ready;
            currBit       <= AGGREGATE_WIDTH - 1;
            doneOut       <= false;
            -- i2c idle state (data + clk both HIGH)
            dataOut       := '1';
        elsif rising_edge(clkIn) then
            if currState = Ready or currState = SendStop then
                clkDivider <= B"10";
            else
                clkDivider <= clkDivider + 1;
            end if;

            case currState is
                when Ready =>
                    doneOut <= false;
                    currBit <= AGGREGATE_WIDTH - 1;
                    error   <= false;
                    if enableInStrobe then
                        dataAggregate <= std_logic_vector(devAddrIn) & std_logic_vector(dataAddrIn) & dataIn;
                        currState     <= SendData;
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
                            currState <= ReleaseLine;
                        end if;
                    end if;

                when ReleaseLine =>
                    if canWriteData then
                        -- release SDA
                        dataOut   := '1';
                        currState <= WaitForAck;
                    end if;

                when WaitForAck =>
                    if canReadData then
                        if sDataIO = '0' then
                            if currBit = 0 then
                                dataOut   := '0';
                                currState <= SendStop;
                            else
                                currState <= SendData;
                            end if;
                        else
                            report "slave didn't acknowledge data" severity failure;
                            currState <= SendStop;
                            error     <= true;
                        end if;
                    end if;

                when SendStop =>
                    if canControlBus then
                        dataOut   := '1';
                        currState <= Ready;
                        doneOut   <= true;
                        error     <= false;
                    end if;
            end case;

            -- i2c data conversion
            dataBusDbg <= logic_to_i2c(dataOut);
            sDataIO    <= logic_to_i2c(dataOut);
        end if;
    end process controlProc;

    errorOut <= error;
    sClkOut  <= clkDivider(1);
end architecture behavioral;

