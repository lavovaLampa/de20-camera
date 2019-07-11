library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ram_ctrl_pkg.all;
use work.sdram_pkg.all;

-- writes/reads only FullPage bursts (256 x 16b)
-- auto refresh when Idle
entity sdram_ctrl is
    port(
        clkIn, rstAsyncIn         : in  std_logic;
        -- ==============================
        -- |    row_addr    | bank_addr |
        -- ==============================
        -- 13              2 1          0
        addrIn                    : in  Ctrl_Addr_T;
        cmdIn                     : in  Ctrl_Cmd_T;
        cmdReadyOut, dataReadyOut : out boolean;
        dataIn                    : in  Data_T;
        dataOut                   : out Data_T
    );
end entity sdram_ctrl;

architecture RTL of sdram_ctrl is
    type Ctrl_State_T is (Idle, BurstStart, Burst, BurstEnd);
    type Ctrl_Addr_R is record
        row  : Addr_T;
        bank : Bank_Addr_T;
    end record Ctrl_Addr_R;

    -- input register
    signal currAddr : Ctrl_Addr_R := (row => (others => '0'), bank => (others => '0'));
    signal currCmd  : Ctrl_Cmd_T  := NoOp;

    -- external signals
    signal ramInitialized : boolean := false;

    -- internal signals
    signal currState      : Ctrl_State_T := Idle;
    signal prechargeBurst : boolean      := true;
begin
    mainProc : process(clkIn, rstAsyncIn)
        pure function addr_to_record(addr : Ctrl_Addr_T) return Ctrl_Addr_R is
        begin
            return (
                row  => addr(addr'high downto BANK_ADDR_WIDTH),
                bank => addr(BANK_ADDR_WIDTH - 1 downto 0)
            );
        end function addr_to_record;

        -- keep last addresses for early bank/row (pre)activation
        variable lastReadAddr, lastWriteAddr : Ctrl_Addr_R := addr_to_record((others => '0'));

        -- helper variables
        variable bankPtr : Bank_Ptr_T := 0;
        variable rowPtr  : Row_Ptr_T  := 0;
    begin
        -- helper variables
        bankPtr := to_integer(addrIn(BANK_ADDR_WIDTH - 1 downto 0));
        rowPtr  := to_integer(addrIn(Ctrl_Addr_T'high downto BANK_ADDR_WIDTH));

        if rstAsyncIn = '1' then
            cmdReadyOut    <= false;
            ramInitialized <= false;
            currState      <= Idle;
            currAddr       <= addr_to_record((others => '0'));
            prechargeBurst <= true;

            lastReadAddr  := addr_to_record((others => '0'));
            lastWriteAddr := addr_to_record((others => '0'));
        elsif rising_edge(clkIn) then
            if ramInitialized then
                case currState is
                    when Idle =>
                        case cmdIn is
                            when Read =>
                                currAddr     <= addr_to_record(addrIn);
                                lastReadAddr := addr_to_record(addrIn);

                            when Write =>
                                currAddr      <= addr_to_record(addrIn);
                                lastWriteAddr := addr_to_record(addrIn);

                            when Refresh =>
                                null;

                            when NoOP =>
                                null;

                        end case;

                    when BurstStart =>
                        null;

                    when Burst =>
                        null;

                    when BurstEnd =>
                        null;

                end case;
            end if;
        end if;
    end process mainProc;

    ramBlock : block
        -- SDRAM I/O
        signal chipSelectNeg, rowAddrStrobeNeg  : std_logic;
        signal colAddrStrobeNeg, writeEnableNeg : std_logic;
        signal sdramAddr                        : Addr_T      := (others => '0');
        signal bankSelect                       : Bank_Addr_T := (others => '0');
        signal sdramData                        : Data_T      := (others => 'Z');

        -- internal data
        signal nextIo     : Mem_IO_Aggregate_R := nop;
        signal encodedCmd : Cmd_Aggregate_R    := encode_cmd(NoOp);
    begin
        -- destructure SDRAM I/O fields
        sdramAddr  <= nextIo.addr;
        bankSelect <= nextIo.bank;
        sdramData  <= nextIo.data;
        encodedCmd <= encode_cmd(nextIo.cmd);

        -- destructure SDRAM command
        chipSelectNeg    <= encodedCmd.chipSelectNeg;
        rowAddrStrobeNeg <= encodedCmd.rowAddrStrobeNeg;
        colAddrStrobeNeg <= encodedCmd.colAddrStrobeNeg;
        writeEnableNeg   <= encodedCmd.writeEnableNeg;

        -- schedule (respect timing constraints) commands
        scheduleProc : process(clkIn, rstAsyncIn)
            type Burst_State_T is (Idle, Precharge, Active, Burst, BurstTerminate, Refresh);
            variable currState, nextState : Burst_State_T         := Idle;
            variable waitCounter          : natural range 0 to 15 := 0;
            variable cmdSent              : boolean               := false;

            -- return command to execute if entered state
            impure function execute_state(state : Burst_State_T) return Mem_IO_Aggregate_R is
            begin
                case state is
                    when Idle           => return nop;
                    when Precharge      => return precharge(currAddr.bank, false);
                    when Active         => return active(currAddr.row, currAddr.bank);
                    when Burst =>
                        if currCmd = Write then
                            -- FIXME: fix data declaration and handling
                            return work.sdram_pkg.write((others => '0'), currAddr.bank, false, dataIn);
                        elsif currCmd = Read then
                            return work.sdram_pkg.read((others => '0'), currAddr.bank, false);
                        end if;
                    when BurstTerminate => return burst_terminate;
                    when Refresh        => return refresh;
                end case;
            end function execute_state;

            -- state -> desired command mapping
            impure function state_to_cmd(state : Burst_State_T) return Cmd_T is
            begin
                case state is
                    when Idle | Burst   => return NoOp;
                    when Refresh        => return Refresh;
                    when Precharge      => return Precharge;
                    when Active         => return Active;
                    when BurstTerminate => return BurstTerminate;
                end case;
            end function state_to_cmd;
        begin
            -- helper variables
            nextState := Burst_State_T'rightof(currState);

            if rstAsyncIn = '1' then
                -- FIXME: handle dataReady flag setting
                dataReadyOut <= false;
                nextIo       <= nop;
                currState    := Idle;
                waitCounter  := 0;
            elsif rising_edge(clkIn) then
                -- send nop command by default
                nextIo <= nop;

                -- handle row activation/precharging + burst termination and
                -- pre-activate commands during bursts
                if ramInitialized then
                    case currState is
                        when Idle =>
                            null;

                        -- TODO: handle Activation of next row
                    when Burst =>
                        if cmdSent then
                            
                        else
                            
                            end if;

                        when BurstTerminate =>
                            if cmdSent then
                                currState := Idle;
                                cmdSent   := false;
                            else
                                waitCounter := cmd_wait_cycles(state_to_cmd(currState));
                                nextIo      <= execute_state(currState);
                                cmdSent     := true;
                            end if;

                        when Refresh =>
                            null;

                        when Precharge | Active =>
                            if cmdSent then
                                if waitCounter = 0 then
                                    currState := nextState;
                                    cmdSent   := false;
                                else
                                    waitCounter := waitCounter - 1;
                                end if;
                            else
                                waitCounter := cmd_wait_cycles(state_to_cmd(currState));
                                nextIo      <= execute_state(currState);
                                cmdSent     := true;
                            end if;
                    end case;
                end if;
            end if;
        end process scheduleProc;

        dataProc : process(clkIn, rstAsyncIn)
        begin
        end process dataProc;
    end block ramBlock;

    cmdReadyOut <= ramInitialized and currState = Idle;
end architecture RTL;
