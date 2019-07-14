library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ram_ctrl_pkg.all;
use work.sdram_pkg.all;

-- writes/reads only FullPage bursts (256 x 16b)
-- auto refresh when Idle
entity sdram_ctrl is
    generic(
        ROW_MAX : natural := 1800
    );
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
    -- state registers
    signal bankState  : Bank_State_Array_T := (others => (active => false, row => (others => '0')));
    signal burstState : Burst_State_R      := (bursting => false, counter => 0, bank => (others => '0'), reading => false);
    signal currAddr   : Ctrl_Addr_R        := addr_to_record((others => '0'));

    -- SDRAM I/O
    signal chipSelectNeg, rowAddrStrobeNeg  : std_logic;
    signal colAddrStrobeNeg, writeEnableNeg : std_logic;
    signal sdramAddr                        : Addr_T          := (others => '0');
    signal bankSelect                       : Bank_Addr_T     := (others => '0');
    signal sdramData                        : Data_T          := (others => 'Z');
    -- helpers
    signal encodedCmd                       : Cmd_Aggregate_R := encode_cmd(NoOp);

    -- external signals
    signal ramInitialized : boolean := false;

    -- internal signals
    signal schedulerIn  : Scheduler_In_R;
    signal schedulerOut : Scheduler_Out_R;
    signal memOut       : Mem_IO_Aggregate_R := nop;
    signal nextIo       : Mem_IO_Aggregate_R := nop;
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

    mainProc : process(clkIn, rstAsyncIn, addrIn)
        type Ctrl_State_T is (Idle, BatchWait, Burst);

        variable currState                   : Ctrl_State_T := Idle;
        -- keep last address for early bank/row activation/precharge
        variable lastReadAddr, lastWriteAddr : Ctrl_Addr_R  := addr_to_record((others => '0'));
        variable currCmd                     : Ctrl_Cmd_T   := NoOp;

        -- helper variables
        variable addrTmp : Ctrl_Addr_R := addr_to_record((others => '0'));

        procedure schedule_cmd(cmd : in Ctrl_Cmd_T; addr : in Ctrl_Addr_R) is
            variable currBank        : Bank_Ptr_T           := to_integer(addr.bank);
            variable opCount         : natural range 0 to 2 := 0;
            variable banksPrecharged : boolean              := true;
        begin
            case cmd is
                when NoOp => null;
                when Read | Write =>
                    -- if bank is not Active or wrong row is still open, queue its activation
                    if not bankState(currBank).active or bankState(currBank).row /= addr.row then
                        schedulerIn.cmdArray(0) <= Active;
                        opCount                 := opCount + 1;
                    end if;

                    -- if bank has wrong row active, queue its Precharge first
                    if bankState(currBank).active and bankState(currBank).row /= addr.row then
                        schedulerIn.cmdArray(1) <= Precharge;
                        opCount                 := opCount + 1;
                    end if;

                    schedulerIn.arrayPtr <= opCount;

                when Refresh =>
                    -- if all banks are not idle, we have to precharge them before refresh
                    for i in 0 to BANK_COUNT - 1 loop
                        banksPrecharged := banksPrecharged and not bankState(i).active;
                    end loop;

                    schedulerIn.cmdArray(0) <= Refresh;

                    if not banksPrecharged then
                        schedulerIn.cmdArray(1) <= PrechargeAll;
                        opCount                 := opCount + 1;
                    end if;

                    schedulerIn.arrayPtr <= opCount;
            end case;

            schedulerIn.batchExecute <= true;
        end procedure schedule_cmd;

        impure function is_row_open(addr : Ctrl_Addr_R) return boolean is
            variable bankPtr : Bank_Ptr_T := to_integer(addr.bank);
        begin
            return bankState(bankPtr).active and bankState(bankPtr).row = addr.row;
        end function is_row_open;

        pure function decide_prefetch(tmp : boolean) return boolean is
        begin
            return true;
        end function decide_prefetch;

        procedure schedule_bank_prefetch(thisAddr, otherAddr : in Ctrl_Addr_T) is
            variable thisNext     : Ctrl_Addr_R := addr_to_record(next_row_addr(thisAddr, ROW_MAX));
            variable otherNext    : Ctrl_Addr_R := addr_to_record(next_row_addr(otherAddr, ROW_MAX));
            variable nextBankPtr  : Bank_Ptr_T  := to_integer(thisNext.bank);
            variable prefetchThis : boolean     := true;
        begin
            if thisNext.bank = otherNext.bank and thisNext.row /= otherNext.row then
                -- TODO: decide a tie
            end if;

            -- check if we don't collide with currently bursting row/bank combination
            if prefetchThis then
                if thisNext.bank = burstState.bank then
                    if bankState(nextBankPtr).row /= thisNext.row then
                        burstPrecharge <= true;
                    end if;
                else
                    schedulerIn <= (
                        arrayPtr => 1,
                        cmdArray => (Precharge, Precharge, Active),
                        row      => thisNext.row,
                        bank     => thisNext.bank
                    );
                end if;
            end if;
        end procedure schedule_bank_prefetch;
    begin
        addrTmp := addr_to_record(addrIn);

        if rstAsyncIn = '1' then
            cmdReadyOut <= false;

            schedulerIn <= (
                arrayPtr     => 0,
                cmdArray     => (others => Refresh),
                addr         => addr_to_record((others => '0')),
                batchExecute => false
            );
            memOut      <= nop;

            currState     := Idle;
            lastReadAddr  := addr_to_record((others => '0'));
            lastWriteAddr := addr_to_record((others => '0'));
        elsif rising_edge(clkIn) then
            -- default values
            schedulerIn.batchExecute <= false;
            memOut                   <= nop;

            if ramInitialized then
                case currState is
                    when Idle =>
                        currCmd  := cmdIn;
                        currAddr <= addrTmp;

                        case cmdIn is
                            when NoOp =>
                                null;

                            when Read =>
                                lastReadAddr := addrTmp when cmdIn = Read;
                                if is_row_open(addrTmp) then
                                    currState  := Burst;
                                    memOut     <= read((others => '0'), addrTmp.bank, false);
                                    burstState <= (
                                        bursting => true,
                                        counter  => 2**COL_ADDR_WIDTH - 1,
                                        bank     => currAddr.bank,
                                        reading  => currCmd = Read
                                    );
                                else
                                    currState := BatchWait;
                                    schedule_cmd(Write, addrTmp);
                                end if;

                            when Write =>
                                lastWriteAddr := addrTmp when cmdIn = Write;
                                if is_row_open(addrTmp) then
                                    currState    := Burst;
                                    memOut       <= write((others => '0'), addrTmp.bank, false, dataIn);
                                    dataReadyOut <= true;
                                    burstState   <= (
                                        bursting => true,
                                        counter  => 2**COL_ADDR_WIDTH - 1,
                                        bank     => currAddr.bank,
                                        reading  => currCmd = Read
                                    );
                                else
                                    currState := BatchWait;
                                    schedule_cmd(Write, addrTmp);
                                end if;

                            when Refresh =>
                                currState := BatchWait;
                                schedule_cmd(Refresh, addrTmp);
                        end case;

                        cmdReadyOut <= cmdIn = NoOp;

                    when BatchWait =>
                        if schedulerOut.batchDone then
                            if currCmd = Refresh then
                                currState := Idle;
                            else
                                currState  := Burst;
                                burstState <= (
                                    bursting => true,
                                    counter  => 2**COL_ADDR_WIDTH - 1,
                                    bank     => currAddr.bank,
                                    reading  => currCmd = Read
                                );
                            end if;
                        end if;

                    when Burst =>
                        if burstState.counter = 0 then

                        else
                            
                            end if;

                end case;
            end if;
        end if;
    end process mainProc;

    -- schedule (respect timing constraints) commands
    schedulerProc : process(clkIn, rstAsyncIn, schedulerIn, currAddr)
        type Internal_State_T is (Idle, ScheduleOp);

        -- state variables
        variable currState   : Internal_State_T      := Idle;
        variable arrayPtr    : integer range -1 to 2 := 0;
        variable waitCounter : natural range 0 to 15 := 0;

        -- helper variables
        variable currSchedOp  : Scheduler_Cmd_T;
        variable encodedMemOp : Mem_IO_Aggregate_R;

        procedure update_bank_state(cmd : in Scheduler_Cmd_T; row : in Addr_T; bank : in Bank_Addr_T) is
            variable bankPtr : Bank_Ptr_T := to_integer(bank);
        begin
            case cmd is
                -- these commands do not change state of bank
                when Refresh   => null;
                when Active    => bankState(bankPtr) <= (active => true, row => row);
                when Precharge => bankState(bankPtr).active <= false;
                when PrechargeAll =>
                    for i in 0 to BANK_COUNT - 1 loop
                        bankState(i).active <= false;
                    end loop;
            end case;
        end procedure update_bank_state;

        impure function encode_curr_cmd(cmd : Scheduler_Cmd_T; row : Addr_T; bank : Bank_Addr_T) return Mem_IO_Aggregate_R is
        begin
            case cmd is
                when Active       => return active(row, bank);
                when Precharge    => return precharge(bank, false);
                when PrechargeAll => return precharge((others => '-'), true);
                when Refresh      => return refresh;
            end case;
        end function encode_curr_cmd;
    begin
        -- currently pointed operation in array
        currSchedOp  := schedulerIn.cmdArray(arrayPtr);
        -- encoded sdram command according to current operation
        encodedMemOp := encode_curr_cmd(currSchedOp, currAddr.row, currAddr.bank);
        -- data delay during read

        if rstAsyncIn = '1' then
            schedulerOut.memIo <= nop;
            bankState          <= (others => (active => false, row => (others => '0')));

            currState   := Idle;
            arrayPtr    := 0;
            waitCounter := 0;
        elsif rising_edge(clkIn) then
            -- send nop command by default
            schedulerOut.memIo     <= nop;
            -- only signal batch completion for 1 clock
            schedulerOut.batchDone <= false;

            if ramInitialized then
                case currState is
                    when Idle =>
                        arrayPtr := schedulerIn.arrayPtr;

                        if schedulerIn.batchExecute then
                            assert not burstState.bursting or burstState.counter > 50
                            report "Cannot start any operation right before burst end!"
                            severity error;

                            assert not (burstState.bursting and currSchedOp = Refresh)
                            report "Cannot perform " & Scheduler_Cmd_T'image(currSchedOp) & " during read/write burst!"
                            severity error;

                            schedulerOut.memIo <= encodedMemOp;
                            waitCounter        := cmd_wait_cycles(encodedMemOp.cmd);
                            currState          := ScheduleOp;
                        end if;

                    when ScheduleOp =>
                        if waitCounter = 0 then
                            update_bank_state(currSchedOp, currAddr.row, currAddr.bank);
                            if arrayPtr = 0 then
                                currState              := Idle;
                                schedulerOut.batchDone <= true;
                            else
                                currState := ScheduleOp;
                                arrayPtr  := arrayPtr - 1;

                                assert not (burstState.bursting and currSchedOp = Refresh)
                                report "Cannot perform " & Scheduler_Cmd_T'image(currSchedOp) & " during read/write burst!"
                                severity error;

                                schedulerOut.memIo <= encodedMemOp;
                                waitCounter        := cmd_wait_cycles(encodedMemOp.cmd);
                            end if;
                        else
                            waitCounter := waitCounter - 1;
                        end if;
                end case;
            end if;
        end if;
    end process schedulerProc;
end architecture RTL;
