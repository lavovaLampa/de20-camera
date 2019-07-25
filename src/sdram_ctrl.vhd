library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.sdram_ctrl_pkg.all;
use work.sdram_pkg.all;
use work.sdram_model_pkg.to_safe_natural;

library osvvm;
context osvvm.OsvvmContext;

entity sdram_ctrl is
    generic(
        ROW_MAX   : natural     := 1800;
        BURST_LEN : Burst_Len_T := (
            Read  => 5,
            Write => 4
        )
    );
    port(
        clkIn, rstAsyncIn         : in    std_logic;
        -- ==============================
        -- |    row_addr    | bank_addr |
        -- ==============================
        -- 13              2 1          0
        addrIn                    : in    Ctrl_Addr_T;
        cmdIn                     : in    Ctrl_Cmd_T;
        cmdReadyOut, dataReadyOut : out   boolean;
        dataIn                    : in    Data_T;
        dataOut                   : out   Data_T;
        -- init controller I/O
        memInitializedIn          : in    boolean;
        -- SDRAM I/O
        sdramOut                  : out   Mem_IO_R;
        sdramDataIo               : inout Data_T := (others => 'Z')
    );
    constant CTRL_ALERT_ID : AlertLogIDType := GetAlertLogID("CTRL MEM", ALERTLOG_BASE_ID);
end entity sdram_ctrl;

architecture RTL of sdram_ctrl is
    type Internal_State_T is (Idle, ReadToWriteWait, BatchWait, Burst);

    -- SDRAM I/O
    signal nextCmd : Mem_IO_Aggregate_R;
    signal dqm     : Dqm_T;

    -- internal registers
    signal bankState   : Bank_State_Array_T    := (others => (active => false, row => (others => '0')));
    signal burstState  : Burst_State_R         := (inBurst => false, counter => 0, precharge => false, burstType => Write);
    signal currState   : Internal_State_T      := Idle;
    signal scheduler   : Scheduler_R           := (cmd => NoOp, addr => addr_to_record((others => '0')), startBurst => false, isScheduled => false);
    signal waitCounter : natural range 0 to 10 := 0;
    signal internalOp  : boolean               := false;

    -- debug signals
    signal dbgLastAddr     : Ctrl_Addr_R;
    signal dbgLastCmd      : Ctrl_Cmd_T;
    signal dbgBankPtr      : Bank_Ptr_T;
    signal dbgPrefetchData : Prefetch_Array_T;
begin
    -- pack sdram signals
    sdramOut <= (
        addr         => nextCmd.addr,
        bankSelect   => nextCmd.bank,
        cmdAggregate => encode_cmd(nextCmd.cmd),
        dqm          => dqm,
        clkEnable    => '1'
    );

    mainProc : process(clkIn, rstAsyncIn)
        impure function all_banks_precharged return boolean is
            variable retval : boolean := true;
        begin
            for i in 0 to BANK_COUNT - 1 loop
                retval := retval and not bankState(i).active;
            end loop;
            return retval;
        end function all_banks_precharged;

        -- reg
        variable currAddr     : Ctrl_Addr_R      := addr_to_record((others => '0'));
        variable currCmd      : Ctrl_Cmd_T       := NoOp;
        variable bankPtr      : Bank_Ptr_T       := 0;
        --        variable scheduledCmd : Scheduler_R            := (cmd => NoOp, addr => addr_to_record((others => '0')), startBurst => false, isScheduled => false);
        --        variable waitCounter  : integer range -2 to 10 := 10;
        variable prefetchData : Prefetch_Array_T := (others => (lastAddr => addr_to_record((others => '0')), cmdCounter => 0, isPrefetched => false));

        -- scheduled bank activation (e.g. if wrong row is active, first Precharge, then Activate)
        procedure schedule_bank_activation(addr : in Ctrl_Addr_R; startBurst : in boolean) is
            variable currBank : Bank_Ptr_T := to_integer(addr.bank);
        begin
            if not bankState(currBank).active then
                nextCmd     <= active(addr.row, addr.bank);
                waitCounter <= cmd_delay(Active) - 1;
            else
                nextCmd     <= precharge(addr.bank, false);
                waitCounter <= cmd_delay(Precharge) - 1;
                -- schedule row activation after we precharge the old one
                scheduler   <= (
                    cmd         => Active,
                    addr        => addr,
                    startBurst  => startBurst,
                    isScheduled => true
                );
            end if;

            scheduler.startBurst <= startBurst;
        end procedure schedule_bank_activation;

        -- start a burst and setup burst state
        procedure burst_start(burstType : in Op_T) is
        begin
            case burstType is
                when Read =>
                    nextCmd <= read((others => '0'), currAddr.bank, false);
                when Write =>
                    nextCmd     <= write((others => '0'), currAddr.bank, false);
                    sdramDataIo <= dataIn;
            end case;

            burstState <= (
                counter   => 2**COL_ADDR_WIDTH - 1,
                inBurst   => true,
                precharge => false,
                burstType => burstType
            );
        end procedure burst_start;

        -- return whether we should try to early activate this address' bank/row
        impure function should_prefetch_addr(thisAddr, otherAddr : in Ctrl_Addr_R; thisOp : Op_T) return boolean is
            variable currBank       : Bank_Ptr_T := to_integer(thisAddr.bank);
            variable shouldPrefetch : boolean    := true;
            variable otherOp        : Op_T       := next_op(thisOp);
        begin
            if thisAddr.bank = otherAddr.bank then
                if thisAddr.row /= otherAddr.row then
                    shouldPrefetch := (currCmd = thisOp and prefetchData(thisOp).cmdCounter < BURST_LEN(thisOp)) or (currCmd = otherOp and prefetchData(otherOp).cmdCounter >= BURST_LEN(otherOp));
                end if;
            end if;

            --            Log(CTRL_ALERT_ID, to_string(BURST_LEN(thisOp)), DEBUG);
            --            Log(CTRL_ALERT_ID, to_string(currCmd), DEBUG);
            --            Log(CTRL_ALERT_ID, to_string(thisOp) & " cmd counter = " & to_string(prefetchData(thisOp).cmdCounter), DEBUG);
            Log(CTRL_ALERT_ID, "Should prefetch " & to_string(thisOp) & ", Bank: " & to_string(thisAddr.bank) & ", Row: " & to_hstring(thisAddr.row) & " = " & to_string(shouldPrefetch), DEBUG);

            return shouldPrefetch and not (bankState(currBank).active and bankState(currBank).row = thisAddr.row);
        end function should_prefetch_addr;

        -- try to queue row/bank activation based on next predicted address
        procedure schedule_addr_prefetch(thisAddr, otherAddr : in Ctrl_Addr_R; thisOp : Op_T) is
            variable shouldPrefetchAddr : boolean := should_prefetch_addr(thisAddr, otherAddr, thisOp);
        begin
            -- if next predicted bank is the same as the one being bursted
            -- either keep it open or precharge it at the end of the burst
            if shouldPrefetchAddr then
                if thisAddr.bank = currAddr.bank then
                    if thisAddr.row /= currAddr.row then
                        burstState.precharge <= true;
                    end if;
                -- else just keep the row open after burst end
                else
                    schedule_bank_activation(thisAddr, false);
                end if;
            end if;
        end procedure schedule_addr_prefetch;

        procedure update_bank_state(cmd : in Cmd_T; rowAddr : in Addr_T; bankAddr : in Bank_Addr_T) is
            variable currBank : Bank_Ptr_T := to_safe_natural(bankAddr);
        begin
            case cmd is
                when Active =>
                    bankState(currBank) <= (
                        active => true,
                        row    => rowAddr
                    );

                when Precharge =>
                    if rowAddr(10) = '1' then
                        for i in Bank_Ptr_T loop
                            bankState(i).active <= false;
                        end loop;
                    else
                        bankState(currBank).active <= false;
                    end if;

                when others =>
                    null;
            end case;
        end procedure update_bank_state;
    begin
        if rstAsyncIn = '1' then
            -- by default mask data
            dqm        <= (others => '1');
            burstState <= (inBurst => false, counter => 0, precharge => false, burstType => Read);
            currState  <= Idle;
            internalOp <= false;

            prefetchData := (others => (lastAddr => addr_to_record((others => '0')), cmdCounter => 0, isPrefetched => false));
        elsif rising_edge(clkIn) then
            -- signals only active for one clock
            nextCmd     <= nop;
            sdramDataIo <= (others => 'Z');
            internalOp  <= false;
            dqm         <= (others => '1');

            -- update bank_state according to currently scheduled sdram cmd batch
            update_bank_state(nextCmd.cmd, nextCmd.addr, nextCmd.bank);

            if memInitializedIn then
                -- decrement burst counter
                if burstState.counter > -tCAS then
                    burstState.counter <= burstState.counter - 1;
                end if;

                -- burst state and data handling
                if burstState.inBurst then
                    if currCmd = Write then
                        sdramDataIo <= dataIn;
                        if burstState.counter > 0 then
                            dqm <= (others => '0');
                        end if;
                    elsif currCmd = Read then
                        dataOut <= sdramDataIo;
                        if burstState.counter > -tCAS + tDQZ then
                            dqm <= (others => '0');
                        end if;
                    end if;

                    if burstState.counter > -tCAS + tDQZ then
                        dqm <= (others => '0');
                    end if;

                    if burstState.counter = 0 then -- end burst for write burst at 0
                        -- cmd can be overridden by state handler (e.g. read/write burst succeeded by another read/write burst)
                        if burstState.precharge then
                            -- FIXME: cannot instantly precharge after write burst - tRDL
                            if burstState.burstType = Read then
                                nextCmd <= precharge(currAddr.bank, false);
                            else
                                nextCmd <= burst_terminate;
                            end if;
                        else
                            nextCmd <= burst_terminate;
                        end if;
                    elsif burstState.counter <= -tCAS then -- factor in tCAS latency for read bursts
                        burstState.inBurst <= false;
                    end if;
                end if;

                -- sdram cmd & state handling
                case currState is
                    when Idle =>
                        -- if next operation is generated internally, don't update registers to external values
                        if not internalOp then
                            currAddr := addr_to_record(addrIn);
                            currCmd  := cmdIn;
                            bankPtr  := to_integer(currAddr.bank);
                        end if;

                        -- log currently requested operation
                        if currCmd /= NoOp then
                            Log(CTRL_ALERT_ID, "Received command: " & to_string(currCmd), INFO);
                        end if;

                        case currCmd is
                            when Read | Write =>
                                if currCmd = Write and burstState.burstType = Read and burstState.inBurst and burstState.counter > -tCAS then
                                    currState <= ReadToWriteWait;
                                else
                                    if bankState(bankPtr).row = currAddr.row and bankState(bankPtr).active then
                                        burst_start(currCmd);
                                        currState <= Burst;
                                    else
                                        schedule_bank_activation(currAddr, true);
                                        currState <= BatchWait;
                                    end if;

                                    prefetchData(currCmd)                     := (
                                        lastAddr     => currAddr,
                                        cmdCounter   => prefetchData(currCmd).cmdCounter + 1,
                                        isPrefetched => false
                                    );
                                    prefetchData(next_op(currCmd)).cmdCounter := 0;
                                end if;

                            when Refresh =>
                                currState <= BatchWait;

                                if all_banks_precharged then
                                    nextCmd     <= refresh;
                                    waitCounter <= cmd_delay(Refresh) - 1;
                                else
                                    nextCmd     <= precharge((others => '-'), true);
                                    waitCounter <= cmd_delay(Precharge) - 1;
                                end if;

                            when NoOp =>
                                null;
                        end case;

                    -- if we want to immediately start a write burst after a read burst,
                    -- we have to wait until a tCAS penalty is over (reads data output shifted by tCAS)
                    when ReadToWriteWait =>
                        -- add state change penalty
                        if burstState.counter <= -tCAS + 1 then
                            currState  <= Idle;
                            internalOp <= true;
                        end if;

                    -- wait for current batch and/or start burst
                    when BatchWait =>
                        if waitCounter <= 0 then
                            if scheduler.isScheduled then
                                waitCounter           <= cmd_delay(scheduler.cmd) - 1;
                                scheduler.isScheduled <= false;
                                case scheduler.cmd is
                                    when Active    => nextCmd <= active(scheduler.addr.row, scheduler.addr.bank);
                                    when Precharge => nextCmd <= precharge(scheduler.addr.bank, false);
                                    when Refresh   => nextCmd <= refresh;
                                    when others =>
                                        report "Unsupported command"
                                        severity error;
                                end case;
                            else
                                if scheduler.startBurst then
                                    scheduler.startBurst <= false;
                                    burst_start(currCmd);

                                    currState <= Burst;
                                elsif burstState.inBurst then
                                    currState <= Burst;
                                else
                                    currState <= Idle;
                                end if;
                            end if;
                        else
                            waitCounter <= waitCounter - 1;
                        end if;

                    when Burst =>
                        if not prefetchData(Read).isPrefetched then
                            prefetchData(Read).isPrefetched := true;
                            schedule_addr_prefetch(next_row_addr(prefetchData(Read).lastAddr, ROW_MAX), next_row_addr(prefetchData(Write).lastAddr, ROW_MAX), Read);
                            currState                       <= BatchWait;

                        elsif not prefetchData(Write).isPrefetched then
                            prefetchData(Write).isPrefetched := true;
                            schedule_addr_prefetch(next_row_addr(prefetchData(Write).lastAddr, ROW_MAX), next_row_addr(prefetchData(Read).lastAddr, ROW_MAX), Write);
                            currState                        <= BatchWait;

                        -- subtract state change penalty (state register takes new value only after next rising_edge)
                        elsif burstState.counter = 1 then
                            -- reset prefetch state
                            for i in Op_T loop
                                prefetchData(i).isPrefetched := false;
                            end loop;

                            currState <= Idle;
                        end if;
                end case;
            end if;
        end if;

        -- debug signals
        dbgBankPtr      <= bankPtr;
        dbgLastAddr     <= currAddr;
        dbgLastCmd      <= currCmd;
        dbgPrefetchData <= prefetchData;
    end process mainProc;

    readyFlagProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            cmdReadyOut <= false;
        elsif rising_edge(clkIn) then
            if memInitializedIn then
                case currState is
                    when Idle =>
                        case cmdIn is
                            when NoOp =>
                                cmdReadyOut <= true;

                            when Read | Write | Refresh =>
                                cmdReadyOut <= false;
                        end case;

                    when ReadToWriteWait =>
                        null;

                    when BatchWait =>
                        if waitCounter <= 0 and not scheduler.isScheduled and not (scheduler.startBurst or burstState.inBurst) then
                            cmdReadyOut <= true;
                        end if;

                    when Burst =>
                        if burstState.counter <= 1 then
                            cmdReadyOut <= true;
                        end if;
                end case;
            end if;
        end if;
    end process readyFlagProc;

    dataFlagProc : process(clkIn, rstAsyncIn)
        -- alternative counter shifted by CAS cycles (for read bursts)
        variable burstCounter : natural range 0 to 2**COL_ADDR_WIDTH - 1 := 0;
    begin
        if rstAsyncIn = '1' then
        elsif rising_edge(clkIn) then

        end if;
    end process dataFlagProc;
end architecture RTL;
