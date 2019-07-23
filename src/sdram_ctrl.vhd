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
    type Internal_State_T is (Idle, BatchWait, Burst);

    -- SDRAM I/O
    signal nextCmd : Mem_IO_Aggregate_R;

    -- internal registers
    signal bankState   : Bank_State_Array_T    := (others => (active => false, row => (others => '0')));
    signal burstState  : Burst_State_R         := (inBurst => false, counter => 0, precharge => false);
    signal currState   : Internal_State_T      := Idle;
    signal scheduler   : Scheduler_R           := (cmd => NoOp, addr => addr_to_record((others => '0')), startBurst => false, isScheduled => false);
    signal waitCounter : natural range 0 to 10 := 0;

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
        dqm          => (others => '0'),
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
                    nextCmd            <= read((others => '0'), currAddr.bank, false);
                    -- FIXME: not final
                    burstState.counter <= 2**COL_ADDR_WIDTH - 1 + tCAS;

                when Write =>
                    nextCmd            <= write((others => '0'), currAddr.bank, false);
                    sdramDataIo        <= dataIn;
                    burstState.counter <= 2**COL_ADDR_WIDTH - 1;
            end case;

            burstState.inBurst   <= true;
            burstState.precharge <= false;
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
            burstState <= (inBurst => false, counter => 0, precharge => false);
            currState  <= Idle;

            prefetchData := (others => (lastAddr => addr_to_record((others => '0')), cmdCounter => 0, isPrefetched => false));
        elsif rising_edge(clkIn) then
            -- signals only active for one clock
            nextCmd     <= nop;
            sdramDataIo <= (others => 'Z');

            -- update bank_state according to currently scheduled sdram cmd batch
            update_bank_state(nextCmd.cmd, nextCmd.addr, nextCmd.bank);

            if memInitializedIn then
                -- burst counter and data handling
                if burstState.inBurst then
                    if currCmd = Write then
                        sdramDataIo <= dataIn;
                    elsif currCmd = Read then
                        dataOut <= sdramDataIo;
                    end if;

                    if burstState.counter > 0 then
                        burstState.counter <= burstState.counter - 1;
                    else
                        burstState.inBurst <= false;
                    end if;
                end if;

                -- sdram cmd & state handling
                case currState is
                    when Idle =>
                        currAddr := addr_to_record(addrIn);
                        currCmd  := cmdIn;
                        bankPtr  := to_integer(currAddr.bank);

                        if cmdIn /= NoOp then
                            Log(CTRL_ALERT_ID, "Received command: " & to_string(cmdIn), INFO);
                        end if;

                        case cmdIn is
                            when Read | Write =>
                                if bankState(bankPtr).row = currAddr.row and bankState(bankPtr).active then
                                    burst_start(cmdIn);
                                    currState <= Burst;
                                else
                                    schedule_bank_activation(currAddr, true);
                                    currState <= BatchWait;
                                end if;

                                prefetchData(cmdIn)                     := (
                                    lastAddr     => currAddr,
                                    cmdCounter   => prefetchData(cmdIn).cmdCounter + 1,
                                    isPrefetched => false
                                );
                                prefetchData(next_op(cmdIn)).cmdCounter := 0;

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
                        elsif burstState.counter = 0 then
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

                    when BatchWait =>
                        if waitCounter <= 0 and not scheduler.isScheduled and not (scheduler.startBurst or burstState.inBurst) then
                            cmdReadyOut <= true;
                        end if;

                    when Burst =>
                        if burstState.counter <= 0 then
                            cmdReadyOut <= true;
                        end if;
                end case;
            end if;
        end if;
    end process readyFlagProc;
end architecture RTL;
