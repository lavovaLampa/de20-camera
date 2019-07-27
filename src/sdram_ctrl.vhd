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
    type Internal_State_T is (Idle, ExecutePlan, Burst);

    -- SDRAM I/O
    signal nextCmd : Mem_IO_Aggregate_R;
    signal dqm     : Dqm_T;

    -- internal registers
    signal bankState   : Bank_State_Array_T    := (others => (active => false, row => (others => '0')));
    signal burstState  : Burst_State_R         := (inBurst => false, counter => 0, burstType => Write, interleavedRead => false);
    signal currState   : Internal_State_T      := Idle;
    signal currPlan    : Execution_Plan_R      := (addr => addr_to_record((others => '0')), cmdPlan => (others => Precharge), cmdPtr => 0, waitForBurstEnd => false);
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
        dqm          => dqm,
        clkEnable    => '1'
    );

    mainProc : process(clkIn, rstAsyncIn)
        -- reg
        variable currAddr, lastAddr : Ctrl_Addr_R      := addr_to_record((others => '0'));
        variable currCmd            : Ctrl_Cmd_T       := NoOp;
        variable bankPtr            : Bank_Ptr_T       := 0;
        variable prefetchData       : Prefetch_Array_T := (others => (lastAddr => addr_to_record((others => '0')), cmdCounter => 0, isPrefetched => false));
        variable prechargeBurst     : boolean          := false;

        -- HELPER FUNCTIONS

        -- check whether we can issue Refresh command
        impure function all_banks_precharged return boolean is
            variable retval : boolean := true;
        begin
            for i in 0 to BANK_COUNT - 1 loop
                retval := retval and not bankState(i).active;
            end loop;
            return retval;
        end function all_banks_precharged;

        -- start a burst and setup burst state
        procedure burst_start(burstType : in Op_T) is
            variable interleavedRead : boolean := burstType = Read and (burstState.inBurst and burstState.burstType = Read and burstState.counter = 0);
        begin
            case burstType is
                when Read =>
                    nextCmd <= read((others => '0'), currAddr.bank, false);
                when Write =>
                    nextCmd     <= write((others => '0'), currAddr.bank, false);
                    sdramDataIo <= dataIn;
                    dqm         <= (others => '0');
            end case;

            burstState     <= (
                counter         => 2**COL_ADDR_WIDTH - 1,
                inBurst         => true,
                burstType       => burstType,
                interleavedRead => interleavedRead
            );
            prechargeBurst := false;
        end procedure burst_start;

        procedure schedule_bank_activation(addr : in Ctrl_Addr_R) is
            variable cmdBuilder      : Cmd_Plan_Array_T      := (others => Precharge);
            variable currBank        : Bank_Ptr_T            := to_integer(addr.bank);
            variable lastCmdPtr      : integer range -1 to 1 := -1;
            variable shouldPrecharge : boolean               := bankState(currBank).active and bankState(currBank).row /= addr.row;
            variable shouldActivate  : boolean               := not bankState(currBank).active or bankState(currBank).row /= addr.row;
        begin
            -- default values
            currState   <= ExecutePlan;
            waitCounter <= 0;

            if shouldActivate then
                cmdBuilder(0) := Active;
                lastCmdPtr    := lastCmdPtr + 1;
            end if;

            if shouldPrecharge then
                cmdBuilder(1) := Precharge;
                lastCmdPtr    := lastCmdPtr + 1;
            end if;

            currPlan <= (
                addr            => addr,
                cmdPlan         => cmdBuilder,
                cmdPtr          => lastCmdPtr,
                waitForBurstEnd => false
            );
        end procedure schedule_bank_activation;

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
                        prechargeBurst := true;
                    end if;
                -- else just keep the row open after burst end
                else
                    -- FIXME: implement
                    schedule_bank_activation(thisAddr);
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

        procedure schedule_cmd(cmd : in Ctrl_Executable_Cmd_T; addr : in Ctrl_Addr_R) is
            variable mainCmd         : Tmp_Cmd_T            := cmd_to_op(cmd);
            variable cmdBuilder      : Cmd_Plan_Array_T     := (others => Precharge);
            variable currBank        : Bank_Ptr_T           := to_integer(addr.bank);
            variable lastCmdPtr      : natural range 0 to 3 := 0;
            variable shouldPrecharge : boolean              := bankState(currBank).active and bankState(currBank).row /= addr.row;
            variable shouldActivate  : boolean              := not bankState(currBank).active or bankState(currBank).row /= addr.row;
            variable lastNextCmd     : Mem_IO_Aggregate_R;
        begin
            -- default values
            currState   <= ExecutePlan;
            waitCounter <= 0;

            -- create execution plan according to command
            case cmd is
                when Read  | Write =>
                    cmdBuilder(0) := mainCmd;
                    if cmd = Write then
                        lastNextCmd := write((others => '0'), addr.bank, false);
                    else
                        lastNextCmd := read((others => '0'), addr.bank, false);
                    end if;

                    if shouldActivate then
                        cmdBuilder(1) := Active;
                        lastCmdPtr    := lastCmdPtr + 1;
                        lastNextCmd   := active(addr.row, addr.bank);
                    end if;

                    if shouldPrecharge then
                        cmdBuilder(2) := Precharge;
                        lastCmdPtr    := lastCmdPtr + 1;
                        lastNextCmd   := precharge(addr.bank, false);
                    end if;

                when Refresh =>
                    cmdBuilder(0) := Refresh;
                    lastNextCmd   := refresh;

                    if not all_banks_precharged then
                        cmdBuilder(1) := PrechargeAll;
                        lastCmdPtr    := lastCmdPtr + 1;
                        lastNextCmd   := precharge(addr.bank, true);
                    end if;
            end case;

            currPlan <= (
                addr            => addr,
                cmdPlan         => cmdBuilder,
                cmdPtr          => lastCmdPtr,
                waitForBurstEnd => false
            );

            if burstState.inBurst then
                if cmd = Read then
                    -- if we want to start a read burst to same bank/row or
                    -- another already active bank/row, we can start immediately
                    if not shouldPrecharge and not shouldActivate then
                        burst_start(cmd);
                        currState       <= Burst;
                        currPlan.cmdPtr <= lastCmdPtr - 1;
                    -- if we want to start a read burst to same bank but
                    -- another row, end current burst by a precharge cmd
                    elsif (shouldPrecharge and addr.bank = lastAddr.bank and burstState.burstType = Read) or burstState.counter < 0 then
                        nextCmd         <= lastNextCmd;
                        waitCounter     <= cmd_delay(lastNextCmd.cmd) - 1;
                        currPlan.cmdPtr <= lastCmdPtr - 1;
                    end if;
                -- IMPLICIT ELSE
                -- just wait for one cycle to let burst terminate command apply
                -- wait is implicit in state transition

                -- TODO: add helpful comment
                elsif cmd = Write then
                    if (shouldPrecharge and burstState.burstType = Read) or (shouldActivate and burstState.counter < 0) then
                        nextCmd         <= lastNextCmd;
                        waitCounter     <= cmd_delay(lastNextCmd.cmd) - 1;
                        currPlan.cmdPtr <= lastCmdPtr - 1;
                    elsif not shouldPrecharge and not shouldActivate and burstState.burstType = Write then
                        burst_start(cmd);
                        currState       <= Burst;
                        currPlan.cmdPtr <= lastCmdPtr - 1;
                    else
                        currPlan.waitForBurstEnd <= true;
                    end if;

                -- if we are in burst and want to Refresh banks, we implicitly
                -- have to precharge all banks
                elsif cmd = Refresh then
                    if burstState.burstType = Read then
                        nextCmd         <= lastNextCmd;
                        waitCounter     <= cmd_delay(lastNextCmd.cmd) - 1;
                        currPlan.cmdPtr <= lastCmdPtr - 1;
                    else
                        currPlan.waitForBurstEnd <= true;
                    end if;
                end if;
            else
                if (cmd = Write or cmd = Read) and lastCmdPtr = 0 then
                    burst_start(cmd);
                else
                    nextCmd         <= lastNextCmd;
                    waitCounter     <= cmd_delay(lastNextCmd.cmd) - 1;
                    currPlan.cmdPtr <= lastCmdPtr - 1;
                end if;
            end if;
        end procedure schedule_cmd;
    begin
        if rstAsyncIn = '1' then
            -- by default mask data
            dqm        <= (others => '1');
            burstState <= (inBurst => false, counter => 0, burstType => Read, interleavedRead => false);
            currPlan   <= (addr => addr_to_record((others => '0')), cmdPlan => (others => Precharge), cmdPtr => 0, waitForBurstEnd => false);
            currState  <= Idle;

            prefetchData := (others => (lastAddr => addr_to_record((others => '0')), cmdCounter => 0, isPrefetched => false));
            lastAddr     := addr_to_record((others => '0'));
        elsif rising_edge(clkIn) then
            -- signals only active for one clock
            nextCmd     <= nop;
            sdramDataIo <= (others => 'Z');
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
                    -- let the data go High-Z after a full page burst
                    if currCmd = Write then
                        if burstState.counter > 0 then
                            dqm         <= (others => '0');
                            sdramDataIo <= dataIn;
                        end if;
                    elsif currCmd = Read then
                        if burstState.counter >= -tCAS and (burstState.counter <= PAGE_LEN - tCAS or burstState.interleavedRead) then
                            dqm     <= (others => '0');
                            dataOut <= sdramDataIo;
                        end if;
                    end if;

                    if burstState.counter = 0 then
                        -- end read/write burst (cmd can be overwritten for interleaved writes/reads)
                        nextCmd <= burst_terminate;
                    end if;

                    -- a write burst end indicates that we can use any command
                    if burstState.counter = 0 and burstState.burstType = Write then
                        burstState.inBurst <= false;
                    -- a read burst end indicates that we can use any command (including a write)
                    elsif burstState.counter = -tCAS + 1 and burstState.burstType = Read then
                        burstState.inBurst <= false;
                    end if;
                end if;

                -- sdram cmd & state handling
                case currState is
                    when Idle =>
                        currAddr := addr_to_record(addrIn);
                        currCmd  := cmdIn;
                        bankPtr  := to_integer(currAddr.bank);

                        -- log currently requested operation
                        if currCmd /= NoOp then
                            Log(CTRL_ALERT_ID, "Received command: " & to_string(currCmd), INFO);
                        end if;

                        case currCmd is
                            when Read | Write =>
                                schedule_cmd(currCmd, currAddr);
                                prefetchData(currCmd)                     := (
                                    lastAddr     => currAddr,
                                    cmdCounter   => prefetchData(currCmd).cmdCounter + 1,
                                    isPrefetched => false
                                );
                                prefetchData(next_op(currCmd)).cmdCounter := 0;
                                lastAddr                                  := currAddr;
                                prechargeBurst                            := false;

                            when Refresh =>
                                schedule_cmd(currCmd, currAddr);
                                prechargeBurst := false;

                            -- precharge current bank after a read/write burst 
                            -- if the action is not interrupted by another command
                            when NoOp =>
                                if prechargeBurst then
                                    if burstState.burstType = Read or (burstState.burstType = Write and burstState.inBurst = false) then
                                        currState   <= ExecutePlan;
                                        nextCmd     <= precharge(lastAddr.bank, false);
                                        waitCounter <= cmd_delay(Precharge) - 1;
                                    end if;
                                end if;
                        end case;

                    -- execute cmd plan
                    when ExecutePlan =>
                        if waitCounter = 0 then
                            if (not currPlan.waitForBurstEnd or not burstState.inBurst) then
                                if currPlan.cmdPtr >= 0 then
                                    case currPlan.cmdPlan(currPlan.cmdPtr) is
                                        when Read | Write =>
                                            burst_start(currCmd);
                                            currState <= Burst;
                                        when Precharge =>
                                            nextCmd     <= precharge(currPlan.addr.bank, false);
                                            waitCounter <= cmd_delay(Precharge) - 1;
                                        when Refresh =>
                                            nextCmd     <= refresh;
                                            waitCounter <= cmd_delay(Refresh) - 1;
                                        when Active =>
                                            nextCmd     <= active(currPlan.addr.row, currPlan.addr.bank);
                                            waitCounter <= cmd_delay(Active) - 1;
                                        when PrechargeAll =>
                                            nextCmd     <= precharge(currPlan.addr.bank, true);
                                            waitCounter <= cmd_delay(Precharge) - 1;
                                    end case;

                                    currPlan.cmdPtr <= currPlan.cmdPtr - 1;
                                else
                                    if burstState.inBurst then
                                        currState <= Burst;
                                    else
                                        currState <= Idle;
                                    end if;
                                end if;
                            end if;
                        else
                            waitCounter <= waitCounter - 1;
                        end if;

                    when Burst =>
                        if not prefetchData(Read).isPrefetched then
                            prefetchData(Read).isPrefetched := true;
                            schedule_addr_prefetch(next_row_addr(prefetchData(Read).lastAddr, ROW_MAX), next_row_addr(prefetchData(Write).lastAddr, ROW_MAX), Read);
                            currState                       <= ExecutePlan;

                        elsif not prefetchData(Write).isPrefetched then
                            prefetchData(Write).isPrefetched := true;
                            schedule_addr_prefetch(next_row_addr(prefetchData(Write).lastAddr, ROW_MAX), next_row_addr(prefetchData(Read).lastAddr, ROW_MAX), Write);
                            currState                        <= ExecutePlan;

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

                    when ExecutePlan =>
                        if waitCounter = 0 and (not currPlan.waitForBurstEnd or not burstState.inBurst) and currPlan.cmdPtr = -1 and not burstState.inBurst then
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
