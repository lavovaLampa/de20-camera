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
        clkIn, rstAsyncIn : in    std_logic;
        -- ==============================
        -- |    row_addr    | bank_addr |
        -- ==============================
        -- 13              2 1          0
        addrIn            : in    Ctrl_Addr_T;
        cmdIn             : in    Ctrl_Cmd_T;
        cmdReadyOut       : out   boolean;
        dataReadyOut      : out   boolean; -- new data ready for reading
        nextDataOut       : out   boolean; -- get next data ready for writing
        dataIn            : in    Data_T;
        dataOut           : out   Data_T;
        -- init controller I/O
        memInitializedIn  : in    boolean;
        -- SDRAM I/O
        sdramOut          : out   Mem_IO_R;
        sdramDataIo       : inout Data_T := (others => 'Z')
    );
    constant CTRL_ALERT_ID : AlertLogIDType := GetAlertLogID("CTRL MEM", ALERTLOG_BASE_ID);
end entity sdram_ctrl;

architecture RTL of sdram_ctrl is
    type Internal_State_T is (Idle, ExecutePlan, Burst);

    -- SDRAM I/O
    signal nextCmd  : Mem_IO_Aggregate_R;
    signal dqm      : Dqm_T;
    signal nextData : Data_T := (others => 'Z'); -- i/o reg

    -- internal registers
    signal bankState      : Bank_State_Array_T    := (others => (active => false, row => (others => '0')));
    signal burstState     : Burst_State_R         := (counter => Burst_Counter_Range_T'low, burstType => Write, interleavedRead => false, interleaveDelay => 0);
    signal bursting       : boolean               := false;
    signal currState      : Internal_State_T      := Idle;
    signal currPlan       : Execution_Plan_R      := (addr => addr_to_record((others => '0')), cmdPlan => (others => Precharge), cmdPtr => 0, waitForBurstEnd => false);
    signal waitCounter    : natural range 0 to 10 := 0;
    signal prechargeBurst : boolean               := false;

    -- debug signals
    signal dbgLastAddr     : Ctrl_Addr_R;
    signal dbgLastCmd      : Ctrl_Cmd_T;
    signal dbgBankPtr      : Bank_Ptr_T;
    signal dbgPrefetchData : Prefetch_Array_T;
begin
    -- pack sdram signals
    sdramOut    <= (
        addr         => nextCmd.addr,
        bankSelect   => nextCmd.bank,
        cmdAggregate => encode_cmd(nextCmd.cmd),
        dqm          => dqm,
        clkEnable    => '1'
    );
    sdramDataIo <= nextData;

    -- signal applies to internal controller cmd operations
    bursting <= (burstState.burstType = Write and burstState.counter > -1) or (burstState.burstType = Read and burstState.counter > -tCAS);

    mainProc : process(clkIn, rstAsyncIn)
        -- reg
        variable currAddr, lastAddr : Ctrl_Addr_R      := addr_to_record((others => '0'));
        variable currCmd            : Ctrl_Cmd_T       := NoOp;
        variable prefetchData       : Prefetch_Array_T := (others => (lastAddr => addr_to_record((others => '0')), cmdCounter => 0, isPrefetched => false));

        -- helpers (think of these variables as aliases)
        variable bankPtr     : Bank_Ptr_T := 0;
        variable currPlanCmd : Executable_Op_T;

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

        procedure reset_prefetch_state is
        begin
            for i in Burst_Op_T loop
                prefetchData(i).isPrefetched := false;
            end loop;
        end procedure reset_prefetch_state;

        -- start a burst and setup burst state
        procedure burst_start(burstType : in Burst_Op_T) is
            variable interleavedRead : boolean :=
                (burstType = Read and (bursting and burstState.burstType = Read and burstState.counter = 0)) or 
                (burstType = Write and burstState.burstType = Read and burstState.counter = -tCAS);
        begin
            case burstType is
                when Read =>
                    nextCmd <= read((others => '0'), currAddr.bank, false);
                when Write =>
                    nextCmd  <= write((others => '0'), currAddr.bank, false);
                    nextData <= dataIn;
            end case;
            dqm <= (others => '0');

            burstState     <= (
                counter         => 2**COL_ADDR_WIDTH - 1,
                burstType       => burstType,
                interleavedRead => interleavedRead,
                interleaveDelay => 0
            );
            prechargeBurst <= false;
        end procedure burst_start;

        -- return whether we should try to early activate this address' bank/row
        impure function should_prefetch_addr(thisAddr, otherAddr : in Ctrl_Addr_R; thisOp : Burst_Op_T) return boolean is
            variable currBank       : Bank_Ptr_T := to_integer(thisAddr.bank);
            variable shouldPrefetch : boolean    := true;
            variable otherOp        : Burst_Op_T := next_op(thisOp);
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

        -- self-explanatory save current bank state (to keep track of Activating/Precharging)
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

        -- create execution plan according to current state
        impure function create_execution_plan(op : Schedulable_Op_T; addr : Ctrl_Addr_R) return Execution_Plan_R is
            variable executableOp  : Executable_Op_T       := sched_op_map(op);
            variable cmdBuilder    : Executable_Op_Array_T := (others => Precharge);
            variable cmdPtrBuilder : integer range -1 to 3 := -1;

            -- helper state variables
            variable currBank        : Bank_Ptr_T := to_integer(addr.bank);
            variable shouldPrecharge : boolean    := bankState(currBank).active and bankState(currBank).row /= addr.row;
            variable shouldActivate  : boolean    := not bankState(currBank).active or bankState(currBank).row /= addr.row;
        begin
            if op /= Active and op /= Precharge then
                cmdPtrBuilder             := cmdPtrBuilder + 1;
                cmdBuilder(cmdPtrBuilder) := executableOp;
            end if;

            if op /= Refresh and op /= Precharge and shouldActivate then
                cmdPtrBuilder             := cmdPtrBuilder + 1;
                cmdBuilder(cmdPtrBuilder) := Active;
            end if;

            if op = Precharge or (op /= Refresh and shouldPrecharge) then
                cmdPtrBuilder             := cmdPtrBuilder + 1;
                cmdBuilder(cmdPtrBuilder) := Precharge;
            elsif op = Refresh and not all_banks_precharged then
                cmdPtrBuilder             := cmdPtrBuilder + 1;
                cmdBuilder(cmdPtrBuilder) := PrechargeAll;
            end if;

            -- waitForBurstEnd setting is a schedulers' responsibility
            return (
                addr            => addr,
                cmdPlan         => cmdBuilder,
                cmdPtr          => cmdPtrBuilder,
                waitForBurstEnd => false
            );
        end function create_execution_plan;

        -- schedule generated execute plan according to current ctrl/mem state
        procedure schedule_cmd(thisPlan : in Execution_Plan_R) is
            variable tmpPlan  : Execution_Plan_R := thisPlan;
            variable firstCmd : Executable_Op_T  := tmpPlan.cmdPlan(tmpPlan.cmdPtr);
        begin
            -- default values
            currState   <= ExecutePlan;
            waitCounter <= 0;

            if tmpPlan.cmdPtr >= 0 then
                if bursting then
                    case firstCmd is
                        -- we can start a Read burst immediately following a Read/Write burst
                        when Read =>
                            burst_start(Read);
                            currState <= Burst;

                        when Write =>
                            -- we can start a Write burst immediately following a Write burst
                            if burstState.burstType = Write then
                                burst_start(Write);
                                currState <= Burst;

                            -- but we have to wait until Read burst data come throught (tCAS delay)
                            else
                                tmpPlan.waitForBurstEnd := true;
                            end if;

                        when Active =>
                            -- Active command to same bank should never be first executable op during a burst
                            assert tmpPlan.addr.bank /= currAddr.bank
                            report "Cannot Acitvate current bank without Precharging first"
                            severity error;

                            -- if burst counter = 0 we let the ctrl issue a burst terminate command
                            -- and only the do we issue an Active command to another bank
                            if burstState.counter /= 0 then
                                nextCmd        <= executable_op_to_mem_io(firstCmd, tmpPlan.addr);
                                waitCounter    <= cmd_delay(Active) - 1;
                                tmpPlan.cmdPtr := tmpPlan.cmdPtr - 1;
                            end if;
                        -- IMPLICIT ELSE
                        -- wait one clock to issue a burst stop command, then
                        -- begin activating another row/bank

                        when Precharge | PrechargeAll =>
                            assert burstState.counter <= 0 or tmpPlan.addr.bank /= currAddr.bank
                            report "Shouldn't generate a Precharge to currently bursting bank during a valid burst region"
                            severity error;

                            if not (tmpPlan.addr.bank /= currAddr.bank and burstState.counter = 0) and burstState.burstType = Read then
                                nextCmd        <= executable_op_to_mem_io(firstCmd, tmpPlan.addr);
                                waitCounter    <= cmd_delay(Precharge) - 1;
                                tmpPlan.cmdPtr := tmpPlan.cmdPtr - 1;
                            end if;
                        -- IMPLICIT ELSE
                        -- wait one clock to let Burst Terminate command be issued

                        when Refresh =>
                            report "Cannot do a Refresh during Burst (invalid command)" severity error;

                    end case;
                else
                    case firstCmd is
                        when Read =>
                            burst_start(Read);
                            currState <= Burst;

                        when Write =>
                            burst_start(Write);
                            currState <= Burst;

                        when others =>
                            nextCmd        <= executable_op_to_mem_io(firstCmd, tmpPlan.addr);
                            waitCounter    <= cmd_delay(executable_op_to_mem_io(firstCmd, tmpPlan.addr).cmd) - 1;
                            tmpPlan.cmdPtr := tmpPlan.cmdPtr - 1;

                    end case;
                end if;
            else
                report "Should always create execution plan containing atleast one operation"
                severity error;
            end if;

            -- queue the updated plan for execution by Plan Executor
            currPlan <= tmpPlan;
        end procedure schedule_cmd;

        -- try to queue row/bank activation based on next predicted address
        procedure schedule_addr_prefetch(thisAddr, otherAddr : in Ctrl_Addr_R; thisOp : Burst_Op_T) is
            variable shouldPrefetchAddr : boolean := should_prefetch_addr(thisAddr, otherAddr, thisOp);
        begin
            -- if next predicted bank is the same as the one being bursted
            -- either keep it open or precharge it at the end of the burst
            if shouldPrefetchAddr then
                if thisAddr.bank = currAddr.bank then
                    if thisAddr.row /= currAddr.row then
                        prechargeBurst <= true;
                    end if;
                -- else just keep the row open after burst end
                else
                    schedule_cmd(create_execution_plan(Active, thisAddr));
                end if;
            end if;
        end procedure schedule_addr_prefetch;
    begin
        if rstAsyncIn = '1' then
            -- by default mask data
            dqm            <= (others => '1');
            burstState     <= (counter => Burst_Counter_Range_T'low, burstType => Write, interleavedRead => false, interleaveDelay => 0);
            currPlan       <= (addr => addr_to_record((others => '0')), cmdPlan => (others => Precharge), cmdPtr => 0, waitForBurstEnd => false);
            currState      <= Idle;
            nextData       <= (others => 'Z');
            prechargeBurst <= false;

            prefetchData := (others => (lastAddr => addr_to_record((others => '0')), cmdCounter => 0, isPrefetched => false));
            lastAddr     := addr_to_record((others => '0'));
        elsif rising_edge(clkIn) then
            -- signals only active for one clock
            nextCmd  <= nop;
            dqm      <= (others => '1');
            nextData <= (others => 'Z');
            dataOut  <= (others => '-');

            -- update bank_state according to currently scheduled sdram cmd batch
            update_bank_state(nextCmd.cmd, nextCmd.addr, nextCmd.bank);

            if memInitializedIn then
                -- decrement burst counter
                if burstState.counter > Burst_Counter_Range_T'low then
                    burstState.counter <= burstState.counter - 1;
                end if;

                -- burst state and data handling
                if burstState.burstType = Read then
                    if (burstState.counter < PAGE_LEN - (tCAS - tDQZ) and burstState.counter > (tDQZ - tCAS)) or (burstState.interleavedRead and burstState.counter >= PAGE_LEN - (tCAS - tDQZ)) then
                        dqm <= (others => '0');
                    end if;
                    if (burstState.counter >= -tCAS and burstState.counter < PAGE_LEN - tCAS) or (burstState.interleavedRead and burstState.counter >= PAGE_LEN - tCAS) then
                        dataOut <= sdramDataIo;
                    end if;
                elsif burstState.burstType = Write then
                    if burstState.counter > 0 then
                        dqm      <= (others => '0');
                        nextData <= dataIn;
                    end if;
                end if;

                if burstState.counter = 0 then
                    -- end read/write burst (cmd can be overwritten for interleaved writes/reads)
                    nextCmd <= burst_terminate;
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
                                if currCmd = Read then
                                    schedule_cmd(create_execution_plan(Read, currAddr));
                                else
                                    schedule_cmd(create_execution_plan(Write, currAddr));
                                end if;
                                prefetchData(currCmd)                     := (
                                    lastAddr     => currAddr,
                                    cmdCounter   => prefetchData(currCmd).cmdCounter + 1,
                                    isPrefetched => false
                                );
                                prefetchData(next_op(currCmd)).cmdCounter := 0;
                                lastAddr                                  := currAddr;
                                prechargeBurst                            <= false;

                            when Refresh =>
                                schedule_cmd(create_execution_plan(Refresh, currAddr));
                                prechargeBurst <= false;

                            -- precharge current bank after a read/write burst 
                            -- if the action is not interrupted by another command
                            when NoOp =>
                                if prechargeBurst then
                                    schedule_cmd(create_execution_plan(Precharge, lastAddr));
                                    prechargeBurst <= false;
                                end if;
                        end case;

                    -- execute cmd plan
                    when ExecutePlan =>
                        if waitCounter = 0 then
                            if (not currPlan.waitForBurstEnd or not bursting) then
                                if currPlan.cmdPtr >= 0 then
                                    currPlanCmd := currPlan.cmdPlan(currPlan.cmdPtr);

                                    if currPlanCmd = Read or currPlanCmd = Write then
                                        burst_start(currCmd);
                                        currState <= Burst;
                                    else
                                        nextCmd     <= executable_op_to_mem_io(currPlanCmd, currPlan.addr);
                                        waitCounter <= cmd_delay(executable_op_to_mem_io(currPlanCmd, currPlan.addr).cmd) - 1;
                                    end if;
                                    currPlan.cmdPtr <= currPlan.cmdPtr - 1;
                                else
                                    if bursting then
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

                        elsif not prefetchData(Write).isPrefetched then
                            prefetchData(Write).isPrefetched := true;
                            schedule_addr_prefetch(next_row_addr(prefetchData(Write).lastAddr, ROW_MAX), next_row_addr(prefetchData(Read).lastAddr, ROW_MAX), Write);

                        -- subtract state change penalty (state register takes new value only after next rising_edge)
                        elsif burstState.counter = 1 then
                            reset_prefetch_state;
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
                                if prechargeBurst then
                                    cmdReadyOut <= false;
                                else
                                    cmdReadyOut <= true;
                                end if;

                            when Read | Write | Refresh =>
                                cmdReadyOut <= false;
                        end case;

                    when ExecutePlan =>
                        if waitCounter = 0 and (not currPlan.waitForBurstEnd or not bursting) and currPlan.cmdPtr = -1 and not bursting then
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

    dataFlagProc : process(burstState, currPlan, waitCounter, addrIn, cmdIn, bankState, currState, bursting)
        variable currAddr : Ctrl_Addr_R := addr_to_record(addrIn);
        variable bankPtr  : Bank_Ptr_T  := to_integer(currAddr.bank);
    begin
        currAddr := addr_to_record(addrIn);
        bankPtr  := to_integer(currAddr.bank);

        -- signalize to provide new data to write to memory during write burst
        if burstState.burstType = Write and burstState.counter > 0 then
            dataReadyOut <= true;
        elsif currState = Idle and cmdIn = Write and bankState(bankPtr).active and bankState(bankPtr).row = currAddr.row and (burstState.burstType /= Read or burstState.counter <= -tCAS) then
            dataReadyOut <= true;
        elsif currPlan.cmdPtr >= 0 and currPlan.cmdPlan(currPlan.cmdPtr) = Write and waitCounter = 0 and (not currPlan.waitForBurstEnd or not bursting) then
            dataReadyOut <= true;
        else
            dataReadyOut <= false;
        end if;

        -- signalize if new data arrived to be read from memory during read burst
        if burstState.burstType = Read and burstState.counter >= -tCAS - 1 and burstState.counter < PAGE_LEN - tCAS - 1 then
            nextDataOut <= true;
        elsif burstState.burstType = Read and burstState.interleavedRead and burstState.counter >= PAGE_LEN - tCAS - 1 + burstState.interleaveDelay then
            nextDataOut <= true;
        elsif burstState.burstType = Write and burstState.interleavedRead and burstState.counter = PAGE_LEN - 1 then
            nextDataOut <= true;
        else
            nextDataOut <= false;
        end if;
    end process dataFlagProc;
end architecture RTL;
