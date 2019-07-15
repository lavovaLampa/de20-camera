library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.sdram_ctrl_pkg.all;
use work.sdram_pkg.all;

entity sdram_ctrl is
    generic(
        ROW_MAX         : natural := 1800;
        READ_BURST_LEN  : natural := 5;
        WRITE_BURST_LEN : natural := 4
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
    type Internal_State_T is (Idle, BatchWait, Burst);

    -- SDRAM I/O
    signal memIo : Mem_IO_R;

    -- next memory i/o (to prevent contention by init+mem controllers)
    signal nextMemData : Mem_Data_Aggregate_R;

    -- cmd output of controller
    signal nextCmd  : Mem_IO_Aggregate_R := nop;
    signal nextData : Data_T             := (others => 'Z');

    -- sdram signal providers
    signal memCtrlIo  : Mem_Data_Aggregate_R := pack_io_data(nextData, nextCmd);
    signal initCtrlIo : Mem_Data_Aggregate_R;
    signal clkEnable  : std_logic            := '0';

    -- internal signals
    signal ramInitialized : boolean            := false;
    signal bankState      : Bank_State_Array_T := (others => (active => false, row => (others => '0')));
    signal burstState     : Burst_State_R      := (inBurst => false, counter => 0, precharge => false);
begin
    -- if initialized let the controller communicate with the sdram
    with ramInitialized select nextMemData <=
        memCtrlIo when true,
        initCtrlIo when false;

    -- unpack mem i/o signals
    memIo <= (
        addr         => nextMemData.addr,
        bankSelect   => nextMemData.bank,
        data         => nextMemData.data,
        clkEnable    => clkEnable,
        cmdAggregate => encode_cmd(nextMemData.cmd),
        dqm          => (others => '0')
    );

    mainProc : process(clkIn, rstAsyncIn, nextCmd, nextData)
        impure function all_banks_precharged return boolean is
            variable retval : boolean := true;
        begin
            for i in 0 to BANK_COUNT - 1 loop
                retval := retval and not bankState(i).active;
            end loop;
            return retval;
        end function all_banks_precharged;

        -- reg
        variable currState                   : Internal_State_T       := Idle;
        variable lastAddr                    : Ctrl_Addr_R            := addr_to_record((others => '0'));
        variable lastCmd                     : Ctrl_Cmd_T             := NoOp;
        variable bankPtr                     : Bank_Ptr_T             := 0;
        variable scheduledCmd                : Scheduled_Cmd_R        := (cmd => NoOp, addr => addr_to_record((others => '0')), startBurst => false, done => false);
        variable waitCounter                 : integer range -2 to 10 := 10;
        variable readPrefetch, writePrefetch : Prefetch_Data_R        := (lastAddr => addr_to_record((others => '0')), cmdCounter => 0, isPrefetched => false);

        -- scheduled bank activation (e.g. if wrong row is active, first Precharge, then Activate)
        procedure schedule_bank_activation(addr : in Ctrl_Addr_R; startBurst : in boolean) is
            variable currBank : Bank_Ptr_T := to_integer(addr.bank);
        begin
            if not bankState(currBank).active then
                nextCmd     <= active(addr.row, addr.bank);
                waitCounter := cmd_delay(Active);
            else
                nextCmd      <= precharge(addr.bank, false);
                waitCounter  := cmd_delay(Precharge);
                -- schedule row activation after we precharge the old one
                scheduledCmd := (
                    cmd        => Active,
                    addr       => addr,
                    startBurst => startBurst,
                    done       => false
                );
            end if;
        end procedure schedule_bank_activation;

        -- start a burst and setup burst state
        procedure schedule_burst_start(isRead : in boolean) is
        begin
            if isRead then
                nextCmd            <= read((others => '0'), lastAddr.bank, false);
                burstState.counter <= 2**COL_ADDR_WIDTH - 1 + tCAS;
            else
                nextCmd            <= write((others => '0'), lastAddr.bank, false);
                nextData           <= dataIn;
                burstState.counter <= 2**COL_ADDR_WIDTH - 1;
            end if;

            burstState.inBurst   <= true;
            burstState.precharge <= false;
        end procedure schedule_burst_start;

        -- return whether we should try to early activate this address' bank/row
        impure function should_prefetch_addr(thisAddr, otherAddr : in Ctrl_Addr_R; isRead : boolean) return boolean is
            variable currBank       : Bank_Ptr_T := to_integer(thisAddr.bank);
            variable shouldPrefetch : boolean    := true;
        begin
            if thisAddr.bank = otherAddr.bank then
                if thisAddr.row /= otherAddr.row then
                    if isRead then
                        shouldPrefetch := (lastCmd = Read and readPrefetch.cmdCounter < READ_BURST_LEN) or (lastCmd = Write and writePrefetch.cmdCounter >= WRITE_BURST_LEN);
                    else
                        shouldPrefetch := (lastCmd = Read and readPrefetch.cmdCounter >= READ_BURST_LEN) or (lastCmd = Write and writePrefetch.cmdCounter < WRITE_BURST_LEN);
                    end if;
                end if;
            end if;

            return shouldPrefetch and not (bankState(currBank).active and bankState(currBank).row = thisAddr.row);
        end function should_prefetch_addr;

        -- try to queue row/bank activation based on next predicted address
        procedure schedule_addr_prefetch(thisAddr, otherAddr : in Ctrl_Addr_R; isRead : boolean) is
            variable shouldPrefetchAddr : boolean := should_prefetch_addr(thisAddr, otherAddr, isRead);
        begin
            -- if next predicted bank is the same as the one being bursted
            -- either keep it open or precharge it at the end of the burst
            if shouldPrefetchAddr then
                if thisAddr.bank = lastAddr.bank then
                    if thisAddr.row /= lastAddr.row then
                        burstState.precharge <= true;
                    end if;
                -- else just keep the row open after burst end
                else
                    schedule_bank_activation(thisAddr, false);
                end if;
            end if;
        end procedure schedule_addr_prefetch;
    begin
        -- pack controller sdram i/o
        memCtrlIo <= (
            cmd  => nextCmd.cmd,
            addr => nextCmd.addr,
            bank => nextCmd.bank,
            data => nextData
        );

        -- FIXME: rewrite cmd ready flag handling
        cmdReadyOut <= currState = Idle;

        if rstAsyncIn = '1' then
            currState     := Idle;
            burstState    <= (inBurst => false, counter => 0, precharge => false);
            readPrefetch  := (lastAddr => addr_to_record((others => '0')), cmdCounter => 0, isPrefetched => false);
            writePrefetch := (lastAddr => addr_to_record((others => '0')), cmdCounter => 0, isPrefetched => false);
        elsif rising_edge(clkIn) then
            -- signals only active for one clock
            nextCmd  <= nop;
            nextData <= (others => 'Z');

            -- burst counter and data handling
            if burstState.inBurst then
                if lastCmd = Write then
                    nextData <= dataIn;
                elsif lastCmd = Read then
                    dataOut <= memIo.data;
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
                    lastAddr := addr_to_record(addrIn);
                    lastCmd  := cmdIn;
                    bankPtr  := to_integer(lastAddr.bank);

                    case cmdIn is
                        when Read | Write =>
                            if bankState(bankPtr).row = lastAddr.row and bankState(bankPtr).active then
                                schedule_burst_start(lastCmd = Read);
                                currState := Burst;
                            else
                                schedule_bank_activation(lastAddr, true);
                                currState := BatchWait;
                            end if;

                            if cmdIn = Read then
                                readPrefetch             := (
                                    lastAddr     => lastAddr,
                                    cmdCounter   => readPrefetch.cmdCounter + 1,
                                    isPrefetched => false
                                );
                                writePrefetch.cmdCounter := 0;
                            else
                                writePrefetch           := (
                                    lastAddr     => lastAddr,
                                    cmdCounter   => writePrefetch.cmdCounter + 1,
                                    isPrefetched => false
                                );
                                readPrefetch.cmdCounter := 0;
                            end if;

                        when Refresh =>
                            currState := BatchWait;
                            if all_banks_precharged then
                                nextCmd     <= refresh;
                                waitCounter := cmd_delay(Refresh);
                            else
                                nextCmd     <= precharge((others => '-'), true);
                                waitCounter := cmd_delay(Precharge);
                            end if;

                        when NoOp =>
                            null;
                    end case;

                when BatchWait =>
                    if waitCounter <= 0 then
                        if scheduledCmd.done then
                            if scheduledCmd.startBurst or burstState.inBurst then
                                currState := Burst;
                            else
                                currState := Idle;
                            end if;

                            if scheduledCmd.startBurst then
                                schedule_burst_start(lastCmd = Read);
                            end if;
                        else
                            waitCounter       := cmd_delay(scheduledCmd.cmd) - 1;
                            scheduledCmd.done := true;
                            case scheduledCmd.cmd is
                                when Active    => nextCmd <= active(scheduledCmd.addr.row, scheduledCmd.addr.bank);
                                when Precharge => nextCmd <= precharge(scheduledCmd.addr.bank, false);
                                when Refresh   => nextCmd <= refresh;
                                when others =>
                                    report "Unsupported command"
                                    severity error;
                            end case;
                        end if;
                    else
                        waitCounter := waitCounter - 1;
                    end if;

                when Burst =>
                    if not readPrefetch.isPrefetched then
                        readPrefetch.isPrefetched := true;
                        schedule_addr_prefetch(next_row_addr(readPrefetch.lastAddr, ROW_MAX), next_row_addr(writePrefetch.lastAddr, ROW_MAX), true);
                        currState                 := BatchWait;
                    elsif not writePrefetch.isPrefetched then
                        writePrefetch.isPrefetched := true;
                        schedule_addr_prefetch(next_row_addr(writePrefetch.lastAddr, ROW_MAX), next_row_addr(readPrefetch.lastAddr, ROW_MAX), false);
                        currState                  := BatchWait;
                    elsif burstState.counter = 0 then
                        currState := Idle;
                    end if;
            end case;
        end if;
    end process mainProc;
end architecture RTL;
