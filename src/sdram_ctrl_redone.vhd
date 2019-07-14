library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.sdram_ctrl_pkg.all;
use work.sdram_pkg.all;

entity sdram_ctrl_redone is
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
end entity sdram_ctrl_redone;

architecture RTL of sdram_ctrl_redone is
    type Internal_State_T is (Idle, BatchWait, Burst);

    -- SDRAM I/O
    signal memIo                            : Mem_IO_Aggregate_R;
    -- helpers
    signal chipSelectNeg, rowAddrStrobeNeg  : std_logic;
    signal colAddrStrobeNeg, writeEnableNeg : std_logic;
    signal sdramAddr                        : Addr_T          := (others => '0');
    signal bankSelect                       : Bank_Addr_T     := (others => '0');
    signal sdramData                        : Data_T          := (others => 'Z');
    signal encodedCmd                       : Cmd_Aggregate_R := encode_cmd(NoOp);

    -- sdram signal providers
    signal nextIo : Mem_IO_Aggregate_R := nop;
    signal initIo : Mem_IO_Aggregate_R;

    -- internal signals
    signal ramInitialized : boolean            := false;
    signal bankState      : Bank_State_Array_T := (others => (active => false, row => (others => '0')));
    signal burstState     : Burst_State_R      := (inBurst => false, counter => 0);
begin
    -- if initialized let the controller communicate with the sdram
    with ramInitialized select memIo <=
        nextIo when true,
        initIo when false;

    -- encode mem cmd
    encodedCmd <= encode_cmd(memIo.cmd);

    -- unpack mem i/o signals
    sdramData  <= memIo.data;
    bankSelect <= memIo.bank;
    sdramAddr  <= memIo.addr;

    -- unpack mem cmd signals 
    chipSelectNeg    <= encodedCmd.chipSelectNeg;
    rowAddrStrobeNeg <= encodedCmd.rowAddrStrobeNeg;
    colAddrStrobeNeg <= encodedCmd.colAddrStrobeNeg;
    writeEnableNeg   <= encodedCmd.writeEnableNeg;

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
        variable currState                       : Internal_State_T       := Idle;
        variable lastAddr                        : Ctrl_Addr_R            := addr_to_record((others => '0'));
        variable lastCmd                         : Ctrl_Cmd_T             := NoOp;
        variable bankPtr                         : Bank_Ptr_T             := 0;
        variable scheduledCmd                    : Scheduled_Cmd_R        := (cmd => NoOp, addr => addr_to_record((others => '0')), startBurst => false, done => false);
        variable waitCounter                     : integer range -2 to 10 := 10;
        variable readPrefetched, writePrefetched : boolean                := false;

        -- wire

        -- scheduled bank activation (e.g. if wrong row is active, first Precharge, then Activate)
        procedure activate_bank(addr : in Ctrl_Addr_R; startBurst : in boolean) is
            variable currBank : Bank_Ptr_T := to_integer(addr.bank);
        begin
            if not bankState(currBank).active then
                nextIo      <= active(addr.row, addr.bank);
                waitCounter := cmd_delay(Active);
            else
                nextIo       <= precharge(addr.bank, false);
                waitCounter  := cmd_delay(Precharge);
                -- schedule row activation after we precharge the old one
                scheduledCmd := (
                    cmd        => Active,
                    addr       => addr,
                    startBurst => startBurst,
                    done       => false
                );
            end if;
        end procedure activate_bank;

        -- start a burst and setup burst state
        procedure start_burst(isRead : in boolean) is
        begin
            if isRead then
                nextIo             <= read((others => '0'), lastAddr.bank, false);
                burstState.counter <= 2**COL_ADDR_WIDTH - 1 + tCAS;
            else
                nextIo             <= write((others => '0'), lastAddr.bank, false, dataIn);
                burstState.counter <= 2**COL_ADDR_WIDTH - 1;
            end if;

            burstState.inBurst <= true;
        end procedure start_burst;

        procedure prefetch_addr(thisAddr, otherAddr : in Ctrl_Addr_R; isRead : boolean) is
        begin

        end procedure prefetch_addr;
    begin
        cmdReadyOut <= currState = Idle;

        if rstAsyncIn = '1' then
            currState       := Idle;
            burstState      <= (inBurst => false, counter => 0);
            writePrefetched := false;
            readPrefetched  := false;
        elsif rising_edge(clkIn) then
            nextIo <= nop;

            -- burst counter and state handling
            if burstState.inBurst then
                if lastCmd = Write then
                    nextIo <= nop(dataIn);
                end if;

                if lastCmd = Read and burstState.counter < 3 then

                elsif burstState.counter = 0 then

                else
                    burstState.counter <= burstState.counter - 1;
                end if;
            end if;

            case currState is
                when Idle =>
                    lastAddr        := addr_to_record(addrIn);
                    lastCmd         := cmdIn;
                    bankPtr         := to_integer(lastAddr.bank);
                    writePrefetched := false;
                    readPrefetched  := false;

                    case cmdIn is
                        when Read | Write =>
                            if bankState(bankPtr).row = lastAddr.row and bankState(bankPtr).active then
                                start_burst(lastCmd = Read);
                                currState := Burst;
                            else
                                activate_bank(lastAddr, true);
                                currState := BatchWait;
                            end if;

                        when Refresh =>
                            currState := BatchWait;
                            if all_banks_precharged then
                                nextIo      <= refresh;
                                waitCounter := cmd_delay(Refresh);
                            else
                                nextIo      <= precharge((others => '-'), true);
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
                                start_burst(lastCmd = Read);
                            end if;
                        else
                            waitCounter       := cmd_delay(scheduledCmd.cmd) - 1;
                            scheduledCmd.done := true;
                            case scheduledCmd.cmd is
                                when Active    => nextIo <= active(scheduledCmd.addr.row, scheduledCmd.addr.bank);
                                when Precharge => nextIo <= precharge(scheduledCmd.addr.bank, false);
                                when Refresh   => nextIo <= refresh;
                                when others =>
                                    report "Unsupported command"
                                    severity error;
                            end case;
                        end if;
                    else
                        waitCounter := waitCounter - 1;
                    end if;

                -- TOOD: prefetch read/write banks
                when Burst =>
                    if not readPrefetched then
                        readPrefetched := true;

                    elsif not writePrefetched then
                        writePrefetched := true;

                    end if;
            end case;
        end if;
    end process mainProc;
end architecture RTL;
