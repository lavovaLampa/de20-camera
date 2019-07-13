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
    type Ctrl_State_T is (Idle, BatchWait);
    type Scheduler_Cmd_T is (Read, Write, Active, Precharge, PrechargeAll, Refresh);

    type Bank_State_R is record
        active : boolean;
        row    : Addr_T;
    end record Bank_State_R;
    type Bank_State_Array_T is array (BANK_COUNT - 1 downto 0) of Bank_State_R;

    type Ctrl_Addr_R is record
        row  : Addr_T;
        bank : Bank_Addr_T;
    end record Ctrl_Addr_R;

    type Scheduler_Cmd_Array_T is array (2 downto 0) of Scheduler_Cmd_T;
    type Scheduler_R is record
        cmdArray : Scheduler_Cmd_Array_T;
        arrayPtr : natural range 0 to 2;
        row      : Addr_T;
        bank     : Bank_Addr_T;
    end record Scheduler_R;

    type Burst_State_R is record
        bursting : boolean;
        bank     : Bank_Addr_T;
        reading  : boolean;
    end record Burst_State_R;

    -- state registers
    signal ctrlState      : Ctrl_State_T       := Idle;
    signal bankState      : Bank_State_Array_T := (others => (active => false, row => (others => '0')));
    signal burstState     : Burst_State_R      := (bursting => false, bank => (others => '0'), reading => false);
    signal burstPrecharge : boolean            := false;

    -- external signals
    signal ramInitialized : boolean := false;

    -- internal signals
    signal schedulerData : Scheduler_R;
    signal batchExecute  : boolean := false;
    signal batchDone     : boolean := false;
begin
    mainProc : process(clkIn, rstAsyncIn, addrIn)
        pure function addr_to_record(addr : Ctrl_Addr_T) return Ctrl_Addr_R is
        begin
            return (
                row  => addr(addr'high downto BANK_ADDR_WIDTH),
                bank => addr(BANK_ADDR_WIDTH - 1 downto 0)
            );
        end function addr_to_record;

        -- keep last address for early bank/row activation/precharge
        variable lastReadAddr, lastWriteAddr : Ctrl_Addr_R := addr_to_record((others => '0'));

        -- helper variables
        variable bankPtr : Bank_Ptr_T := 0;
        variable rowPtr  : Row_Ptr_T  := 0;

        procedure schedule_cmd(cmd : in Ctrl_Cmd_T; addr : in Ctrl_Addr_R) is
            variable currBank        : Bank_Ptr_T           := to_integer(addr.bank);
            variable opCount         : natural range 0 to 2 := 0;
            variable banksPrecharged : boolean              := true;
        begin
            schedulerData.bank <= addr.bank;
            schedulerData.row  <= addr.row;

            case cmd is
                when NoOp => null;
                when Read | Write =>
                    -- queue corresponding burst-starting operation
                    if cmd = Write then
                        schedulerData.cmdArray(0) <= Write;
                    elsif cmd = Read then
                        schedulerData.cmdArray(0) <= Read;
                    end if;

                    -- if bank is not Active or wrong row is still open, queue its activation
                    if not bankState(currBank).active or bankState(currBank).row /= addr.row then
                        schedulerData.cmdArray(1) <= Active;
                        opCount                   := opCount + 1;
                    end if;

                    -- FIXME: this condition is wrong
                    -- if bank has wrong row active, queue its Precharge first
                    if bankState(currBank).active and bankState(currBank).row /= addr.row then
                        schedulerData.cmdArray(2) <= Precharge;
                        opCount                   := opCount + 1;
                    end if;

                    schedulerData.arrayPtr <= opCount;

                when Refresh =>
                    -- if all banks are not idle, we have to precharge them before refresh
                    for i in 0 to BANK_COUNT - 1 loop
                        banksPrecharged := banksPrecharged and not bankState(i).active;
                    end loop;

                    schedulerData.cmdArray(0) <= Refresh;

                    if not banksPrecharged then
                        schedulerData.cmdArray(1) <= PrechargeAll;
                        opCount                   := opCount + 1;
                    end if;

                    schedulerData.arrayPtr <= opCount;
            end case;
        end procedure schedule_cmd;

        pure function decide_prefetch(tmp : boolean) return boolean is
        begin
            return true;
        end function decide_prefetch;

        -- prevent addr overflow
        impure function next_row_addr(currAddr : Ctrl_Addr_T) return Ctrl_Addr_T is
        begin
            if currAddr < ROW_MAX - 1 then
                return currAddr + 1;
            else
                return (others => '0');
            end if;
        end function next_row_addr;

        procedure schedule_bank_prefetch(thisAddr, otherAddr : in Ctrl_Addr_T) is
            variable thisNext     : Ctrl_Addr_R := addr_to_record(next_row_addr(thisAddr));
            variable otherNext    : Ctrl_Addr_R := addr_to_record(next_row_addr(otherAddr));
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
                    schedulerData <= (
                        arrayPtr => 1,
                        cmdArray => (Precharge, Precharge, Active),
                        row      => thisNext.row,
                        bank     => thisNext.bank
                    );
                end if;
            end if;
        end procedure schedule_bank_prefetch;
    begin
        -- helper variables
        bankPtr := to_integer(addrIn(BANK_ADDR_WIDTH - 1 downto 0));
        rowPtr  := to_integer(addrIn(Ctrl_Addr_T'high downto BANK_ADDR_WIDTH));

        if rstAsyncIn = '1' then
            cmdReadyOut <= false;

            ctrlState     <= Idle;
            schedulerData <= (
                arrayPtr => 0,
                cmdArray => (others => Refresh),
                row      => (others => '0'),
                bank     => (others => '0')
            );

            lastReadAddr  := addr_to_record((others => '0'));
            lastWriteAddr := addr_to_record((others => '0'));
        elsif rising_edge(clkIn) then
            -- default values
            batchExecute <= false;

            if ramInitialized then
                case ctrlState is
                    when Idle =>
                        if burstState.bursting then

                        else
                            if cmdIn /= NoOp then
                                ctrlState <= BatchWait;

                                schedule_cmd(cmdIn, addr_to_record(addrIn));
                                batchExecute <= true;

                                lastReadAddr  := addr_to_record(addrIn) when cmdIn = Read;
                                lastWriteAddr := addr_to_record(addrIn) when cmdIn = Write;
                            end if;
                        end if;

                    when BatchWait =>
                        if batchDone then
                            ctrlState <= Idle;
                        end if;
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
        schedulerProc : process(clkIn, rstAsyncIn)
            type Internal_State_T is (Idle, Executing, SendCmd);

            variable currState    : Internal_State_T                          := Idle;
            variable arrayPtr     : natural range Scheduler_Cmd_Array_T'range := 0;
            variable waitCounter  : natural range 0 to 15                     := 0;
            variable burstCounter : natural range 0 to 2**COL_ADDR_WIDTH      := 2**COL_ADDR_WIDTH;

            -- helper variables
            variable currOp    : Scheduler_Cmd_T;
            variable encodedOp : Mem_IO_Aggregate_R;

            procedure update_bank_state(cmd : in Scheduler_Cmd_T; row : in Addr_T; bank : in Bank_Addr_T) is
                variable bankPtr : Bank_Ptr_T := to_integer(bank);
            begin
                case cmd is
                    -- these commands do not change state of bank
                    when Read | Write | Refresh => null;
                    when Active                 => bankState(bankPtr) <= (active => true, row => row);
                    when Precharge              => bankState(bankPtr).active <= false;
                    when PrechargeAll =>
                        for i in 0 to BANK_COUNT - 1 loop
                            bankState(i).active <= false;
                        end loop;
                end case;
            end procedure update_bank_state;

            impure function encode_curr_cmd(cmd : Scheduler_Cmd_T; row : Addr_T; bank : Bank_Addr_T) return Mem_IO_Aggregate_R is
            begin
                case cmd is
                    when Read         => return read((others => '0'), bank, false);
                    when Write        => return write((others => '0'), bank, false, dataIn);
                    when Active       => return active(row, bank);
                    when Precharge    => return precharge(bank, false);
                    when PrechargeAll => return precharge((others => '-'), true);
                    when Refresh      => return refresh;
                end case;
            end function encode_curr_cmd;
        begin
            if rstAsyncIn = '1' then
                -- FIXME: handle dataReady flag setting
                dataReadyOut <= false;

                nextIo              <= nop;
                burstState.bursting <= false;
                bankState           <= (others => (active => false, row => (others => '0')));

                currState    := Idle;
                arrayPtr     := 0;
                waitCounter  := 0;
                burstCounter := 2**COL_ADDR_WIDTH;
            elsif rising_edge(clkIn) then
                -- send nop command by default
                nextIo    <= nop;
                -- only signal batch completion for 1 clock
                batchDone <= false;

                -- currently pointed operation in array
                currOp    := schedulerData.cmdArray(arrayPtr);
                -- encoded sdram command according to current operation
                encodedOp := encode_curr_cmd(currOp, schedulerData.row, schedulerData.bank);

                if ramInitialized then
                    -- burst scheduler
                    if burstState.bursting then
                        if burstCounter > 0 then
                            burstCounter := burstCounter - 1;
                        else
                            -- terminate current burst (do not precharge automatically)
                            nextIo              <= burst_terminate;
                            burstState.bursting <= false;
                        end if;
                    end if;

                    -- cmd scheduler
                    case currState is
                        when Idle =>
                            if batchExecute then
                                assert not burstState.bursting or burstCounter > 20
                                report "Cannot start any operation right before burst end!"
                                severity error;

                                currState := SendCmd;
                                arrayPtr  := schedulerData.arrayPtr;
                            end if;

                        when Executing =>
                            if waitCounter <= 0 then
                                update_bank_state(currOp, schedulerData.row, schedulerData.bank);
                                if arrayPtr <= 0 then
                                    currState := Idle;
                                    batchDone <= true;
                                else
                                    arrayPtr     := arrayPtr - 1;
                                    currState    := SendCmd;
                                    -- request data for read operation
                                    dataReadyOut <= schedulerData.cmdArray(arrayPtr) = Write;
                                end if;
                            else
                                waitCounter := waitCounter - 1;
                            end if;

                        when SendCmd =>
                            assert not (burstState.bursting and (currOp = Read or currOp = Write or currOp = Refresh))
                            report "Cannot perform " & Scheduler_Cmd_T'image(currOp) & " during read/write burst!"
                            severity error;

                            nextIo      <= encodedOp;
                            -- FIXME: check if offset is right
                            waitCounter := cmd_wait_cycles(encodedOp.cmd) - 1;
                            currState   := Executing;

                            -- signal we started a read/write burst
                            if currOp = Write or currOp = Read then
                                burstState.bursting <= true;
                                burstCounter        := 2**COL_ADDR_WIDTH - 1;
                            end if;
                    end case;
                end if;
            end if;
        end process schedulerProc;
    end block ramBlock;
end architecture RTL;
