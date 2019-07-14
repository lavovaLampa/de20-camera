library ieee;
use std.textio.all;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.sdram_pkg.all;
use std.textio.all;

-- TODO: check correct RAM initialization

entity sdram_model is
    generic(
        -- Timing Parameters for -75 (PC133) and CAS Latency = 2
        -- FIXME: fix this
        tCAS       : time    := 0 ns;
        tAC        : time    := 6.0 ns; -- access time from clk
        tHZ        : time    := 7.0 ns; -- output high impedance time
        tOH        : time    := 2.7 ns; -- output data hold time
        tMRD       : natural := 2;      -- clock cycles between write to mode register and next command
        tRAS       : time    := 44.0 ns; -- row address strobe (active to precharge)
        tRC        : time    := 66.0 ns; -- row cycle (ref to ref / activate to activate)
        tRCD       : time    := 20.0 ns; -- RAS to CAS delay (active command to read/write command delay time)
        tRP        : time    := 20.0 ns; -- row precharge time (min. time between precharging row and activating new one)
        tRRD       : time    := 15.0 ns; -- bank to bank delay time (min. time between successive ACTIVE commands to different banks)
        tWRa       : time    := 7.5 ns; -- A2 Version - Auto precharge mode only (1 Clk + 7.5 ns)
        tWRp       : time    := 15.0 ns; -- A2 Version - Precharge mode only (15 ns)
        -- FIXME: implement 
        tCKA       : time    := 0 ns;   -- CKE to CLK recovery delay time

        tAH        : time    := 0.8 ns; -- address hold time
        tAS        : time    := 1.5 ns; -- address setup time
        tCH        : time    := 2.5 ns; -- clk high level width
        tCL        : time    := 2.5 ns; -- clk low level width
        tCK        : time    := 10.0 ns; -- clk cycle time
        tDH        : time    := 0.8 ns; -- input data hold time
        tDS        : time    := 1.5 ns; -- input data setup time
        tCKH       : time    := 0.8 ns; -- CKE hold time (clk enable?)
        tCKS       : time    := 1.5 ns; -- CKE setup time
        tCMH       : time    := 0.8 ns; -- command hold time
        tCMS       : time    := 1.5 ns; -- command setup time

        tREF       : time    := 64 ms;  -- refresh cycle time (4096)

        ADDR_BITS  : natural := 12;     -- row address width
        DATA_WIDTH : natural := 16;
        COL_BITS   : natural := 8;      -- page width (column width)

        index      : natural := 0;
        fname      : string  := "ram.srec"; -- File to read from
        LOAD, DUMP : boolean := false
    );
    port(
        dataInOut     : inout std_logic_vector(DATA_WIDTH - 1 downto 0) := (others => 'Z');
        addrIn        : in    unsigned(ADDR_BITS - 1 downto 0)          := (others => '0');
        bankIn        : in    std_logic_vector(1 downto 0)              := "00"; -- bank select
        clkIn         : in    std_logic                                 := '0';
        clkEnableIn   : in    std_logic                                 := '1'; -- clock enable
        csNegIn       : in    std_logic                                 := '1'; -- chip select (neg input)
        rasNegIn      : in    std_logic                                 := '1'; -- row address strobe (neg input)
        casNegIn      : in    std_logic                                 := '1'; -- column address strobe (neg input)
        writeEnableIn : in    std_logic                                 := '1'; -- write enable (neg input)
        dqmIn         : in    std_logic_vector(1 downto 0)              := "00" -- input/output mask
    );
    constant PAGE_WIDTH : natural := 2**COL_BITS;
    pure function burst_to_natural(val : Burst_Length_T) return natural is
    begin
        case val is
            when '1'           => return 1;
            when '2'           => return 2;
            when '4'           => return 4;
            when '8'           => return 8;
            when FullPage      => return PAGE_WIDTH;
            when InvalidLength => return 1;
        end case;
    end function burst_to_natural;
    pure function latency_to_natural(val : Latency_Mode_T) return natural is
    begin
        case val is
            when '2'            => return 2;
            when '3'            => return 3;
            when InvalidLatency => return 2;
        end case;
    end function latency_to_natural;
    subtype Col_Range_T is natural range 2**COL_BITS - 1 downto 0;
    subtype Row_Range_T is natural range 2**ADDR_BITS - 1 downto 0;
    subtype Bank_Range_T is natural range 3 downto 0;
END sdram_model;

architecture behave of sdram_model is
    signal rowAddrStrobe, colAddrStrobe, writeEnable : bit := '0'; -- decoded signals

    signal currCommand : Command_T                                := ModeRegEna; -- decoded currently requested command
    signal modeReg     : std_logic_vector(ADDR_BITS - 1 downto 0) := (others => '0'); -- RAM mode register

    signal clkEnabled          : boolean                := false;
    signal clkInternal, clkRas : std_logic              := '0';
    signal burstMode           : Burst_Length_T         := '1'; -- decoded burst length setting
    signal burstInterleaved    : boolean                := false; -- decoded burst interleaved setting
    signal commandAggregate    : bit_vector(2 downto 0) := "000"; -- temp aggregate signal
    signal latencyMode         : Latency_Mode_T         := '2'; -- decoded latency mode setting
    signal latencyLength       : natural                := 2; -- decoded latency mode as natural number
    signal writeBurstEnabled   : boolean                := false; -- decoded write burst enabled setting
    signal burstLength         : natural                := 1; -- decoded burst length as natural number
    signal prechargeFlag       : boolean                := false; -- addrIn(10) [flag used in various ways, controlling precharge]

    -- Checking internal wires
    signal prechargeCheck        : Array4xBool                      := (others => false);
    signal activeCheck           : Array4xBool                      := (others => false);
    signal Dq_in_chk, Dq_out_chk : boolean                          := false;
    signal bankCheck             : unsigned(1 downto 0)             := "00";
    signal rowCheck              : unsigned(ADDR_BITS - 1 downto 0) := (others => '0');
    signal colCheck              : unsigned(COL_BITS - 1 downto 0)  := (others => '0');

begin
    -- CS# Decode
    with csNegIn select colAddrStrobe <=
        to_bit(casNegIn, '1') when '0',
        '1' when '1',
                      '1' when others;
    with csNegIn select rowAddrStrobe <=
        to_bit(rasNegIn, '1') when '0',
        '1' when '1',
                      '1' when others;
    with csNegIn select writeEnable <=
        to_bit(writeEnableIn, '1') when '0',
        '1' when '1',
                      '1' when others;

    with addrIn(10) select prechargeFlag <=
        true when '1',
        false when others;

    commandAggregate <= (rowAddrStrobe, colAddrStrobe, writeEnable);
    with commandAggregate select currCommand <=
        ModeRegEna when "000",
        ArefEna when "001",
        PrechargeEna when "010",
        ActiveEna when "011",
        WriteEna when "100",
        ReadEna when "101",
        BurstTerm when "110",
        NoOp when "111";

    with modeReg(2 downto 0) select burstMode <=
        '1' when "000",
        '2' when "001",
            '4' when "010",
            '8' when "011",
            FullPage when "111",
            InvalidLength when others;

    burstInterleaved <= toBool(modeReg(3));

    with modeReg(6 downto 4) select latencyMode <=
        '2' when "010",
        '3' when "011",
            InvalidLatency when others;

    writeBurstEnabled <= not toBool(modeReg(9));
    burstLength       <= burst_to_natural(burstMode);
    latencyLength     <= latency_to_natural(latencyMode);

    -- RAS Clock for checking tWR and tRP
    secondaryClkProc : process
        variable Clk0, Clk1 : integer := 0;
    begin
        clkRas <= '1';
        wait for 0.5 ns;
        clkRas <= '0';
        wait for 0.5 ns;
        if Clk0 > 100 or Clk1 > 100 then
            wait;
        else
            if clkIn = '1' and clkEnableIn = '1' then
                Clk0 := 0;
                Clk1 := Clk1 + 1;
            elsif clkIn = '0' and clkEnableIn = '1' then
                Clk0 := Clk0 + 1;
                Clk1 := 0;
            end if;
        end if;
    end process secondaryClkProc;

    -- System Clock
    clkProc : process(clkIn)
    begin
        if rising_edge(clkIn) then
            clkEnabled <= toBool(clkEnableIn);
        end if;
    end process clkProc;

    with clkEnabled select clkInternal <=
        clkIn when true,
        '0' when others;

    initCheckProc : process(clkInternal)
        variable initSucc : boolean := false;
    begin
        if rising_edge(clkInternal) then
            if not initSucc then
                -- FIXME: check initialization
            end if;
        end if;
    end process initCheckProc;

    tmpBlock : block
        --        type Array4xUnsigned is array (BANK_COUNT - 1 downto 0) of unsigned(COL_BITS - 1 downto 0);
        type Col_Pipeline_T is array (3 downto 0) of unsigned(Col_Range_T);
        type Bank_Pipeline_T is array (3 downto 0) of unsigned(1 downto 0);
        type Command_Pipeline_T is array (3 downto 0) of Command_T;
        type Boolean_Pipeline_T is array (3 downto 0) of boolean;
        type Mem_Pipeline_R is record
            cmd           : Command_Pipeline_T;
            colAddr       : Col_Pipeline_T;
            bankAddr      : Bank_Pipeline_T;
            bankPrecharge : Bank_Pipeline_T;
            a10Precharge  : Boolean_Pipeline_T;
        end record Mem_Pipeline_R;
        type Bank_State_T is (Idle, Activating, Precharging, Active);
        type Mem_Bank_R is record
            state          : Bank_State_T;
            rowAddr        : unsigned(ADDR_BITS - 1 downto 0);
            rasCheck       : time;
            rcdCheck       : time;
            rpCheck        : time;
            wrCheckpoint   : time;
            countTime      : time;
            autoPrecharge  : boolean;
            countPrecharge : natural;
            readPrecharge  : boolean;
            writePrecharge : boolean;
        end record Mem_Bank_R;
        type Bank_Array_T is array (3 downto 0) of Mem_Bank_R;

        -- TODO: initial value
        signal pipeline : Mem_Pipeline_R;
        signal memBank  : Bank_Array_T;
    begin
        frontProc : process(clkInternal)
            variable bankPtr, addrPtr : natural;
        begin
            if rising_edge(clkInternal) then
                bankPtr := to_integer(unsigned(bankIn));
                addrPtr := to_integer(unsigned(addrIn));

                -- Internal Command Pipeline
                pipeline.cmd(2 downto 0) <= pipeline.cmd(3 downto 1);
                pipeline.cmd(3)          <= NoOp;

                pipeline.colAddr(2 downto 0) <= pipeline.colAddr(3 downto 1);
                pipeline.colAddr(3)          <= (others => '0');

                pipeline.bankAddr(2 downto 0) <= pipeline.bankAddr(3 downto 1);
                pipeline.bankAddr(3)          <= "00";

                pipeline.bankPrecharge(2 downto 0) <= pipeline.bankPrecharge(3 downto 1);
                pipeline.bankPrecharge(3)          <= "00";

                pipeline.a10Precharge(2 downto 0) <= pipeline.a10Precharge(3 downto 1);
                pipeline.a10Precharge(3)          <= false;

                -- Dqm pipeline for Read
                -- TODO: what to do with this?
                Dqm_reg0 := Dqm_reg1;
                Dqm_reg1 := TO_BITVECTOR(dqmIn);

                -- Read or Write with Auto Precharge Counter
                for i in 0 to 3 loop
                    if memBank(i).autoPrecharge then
                        memBank(i).countPrecharge <= memBank(i).countPrecharge + 1;
                    end if;
                end loop;

                -- Auto Precharge Timer for tWR
                if writeBurstEnabled then
                    for i in 0 to 3 loop
                        if memBank(i).countPrecharge = 1 then
                            memBank(i).countTime <= NOW;
                        end if;
                    end loop;
                else
                    for i in 0 to 3 loop
                        if memBank(i).countPrecharge = burstLength then
                            memBank(i).countTime <= NOW;
                        end if;
                    end loop;
                end if;

                -- tMRD Counter
                MRD_chk := MRD_chk + 1;

                -- tWR Counter
                for i in 0 to 3 loop
                    WR_counter(i) := WR_counter(i) + 1;
                end loop;

                --------------------------------------------------------------------------------
                -- ######## ########   #######  ##    ## ######## ######## ##    ## ########  -- 
                -- ##       ##     ## ##     ## ###   ##    ##    ##       ###   ## ##     ## -- 
                -- ##       ##     ## ##     ## ####  ##    ##    ##       ####  ## ##     ## -- 
                -- ######   ########  ##     ## ## ## ##    ##    ######   ## ## ## ##     ## -- 
                -- ##       ##   ##   ##     ## ##  ####    ##    ##       ##  #### ##     ## -- 
                -- ##       ##    ##  ##     ## ##   ###    ##    ##       ##   ### ##     ## -- 
                -- ##       ##     ##  #######  ##    ##    ##    ######## ##    ## ########  -- 
                --------------------------------------------------------------------------------

                case currCommand is
                    -- Active Block (latch Bank and Row Address)
                    when ActiveEna =>
                        --                        if memBank(bankPtr).precharged then
                        if memBank(bankPtr).state = Idle then
                            memBank(bankPtr).state    <= Activating;
                            --                            memBank(bankPtr).active     := true;
                            --                            memBank(bankPtr).precharged := false;
                            memBank(bankPtr).rowAddr  <= unsigned(addrIn);
                            memBank(bankPtr).rcdCheck <= NOW;
                            memBank(bankPtr).rasCheck <= NOW;
                            -- Precharge to Active Bank 0
                            assert (NOW - memBank(bankPtr).rpCheck >= tRP)
                            report "tRP violation during Activate Bank " & natural'image(bankPtr)
                            severity warning;
                        else
                            report "Bank " & natural'image(bankPtr) & " is not Idle, cannot be Activated"
                            severity warning;
                        end if;

                        -- Active Bank A to Active Bank B
                        if ((Previous_bank /= unsigned(bankIn)) and (NOW - RRD_chk < tRRD)) then
                            report "tRRD violation during Activate"
                            severity warning;
                        end if;
                        -- LMR to ACT
                        assert (MRD_chk >= tMRD)
                        report "tMRD violation during Activate"
                        severity warning;
                        -- AutoRefresh to Activate
                        assert (NOW - RC_chk >= tRC)
                        report "tRC violation during Activate"
                        severity warning;
                        -- Record variable for checking violation
                        RRD_chk       := NOW;
                        Previous_bank := unsigned(bankIn);

                    -- Auto Refresh
                    when ArefEna =>
                        -- Auto Refresh to Auto Refresh
                        assert (NOW - RC_chk >= tRC)
                        report "tRC violation during Auto Refresh"
                        severity warning;
                        -- Precharge to Auto Refresh
                        assert (NOW - memBank(0).rpCheck >= tRP or NOW - memBank(1).rpCheck >= tRP or NOW - memBank(2).rpCheck >= tRP or NOW - memBank(3).rpCheck >= tRP)
                        report "tRP violation during Auto Refresh"
                        severity warning;
                        -- All banks must be idle before refresh
                        -- FIXME: is this right?
                        --                assert Pc_b3 = '0' OR Pc_b2 = '0' OR Pc_b1 = '0' OR Pc_b0 = '0'
                        assert memBank(0).state = Idle and memBank(1).state = Idle and memBank(2).state = Idle and memBank(3).state = Idle
                        report "All banks must be Precharge before Auto Refresh"
                        severity warning;
                        -- Record current tRC time
                        RC_chk := NOW;

                    -- Burst Terminate
                    when BurstTerm =>
                        -- Terminate a Write immediately
                        if dataInEnable then
                            dataInEnable := false;
                        end if;
                        -- Terminate a Read depending on CAS Latency
                        if latencyMode = '3' then
                            cmdPipeline(2) := BST;
                        elsif latencyMode = '2' then
                            cmdPipeline(1) := BST;
                        end if;

                    -- Load Mode Register
                    when ModeRegEna =>
                        modeReg <= std_logic_vector(addrIn);

                        assert memBank(0).state = Idle and memBank(1).state = Idle and memBank(2).state = Idle and memBank(3).state = Idle
                        report "All bank must be Precharged before Load Mode Register"
                        severity warning;
                        -- REF to LMR
                        assert (NOW - RC_chk >= tRC)
                        report "tRC violation during Load Mode Register"
                        severity warning;
                        -- LMR to LMR
                        assert (MRD_chk >= tMRD)
                        report "tMRD violation during Load Mode Register"
                        severity warning;
                        -- Record current tMRD time
                        MRD_chk := 0;

                    -- Precharge Block
                    when PrechargeEna =>
                        if prechargeFlag then -- precharge all banks
                            for i in Bank_Range_T loop
                                memBank(i).state   <= Precharging;
                                memBank(i).rpCheck <= NOW;
                            end loop;
                            -- Activate to Precharge all banks
                            --                        assert ((NOW - RAS_chk0 >= tRAS) or (NOW - RAS_chk1 >= tRAS))
                            assert (NOW - memBank(0).rasCheck >= tRAS) and (NOW - memBank(1).rasCheck >= tRAS) and (NOW - memBank(2).rasCheck >= tRAS) and (NOW - memBank(3).rasCheck >= tRAS)
                            report "tRAS violation during Precharge all banks"
                            severity warning;
                            -- tWR violation check for Write
                            if ((NOW - memBank(0).wrCheckpoint < tWRp) or (NOW - memBank(1).wrCheckpoint < tWRp) or (NOW - memBank(2).wrCheckpoint < tWRp) OR (NOW - memBank(3).wrCheckpoint < tWRp)) then
                                report "tWR violation during Precharge ALL banks"
                                severity warning;
                            end if;
                        else            -- precharge only selected bank
                            assert memBank(bankPtr).state = Active
                            report "Tried to Precharge row that's not Active"
                            severity warning;

                            memBank(bankPtr).state   <= Precharging;
                            memBank(bankPtr).rpCheck <= NOW;

                            -- Activate to Precharge bank
                            assert (NOW - memBank(bankPtr).rasCheck >= tRAS)
                            report "tRAS violation during Precharge bank " & natural'image(bankPtr)
                            severity warning;
                            -- tWR violation check for Write
                            assert (NOW - memBank(bankPtr).wrCheckpoint >= tWRp)
                            report "tWR violation during Precharge"
                            severity warning;
                        end if;

                        -- Terminate a Write Immediately (if same bank or all banks)
                        if (dataInEnable and (Bank = unsigned(bankIn) or prechargeFlag)) then
                            dataInEnable := false;
                        end if;

                        -- Precharge Command Pipeline for READ
                        if latencyMode = '3' then
                            pipeline.cmd(2)           <= PrechargeEna;
                            pipeline.bankPrecharge(2) <= unsigned(bankIn);
                            pipeline.a10Precharge(2)  := TO_BIT(addrIn(10));
                        elsif latencyMode = '2' then
                            pipeline.cmd(1)          <= PrechargeEna;
                            bankPrechargePipeline(1) := unsigned(bankIn);
                            A10_precharge(1)         := TO_BIT(addrIn(10));
                        end if;

                    -- Read, Write, Column Latch
                    when ReadEna | WriteEna =>
                        -- Check to see if bank is open (ACT) for Read or Write
                        assert not memBank(bankPtr).precharged
                        report "Cannot Read or Write - Bank is not Activated"
                        severity warning;

                        -- Activate to Read or Write
                        assert NOW - memBank(bankPtr).rcdCheck >= tRCD
                        report "tRCD violation during Read or Write to Bank " & natural'image(bankPtr)
                        severity warning;

                        -- Read Command
                        if currCommand = ReadEna then
                            -- CAS Latency Pipeline
                            if latencyMode = '3' then
                                -- TODO: find out what this flag controls during READ
                                if prechargeFlag then
                                    cmdPipeline(2) := READ_A;
                                else
                                    cmdPipeline(2) := READ;
                                end if;
                                colAddrPipeline(2)  := unsigned(addrIn(COL_BITS - 1 DOWNTO 0));
                                bankAddrPipeline(2) := unsigned(bankIn);
                            elsif latencyMode = '2' then
                                if prechargeFlag then
                                    cmdPipeline(1) := READ_A;
                                else
                                    cmdPipeline(1) := READ;
                                end if;
                                colAddrPipeline(1)  := unsigned(addrIn(COL_BITS - 1 DOWNTO 0));
                                bankAddrPipeline(1) := unsigned(bankIn);
                            end if;

                            -- Read intterupt a Write (terminate Write immediately)
                            if dataInEnable then
                                dataInEnable := false;
                            end if;

                        -- Write Command
                        elsif currCommand = WriteEna then
                            if prechargeFlag then
                                cmdPipeline(0) := WRITE_A;
                            else
                                cmdPipeline(0) := WRITE;
                            end if;
                            colAddrPipeline(0)  := unsigned(addrIn(COL_BITS - 1 downto 0));
                            bankAddrPipeline(0) := unsigned(bankIn);

                            -- Write intterupts a Write (terminate Write immediately)
                            if dataInEnable then
                                dataInEnable := false;
                            end if;

                            -- Write interrupts a Read (terminate Read immediately)
                            if dataOutEnable then
                                dataOutEnable := false;
                            end if;
                        end if;

                        -- Interrupt a Write with Auto Precharge
                        if memBank(to_integer(RW_interrupt_bank)).autoPrecharge and memBank(to_integer(RW_interrupt_bank)).writePrecharge then
                            RW_interrupt_write(TO_INTEGER(RW_Interrupt_Bank)) := '1';
                        end if;

                        -- Interrupt a Read with Auto Precharge
                        if memBank(to_integer(RW_interrupt_bank)).autoPrecharge and memBank(to_integer(RW_interrupt_bank)).readPrecharge then
                            RW_interrupt_read(TO_INTEGER(RW_Interrupt_Bank)) := '1';
                        end if;

                        -- Read or Write with Auto Precharge
                        if prechargeFlag then
                            bankPtr                         := to_integer(unsigned(bankIn));
                            memBank(bankPtr).autoPrecharge  := true;
                            memBank(bankPtr).countPrecharge := 0;
                            RW_Interrupt_Bank               := unsigned(bankIn);
                            if currCommand = ReadEna then
                                memBank(bankPtr).readPrecharge := true;
                            elsif currCommand = WriteEna then
                                memBank(bankPtr).writePrecharge := true;
                            end if;
                        end if;

                    when NoOp =>
                        null;

                    when CmdInhibit =>
                        null;

                end case;

            end if;
        end process frontProc;

        backProc : process(clkInternal)
        begin
        end process backProc;

        dataProc : process
            type Mem_Data_R is record
                data  : bit_vector(DATA_WIDTH - 1 downto 0);
                inUse : boolean;
            end record Mem_Data_R;
            type Mem_Row_T is array (Col_Range_T) of Mem_Data_R;
            type Mem_Row_Ptr_T is access Mem_Row_T;
            type Mem_Bank_T is array (Row_Range_T) of Mem_Row_Ptr_T;
            type Mem_Bank_Array_T is array (Bank_Range_T) of Mem_Bank_T;

            variable bankData : Mem_Bank_Array_T := (others => (others => NULL));

            -- Load and Dumb variables
            file file_load                      : text open read_mode is fname; -- Data load
            file file_dump                      : text open write_mode is "dumpdata.txt"; -- Data dump
            variable currBank, currRow, currCol : natural;
            --            variable data_load                  : std_logic_vector(DATA_WIDTH - 1 downto 0);
            --            variable good_load                  : boolean;
            variable l                          : line;
            variable ch                         : character;
            variable rectype                    : bit_vector(3 downto 0);
            variable recaddr                    : std_logic_vector(31 downto 0);
            variable reclen                     : bit_vector(7 downto 0);
            variable recdata                    : bit_vector(0 to 16 * 8 - 1);

            -- initialize empty rows
            procedure initMem(bankSel : natural; rowSel : natural) is
                variable currBank : Mem_Bank_T := bankData(bankSel);
            begin
                if currBank(rowSel) = null then -- check if row is uninitialized
                    currBank(rowSel) := new Mem_Row_T; -- open row for access
                    for i in Col_Range_T loop
                        currBank(rowSel)(i) := (data => (others => '0'), inUse => false); -- fill with zeroes
                    end loop;
                end if;
            end procedure initMem;
        begin
            if LOAD then
                report "Reading memory array from file. This operation may take several minutes. Please wait..." severity note;
                while not endfile(file_load) loop
                    readline(file_load, l);
                    read(l, ch);
                    if (ch /= 'S') or (ch /= 's') then
                        hread(l, rectype);
                        hread(l, reclen);
                        recaddr := (others => '0');

                        case rectype is
                            when "0001" =>
                                hread(l, recaddr(15 downto 0));
                            when "0010" =>
                                hread(l, recaddr(23 downto 0));
                            when "0011" =>
                                hread(l, recaddr);
                                recaddr(31 downto 24) := (others => '0');
                            when others => next;
                        end case;

                        if L.all'length * 4 < recdata'length then
                            hread(l, recdata(0 to L.all'length * 4 - 1));
                        else
                            hread(l, recdata);
                        end if;

                        if index < 32 then
                            currBank := to_integer(unsigned(recaddr(25 downto 24)));
                            currRow  := to_integer(unsigned(recaddr(23 downto 11)));
                            currCol  := to_integer(unsigned(recaddr(10 downto 2)));
                            initMem(currBank, currRow);
                            for i in 0 to 3 loop
                                bankData(currBank + i)(currRow)(currCol) := (inUse => true, data => recdata(i * 32 + index to i * 32 + index + 15));
                            end loop;
                        else
                            currBank := to_integer(unsigned(recaddr(26 downto 25)));
                            currRow  := to_integer(unsigned(recaddr(24 downto 12)));
                            currCol  := to_integer(unsigned(recaddr(11 downto 3)));
                            initMem(currBank, currRow);
                            for i in 0 to 3 loop
                                bankData(currBank)(currRow)(currCol + i) := (inUse => true, data => recdata(i * 64 + index - 32 to i * 64 + index - 32 + 15));
                            end loop;
                        end if;
                    end if;
                end loop;
            end if;

            if DUMP then
                report "Writing memory array to file.  This operation may take several minutes.  Please wait..."
                severity note;
                write(l, string'("# Micron Technology, Inc. (FILE DUMP / MEMORY DUMP)"));
                writeline(file_dump, l);
                write(l, string'("# BA ROWS          COLS      DQ"));
                writeline(file_dump, l);
                write(l, string'("# -- ------------- --------- ----------------"));
                writeline(file_dump, l);

                -- dump all banks
                for bank in 0 to BANK_COUNT - 1 loop
                    for row in 0 to 2**ADDR_BITS - 1 loop
                        -- Check if ROW is NULL
                        if bankData(bank)(row) /= NULL then
                            for col in 0 to 2**COL_BITS - 1 loop
                                -- Check if COL is NULL
                                next when not bankData(bank)(row)(col).inUse;
                                write(l, to_string(to_unsigned(bank, 2)), right, 4);
                                write(l, To_BitVector(Conv_Std_Logic_Vector(row, ADDR_BITS)), right, ADDR_BITS + 1);
                                write(l, To_BitVector(Conv_std_Logic_Vector(col, COL_BITS)), right, COL_BITS + 1);
                                write(l, bankData(bank)(row)(col).data, right, DATA_WIDTH + 1);
                                writeline(file_dump, l);
                            end loop;
                        end if;
                    end loop;
                end loop;
            end if;

            wait until rising_edge(clkInternal);
        end process dataProc;
    end block tmpBlock;

    stateProc : process
        -- NOTE: The extra bits in RAM_TYPE is for checking memory access.  A logic 1 means
        --       the location is in use.  This will be checked when doing memory DUMP.

        variable bankPtr : natural := 0;

        variable memBank            : Bank_Array_T                    := (others => (storage => (others => NULL), precharged => false, active => false, rasCheck => NOW, rcdCheck => NOW, rpCheck => NOW, rowAddr => (others => '0'), wrCheckpoint => NOW, autoPrecharge => false, countPrecharge => 0, readPrecharge => false, writePrecharge => false, countTime => NOW));
        variable rowIndex, colIndex : natural                         := 0;
        variable Dq_temp            : bit_vector(DATA_WIDTH downto 0) := (others => '0');

        variable Dqm_reg0, Dqm_reg1 : bit_vector(1 downto 0) := "00";

        variable Bank, Previous_bank : unsigned(1 downto 0)             := "00";
        variable Col_brst            : unsigned(COL_BITS - 1 downto 0)  := (others => '0');
        variable currRow             : unsigned(ADDR_BITS - 1 downto 0) := (others => '0');
        variable currCol             : unsigned(COL_BITS - 1 downto 0)  := (others => '0');
        variable burstCounter        : natural                          := 0;

        variable bankPrechargePipeline : Array4xUnsigned;
        variable A10_precharge         : Array4xB             := ('0' & '0' & '0' & '0');
        variable RW_interrupt_read     : Array4xB             := ('0' & '0' & '0' & '0');
        variable RW_interrupt_write    : Array4xB             := ('0' & '0' & '0' & '0');
        variable RW_interrupt_bank     : unsigned(1 downto 0) := "00";

        variable dataInEnable, dataOutEnable : boolean := false;

        -- Timing Check
        variable MRD_chk         : natural  := 0;
        variable WR_counter      : Array4xI := (0 & 0 & 0 & 0);
        variable WR_time         : Array4xT := (0 ns & 0 ns & 0 ns & 0 ns);
        --        variable WR_chkp         : Array4xT := (0 ns & 0 ns & 0 ns & 0 ns);
        variable RC_chk, RRD_chk : time     := 0 ns;

        -- Burst Counter
        procedure decodeBurst is
            variable colPtr           : natural                         := 0;
            variable Col_vec, colTemp : unsigned(COL_BITS - 1 downto 0) := (others => '0');
        begin
            -- Advance Burst Counter
            burstCounter := burstCounter + 1;

            -- Burst Type
            if burstInterleaved then
                Col_vec    := to_unsigned(burstCounter, Col_vec'length);
                colTemp(2) := Col_vec(2) xor Col_brst(2);
                colTemp(1) := Col_vec(1) xor Col_brst(1);
                colTemp(0) := Col_vec(0) xor Col_brst(0);
            else
                colPtr  := to_integer(currCol);
                colPtr  := colPtr + 1;
                colTemp := to_unsigned(colPtr, colTemp'length);
            end if;

            case burstMode is
                when '1' =>
                    currCol := colTemp;

                when '2' =>
                    currCol(0) := colTemp(0);

                when '4' =>
                    currCol(1 downto 0) := colTemp(1 downto 0);

                when '8' =>
                    currCol(2 downto 0) := colTemp(2 downto 0);

                when FullPage =>
                    -- FIXME: what to do with full page?
                    null;

                when InvalidLength =>
                    report "Invalid burst length" severity failure;
            end case;

            -- Burst Read Single Write
            if not writeBurstEnabled and dataInEnable then
                dataInEnable := false;
            end if;

            -- Data counter
            -- FIXME: fix FullPage
            if burstMode /= InvalidLength and burstMode /= FullPage then
                if burstCounter >= burstLength then
                    if dataInEnable then
                        dataInEnable := false;
                    elsif dataOutEnable then
                        dataOutEnable := false;
                    end if;
                end if;
            end if;
        end procedure decodeBurst;

    begin
        wait on clkInternal, clkRas;
        if rising_edge(clkInternal) and Load = '0' and Dump = '0' then

            -----------------------------------------------------------------------
            -- ########     ###     ######  ##    ## ######## ##    ## ########  --
            -- ##     ##   ## ##   ##    ## ##   ##  ##       ###   ## ##     ## --
            -- ##     ##  ##   ##  ##       ##  ##   ##       ####  ## ##     ## --
            -- ########  ##     ## ##       #####    ######   ## ## ## ##     ## --
            -- ##     ## ######### ##       ##  ##   ##       ##  #### ##     ## --
            -- ##     ## ##     ## ##    ## ##   ##  ##       ##   ### ##     ## --
            -- ########  ##     ##  ######  ##    ## ######## ##    ## ########  --
            -----------------------------------------------------------------------

            -- Read with AutoPrecharge Calculation
            --      The device start internal precharge when:
            --          1.  BL/2 cycles after command
            --      and 2.  Meet tRAS requirement
            --       or 3.  Interrupt by a Read or Write (with or without Auto Precharge)
            for i in 0 to BANK_COUNT loop
                if memBank(i).autoPrecharge and memBank(i).readPrecharge then
                    if ((NOW - memBank(i).rasCheck >= tRAS) and (memBank(i).countPrecharge >= burstLength)) or (RW_interrupt_read(i) = '1') then
                        memBank(i).precharged    := true;
                        memBank(i).active        := false;
                        memBank(i).rpCheck       := NOW;
                        memBank(i).autoPrecharge := false;
                        memBank(i).readPrecharge := false;
                        RW_interrupt_read(0)     := '0';
                    end if;
                end if;
            end loop;

            -- Internal Precharge or Bst
            if cmdPipeline(0) = PRECH then -- PRECH terminate a read if same bank or all banks
                if bankPrechargePipeline(0) = Bank or A10_precharge(0) = '1' then
                    if dataOutEnable then
                        dataOutEnable := false;
                    end if;
                end if;
            elsif cmdPipeline(0) = BST then -- BST terminate a read regardless of bank
                if dataOutEnable then
                    dataOutEnable := false;
                end if;
            end if;

            if not dataOutEnable then
                dataInOut <= transport (others => 'Z') after tOH;
            end if;

            -- Detect Read or Write Command
            if cmdPipeline(0) = READ OR cmdPipeline(0) = READ_A then
                Bank          := bankAddrPipeline(0);
                currCol       := colAddrPipeline(0);
                Col_brst      := colAddrPipeline(0);
                currRow       := memBank(to_integer(bankAddrPipeline(0))).rowAddr;
                burstCounter  := 0;
                dataInEnable  := false;
                dataOutEnable := true;
            elsif cmdPipeline(0) = WRITE OR cmdPipeline(0) = WRITE_A then
                Bank          := bankAddrPipeline(0);
                currCol       := colAddrPipeline(0);
                Col_brst      := colAddrPipeline(0);
                currRow       := memBank(to_integer(bankAddrPipeline(0))).rowAddr;
                burstCounter  := 0;
                dataInEnable  := true;
                dataOutEnable := false;
            end if;

            -- DQ (Driver / Receiver)
            rowIndex := TO_INTEGER(currRow);
            colIndex := TO_INTEGER(currCol);
            if dataInEnable then
                if dqmIn /= "11" then
                    initMem(Bank, rowIndex);
                    Dq_temp                                               := memBank(to_integer(Bank)).storage(rowIndex)(colIndex).data;
                    if dqmIn = "01" then
                        Dq_temp(15 downto 8) := TO_BITVECTOR(dataInOut(15 downto 8));
                    elsif dqmIn = "10" then
                        Dq_temp(7 downto 0) := TO_BITVECTOR(dataInOut(7 downto 0));
                    else
                        Dq_temp(15 downto 0) := TO_BITVECTOR(dataInOut(15 downto 0));
                    end if;
                    memBank(to_integer(Bank)).storage(rowIndex)(colIndex) := (inUse => true, data => Dq_temp);
                    memBank(to_integer(Bank)).wrCheckpoint                := NOW;
                    WR_counter(to_integer(Bank))                          := 0;
                END if;
                decodeBurst;
            elsif dataOutEnable then
                if Dqm_reg0 /= "11" then
                    initMem(Bank, rowIndex);
                    Dq_temp := memBank(to_integer(Bank)).storage(rowIndex)(colIndex).data;
                    if Dqm_reg0 = "00" then
                        dataInOut(15 downto 0) <= transport to_StdLogicVector(Dq_temp(15 downto 0)) after tAC;
                    elsif Dqm_reg0 = "01" then
                        dataInOut(15 downto 8) <= transport to_StdLogicVector(Dq_temp(15 downto 8)) after tAC;
                        dataInOut(7 downto 0)  <= transport (others => 'Z') after tAC;
                    elsif Dqm_reg0 = "10" then
                        dataInOut(15 downto 8) <= transport (others => 'Z') after tAC;
                        dataInOut(7 downto 0)  <= transport to_StdLogicVector(Dq_temp(7 downto 0)) after tAC;
                    end if;
                else
                    dataInOut <= transport (others => 'Z') after tHZ;
                end if;
                decodeBurst;
            end if;

        end if;

        -- Write with AutoPrecharge Calculation
        --      The device start internal precharge when:
        --          1.  tWR cycles after command
        --      and 2.  Meet tRAS requirement
        --       or 3.  Interrupt by a Read or Write (with or without Auto Precharge)
        for i in 0 to BANK_COUNT loop
            if memBank(i).autoPrecharge and memBank(i).writePrecharge then
                if (NOW - memBank(i).rasCheck >= tRAS and (((writeBurstEnabled or burstLength = 1) and memBank(i).countPrecharge >= 1 and memBank(i).countTime >= tWRa) or (memBank(i).countPrecharge >= burstLength and NOW - memBank(i).countTime >= tWRa))) or (RW_interrupt_write(i) = '1' and WR_counter(i) >= 1 and NOW - WR_time(i) >= tWRa) then
                    memBank(i).autoPrecharge  := false;
                    memBank(i).writePrecharge := false;
                    RW_interrupt_write(i)     := '0';
                    memBank(i).precharged     := true;
                    memBank(i).active         := false;
                    memBank(i).rpCheck        := NOW;
                    report "Start Internal Precharge Bank " & natural'image(i)
                    severity note;
                end if;
            end if;
        end loop;

        --        if ((Auto_precharge(3) = '1') AND (Write_precharge(3) = '1')) then
        --            if (((NOW - RAS_chk3 >= tRAS) AND (((Burst_length_1 = '1' OR Write_burst_mode = '1') AND Count_precharge(3) >= 1 AND NOW - Count_time(3) >= tWRa) OR (Burst_length_2 = '1' AND Count_precharge(3) >= 2 AND NOW - Count_time(3) >= tWRa) OR (Burst_length_4 = '1' AND Count_precharge(3) >= 4 AND NOW - Count_time(3) >= tWRa) OR (Burst_length_8 = '1' AND Count_precharge(3) >= 8 AND NOW - Count_time(3) >= tWRa))) OR (RW_interrupt_write(0) = '1' AND WR_counter(0) >= 1 AND NOW - WR_time(3) >= tWRa)) then
        --                Auto_precharge(3)     := '0';
        --                Write_precharge(3)    := '0';
        --                RW_interrupt_write(3) := '0';
        --                Pc_b3                 := '1';
        --                Act_b3                := '0';
        --                RP_chk3               := NOW;
        --            END if;
        --        END if;

        -- Checking internal wires (Optional for debug purpose)
        for i in 0 to BANK_COUNT loop
            prechargeCheck(i) <= memBank(i).precharged;
            activeCheck(i)    <= memBank(i).active;
        end loop;
        Dq_in_chk  <= dataInEnable;
        Dq_out_chk <= dataOutEnable;
        bankCheck  <= Bank;
        rowCheck   <= currRow;
        colCheck   <= currCol;
    end process stateProc;

    -- Clock timing checks
    --    Clock_check : PROCESS
    --        VARIABLE Clk_low, Clk_high : TIME := 0 ns;
    --    BEGIN       
    --        WAIT ON Clk;
    --        if (Clk = '1' AND NOW >= 10 ns) then
    --            assert (NOW - Clk_low >= tCL)
    --                report "tCL violation"
    --                severity warning;
    --            assert (NOW - Clk_high >= tCK)
    --                report "tCK violation"
    --                severity warning;
    --            Clk_high := NOW;
    --        elsif (Clk = '0' AND NOW /= 0 ns) then
    --            assert (NOW - Clk_high >= tCH)
    --                report "tCH violation"
    --                severity warning;
    --            Clk_low := NOW;
    --        END if;
    --    END PROCESS;    

    -- Setup timing checks
    setupChecks : process
    begin
        wait;
        wait on clkIn;
        if clkIn = '1' then
            assert (clkEnableIn'last_event >= tCKS) --'
            report "CKE Setup time violation -- tCKS"
            severity warning;
            assert (csNegIn'last_event >= tCMS) --'
            report "CS# Setup time violation -- tCMS"
            severity warning;
            assert (casNegIn'last_event >= tCMS) --'
            report "CAS# Setup time violation -- tCMS"
            severity warning;
            assert (rasNegIn'last_event >= tCMS) --'
            report "RAS# Setup time violation -- tCMS"
            severity warning;
            assert (writeEnableIn'last_event >= tCMS) --'
            report "WE# Setup time violation -- tCMS"
            severity warning;
            assert (dqmIn'last_event >= tCMS) --'
            report "Dqm Setup time violation -- tCMS"
            severity warning;
            assert (addrIn'last_event >= tAS) --'
            report "ADDR Setup time violation -- tAS"
            severity warning;
            assert (bankIn'last_event >= tAS) --'
            report "BA Setup time violation -- tAS"
            severity warning;
            assert (dataInOut'last_event >= tDS) --'
            report "Dq Setup time violation -- tDS"
            severity warning;
        end if;
    end process setupChecks;

    -- Hold timing checks
    holdChecks : process
    begin
        wait;
        wait on clkIn'delayed(tCKH), clkIn'delayed(tCMH), clkIn'delayed(tAH), clkIn'delayed(tDH);
        if clkIn'delayed(tCKH) = '1' then --'
            assert (clkEnableIn'last_event > tCKH)
            report "CKE Hold time violation -- tCKH"
            severity warning;
        end if;
        if clkIn'delayed(tCMH) = '1' then
            assert (csNegIn'last_event > tCMH)
            report "CS# Hold time violation -- tCMH"
            severity warning;
            assert (casNegIn'last_event > tCMH)
            report "CAS# Hold time violation -- tCMH"
            severity warning;
            assert (rasNegIn'last_event > tCMH)
            report "RAS# Hold time violation -- tCMH"
            severity warning;
            assert (writeEnableIn'last_event > tCMH)
            report "WE# Hold time violation -- tCMH"
            severity warning;
            assert (dqmIn'last_event > tCMH)
            report "Dqm Hold time violation -- tCMH"
            severity warning;
        end if;
        if clkIn'delayed(tAH) = '1' then
            assert (addrIn'last_event > tAH)
            report "ADDR Hold time violation -- tAH"
            severity warning;
            assert (bankIn'last_event > tAH)
            report "BA Hold time violation -- tAH"
            severity warning;
        end if;
        if clkIn'delayed(tDH) = '1' then
            assert (dataInOut'last_event > tDH)
            report "Dq Hold time violation -- tDH"
            severity warning;
        end if;
    end process holdChecks;
end behave;

-- pragma translate_on