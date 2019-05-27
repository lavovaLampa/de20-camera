
--*****************************************************************************
--
-- Micron Semiconductor Products, Inc.
--
-- Copyright 1997, Micron Semiconductor Products, Inc.
-- All rights reserved.
--
--*****************************************************************************

-- pragma translate_off

-----------------------------------------------------------------------------------------
--
--     File Name: MT48LC16M16A2.VHD
--       Version: 0.0g
--          Date: June 29th, 2000
--         Model: Behavioral
--     Simulator: Model Technology (PC version 5.3 PE)
--
--  Dependencies: None
--
--        Author: Son P. Huynh
--         Email: sphuynh@micron.com
--         Phone: (208) 368-3825
--       Company: Micron Technology, Inc.
--   Part Number: MT48LC16M16A2 (4Mb x 16 x 4 Banks)
--
--   Description: Micron 256Mb SDRAM
--
--    Limitation: - Doesn't check for 4096-cycle refresh        --'
--
--          Note: - Set simulator resolution to "ps" accuracy
--
--    Disclaimer: THESE DESIGNS ARE PROVIDED "AS IS" WITH NO WARRANTY 
--                WHATSOEVER AND MICRON SPECifICALLY DISCLAIMS ANY 
--                IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR
--                A PARTICULAR PURPOSE, OR AGAINST INFRINGEMENT.
--
--                Copyright (c) 1998 Micron Semiconductor Products, Inc.
--                All rights researved
--
--  Rev   Author          Phone         Date        Changes
--  ----  ----------------------------  ----------  -------------------------------------
--  0.0g  Son Huynh       208-368-3825  06/29/2000  Add Load/Dump memory array
--        Micron Technology Inc.                    Modify tWR + tRAS timing check
--
--  0.0f  Son Huynh       208-368-3825  07/08/1999  Fix tWR = 1 Clk + 7.5 ns (Auto)
--        Micron Technology Inc.                    Fix tWR = 15 ns (Manual)
--                                                  Fix tRP (Autoprecharge to AutoRefresh)
--
--  0.0c  Son P. Huynh    208-368-3825  04/08/1999  Fix tWR + tRP in Write with AP
--        Micron Technology Inc.                    Fix tRC check in Load Mode Register
--
--  0.0b  Son P. Huynh    208-368-3825  01/06/1998  Derive from 64Mb SDRAM model
--        Micron Technology Inc.
--
-----------------------------------------------------------------------------------------

library ieee;
use std.textio.all;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.sdram_pkg.all;
use std.textio.all;

--ENTITY mt48lc16m16a2 IS
entity sdram_model is
    generic(
        -- Timing Parameters for -75 (PC133) and CAS Latency = 2
        tAC        : time    := 6.0 ns;
        tHZ        : time    := 7.0 ns;
        tOH        : time    := 2.7 ns;
        tMRD       : natural := 2;      -- 2 Clk Cycles
        tRAS       : time    := 44.0 ns;
        tRC        : time    := 66.0 ns;
        tRCD       : time    := 20.0 ns;
        tRP        : time    := 20.0 ns;
        tRRD       : time    := 15.0 ns;
        tWRa       : time    := 7.5 ns; -- A2 Version - Auto precharge mode only (1 Clk + 7.5 ns)
        tWRp       : time    := 15.0 ns; -- A2 Version - Precharge mode only (15 ns)

        tAH        : time    := 0.8 ns;
        tAS        : time    := 1.5 ns;
        tCH        : time    := 2.5 ns;
        tCL        : time    := 2.5 ns;
        tCK        : time    := 10.0 ns;
        tDH        : time    := 0.8 ns;
        tDS        : time    := 1.5 ns;
        tCKH       : time    := 0.8 ns;
        tCKS       : time    := 1.5 ns;
        tCMH       : time    := 0.8 ns;
        tCMS       : time    := 1.5 ns;
        ADDR_BITS  : natural := 13;
        DATA_WIDTH : natural := 16;
        COL_BITS   : natural := 9;      -- page width?
        index      : natural := 0;
        fname      : string  := "ram.srec" -- File to read from
    );
    port(
        dataInOut     : inout std_logic_vector(DATA_WIDTH - 1 downto 0) := (others => 'Z');
        addrIn        : in    std_logic_vector(ADDR_BITS - 1 downto 0)  := (others => '0');
        baIn          : in    std_logic_vector(1 downto 0)              := "00"; -- bank select
        clkIn         : in    std_logic                                 := '0';
        clkEnableIn   : in    std_logic                                 := '1'; -- clock enable
        csNegIn       : in    std_logic                                 := '1'; -- chip select (neg input)
        rasNegIn      : in    std_logic                                 := '1'; -- row address strobe (neg input)
        casNegIn      : in    std_logic                                 := '1'; -- column address strobe (neg input)
        writeEnableIn : in    std_logic                                 := '1'; -- write enable (neg input)
        dqmIn         : in    std_logic_vector(1 downto 0)              := "00" -- input/output mask
    );
END sdram_model;

architecture behave of sdram_model is
    type Array4xCBV is array (4 downto 0) of bit_vector(COL_BITS - 1 downto 0);
    signal currOp                                    : State_T                            := NOP;
    signal modeReg                                   : bit_vector(ADDR_BITS - 1 downto 0) := (others => '0');
    --    SIGNAL Active_enable, Aref_enable, Burst_term                         : bit                                := '0';
    --    SIGNAL Mode_reg_enable, Prech_enable, Read_enable, Write_enable       : bit                                := '0';
    --    SIGNAL Burst_length_1, Burst_length_2, Burst_length_4, Burst_length_8 : bit                                := '0';
    --    SIGNAL Cas_latency_2, Cas_latency_3                                   : bit                                := '0';
    signal rowAddrStrobe, colAddrStrobe, writeEnable : bit                                := '0';
    --    SIGNAL Write_burst_mode                          : bit                                := '0';
    signal RAS_clk, Sys_clk, CkeZ                    : bit                                := '0';
    signal currCommand                               : Command_T                          := ModeRegEna;
    signal burstMode                                 : Burst_Length_T                     := '1';
    signal burstInterleaved                          : boolean                            := false;
    signal commandAggregate                          : bit_vector(2 downto 0)             := "000";
    signal latencyMode                               : Latency_Mode_T                     := '2';
    signal writeBurstEnabled                         : boolean                            := false;
    signal burstLength                               : natural                            := 1;

    -- Checking internal wires
    signal prechargeCheck        : bit_vector(3 downto 0)             := "0000";
    signal activeCheck           : bit_vector(3 downto 0)             := "0000";
    signal Dq_in_chk, Dq_out_chk : bit                                := '0';
    signal bankCheck             : bit_vector(1 downto 0)             := "00";
    signal rowCheck              : bit_vector(ADDR_BITS - 1 downto 0) := (others => '0');
    signal colCheck              : bit_vector(COL_BITS - 1 downto 0)  := (others => '0');

begin
    -- CS# Decode
    with csNegIn SELECT colAddrStrobe <=
        TO_BIT(casNegIn, '1') WHEN '0',
        '1' WHEN '1',
                      '1' WHEN OTHERS;
    WITH csNegIn SELECT rowAddrStrobe <=
        TO_BIT(rasNegIn, '1') WHEN '0',
        '1' WHEN '1',
                      '1' WHEN OTHERS;
    WITH csNegIn SELECT writeEnable <=
        TO_BIT(writeEnableIn, '1') WHEN '0',
        '1' WHEN '1',
                      '1' WHEN OTHERS;

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
    burstLength       <= burstToNatural(burstMode);

    -- RAS Clock for checking tWR and tRP
    secondaryClkProc : process
        variable Clk0, Clk1 : integer := 0;
    begin
        RAS_clk <= '1';
        wait for 0.5 ns;
        RAS_clk <= '0';
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
        if clkIn'LAST_VALUE = '0' and clkIn = '1' then
            CkeZ <= TO_BIT(clkEnableIn, '1');
        end if;
        Sys_clk <= CkeZ and TO_BIT(clkIn, '0');
    end process clkProc;

    stateProc : process
        -- NOTE: The extra bits in RAM_TYPE is for checking memory access.  A logic 1 means
        --       the location is in use.  This will be checked when doing memory DUMP.
        type Mem_Data_R is record
            data  : bit_vector(DATA_WIDTH - 1 downto 0);
            inUse : boolean;
        end record Mem_Data_R;
        type Ram_Page_t is array (2**COL_BITS - 1 downto 0) of Mem_Data_R;
        type Ram_Ptr_T is access Ram_Page_t;
        type Ram_Bank_T is array (2**ADDR_BITS - 1 downto 0) of Ram_Ptr_T;
        type Bank_Array_T is array (3 downto 0) of Ram_Bank_T;
        type Bank_State_T is array (3 downto 0) of boolean;
        type Bank_Row_Addr_T is array (3 downto 0) of bit_vector(ADDR_BITS - 1 downto 0);
        type Bank_Timing_T is array (3 downto 0) of time;

        variable memBankArray       : Bank_Array_T;
        variable rowIndex, colIndex : natural                         := 0;
        variable Dq_temp            : bit_vector(DATA_WIDTH DOWNTO 0) := (OTHERS => '0');

        variable Col_addr           : Array4xCBV;
        variable Bank_addr          : Array4x2BV;
        variable Dqm_reg0, Dqm_reg1 : bit_vector(1 DOWNTO 0) := "00";

        variable Bank, Previous_bank : bit_vector(1 DOWNTO 0)             := "00";
        variable bankRowAddr         : Bank_Row_Addr_T                    := (others => (others => '0'));
        --        variable B0_row_addr, B1_row_addr, B2_row_addr, B3_row_addr : bit_vector(ADDR_BITS - 1 DOWNTO 0) := (OTHERS => '0');
        variable Col_brst            : bit_vector(COL_BITS - 1 DOWNTO 0)  := (OTHERS => '0');
        variable currRow             : bit_vector(ADDR_BITS - 1 DOWNTO 0) := (OTHERS => '0');
        variable currCol             : bit_vector(COL_BITS - 1 DOWNTO 0)  := (OTHERS => '0');
        variable burstCounter        : natural                            := 0;

        variable Command            : Array_state;
        variable Bank_precharge     : Array4x2BV;
        variable A10_precharge      : Array4xB               := ('0' & '0' & '0' & '0');
        variable Auto_precharge     : Array4xB               := ('0' & '0' & '0' & '0');
        variable Read_precharge     : Array4xB               := ('0' & '0' & '0' & '0');
        variable Write_precharge    : Array4xB               := ('0' & '0' & '0' & '0');
        variable RW_interrupt_read  : Array4xB               := ('0' & '0' & '0' & '0');
        variable RW_interrupt_write : Array4xB               := ('0' & '0' & '0' & '0');
        variable RW_interrupt_bank  : bit_vector(1 DOWNTO 0) := "00";
        variable Count_time         : Array4xT               := (0 ns & 0 ns & 0 ns & 0 ns);
        variable Count_precharge    : Array4xI               := (0 & 0 & 0 & 0);

        variable Data_in_enable, Data_out_enable : bit          := '0';
        variable bankPrecharged                  : Bank_State_T := (others => false);
        --        variable Pc_b0, Pc_b1, Pc_b2, Pc_b3      : bit          := '0';
        variable bankActive                      : Bank_State_T := (others => false);
        --        variable Act_b0, Act_b1, Act_b2, Act_b3  : bit          := '0';

        -- Timing Check
        variable MRD_chk                                 : natural       := 0;
        variable WR_counter                              : Array4xI      := (0 & 0 & 0 & 0);
        variable WR_time                                 : Array4xT      := (0 ns & 0 ns & 0 ns & 0 ns);
        variable WR_chkp                                 : Array4xT      := (0 ns & 0 ns & 0 ns & 0 ns);
        variable RC_chk, RRD_chk                         : time          := 0 ns;
        variable bankRasCheck, bankRcdCheck, bankRpCheck : Bank_Timing_T := (others => 0 ns);
        --        variable RAS_chk0, RAS_chk1, RAS_chk2, RAS_chk3 : time          := 0 ns;
        --        variable RCD_chk0, RCD_chk1, RCD_chk2, RCD_chk3 : time          := 0 ns;
        --        variable RP_chk0, RP_chk1, RP_chk2, RP_chk3     : time          := 0 ns;

        -- Load and Dumb variables
        file file_load     : text open read_mode is fname; -- Data load
        file file_dump     : text open write_mode is "dumpdata.txt"; -- Data dump
        variable bank_load : bit_vector(1 downto 0);
        variable rows_load : bit_vector(12 downto 0);
        variable cols_load : bit_vector(8 downto 0);
        variable data_load : bit_vector(15 downto 0);
        --        variable i, j      : INTEGER;
        variable good_load : BOOLEAN;
        variable l         : LINE;
        variable load      : std_logic := '1';
        variable dump      : std_logic := '0';
        variable ch        : character;
        variable rectype   : bit_vector(3 downto 0);
        variable recaddr   : bit_vector(31 downto 0);
        variable reclen    : bit_vector(7 downto 0);
        variable recdata   : bit_vector(0 to 16 * 8 - 1);

        -- initialize empty rows
        procedure initMem(bankSel : bit_vector(1 downto 0); rowIndex : natural) is
            variable bankPtr  : natural    := to_integer(unsigned(to_StdLogicVector(bankSel)));
            variable currBank : Ram_Bank_T := memBankArray(bankPtr);
        begin
            if currBank(rowIndex) = null then -- check if row is uninitialized
                currBank(rowIndex) := new Ram_Page_t; -- open row for access
                for i in (2**COL_BITS - 1) downto 0 loop
                    for j in data_width downto 0 loop
                        currBank(rowIndex)(i) := (data => (others => '0'), inUse => false); -- fill with zeroes
                    end loop;
                end loop;
            end if;
        end procedure initMem;

        -- Burst Counter
        procedure decodeBurst is
            variable colPtr           : natural                           := 0;
            variable Col_vec, colTemp : bit_vector(COL_BITS - 1 downto 0) := (others => '0');
        begin
            -- Advance Burst Counter
            burstCounter := burstCounter + 1;

            -- Burst Type
            if burstInterleaved then
                TO_BITVECTOR(burstCounter, Col_vec);
                colTemp(2) := Col_vec(2) XOR Col_brst(2);
                colTemp(1) := Col_vec(1) XOR Col_brst(1);
                colTemp(0) := Col_vec(0) XOR Col_brst(0);
            else
                colPtr := TO_INTEGER(currCol);
                colPtr := colPtr + 1;
                TO_BITVECTOR(colPtr, colTemp);
            end if;

            --            if modeReg(3) = '0' then
            --                Col_int := TO_INTEGER(Col);
            --                Col_int := Col_int + 1;
            --                TO_BITVECTOR(Col_int, Col_temp);
            --            elsif modeReg(3) = '1' then
            --                TO_BITVECTOR(burstCounter, Col_vec);
            --                Col_temp(2) := Col_vec(2) XOR Col_brst(2);
            --                Col_temp(1) := Col_vec(1) XOR Col_brst(1);
            --                Col_temp(0) := Col_vec(0) XOR Col_brst(0);
            --            END if;

            case burstMode is
                when '1' =>
                    currCol := colTemp;

                when '2' =>
                    currCol(0) := colTemp(0);

                when '4' =>
                    currCol(1 DOWNTO 0) := colTemp(1 DOWNTO 0);

                when '8' =>
                    currCol(2 DOWNTO 0) := colTemp(2 DOWNTO 0);

                when FullPage =>
                    -- FIXME: what to do with full page?
                    null;

                when InvalidLength =>
                    report "Invalid burst length" severity failure;
            end case;

            -- Burst Read Single Write
            if writeBurstEnabled and Data_in_enable = '1' then
                Data_in_enable := '0';
            end if;

            -- Data counter
            -- FIXME: fix FullPage
            if burstMode /= InvalidLength and burstMode /= FullPage then
                if burstCounter >= burstLength then
                    if Data_in_enable = '1' then
                        Data_in_enable := '0';
                    elsif Data_out_enable = '1' then
                        Data_out_enable := '0';
                    end if;
                end if;
            end if;
        end procedure decodeBurst;

    begin
        wait on Sys_clk, RAS_clk;
        if rising_edge(Sys_clk) and Load = '0' and Dump = '0' then
            -- Internal Command Pipeline
            Command(2 downto 0) := Command(3 downto 1);
            Command(3)          := NOP;

            Col_addr(2 downto 0) := Col_addr(3 downto 1);
            Col_addr(3)          := (others => '0');

            Bank_addr(2 downto 0) := Bank_addr(3 downto 1);
            Bank_addr(3)          := "00";

            Bank_precharge(2 downto 0) := Bank_precharge(3 downto 1);
            Bank_precharge(3)          := "00";

            A10_precharge(2 downto 0) := A10_precharge(3 downto 1);
            A10_precharge(3)          := '0';

            -- Operation Decode (Optional for showing current command on posedge clock / debug feature)
            --    if Active_enable = '1' then
            --        currOp <= ACT;
            --    elsif Aref_enable = '1' then
            --        currOp <= A_REF;
            --    elsif Burst_term = '1' then
            --        currOp <= BST;
            --    elsif Mode_reg_enable = '1' then
            --        currOp <= LMR;
            --    elsif Prech_enable = '1' then
            --        currOp <= PRECH;
            --    elsif Read_enable = '1' then
            --        if addrIn(10) = '0' then
            --            currOp <= READ;
            --        ELSE
            --            currOp <= READ_A;
            --        END if;
            --    elsif Write_enable = '1' then
            --        if addrIn(10) = '0' then
            --            currOp <= WRITE;
            --        ELSE
            --            currOp <= WRITE_A;
            --        END if;
            --    ELSE
            --        currOp <= NOP;
            --    END if;

            -- Dqm pipeline for Read
            Dqm_reg0 := Dqm_reg1;
            Dqm_reg1 := TO_BITVECTOR(dqmIn);

            -- Read or Write with Auto Precharge Counter
            for i in 0 to 3 loop
                if Auto_precharge(i) = '1' then
                    Count_precharge(i) := Count_precharge(i) + 1;
                end if;
            end loop;

            --    if Auto_precharge(0) = '1' then
            --        Count_precharge(0) := Count_precharge(0) + 1;
            --    END if;
            --    if Auto_precharge(1) = '1' then
            --        Count_precharge(1) := Count_precharge(1) + 1;
            --    END if;
            --    if Auto_precharge(2) = '1' then
            --        Count_precharge(2) := Count_precharge(2) + 1;
            --    END if;
            --    if Auto_precharge(3) = '1' then
            --        Count_precharge(3) := Count_precharge(3) + 1;
            --    END if;

            -- Auto Precharge Timer for tWR
            if writeBurstEnabled then
                for i in 0 to 3 loop
                    if Count_precharge(i) = 1 then
                        Count_time(i) := NOW;
                    end if;
                end loop;
            else
                for i in 0 to 3 loop
                    if Count_precharge(i) = burstLength then
                        Count_time(i) := NOW;
                    end if;
                end loop;
            end if;

            --    if (Burst_length_1 = '1' OR Write_burst_mode = '1') then
            --    elsif (Burst_length_2 = '1') then
            --        if (Count_precharge(0) = 2) then
            --            Count_time(0) := NOW;
            --        end if;
            --        if (Count_precharge(1) = 2) then
            --            Count_time(1) := NOW;
            --        end if;
            --        if (Count_precharge(2) = 2) then
            --            Count_time(2) := NOW;
            --        end if;
            --        if (Count_precharge(3) = 2) then
            --            Count_time(3) := NOW;
            --        end if;
            --    elsif (Burst_length_4 = '1') then
            --        if (Count_precharge(0) = 4) then
            --            Count_time(0) := NOW;
            --        end if;
            --        if (Count_precharge(1) = 4) then
            --            Count_time(1) := NOW;
            --        end if;
            --        if (Count_precharge(2) = 4) then
            --            Count_time(2) := NOW;
            --        end if;
            --        if (Count_precharge(3) = 4) then
            --            Count_time(3) := NOW;
            --        end if;
            --    elsif (Burst_length_8 = '1') then
            --        if (Count_precharge(0) = 8) then
            --            Count_time(0) := NOW;
            --        end if;
            --        if (Count_precharge(1) = 8) then
            --            Count_time(1) := NOW;
            --        end if;
            --        if (Count_precharge(2) = 8) then
            --            Count_time(2) := NOW;
            --        end if;
            --        if (Count_precharge(3) = 8) then
            --            Count_time(3) := NOW;
            --        end if;
            --    end if;

            -- tMRD Counter
            MRD_chk := MRD_chk + 1;

            -- tWR Counter
            for i in 0 to 3 loop
                WR_counter(i) := WR_counter(i) + 1;
            end loop;
            --    WR_counter(0) := WR_counter(0) + 1;
            --    WR_counter(1) := WR_counter(1) + 1;
            --    WR_counter(2) := WR_counter(2) + 1;
            --    WR_counter(3) := WR_counter(3) + 1;

            -- Auto Refresh
            if currCommand = ArefEna then
                --            if Aref_enable = '1' then
                -- Auto Refresh to Auto Refresh
                assert (NOW - RC_chk >= tRC)
                report "tRC violation during Auto Refresh"
                severity warning;
                -- Precharge to Auto Refresh
                assert (NOW - RP_chk0 >= tRP OR NOW - RP_chk1 >= tRP OR NOW - RP_chk2 >= tRP OR NOW - RP_chk3 >= tRP)
                report "tRP violation during Auto Refresh"
                severity warning;
                -- All banks must be idle before refresh
                -- FIXME: is this right?
                --                assert Pc_b3 = '0' OR Pc_b2 = '0' OR Pc_b1 = '0' OR Pc_b0 = '0'
                assert bankPrecharged(0) and bankPrecharged(1) and bankPrecharged(2) and bankPrecharged(3)
                report "All banks must be Precharge before Auto Refresh"
                severity warning;
                -- Record current tRC time
                RC_chk := NOW;
            end if;

            -- Load Mode Register
            --if Mode_reg_enable = '1' then
            if currCommand = ModeRegEna then
                modeReg <= to_BitVector(addrIn);
                --                assert Pc_b3 = '0' OR Pc_b2 = '0' OR Pc_b1 = '0' OR Pc_b0 = '0'
                assert bankPrecharged(0) and bankPrecharged(1) and bankPrecharged(2) and bankPrecharged(3)
                report "All bank must be Precharge before Load Mode Register"
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
            end if;

            -- Active Block (latch Bank and Row Address)
            --if Active_enable = '1' then
            if currCommand = ActiveEna then
                if bankPrecharged(to_integer(unsigned(baIn))) then
                    bankActive(to_integer(unsigned(baIn)))     := true;
                    bankPrecharged(to_integer(unsigned(baIn))) := false;
                    bankRowAddr(to_integer(unsigned(baIn)))    := to_BitVector(addrIn);
                    bankRcdCheck(to_integer(unsigned(baIn)))   := NOW;
                    bankRasCheck(to_integer(unsigned(baIn)))   := NOW;
                    -- Precharge to Active Bank 0
                    assert (NOW - bankRpCheck(to_integer(unsigned(baIn))) >= tRP)
                    report "tRP violation during Activate Bank " & natural'image(to_integer(unsigned(baIn)))
                    severity warning;
                else
                    report "Bank " & natural'image(to_integer(unsigned(baIn))) & " is not Precharged"
                    severity warning;
                end if;

                -- Active Bank A to Active Bank B
                if ((Previous_bank /= TO_BITVECTOR(baIn)) AND (NOW - RRD_chk < tRRD)) then
                    report "tRRD violation during Activate"
                    severity warning;
                END if;
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
                Previous_bank := to_BitVector(baIn);
            end if;

            -- Precharge Block
            --if Prech_enable = '1' then
            if currCommand = PrechargeEna then
                if addrIn(10) = '1' then
                    bankPrecharged := (others => true);
                    bankActive     := (others => false);
                    bankRpCheck    := (others => NOW);
                    -- Activate to Precharge all banks
                    assert ((NOW - RAS_chk0 >= tRAS) OR (NOW - RAS_chk1 >= tRAS))
                    report "tRAS violation during Precharge all banks"
                    severity warning;
                    -- tWR violation check for Write
                    if ((NOW - WR_chkp(0) < tWRp) OR (NOW - WR_chkp(1) < tWRp) OR (NOW - WR_chkp(2) < tWRp) OR (NOW - WR_chkp(3) < tWRp)) then
                        assert (FALSE)
                        report "tWR violation during Precharge ALL banks"
                        severity warning;
                    END if;
                elsif addrIn(10) = '0' then
                    bankPrecharged(to_integer(unsigned(baIn))) := true;
                    bankActive(to_integer(unsigned(baIn)))     := false;
                    bankRpCheck(to_integer(unsigned(baIn)))    := NOW;
                    -- Activate to Precharge bank
                    assert (NOW - bankRasCheck(to_integer(unsigned(baIn))) >= tRAS)
                    report "tRAS violation during Precharge bank " & natural'image(to_integer(unsigned(baIn)))
                    severity warning;
                    -- tWR violation check for Write
                    assert (NOW - WR_chkp(TO_INTEGER(baIn)) >= tWRp)
                    report "tWR violation during Precharge"
                    severity warning;
                END if;
                -- Terminate a Write Immediately (if same bank or all banks)
                if (Data_in_enable = '1' AND (Bank = TO_BITVECTOR(baIn) OR addrIn(10) = '1')) then
                    Data_in_enable := '0';
                END if;
                -- Precharge Command Pipeline for READ
                if CAS_latency_3 = '1' then
                    Command(2)        := PRECH;
                    Bank_precharge(2) := TO_BITVECTOR(baIn);
                    A10_precharge(2)  := TO_BIT(addrIn(10));
                elsif CAS_latency_2 = '1' then
                    Command(1)        := PRECH;
                    Bank_precharge(1) := TO_BITVECTOR(baIn);
                    A10_precharge(1)  := TO_BIT(addrIn(10));
                END if;
            END if;

            -- Burst Terminate
            --if Burst_term = '1' then
            if currCommand = BurstTerm then
                -- Terminate a Write immediately
                if Data_in_enable = '1' then
                    Data_in_enable := '0';
                end if;
                -- Terminate a Read depend on CAS Latency
                if CAS_latency_3 = '1' then
                    Command(2) := BST;
                elsif CAS_latency_2 = '1' then
                    Command(1) := BST;
                end if;
            end if;

            -- Read, Write, Column Latch
            --if Read_enable = '1' OR Write_enable = '1' then
            if currCommand = ReadEna or currCommand = WriteEna then
                -- Check to see if bank is open (ACT) for Read or Write
                assert not bankPrecharged(to_integer(unsigned(baIn)))
                report "Cannot Read or Write - Bank is not Activated"
                severity warning;
                --                if ((baIn = "00" AND Pc_b0 = '1') OR (baIn = "01" AND Pc_b1 = '1') OR (baIn = "10" AND Pc_b2 = '1') OR (baIn = "11" AND Pc_b3 = '1')) then
                --                    assert (FALSE)
                --                    report "Cannot Read or Write - Bank is not Activated"
                --                    severity warning;
                --                end if;
                -- Activate to Read or Write
                assert NOW - bankRcdCheck(to_integer(unsigned(baIn))) >= tRCD
                report "tRCD violation during Read or Write to Bank " & natural'image(to_integer(unsigned(baIn)))
                severity warning;

                -- Read Command
                if currCommand = ReadEna then
                    -- CAS Latency Pipeline
                    if Cas_latency_3 = '1' then
                        if addrIn(10) = '1' then
                            Command(2) := READ_A;
                        ELSE
                            Command(2) := READ;
                        END if;
                        Col_addr(2)  := TO_BITVECTOR(addrIn(COL_BITS - 1 DOWNTO 0));
                        Bank_addr(2) := TO_BITVECTOR(baIn);
                    elsif Cas_latency_2 = '1' then
                        if addrIn(10) = '1' then
                            Command(1) := READ_A;
                        ELSE
                            Command(1) := READ;
                        END if;
                        Col_addr(1)  := TO_BITVECTOR(addrIn(COL_BITS - 1 DOWNTO 0));
                        Bank_addr(1) := TO_BITVECTOR(baIn);
                    END if;

                    -- Read intterupt a Write (terminate Write immediately)
                    if Data_in_enable = '1' then
                        Data_in_enable := '0';
                    END if;

                -- Write Command
                elsif currCommand = WriteEna then
                    if addrIn(10) = '1' then
                        Command(0) := WRITE_A;
                    ELSE
                        Command(0) := WRITE;
                    END if;
                    Col_addr(0)  := TO_BITVECTOR(addrIn(COL_BITS - 1 DOWNTO 0));
                    Bank_addr(0) := TO_BITVECTOR(baIn);

                    -- Write intterupt a Write (terminate Write immediately)
                    if Data_in_enable = '1' then
                        Data_in_enable := '0';
                    END if;

                    -- Write interrupt a Read (terminate Read immediately)
                    if Data_out_enable = '1' then
                        Data_out_enable := '0';
                    END if;
                end if;

                -- Interrupt a Write with Auto Precharge
                if Auto_precharge(TO_INTEGER(RW_Interrupt_Bank)) = '1' and Write_precharge(TO_INTEGER(RW_Interrupt_Bank)) = '1' then
                    RW_interrupt_write(TO_INTEGER(RW_Interrupt_Bank)) := '1';
                end if;

                -- Interrupt a Read with Auto Precharge
                if Auto_precharge(TO_INTEGER(RW_Interrupt_Bank)) = '1' and Read_precharge(TO_INTEGER(RW_Interrupt_Bank)) = '1' then
                    RW_interrupt_read(TO_INTEGER(RW_Interrupt_Bank)) := '1';
                end if;

                -- Read or Write with Auto Precharge
                if addrIn(10) = '1' then
                    Auto_precharge(TO_INTEGER(baIn))  := '1';
                    Count_precharge(TO_INTEGER(baIn)) := 0;
                    RW_Interrupt_Bank                 := TO_BitVector(baIn);
                    if Read_enable = '1' then
                        Read_precharge(TO_INTEGER(baIn)) := '1';
                    elsif Write_enable = '1' then
                        Write_precharge(TO_INTEGER(baIn)) := '1';
                    END if;
                END if;
            END if;

            -- Read with AutoPrecharge Calculation
            --      The device start internal precharge when:
            --          1.  BL/2 cycles after command
            --      and 2.  Meet tRAS requirement
            --       or 3.  Interrupt by a Read or Write (with or without Auto Precharge)
            for i in 0 to 3 loop
                if ((Auto_precharge(i) = '1') and (Read_precharge(i) = '1')) then
                    if (((NOW - bankRasCheck(i) >= tRAS) and ((Burst_length_1 = '1' and Count_precharge(i) >= 1) or (Burst_length_2 = '1' and Count_precharge(i) >= 2) or (Burst_length_4 = '1' and Count_precharge(i) >= 4) or (Burst_length_8 = '1' and Count_precharge(i) >= 8))) or (RW_interrupt_read(i) = '1')) then
                        bankPrecharged(i)    := true;
                        bankActive(i)        := false;
                        bankRpCheck(i)       := NOW;
                        Auto_precharge(0)    := '0';
                        Read_precharge(0)    := '0';
                        RW_interrupt_read(0) := '0';
                    end if;
                end if;
            end loop;

            --            if ((Auto_precharge(0) = '1') AND (Read_precharge(0) = '1')) then
            --                if (((NOW - RAS_chk0 >= tRAS) AND ((Burst_length_1 = '1' AND Count_precharge(0) >= 1) OR (Burst_length_2 = '1' AND Count_precharge(0) >= 2) OR (Burst_length_4 = '1' AND Count_precharge(0) >= 4) OR (Burst_length_8 = '1' AND Count_precharge(0) >= 8))) OR (RW_interrupt_read(0) = '1')) then
            --                    Pc_b0                := '1';
            --                    Act_b0               := '0';
            --                    RP_chk0              := NOW;
            --                    Auto_precharge(0)    := '0';
            --                    Read_precharge(0)    := '0';
            --                    RW_interrupt_read(0) := '0';
            --                END if;
            --            END if;
            --            if ((Auto_precharge(1) = '1') AND (Read_precharge(1) = '1')) then
            --                if (((NOW - RAS_chk1 >= tRAS) AND ((Burst_length_1 = '1' AND Count_precharge(1) >= 1) OR (Burst_length_2 = '1' AND Count_precharge(1) >= 2) OR (Burst_length_4 = '1' AND Count_precharge(1) >= 4) OR (Burst_length_8 = '1' AND Count_precharge(1) >= 8))) OR (RW_interrupt_read(1) = '1')) then
            --                    Pc_b1                := '1';
            --                    Act_b1               := '0';
            --                    RP_chk1              := NOW;
            --                    Auto_precharge(1)    := '0';
            --                    Read_precharge(1)    := '0';
            --                    RW_interrupt_read(1) := '0';
            --                END if;
            --            END if;
            --            if ((Auto_precharge(2) = '1') AND (Read_precharge(2) = '1')) then
            --                if (((NOW - RAS_chk2 >= tRAS) AND ((Burst_length_1 = '1' AND Count_precharge(2) >= 1) OR (Burst_length_2 = '1' AND Count_precharge(2) >= 2) OR (Burst_length_4 = '1' AND Count_precharge(2) >= 4) OR (Burst_length_8 = '1' AND Count_precharge(2) >= 8))) OR (RW_interrupt_read(2) = '1')) then
            --                    Pc_b2                := '1';
            --                    Act_b2               := '0';
            --                    RP_chk2              := NOW;
            --                    Auto_precharge(2)    := '0';
            --                    Read_precharge(2)    := '0';
            --                    RW_interrupt_read(2) := '0';
            --                END if;
            --            END if;
            --            if ((Auto_precharge(3) = '1') AND (Read_precharge(3) = '1')) then
            --                if (((NOW - RAS_chk3 >= tRAS) AND ((Burst_length_1 = '1' AND Count_precharge(3) >= 1) OR (Burst_length_2 = '1' AND Count_precharge(3) >= 2) OR (Burst_length_4 = '1' AND Count_precharge(3) >= 4) OR (Burst_length_8 = '1' AND Count_precharge(3) >= 8))) OR (RW_interrupt_read(3) = '1')) then
            --                    Pc_b3                := '1';
            --                    Act_b3               := '0';
            --                    RP_chk3              := NOW;
            --                    Auto_precharge(3)    := '0';
            --                    Read_precharge(3)    := '0';
            --                    RW_interrupt_read(3) := '0';
            --                END if;
            --            END if;

            -- Internal Precharge or Bst
            if Command(0) = PRECH then  -- PRECH terminate a read if same bank or all banks
                if Bank_precharge(0) = Bank OR A10_precharge(0) = '1' then
                    if Data_out_enable = '1' then
                        Data_out_enable := '0';
                    END if;
                END if;
            elsif Command(0) = BST then -- BST terminate a read regardless of bank
                if Data_out_enable = '1' then
                    Data_out_enable := '0';
                END if;
            END if;

            if Data_out_enable = '0' then
                dataInOut <= TRANSPORT (OTHERS => 'Z') AFTER tOH;
            END if;

            -- Detect Read or Write Command
            if Command(0) = READ OR Command(0) = READ_A then
                Bank            := Bank_addr(0);
                currCol         := Col_addr(0);
                Col_brst        := Col_addr(0);
                if Bank_addr(0) = "00" then
                    currRow := B0_row_addr;
                elsif Bank_addr(0) = "01" then
                    currRow := B1_row_addr;
                elsif Bank_addr(0) = "10" then
                    currRow := B2_row_addr;
                ELSE
                    currRow := B3_row_addr;
                END if;
                burstCounter    := 0;
                Data_in_enable  := '0';
                Data_out_enable := '1';
            elsif Command(0) = WRITE OR Command(0) = WRITE_A then
                Bank            := Bank_addr(0);
                currCol         := Col_addr(0);
                Col_brst        := Col_addr(0);
                if Bank_addr(0) = "00" then
                    currRow := B0_row_addr;
                elsif Bank_addr(0) = "01" then
                    currRow := B1_row_addr;
                elsif Bank_addr(0) = "10" then
                    currRow := B2_row_addr;
                ELSE
                    currRow := B3_row_addr;
                END if;
                burstCounter    := 0;
                Data_in_enable  := '1';
                Data_out_enable := '0';
            END if;

            -- DQ (Driver / Receiver)
            rowIndex := TO_INTEGER(currRow);
            colIndex := TO_INTEGER(currCol);
            if Data_in_enable = '1' then
                if dqmIn /= "11" then
                    initMem(Bank, rowIndex);
                    Dq_temp                                                                               := memBankArray(to_integer(unsigned(to_StdLogicVector(Bank))))(rowIndex)(colIndex).data;
                    if dqmIn = "01" then
                        Dq_temp(15 DOWNTO 8) := TO_BITVECTOR(dataInOut(15 DOWNTO 8));
                    elsif dqmIn = "10" then
                        Dq_temp(7 DOWNTO 0) := TO_BITVECTOR(dataInOut(7 DOWNTO 0));
                    ELSE
                        Dq_temp(15 DOWNTO 0) := TO_BITVECTOR(dataInOut(15 DOWNTO 0));
                    END if;
                    memBankArray(to_integer(unsigned(to_StdLogicVector(Bank))))(rowIndex)(colIndex).inUse := true;
                    --        if Bank = "00" then
                    --            Dq_temp                   := Bank0(rowIndex)(colIndex);
                    --            if dqmIn = "01" then
                    --                Dq_temp(15 DOWNTO 8) := TO_BITVECTOR(dataInOut(15 DOWNTO 8));
                    --            elsif dqmIn = "10" then
                    --                Dq_temp(7 DOWNTO 0) := TO_BITVECTOR(dataInOut(7 DOWNTO 0));
                    --            ELSE
                    --                Dq_temp(15 DOWNTO 0) := TO_BITVECTOR(dataInOut(15 DOWNTO 0));
                    --            END if;
                    --            Bank0(rowIndex)(colIndex) := ('1' & Dq_temp(DATA_WIDTH - 1 DOWNTO 0));
                    --        elsif Bank = "01" then
                    --            Dq_temp                   := Bank1(rowIndex)(colIndex);
                    --            if dqmIn = "01" then
                    --                Dq_temp(15 DOWNTO 8) := TO_BITVECTOR(dataInOut(15 DOWNTO 8));
                    --            elsif dqmIn = "10" then
                    --                Dq_temp(7 DOWNTO 0) := TO_BITVECTOR(dataInOut(7 DOWNTO 0));
                    --            ELSE
                    --                Dq_temp(15 DOWNTO 0) := TO_BITVECTOR(dataInOut(15 DOWNTO 0));
                    --            END if;
                    --            Bank1(rowIndex)(colIndex) := ('1' & Dq_temp(DATA_WIDTH - 1 DOWNTO 0));
                    --        elsif Bank = "10" then
                    --            Dq_temp                   := Bank2(rowIndex)(colIndex);
                    --            if dqmIn = "01" then
                    --                Dq_temp(15 DOWNTO 8) := TO_BITVECTOR(dataInOut(15 DOWNTO 8));
                    --            elsif dqmIn = "10" then
                    --                Dq_temp(7 DOWNTO 0) := TO_BITVECTOR(dataInOut(7 DOWNTO 0));
                    --            ELSE
                    --                Dq_temp(15 DOWNTO 0) := TO_BITVECTOR(dataInOut(15 DOWNTO 0));
                    --            END if;
                    --            Bank2(rowIndex)(colIndex) := ('1' & Dq_temp(DATA_WIDTH - 1 DOWNTO 0));
                    --        elsif Bank = "11" then
                    --            Dq_temp                   := Bank3(rowIndex)(colIndex);
                    --            if dqmIn = "01" then
                    --                Dq_temp(15 DOWNTO 8) := TO_BITVECTOR(dataInOut(15 DOWNTO 8));
                    --            elsif dqmIn = "10" then
                    --                Dq_temp(7 DOWNTO 0) := TO_BITVECTOR(dataInOut(7 DOWNTO 0));
                    --            ELSE
                    --                Dq_temp(15 DOWNTO 0) := TO_BITVECTOR(dataInOut(15 DOWNTO 0));
                    --            END if;
                    --            Bank3(rowIndex)(colIndex) := ('1' & Dq_temp(DATA_WIDTH - 1 DOWNTO 0));
                    --        END if;
                    WR_chkp(TO_INTEGER(Bank))                                                             := NOW;
                    WR_counter(TO_INTEGER(Bank))                                                          := 0;
                END if;
                decodeBurst;
            elsif Data_out_enable = '1' then
                if Dqm_reg0 /= "11" then
                    initMem(Bank, rowIndex);
                    Dq_temp := memBankArray(to_integer(unsigned(to_StdLogicVector(Bank))))(rowIndex)(colIndex).data;
                    if Dqm_reg0 = "00" then
                        dataInOut(15 DOWNTO 0) <= TRANSPORT TO_STDLOGICVECTOR(Dq_temp(15 DOWNTO 0)) AFTER tAC;
                    elsif Dqm_reg0 = "01" then
                        dataInOut(15 DOWNTO 8) <= TRANSPORT TO_STDLOGICVECTOR(Dq_temp(15 DOWNTO 8)) AFTER tAC;
                        dataInOut(7 DOWNTO 0)  <= TRANSPORT (OTHERS => 'Z') AFTER tAC;
                    elsif Dqm_reg0 = "10" then
                        dataInOut(15 DOWNTO 8) <= TRANSPORT (OTHERS => 'Z') AFTER tAC;
                        dataInOut(7 DOWNTO 0)  <= TRANSPORT TO_STDLOGICVECTOR(Dq_temp(7 DOWNTO 0)) AFTER tAC;
                    end if;
                --        if Bank = "00" then
                --            Dq_temp := Bank0(rowIndex)(colIndex);
                --            if Dqm_reg0 = "00" then
                --                dataInOut(15 DOWNTO 0) <= TRANSPORT TO_STDLOGICVECTOR(Dq_temp(15 DOWNTO 0)) AFTER tAC;
                --            elsif Dqm_reg0 = "01" then
                --                dataInOut(15 DOWNTO 8) <= TRANSPORT TO_STDLOGICVECTOR(Dq_temp(15 DOWNTO 8)) AFTER tAC;
                --                dataInOut(7 DOWNTO 0)  <= TRANSPORT (OTHERS => 'Z') AFTER tAC;
                --            elsif Dqm_reg0 = "10" then
                --                dataInOut(15 DOWNTO 8) <= TRANSPORT (OTHERS => 'Z') AFTER tAC;
                --                dataInOut(7 DOWNTO 0)  <= TRANSPORT TO_STDLOGICVECTOR(Dq_temp(7 DOWNTO 0)) AFTER tAC;
                --            END if;
                --        elsif Bank = "01" then
                --            Dq_temp := Bank1(rowIndex)(colIndex);
                --            if Dqm_reg0 = "00" then
                --                dataInOut(15 DOWNTO 0) <= TRANSPORT TO_STDLOGICVECTOR(Dq_temp(15 DOWNTO 0)) AFTER tAC;
                --            elsif Dqm_reg0 = "01" then
                --                dataInOut(15 DOWNTO 8) <= TRANSPORT TO_STDLOGICVECTOR(Dq_temp(15 DOWNTO 8)) AFTER tAC;
                --                dataInOut(7 DOWNTO 0)  <= TRANSPORT (OTHERS => 'Z') AFTER tAC;
                --            elsif Dqm_reg0 = "10" then
                --                dataInOut(15 DOWNTO 8) <= TRANSPORT (OTHERS => 'Z') AFTER tAC;
                --                dataInOut(7 DOWNTO 0)  <= TRANSPORT TO_STDLOGICVECTOR(Dq_temp(7 DOWNTO 0)) AFTER tAC;
                --            END if;
                --        elsif Bank = "10" then
                --            Dq_temp := Bank2(rowIndex)(colIndex);
                --            if Dqm_reg0 = "00" then
                --                dataInOut(15 DOWNTO 0) <= TRANSPORT TO_STDLOGICVECTOR(Dq_temp(15 DOWNTO 0)) AFTER tAC;
                --            elsif Dqm_reg0 = "01" then
                --                dataInOut(15 DOWNTO 8) <= TRANSPORT TO_STDLOGICVECTOR(Dq_temp(15 DOWNTO 8)) AFTER tAC;
                --                dataInOut(7 DOWNTO 0)  <= TRANSPORT (OTHERS => 'Z') AFTER tAC;
                --            elsif Dqm_reg0 = "10" then
                --                dataInOut(15 DOWNTO 8) <= TRANSPORT (OTHERS => 'Z') AFTER tAC;
                --                dataInOut(7 DOWNTO 0)  <= TRANSPORT TO_STDLOGICVECTOR(Dq_temp(7 DOWNTO 0)) AFTER tAC;
                --            END if;
                --        elsif Bank = "11" then
                --            Dq_temp := Bank3(rowIndex)(colIndex);
                --            if Dqm_reg0 = "00" then
                --                dataInOut(15 DOWNTO 0) <= TRANSPORT TO_STDLOGICVECTOR(Dq_temp(15 DOWNTO 0)) AFTER tAC;
                --            elsif Dqm_reg0 = "01" then
                --                dataInOut(15 DOWNTO 8) <= TRANSPORT TO_STDLOGICVECTOR(Dq_temp(15 DOWNTO 8)) AFTER tAC;
                --                dataInOut(7 DOWNTO 0)  <= TRANSPORT (OTHERS => 'Z') AFTER tAC;
                --            elsif Dqm_reg0 = "10" then
                --                dataInOut(15 DOWNTO 8) <= TRANSPORT (OTHERS => 'Z') AFTER tAC;
                --                dataInOut(7 DOWNTO 0)  <= TRANSPORT TO_STDLOGICVECTOR(Dq_temp(7 DOWNTO 0)) AFTER tAC;
                --            END if;
                --        END if;
                else
                    dataInOut <= transport (others => 'Z') after tHZ;
                end if;
                decodeBurst;
            end if;
        elsif Sys_clk'event AND Sys_clk = '1' AND Load = '1' AND Dump = '0' then --'
            currOp <= LOAD_FILE;
            load   := '0';
            --            assert (FALSE) report "Reading memory array from file.  This operation may take several minutes.  Please wait..."
            --                severity NOTE;
            WHILE NOT endfile(file_load) LOOP
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
                        Bank_Load := recaddr(25 downto 24);
                        Rows_Load := recaddr(23 downto 11);
                        Cols_Load := recaddr(10 downto 2);
                        initMem(Bank_Load, TO_INTEGER(Rows_Load));
                        for i in 0 to 3 loop
                            memBankArray(to_integer(unsigned(to_StdLogicVector(Bank_Load))))(to_integer(Rows_Load))(to_integer(Cols_Load) + i) := (inUse => true, data => recdata(i * 32 + index to i * 32 + index + 15));
                        end loop;
                    --                        if Bank_Load = "00" then
                    --                            for i in 0 to 3 loop
                    --                                Bank0(TO_INTEGER(Rows_Load))(TO_INTEGER(Cols_Load) + i) := ('1' & recdata(i * 32 + index to i * 32 + index + 15));
                    --                            end loop;
                    --                        elsif Bank_Load = "01" then
                    --                            for i in 0 to 3 loop
                    --                                Bank1(TO_INTEGER(Rows_Load))(TO_INTEGER(Cols_Load) + i) := ('1' & recdata(i * 32 + index to i * 32 + index + 15));
                    --                            end loop;
                    --                        elsif Bank_Load = "10" then
                    --                            for i in 0 to 3 loop
                    --                                Bank2(TO_INTEGER(Rows_Load))(TO_INTEGER(Cols_Load) + i) := ('1' & recdata(i * 32 + index to i * 32 + index + 15));
                    --                            end loop;
                    --                        elsif Bank_Load = "11" then
                    --                            for i in 0 to 3 loop
                    --                                Bank3(TO_INTEGER(Rows_Load))(TO_INTEGER(Cols_Load) + i) := ('1' & recdata(i * 32 + index to i * 32 + index + 15));
                    --                            end loop;
                    --                        END if;
                    else
                        Bank_Load := recaddr(26 downto 25);
                        Rows_Load := recaddr(24 downto 12);
                        Cols_Load := recaddr(11 downto 3);
                        initMem(Bank_Load, TO_INTEGER(Rows_Load));
                        for i in 0 to 3 loop
                            memBankArray(to_integer(unsigned(to_StdLogicVector(Bank_Load))))(to_integer(Rows_Load))(to_integer(Cols_Load) + i) := (inUse => true, data => recdata(i * 64 + index - 32 to i * 64 + index - 32 + 15));
                        end loop;
                        --                        if Bank_Load = "00" then
                        --                            for i in 0 to 1 loop
                        --                                Bank0(TO_INTEGER(Rows_Load))(TO_INTEGER(Cols_Load) + i) := ('1' & recdata(i * 64 + index - 32 to i * 64 + index - 32 + 15));
                        --                            end loop;
                        --                        elsif Bank_Load = "01" then
                        --                            for i in 0 to 1 loop
                        --                                Bank1(TO_INTEGER(Rows_Load))(TO_INTEGER(Cols_Load) + i) := ('1' & recdata(i * 64 + index - 32 to i * 64 + index - 32 + 15));
                        --                            end loop;
                        --                        elsif Bank_Load = "10" then
                        --                            for i in 0 to 1 loop
                        --                                Bank2(TO_INTEGER(Rows_Load))(TO_INTEGER(Cols_Load) + i) := ('1' & recdata(i * 64 + index - 32 to i * 64 + index - 32 + 15));
                        --                            end loop;
                        --                        elsif Bank_Load = "11" then
                        --                            for i in 0 to 1 loop
                        --                                Bank3(TO_INTEGER(Rows_Load))(TO_INTEGER(Cols_Load) + i) := ('1' & recdata(i * 64 + index - 32 to i * 64 + index - 32 + 15));
                        --                            end loop;
                        --                        END if;
                    end if;
                end if;
            end loop;
        elsif rising_edge(Sys_clk) AND Load = '0' AND Dump = '1' then --'
            currOp <= DUMP_FILE;
            assert (FALSE) report "Writing memory array to file.  This operation may take several minutes.  Please wait..."
            severity NOTE;
            WRITE(l, string'("# Micron Technology, Inc. (FILE DUMP / MEMORY DUMP)")); --'
            WRITELINE(file_dump, l);
            WRITE(l, string'("# BA ROWS          COLS      DQ")); --'
            WRITELINE(file_dump, l);
            WRITE(l, string'("# -- ------------- --------- ----------------")); --'
            WRITELINE(file_dump, l);
            -- Dumping Bank 0
            FOR i IN 0 TO 2**ADDR_BITS - 1 LOOP
                -- Check if ROW is NULL
                if Bank0(i) /= NULL then
                    For j IN 0 TO 2**COL_BITS - 1 LOOP
                        -- Check if COL is NULL
                        NEXT WHEN Bank0(i)(j)(DATA_WIDTH) = '0';
                        WRITE(l, string'("00"), right, 4); --'
                        WRITE(l, To_BitVector(Conv_Std_Logic_Vector(i, ADDR_BITS)), right, ADDR_BITS + 1);
                        WRITE(l, To_BitVector(Conv_std_Logic_Vector(j, COL_BITS)), right, COL_BITS + 1);
                        WRITE(l, Bank0(i)(j)(DATA_WIDTH - 1 DOWNTO 0), right, DATA_WIDTH + 1);
                        WRITELINE(file_dump, l);
                    END LOOP;
                END if;
            END LOOP;
            -- Dumping Bank 1
            FOR i IN 0 TO 2**ADDR_BITS - 1 LOOP
                -- Check if ROW is NULL
                if Bank1(i) /= NULL then
                    For j IN 0 TO 2**COL_BITS - 1 LOOP
                        -- Check if COL is NULL
                        NEXT WHEN Bank1(i)(j)(DATA_WIDTH) = '0';
                        WRITE(l, string'("01"), right, 4); --'
                        WRITE(l, To_BitVector(Conv_Std_Logic_Vector(i, ADDR_BITS)), right, ADDR_BITS + 1);
                        WRITE(l, To_BitVector(Conv_std_Logic_Vector(j, COL_BITS)), right, COL_BITS + 1);
                        WRITE(l, Bank1(i)(j)(DATA_WIDTH - 1 DOWNTO 0), right, DATA_WIDTH + 1);
                        WRITELINE(file_dump, l);
                    END LOOP;
                END if;
            END LOOP;
            -- Dumping Bank 2
            FOR i IN 0 TO 2**ADDR_BITS - 1 LOOP
                -- Check if ROW is NULL
                if Bank2(i) /= NULL then
                    For j IN 0 TO 2**COL_BITS - 1 LOOP
                        -- Check if COL is NULL
                        NEXT WHEN Bank2(i)(j)(DATA_WIDTH) = '0';
                        WRITE(l, string'("10"), right, 4); --'
                        WRITE(l, To_BitVector(Conv_Std_Logic_Vector(i, ADDR_BITS)), right, ADDR_BITS + 1);
                        WRITE(l, To_BitVector(Conv_std_Logic_Vector(j, COL_BITS)), right, COL_BITS + 1);
                        WRITE(l, Bank2(i)(j)(DATA_WIDTH - 1 DOWNTO 0), right, DATA_WIDTH + 1);
                        WRITELINE(file_dump, l);
                    END LOOP;
                END if;
            END LOOP;
            -- Dumping Bank 3
            FOR i IN 0 TO 2**ADDR_BITS - 1 LOOP
                -- Check if ROW is NULL
                if Bank3(i) /= NULL then
                    For j IN 0 TO 2**COL_BITS - 1 LOOP
                        -- Check if COL is NULL
                        NEXT WHEN Bank3(i)(j)(DATA_WIDTH) = '0';
                        WRITE(l, string'("11"), right, 4); --'
                        WRITE(l, To_BitVector(Conv_Std_Logic_Vector(i, ADDR_BITS)), right, ADDR_BITS + 1);
                        WRITE(l, To_BitVector(Conv_std_Logic_Vector(j, COL_BITS)), right, COL_BITS + 1);
                        WRITE(l, Bank3(i)(j)(DATA_WIDTH - 1 DOWNTO 0), right, DATA_WIDTH + 1);
                        WRITELINE(file_dump, l);
                    END LOOP;
                END if;
            END LOOP;
        END if;

        -- Write with AutoPrecharge Calculation
        --      The device start internal precharge when:
        --          1.  tWR cycles after command
        --      and 2.  Meet tRAS requirement
        --       or 3.  Interrupt by a Read or Write (with or without Auto Precharge)
        if ((Auto_precharge(0) = '1') AND (Write_precharge(0) = '1')) then
            if (((NOW - RAS_chk0 >= tRAS) AND (((Burst_length_1 = '1' OR Write_burst_mode = '1') AND Count_precharge(0) >= 1 AND NOW - Count_time(0) >= tWRa) OR (Burst_length_2 = '1' AND Count_precharge(0) >= 2 AND NOW - Count_time(0) >= tWRa) OR (Burst_length_4 = '1' AND Count_precharge(0) >= 4 AND NOW - Count_time(0) >= tWRa) OR (Burst_length_8 = '1' AND Count_precharge(0) >= 8 AND NOW - Count_time(0) >= tWRa))) OR (RW_interrupt_write(0) = '1' AND WR_counter(0) >= 1 AND NOW - WR_time(0) >= tWRa)) then
                Auto_precharge(0)     := '0';
                Write_precharge(0)    := '0';
                RW_interrupt_write(0) := '0';
                Pc_b0                 := '1';
                Act_b0                := '0';
                RP_chk0               := NOW;
                assert FALSE report "Start Internal Precharge Bank 0" severity NOTE;
            END if;
        END if;
        if ((Auto_precharge(1) = '1') AND (Write_precharge(1) = '1')) then
            if (((NOW - RAS_chk1 >= tRAS) AND (((Burst_length_1 = '1' OR Write_burst_mode = '1') AND Count_precharge(1) >= 1 AND NOW - Count_time(1) >= tWRa) OR (Burst_length_2 = '1' AND Count_precharge(1) >= 2 AND NOW - Count_time(1) >= tWRa) OR (Burst_length_4 = '1' AND Count_precharge(1) >= 4 AND NOW - Count_time(1) >= tWRa) OR (Burst_length_8 = '1' AND Count_precharge(1) >= 8 AND NOW - Count_time(1) >= tWRa))) OR (RW_interrupt_write(1) = '1' AND WR_counter(1) >= 1 AND NOW - WR_time(1) >= tWRa)) then
                Auto_precharge(1)     := '0';
                Write_precharge(1)    := '0';
                RW_interrupt_write(1) := '0';
                Pc_b1                 := '1';
                Act_b1                := '0';
                RP_chk1               := NOW;
            END if;
        END if;
        if ((Auto_precharge(2) = '1') AND (Write_precharge(2) = '1')) then
            if (((NOW - RAS_chk2 >= tRAS) AND (((Burst_length_1 = '1' OR Write_burst_mode = '1') AND Count_precharge(2) >= 1 AND NOW - Count_time(2) >= tWRa) OR (Burst_length_2 = '1' AND Count_precharge(2) >= 2 AND NOW - Count_time(2) >= tWRa) OR (Burst_length_4 = '1' AND Count_precharge(2) >= 4 AND NOW - Count_time(2) >= tWRa) OR (Burst_length_8 = '1' AND Count_precharge(2) >= 8 AND NOW - Count_time(2) >= tWRa))) OR (RW_interrupt_write(2) = '1' AND WR_counter(2) >= 1 AND NOW - WR_time(2) >= tWRa)) then
                Auto_precharge(2)     := '0';
                Write_precharge(2)    := '0';
                RW_interrupt_write(2) := '0';
                Pc_b2                 := '1';
                Act_b2                := '0';
                RP_chk2               := NOW;
            END if;
        END if;
        if ((Auto_precharge(3) = '1') AND (Write_precharge(3) = '1')) then
            if (((NOW - RAS_chk3 >= tRAS) AND (((Burst_length_1 = '1' OR Write_burst_mode = '1') AND Count_precharge(3) >= 1 AND NOW - Count_time(3) >= tWRa) OR (Burst_length_2 = '1' AND Count_precharge(3) >= 2 AND NOW - Count_time(3) >= tWRa) OR (Burst_length_4 = '1' AND Count_precharge(3) >= 4 AND NOW - Count_time(3) >= tWRa) OR (Burst_length_8 = '1' AND Count_precharge(3) >= 8 AND NOW - Count_time(3) >= tWRa))) OR (RW_interrupt_write(0) = '1' AND WR_counter(0) >= 1 AND NOW - WR_time(3) >= tWRa)) then
                Auto_precharge(3)     := '0';
                Write_precharge(3)    := '0';
                RW_interrupt_write(3) := '0';
                Pc_b3                 := '1';
                Act_b3                := '0';
                RP_chk3               := NOW;
            END if;
        END if;

        -- Checking internal wires (Optional for debug purpose)
        prechargeCheck(0) <= Pc_b0;
        prechargeCheck(1) <= Pc_b1;
        prechargeCheck(2) <= Pc_b2;
        prechargeCheck(3) <= Pc_b3;
        activeCheck(0)    <= Act_b0;
        activeCheck(1)    <= Act_b1;
        activeCheck(2)    <= Act_b2;
        activeCheck(3)    <= Act_b3;
        Dq_in_chk         <= Data_in_enable;
        Dq_out_chk        <= Data_out_enable;
        bankCheck         <= Bank;
        rowCheck          <= currRow;
        colCheck          <= currCol;
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
            assert (clkEnableIn'LAST_EVENT >= tCKS) --'
            report "CKE Setup time violation -- tCKS"
            severity warning;
            assert (csNegIn'LAST_EVENT >= tCMS) --'
            report "CS# Setup time violation -- tCMS"
            severity warning;
            assert (casNegIn'LAST_EVENT >= tCMS) --'
            report "CAS# Setup time violation -- tCMS"
            severity warning;
            assert (rasNegIn'LAST_EVENT >= tCMS) --'
            report "RAS# Setup time violation -- tCMS"
            severity warning;
            assert (writeEnableIn'LAST_EVENT >= tCMS) --'
            report "WE# Setup time violation -- tCMS"
            severity warning;
            assert (dqmIn'LAST_EVENT >= tCMS) --'
            report "Dqm Setup time violation -- tCMS"
            severity warning;
            assert (addrIn'LAST_EVENT >= tAS) --'
            report "ADDR Setup time violation -- tAS"
            severity warning;
            assert (baIn'LAST_EVENT >= tAS) --'
            report "BA Setup time violation -- tAS"
            severity warning;
            assert (dataInOut'LAST_EVENT >= tDS) --'
            report "Dq Setup time violation -- tDS"
            severity warning;
        end if;
    end process setupChecks;

    -- Hold timing checks
    holdChecks : process
    begin
        wait;
        wait on clkIn'DELAYED(tCKH), clkIn'DELAYED(tCMH), clkIn'DELAYED(tAH), clkIn'DELAYED(tDH);
        if clkIn'DELAYED(tCKH) = '1' then --'
            assert (clkEnableIn'LAST_EVENT > tCKH) --'
            report "CKE Hold time violation -- tCKH"
            severity warning;
        end if;
        if clkIn'DELAYED(tCMH) = '1' then --'
            assert (csNegIn'LAST_EVENT > tCMH) --'
            report "CS# Hold time violation -- tCMH"
            severity warning;
            assert (casNegIn'LAST_EVENT > tCMH) --'
            report "CAS# Hold time violation -- tCMH"
            severity warning;
            assert (rasNegIn'LAST_EVENT > tCMH) --'
            report "RAS# Hold time violation -- tCMH"
            severity warning;
            assert (writeEnableIn'LAST_EVENT > tCMH) --'
            report "WE# Hold time violation -- tCMH"
            severity warning;
            assert (dqmIn'LAST_EVENT > tCMH) --'
            report "Dqm Hold time violation -- tCMH"
            severity warning;
        end if;
        if clkIn'DELAYED(tAH) = '1' then --'
            assert (addrIn'LAST_EVENT > tAH) --'
            report "ADDR Hold time violation -- tAH"
            severity warning;
            assert (baIn'LAST_EVENT > tAH) --'
            report "BA Hold time violation -- tAH"
            severity warning;
        end if;
        if clkIn'DELAYED(tDH) = '1' then --'
            assert (dataInOut'LAST_EVENT > tDH) --'
            report "Dq Hold time violation -- tDH"
            severity warning;
        end if;
    end process holdChecks;

end behave;

-- pragma translate_on
