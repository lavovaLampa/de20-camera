-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- =============================================================================
-- Authors:					Thomas B. Preusser
--									Steffen Koehler
--									Martin Zabel
--
-- Entity:					FIFO, independent clocks (ic), first-word-fall-through mode
--
-- Description:
-- -------------------------------------
-- Independent clocks meens that read and write clock are unrelated.
--
-- This implementation uses dedicated block RAM for storing data.
--
-- First-word-fall-through (FWFT) mode is implemented, so data can be read out
-- as soon as ``valid`` goes high. After the data has been captured, then the
-- signal ``got`` must be asserted.
--
-- Synchronous reset is used. Both resets may overlap.
--
-- ``DATA_REG`` (=true) is a hint, that distributed memory or registers should be
-- used as data storage. The actual memory type depends on the device
-- architecture. See implementation for details.
--
-- ``*STATE_*_BITS`` defines the granularity of the fill state indicator
-- ``*state_*``. ``fstate_rd`` is associated with the read clock domain and outputs
-- the guaranteed number of words available in the FIFO. ``estate_wr`` is
-- associated with the write clock domain and outputs the number of words that
-- is guaranteed to be accepted by the FIFO without a capacity overflow. Note
-- that both these indicators cannot replace the ``full`` or ``valid`` outputs as
-- they may be implemented as giving pessimistic bounds that are minimally off
-- the true fill state.
--
-- If a fill state is not of interest, set *STATE_*_BITS = 0.
--
-- ``fstate_rd`` and ``estate_wr`` are combinatorial outputs and include an address
-- comparator (subtractor) in their path.
--
-- Examples:
-- - FSTATE_RD_BITS = 1: fstate_rd == 0 => 0/2 full
--                       fstate_rd == 1 => 1/2 full (half full)
--
-- - FSTATE_RD_BITS = 2: fstate_rd == 0 => 0/4 full
--                       fstate_rd == 1 => 1/4 full
--                       fstate_rd == 2 => 2/4 full
--                       fstate_rd == 3 => 3/4 full
--
-- License:
-- =============================================================================
-- Copyright 2007-2014 Technische Universitaet Dresden - Germany
--                     Chair of VLSI-Design, Diagnostics and Architecture
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--		http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-- =============================================================================

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

library poc;
use PoC.utils.all;
use PoC.ocram.all;                      -- "all" required by Quartus RTL simulation

entity fifo_ic_got is
    generic(
        DATA_WIDTH     : positive;      -- Data Width
        MIN_DEPTH      : positive;      -- Minimum FIFO Depth
        DATA_REG       : boolean := false; -- Store Data Content in Registers
        OUTPUT_REG     : boolean := false; -- Registered FIFO Output
        ESTATE_WR_BITS : natural := 0;  -- Empty State Bits
        FSTATE_RD_BITS : natural := 0   -- Full State Bits
    );
    port(
        -- Write Interface
        clkWriteIn, rstWriteIn : in  std_logic; -- synchronous reset
        put                    : in  boolean; -- whether to advance write pointer
        dataIn                 : in  std_logic_vector(DATA_WIDTH - 1 downto 0);
        full                   : out boolean; -- full flag
        estate_wr              : out unsigned(imax(ESTATE_WR_BITS - 1, 0) downto 0); -- write "emptiness" state (i.e. how "empty" the fifo is)
        -- Read Interface
        clkReadIn, rstReadIn   : in  std_logic; -- synchronous reset
        got                    : in  boolean; -- whether to advance read pointer
        valid                  : out boolean; -- whether the data is valid on dataOut (FWFT)
        dataOut                : out std_logic_vector(DATA_WIDTH - 1 downto 0);
        fstate_rd              : out unsigned(imax(FSTATE_RD_BITS - 1, 0) downto 0) -- read "fullness" state (i.e. how "full" the fifo is)
    );
end entity fifo_ic_got;

architecture rtl of fifo_ic_got is

    -- Constants
    constant ADDR_WIDTH : positive := log2ceilnz(MIN_DEPTH);
    constant AN         : positive := ADDR_WIDTH + 1;

    -- Registers, clk_wr domain
    signal IP1     : std_logic_vector(AN - 1 downto 0); -- IP + 1
    signal IP0     : std_logic_vector(AN - 1 downto 0) := (others => '0'); -- Write Pointer IP
    signal IPz     : std_logic_vector(AN - 1 downto 0) := (others => '0'); -- IP delayed by one clock
    signal OPs     : std_logic_vector(AN - 1 downto 0) := (others => '0'); -- Sync stage: OP0 -> OPc
    signal OPc     : std_logic_vector(AN - 1 downto 0) := (others => '0'); -- Copy of OP
    signal memFull : boolean                           := false; -- RAM full

    -- Registers, clk_rd domain
    signal OP1              : std_logic_vector(AN - 1 downto 0); -- OP + 1
    signal OP0              : std_logic_vector(AN - 1 downto 0) := (others => '0'); -- Read Pointer OP
    signal IPs              : std_logic_vector(AN - 1 downto 0) := (others => '0'); -- Sync stage: IPz -> IPc
    signal IPc              : std_logic_vector(AN - 1 downto 0) := (others => '0'); -- Copy of IP
    signal memDataAvailable : boolean                           := false; -- RAM Data available
    signal outputValid      : boolean                           := false; -- Output Valid

    -- Memory Connectivity
    signal memWriteAddr : unsigned(ADDR_WIDTH - 1 downto 0);
    signal memDataIn    : std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal putInternal  : boolean;

    signal memReadAddr : unsigned(ADDR_WIDTH - 1 downto 0);
    signal memDataOut  : std_logic_vector(DATA_WIDTH - 1 downto 0);
    signal getInternal : boolean;

    signal gotInternal : boolean;       -- Internal Read ACK

begin

    -----------------------------------------------------------------------------
    -- Write clock domain
    -----------------------------------------------------------------------------
    blkIP : block
        signal counter : unsigned(AN - 1 downto 0) := to_unsigned(1, AN);
    begin
        process(clkWriteIn)
        begin
            if rising_edge(clkWriteIn) then
                if rstWriteIn = '1' then
                    counter <= to_unsigned(1, AN);
                elsif putInternal then
                    counter <= counter + 1;
                end if;
            end if;
        end process;
        IP1 <= std_logic_vector(counter(ADDR_WIDTH) & (counter(ADDR_WIDTH - 1 downto 0) xor ('0' & counter(ADDR_WIDTH - 1 downto 1))));
    end block blkIP;

    -- Update Write Pointer upon puti
    process(clkWriteIn)
    begin
        if rising_edge(clkWriteIn) then
            if rstWriteIn = '1' then
                IP0     <= (others => '0');
                IPz     <= (others => '0');
                OPs     <= (others => '0');
                OPc     <= (others => '0');
                memFull <= false;
            else
                IPz <= IP0;
                OPs <= OP0;
                OPc <= OPs;
                if putInternal then
                    IP0 <= IP1;
                    if IP1(ADDR_WIDTH - 1 downto 0) = OPc(ADDR_WIDTH - 1 downto 0) then
                        memFull <= true;
                    else
                        memFull <= false;
                    end if;
                end if;
                if memFull then
                    if IP0 = (not OPc(ADDR_WIDTH) & OPc(ADDR_WIDTH - 1 downto 0)) then
                        memFull <= true;
                    else
                        memFull <= false;
                    end if;
                end if;
            end if;
        end if;
    end process;
    putInternal <= put and not memFull;
    full        <= memFull;

    memDataIn    <= dataIn;
    memWriteAddr <= unsigned(IP0(ADDR_WIDTH - 1 downto 0));

    -----------------------------------------------------------------------------
    -- Read clock domain
    -----------------------------------------------------------------------------
    blkOP : block
        signal counter : unsigned(AN - 1 downto 0) := to_unsigned(1, AN);
    begin
        process(clkReadIn)
        begin
            if rising_edge(clkReadIn) then
                if rstReadIn = '1' then
                    counter <= to_unsigned(1, AN);
                elsif getInternal then
                    counter <= counter + 1;
                end if;
            end if;
        end process;
        OP1 <= std_logic_vector(counter(ADDR_WIDTH) & (counter(ADDR_WIDTH - 1 downto 0) xor ('0' & counter(ADDR_WIDTH - 1 downto 1))));
    end block blkOP;

    process(clkReadIn)
    begin
        if rising_edge(clkReadIn) then
            if rstReadIn = '1' then
                OP0              <= (others => '0');
                IPs              <= (others => '0');
                IPc              <= (others => '0');
                memDataAvailable <= false;
                outputValid      <= false;
            else
                IPs <= IPz;
                IPc <= IPs;
                if getInternal then
                    OP0         <= OP1;
                    if OP1(ADDR_WIDTH - 1 downto 0) = IPc(ADDR_WIDTH - 1 downto 0) then
                        memDataAvailable <= false;
                    else
                        memDataAvailable <= true;
                    end if;
                    outputValid <= true;
                elsif gotInternal then
                    outputValid <= false;
                end if;

                if not memDataAvailable then
                    if OP0 = IPc then
                        memDataAvailable <= false;
                    else
                        memDataAvailable <= true;
                    end if;
                end if;

            end if;
        end if;
    end process;
    getInternal <= (not outputValid or gotInternal) and memDataAvailable;
    memReadAddr <= unsigned(OP0(ADDR_WIDTH - 1 downto 0));

    -----------------------------------------------------------------------------
    -- Add register to data output
    --
    -- Not needed if DATA_REG = true, because "dout" is already feed from a
    -- register in that case.
    -----------------------------------------------------------------------------
    genRegN : if DATA_REG or not OUTPUT_REG generate
        gotInternal <= got;
        dataOut     <= memDataOut;
        valid       <= outputValid;
    end generate genRegN;
    genRegY : if (not DATA_REG) and OUTPUT_REG generate
        signal Buf  : std_logic_vector(DATA_WIDTH - 1 downto 0) := (others => '-');
        signal VldB : boolean                                   := false;
    begin
        process(clkReadIn)
        begin
            if rising_edge(clkReadIn) then
                if rstReadIn = '1' then
                    Buf  <= (others => '-');
                    VldB <= false;
                elsif gotInternal then
                    Buf  <= memDataOut;
                    VldB <= outputValid;
                end if;
            end if;
        end process;
        gotInternal <= not VldB or got;
        dataOut     <= Buf;
        valid       <= VldB;
    end generate genRegY;

    -----------------------------------------------------------------------------
    -- Fill State
    -----------------------------------------------------------------------------
    -- Write Clock Domain
    gEstateWr : if ESTATE_WR_BITS >= 1 generate
        signal d : unsigned(ADDR_WIDTH - 1 downto 0);
    begin
        d         <= gray2bin(OPc(ADDR_WIDTH - 1 downto 0)) + not gray2bin(IP0(ADDR_WIDTH - 1 downto 0));
        estate_wr <= (others => '0') when memFull else unsigned(d(d'left downto d'left - ESTATE_WR_BITS + 1));
    end generate gEstateWr;
    gNoEstateWr : if ESTATE_WR_BITS = 0 generate
        estate_wr <= "X";
    end generate gNoEstateWr;

    -- Read Clock Domain
    gFstateRd : if FSTATE_RD_BITS >= 1 generate
        signal d : unsigned(ADDR_WIDTH - 1 downto 0);
    begin
        d         <= gray2bin(IPc(ADDR_WIDTH - 1 downto 0)) + not gray2bin(OP0(ADDR_WIDTH - 1 downto 0));
        fstate_rd <= (others => '0') when not memDataAvailable else unsigned(d(d'left downto d'left - FSTATE_RD_BITS + 1));
    end generate gFstateRd;
    gNoFstateRd : if FSTATE_RD_BITS = 0 generate
        fstate_rd <= "X";
    end generate gNoFstateRd;

    -----------------------------------------------------------------------------
    -- Memory Instantiation
    -----------------------------------------------------------------------------
    gLarge : if not DATA_REG generate
        ram : entity PoC.ocram_sdp
            generic map(
                A_BITS => ADDR_WIDTH,
                D_BITS => DATA_WIDTH
            )
            port map(
                wclk => clkWriteIn,
                rclk => clkReadIn,
                wce  => '1',
                rce  => to_sl(getInternal),
                we   => to_sl(putInternal),
                ra   => memReadAddr,
                wa   => memWriteAddr,
                d    => memDataIn,
                q    => memDataOut
            );
    end generate gLarge;

    gSmall : if DATA_REG generate
        -- Memory modelled as Array
        type regfile_t is array (0 to 2**ADDR_WIDTH - 1) of std_logic_vector(DATA_WIDTH - 1 downto 0);
        signal regfile      : regfile_t;
        attribute ram_style : string;   -- XST specific
        attribute ram_style of regfile : signal is "distributed";

        -- Altera Quartus II: Allow automatic RAM type selection.
        -- For small RAMs, registers are used on Cyclone devices and the M512 type
        -- is used on Stratix devices. Pass-through logic is not required as
        -- reads do not occur on write addresses.
        -- Warning about undefined read-during-write behaviour can be ignored.
        attribute ramstyle : string;
        attribute ramstyle of regfile : signal is "no_rw_check";
    begin

        -- Memory State
        process(clkWriteIn)
        begin
            if rising_edge(clkWriteIn) then
                --synthesis translate_off
                if SIMULATION and (rstWriteIn = '1') then
                    regfile <= (others => (others => '-'));
                else
                    --synthesis translate_on
                    if putInternal then
                        regfile(to_integer(memWriteAddr)) <= memDataIn;
                    end if;
                    --synthesis translate_off
                end if;
                --synthesis translate_on
            end if;
        end process;

        -- Memory Output
        process(clkReadIn)
        begin                           -- process
            if rising_edge(clkReadIn) then
                if SIMULATION and (rstReadIn = '1') then
                    memDataOut <= (others => 'U');
                elsif getInternal then
                    if Is_X(std_logic_vector(memReadAddr)) then
                        memDataOut <= (others => 'X');
                    else
                        memDataOut <= regfile(to_integer(memReadAddr));
                    end if;
                end if;
            end if;
        end process;
    end generate gSmall;

end rtl;
