package sdram_pkg is
    type Ctrl_Cmd_T is (NoOp, Read, Write, Refresh);

    -- input clk period
    constant CLK_PERIOD : time := 7.5 ns;

    constant COL_ADDR_WIDTH  : natural := 12;
    constant ROW_ADDR_WIDTH  : natural := 8;
    constant BANK_ADDR_WIDTH : natural := 2;

    constant ADDR_WIDTH : natural := COL_ADDR_WIDTH + ROW_ADDR_WIDTH;
    constant DATA_WIDTH : natural := 16;

    -- command timings
    constant tRC  : time := 55 ns;      -- row cycle (ref to ref / activate to activate) [shortest row access strobe (Idle -> Access -> Idle)]
    constant tRAS : time := 40 ns;      -- row address strobe (activate to precharge) [shortest row access time (capacitors take time to recover)]
    constant tRP  : time := 15 ns;      -- row precharge time (min. time between precharging row and activating new one)
    constant tRCD : time := 15 ns;      -- RAS to CAS delay (active command to read/write command delay time) [min. time between activating a row and issuing Read/Write command]
    constant tRRD : time := 10 ns;      -- bank to bank delay time (min. time between successive Active commands to different banks)
    constant tDPL : time := 2 * CLK_PERIOD; -- input data to Precharge command delay (also defined as tWR)
    constant tDAL : time := (2 * CLK_PERIOD) + tRP; -- input data to Active/Refresh command delay (during Auto Precharge)
    constant tXSR : time := 60 ns;      -- exit to Self Refresh to Active
    constant tREF : time := 64 ms;      -- Refresh cycle time (all rows) [4096 for current SDRAM]

    -- command-related timings defined in clock cycles
    -- tDAL : natural := 4;
    -- tDPL : natural := 2;
    constant tCCD  : natural := 1;      -- Read/Write command to Read/Write command
    constant tCKED : natural := 1;      -- CKE to clock disable or power-down entry mode
    constant tPED  : natural := 1;      -- CKE to clock enable or power-down exit setup mode
    constant tDQD  : natural := 0;      -- DQM to input data delay
    constant tDQM  : natural := 0;      -- DQM to data mask during Writes
    constant tDQZ  : natural := 2;      -- DQM to data high-impedance during Reads
    constant tDWD  : natural := 0;      -- Write command to input data delay
    constant tBDL  : natural := 1;      -- Last data-in to Burst Terminate command
    constant tCDL  : natural := 1;      -- Last data-in to new Read/Write command
    constant tRDL  : natural := 2;      -- Last data-in to Precharge command
    constant tMRD  : natural := 2;      -- Load Mode Register command to Active or Refresh command
    constant tROH  : natural := 2;      -- Data-out ot high-impedance from Precharge command

    -- low-level timings
    constant tAC  : time := 6.0 ns;     -- max. access time from clk
    constant tHZ  : time := 7.0 ns;     -- output high impedance time
    constant tOH  : time := 2.7 ns;     -- output data hold time
    constant tWRa : time := 7.5 ns;     -- A2 Version - Auto precharge mode only (1 Clk + 7.5 ns)
    constant tWRp : time := 15.0 ns;    -- A2 Version - Precharge mode only (15 ns)
    constant tCKA : time := 0 ns;       -- CKE to CLK recovery delay time
    constant tAH  : time := 0.8 ns;     -- address hold time
    constant tAS  : time := 1.5 ns;     -- address setup time
    constant tCH  : time := 2.5 ns;     -- clk high level width
    constant tCL  : time := 2.5 ns;     -- clk low level width
    constant tCK  : time := 7.5 ns;     -- clk cycle time
    constant tDH  : time := 0.8 ns;     -- input data hold time
    constant tDS  : time := 1.5 ns;     -- input data setup time
    constant tCKH : time := 0.8 ns;     -- CKE hold time (clk enable?)
    constant tCKS : time := 1.5 ns;     -- CKE setup time
    constant tCMH : time := 0.8 ns;     -- command hold time
    constant tCMS : time := 1.5 ns;     -- command setup time
end package sdram_pkg;
