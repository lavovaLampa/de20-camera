library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library osvvm;
context osvvm.OsvvmContext;

package sdram_pkg is
    -- input clk period
    constant CLK_PERIOD : time := 10 ns;

    -- sdram port widths
    constant BANK_ADDR_WIDTH : natural := 2;
    constant ROW_ADDR_WIDTH  : natural := 12;
    constant COL_ADDR_WIDTH  : natural := 8;
    constant DATA_WIDTH      : natural := 16;
    constant DQM_WIDTH       : natural := DATA_WIDTH / 8;

    -- useful computed constants
    constant BANK_COUNT : natural := 2**BANK_ADDR_WIDTH;
    constant PAGE_LEN   : natural := 2**COL_ADDR_WIDTH;

    -- i/o types
    subtype Addr_T is unsigned(ROW_ADDR_WIDTH - 1 downto 0);
    subtype Col_Addr_T is unsigned(COL_ADDR_WIDTH - 1 downto 0);
    subtype Bank_Addr_T is unsigned(BANK_ADDR_WIDTH - 1 downto 0);
    subtype Data_T is std_logic_vector(DATA_WIDTH - 1 downto 0);
    subtype Dqm_T is std_logic_vector(DQM_WIDTH - 1 downto 0);

    -- internal types/data ranges
    subtype Row_Ptr_T is natural range 0 to 2**ROW_ADDR_WIDTH - 1;
    subtype Col_Ptr_T is natural range 0 to 2**COL_ADDR_WIDTH - 1;
    subtype Bank_Ptr_T is natural range 0 to BANK_COUNT - 1;

    -- SDRAM/banks state definitions
    type Bank_State_T is (Idle, Activating, ActiveRecharging, ActiveIdle, Precharging, Refreshing);
    type Sdram_State_T is (Idle, ReadBurst, WriteBurst, AccessingModeReg);

    -- SDRAM commands
    type Cmd_T is (CmdInhibit, NoOp, Active, Read, Write, Precharge, Refresh, BurstTerminate, LoadModeReg);
    -- Mode register burst types
    type Burst_Type_T is (Interleaved, Sequential);
    -- Mode register burst length
    subtype Burst_Length_T is natural range 0 to 2**ROW_ADDR_WIDTH;
    -- Mode register latency mode
    subtype Latency_Mode_T is natural range 2 to 3;
    -- Mode register write burst mode
    type Write_Burst_Mode_T is (SingleLocation, ProgrammedLength);

    type Mode_Reg_R is record
        burstType      : Burst_Type_T;
        burstLength    : Burst_Length_T;
        latencyMode    : Latency_Mode_T;
        writeBurstMode : Write_Burst_Mode_T;
    end record Mode_Reg_R;

    -- grouped SDRAM command encoding signals
    type Cmd_Aggregate_R is record
        chipSelectNeg    : std_logic;
        rowAddrStrobeNeg : std_logic;
        colAddrStrobeNeg : std_logic;
        writeEnableNeg   : std_logic;
    end record Cmd_Aggregate_R;

    type Mem_IO_R is record
        addr         : Addr_T;
        bankSelect   : Bank_Addr_T;
        clkEnable    : std_logic;
        cmdAggregate : Cmd_Aggregate_R;
        -- x16 input/output data mask
        dqm          : Dqm_T;
    end record Mem_IO_R;

    type Mem_Data_Aggregate_R is record
        cmd  : Cmd_T;
        addr : Addr_T;
        bank : Bank_Addr_T;
        data : Data_T;
    end record Mem_Data_Aggregate_R;

    type Mem_IO_Aggregate_R is record
        cmd  : Cmd_T;
        addr : Addr_T;
        bank : Bank_Addr_T;
    end record Mem_IO_Aggregate_R;

    -- command timings
    -- TODO: do i need this timing?
    constant tARFC   : time := 60 ns;   -- one Auto Refresh cycle time [shortest refresh strobe (Refresh -> Refresh)] (in some SDRAMs tARFC = tRC)
    constant tRC     : time := 60 ns;   -- row cycle (ref to ref / activate to activate) [shortest row access strobe (Idle -> Access -> Idle)]
    constant tRASmin : time := 42 ns;   -- row address strobe (activate to precharge) [shortest row access time (capacitors take time to recover)]
    constant tRASmax : time := 100 us;  -- row active hold time [longest time row can be held active]
    constant tRP     : time := 18 ns;   -- row precharge time (min. time between precharging row and activating new one)
    constant tRCD    : time := 18 ns;   -- RAS to CAS delay (active command to read/write command delay time) [min. time between activating a row and issuing Read/Write command]
    constant tRRD    : time := 12 ns;   -- bank to bank delay time (min. time between successive Active commands to different banks)
    constant tDPL    : time := 2 * CLK_PERIOD; -- input data to Precharge command delay (also defined as tWR)
    constant tDAL    : time := (2 * CLK_PERIOD) + tRP; -- input data to Active/Refresh command delay (during Auto Precharge)
    constant tXSR    : time := 60 ns;   -- exit to Self Refresh to Active
    constant tREF    : time := 64 ms;   -- max. Refresh cycle time (all rows) [4096 rows for current SDRAM]

    -- command-related timings defined in clock cycles
    -- tDAL : natural := 5;
    -- tDPL : natural := 2;
    constant tCAS  : natural := 2;      -- Read command to valid data out/Data in to Precharge? 
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

    -- cycle-converted timings
    constant tRCCycles     : natural;   -- row cycle (ref to ref / activate to activate) [shortest row access strobe (Idle -> Access -> Idle)]
    constant tRASminCycles : natural;   -- row address strobe (activate to precharge) [shortest row access time (capacitors take time to recover)]
    constant tRASmaxCycles : natural;   -- row active hold time [longest time row can be held active]
    constant tRPCycles     : natural;   -- row precharge time (min. time between precharging row and activating new one)
    constant tRCDCycles    : natural;   -- RAS to CAS delay (active command to read/write command delay time) [min. time between activating a row and issuing Read/Write command]
    constant tRRDCycles    : natural;   -- bank to bank delay time (min. time between successive Active commands to different banks)
    constant tDPLCycles    : natural;   -- input data to Precharge command delay (also defined as tWR)
    constant tDALCycles    : natural;   -- input data to Active/Refresh command delay (during Auto Precharge)
    constant tXSRCycles    : natural;   -- exit to Self Refresh to Active
    constant tREFCycles    : natural;   -- Refresh cycle time (all rows) [4096 for current SDRAM]
    constant tARFCCycles   : natural;   -- 

    -- encode/decode sdram commands (read, write, etc.)
    pure function decode_cmd(chipSelectNeg, rowAddrStrobeNeg, colAddrStrobeNeg, writeEnableNeg : std_logic) return Cmd_T;
    pure function encode_cmd(cmd : Cmd_T) return Cmd_Aggregate_R;
    -- encode/decode/validate mode register
    pure function encode_mode_reg(burstLength : Burst_Length_T; burstType : Burst_Type_T; latencyMode : Latency_Mode_T; writeBurstMode : Write_Burst_Mode_T) return Data_T;
    pure function decode_mode_reg(modeReg : Data_T) return Mode_Reg_R;
    pure function validate_mode_reg(modeReg : Data_T) return boolean;

    /** 
     * Return min. number of cycles the ctrl has to wait after requesting specified command.
     * Controller can receive another command in tNow + return_val cycles.
     * E.g. 1 means controller can send next command right during the next clock cycle. 
     **/
    pure function cmd_delay(cmd : Cmd_T) return natural;

    -- cmd i/o interfacing helper functions
    -- these function handle data, don't forget to also update data register!
    pure function write(col : Col_Addr_T; bank : Bank_Addr_T; autoPrecharge : boolean) return Mem_IO_Aggregate_R;
    pure function load_mode_reg(data : Data_T) return Mem_IO_Aggregate_R;

    -- cmd i/o functions w/o data handling
    pure function active(row : Addr_T; bank : Bank_Addr_T) return Mem_IO_Aggregate_R;
    pure function read(col : Col_Addr_T; bank : Bank_Addr_T; autoPrecharge : boolean) return Mem_IO_Aggregate_R;
    pure function precharge(bank : Bank_Addr_T; allBanks : boolean) return Mem_IO_Aggregate_R;
    pure function burst_terminate return Mem_IO_Aggregate_R;
    pure function refresh return Mem_IO_Aggregate_R;
    pure function nop return Mem_IO_Aggregate_R;

    -- cmd i/o intefacing helper functions
    pure function pack_io_data(data : Data_T; memIo : Mem_IO_Aggregate_R) return Mem_Data_Aggregate_R;
    pure function repack_pure_io(memDataIo : Mem_Data_Aggregate_R) return Mem_IO_Aggregate_R;

end package sdram_pkg;

package body sdram_pkg is
    -- types
    subtype Cmd_Aggregate is std_logic_vector(3 downto 0);
    subtype Burst_Length_Logic_T is std_logic_vector(2 downto 0);
    subtype Latency_Mode_Logic_T is std_logic_vector(2 downto 0);

    -- constants
    constant REAL_PERIOD : real := real(CLK_PERIOD / 1 ps);

    -- exported constants
    impure function time_to_cycles(val : time) return natural is
    begin
        assert val >= 1 ns
        report "Time divided by ps, don't use such low values";

        return natural(ceil(real(val / 1 ps) / REAL_PERIOD));
    end function time_to_cycles;

    constant tRCCycles     : natural := time_to_cycles(tRC);
    constant tRASminCycles : natural := time_to_cycles(tRASmin);
    constant tRASmaxCycles : natural := time_to_cycles(tRASmax);
    constant tRPCycles     : natural := time_to_cycles(tRP);
    constant tRCDCycles    : natural := time_to_cycles(tRCD);
    constant tRRDCycles    : natural := time_to_cycles(tRRD);
    constant tDPLCycles    : natural := time_to_cycles(tDPL);
    constant tDALCycles    : natural := time_to_cycles(tDAL);
    constant tXSRCycles    : natural := time_to_cycles(tXSR);
    constant tREFCycles    : natural := time_to_cycles(tREF);
    constant tARFCCycles   : natural := time_to_cycles(tARFC);

    -- functions
    pure function cmd_delay(cmd : Cmd_T) return natural is
    begin
        case cmd is
            when Active                                     => return tRCDCycles;
            when Read                                       => return tCAS;
            when Precharge                                  => return tRPCycles;
            when Refresh                                    => return tARFCCycles;
            when LoadModeReg                                => return tMRD;
            when CmdInhibit | NoOp | Write | BurstTerminate => return 1;
        end case;
    end function cmd_delay;

    pure function decode_cmd(chipSelectNeg, rowAddrStrobeNeg, colAddrStrobeNeg, writeEnableNeg : std_logic) return Cmd_T is
        variable cmdSelectAggregate : std_logic_vector(3 downto 0) := chipSelectNeg & rowAddrStrobeNeg & colAddrStrobeNeg & writeEnableNeg;
    begin
        case cmdSelectAggregate is
            when "1---" => return CmdInhibit;
            when "0111" => return NoOp;
            when "0011" => return Active;
            when "0101" => return Read;
            when "0100" => return Write;
            when "0110" => return BurstTerminate;
            when "0010" => return Precharge;
            when "0001" => return Refresh;
            when "0000" => return LoadModeReg;
            when others =>
                report "Invalid cmd" severity warning;
                return NoOp;
        end case;
    end function decode_cmd;

    pure function encode_cmd_internal(cmd : Cmd_T) return Cmd_Aggregate is
    begin
        case cmd is
            when CmdInhibit     => return "1---";
            when NoOp           => return "0111";
            when Active         => return "0011";
            when Read           => return "0101";
            when Write          => return "0100";
            when BurstTerminate => return "0110";
            when Precharge      => return "0010";
            when Refresh        => return "0001";
            when LoadModeReg    => return "0000";
        end case;
    end function encode_cmd_internal;

    pure function encode_cmd(cmd : Cmd_T) return Cmd_Aggregate_R is
        variable cmdAggregate : Cmd_Aggregate := encode_cmd_internal(cmd);
    begin
        return (
            chipSelectNeg    => cmdAggregate(3),
            rowAddrStrobeNeg => cmdAggregate(2),
            colAddrStrobeNeg => cmdAggregate(1),
            writeEnableNeg   => cmdAggregate(0)
        );
    end function encode_cmd;

    pure function validate_mode_reg(modeReg : Data_T) return boolean is
        variable burstLength    : std_logic_vector(2 downto 0) := modeReg(2 downto 0);
        variable burstType      : std_logic                    := modeReg(3);
        variable latencyMode    : std_logic_vector(2 downto 0) := modeReg(6 downto 4);
        variable operatingMode  : std_logic_vector(1 downto 0) := modeReg(8 downto 7);
        variable writeBurstMode : std_logic                    := modeReg(9);
        variable reserved       : std_logic_vector(1 downto 0) := modeReg(11 downto 10);
        variable isValid        : boolean                      := true;
    begin
        isValid := isValid and (burstLength = "000" or burstLength = "001" or burstLength = "010" or burstLength = "011" or burstLength = "111");
        isValid := isValid and (burstType = '1' or burstType = '0');
        isValid := isValid and not (burstLength = "111" and burstType = '1');
        isValid := isValid and (latencyMode = "010" or latencyMode = "011");
        isValid := isValid and operatingMode = "00";
        isValid := isValid and (writeBurstMode = '1' or writeBurstMode = '0');
        isValid := isValid and reserved = (reserved'range => '0');
        return isValid;
    end function validate_mode_reg;

    pure function encode_burst_length(burstLength : Burst_Length_T) return Burst_Length_Logic_T is
    begin
        case burstLength is
            when 1        => return "000";
            when 2        => return "001";
            when 4        => return "010";
            when 8        => return "011";
            when PAGE_LEN => return "111";
            when others =>
                report "Invalid burst length, cannot encode"
                severity error;
        end case;
    end function encode_burst_length;

    pure function decode_burst_length(data : std_logic_vector(2 downto 0)) return Burst_Length_T is
    begin
        case data is
            when "000" => return 1;
            when "001" => return 2;
            when "010" => return 4;
            when "011" => return 8;
            when "111" => return PAGE_LEN;
            when others =>
                report "Invalid burst length, cannot decode"
                severity error;
        end case;
    end function decode_burst_length;

    pure function encode_burst_type(burstType : Burst_Type_T) return std_logic is
    begin
        case burstType is
            when Interleaved => return '1';
            when Sequential  => return '0';
        end case;
    end function encode_burst_type;

    pure function decode_burst_type(data : std_logic) return Burst_Type_T is
    begin
        case data is
            when '1' => return Interleaved;
            when '0' => return Sequential;
            when others =>
                report "Cannot decode burst type, invalid value"
                severity error;
        end case;
    end function decode_burst_type;

    pure function encode_latency_mode(latencyMode : Latency_Mode_T) return Latency_Mode_Logic_T is
    begin
        case latencyMode is
            when 2 => return "010";
            when 3 => return "011";
            when others =>
                report "Invalid latency mode"
                severity error;
        end case;
    end function encode_latency_mode;

    pure function decode_latency_mode(data : std_logic_vector(2 downto 0)) return Latency_Mode_T is
    begin
        case data is
            when "010" => return 2;
            when "011" => return 3;
            when others =>
                report "Cannot decode latency mode, invalid value"
                severity error;
        end case;
    end function decode_latency_mode;

    pure function encode_write_burst_mode(writeBurstMode : Write_Burst_Mode_T) return std_logic is
    begin
        case writeBurstMode is
            when SingleLocation   => return '1';
            when ProgrammedLength => return '0';
        end case;
    end function encode_write_burst_mode;

    pure function decode_write_burst_mode(data : std_logic) return Write_Burst_Mode_T is
    begin
        case data is
            when '1' => return SingleLocation;
            when '0' => return ProgrammedLength;
            when others =>
                report "Cannot decode write burst mode, invalid value"
                severity error;
        end case;
    end function decode_write_burst_mode;

    pure function encode_mode_reg(burstLength : Burst_Length_T; burstType : Burst_Type_T; latencyMode : Latency_Mode_T; writeBurstMode : Write_Burst_Mode_T) return Data_T is
        variable modeReg : Data_T := (others => '0');
    begin
        modeReg(2 downto 0) := encode_burst_length(burstLength);
        modeReg(3)          := encode_burst_type(burstType);
        modeReg(6 downto 4) := encode_latency_mode(latencyMode);
        modeReg(9)          := encode_write_burst_mode(writeBurstMode);

        assert validate_mode_reg(modeReg)
        report "Invalid mode register setting"
        severity error;

        return modeReg;
    end function encode_mode_reg;

    pure function decode_mode_reg(modeReg : Data_T) return Mode_Reg_R is
    begin
        assert validate_mode_reg(modeReg)
        report "Invalid mode register setting"
        severity error;

        return (
            burstType      => decode_burst_type(modeReg(3)),
            burstLength    => decode_burst_length(modeReg(2 downto 0)),
            latencyMode    => decode_latency_mode(modeReg(6 downto 4)),
            writeBurstMode => decode_write_burst_mode(modeReg(9))
        );
    end function decode_mode_reg;

    pure function active(row : Addr_T; bank : Bank_Addr_T) return Mem_IO_Aggregate_R is
    begin
        return (cmd => Active, bank => bank, addr => row);
    end function active;

    pure function read(col : Col_Addr_T; bank : Bank_Addr_T; autoPrecharge : boolean) return Mem_IO_Aggregate_R is
        variable tmpAddr : Addr_T := (others => '-');
    begin
        tmpAddr(10)                       := '1' when autoPrecharge else '0';
        tmpAddr(Col_Addr_T'high downto 0) := col;

        return (cmd => Read, bank => bank, addr => tmpAddr);
    end function read;

    pure function write(col : Col_Addr_T; bank : Bank_Addr_T; autoPrecharge : boolean) return Mem_IO_Aggregate_R is
        variable tmpAddr : Addr_T := (others => '-');
    begin
        tmpAddr(10)                       := '1' when autoPrecharge else '0';
        tmpAddr(Col_Addr_T'high downto 0) := col;

        return (cmd => Write, bank => bank, addr => tmpAddr);
    end function write;

    pure function precharge(bank : Bank_Addr_T; allBanks : boolean) return Mem_IO_Aggregate_R is
        variable tmpAddr : Addr_T := (others => '-');
    begin
        tmpAddr(10) := '1' when allBanks else '0';

        return (cmd => Precharge, bank => bank, addr => tmpAddr);
    end function precharge;

    pure function burst_terminate return Mem_IO_Aggregate_R is
    begin
        return (cmd => BurstTerminate, bank => (others => '-'), addr => (others => '-'));
    end function burst_terminate;

    pure function refresh return Mem_IO_Aggregate_R is
    begin
        return (cmd => Refresh, bank => (others => '-'), addr => (others => '-'));
    end function refresh;

    pure function load_mode_reg(data : Data_T) return Mem_IO_Aggregate_R is
    begin
        assert validate_mode_reg(data)
        report "Invalid mode register data specified"
        severity warning;

        return (cmd => LoadModeReg, bank => (others => '-'), addr => (others => '-'));
    end function load_mode_reg;

    pure function nop return Mem_IO_Aggregate_R is
    begin
        return (cmd => NoOp, bank => (others => '-'), addr => (others => '-'));
    end function nop;

    pure function pack_io_data(data : Data_T; memIo : Mem_IO_Aggregate_R) return Mem_Data_Aggregate_R is
    begin
        return (addr => memIo.addr, bank => memIo.bank, cmd => memIo.cmd, data => data);
    end function pack_io_data;

    pure function repack_pure_io(memDataIo : Mem_Data_Aggregate_R) return Mem_IO_Aggregate_R is
    begin
        return (addr => memDataIo.addr, bank => memDataIo.bank, cmd => memDataIo.cmd);
    end function repack_pure_io;
end package body sdram_pkg;
