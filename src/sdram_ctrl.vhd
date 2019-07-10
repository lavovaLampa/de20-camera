library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ram_ctrl_pkg.all;
use work.sdram_pkg.all;

-- writes/reads only FullPage bursts (256 x 16b)
-- auto refresh when Idle
entity sdram_ctrl is
    generic(
        -- assumed refresh burst len
        REFRESH_BURST_LEN : natural := 5
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
    type Temp_State_T is (Idle, CmdWait, Burst);
    type Bank_State_R is record
        isActive : boolean;
        row      : Row_T;
    end record Bank_State_R;
    type Bank_State_Array_T is array (BANK_COUNT - 1 downto 0) of Bank_State_R;

    -- external signals
    signal ramInitialized : boolean := false;

    -- internal signals
    signal ctrlBusy  : boolean            := false;
    signal currState : Temp_State_T       := Idle;
    signal bankState : Bank_State_Array_T := (others => (isActive => false, row => 0));
    signal cmdDone   : boolean            := true;
begin
    mainProc : process(clkIn, rstAsyncIn)
        -- keep last addresses for early bank/row (pre)activation
        variable lastReadAddr, lastWriteAddr : Ctrl_Addr_T := (others => '0');

        -- helper variables
        variable bankPtr : Bank_T := 0;
        variable rowPtr  : Row_T  := 0;
    begin
        if rstAsyncIn = '1' then
            cmdReadyOut    <= false;
            dataReadyOut   <= false;
            ramInitialized <= false;
            ctrlBusy       <= true;
            currState      <= Idle;

            lastReadAddr  := (others => '0');
            lastWriteAddr := (others => '0');
        elsif rising_edge(clkIn) then
            bankPtr := to_integer(addrIn(BANK_ADDR_WIDTH - 1 downto 0));
            rowPtr  := to_integer(addrIn(Ctrl_Addr_T'high downto BANK_ADDR_WIDTH));

            if ramInitialized then
                case currState is
                    when Idle =>
                        case cmdIn is
                            when Read =>
                                lastReadAddr := addrIn;

                            when Write =>
                                lastWriteAddr := addrIn;

                            when Refresh =>
                                null;

                            when NoOP =>
                                null;
                        end case;

                    when CmdWait =>
                        null;

                    when Burst =>
                        null;

                end case;
            end if;
        end if;
    end process mainProc;

    scheduleProc : process(clkIn, rstAsyncIn)
        type Tmp_Statee_T is (Idle, Busy);
        variable currState : Tmp_Statee_t := Idle;
    begin
        if rstAsyncIn = '1' then
            ctrlBusy  <= true;
            cmdDone   <= true;
            currState := Idle;
        elsif rising_edge(clkIn) then
            if ramInitialized then
                case currState is
                    when Idle =>
                        null;

                    when Busy =>
                        null;

                end case;
            end if;
        end if;
    end process scheduleProc;

    cmdReadyOut <= ramInitialized and currState = Idle;
end architecture RTL;
