library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.sdram_pkg.all;
use work.sdram_ctrl_pkg.all;

entity sdram_ctrl_top is
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
end entity sdram_ctrl_top;

architecture RTL of sdram_ctrl_top is
    signal clkStable      : std_logic := '0';
    signal memInitialized : boolean   := false;
begin
    -- if initialized let the controller communicate with the sdram
    with memInitialized select nextMemData <=
        memCtrlIo when true,
        initCtrlIo when false;

    burstCtrl : entity work.sdram_ctrl
        generic map(
            ROW_MAX         => ROW_MAX,
            READ_BURST_LEN  => READ_BURST_LEN,
            WRITE_BURST_LEN => WRITE_BURST_LEN
        )
        port map(
            clkIn        => clkIn,
            rstAsyncIn   => rstAsyncIn,
            addrIn       => addrIn,
            cmdIn        => cmdIn,
            cmdReadyOut  => cmdReadyOut,
            dataReadyOut => dataReadyOut,
            dataIn       => dataIn,
            dataOut      => dataOut,
            clkStableIn  => clkStable
        );

end architecture RTL;
