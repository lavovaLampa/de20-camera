library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.sdram_pkg.all;

-- writes/reads only FullPage bursts (256 x 16b)
-- auto refresh when Idle
entity sdram_ctrl is
    port(
        clkIn, rstAsyncIn         : in    std_logic;
        -- ==============================
        -- |    col_addr    | bank_addr |
        -- ==============================
        -- 13              2 1          0
        addrIn                    : in    std_logic_vector(ADDR_WIDTH - 1 downto 0);
        cmdIn                     : in    Ctrl_Cmd_T;
        cmdReadyOut, dataReadyOut : out   boolean;
        dataIO                    : inout std_logic_vector(DATA_WIDTH - 1 downto 0)
    );
end entity sdram_ctrl;

architecture RTL of sdram_ctrl is

begin
    mainProc : process(clkIn)
    begin
    end process mainProc;

    bankCtrl : process(clkIn)
    begin
    end process bankCtrl;
end architecture RTL;
