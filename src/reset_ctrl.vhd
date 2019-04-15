library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity reset_ctrl is
    generic(
        counterMax : natural := 10000
    );
    port(
        clkIn, rstAsyncIn : in  std_logic;
        rstOut            : out std_logic := '1';
        ccdNegRstOut      : out std_logic := '0'
    );
end entity reset_ctrl;

architecture RTL of reset_ctrl is
    signal rstCounter : natural range 0 to counterMax := 0;
begin
    rstProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            ccdNegRstOut <= '0';
            rstOut       <= '1';
            rstCounter   <= 0;
        elsif rising_edge(clkIn) then
            if rstCounter < counterMax then
                rstCounter <= rstCounter + 1;
            end if;

            -- change timing after SDRAM ctrl addition
            -- bring up digital supplies first
            if rstCounter > 1000 then
                rstOut <= '0';
            end if;
            -- start ccd
            if rstCounter > 1500 then
                ccdNegRstOut <= '1';
            end if;
        end if;

    end process rstProc;

end architecture RTL;
