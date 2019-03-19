library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity reset_ctrl is
    generic(
        counterMax : natural := 10000
    );
    port(
        clkIn, rstAsyncIn : in  std_logic;
        i2cDoneIn         : in  boolean;
        rstI2cOut         : out std_logic := '1';
        rstCtrlOut        : out std_logic := '1';
        rstCcdOutNeg      : out std_logic := '0'
    );
end entity reset_ctrl;

architecture RTL of reset_ctrl is
    signal rstCounter          : natural range 0 to counterMax := 0;
    -- cross-clock domain synchronization (waiting for slower clocked reg)
    signal sync1, sync2, sync3 : boolean                       := false;
begin
    rstProc : process(clkIn, rstAsyncIn)
    begin
        if rstAsyncIn = '1' then
            rstCcdOutNeg <= '0';
            rstI2cOut    <= '1';
            rstCtrlOut   <= '1';
            sync1        <= false;
            sync2        <= false;
            sync3        <= false;
            rstCounter   <= 0;
        elsif rising_edge(clkIn) then
            sync1 <= i2cDoneIn;
            sync2 <= sync1;
            sync3 <= sync2;

            if rstCounter < counterMax then
                rstCounter <= rstCounter + 1;
            end if;
            if rstCounter > counterMax / 10 then
                rstCcdOutNeg <= '1';
            end if;
            if rstCounter > counterMax / 2 then
                rstI2cOut <= '0';
            end if;
            if sync2 and sync3 then
                rstCtrlOut <= '0';
            end if;
        end if;

    end process rstProc;

end architecture RTL;
