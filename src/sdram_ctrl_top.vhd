library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.sdram_pkg.all;
use work.sdram_ctrl_pkg.all;

entity sdram_ctrl_top is
    generic(
        PAGES_REQUIRED  : natural := 1800;
        READ_BURST_LEN  : natural := 5;
        WRITE_BURST_LEN : natural := 4
    );
    port(
        clkIn, rstAsyncIn         : in    std_logic;
        -- ==============================
        -- |    row_addr    | bank_addr |
        -- ==============================
        -- 13              2 1          0
        addrIn                    : in    Ctrl_Addr_T;
        cmdIn                     : in    Ctrl_Cmd_T;
        cmdReadyOut               : out   boolean;
        dataReadyOut, nextDataOut : out   boolean;
        dataIn                    : in    Data_T;
        dataOut                   : out   Data_T;
        -- SDRAM I/O
        sdramDataIo               : inout Data_T;
        sdramOut                  : out   Mem_IO_R;
        clkStableIn               : in    std_logic
    );
end entity sdram_ctrl_top;

architecture RTL of sdram_ctrl_top is
    signal memInitialized : boolean := false;

    -- init ctrl SDRAM I/O
    signal initMemOut    : Mem_IO_R;
    signal initMemDataIo : Data_T;

    -- burst ctrl SDRAM I/O
    signal ctrlMemOut    : Mem_IO_R;
    signal ctrlMemDataIo : Data_T;
begin
    -- if initialized let the controller communicate with the sdram
    with memInitialized select sdramOut <=
        ctrlMemOut when true,
        initMemOut when false;

    --    ctrlMemDataIo <= sdramDataIo;

    -- same with sdram data
    with memInitialized select sdramDataIo <=
        ctrlMemDataIo when true,
        initMemDataIo when false;

    burstCtrl : entity work.sdram_ctrl
        generic map(
            PAGES_REQUIRED => PAGES_REQUIRED,
            BURST_LEN      => (
                Read  => READ_BURST_LEN,
                Write => WRITE_BURST_LEN
            )
        )
        port map(
            clkIn            => clkIn,
            rstAsyncIn       => rstAsyncIn,
            addrIn           => addrIn,
            cmdIn            => cmdIn,
            cmdReadyOut      => cmdReadyOut,
            dataReadyOut     => dataReadyOut,
            nextDataOut      => nextDataOut,
            dataIn           => dataIn,
            dataOut          => dataOut,
            memInitializedIn => memInitialized,
            sdramOut         => ctrlMemOut,
            sdramDataIo      => ctrlMemDataIo
        );

    initCtrl : entity work.sdram_init_ctrl
        generic map(
            MODE_REG => encode_mode_reg(PAGE_LEN, Sequential, 2, ProgrammedLength)
        )
        port map(
            clkIn             => clkIn,
            rstAsyncIn        => rstAsyncIn,
            clkStableIn       => clkStableIn,
            memInitializedOut => memInitialized,
            memOut            => initMemOut,
            memDataIo         => initMemDataIo
        );

end architecture RTL;
