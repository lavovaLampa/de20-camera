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
        clkIn, rstAsyncIn             : in    std_logic;
        -- ==============================
        -- |    row_addr    | bank_addr |
        -- ==============================
        -- 13              2 1          0
        addrIn                        : in    Ctrl_Addr_T;
        cmdIn                         : in    Ctrl_Cmd_T;
        cmdReadyOut                   : out   boolean;
        provideNewDataOut, newDataOut : out   boolean;
        dataIn                        : in    Data_T;
        dataOut                       : out   Data_T;
        -- SDRAM I/O
        sdramDataIo                   : inout Data_T;
        sdramOut                      : out   Mem_IO_R;
        clkStableIn                   : in    std_logic
    );
end entity sdram_ctrl_top;

architecture RTL of sdram_ctrl_top is
    signal memInitialized : boolean := false;

    -- managed data inout port
    signal outputEnable : boolean;
    signal memDataIn    : Data_T;
    signal memDataOut   : Data_T;

    -- init ctrl sdram i/o
    signal initMemOut       : Mem_IO_R;
    signal initMemDataOut   : Data_T;
    signal initOutputEnable : boolean;

    -- burst ctrl sdram i/o
    signal ctrlMemOut                    : Mem_IO_R;
    signal ctrlMemDataIn, ctrlMemDataOut : Data_T;
    signal ctrlOutputEnable              : boolean;
begin
    -- manage tri-state data buffer
    sdramDataIo <= memDataOut when outputEnable else (others => 'Z');
    memDataIn   <= sdramDataIo;

    -- if initialized let the controller communicate with the sdram
    with memInitialized select sdramOut <=
        ctrlMemOut when true,
        initMemOut when false;

    -- same with sdram data
    with memInitialized select memDataOut <=
        ctrlMemDataOut when true,
        initMemDataOut when false;

    -- and with output enable controlling tri-state buffer
    with memInitialized select outputEnable <=
        ctrlOutputEnable when true,
        initOutputEnable when false;

    ctrlMemDataIn <= memDataIn;

    burstCtrl : entity work.sdram_burst_ctrl
        generic map(
            PAGES_REQUIRED => PAGES_REQUIRED,
            BURST_LEN      => (
                Read  => READ_BURST_LEN,
                Write => WRITE_BURST_LEN
            )
        )
        port map(
            clkIn                  => clkIn,
            rstAsyncIn             => rstAsyncIn,
            addrIn                 => addrIn,
            cmdIn                  => cmdIn,
            cmdReadyOut            => cmdReadyOut,
            provideNewDataOut      => provideNewDataOut,
            newDataOut             => newDataOut,
            dataIn                 => dataIn,
            dataOut                => dataOut,
            memInitializedIn       => memInitialized,
            memOut                 => ctrlMemOut,
            memDataOut             => ctrlMemDataOut,
            memDataIn              => ctrlMemDataIn,
            memDataOutputEnableOut => ctrlOutputEnable
        );

    initCtrl : entity work.sdram_init_ctrl
        generic map(
            MODE_REG => encode_mode_reg(PAGE_LEN, Sequential, 2, ProgrammedLength)
        )
        port map(
            clkIn               => clkIn,
            rstAsyncIn          => rstAsyncIn,
            clkStableIn         => clkStableIn,
            memInitializedOut   => memInitialized,
            memOut              => initMemOut,
            memDataOut          => initMemDataOut,
            memDataOutputEnable => initOutputEnable
        );

end architecture RTL;
