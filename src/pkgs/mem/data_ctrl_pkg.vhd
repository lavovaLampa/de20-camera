library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.sdram_ctrl_pkg.Ctrl_Addr_T;
use work.sdram_pkg.Data_T;

package data_ctrl_pkg is
    constant STATE_BITS : natural := 3;

    subtype Packed_Pixel_T is std_logic_vector(23 downto 0);

    subtype Fifo_Fill_State_T is unsigned(STATE_BITS - 1 downto 0);
    type Fifo_Fill_State_R is record
        readFillState   : Fifo_Fill_State_T;
        writeEmptyState : Fifo_Fill_State_T;
    end record Fifo_Fill_State_R;
    type Fifo_Write_Port_State_R is record
        full       : boolean;
        data       : Packed_Pixel_T;
        put        : boolean;
        emptyState : Fifo_Fill_State_T;
    end record Fifo_Write_Port_State_R;
    type Fifo_Read_Port_State_R is record
        valid     : boolean;
        data      : Packed_Pixel_T;
        got       : boolean;
        fillState : Fifo_Fill_State_T;
    end record Fifo_Read_Port_State_R;
    type Fifo_State_R is record
        readPort  : Fifo_Read_Port_State_R;
        writePort : Fifo_Write_Port_State_R;
    end record Fifo_State_R;

    type Pixel_Pack_T is (RedGreen, BlueRed, GreenBlue);
    type Pixel_Pack_State_Map_T is array (Pixel_Pack_T) of Pixel_Pack_T;
    constant next_packing : Pixel_Pack_State_Map_T := (
        RedGreen  => BlueRed,
        BlueRed   => GreenBlue,
        GreenBlue => RedGreen
    );

    type Mem_Port_T is (Read, Write);
    type Mem_Port_State_R is record
        addrPtr      : Ctrl_Addr_T;
        pixelPacking : Pixel_Pack_T;
        dataHold     : Data_T;
    end record Mem_Port_State_R;
    type Port_State_Array_T is array (Mem_Port_T) of Mem_Port_State_R;
end package data_ctrl_pkg;
