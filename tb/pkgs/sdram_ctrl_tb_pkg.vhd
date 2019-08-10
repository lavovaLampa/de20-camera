library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.sdram_pkg.all;

library osvvm;
context osvvm.OsvvmContext;

package sdram_ctrl_tb_pkg is
    type Page_Data_Array_T is array (Col_Ptr_T) of Data_T;
    type Page_Array_T is protected
        procedure initRandomGen;

        procedure generateFullPage;
        procedure initFromFullPage(newPageArray : Page_Data_Array_T);

        impure function getCol(col : Col_Ptr_T) return Data_T;
        impure function getFullPage return Page_Data_Array_T;
    end protected Page_Array_T;

end package sdram_ctrl_tb_pkg;

package body sdram_ctrl_tb_pkg is
    type Page_Array_T is protected body
        variable pageArray : Page_Data_Array_T;
        variable randomGen : RandomPType;

        procedure initRandomGen is
        begin
            randomGen.InitSeed("Page_Array_T");
        end procedure initRandomGen;

        procedure generateFullPage is
        begin
            for i in pageArray'range loop
                pageArray(i) := randomGen.RandSlv(Data_T'length);
            end loop;
        end procedure generateFullPage;

        procedure initFromFullPage(newPageArray : Page_Data_Array_T) is
        begin
            pageArray := newPageArray;
        end procedure initFromFullPage;

        impure function getCol(col : Col_Ptr_T) return Data_T is
        begin
            return pageArray(col);
        end function getCol;

        impure function getFullPage return Page_Data_Array_T is
        begin
            return pageArray;
        end function getFullPage;
    end protected body Page_Array_T;
end package body sdram_ctrl_tb_pkg;
