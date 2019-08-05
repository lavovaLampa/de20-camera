library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package img_pkg is
    constant IMG_WIDTH : natural := 640;
    constant IMG_HEIGHT : natural := 480;
    constant PIXEL_WIDTH : natural := 8;

    subtype Img_Width_Ptr_T is natural range 0 to IMG_WIDTH - 1;
    subtype Img_Height_Ptr_T is natural range 0 to IMG_HEIGHT - 1;
    subtype Img_Ptr_T is natural range 0 to (IMG_WIDTH * IMG_HEIGHT) - 1;

    -- shared internal pixel type definitions
    type Pixel_Color_T is (Red, Green, Blue);

    subtype Pixel_Data_T is unsigned(PIXEL_WIDTH - 1 downto 0);
    type Pixel_Aggregate_T is array (Pixel_Color_T) of Pixel_Data_T;
    type Pixel_Matrix_T is array (2 downto 0, 2 downto 0) of Pixel_Data_T;
end package img_pkg;

package body img_pkg is

end package body img_pkg;
