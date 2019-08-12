package vga_pkg is
    constant IMG_HEIGHT : natural := 480;
    constant IMG_WIDTH  : natural := 640;

    -- horizontal timing
    constant LINE_FRONT_PORCH        : natural := 16;
    constant LINE_SYNC_PULSE         : natural := 96;
    constant LINE_BACK_PORCH         : natural := 48;
    -- blanking sum
    constant HORIZONTAL_BLANK_CYCLES : natural := LINE_FRONT_PORCH + LINE_SYNC_PULSE + LINE_BACK_PORCH;

    -- vertical timing
    constant FRAME_FRONT_PORCH     : natural := 10;
    constant FRAME_SYNC_PULSE      : natural := 2;
    constant FRAME_BACK_PORCH      : natural := 33;
    -- blanking sum
    constant VERTICAL_BLANK_CYCLES : natural := FRAME_FRONT_PORCH + FRAME_SYNC_PULSE + FRAME_BACK_PORCH;
end package vga_pkg;
