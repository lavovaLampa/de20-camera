library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.ccd_pkg.all;

entity color_kernel is
    generic(
        -- edge detection kernel
        kernelParams   : Kernel_Params_Arr  := (
            -1, -1, -1,
            -1,  8, -1,
            -1, -1, -1
        );
        prescaleAmount : Prescale_Range := 0
    );
    port(
        clkIn, rstAsyncIn : in  std_logic;
        dataIn            : in  Ccd_Color_Matrix;
        enableIn          : in  boolean;
        dataOut           : out Ccd_Pixel_Data := X"000"
    );
    type Adder_Tree is record
        stage1 : Adder_Acc_1;
        stage2 : Adder_Acc_2;
        stage3 : Adder_Acc_3;
    end record Adder_Tree;
end entity color_kernel;

architecture RTL of color_kernel is
    -- parallel multiplication register (to enable pipelining)
    signal mulAcc  : Mul_Acc    := (others => (others => '0'));
    -- pipelined adder tree
    signal addTree : Adder_Tree := (stage1 => (others => (others => '0')),
                                    stage2 => (others => (others => '0')),
                                    stage3 => (others => (others => '0'))
                                   );
begin
    kernelProc : process(clkIn, rstAsyncIn)
        variable tempVal : signed(25 downto 0);
    begin
        if rstAsyncIn = '1' then
            mulAcc         <= (others => (others => '0'));
            addTree.stage1 <= (others => (others => '0'));
            addTree.stage2 <= (others => (others => '0'));
            addTree.stage3 <= (others => (others => '0'));
            tempVal        := (others => '0');
        elsif rising_edge(clkIn) then
            if enableIn then
                for i in dataIn'low to dataIn'high loop
                    -- signed 13b * signed 9b = signed 21b
                    mulAcc(i) <= signed(resize(dataIn(i), dataIn(0)'length + 1)) * to_signed(kernelParams(i), KERNEL_PARAMS.data_len);
                end loop;

                for i in mulAcc'low to (mulAcc'high - 1) / 2 loop
                    -- widen numbers before addition to prevent overflow (21b -> 22b)
                    addTree.stage1(i) <= widen(mulAcc(2 * i)) + widen(mulAcc((2 * i) + 1));
                end loop;
                addTree.stage1(addTree.stage1'high) <= widen(mulAcc(mulAcc'high));

                for i in addTree.stage1'low to (addTree.stage1'high - 1) / 2 loop
                    -- widen numbers before addition to prevent overflow (22b -> 23b)
                    addTree.stage2(i) <= widen(addTree.stage1(2 * i)) + widen(addTree.stage1((2 * i) + 1));
                end loop;
                addTree.stage2(addTree.stage2'high) <= widen(addTree.stage1(addTree.stage1'high));

                for i in addTree.stage2'low to (addTree.stage2'high - 1) / 2 loop
                    -- widen numbers before addition to prevent overflow (23b -> 24b)
                    addTree.stage3(i) <= widen(addTree.stage2(2 * i)) + widen(addTree.stage2((2 * i) + 1));
                end loop;
                addTree.stage3(addTree.stage3'high) <= widen(addTree.stage2(addTree.stage2'high));

                -- widen numbers before addition to prevent overflow (24b -> 25b)
                tempVal := widen(addTree.stage3(0)) + widen(addTree.stage3(1));
                -- optional final sum prescaling (division by power of 2)
                tempVal := tempVal / 2**(abs prescaleAmount);
                -- optional final sum negation
                if (prescaleAmount < 0) then
                    tempVal := -tempVal;
                else
                    tempVal := tempVal;
                end if;

                -- TODO: test this function
                dataOut <= toSaturatedUnsigned(tempVal, dataOut'length);

            end if;
        end if;
    end process kernelProc;
end architecture RTL;
