WORK_DIR = work
TB_DIR = tb
SRC_DIR = src
LIB_DIR = lib

PKG_DIR_SUFFIX = pkgs
MODEL_DIR_SUFFIX = model

GHW_OPTS_SUFFIX = _ghw_opts.txt

COMMON_OPTS = --std=08 --workdir=$(WORK_DIR) -fpsl
IMPORT_OPTS = $(COMMON_OPTS)
MAKE_OPTS = $(COMMON_OPTS) -g -Pwork
RUN_OPTS = --wave=$(WORK_DIR)/$<.ghw

targets = sdram_init_ctrl ccd_ctrl img_convolution i2c_ccd_config i2c_ctrl sdram_ctrl ccd_demosaic data_ctrl ccd_pixel_pipeline
testbenches = $(foreach target, $(targets), $(target)_tb)
runnables = $(foreach target, $(targets), $(target)_test)

poc-obj08.cf: $(LIB_DIR)/PoC/src/mem/ocram/*.vhdl $(LIB_DIR)/PoC/src/mem/*.vhdl $(LIB_DIR)/PoC/src/common/*.vhdl $(LIB_DIR)/PoC/src/fifo/*.vhdl
	ghdl -i $(IMPORT_OPTS) --work=poc lib/PoC/src/mem/*.vhdl lib/PoC/src/mem/ocram/*.vhdl lib/PoC/src/common/*.vhdl lib/PoC/src/fifo/*.vhdl

work-obj08.cf: $(SRC_DIR)/*.vhd $(SRC_DIR)/$(PKG_DIR_SUFFIX)/*.vhd $(SRC_DIR)/$(PKG_DIR_SUFFIX)/img/*.vhd $(SRC_DIR)/$(PKG_DIR_SUFFIX)/mem/*.vhd
work-obj08.cf: $(TB_DIR)/*.vhd $(TB_DIR)/$(PKG_DIR_SUFFIX)/*.vhd $(TB_DIR)/$(MODEL_DIR_SUFFIX)/*.vhd osvvm/
	ghdl -i $(IMPORT_OPTS)  src/*.vhd src/pkgs/*.vhd src/pkgs/img/*.vhd src/pkgs/mem/*.vhd tb/*.vhd tb/pkgs/*.vhd tb/model/*.vhd

$(testbenches): %_tb: poc-obj08.cf work-obj08.cf 
	ghdl -m $(MAKE_OPTS) $@

.PHONY: $(runnables)
$(runnables): %_test: %_tb
	./$*_tb $(if $(wildcard $(TB_DIR)/$*_tb$(GHW_OPTS_SUFFIX)), --read-wave-opt=$(TB_DIR)/$*_tb$(GHW_OPTS_SUFFIX)) $(RUN_OPTS)

osvvm:
	./setup_osvvm.fish

.PHONY: clean
clean: 
	-rm ./*.o
	-rm $(testbenches)
	-rm $(WORK_DIR)/*.o
	-rm $(WORK_DIR)/*.cf
	-rm $(WORK_DIR)/*.ghw

