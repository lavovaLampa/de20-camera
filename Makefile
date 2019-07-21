WORK_DIR = work
TB_DIR = tb
SRC_DIR = src
LIB_DIR = lib

PKG_DIR_SUFFIX = common_pkgs
MODEL_DIR_SUFFIX = model

GHW_OPTS_SUFFIX = _ghw_opts.txt

COMMON_OPTS = --std=08 --workdir=$(WORK_DIR)
IMPORT_OPTS = $(COMMON_OPTS)
MAKE_OPTS = $(COMMON_OPTS) -g
RUN_OPTS = --wave=$(WORK_DIR)/$<.ghw

targets = sdram_init_ctrl ccd_ctrl color_kernel i2c_ccd_config i2c_ctrl
testbenches = $(foreach target, $(targets), $(target)_tb)
runnables = $(foreach target, $(targets), $(target)_test)

work-obj08.cf: $(SRC_DIR)/*.vhd $(SRC_DIR)/$(PKG_DIR_SUFFIX)/*.vhd $(TB_DIR)/*.vhd $(TB_DIR)/$(PKG_DIR_SUFFIX)/*.vhd $(TB_DIR)/$(MODEL_DIR_SUFFIX)/*.vhd osvvm/
	ghdl -i $(IMPORT_OPTS)  src/*.vhd src/common_pkgs/*.vhd tb/*.vhd tb/common_pkgs/*.vhd tb/model/*.vhd

$(testbenches): %_tb: work-obj08.cf
	ghdl -m $(MAKE_OPTS) $@

.PHONY: $(runnables)
$(runnables): %_test: %_tb
	./$*_tb $(if $(wildcard $(TB_DIR)/$*_tb$(GHW_OPTS_SUFFIX)), --read-wave-opt=$(TB_DIR)/$*_tb$(GHW_OPTS_SUFFIX)) $(RUN_OPTS)

osvvm/:
	./setup_osvvm.fish

.PHONY: clean
clean: 
	-rm ./*.o
	-rm $(testbenches)
	-rm $(WORK_DIR)/*.o
	-rm $(WORK_DIR)/work-obj08.cf
	-rm $(WORK_DIR)/*.ghw

