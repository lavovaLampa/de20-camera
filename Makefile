# where are sources located
SRC_DIR = src
TB_DIR = tb
LIB_DIR = $(SRC_DIR)/common_pkgs
TB_LIB_DIR = $(TB_DIR)/common

# list all sources
LIBS = $(wildcard $(LIB_DIR)/*.vhd)
LIBS += $(wildcard $(TB_LIB_DIR)/*.vhd)
FILES = $(wildcard $(SRC_DIR)/*.vhd)
FILES += $(wildcard $(TB_DIR)/*.vhd)
FILES += $(LIBS)

# simulator (GHDL) variables
GHDL_CMD = ghdl
OUT_NAME = wave_out
VCD_OUT_NAME = $(OUT_NAME).vcd
GHW_OUT_NAME = $(OUT_NAME).ghw
WORKDIR = work
MAKE_OPTS = --std=08
ANALYSIS_OPTS = --std=08 
ELABORATE_OPTS = --std=08
RUN_OPTS = --std=08 -Wunused -Wothers -Wstatic 
VCD_OPTS = --vcd=$(WORKDIR)/$(VCD_OUT_NAME)
GHW_OPTS = --wave=$(WORKDIR)/$(GHW_OUT_NAME)

# GHDL file/entity mapping file
GHDL_LIB_INFO = $(WORKDIR)/work-obj08.cf

# waveform viewer
VIEW_CMD = gtkwave

# import sources
$(GHDL_LIB_INFO) : $(FILES) $(WORKDIR) $(LIBS)
	$(GHDL_CMD) -i $(MAKE_OPTS) --workdir=$(WORKDIR) $(SRC_DIR)/*.vhd $(TB_DIR)/*.vhd $(LIB_DIR)/*.vhd $(TB_LIB_DIR)/*.vhd

$(WORKDIR) :
	mkdir $(WORKDIR)

.PHONY : ccd_ctrl
ccd_ctrl : $(GHDL_LIB_INFO) $(LIBS) $(TB_DIR)/ccd_ctrl_tb.vhd $(SRC_DIR)/ccd_ctrl.vhd
	$(GHDL_CMD) -m $(ELABORATE_OPTS) --workdir=$(WORKDIR) ccd_ctrl_tb
	$(GHDL_CMD) -r $(RUN_OPTS) --workdir=$(WORKDIR) ccd_ctrl_tb $(GHW_OPTS) --read-wave-opt=$(TB_DIR)/ccd_ctrl_tb_ghw_opts.txt

.PHONY : color_kernel
color_kernel : $(GHDL_LIB_INFO) $(LIBS) $(TB_DIR)/color_kernel_tb.vhd $(SRC_DIR)/color_kernel.vhd $(SRC_DIR)/pixel_shiftreg.vhd
	$(GHDL_CMD) -m $(ELABORATE_OPTS) --workdir=$(WORKDIR) color_kernel_tb
	$(GHDL_CMD) -r $(RUN_OPTS) --workdir=$(WORKDIR) color_kernel_tb $(VCD_OPTS)

.PHONY : i2c_ctrl
i2c_ctrl : $(GHDL_LIB_INFO) $(LIBS) $(TB_DIR)/i2c_ctrl_tb.vhd $(SRC_DIR)/i2c_ctrl.vhd
	$(GHDL_CMD) -m $(ELABORATE_OPTS) --workdir=$(WORKDIR) i2c_ctrl_tb
	$(GHDL_CMD) -r $(RUN_OPTS) --workdir=$(WORKDIR) i2c_ctrl_tb $(GHW_OPTS)

.PHONY : i2c_ccd_config
i2c_ccd_config : $(GHDL_LIB_INFO) $(LIBS) $(TB_DIR)/i2c_ccd_config_tb.vhd $(SRC_DIR)/i2c_ccd_config.vhd
	$(GHDL_CMD) -m $(ELABORATE_OPTS) --workdir=$(WORKDIR) i2c_ccd_config_tb
	$(GHDL_CMD) -r $(RUN_OPTS) --workdir=$(WORKDIR) i2c_ccd_config_tb $(GHW_OPTS)

.PHONY : sdram_ctrl
sdram_ctrl : $(GHDL_LIB_INFO) $(LIBS) $(TB_DIR)/sdram_ctrl_tb.vhd $(SRC_DIR)/sdram_ctrl.vhd $(SRC_DIR)/sdram_init.vhd
	$(GHDL_CMD) -m $(ELABORATE_OPTS) --workdir=$(WORKDIR) sdram_ctrl_tb
	$(GHDL_CMD) -r $(RUN_OPTS) --workdir=$(WORKDIR) sdram_ctrl_tb $(GHW_OPTS)

.PHONY : show-wave
show-wave : $(WORKDIR)/$(VCD_OUT_NAME)
	$(VIEW_CMD) $(WORKDIR)/$(VCD_OUT_NAME)

.PHONY : clean
clean :
	$(GHDL_CMD) --clean --workdir=$(WORKDIR)
	rm $(GHDL_LIB_INFO) $(WORKDIR)/wave_out.* $(WORKDIR)/*.o ./*.o
