# where are sources located
SRC_DIR = src
TB_DIR = tb
LIB_DIR = $(SRC_DIR)/common_pkgs

# list all sources
FILES = $(wildcard $(SRC_DIR)/*.vhd)
FILES += $(wildcard $(TB_DIR)/*.vhd)
FILES += $(wildcard $(LIB_DIR)/*.vhd)

# simulator (GHDL) variables
GHDL_CMD = ghdl
WAVE_OUT_NAME = wave_out.ghw
WORKDIR = work
MAKE_OPTS = --std=08
ANALYSIS_OPTS = --std=08 
ELABORATE_OPTS = --std=08
RUN_OPTS = --std=08 -Wunused -Wothers -Wstatic 
WAVE_OPTS = --wave=$(WORKDIR)/$(WAVE_OUT_NAME)

# GHDL file/entity mapping file
GHDL_LIB_INFO = $(WORKDIR)/work-obj08.cf

# waveform viewer
VIEW_CMD = gtkwave

# import sources
$(GHDL_LIB_INFO) : $(FILES)
	$(GHDL_CMD) -i $(MAKE_OPTS) --workdir=$(WORKDIR) $(SRC_DIR)/*.vhd $(TB_DIR)/*.vhd $(LIB_DIR)/*.vhd
	#$(GHDL_CMD) -a $(ANALYSIS_OPTS) --workdir=$(WORKDIR) $(LIB_DIR)/*.vhd $(SRC_DIR)/*.vhd $(TB_DIR)/*.vhd

.PHONY : ccd_ctrl
ccd_ctrl : $(GHDL_LIB_INFO) $(TB_DIR)/ccd_ctrl_tb.vhd $(SRC_DIR)/ccd_ctrl.vhd $(LIB_DIR)/ccd_pkg.vhd
	$(GHDL_CMD) -m $(ELABORATE_OPTS) --workdir=$(WORKDIR) ccd_ctrl_tb
	$(GHDL_CMD) -r $(RUN_OPTS) --workdir=$(WORKDIR) ccd_ctrl_tb $(WAVE_OPTS)

.PHONY : show-wave
show-wave : $(WORKDIR)/$(WAVE_OUT_NAME)
	$(VIEW_CMD) $(WORKDIR)/$(WAVE_OUT_NAME)

.PHONY : clean
clean :
	$(GHDL_CMD) --clean --workdir=$(WORKDIR)
	rm $(GHDL_LIB_INFO) $(WORKDIR)/$(WAVE_OUT_NAME)
