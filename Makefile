# where are sources located
SRC_DIR = src
TB_DIR = tb
LIB_DIR = $(SRC_DIR)/common_pkgs

# list all sources
LIBS = $(wildcard $(LIB_DIR)/*.vhd)
FILES = $(wildcard $(SRC_DIR)/*.vhd)
FILES += $(wildcard $(TB_DIR)/*.vhd)
FILES += $(LIBS)

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
$(GHDL_LIB_INFO) : $(FILES) $(WORKDIR)
	$(GHDL_CMD) -i $(MAKE_OPTS) --workdir=$(WORKDIR) $(SRC_DIR)/*.vhd $(TB_DIR)/*.vhd $(LIB_DIR)/*.vhd

$(WORKDIR) :
	mkdir $(WORKDIR)

.PHONY : ccd_ctrl
ccd_ctrl : $(GHDL_LIB_INFO) $(LIBS) $(TB_DIR)/ccd_ctrl_tb.vhd $(SRC_DIR)/ccd_ctrl.vhd
	$(GHDL_CMD) -m $(ELABORATE_OPTS) --workdir=$(WORKDIR) ccd_ctrl_tb
	$(GHDL_CMD) -r $(RUN_OPTS) --workdir=$(WORKDIR) ccd_ctrl_tb $(WAVE_OPTS)

.PHONY : color_kernel
color_kernel : $(GHDL_LIB_INFO) $(LIBS) $(TB_DIR)/color_kernel_tb.vhd $(SRC_DIR)/color_kernel.vhd $(SRC_DIR)/pixel_shiftreg.vhd
	$(GHDL_CMD) -m $(ELABORATE_OPTS) --workdir=$(WORKDIR) color_kernel_tb
	$(GHDL_CMD) -r $(RUN_OPTS) --workdir=$(WORKDIR) color_kernel_tb $(WAVE_OPTS)

.PHONY : show-wave
show-wave : $(WORKDIR)/$(WAVE_OUT_NAME)
	$(VIEW_CMD) $(WORKDIR)/$(WAVE_OUT_NAME)

.PHONY : clean
clean :
	$(GHDL_CMD) --clean --workdir=$(WORKDIR)
	rm $(GHDL_LIB_INFO) $(WORKDIR)/$(WAVE_OUT_NAME)
