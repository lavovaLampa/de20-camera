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
GHDL_OPTS = --std=08 --warn-no-vital-generic
WORKDIR = work
OUTFILE_NAME = wave_out.ghw

# GHDL file/entity mapping file
GHDL_LIB_INFO = $(WORKDIR)/work-obj08.cf

# waveform viewer
VIEW_CMD = gtkwave

# import sources
$(GHDL_LIB_INFO) : $(FILES)
	$(GHDL_CMD) -i $(GHDL_OPTS) --workdir=$(WORKDIR) $(SRC_DIR)/*.vhd $(TB_DIR)/*.vhd $(LIB_DIR)/*.vhd

ccd_ctrl : $(GHDL_LIB_INFO) $(TB_DIR)/ccd_ctrl_tb.vhd $(SRC_DIR)/ccd_ctrl.vhd $(LIB_DIR)/ccd_pkg.vhd
	$(GHDL_CMD) -m $(GHDL_OPTS) --workdir=$(WORKDIR) ccd_ctrl_tb
	$(GHDL_CMD) -r $(GHDL_OPTS) --workdir=$(WORKDIR) ccd_ctrl_tb

.PHONY : clean
clean :
	$(GHDL_CMD) --clean --workdir=$(WORKDIR)
	rm $(GHDL_LIB_INFO) $(WORKDIR)/$(OUTFILE_NAME)
