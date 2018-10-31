TARGET = run_elphys_utils
SOURCES = src/*.ad?

# rule to link the program
elphys_utils: $(SOURCES)
	gprbuild -P elphys_utils.gpr
