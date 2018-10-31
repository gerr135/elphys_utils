TARGET = run_elphys_utils
SOURCES = src/*.ad?

# rule to link the program
all: $(SOURCES)
	gprbuild -P elphys_utils.gpr

clean:
	rm obj/*/*

.PHONEY: clean
