CC = g++
TARGETS = abfextract abfinfo chcalc chint dumpdata dwtpHyst opncurve statstates
OBJECTS = abfextract.o abfinfo.o chcalc.o chint.o dumpdata.o dwtpHyst.o noise-fft.o opncurve.o statstates.o
CFLAGS = -Wall -O2
LDLIBS = -lm -lgsl -lgslcblas ../lib/libABFaccess.a
INCLUDES = -I../lib

.SUFFIXES: .cpp .o

.cpp.o:
	$(CC) $(CFLAGS) $(INCLUDES) -c $<

all: $(TARGETS)

abfinfo: abfinfo.o
chcalc: chcalc.o
chint: chint.o
dumpdata: dumpdata.o 
dwtpHyst: dwtpHyst.o 
opncurve: opncurve.o
statstates: statstates.o
abfextract: abfextract.o

abfinfo.o: abfinfo.cpp ../lib/abfheader.h ../lib/compdefs.h ../lib/abffile.h
chcalc.o: chcalc.cpp ../lib/abfheader.h ../lib/compdefs.h ../lib/abffile.h
chint.o: chint.cpp ../lib/abfheader.h ../lib/compdefs.h ../lib/abffile.h
dumpdata.o: dumpdata.cpp ../lib/abfheader.h ../lib/compdefs.h ../lib/abffile.h
dwtpHyst.o: dwtpHyst.cpp
opncurve.o: opncurve.cpp ../lib/abfheader.h ../lib/compdefs.h ../lib/abffile.h
statstates.o: statstates.cpp ../lib/abfheader.h ../lib/compdefs.h ../lib/abffile.h
abfextract.o: abfextract.cpp ../lib/abfheader.h ../lib/compdefs.h ../lib/abffile.h

clean:
	rm -f $(TARGETS) $(OBJECTS)
