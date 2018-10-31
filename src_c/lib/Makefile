CXX = g++
OBJECTS = abfheader.o abffile.o

all: $(OBJECTS)
	ar cru libABFaccess.a $(OBJECTS)
	ranlib libABFaccess.a

abfheader.o: abfheader.cpp abfheader.h
abffile.o: abffile.cpp abffile.h

clean:
	rm *.o *.a
