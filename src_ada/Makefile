BINs=alignfrags_vertically concat_atfs dumpatf2plot fit_transitions realignfrags list_starts

all:
	for fn in ${BINs}; do \
		gnatmake -O2 -Ilib -I../../ada/lib -I../../ada/lib/swing -I../../ada/lib/astro $${fn}; \
	done

clean:
	rm -f *.ali *.o ${BINs}

list:
	for fn in ${BINs}; do \
		echo $${fn}; \
	done

