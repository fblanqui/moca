# $Id: Makefile,v 1.1 2010-08-11 21:27:22 weis Exp $

include ./Makefile.common

LIBFILES=atom

GENFILES=bdd

MLFILES=queens main

EXECFILE=queens

MOCAGENFILES=$(GENFILES:%=%.ml) $(GENFILES:%=%.mli)
FILES=$(LIBFILES) $(GENFILES) $(MLFILES)
CMXFILES=$(FILES:%=%.cmx)
CMOFILES=$(FILES:%=%.cmo)

all: depend ${EXECFILE} ${EXECFILE}.byt

include .depend

${EXECFILE} : $(CMXFILES)
	$(CAMLBIN) -o $@ $(OPTLIBDEPS) $(CMXFILES)

${EXECFILE}.byt: $(CMOFILES)
	$(CAMLBYT) -o $@ $(LIBDEPS) $(CMOFILES)

beforedepend: $(MOCAGENFILES)

clean:
	rm -f *.cmx *.cmo *.cmi *.o \#* *~;
	rm -f $(MOCAGENFILES)
	rm -f ${EXECFILE} ${EXECFILE}.byt
