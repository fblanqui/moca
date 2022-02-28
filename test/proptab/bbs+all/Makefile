include ../Makefile.common

MOCAFILES=boolean_sharing
MLFILES=tableau_sharing test_sharing
FILES= $(MOCAFILES) $(MLFILES)

CMXFILES=$(FILES:%=%.cmx)
CMOFILES=$(FILES:%=%.cmo)

all: depend sharing_test sharing_test.byt

sharing_test: $(CMXFILES)
	$(CAMLBIN) -o $@ $(OPTLIBDEPS) $(CMXFILES)

sharing_test.byt: $(CMOFILES)
	$(CAMLBYT) -o $@ $(LIBDEPS) $(CMOFILES)

beforedepend: boolean_sharing.ml boolean_sharing.mli

clean:
	-rm -f *.cmx *.cmo *.cmi *.o
	for i in $(MOCAFILES); do \
	   rm -f $$i.ml $$i.mli; \
	done
	-rm -f \#* *~
	-rm -f sharing_test sharing_test.byt

include .depend
