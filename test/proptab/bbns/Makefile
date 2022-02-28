include ../Makefile.common

MOCAFILES=boolean
MOCAGENFILES=$(MOCAFILES:%=%.ml) $(MOCAFILES:%=%.mli) 
MLFILES=tableau test
FILES= $(MOCAFILES) $(MLFILES)

CMXFILES=$(FILES:%=%.cmx)
CMOFILES=$(FILES:%=%.cmo)

all: depend inv_test.byt inv_test

include .depend

inv_test : $(CMXFILES)
	$(CAMLBIN) -g -o  $@ $(OPTLIBDEPS) $(CMXFILES)

inv_test.byt: $(CMOFILES)

	$(CAMLBYT) -g -o $@ $(LIBDEPS) $(CMOFILES)

beforedepend: $(MOCAGENFILES)

clean:
	-rm -f *.cmx *.cmo *.cmi *.o;
	for i in $(MOCAFILES); do \
	   rm -f $$i.ml $$i.mli; \
	done;
	-rm -f \#* *~
	-rm -f inv_test inv_test.byt
