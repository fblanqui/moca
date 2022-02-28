include ../Makefile.common

MOCAFILES=boolean
MLFILES=tableau test
MOCAGENFILES=$(MOCAFILES:%=%.ml) $(MOCAFILES:%=%.mli) 
FILES= $(MOCAFILES) $(MLFILES)
CMXFILES=$(FILES:%=%.cmx)
CMOFILES=$(FILES:%=%.cmo)

all: depend all_test.byt all_test

include .depend

all_test : $(CMXFILES)
	$(CAMLBIN) -o $@ $(OPTLIBDEPS) $(CMXFILES)

all_test.byt: $(CMOFILES) 
	$(CAMLBYT) -o $@ $(LIBDEPS) $(CMOFILES)


beforedepend: $(MOCAGENFILES) 

clean:
	-rm -f *.cmx *.cmo *.cmi *.o; 
	for i in $(MOCAFILES); do\
	   rm -f $$i.ml $$i.mli; \
	done;
	-rm -f \#* *~; 
	-rm -f all_test all_test.byt;

