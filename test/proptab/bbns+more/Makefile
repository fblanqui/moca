include ../Makefile.common

MOCAFILES=boolean
MLFILES=tableau test 
FILES= $(MOCAFILES) $(MLFILES)

CMXFILES=$(FILES:%=%.cmx)
CMOFILES=$(FILES:%=%.cmo)

all: depend more_test.byt more_test

include .depend

more_test : $(CMXFILES)
	$(CAMLBIN) -o $@ $(OPTLIBDEPS) $(CMXFILES)

more_test.byt: $(CMOFILES) 
	$(CAMLBYT) -o $@ $(LIBDEPS) $(CMOFILES)


beforedepend: boolean.ml boolean.mli

clean:
	-rm -f *.cmx *.cmo *.cmi *.o; 
	for i in $(MOCAFILES); do\
	   rm -f $$i.ml $$i.mli; \
	done;
	-rm -f \#* *~; 
	-rm -f more_test more_test.byt;

