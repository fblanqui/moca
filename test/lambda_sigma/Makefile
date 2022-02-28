include ./Makefile.common

.PHONY: lambdasigma

MOCAFILES=lambda_sigma
MLFILES=lambda_sigma_utils lambda_sigma_beta ls_test
FILES= $(MOCAFILES) $(MLFILES)

GENFILES=$(MOCAFILES:%=%.mli) $(MOCAFILES:%=%.ml)

CMXFILES=$(FILES:%=%.cmx)
CMOFILES=$(FILES:%=%.cmo)

all: depend lstest.byt lstest

include .depend

lstest : $(CMXFILES)
	$(CAMLBIN) -o $@  $(CMXFILES)

lstest.byt: $(CMOFILES) 
	$(CAMLBYT) -o $@  $(CMOFILES)

beforedepend: $(GENFILES)

clean:
	-rm -f *.cmx *.cmo *.cmi *.o; 
	for i in $(MOCAFILES); do\
	   rm -f $$i.ml $$i.mli; \
	done;
	-rm -f \#* *~; 
	-rm -f test test.byt;


