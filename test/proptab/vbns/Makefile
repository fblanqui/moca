include ../Makefile.common

MOCAFILES=boolean_list
MLFILES=tableau_list test_list
FILES= $(MOCAFILES) $(MLFILES)

CMXFILES=$(FILES:%=%.cmx)
CMOFILES=$(FILES:%=%.cmo)

.PHONY: .depend

all: .depend list_test list_test.byt


list_test: $(CMXFILES)
	$(CAMLBIN) -o $@ $(OPTLIBDEPS) $(CMXFILES)

list_test.byt: $(CMOFILES)
	$(CAMLBYT) -o $@ $(LIBDEPS) $(CMOFILES)

beforedepend: boolean_list.ml boolean_list.mli 

clean:
	-rm -f *.cmx *.cmo *.cmi *.o 
	for i in $(MOCAFILES); do\
	   rm -f $$i.ml $$i.mli;\
	done
	-rm -f \#* *~
	-rm -f list_test list_test.byt

include .depend