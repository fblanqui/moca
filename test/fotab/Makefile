include ./Makefile.common

MOCAFILES=fol
UTILMLFILES=gensymb configuration
MLFILES=pprint foltab main
EXECFILE=fol
MOCAGENFILES=$(MOCAFILES:%=%.ml) $(MOCAFILES:%=%.mli) 
FILES= ${UTILMLFILES} $(MOCAFILES) $(MLFILES)
CMXFILES=$(FILES:%=%.cmx)
CMOFILES=$(FILES:%=%.cmo)

all: depend  ${EXECFILE} ${EXECFILE}.byt

include .depend

${EXECFILE} : $(CMXFILES)
	$(CAMLBIN) -o $@ $(OPTLIBDEPS) $(CMXFILES)

${EXECFILE}.byt: $(CMOFILES) 
	$(CAMLBYT) -o $@ $(LIBDEPS) $(CMOFILES)


beforedepend: $(MOCAGENFILES) 

clean:
	-rm -f *.cmx *.cmo *.cmi *.o; 
	for i in $(MOCAFILES); do\
	   rm -f $$i.ml $$i.mli; \
	done;
	-rm -f \#* *~; 
	-rm -f ${EXECFILE} ${EXECFILE}.byt