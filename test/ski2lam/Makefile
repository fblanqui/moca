include ./Makefile.common


MOCAFILES=ski2lam lam2ski type skiv
MLFILES=gensymb utils skilam pprint main 

FILES= $(MOCAFILES) $(MLFILES)

MYFILES = utils gensymb skilam ski2lam lam2ski skiv type pprint main
MOCAGENFILES= $(MOCAFILES:%=%.ml) $(MOCAFILES:%=%.mli)
CMXFILES=$(MYFILES:%=%.cmx)
CMOFILES=$(MYFILES:%=%.cmo)
CMIFILES=$(MYFILES:%=%.cmi)
OFILES=$(MYFILES:%=%.o)
BIN=lam2ski lam2ski.byt

.PHONY : all clean

all : depend  $(BIN)

world: all

include .depend

lam2ski : $(CMXFILES)
	$(CAMLBIN) -o  $@ $(OPTLIBDEPS) $(CMXFILES)

lam2ski.byt: $(CMOFILES)

	$(CAMLBYT) -o $@ $(LIBDEPS) $(CMOFILES)

beforedepend: $(MOCAGENFILES)

clean:
	-rm -f \#*
	-rm -f $(CMOFILES) $(CMXFILES) $(MOCAGENFILES)
	-rm -r $(CMIFILES) $(OFILES) $(BIN)
	find . -name '*~' | xargs $(RM)

veryclean: clean
	-rm -f .depend
