#**********************************************************************#
#                                                                      #
#                            Moca                                      #
#                                                                      #
#           Pierre Weis, INRIA Rocquencourt                            #
#           Frédéric Blanqui, projet Protheo, INRIA Lorraine           #
#                                                                      #
#   Copyright 2005-2008,                                               #
#   Institut National de Recherche en Informatique et en Automatique.  #
#   All rights reserved.                                               #
#                                                                      #
#   This file is distributed under the terms of the Q Public License.  #
#                                                                      #
#**********************************************************************#

# $Id: Makefile,v 1.6 2011-05-25 08:20:24 bonichon Exp $

include ./Makefile.common

MOCACFLAGS=-sharing

MOCAFILES=bdd

LIBFILES=

GENFILES=bdd dt

MLFILES=pprint nqueens main

EXECFILE=nqueens

MOCAGENFILES=$(GENFILES:%=%.ml) $(GENFILES:%=%.mli)
FILES= $(LIBFILES) $(GENFILES) $(MLFILES)
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
	rm -f *.cmx *.cmo *.cmi *.o \#* *~;
	rm -f $(MOCAGENFILES)
	rm -f ${EXECFILE} ${EXECFILE}.byt
