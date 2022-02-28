######################################################################
#                                                                    #
#                           Moca                                     #
#                                                                    #
#          Pierre Weis, INRIA Rocquencourt                           #
#          Frédéric Blanqui, projet Protheo, INRIA Lorraine          #
#                                                                    #
#  Copyright 2005-2007,                                              #
#  Institut National de Recherche en Informatique et en Automatique. #
#  All rights reserved.                                              #
#                                                                    #
#  This file is distributed under the terms of the Q Public License. #
#                                                                    #
######################################################################

# $Id: Makefile,v 1.13 2008-02-11 19:37:34 weis Exp $

# The Makefile for numbers tests.

include ../../Mk/Config.mk

MOCAC = ../../mocac

FILES = sign nat natge modint natdigit_by_hand \
  peano diadic cantor cantor_op \
  ../gentest tnatdigit tcantor

MLMFILES = natdigit intdigit diadic cantor

MLGENERATEDNOSHARING = $(MLMFILES:=.mli) $(MLMFILES:=.ml)

GENERATED_FILES = $(MLGENERATEDNOSHARING) runtest.byt

CAMLINTFILES = $(FILES:=.mli)

CAMLIMPFILES = $(FILES:=.ml)

CAMLFILES = $(CAMLINTFILES) $(CAMLIMPFILES)

BYTOBJS = $(CAMLIMPFILES:.ml=.cmo)

CAMLINCLUDES = -I ..
CAMLBYTCONFIG = -w Ae -warn-error Ae $(CAMLINCLUDES)

all: byt run

byt : $(BYTOBJS)

run: runbyt

runbyt: runtest.byt
	./runtest.byt

runtest.byt: $(BYTOBJS)
	$(CAMLBYT) $(LINKFLAGS) -o $@ $(BYTOBJS)

../gentest.cmo:
	cd ..; $(MAKE) gentest.cmo

../gentest.cmi:
	cd ..; $(MAKE) gentest.cmi

$(GENERATED_FILES): $(MOCAC)

beforedepend::
	if test -x "$(MOCAC)" ; then \
	  $(MAKE) $(MLGENERATEDNOSHARING); \
	fi

depend:: beforedepend

clean::
	$(RM) $(GENERATED_FILES)

include ../../Mk/Caml.mk
