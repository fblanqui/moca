#(***********************************************************************)
#(*                                                                     *)
#(*                           LPI                                       *)
#(*                                                                     *)
#(*                 Pierre Weis, INRIA Rocquencourt                     *)
#(*                 Areski Nait-Abdallah, INRIA Rocquencourt           *)
#(*                                                                     *)
#(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
#(*  en Automatique.  Distributed only by permission.                   *)
#(*                                                                     *)
#(***********************************************************************)

# $Id: Makefile,v 1.1 2012-03-07 09:57:13 weis Exp $

# The directory where Makfile templates reside.
MAKEFILES_DIR=../../Mk

# Include the generic setup for Caml applications.
include $(MAKEFILES_DIR)/Config.mk

# Redefine the Caml compilers settings
CAMLBYTCONFIG = -w Ae -warn-error Ae -g $(CAMLBYTINCLUDES)
CAMLBINCONFIG = -w Ae -warn-error Ae $(CAMLBININCLUDES)\
  -unsafe -noassert -inline 10000

MOCAC=../../mocac -test

CPCONFIG=-p

# The application that have to be compiled.
APPLI=

MOCAFILES=formal_sum.mlm formal_sum_rev.mlm

CAMLIMPFILES=formal_sum_test.ml formal_sum_rev_test.ml

CAMLINTFILES=\
  $(CAMLIMPFILES:.ml=.mli)

MLGENERATEDNOSHARING=$(MOCAFILES:.mlm=.ml) $(MOCAFILES:.mlm=.mli)
MLGENERATEDSHARING=

ALLGENERATED=$(MLGENERATEDSHARING) $(MLGENERATEDNOSHARING)

CAMLFILES=$(CAMLINTFILES) $(CAMLIMPFILES)

BYTOBJS=$(CAMLIMPFILES:.ml=.cmo)

BINOBJS=$(BYTOBJS:.cmo=.cmx)

all: byt bin

$(ALLGENERATED): $(MOCAC)

$(MOCAC):
	cd ..; $(MAKE) all

byt: $(BYTOBJS)

bin: $(BINOBJS)

clean::
	$(RM) $(CAMLINTFILES:.mli=.cmi)
	$(RM) $(MLGENERATEDNOSHARING)
	$(RM) $(BYTOBJS)
	$(RM) $(BINOBJS:.cmx=.o) $(BINOBJS)

depend:: beforedepend

beforedepend:
	if test -x "$(MOCAC)" ; then \
	  $(MAKE) $(MLGENERATEDNOSHARING); \
	fi

include $(MAKEFILES_DIR)/Caml.mk
include $(MAKEFILES_DIR)/../config/Makefile.config
