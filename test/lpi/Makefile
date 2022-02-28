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

# $Id: Makefile,v 1.9 2008-02-11 16:40:34 weis Exp $

# The directory where Makfile templates reside.
MAKEFILES_DIR=../../Mk

# Include the generic setup for Caml applications.
include $(MAKEFILES_DIR)/Config.mk

SUB_DIRS = test

# Redefine the Caml compilers settings
CAMLBYTCONFIG = -w Ae -warn-error Ae -g $(CAMLBYTINCLUDES)
CAMLBINCONFIG = -w Ae -warn-error Ae $(CAMLBININCLUDES) \
  -unsafe -noassert -inline 10000

MOCAC=../../mocac

CPCONFIG=-p

# The application that have to be compiled.
APPLI=lpi

MOCAFILES=semi_algebra.mlm

CAMLIMPFILES=semi_algebra.ml tableau.ml

CAMLINTFILES= formula.mli \
  $(CAMLIMPFILES:.ml=.mli)

MLGENERATEDNOSHARING=$(MOCAFILES:.mlm=.ml) $(MOCAFILES:.mlm=.mli)

ALLGENERATED=$(MLGENERATEDSHARING) $(MLGENERATEDNOSHARING)

CAMLFILES=$(CAMLINTFILES) $(CAMLIMPFILES)

BYTOBJS=$(CAMLIMPFILES:.ml=.cmo)

BINOBJS=$(BYTOBJS:.cmo=.cmx)

all: byt bin
	for i in $(SUB_DIRS); do \
	  (cd $$i; make all); \
	done

$(ALLGENERATED): $(MOCAC)

$(MOCAC):
	cd ..; $(MAKE) all

byt: lpi.byt

bin: lpi.bin

lpi.byt: $(BYTOBJS)
	$(CAMLBYT) $(LINKFLAGS) -a -o lpi.cma $(BYTOBJS)

lpi.bin: $(BINOBJS)
	$(CAMLBIN) $(LINKFLAGS) -a -o lpi.cmxa $(BINOBJS)

clean::
	$(RM) $(CAMLINTFILES:.mli=.cmi)
	$(RM) $(MLGENERATEDNOSHARING)
	$(RM) $(BYTOBJS)
	$(RM) $(BINOBJS:.cmx=.o) $(BINOBJS)
	for i in $(SUB_DIRS); do \
	  (cd $$i; $(MAKE) clean); \
	done

depend:: beforedepend

beforedepend:
	if test -x "$(MOCAC)" ; then \
	  $(MAKE) $(MLGENERATEDNOSHARING); \
	fi

configure::
	for i in $(SUB_DIRS); do \
	  echo "Making configure in sub directory $$i..."; \
	  (cd $$i; $(TOUCH) ./.depend; make configure); \
	  echo "Make configure in sub directory $$i done."; \
	done

include $(MAKEFILES_DIR)/Caml.mk
include $(MAKEFILES_DIR)/../config/Makefile.config
