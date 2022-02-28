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

# $Id: Makefile,v 1.2 2007-09-24 16:18:28 weis Exp $

# The directory where Makfile templates reside.
MAKEFILES_DIR=../../../Mk

# Include the generic setup for Caml applications.
include $(MAKEFILES_DIR)/Config.mk

CPCONFIG=-p

# The application that have to be compiled.
APPLI=lpi

#CAMLIMPFILES=model_scheme.ml tableau.ml
CAMLIMPFILES=semi_algebra.ml tableau.ml

CAMLINTFILES=\
  formula.mli \
  $(CAMLIMPFILES:.ml=.mli)

CAMLFILES=$(CAMLINTFILES) $(CAMLIMPFILES)

BYTOBJS=$(CAMLIMPFILES:.ml=.cmo)

BINOBJS=$(BYTOBJS:.cmo=.cmx)

all: byt bin

byt: lpi.byt

bin: lpi.bin

lpi.byt: $(BYTOBJS)
	$(CAMLBYT) $(LINKFLAGS) -a -o lpi.cma $(BYTOBJS)

lpi.bin: $(BINOBJS)
	$(CAMLBIN) $(LINKFLAGS) -a -o lpi.cmxa $(BINOBJS)

clean::
	$(RM) $(CAMLINTFILES:.mli=.cmi)
	$(RM) $(BYTOBJS)
	$(RM) $(BINOBJS:.cmx=.o) $(BINOBJS)

configure:: clean

include $(MAKEFILES_DIR)/Caml.mk
include $(MAKEFILES_DIR)/../config/Makefile.config
