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

# $Id: Makefile,v 1.5 2008-02-11 16:48:01 weis Exp $

# The general toplevel Makefile for the mocac compiler

# The directory where Makfile templates reside.
MAKEFILES_DIR=../../../Mk

# Include the generic setup for Caml applications.
include $(MAKEFILES_DIR)/Config.mk

CPCONFIG=-p

# The application that have to be compiled.
APPLI=examples

CAMLLIBS=lpi.cma

CAMLIMPFILES=ex0.ml ex1.ml
CAMLINTFILES=

CAMLINCLUDES=-I ..

CAMLFILES=$(CAMLINTFILES) $(CAMLIMPFILES)

BYTOBJS=$(CAMLIMPFILES:.ml=.cmo)

BINOBJS=$(BYTOBJS:.cmo=.cmx)

CAMLLIBBYTS = $(CAMLLIBS:.ml=.cmo) $(CAMLLIBFILES:.ml=.cmo)
CAMLLIBBINS = $(CAMLLIBS:.cma=.cmxa)  $(CAMLLIBFILES:.ml=.cmx)

all: byt bin run

byt: examples.byt

bin: examples.bin

run: byt bin
	./examples.byt;
	./examples.bin;

clean::
	$(RM) ./examples.byt ./examples.bin

examples.byt: $(BYTOBJS)
	$(CAMLBYT) $(LINKFLAGS) -o examples.byt $(CAMLLIBBYTS) $(BYTOBJS)

examples.bin: $(BINOBJS)
	$(CAMLBIN) $(LINKFLAGS) -o examples.bin $(CAMLLIBBINS) $(BINOBJS)

clean::
	$(RM) $(BYTOBJS)
	$(RM) $(BINOBJS:.cmx=.o) $(BINOBJS)

configure:: clean depend

include $(MAKEFILES_DIR)/Caml.mk
include $(MAKEFILES_DIR)/../config/Makefile.config
