######################################################################
#                                                                    #
#                           Moca                                     #
#                                                                    #
#          Pierre Weis, INRIA Rocquencourt                           #
#          Frédéric Blanqui, projet Protheo, INRIA Lorraine          #
#                                                                    #
#  Copyright 2005-2008,                                              #
#  Institut National de Recherche en Informatique et en Automatique. #
#  All rights reserved.                                              #
#                                                                    #
#  This file is distributed under the terms of the Q Public License. #
#                                                                    #
######################################################################

# $Id: Makefile,v 1.10 2008-02-12 23:34:35 weis Exp $

SHELL := /bin/sh

MAKEFLAGS := -r

.SUFFIXES:
.SUFFIXES: .cmo .cmi .cmx .cma .cmxa .ml .mli .mly .mll

.PHONY: default clean very-clean depend tags lib

#WARNINGS := -w A -warn-error A
OCAMLC := ocamlc -I ../ocaml_src/parsing $(WARNINGS) -g
OCAMLOPT := ocamlopt -I ../ocaml_src/parsing $(WARNINGS) -unsafe -noassert -inline 10000
OCAMLLEX := ocamllex
OCAMLYACC := ocamlyacc
OCAMLDEP := ocamldep

# the order is important !
FILES := useful debug mylist myset var symb term subterm order \
  prec subs match rename unif rule equation cparser clexer cp norm comp axiom

MLFILES := $(FILES:%=%.ml)
CMOFILES := $(FILES:%=%.cmo)
CMXFILES := $(FILES:%=%.cmx)
MLIFILES := $(FILES:%=%.mli)
CMIFILES := $(FILES:%=%.cmi)

.PRECIOUS: cparser.ml cparser.mli clexer.ml

default: complete.byt complete.opt

complete.byt: rlib.cma main_complete.cmo
	$(OCAMLC) -o $@ $^

complete.opt: rlib.cmxa main_complete.cmx
	$(OCAMLOPT) -o $@ $^

rlib.cma: $(CMOFILES)
	$(OCAMLC) -a -o $@ $(CMOFILES)

rlib.cmxa: $(CMXFILES)
	$(OCAMLOPT) -a -o $@ $(CMXFILES)

lib: rlib.cma rlib.cmxa

cparser.cmo: cparser.cmi
clexer.cmo: cparser.cmi clexer.cmi

.mli.cmi:
	$(OCAMLC) -c $<

.ml.cmo:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<

.mly.mli:
	$(OCAMLYACC) $<

.mly.ml:
	$(OCAMLYACC) $<

.mll.ml:
	$(OCAMLLEX) $<

tags:
	otags -sr '.mli' -r .

clean:
	rm -f *.cm[io] clexer.ml cparser.ml cparser.mli *~ cparser.output
	$(MAKE) depend

very-clean: clean
	rm -f complete.byt complete.bin $(RLIB) TAGS .depend

depend:
	$(OCAMLDEP) $(MLIFILES) $(MLFILES) > .depend

configure:
	$(MAKE) depend

# dependancies
include .depend
