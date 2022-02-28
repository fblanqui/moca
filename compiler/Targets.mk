######################################################################
#                                                                    #
#                           Moca                                     #
#                                                                    #
#          Pierre Weis, INRIA Rocquencourt                           #
#          Frédéric Blanqui, projet Protheo, INRIA Lorraine          #
#                                                                    #
#  Copyright 2005-2012,                                              #
#  Institut National de Recherche en Informatique et en Automatique. #
#  All rights reserved.                                              #
#                                                                    #
#  This file is distributed under the terms of the Q Public License. #
#                                                                    #
######################################################################

# $Id: Targets.mk,v 1.3 2012-03-08 14:56:52 weis Exp $

# Definitions of macros and target set for Moca's Makefile
# Included in doc_src's Makefile too. Developer beware

# The application that have to be compiled.
APPLI=mocac
BYTAPPLI=$(APPLI:=.byt)
BINAPPLI=$(BYTAPPLI:.byt=.bin)
#BYTLIB=$(BYTAPPLI:.byt=.cma)
#BINLIB=$(BYTLIB:.cma=.cmxa)

# The sub directories of this one in linking order.
DIR_UTILS=ocaml_src/utils
DIR_PARSING=ocaml_src/parsing
DIR_LIB=lib
DIR_CHECKING=checking
DIR_DRIVER=driver
DIR_COMPLETION=completion
DIR_GENERATION=generation
DIR_TESTING=testing

ALL_SUB_DIRECTORIES=\
 $(DIR_UTILS) $(DIR_PARSING) $(DIR_LIB) $(DIR_CHECKING)\
 $(DIR_COMPLETION) $(DIR_GENERATION) $(DIR_TESTING) $(DIR_DRIVER)

# The includes directive for the caml compiler
CAMLINCLUDES=\
 -I $(DIR_UTILS) -I $(DIR_PARSING) -I $(DIR_LIB) -I $(DIR_CHECKING)\
 -I $(DIR_COMPLETION) -I $(DIR_GENERATION) -I $(DIR_TESTING) -I $(DIR_DRIVER)

# All the moca directories listed in link order,
# with their source files listed in link order as well.
UTILS=\
 $(DIR_UTILS)/misc\
 $(DIR_UTILS)/tbl\
 $(DIR_UTILS)/config $(DIR_UTILS)/clflags\
 $(DIR_UTILS)/terminfo\
 $(DIR_UTILS)/ccomp $(DIR_UTILS)/warnings\
 $(DIR_UTILS)/consistbl

PARSINGIO=\
 $(DIR_PARSING)/parsetree $(DIR_PARSING)/asttypes

PARSING=\
 $(DIR_PARSING)/linenum $(DIR_PARSING)/location\
 $(DIR_PARSING)/longident\
 $(DIR_PARSING)/syntaxerr $(DIR_PARSING)/parser\
 $(DIR_PARSING)/lexer $(DIR_PARSING)/parse\
 $(DIR_PARSING)/printast

LIB=\
 $(DIR_LIB)/configuration $(DIR_LIB)/file\
 $(DIR_LIB)/useful $(DIR_LIB)/debug $(DIR_LIB)/mylist $(DIR_LIB)/myset\
 $(DIR_LIB)/relation $(DIR_LIB)/otype\
 $(DIR_LIB)/pr_ocaml $(DIR_LIB)/code $(DIR_LIB)/pr_code

CHECKING=\
 $(DIR_CHECKING)/check

DRIVER=\
 $(DIR_DRIVER)/main

COMPLETION=\
 $(DIR_COMPLETION)/var\
 $(DIR_COMPLETION)/symb $(DIR_COMPLETION)/term\
 $(DIR_COMPLETION)/term_utils $(DIR_COMPLETION)/subterm\
 $(DIR_COMPLETION)/order $(DIR_COMPLETION)/prec\
 $(DIR_COMPLETION)/subs $(DIR_COMPLETION)/match\
 $(DIR_COMPLETION)/rename $(DIR_COMPLETION)/unif\
 $(DIR_COMPLETION)/rule $(DIR_COMPLETION)/equation\
 $(DIR_COMPLETION)/cparser $(DIR_COMPLETION)/clexer\
 $(DIR_COMPLETION)/cp $(DIR_COMPLETION)/norm $(DIR_COMPLETION)/norm_ml\
 $(DIR_COMPLETION)/comp $(DIR_COMPLETION)/axiom

GENERATION=\
 $(DIR_GENERATION)/genr_pattern $(DIR_GENERATION)/genr_base\
 $(DIR_GENERATION)/genr_memo\
 $(DIR_GENERATION)/genr_sharing $(DIR_GENERATION)/genr_expression\
 $(DIR_GENERATION)/eqnrel $(DIR_GENERATION)/complete\
 $(DIR_GENERATION)/genr_nary $(DIR_GENERATION)/genr_listary\
 $(DIR_GENERATION)/genr_binary $(DIR_GENERATION)/genr_unary\
 $(DIR_GENERATION)/genr_nullary $(DIR_GENERATION)/genr $(DIR_GENERATION)/genr_mli

TESTING=\
 $(DIR_TESTING)/listutils\
 $(DIR_TESTING)/randomutils\
 $(DIR_TESTING)/tgenr_base $(DIR_TESTING)/genr_values\
 $(DIR_TESTING)/typed_vars $(DIR_TESTING)/genr_listary_equations\
 $(DIR_TESTING)/genr_substitutions $(DIR_TESTING)/genr_equalities\
 $(DIR_TESTING)/genr_testing

# Caml .mli files with no corresponding .ml file.
CAMLIOSOURCES=\
 $(PARSINGIO:=.mli)

# Caml .ml files with no corresponding .mli file.
CAMLNISOURCES=

# All Caml .ml files with a corresponding .mli file.
CAMLSOURCES=\
 $(UTILS:=.ml) $(PARSING:=.ml) $(LIB:=.ml)\
 $(CHECKING:=.ml) $(COMPLETION:=.ml)\
 $(GENERATION:=.ml) $(TESTING:=.ml) $(DRIVER:=.ml)

# All the Caml .mli interface files.
CAMLINTFILES=\
 $(CAMLIOSOURCES) $(CAMLSOURCES:.ml=.mli)

# All the Caml .ml IMPlementation files.
CAMLIMPFILES=\
 $(CAMLSOURCES) $(CAMLNISOURCES)

CAMLFILES=\
 $(CAMLINTFILES) $(CAMLIMPFILES)

# The list of Caml compiled object files.
BYTOBJS=$(CAMLIMPFILES:.ml=.cmo)

BINOBJS=$(BYTOBJS:.cmo=.cmx)

#LIBRARYOBJS=$(BYTOBJS)
