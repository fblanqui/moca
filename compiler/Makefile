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

# $Id: Makefile,v 1.100 2012-06-04 13:01:21 weis Exp $

# The general toplevel Makefile for the mocac compiler

# The directory where Makefile templates reside.
MAKEFILES_DIR = ../Mk

# Include the generic setup for Caml applications.
include $(MAKEFILES_DIR)/Config.mk
include $(MAKEFILES_DIR)/../config/Makefile.config

# Include the specific list of targets to build and set of files necessary
# for the compilation.
include ./Targets.mk

# The targets to make.
all:: depend byt bin

byt: $(BYTAPPLI)

clean::
	$(RM) $(BYTAPPLI) $(BYTLIB)

bin: $(BINAPPLI)

clean::
	$(RM) $(BINAPPLI) $(BINLIB)

$(BYTAPPLI): $(BYTOBJS)
	$(CAMLBYT) $(LINKFLAGS) -o $@ $(BYTOBJS)

#$(BYTLIB): $(LIBRARYOBJS)
#	$(CAMLBYTLIBR) $(LINKFLAGS) -o $@ $(LIBRARYOBJS)

$(BINAPPLI): $(BINOBJS)
	$(CAMLBIN) $(LINKFLAGS) -o $@ $(BINOBJS)

clean::
	$(RM) $(CAMLINTFILES:.mli=.cmi)
	$(RM) $(BYTOBJS)
	$(RM) $(BINOBJS:.cmx=.o) $(BINOBJS)

configure:: clean

# The list of files to remove when cleaning which are not regular object files.
partialclean::
# FIXME should configure config.ml using config.mlp
#	$(RM) $(DIR_UTILS)/config.ml
	$(RM) $(DIR_PARSING)/parser.mli $(DIR_PARSING)/parser.ml
	$(RM) $(DIR_PARSING)/parser.output
	$(RM) $(DIR_PARSING)/lexer.ml
	$(RM) $(DIR_PARSING)/linenum.ml
	$(RM) $(DIR_COMPLETION)/cparser.mli $(DIR_COMPLETION)/cparser.ml
	$(RM) $(DIR_COMPLETION)/cparser.output
	$(RM) $(DIR_COMPLETION)/clexer.ml

clean:: partialclean

clean::
	for d in $(ALL_SUB_DIRECTORIES); do\
		(cd $$d; $(RM) *.cm* *.annot *~ *.o .\#* a.out);\
	done
# The parsers.

$(DIR_PARSING)/parser.mli $(DIR_PARSING)/parser.ml: $(DIR_PARSING)/parser.mly
	$(CAMLYAC) $(DIR_PARSING)/parser.mly

$(DIR_COMPLETION)/cparser.mli $(DIR_COMPLETION)/cparser.ml: $(DIR_COMPLETION)/cparser.mly
	$(CAMLYAC) $(DIR_COMPLETION)/cparser.mly

# The lexers.

$(DIR_PARSING)/lexer.ml: $(DIR_PARSING)/lexer.mll
	$(CAMLLEX) $(DIR_PARSING)/lexer.mll

$(DIR_COMPLETION)/clexer.ml: $(DIR_COMPLETION)/clexer.mll
	$(CAMLLEX) $(DIR_COMPLETION)/clexer.mll

# The auxiliary lexer for counting line numbers.

$(DIR_PARSING)/linenum.ml: $(DIR_PARSING)/linenum.mll
	$(CAMLLEX) $(DIR_PARSING)/linenum.mll

# Fragile compilation.

FRAGILE=-w Ae -warn-error Ae

$(DIR_PARSING)/parser.cmo: $(DIR_PARSING)/parser.ml
	$(CAMLBYT) $(FRAGILE) -c $(DIR_PARSING)/parser.ml
$(DIR_PARSING)/parser.cmx: $(DIR_PARSING)/parser.ml
	$(CAMLBIN) $(FRAGILE) -c $(DIR_PARSING)/parser.ml

#$(DIR_GENERATOR)/genr_memo.cmo: $(DIR_GENERATOR)/genr_memo.ml
#	$(CAMLBYT) $(FRAGILE) -c $(DIR_GENERATOR)/genr_memo.ml
#$(DIR_GENERATOR)/genr_memo.cmx: $(DIR_GENERATOR)/genr_memo.ml
#	$(CAMLBIN) $(FRAGILE) -c $(DIR_GENERATOR)/genr_memo.ml

lib/otype.cmo: lib/otype.ml
	$(CAMLBYT) $(FRAGILE) -c lib/otype.ml
lib/otype.cmx: lib/otype.ml
	$(CAMLBIN) $(FRAGILE) -c lib/otype.ml
lib/pr_otype.cmo: lib/pr_otype.ml
	$(CAMLBYT) $(FRAGILE) -c lib/pr_otype.ml
lib/pr_otype.cmx: lib/pr_otype.ml
	$(CAMLBIN) $(FRAGILE) -c lib/pr_otype.ml
lib/pr_ocaml.cmo: lib/pr_ocaml.ml
	$(CAMLBYT) $(FRAGILE) -c lib/pr_ocaml.ml
lib/pr_ocaml.cmx: lib/pr_ocaml.ml
	$(CAMLBIN) $(FRAGILE) -c lib/pr_ocaml.ml
#lib/code.cmo: lib/code.ml
#	$(CAMLBYT) $(FRAGILE) -c lib/code.ml
#lib/code.cmx: lib/code.ml
#	$(CAMLBIN) $(FRAGILE) -c lib/code.ml
#lib/pr_code.cmo: lib/pr_code.ml
#	$(CAMLBYT) $(FRAGILE) -c lib/pr_code.ml
#lib/pr_code.cmx: lib/pr_code.ml
#	$(CAMLBIN) $(FRAGILE) -c lib/pr_code.ml
lib/relation.cmo: lib/relation.ml
	$(CAMLBYT) $(FRAGILE) -c lib/relation.ml
lib/relation.cmx: lib/relation.ml
	$(CAMLBIN) $(FRAGILE) -c lib/relation.ml

completion/equation.cmo: completion/equation.ml
	$(CAMLBYT) $(FRAGILE) -c completion/equation.ml
completion/equation.cmx: completion/equation.ml
	$(CAMLBIN) $(FRAGILE) -c completion/equation.ml

checking/check.cmo: checking/check.ml
	$(CAMLBYT) $(FRAGILE) -c checking/check.ml
checking/check.cmx: checking/check.ml
	$(CAMLBIN) $(FRAGILE) -c checking/check.ml

generation/eqnrel.cmo: generation/eqnrel.ml
	$(CAMLBYT) $(FRAGILE) -c generation/eqnrel.ml
generation/eqnrel.cmx: generation/eqnrel.ml
	$(CAMLBIN) $(FRAGILE) -c generation/eqnrel.ml
generation/genr_pattern.cmo: generation/genr_pattern.ml
	$(CAMLBYT) $(FRAGILE) -c generation/genr_pattern.ml
generation/genr_pattern.cmx: generation/genr_pattern.ml
	$(CAMLBIN) $(FRAGILE) -c generation/genr_pattern.ml
generation/genr_sharing.cmo: generation/genr_sharing.ml
	$(CAMLBYT) $(FRAGILE) -c generation/genr_sharing.ml
generation/genr_sharing.cmx: generation/genr_sharing.ml
	$(CAMLBIN) $(FRAGILE) -c generation/genr_sharing.ml
generation/genr_expression.cmo: generation/genr_expression.ml
	$(CAMLBYT) $(FRAGILE) -c generation/genr_expression.ml
generation/genr_expression.cmx: generation/genr_expression.ml
	$(CAMLBIN) $(FRAGILE) -c generation/genr_expression.ml
generation/genr.cmo: generation/genr.ml
	$(CAMLBYT) $(FRAGILE) -c generation/genr.ml
generation/genr.cmx: generation/genr.ml
	$(CAMLBIN) $(FRAGILE) -c generation/genr.ml

driver/main.cmo: driver/main.ml
	$(CAMLBYT) $(FRAGILE) -c driver/main.ml
driver/main.cmx: driver/main.ml
	$(CAMLBIN) $(FRAGILE) -c driver/main.ml

# The installation

install: all
	$(MKDIR) $(BINDIR)
	$(CP) $(BYTAPPLI) $(BINDIR)/
	$(CP) $(BINAPPLI) $(BINDIR)/
	$(LN) $(BINAPPLI) $(BINDIR)/$(APPLI)

uninstall:
	$(RM) $(BINDIR)/$(SOFTWARE)

depend:: beforedepend

beforedepend:\
 $(DIR_PARSING)/linenum.ml\
 $(DIR_PARSING)/lexer.ml $(DIR_PARSING)/parser.ml\
 $(DIR_COMPLETION)/clexer.ml $(DIR_COMPLETION)/cparser.ml

include $(MAKEFILES_DIR)/Caml.mk
