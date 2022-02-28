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

# $Id: Makefile,v 1.8 2012-03-07 15:51:31 weis Exp $

SRCROOTDIR=../..

MAKEFILES_DIR=$(SRCROOTDIR)/Mk

# Include the generic setup for Caml applications.
include $(MAKEFILES_DIR)/Config.mk

# We need to relax the warning is error option of the compiler,
# since moca still generates unused match cases.
CAMLBYTCONFIG=-w A -warn-error Aeuz -g $(CAMLBYTINCLUDES)
CAMLBINCONFIG=-w A -warn-error Aeuz $(CAMLBININCLUDES)

# To provide a dependency to the actual mocac compiler.
MOCACFLAGS=-sharing -memo

MOCAC_COMPILER=$(SRCROOTDIR)/mocac $(MOCACFLAGS)

#include ./Makefile.common

LIBMODS=atom

GENMODS=bdd
MOCAGENFILES=$(GENMODS:%=%.mli) $(GENMODS:%=%.ml)

MLMODS=queens main

MLIFILES=\
 $(LIBMODS:%=%.mli)\
 $(GENMODS:%=%.mli)\

MLFILES=\
 $(LIBMODS:%=%.ml)\
 $(GENMODS:%=%.ml)\
 $(MLMODS:%=%.ml)\

CAMLFILES=\
 $(MLIFILES)\
 $(MLFILES)\

CMXFILES=$(MLFILES:%.ml=%.cmx)
CMOFILES=$(MLFILES:%.ml=%.cmo)

EXECFILE=queens

default: $(EXECFILE)

all: depend $(EXECFILE).bin $(EXECFILE).byt

$(EXECFILE): $(EXECFILE).bin
	cp $(EXECFILE).bin $(EXECFILE)

$(EXECFILE).bin: $(CMXFILES)
	$(CAMLBIN) -o $@ $(CMXFILES)

$(EXECFILE).byt: $(CMOFILES)
	$(CAMLBYT) -o $@ $(CMOFILES)

beforedepend: $(MOCAGENFILES)

clean::
	rm -f $(MOCAGENFILES)
	rm -f $(EXECFILE) $(EXECFILE).bin $(EXECFILE).byt

# Including the spefific rules to compile Caml programs.
include $(MAKEFILES_DIR)/Caml.mk
