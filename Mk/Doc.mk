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

# $Id: Doc.mk,v 1.21 2012-06-04 13:01:21 weis Exp $

# Makefile for HTML documentation generation.
# From a list of source files (.html files), it generates the
# corresponding object files (.htm files) using the HTML compiler Htmlc.

# Usage: if necessary fill in the HTMLC and FILES variables below,
# then type in:
# make (or make all) to rebuild the whole thing
# make clean to remove all the generated files.

# The directory where Makefile templates reside should be defined in including
# Makefiles.
# MAKEFILES_DIR = ../Mk

include $(MAKEFILES_DIR)/../config/Makefile.config
include $(MAKEFILES_DIR)/Config.mk

# The Htmlc compiler with its include path options
HTMLC = htmlc -I Includes -env Includes/env
ROFF = nroff -man

# The list of source files
FILES = copyright.fr.html copyright.en.html

MANFILES = $(SOFTWARE).man

######## This part should automatically handle the generation of
######## object files

# The list of object files
OBJS = fra.htm eng.htm index.htm $(FILES:.html=.htm)
MANOBJS = $(MANFILES:.man=.1)

all:: $(OBJS) $(MANOBJS)

$(OBJS): Includes/*.html

clean::
	$(RM) $(OBJS) $(MANOBJS) *~
	$(RM) *.htm
	cd Includes; $(RM) *.htm *~

index.htm: $(SOFTWARE).index.html
	$(HTMLC) -f $(SOFTWARE).index.html -t index.htm
fra.htm: $(SOFTWARE).fr.html
	$(HTMLC) -f $(SOFTWARE).fr.html -t fra.htm
eng.htm: $(SOFTWARE).en.html
	$(HTMLC) -f $(SOFTWARE).en.html -t eng.htm
copyright.fr.htm: Includes/copyright.fr.html
	$(HTMLC) -f Includes/copyright.fr.html -t copyright.fr.htm
copyright.en.htm: Includes/copyright.en.html
	$(HTMLC) -f Includes/copyright.en.html -t copyright.en.htm

configure: clean

.SUFFIXES:
.SUFFIXES: .htm .html .man .1

.html.htm:
	$(RM) $@
	$(HTMLC) -f $< -t $@
.man.1:
	$(RM) $@
	$(HTMLC) -f $< -t - | tbl > $@
