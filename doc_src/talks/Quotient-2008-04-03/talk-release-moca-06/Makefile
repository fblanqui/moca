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

# $Id: Makefile,v 1.3 2010-09-20 11:41:09 weis Exp $

SRCROOTDIR = ../../../..

MAKEFILES_DIR = $(SRCROOTDIR)/Mk

default: all

DOCDIR = $(SRCROOTDIR)/doc
THISDOCDIR = $(DOCDIR)/talks/Quotient-2008-04-03

include $(SRCROOTDIR)/config/Makefile.config
include $(MAKEFILES_DIR)/Config.mk
include $(MAKEFILES_DIR)/Text_config.mk

# The list of TEX source files.
TEXSRCS = talk.tex slides.tex
TEXSRC = talk.tex
TEXOBJS = $(TEXSRC:.tex=.dvi)
HTMLCHTMOBJS =
HTMLCMOBJS =
HTMLCINCLUDES =
DVIPDFOBJS =

# The list of HTML source files.
HTMLCHTMLSRCS =

# Yet no way to obtain HTML files from this TEX file.
HEVEAOBJS =

docdir::
	$(MKDIR) $(THISDOCDIR)
	$(CP) $(TEXOBJS) $(DVIPSOBJS) $(DVIPDFOBJS) $(THISDOCDIR)/

include $(MAKEFILES_DIR)/Text.mk

all:: talk.dvi talk.ps talk.pdf

clean::
	$(RM) -f *.z[io] *~ *.txt *.log *.aux *.dvi talk.ps talk.pdf spelling

scratch: clean
	$(RM) -f *.bbl

talk.dvi: $(TEXSRCS)
	@ TEXINPUTS=$(OSTYLES):$$TEXINPUTS; export TEXINPUTS; latex talk.tex

talk.ps: talk.dvi
	$(DVIPS) -o talk.ps -t landscape talk.dvi

talk.pdf: talk.dvi
	$(DVIPDF) talk.dvi talk.pdf 
