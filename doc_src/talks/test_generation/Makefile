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

# $Id: Makefile,v 1.7 2008-02-13 10:22:22 weis Exp $

SRCROOTDIR = ../../..

MAKEFILES_DIR = $(SRCROOTDIR)/Mk

default: talk.pdf

DOCDIR = $(SRCROOTDIR)/doc
THISDOCDIR = $(DOCDIR)/talks/test_generation

include $(MAKEFILES_DIR)/../config/Makefile.config
include $(MAKEFILES_DIR)/Config.mk
include $(MAKEFILES_DIR)/Text_config.mk

# The list of TEX source files.
TEXSRCS = talk.tex
TEXOBJS =
DVIPSOBJS =
DVIPDFOBJS = $(TEXSRCS:.tex=.pdf)
HEVEAOBJS = $(TEXOBJS:.dvi=.htm)
HTMLCHTMOBJS =
HTMLCMOBJS =
HTMLCINCLUDES =

# The list of HTML source files.
HTMLCHTMLSRCS =

# Yet no way to obtain HTML files from this TEX file.
HEVEAOBJS =

all:: talk.pdf

docdir::
	$(MKDIR) $(THISDOCDIR)
	$(CP) $(DVIPDFOBJS) $(THISDOCDIR)/

include $(MAKEFILES_DIR)/Text.mk

clean::
	$(RM) -f *.z[io] *~ *.txt *.log *.aux *.dvi talk.ps spelling

scratch: clean
	$(RM) -f *.bbl

talk.pdf: talk.tex slides.tex but.tex structure.tex equations.tex \
	substitutions.tex valeurs.tex a_faire.tex
	@ TEXINPUTS=$(OSTYLES):$$TEXINPUTS; export TEXINPUTS; pdflatex talk.tex
