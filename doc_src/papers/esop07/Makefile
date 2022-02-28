#*********************************************************************#
#                                                                     #
#                             Active-DVI                              #
#                                                                     #
#                   Projet Cristal, INRIA Rocquencourt                #
#                                                                     #
#  Copyright 2003 Institut National de Recherche en Informatique et   #
#  en Automatique.  All rights reserved.  This file is distributed    #
#  under the terms of the GNU Lesser General Public License.          #
#                                                                     #
#  Jun Furuse, Didier Rémy and Pierre Weis.                           #
#  Contributions by Roberto Di Cosmo, Didier Le Botlan,               #
#  Xavier Leroy, and Alan Schmitt.                                    #
#                                                                     #
# Pierre Weis                                                         #
#                                                                     #
#*********************************************************************#

SRCROOTDIR = ../../..

MAKEFILES_DIR = $(SRCROOTDIR)/Mk

default: all

DOCDIR = $(SRCROOTDIR)/doc
THISDOCDIR = $(SRCROOTDIR)/doc/papers/esop07

include $(MAKEFILES_DIR)/../config/Makefile.config
include $(MAKEFILES_DIR)/Config.mk
include $(MAKEFILES_DIR)/Text_config.mk

# The list of TEX source files.
TEXSRCS = main.tex
TEXOBJS = $(TEXSRCS:.tex=.dvi)
DVIPSOBJS = $(TEXSRCS:.tex=.ps)
DVIPDFOBJS = $(TEXSRCS:.tex=.pdf)
HEVEAOBJS = $(TEXSRCS:.tex=.htm)
HTMLCHTMOBJS =
HTMLCMOBJS = 
HTMLCINCLUDES =

# The list of HTML source files.
HTMLCHTMLSRCS =

# Yet no way to obtain HTML files from this TEX file.
HEVEAOBJS =

docdir::
	$(MKDIR) $(THISDOCDIR)
	$(CP) $(TEXOBJS) $(DVIPSOBJS) $(DVIPDFOBJS) $(THISDOCDIR)/

include $(MAKEFILES_DIR)/Text.mk
