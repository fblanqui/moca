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

# $Id: Makefile,v 1.7 2008-02-12 17:57:11 weis Exp $

SUB_DIRS = esop07

MAKEFILES_DIR = ../../Mk

DOCDIR = ../../doc
THISDOCDIR = $(DOCDIR)/papers

include $(MAKEFILES_DIR)/../config/Makefile.config
include $(MAKEFILES_DIR)/Config.mk

# where are include files
HTMLCCONFINCLUDES = -I $(SRCROOTDIR)/doc_src/Includes
# where is the configuration file for Htmlc.
HTMLCCONFENV = -env $(SRCROOTDIR)/doc_src/Includes/env

.SUFFIXES:
.SUFFIXES: .htm .html .shtml .data .1 .man

.html.htm:
	$(RM) $@
	$(HTMLC) -i $< -o $@

SRCS = fra.html eng.html
OBJS = $(SRCS:.html=.htm)

all:: $(OBJS)
clean::
	$(RM) $(OBJS)

docdir::
	$(MKDIR) $(THISDOCDIR)
	$(CP) $(OBJS) $(THISDOCDIR)/

all clean docdir::
	for i in $(SUB_DIRS); do \
	  (cd $$i; $(MAKE) $@) \
	done
