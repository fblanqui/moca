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

# $Id: Banner.mk,v 1.5 2012-06-04 13:01:21 weis Exp $

# Banner handling
# BHFLAGS should be set in the calling Makefile.
# BHFLAGS = -bf $(BANNERFILE)
BH = bh $(BHFLAGS) -wla 1
BHM = $(BH) -slf 2

# Default banner file
# BANNERFILE should be set in the calling Makefile.
# BANNERFILE = banner

.PHONY: bannerize

bannerize: $(BANNERFILE)
	@if [ -n "$(BHFILES)" ]; then \
	  FILES="$(BHFILES)"; \
	  for i in $$FILES; do \
	    echo $$i; \
	    $(BH) $$i; \
	    echo " done"; \
	  done; \
	fi
	@if [ -n "$(BHMFILES)" ]; then \
	  FILES="$(BHMFILES)"; \
	  for i in $$FILES; do \
	    echo $$i; \
	    $(BHM) $$i; \
	    echo " done"; \
	  done; \
	fi
	@if [ -n "$(SUBDIRS)" ]; then \
	  DIRS="$(SUBDIRS)"; \
	  for i in $$DIRS; do \
	    (cd $$i; $(MAKE) -f Makefile.distrib $@) \
	  done; \
	fi
