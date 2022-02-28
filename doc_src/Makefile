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

# $Id: Makefile,v 1.23 2012-06-04 13:01:22 weis Exp $

# The directory where Makefile templates reside.
include ../config/Makefile.config

SUBDIRS=papers talks

include $(MAKEFILES_DIR)/Doc.mk

default: all

MOCAC_COMPILER=$(SRCROOTDIR)/mocac

$(SOFTWARE).man $(SOFTWARE).fr.html $(SOFTWARE).en.html: Includes/group.cmo

Includes/group.cmi: Includes/group.mli
	ocamlc Includes/group.mli
Includes/group.ml Includes/group.mli: Includes/group.mlm
	$(MOCAC_COMPILER) Includes/group.mlm
Includes/group.cmo: Includes/group.cmi
	ocamlc -I Includes Includes/group.ml

# Create the doc directory (the one that will be installed).
docdir:: allsubdirs docdirsubdirs ocamldocdir
	$(MKDIR) ../doc
	$(CP) Makefile.doc ../doc/Makefile
	$(CP) imgs ../doc/
	$(CP) $(OBJS) ../doc/
	$(CP) $(MANOBJS) ../doc/
	$(CP) $(CAMLDOCDIR) ../doc/
	(cd ../doc; \
	  $(LN) index.htm index.html; \
	  $(CHMOD) $(OBJS) $(MANOBJS))


include ../compiler/Targets.mk

DOCSOURCES=$(CAMLSOURCES)
DOCINTFILES=$(CAMLIOSOURCES) $(DOCSOURCES:.ml=.mli)
DOCFILES=$(DOCINTFILES) $(DOCSOURCES)
DOCINCLUDES=$(CAMLINCLUDES)
POSTPROCESSING_FILES=postdoc/jquery.js postdoc/improve.js Includes/style.css
CAMLDOC_GENFILES=`find $(CAMLDOCDIR) -name "[^type]*.html"`

.PHONY: ocamldocdir
ocamldocdir: $(MOCAC_COMPILER)
	(cd $(SRCROOTDIR)/compiler && \
	 $(MKDIR) $(CAMLDOCDIR) && \
	 $(CAMLDOC) $(CAMLDOCHTMLFLAGS) $(DOCINCLUDES) $(DOCFILES)) && \
	for f in $(CAMLDOC_GENFILES); do \
	  TMP=/tmp/`basename $$f` && \
	  $(CP) $(POSTPROCESSING_FILES) $(CAMLDOCDIR) && \
	  sed -e 's/<head>/<head>\n<script src="jquery.js"><\/script>\n<script src="improve.js"><\/script>/g' $$f > $$TMP && \
	  $(MV) $$TMP $$f; \
	done

all:: docdir

clean:: cleansubdirs
	$(RM) ../doc
	$(RM) $(CAMLDOCDIR)

clean-all: clean

allsubdirs::
	@if [ -n "$(SUBDIRS)" ]; then \
	  DIRS="$(SUBDIRS)"; \
	  for i in $$DIRS; do \
	    (cd $$i; $(MAKE) all) \
	  done; \
	fi

cleansubdirs::
	@if [ -n "$(SUBDIRS)" ]; then \
	  DIRS="$(SUBDIRS)"; \
	  for i in $$DIRS; do \
	    (cd $$i; $(MAKE) clean) \
	  done; \
	fi && \
	$(RM) Includes/group.ml Includes/group.mli Includes/group.cm*

docdirsubdirs::
	@if [ -n "$(SUBDIRS)" ]; then \
	  DIRS="$(SUBDIRS)"; \
	  for i in $$DIRS; do \
	    (cd $$i; $(MAKE) docdir) \
	  done; \
	fi
