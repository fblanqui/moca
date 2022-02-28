# Hey, Emacs, this is a -*- makefile -*-
#*********************************************************************#
#                                                                     #
#                                Moca                                 #
#                                                                     #
#            Pierre Weis, projet Cristal, INRIA Rocquencourt          #
#                                                                     #
#  Copyright 2005 Institut National de Recherche en Informatique et   #
#  en Automatique.  Distributed only by permission.                   #
#                                                                     #
#*********************************************************************#

# $Id: Makefile.doc,v 1.10 2012-02-27 16:19:36 bonichon Exp $

# The directory where Makefile templates reside.
MAKEFILES_DIR=../Mk

include ../config/Makefile.config
include $(MAKEFILES_DIR)/Config.mk

# Name of the man page for the software
MANFILES=$(SOFTWARE).$(MANEXT)

install:
	$(MKDIR) $(MANDIR)
	install -m 444 $(MANFILES) $(MANDIR)

uninstall:
	for i in $(MANFILES); do \
	  $(RM) $(MANDIR)/$$i; \
	done
