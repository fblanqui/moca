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

# $Id: Makefile,v 1.64 2012-06-04 12:50:58 weis Exp $

# The general toplevel Makefile for moca

# (0) We include some configuration Makefiles.

# The basic configuration Makefile for this software (moca):
# config/Makefile.config defines PREFIX, SOFTWARE, and PACKAGE (here SOFTWARE
# is "moca" and PACKAGE is "mocac", the command installed by $(SOFTWARE).
# It also defines MAKEFILES_DIR as the absolute path of the directory where
# are the various Makefile templates. The definition is equivalent to
# MAKEFILES_DIR=./Mk
include config/Makefile.config

# The generic setup for Caml applications.
# This Make file defines useful variables for make,
# including macros for basic unix commands (for instance $(RM) is "rm -rf")
# and configuration for the Caml compiler.
# The caml compilers are defined as variables CAMLBYT (the byte-code
# compiler) and CAMLBIN (the optimizing copmiler).
include $(MAKEFILES_DIR)/Config.mk

# (1) We define some useful variables specific to the application.

# Versionning of $(PACKAGE) and $(SOFTWARE)
MAINVERSION=0
SUBVERSION=7
PATCHLEVEL=0
VERSION=$(MAINVERSION).$(SUBVERSION).$(PATCHLEVEL)

# Files that should be edited to change the version
COMPILERVERSIONFILES=compiler/driver/configuration.ml
DOCVERSIONFILES=doc_src/Includes/env
PACKAGEVERSIONFILES=$(COMPILERVERSIONFILES) $(DOCVERSIONFILES)

# Handling the automatic construction of $(PACKAGE)
SUBDIRS=compiler test

# (2) We define the default behaviour we want for make.

.PHONY: default all world compiler byt bin \
 clean-test test examples tags clean version depend \
 install installapp installdoc \
 uninstall uninstallapp uninstalldoc

# (3) We define the targets specific to this Makefile.

default: compiler

all:: $(SUBDIRS)

world: $(SUBDIRS)

compiler: $(MAKEFILES_DIR)/Config.mk byt bin

$(MAKEFILES_DIR)/Config.mk: $(MAKEFILES_DIR)/Config.mk.in
	cd $(MAKEFILES_DIR)/; ./configure
	for i in $(SUBDIRS); do \
	  (cd $$i; $(MAKE) configure; cd ..) || exit $$?; \
	done

byt:
	cd compiler; $(MAKE) byt
	$(LN) compiler/mocac.byt $(SOFTWARE)

bin:
	cd compiler; $(MAKE) bin
	$(LN) compiler/mocac.bin $(SOFTWARE)

tags:
	otags -sr '.mli' -r compiler

examples:
	if test -d examples; then \
	  (cd examples; $(MAKE) all); \
	fi

clean-test:
	if test -d test; then \
	  (cd test; $(MAKE) clean); \
	fi
clean :: clean-test

test:
	if test -d test; then \
	  (cd test; $(MAKE) depend; $(MAKE) byt); \
	fi

alltest:
	if test -d test; then \
	  (cd test; $(MAKE) depend; $(MAKE) all); \
	fi

# Clean up
clean::
	for i in $(SUBDIRS); do \
	 (if test -d $$i; then \
	    (echo $$i; cd $$i; $(MAKE) clean); \
	  fi) || exit $$?; \
	done
	find . -name '*~' | xargs $(RM)
	$(RM) mocac

# Installation
install: installapp installdoc

installapp:
	cd compiler; $(MAKE) install
installdoc:
	if test -d doc; then \
	  (cd doc; $(MAKE) install); \
	fi

# De-Installation
uninstall: uninstallapp uninstalldoc

uninstallapp:
	cd compiler; $(MAKE) uninstall
uninstalldoc:
	if test -d doc; then \
	  (cd doc; $(MAKE) uninstall); \
	fi

# Dependencies
depend:
	cd compiler; $(MAKE) depend
	if test -d test; then \
	 (cd test; $(MAKE) depend); \
	fi
	if test -d examples; then \
	 (cd examples; $(MAKE) depend); \
	fi
