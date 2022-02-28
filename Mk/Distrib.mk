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

#(* $Id: Distrib.mk,v 1.34 2012-06-04 14:29:25 weis Exp $ *)

# Just for the $(PACKAGE) maintainers, a Makefile to distribute the
# software.
# This Makefile includes the toplevel Makefile of the application that is
# supposed to define the variables:
# PACKAGE (the name of the system to distribute, e.g. Moca)
# SOFTWARE (the name of the application or command to distribute, e.g. mocac)

# Usage:
# - you must set the variables WEBSITEDIR and FTPSITEDIR below,
# - verify that the prerequisites below are fulfilled,
# - then type in
# make -f Distrib.mk distribute

# The Makefile builds a tar ball file with the set of source files and moves
# it the $(FTPSITEDIR) directory.
# The Makefile copies the documentation in the $(WEBSITEDIR) directory.

# Pre-requisites:
# - You need a doc_src directory where the documentation has been written,
#   presumably in HTML, with an index.htm file. This directory is copied
#   onto the WEB site directory of the package, as mentioned in
#   $(WEBSITEDIR).
# - The doc directory is supposed to contain
#   * 2 files named eng.htm and fra.htm that respectively contain
#     the English and French documentation,
#   * 2 files copyright.fra.htm and copyright.eng.htm to rule the copyright
#     notice in french and english.

include Makefile
include config/Makefile.config

# The prefix for the package tar ball file (without its extension)
PACKAGE_FULL_NAME=$(PACKAGE)-$(VERSION)

PACKAGE_RELEASE_DIR_NAME=$(PACKAGE_FULL_NAME)
PACKAGE_ARCHIVE_NAME=$(PACKAGE_FULL_NAME).tgz

# The group which should have w/r/x permissions on the distribution.
PACKAGE_GROUP=caml

# Where the software files reside on the CVS repository:
PACKAGE_CVS_ROOT="`cat CVS/Root`"
PACKAGE_CVS_REPOSITORY="`cat CVS/Repository`"

# The CVS root directory on the CVS server for the package.
# Normally, something like: ROOT_OF_THE_CVS_REPOSITORY/$(PACKAGE).
# Here, we get an extra sub-directory: bazar-ocaml.
PACKAGE_CVS_DIRECTORY=bazar-ocaml/$(PACKAGE)

# The tag to set on files in the CVS repository when releasing
PACKAGE_RELEASE_TAG=$(PACKAGE)-$(MAINVERSION)_$(SUBVERSION)_$(PATCHLEVEL)

# The file that contains the announce of the new relase in the
# relevant mailing lists.
ANNOUNCE_FILE=Announce-$(VERSION)
PACKAGE_ANNOUNCE_MAILING_LISTS="caml-list@inria.fr caml-announce@inria.fr"

# The list of files that will be searched and deleted from the entire
# distribution before making the release tar ball.
# This one is pretty standard: it removes working files useless to the user of
# the package.
PACKAGE_RELEASE_EXCLUDE_FILE_LIST=\
 "CVS *cvsignore* banner* .depend* TODO README.devel Makefile.distrib\
 doc_src test"
# The list of files that will be deleted from the distribution before
# making the release tar ball.
# This is the list of private directory/files.
PACKAGE_RELEASE_REMOVE_FILE_LIST=\
 "$(PACKAGE_RELEASE_DIR_NAME)/coq"

WEB_SITE_DIR=/net/yquem/infosystems/www/bazaar/$(PACKAGE)
FTP_SITE_DIR=/net/yquem/infosystems/ftp/bazaar/$(PACKAGE)
#FTP_SITE_DIR2=/net/pauillac/infosystems/ftp/cristal/caml-light/bazar-ocaml/$(PACKAGE)

.PHONY: fake distribute package website ftp ftp2 tag retag untag\
 announce version\
 documentation release_test release_examples release

fake:
	@echo &&\
	@echo 'This Makefile is used to distribute the software.' &&\
	@echo 'Read it to understand how to use it!' &&\
	@echo

# Distribution of the software
distribute: package website ftp tag announce #ftp2 version

package: clean-all all release_test release_examples documentation release

clean-all::
	(cd doc_src && $(MAKE) $@) &&\
	$(RM) doc release

# Make the documentation and test everything before release.
# Then build a clean documentation directory for distribution.
# TODO: express dependancies here:
# documentation has just to update directory doc files.
# directory doc files depends on corresponding doc_src (compiled) files.
# doc_src (compiled) files should be up-to-date as usual and rebuilt with
# regular rules for documentation files.
documentation:
	(cd doc_src && $(MAKE) all) &&\
	(cd doc_src && $(MAKE) docdir)

# Recompile the tests
# TODO: express dependancies here:
# test files should be an explicit list of files that got dependancies wrt
# the executables provided by the software.
release_test:
	echo && echo &&\
	echo "Compiling tests" &&\
	echo && echo &&\
	(cd test && $(MAKE) all) &&\
	echo && echo &&\
	echo "Tests compiled" &&\
	echo && echo

# Create the examples directory && copy the example files.
# TODO: express dependancies here:
# example files should be an explicit list of (compiled) files that got
# extra dependancies wrt their corresponding sources in the test directory,
# (hence an inherited dependancy wrt the executables provided by the
# software).
release_examples:
	echo && echo &&\
	echo "Populating the examples directory." &&\
	echo && echo &&\
	(cd test && $(MAKE) examplesdir) &&\
	echo && echo &&\
	echo "Examples directory populated." &&\
	echo && echo

# Clean the old release directory;
# check out a brand new version in a new release directory,
# copy the documentation in it,
# then clean the new release directory.
# TODO: express dependancies here:
# directory release depends on any of the source files of the software,
# including the doc and examples directory (compiled) files.
release:
	$(RM) release &&\
	$(MKDIR) release &&\
	PACKAGE_CVS_REPOSITORY=$(PACKAGE_CVS_REPOSITORY) &&\
	PACKAGE_CVS_ROOT=$(PACKAGE_CVS_ROOT) &&\
	(cd release &&\
	   $(CVS) -d $$PACKAGE_CVS_ROOT export -r $(PACKAGE_RELEASE_TAG)\
		  -d $(PACKAGE_RELEASE_DIR_NAME) $$PACKAGE_CVS_REPOSITORY &&\
	   $(CP) ../doc $(PACKAGE_RELEASE_DIR_NAME) &&\
	   $(CP) ../examples $(PACKAGE_RELEASE_DIR_NAME) &&\
	   for i in $(PACKAGE_RELEASE_EXCLUDE_FILE_LIST); do\
	     find . -name "$$i" | xargs $(RM);\
	   done &&\
	   for i in $(PACKAGE_RELEASE_REMOVE_FILE_LIST); do\
	     $(RM) $$i;\
	   done)

# Build the Web site
# TODO: express dependancies here:
# The website depends upon the making of the release directory, and more ?
website:
	$(RM) $(WEB_SITE_DIR).new &&\
	$(MKDIR) $(WEB_SITE_DIR).new/archive &&\
	$(CP) release/$(PACKAGE_RELEASE_DIR_NAME)/doc/* $(WEB_SITE_DIR).new &&\
	$(CHGRP) $(PACKAGE_GROUP) $(WEB_SITE_DIR).new &&\
	$(CHMOD) $(WEB_SITE_DIR).new &&\
	$(MV) $(WEB_SITE_DIR) $(WEB_SITE_DIR).old &&\
	$(MV) $(WEB_SITE_DIR).new $(WEB_SITE_DIR) &&\
	$(RM) $(WEB_SITE_DIR).old

tarball: release
	(cd release &&\
	 $(TARC) $(PACKAGE_ARCHIVE_NAME) $(PACKAGE_RELEASE_DIR_NAME))

# Build the FTP site
# Copy the legalease to $(FTP_SITE_DIR).
# Give the release its versionning name for distribution.
# Build the distribution archive and move it in the proper ftp site.
# TODO: express dependancies here:
ftp: tarball
	$(MKDIR) $(FTP_SITE_DIR) &&\
	$(CP) release/$(PACKAGE_RELEASE_DIR_NAME)/README $(FTP_SITE_DIR) &&\
	$(CP) release/$(PACKAGE_RELEASE_DIR_NAME)/LICENSE $(FTP_SITE_DIR) &&\
	$(CP) release/$(PACKAGE_RELEASE_DIR_NAME)/AUTHORS $(FTP_SITE_DIR) &&\
	(cd release &&\
	 $(MV) $(PACKAGE_ARCHIVE_NAME) $(FTP_SITE_DIR)) &&\
	$(CP) $(FTP_SITE_DIR)/$(PACKAGE_ARCHIVE_NAME) $(WEB_SITE_DIR)/archive/

# Build the FTP2 site
# Copy the FTP site to $(FTP_SITE_DIR2)
# TODO: express dependancies here:
ftp2:
	$(MKDIR) $(FTP_SITE_DIR2) &&\
	$(CP) $(FTP_SITE_DIR)/* $(FTP_SITE_DIR2)/

# Tagging the version for the release.
tag:
	$(CVS) rtag -R $(PACKAGE_RELEASE_TAG) $(PACKAGE_CVS_DIRECTORY)

# Tagging the version again with the same tag,
# (in case of bug during the distribution process).
retag:
	$(CVS) rtag -R -F $(PACKAGE_RELEASE_TAG) $(PACKAGE_CVS_DIRECTORY)

# Untagging the version (in case of bug in the software code).
# Do not forget to remove the old release directory if any.
untag:
	$(RM) ./release &&\
	$(CVS) rtag -R -d $(PACKAGE_RELEASE_TAG) $(PACKAGE_CVS_DIRECTORY)

# Sending the announce to the mailing lists.
announce:
	mail -n -s "New release of $(PACKAGE)"\
	  $(PACKAGE_ANNOUNCE_MAILING_LISTS) < $(ANNOUNCE_FILE)

# Automatic handling of versionning
# Should be done with HTMLC
#version:
#	for i in $(PACKAGE_VERSION_FILES); do\
#	echo $$i &&\
#	$(MV) $$i $$i~ &&\
#	sed -e '/ersion/s/$(OLDVERSION)/$(VERSION)/' $$i~ |\
#	sed -e '/year/s/$(OLDYEAR)/$(YEAR)/' > $$i;\
#	done
