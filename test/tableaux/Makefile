SUBDIRS = propositional first_order

include ./Makefile.common

.PHONY : all clean

all :
	for i in $(SUBDIRS); do \
	(cd $$i; make;) \
	done
