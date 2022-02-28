BBNSDIR=bbns bbns+more bbns+all
BBSDIR=bbs
VBNSDIR=vbns vbns+all
SUBDIRS=$(BBNSDIR) $(BBSDIR) $(VBNSDIR)

include ./Makefile.common

.PHONY : all world bbns bbs vbns

all : bbns bbs

world: bbns bbns+more bbns+all bbs vbns vbns+all

bbns:
	for i in $(BBNSDIR); do \
	echo $$i; cd $$i; make all; cd ..; \
	done

bbs:
	for i in $(BBSDIR); do \
	echo $$i; cd $$i; make all; cd ..; \
	done

vbns:	
	for i in $(VBNSDIR); do \
	echo $$i; cd $$i;  make all; cd ..; \
	done

beforedepend:

clean:
	-rm -f \#*;
	for i in $(SUBDIRS); do \
	 (if [ -d $$i ]; then \
	    echo $$i; cd $$i; make clean; cd ..; \
	  fi) || exit $$?; \
	done
	find . -name '*~' | xargs $(RM)

veryclean: clean
	for i in $(SUBDIRS); do \
	 (if [ -d $$i ]; then \
	    echo $$i; cd $$i; rm .depend; cd ..; \
	  fi) || exit $$?; \
	done