LATEX=pdflatex
BIBTEX=bibtex

.PHONY: main.pdf 

default: main.pdf 

clean:
	-rm -f *~ \#* *.aux *.log *.pdf *.dvi *.ps \
	*.bbl *.blg *.toc *.out *.cb*
#for MacOs
	-rm -Rf auto 

publish:
	cp main.pdf bonichon_rapport_postdoc.pdf

examples:
	(cd ./code; $(MAKE))

main.pdf : examples main.tex


.SUFFIXES: .tex .pdf

.tex.pdf:
	$(LATEX) $<
	$(LATEX) $<
	$(BIBTEX) $(<:.tex=.aux)
	$(LATEX) $<

check_spell:
	for i in *.tex; do ispell -t $$i; done
