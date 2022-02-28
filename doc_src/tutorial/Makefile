LATEX=pdflatex
BIBTEX=bibtex

.PHONY: main.pdf tutorial.pdf 

default: main.pdf tutorial.pdf


clean:
	-rm -f *~ \#* *.aux *.log *.pdf *.dvi *.ps *.bbl *.blg *.toc
#for MacOs
	-rm -Rf auto 


main.pdf : main.tex

tutorial.pdf: main.tex
	$(LATEX) $<
	$(LATEX) $<
	$(BIBTEX) $(<:.tex=.aux)
	$(LATEX) $<
	$(LATEX) $<
	-cp main.pdf tutorial.pdf


.SUFFIXES: .tex .pdf


.tex.pdf:
	$(LATEX) $<
	$(LATEX) $<
	$(BIBTEX) $(<:.tex=.aux)
	$(LATEX) $<
