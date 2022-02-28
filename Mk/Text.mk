# Generic rules
.SUFFIXES:
.SUFFIXES: .pic .tex .bib .dvi .advi .bbl .ps .pdf .html .htm .man .m

.pic.tex:
	$(PIC) -t $< > $@

.tex.dvi:
	$(LATEX) $<

.dvi.advi:
	$(DVICOPY) $< > $@

.tex.bbl:
	$(LATEX) $<
	$(BIBTEX) $*

.tex.ps:
	$(LATEX) $<
	$(LATEX) '\def\driver{dvips}\input' $<
	$(DVIPS) -o $@ $*.dvi

.tex.pdf:
	$(LATEX) $<
	$(LATEX) '\def\driver{dvipdfm}\input' $<
	$(DVIPDFM) -o $@ $*.dvi

.tex.htm:
	$(HEVEA) -o $@ $<

.html.htm:
	$(HTMLC) -from $< -to $@

.man.m:
	$(HTMLC) -from $< -to $@

# Generic targets
.PHONY: all dvi ps pdf htm html man m clean clean-all

all:: dvi ps pdf htm

dvi:: $(TEXOBJS)

ps:: $(DVIPSOBJS)

pdf:: $(DVIPDFOBJS)

html htm:: $(HEVEAOBJS) $(HTMLCHTMOBJS)

man m:: $(HTMLCMOBJS)

$(ALLHTMLCOBJS): $(HTMLCINCLUDES)

clean::
	$(RM) *~ *.aux *.log *.idx *.advi *.out *.toc *.bbl *.blg
	$(RM) *.hidx *.hind *.haux *.htoc *.image.tex
	$(RM) *.dvi *.ps* *.pdf *.htm *.m
