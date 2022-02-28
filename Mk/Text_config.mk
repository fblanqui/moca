# Directories
INCDIR = $(ROOTDIR)/doc_src/Includes

# Basic Unix commands
RM = rm -rf
CP = cp -fpRL
MV = mv -f
LN = ln -sf
GZIP = gzip -f
MKDIR = mkdir -p
TARC = tar -czvf
FIND = find
GUNZIP = gunzip -f
ZIP = zip -rq

# Compiling commands and flags
PIC = pic

LATEX = latex
BIBTEX = bibtex
DVICOPY = dvicopy
DVIPS = dvips
DVIPDFM = dvipdfm
DVIPDF = dvipdf

HFLAGS = -entities -fix $(INCDIR)/macros.hva
HEVEA = hevea $(HFLAGS)
IFLAGS = -extra "ppmquant 255"
IMAGEN = imagen $(IFLAGS)

HTMLCFLAGS = -I $(INCDIR) -env env
HTMLCINCLUDES = $(INCDIR)/env $(INCDIR)/version.html
HTMLC = htmlc $(HTMLCFLAGS)

# Compute the objects lists
TEXOBJS = $(TEXSRCS:.tex=.dvi)
DVIPSOBJS = $(TEXOBJS:.dvi=.ps)
DVIPDFOBJS = $(TEXOBJS:.dvi=.pdf)
HEVEAOBJS = $(TEXOBJS:.dvi=.htm)

HTMLCHTMOBJS = $(HTMLCHTMLSRCS:.html=.htm)
HTMLCMOBJS = $(HTMLCMANSRCS:.man=.m)
ALLHTMLCOBJS = $(HTMLCHTMOBJS) $(HTMLCMOBJS)
