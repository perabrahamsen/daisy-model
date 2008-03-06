LIBS = macro.tex daisy.bib

FIGS = start.fig

REFDOCS = reference.tex introduction.tex title.tex dwf.tex

DOCS = notes.tex requirements.tex exercises.tex \
	litterature.tex c-api.tex guide.tex tutorial.tex $(REFDOCS)

PRESENTATIONS = esa-2004.tex 
NOTES = 2DDaisy.txt forced_LAI.txt
FUTURE = organic.tex

TEXT = $(LIBS) $(FIGS) $(DOCS) $(PRESENTATIONS) $(NOTES) $(FUTURE) Makefile

REMOVE = crop-lai.tex

all:
	echo Please be specific...

ps:	reference.ps

pdf:	reference.pdf

reference.ps:	reference.dvi
	dvips -f reference.dvi > reference.ps

reference.dvi: reference.tex components.tex $(LIBS)
	latex reference.tex < /dev/null
	makeindex reference
	bibtex reference
	latex reference.tex < /dev/null

reference.pdf: reference.tex components.tex $(LIBS)
	pdflatex reference.tex < /dev/null
	makeindex reference
	bibtex reference
	pdflatex reference.tex < /dev/null

components.tex:	/usr/local/daisy/$(HOSTTYPE)/daisy
	/usr/local/daisy/$(HOSTTYPE)/daisy all.dai -p document > components.tex

exercises.ps:	exercises.dvi 
	dvips -f exercises.dvi > exercises.ps

exercises.dvi:	exercises.tex $(LIBS)
	latex exercises.tex < /dev/null
	bibtex exercises
	latex exercises.tex < /dev/null

exercises.pdf:	exercises.tex $(LIBS)
	pdflatex exercises.tex < /dev/null
	bibtex exercises
	pdflatex exercises.tex < /dev/null

tutorial.ps:	tutorial.dvi
	dvips -f tutorial.dvi > tutorial.ps

tutorial.dvi:	tutorial.tex $(LIBS)
	latex tutorial.tex < /dev/null
	bibtex tutorial
	latex tutorial.tex < /dev/null

tutorial.pdf:	tutorial.tex $(LIBS)
	pdflatex tutorial.tex < /dev/null
	bibtex tutorial
	pdflatex tutorial.tex < /dev/null

dist: reference.pdf reference.ps tutorial.pdf exercises.pdf
	cp -f reference.pdf $(FTPDIR)/daisy-ref.pdf
	cp -f reference.ps $(FTPDIR)/daisy-ref.ps
	cp -f tutorial.pdf $(FTPDIR)/daisy-tut.pdf
	cp -f exercises.pdf $(FTPDIR)/

cvs: $(TEXT)
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	-cvs add $(TEXT)
	rm -f $(REMOVE) 
	-cvs remove $(REMOVE) 
	cvs commit -m "$(TAG)" # "Version $(TAG)"

commit: $(TEXT)
	-cvs add $(TEXT)
	rm -f $(REMOVE) 
	-cvs remove $(REMOVE) 
	cvs commit -m "commit" # "Version $(TAG)"
