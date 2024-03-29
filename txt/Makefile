LIBS = macro.tex daisy.bib elsart-harv.bst

FIGS = start.fig

REFDOCS = reference.tex introduction.tex title.tex dwf.tex

DOCS = colgen.tex aba.tex geometry.tex \
	input-lang.tex gui-why.tex notes.tex requirements.tex exercises.tex \
	literature.tex c-api.tex guide.tex tutorial.tex $(REFDOCS)

PRESENTATIONS = esa-2004.tex 
NOTES = 2DDaisy.txt forced_LAI.txt
FUTURE = 

TEXT = stdlog.dai \
	$(LIBS) $(FIGS) $(DOCS) $(PRESENTATIONS) $(NOTES) $(FUTURE) Makefile

REMOVE = crop-lai.tex

all:
	echo Please be specific...

pdf:	reference.pdf

reference.dvi: reference.tex components.tex $(LIBS)
	latex reference.tex < /dev/null
	makeindex reference
	bibtex reference
	latex reference.tex < /dev/null

reference.pdf: reference.tex components.tex $(LIBS)
	gnuplot document.gnuplot
	pdflatex reference.tex < /dev/null
	makeindex reference
	bibtex reference
	pdflatex reference.tex < /dev/null
	makeindex reference
	bibtex reference
	pdflatex reference.tex < /dev/null

gnufig:
	mkdir gnufig

components.tex:	$(DAISYEXE) gnufig
	$(DAISYEXE) all.dai -p document

exercises.ps:	exercises.dvi 
	dvips -f exercises.dvi > exercises.ps

exercises.dvi: reference.dvi tutorial.dvi exercises.tex $(LIBS)
	latex exercises.tex < /dev/null
	bibtex exercises
	latex exercises.tex < /dev/null

exercises.pdf:reference.pdf tutorial.pdf exercises.tex $(LIBS)
	pdflatex exercises.tex < /dev/null
	bibtex exercises
	pdflatex exercises.tex < /dev/null

stdlog.tex: stdlog.dai $(DAISYEXE)
	$(DAISYEXE) stdlog.dai

tutorial.ps:	tutorial.dvi
	dvips -f tutorial.dvi > tutorial.ps

tutorial.dvi:reference.dvi tutorial.tex stdlog.tex $(LIBS)
	latex tutorial.tex < /dev/null
	bibtex tutorial
	latex tutorial.tex < /dev/null

tutorial.pdf:	reference.pdf tutorial.tex stdlog.tex $(LIBS)
	pdflatex tutorial.tex < /dev/null
	bibtex tutorial
	pdflatex tutorial.tex < /dev/null

dist:
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	cp -f reference.pdf $(DISTDIR)/daisy-ref-$(TAG).pdf
	cp -f tutorial.pdf $(DISTDIR)/daisy-tut-$(TAG).pdf
	cp -f exercises.pdf $(DISTDIR)/exercises-$(TAG).pdf

docs: reference.pdf tutorial.pdf exercises.pdf

setup:
	mkdir $(SETUPDIR)/doc
	cp -f reference.pdf $(SETUPDIR)/doc
	cp -f tutorial.pdf $(SETUPDIR)/doc
	cp -f exercises.pdf $(SETUPDIR)/doc


checkin: $(TEXT)
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	-git add $(TEXT)
	-rm -f $(REMOVE) 
	-git rm -f --ignore-unmatch $(REMOVE) 
#	git commit -a -m "$(TAG)" # "Version $(TAG)"

commit: $(TEXT)
	-git add $(TEXT)
	rm -f $(REMOVE) 
	-git rm -f --ignore-unmatch $(REMOVE) 
	git commit -a -m "$(TAG)" # "Version $(TAG)"

clean:
	rm -f *.aux *.log *.out *.bbl *.blg
