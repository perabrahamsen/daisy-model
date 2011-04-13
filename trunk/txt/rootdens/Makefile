COMPARE = CAAS-2008-0.tex CAAS-2008-18.tex CAAS-2008-37.tex \
	CAU-2007a-0.tex CAU-2007a-15.tex CAU-2007a-30.tex \
	CAU-2007b-0.tex CAU-2007b-15.tex CAU-2007b-30.tex \
	CAU-2008a-0.tex CAU-2008a-15.tex CAU-2008a-30.tex \
	CAU-2008b-0.tex CAU-2008b-15.tex CAU-2008b-30.tex \

MULTI = CAU2008a.tex CAU2008b.tex

ROOTDENS = rootdens_L.tex rootdens_Q.tex rootdens_W.tex

OTHER = row.tex sample-caas.tex sample-cau.tex

ARTICLE = rootart.tex macro.tex $(OTHER) $(ROOTDENS) $(MULTI) $(COMPARE)

all:	rootart.pdf

rootart.dvi: $(ARTICLE) ../daisy.bib
	latex -interaction=nonstopmode rootart
	bibtex rootart
	latex -interaction=nonstopmode rootart
	latex -interaction=nonstopmode rootart

rootart.ps: rootart.dvi
	dvips $<

rootart.pdf: rootart.ps
	epstopdf $<

CAAS-%.tex CAU-%.tex: compare.plt
	gnuplot $<

CAU2008%.tex: multi.plt
	gnuplot $<

rootdens_%.tex: rootdens.plt
	gnuplot $<

row.tex: row.plt
	gnuplot $<

sample-%.tex: sample-%.plt
	gnuplot $<
