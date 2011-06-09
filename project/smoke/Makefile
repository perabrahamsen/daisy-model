SETUP = setup.dai colloid-bound.dai

DATA = bromid.ddf mixed-weather.dwf aquifer.gwt

DOC = graphs.tex 

DAISY = /home/abraham/daisy/obj/daisy

TEXT = $(SETUP) $(DOC) Makefile $(DATA)

REMOVE 	= biopores.dai ror-manager.dai ror-setup.dai ror-soil.dai 

all:
	echo Please be specific

svnci: $(TEXT) 
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	-svn add $(TEXT) 
	rm -f $(REMOVE) 
	-svn remove $(REMOVE) 
	svn commit -m "$(TAG)" # "Version $(TAG)"

add:
	svn add $(TEXT)
	-svn remove $(REMOVE)

graphs.pdf: graphs.tex .gnuplot-stamp
	pdflatex graphs.tex

.gnuplot-stamp: daisy.gnuplot
	-gnuplot daisy.gnuplot
	@touch .gnuplot-stamp

daisy.gnuplot: $(SETUP) $(DATA) $(DAISY) fig log
	nice $(DAISY) ror-setup.dai

fig:
	mkdir fig

log:
	mkdir log
