SETUP = ror-pa.dai chemical25.dai plotstuff.dai ror-log.dai ror-manager.dai \
	ror-setup.dai ror-soil.dai colloid-bound.dai plot-2d.dai biopores.dai

DATA = piezo-calib.gwt brommass.ddf piezometer.ddf rain.ddf \
	Bromide-25.ddf bromide.ddf drain-flow.ddf ioxynil.ddf particles.ddf \
	pendimethalin.ddf theta_12cm.ddf theta_16cm.ddf theta_20cm.ddf \
	theta_24cm.ddf theta_36cm.ddf theta_4cm.ddf theta_60cm.ddf theta_8cm.ddf

OTHER = ror-mmo.dai 

DOC = plotstuff.tex 

DAISY = /home/abraham/daisy/obj/daisy

TEXT = $(SETUP) $(OTHER) $(DOC) Makefile $(DATA)

REMOVE 	= plotstuff-9899.dai plotstuff-9899.tex

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

plotstuff.pdf: plotstuff.tex .gnuplot-stamp
	pdflatex plotstuff.tex

.gnuplot-stamp: daisy.gnuplot
	-gnuplot daisy.gnuplot
	@touch .gnuplot-stamp

daisy.gnuplot: $(SETUP) $(DAISY) plotstuff.dai fig log
	nice $(DAISY) ror-pa.dai

linux:	fig log
	( export DAISYHOME=/home/abraham/daisy; nice "${DAISYHOME}/obj/daisy" ror-pa.dai )
	make plotstuff.pdf

fig:
	mkdir fig

log:
	mkdir log
