EPSFIGS = drainflow-98-99.eps drainflowacc-98-99.eps particles-98-99.eps \
	particlesacc-98-99.eps bromide-98-99.eps brommass-98-99.eps \
	drainflow-98-99-zoom.eps drainflowacc-98-99-zoom.eps \
	particles-98-99-zoom.eps particlesacc-98-99-zoom.eps \
	bromide-98-99-zoom.eps brommass-98-99-zoom.eps \
	drainflow-99-00.eps drainflowacc-99-00.eps particles-99-00.eps \
	particlesacc-99-00.eps bromide-99-00.eps brommass-99-00.eps \
	pendconc-99-00.eps pendmass-99-00.eps drainflow-99-00-zoom.eps \
	drainflowacc-99-00-zoom.eps particles-99-00-zoom.eps \
	particlesacc-99-00-zoom.eps bromide-99-00-zoom.eps \
	brommass-99-00-zoom.eps pendconc-99-00-zoom.eps \
	pendmass-99-00-zoom.eps drainflow-00-01.eps \
	drainflowacc-00-01.eps particles-00-01.eps particlesacc-00-01.eps \
	ioxconc-00-01.eps ioxmass-00-01.eps pendconc-00-01.eps \
	pendmass-00-01.eps drainflow-00-01-zoom.eps \
	drainflowacc-00-01-zoom.eps particles-00-01-zoom.eps \
	particlesacc-00-01-zoom.eps ioxconc-00-01-zoom.eps \
	ioxmass-00-01-zoom.eps pendconc-00-01-zoom.eps \
	pendmass-00-01-zoom.eps drainflow-01-02.eps \
	drainflowacc-01-02.eps particles-01-02.eps particlesacc-01-02.eps \
	theta4cm.eps theta8cm.eps theta12cm.eps theta16cm.eps \
	theta20cm.eps theta24cm.eps theta36cm.eps theta60cm.eps \
	weather.eps infiltration.eps pondingdepth.eps \
	brom-0-25-output.eps brom-25-50-output.eps brom-50-75-output.eps \
	brom-75-105-output.eps brom-total.eps brom-primary.eps \
	brom-secondary.eps brom-0-25-acc.eps brom-25-50-acc.eps \
	brom-50-75-acc.eps brom-75-105-acc.eps \
	theta_short4cm.eps theta_short8cm.eps theta_short12cm.eps \
	theta_short16cm.eps theta_short20cm.eps theta_short24cm.eps \
	theta_short36cm.eps theta_short60cm.eps weather_short.eps \
	infiltration_short.eps pondingdepth_short.eps \

SETUP = chemical25.dai plotstuff.dai ror-log.dai ror-manager.dai \
	ror-setup.dai ror-soil.dai rorrendechemicals.dai

DATA = rain.ddf \
	Bromide-25.ddf bromide.ddf drain-flow.ddf ioxynil.ddf particles.ddf \
	pendimethalin.ddf theta_12cm.ddf theta_16cm.ddf theta_20cm.ddf \
	theta_24cm.ddf theta_36cm.ddf theta_4cm.ddf theta_60cm.ddf theta_8cm.ddf

OTHER = test-cpe.dai ror-mmo.dai ror-pa.dai plotstuff-9899.dai

DOC = plotstuff.tex plotstuff-9899.tex

DAISY = ../../obj/daisy.exe

TEXT = $(SETUP) $(OTHER) $(DOC) Makefile $(DATA)

REMOVE 	=  none

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

plotstuff.pdf: plotstuff.dvi
	dvipdfm plotstuff.dvi

plotstuff.dvi: plotstuff.tex .gnuplot-stamp
	latex plotstuff.tex

plotstuff-9899.pdf: plotstuff-9899.dvi
	dvipdfm plotstuff-9899.dvi

plotstuff-9899.dvi: plotstuff-9899.tex .gnuplot-stamp
	latex plotstuff-9899.tex

.gnuplot-stamp: daisy.gnuplot
	-gnuplot daisy.gnuplot
	@touch .gnuplot-stamp

daisy.gnuplot: $(SETUP) $(DAISY) plotstuff.dai plotstuff-9899.dai
	$(DAISY) ror-pa.dai

