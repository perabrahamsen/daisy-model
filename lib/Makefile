OLDCROPS = beet-old.dai grass-old.dai pea-old.dai potato-old.dai \
	sbarley-old.dai srape-old.dai wbarley-old.dai wrape-old.dai \
	wwheat-old.dai 
NEWCROPS = ryegrass.dai wclover.dai potato.dai \
	maize.dai pea.dai rye.dai sbarley.dai sugarbeet.dai srape.dai \
	swheat.dai wbarley.dai wrape.dai wwheat.dai grass.dai fodderbeet.dai

DKCROPS = dk-wrape.dai dk-grass.dai dk-maize.dai dk-pea.dai \
	dk-rye.dai dk-wwheat.dai dk-wbarley.dai dk-sbarley.dai

DAISY 	= OpenMI.dai markvand.dai log-std.dai dk-fertilizer.dai \
	dk-crop.dai dk-horizon.dai log-old.dai crop-old.dai vegetation.dai \
	crop.dai fertilizer.dai tillage.dai log.dai mini.dai chemistry.dai \
	$(OLDCROPS) $(NEWCROPS) $(DKCROPS)
TEXT	= Makefile  all.dai ui-Qt.dai
REMOVE 	=  weather.dwf dk-soil.dai mshe.dai mshe-standalone.dai

all:
	echo Nothing to do here.

# Create a ZIP file with all the sources.
#
daisy-lib.zip:	$(DAISY) $(TEXT)
	rm -f daisy-lib.zip
	zip daisy-lib.zip $(DAISY) $(TEXT)

# Move it to ftp.
#
dist:
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	$(MAKE) daisy-lib.zip
	mv -f daisy-lib.zip $(FTPDIR)/daisy-lib/daisy-lib-$(TAG).zip
	rm -f $(FTPDIR)/daisy-lib.zip
	(cd $(FTPDIR); ln -s daisy-lib/daisy-lib-$(TAG).zip daisy-lib.zip)

setup:
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	mkdir $(SETUPDIR)/lib
	cp $(DAISY) $(TEXT) $(SETUPDIR)/lib

# Update the CVS repository.
#
cvs: $(DAISY) $(TEXT)
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	-cvs add $(DAISY) $(TEXT)
	rm -f $(REMOVE) 
	-cvs remove $(REMOVE) 
	cvs commit -m "$(TAG)" # "Version $(TAG)"

