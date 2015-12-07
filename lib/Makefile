OLDCROPS = beet-old.dai grass-old.dai pea-old.dai potato-old.dai \
	sbarley-old.dai srape-old.dai wbarley-old.dai wrape-old.dai \
	wwheat-old.dai 
NEWCROPS = ryegrass.dai wclover.dai potato.dai \
	maize.dai pea.dai rye.dai sbarley.dai sugarbeet.dai srape.dai \
	swheat.dai wbarley.dai wrape.dai wwheat.dai grass.dai fodderbeet.dai

DKCROPS = dk-wrape.dai dk-grass.dai dk-maize.dai dk-pea.dai \
	dk-rye.dai dk-wwheat.dai dk-wbarley.dai dk-sbarley.dai


VEGCROPS = beetroot.dai broccoli.dai brusselssprouts.dai celeriac.dai \
	earlypotato.dai earlywhitecabbage.dai onion.dai \
	onion_plantingofsets.dai whitecabbage.dai

DAISY 	= chemistry-base.dai \
	OpenMI.dai markvand.dai log-std.dai dk-fertilizer.dai \
	dk-crop.dai dk-horizon.dai log-old.dai crop-old.dai vegetation.dai \
	crop.dai fertilizer.dai tillage.dai log.dai mini.dai chemistry.dai \
	$(OLDCROPS) $(NEWCROPS) $(DKCROPS) $(VEGCROPS)
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
setup:
	mkdir $(SETUPDIR)/lib
	cp $(DAISY) $(TEXT) $(SETUPDIR)/lib

# Update the SVN repository.
#
checkin: $(DAISY) $(TEXT)
	-git add $(DAISY) $(TEXT)
	-rm -f $(REMOVE) 
	-git rm $(REMOVE) 

