OLDCROPS = beet-old.dai grass-old.dai pea-old.dai potato-old.dai \
	sbarley-old.dai srape-old.dai wbarley-old.dai wrape-old.dai \
	wwheat-old.dai 
NEWCROPS = maize.dai pea.dai rye.dai sbarley.dai sbeet.dai srape.dai \
	swheat.dai wbarley.dai wrape.dai wwheat.dai grass2.dai

DAISY = crop-old.dai crop.dai fertilizer.dai tillage.dai log.dai \
	$(OLDCROPS) $(NEWCROPS)
TEXT	= Makefile test.dai
CLIMATE = weather.clm
REMOVE = grass.dai

all:
	echo Nothing to do here.

# Create a ZIP file with all the sources.
#
daisy-lib.zip:	$(TEXT)
	rm -f daisy-lib.zip
	zip daisy-lib.zip $(DAISY) $(CLIMATE) $(TEXT)

# Move it to ftp.
#
dist:
	$(MAKE) daisy-lib.zip
	mv -f daisy-lib.zip $(HOME)/.public_ftp/daisy/

# Update the CVS repository.
#
cvs: $(DAISY) $(TEXT)
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	-cvs add $(DAISY) $(TEXT)
	rm -f $(REMOVE) 
	-cvs remove $(REMOVE) 
	cvs commit -m "$(TAG)" # "Version $(TAG)"

