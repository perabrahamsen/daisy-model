DAISY = beet-old.dai crop-old.dai fertilizer.dai grass-old.dai \
	pea-old.dai potato-old.dai sbarley-old.dai sbarley.dai \
	srape-old.dai tillage.dai wbarley-old.dai wrape-old.dai \
	wwheat-old.dai crop.dai wwheat.dai grass.dai log.dai
TEXT	= Makefile test.dai
CLIMATE = weather.clm
REMOVE = none

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

