DAISY 	= dk-management
TEXT	= Makefile README
REMOVE 	=  NOTHING

all:
	echo Nothing to do here.

# Create a ZIP file with all the sources.
#
daisy-sample.zip:	$(DAISY) $(TEXT)
	rm -f daisy-sample.zip
	zip daisy-sample.zip $(DAISY) $(TEXT)

# Move it to ftp.
#
dist:
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	$(MAKE) daisy-sample.zip
	mv -f daisy-sample.zip $(FTPDIR)/daisy-sample/daisy-sample-$(TAG).zip
	rm -f $(FTPDIR)/daisy-sample.zip
	(cd $(FTPDIR); ln -s daisy-sample/daisy-sample-$(TAG).zip daisy-sample.zip)

# Update the CVS repository.
#
cvs: $(DAISY) $(TEXT) $(MSHE) $(CLIMATE)
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	-cvs add $(DAISY) $(TEXT) $(MSHE) $(CLIMATE)
	rm -f $(REMOVE) 
	-cvs remove $(REMOVE) 
	cvs commit -m "$(TAG)" # "Version $(TAG)"

