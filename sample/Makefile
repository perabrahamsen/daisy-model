LIBS 	= pedo-soil.dai irrigation.dai dk-management.dai
SETUPS  = sample.dai
TEXT	= Makefile README $(SETUPS) $(LIBS)
REMOVE 	=  NOTHING

DAISY   = /usr/local/daisy/$(HOSTTYPE)/daisy

all:
	echo Nothing to do here.

# Create a ZIP file with all the sources.
#
daisy-sample.zip:	$(TEXT)
	rm -f daisy-sample.zip
	zip daisy-sample.zip $(TEXT)

# Move it to ftp.
#
dist:
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	$(MAKE) daisy-sample.zip
	mv -f daisy-sample.zip $(FTPDIR)/daisy-lib/daisy-sample-$(TAG).zip
	rm -f $(FTPDIR)/daisy-sample.zip
	(cd $(FTPDIR); ln -s daisy-lib/daisy-sample-$(TAG).zip daisy-sample.zip)

# Update the CVS repository.
#
cvs: $(TEXT) 
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	-cvs add $(TEXT) 
	rm -f $(REMOVE) 
	-cvs remove $(REMOVE) 
	cvs commit -m "$(TAG)" # "Version $(TAG)"

# Check
#
check: new/Cosby-Nitrogen-Balance.sum
	-@if cmp new/Cosby-Nitrogen-Balance.sum old/Cosby-Nitrogen-Balance.sum ; then echo Cosby Nitrogen Balance unchanged; else echo Cosby Nitrogen Balance changed; wdiff new/Cosby-Nitrogen-Balance.sum old/Cosby-Nitrogen-Balance.sum; fi

new/Cosby-Nitrogen-Balance.sum: $(DAISY) test-Cosby.dai
	$(DAISY) test-Cosby.dai

accept: 
	cp new/Cosby-Nitrogen-Balance.sum old
