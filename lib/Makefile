DAISY = beet-old.dai crop-old.dai fertilizer.dai grass-old.dai \
	pea-old.dai potato-old.dai sbarley-old.dai sbarley.dai \
	srape-old.dai tillage.dai wbarley-old.dai wrape-old.dai \
	wwheat-old.dai 
CLIMATE = weather.clm
REMOVE = none

all:
	echo Nothing to do here.

cvs: $(DAISY)
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	-cvs add $(DAISY)
	rm -f $(REMOVE) 
	-cvs remove $(REMOVE) 
	cvs commit -m "$(TAG)" # "Version $(TAG)"

