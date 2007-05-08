TEXTDATA = Taastrup6201.dwf Exercise01.dai PioneerMaize.dai

BINDATA = AArslevWeatherDat.xls MaizeDat.xls

DATA = $(TEXTDATA) $(BINDATA)

ANSWERS = answers/Aarslev.dwf answers/Exercise01.dai answers/Exercise02.dai \
	answers/Exercise03.dai answers/Exercise04.dai answers/Exercise05.dai \
	answers/Exercise06.dai answers/Exercise07.dai answers/Exercise08.dai \
	answers/Exercise09.dai answers/Exercise10.dai answers/Exercise11.dai \
	answers/Exercise12.dai 

TEXT = $(TEXTDATA) $(ANSWERS) Makefile

../txt/exercises.pdf:
	(cd ../txt && make exercises.pdf)

exercises.pdf: ../txt/exercises.pdf
	cp -f ../txt/exercises.pdf .

exercises.zip:  exercises.pdf $(DATA) $(ANSWERS)
	rm -f exercises.zip
	zip exercises.zip  exercises.pdf $(DATA) $(ANSWERS)

dist: exercises.zip
	cp -f exercises.zip $(FTPDIR)/

setup: exercises.pdf $(DATA) $(ANSWERS)
	mkdir $(SETUPDIR)/exercises
	cp -f exercises.pdf $(DATA) $(SETUPDIR)/exercises
	mkdir $(SETUPDIR)/exercises/answers
	cp -f $(ANSWERS) $(SETUPDIR)/exercises/answers

cvs: $(TEXT)
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	-cvs add $(TEXT)
	rm -f $(REMOVE) 
	-cvs remove $(REMOVE) 
	cvs commit -m "$(TAG)" # "Version $(TAG)"
