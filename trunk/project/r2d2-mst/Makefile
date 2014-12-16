TEXT = Makefile
REMOVE = none

all:
	echo Please be specific

svnci: $(TEXT)
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	(cd agrovand; $(MAKE) svnci);
	-svn add $(TEXT) 
	rm -f $(REMOVE) 
	-svn remove $(REMOVE) 
	svn commit -m "$(TAG)" # "Version $(TAG)"

add:
	(cd agrovand && make add)
	svn add $(TEXT)
	-svn remove $(REMOVE)
