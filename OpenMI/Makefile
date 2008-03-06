CSHARP = /cygdrive/C/WINDOWS/Microsoft.NET/Framework/v2.0.50727/csc.exe

OPENMISRC = daisywrapper.cs daisyOpenMIcomponent.cs 
DOTNETSRC = daisyAccess.cs 
SOURCES = $(OPENMISRC) $(DOTNETSRC)

TARGETS = daisy_dotnet.dll daisy_OpenMI.dll
TEXT = $(SOURCES) Makefile

REMOVE = daisyDotNetAccess.cs

help:
	$(CSHARP) /help

all:	$(TARGETS)

setup: $(TARGETS)
	cp $(TARGETS) $(SETUPDIR)/bin

.IGNORE: svnci

svnci: $(TEXT)
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	svn update
	-svn add $(TEXT)
	rm -f $(REMOVE) 
	-svn remove $(REMOVE) 
	svn commit -m "$(TAG)" # "Version $(TAG)"

commit: $(TEXT)
	svn update
	-svn add $(TEXT)
	rm -f $(REMOVE) 
	-svn remove $(REMOVE) 
	svn commit -m "commit" # "Version $(TAG)"

daisy_dotnet.dll: $(DOTNETSRC)
	$(CSHARP) /target:library /out:$@ $<

OPENMIASS = C:\Program Files\OpenMI 1.2.0\OpenMIAssemblyFolder
daisy_OpenMI.dll: $(OPENMISRC)
	$(CSHARP) /target:library /out:$@ /reference:daisy_dotnet.dll \
	"/reference:$(OPENMIASS)\org.OpenMI.Standard.dll" \
	"/reference:$(OPENMIASS)\org.OpenMI.Backbone.dll" \
	"/reference:$(OPENMIASS)\org.OpenMI.Utilities.Wrapper.dll" \
	"/reference:$(OPENMIASS)\org.OpenMI.DevelopmentSupport.dll" \
	daisywrapper.cs daisyOpenMIcomponent.cs 

.cs.netmodule:
	$(CSHARP) /target:module $<
