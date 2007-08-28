CSHARP = /cygdrive/C/WINDOWS/Microsoft.NET/Framework/v2.0.50727/csc.exe

SOURCES = daisyAccess.cs \
	daisyDotNetAccess.cs daisyOpenMIcomponent.cs daisywrapper.cs

TARGETS = daisy_dotnet.dll daisy_OpenMI.dll
TEXT = $(SOURCES) Makefile

REMOVE = none

help:
	$(CSHARP) /help

all:	$(TARGETS)

setup: $(TARGETS)
	cp $(TARGETS) $(SETUPDIR)/bin

.IGNORE: cvs

cvs: $(TEXT)
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	cvs update
	-cvs add $(TEXT)
	rm -f $(REMOVE) 
	-cvs remove $(REMOVE) 
	cvs commit -m "$(TAG)" # "Version $(TAG)"

commit: $(TEXT)
	cvs update
	-cvs add $(TEXT)
	rm -f $(REMOVE) 
	-cvs remove $(REMOVE) 
	cvs commit -m "commit" # "Version $(TAG)"

daisy_dotnet.dll: daisyAccess.cs
	$(CSHARP) /target:library /out:$@ $<

daisy_OpenMI.dll: daisyDotNetAccess.cs daisyOpenMIcomponent.cs daisywrapper.cs
	$(CSHARP) /target:library /out:$@ /reference:daisy_dotnet.dll $<

.cs.netmodule:
	$(CSHARP) /target:module $<
