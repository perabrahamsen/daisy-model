CSHARP = /cygdrive/C/WINDOWS/Microsoft.NET/Framework/v2.0.50727/csc.exe

OPENMISRC = daisywrapper.cs 
DOTNETSRC = daisyAccess.cs 
SOURCES = $(OPENMISRC) $(DOTNETSRC)

TARGETS = daisy_dotnet.dll daisy_OpenMI.dll
TEXT = $(SOURCES) Makefile

REMOVE = daisyDotNetAccess.cs daisyOpenMIcomponent.cs 

OPENMIDLL = OpenMI.Standard.dll Oatc.OpenMI.Sdk.Backbone.dll \
	Oatc.OpenMI.Sdk.Wrapper.dll Oatc.OpenMI.Sdk.DevelopmentSupport.dll \
	Oatc.OpenMI.Sdk.Buffer.dll

OPENMIASS = ..\libdeps32

SETUPDIR = $(HOME)/daisy/install

help:
	$(CSHARP) /help

all:	$(TARGETS)

.PHONY: install
install: $(TARGETS)
	cp $(TARGETS) $(SETUPDIR)/bin
	( cd "$(OPENMIASS)" && cp $(OPENMIDLL) $(SETUPDIR)/bin )

setup: install
	mkdir $(SETUPDIR)/src/OpenMI
	cp $(TEXT) $(SETUPDIR)/src/OpenMI

.IGNORE: checkin

checkin: $(TEXT)
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	git pull
	-git add $(TEXT)
	rm -f $(REMOVE) 
	-git rm $(REMOVE) 
	git commit -a -m "$(TAG)" # "Version $(TAG)"
	git push origin

commit: $(TEXT)
	git pull
	-git add $(TEXT)
	rm -f $(REMOVE) 
	-git rm $(REMOVE) 
	git commit -a -m "$(TAG)" # "Version $(TAG)"

daisy_dotnet.dll: $(DOTNETSRC)
	$(CSHARP) /target:library /out:$@ $<

daisy_OpenMI.dll: $(OPENMISRC) daisy_dotnet.dll
	$(CSHARP) /target:library /out:$@ /reference:daisy_dotnet.dll \
	"/reference:$(OPENMIASS)\OpenMI.Standard.dll" \
	"/reference:$(OPENMIASS)\Oatc.OpenMI.Sdk.Backbone.dll" \
	"/reference:$(OPENMIASS)\Oatc.OpenMI.Sdk.Wrapper.dll" \
	"/reference:$(OPENMIASS)\Oatc.OpenMI.Sdk.DevelopmentSupport.dll" \
	$(OPENMISRC)

.cs.netmodule:
	$(CSHARP) /target:module $<
