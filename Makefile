# Makefile --- For maintaining the Daisy project.
#
# Automatic creation of daisy on multiple platforms.
#
# You need GNU Make for using this file.
#
# The following envirnoment variables are used:
#
# HOSTTYPE
#	sun4	Create code for Solaris-2 / UltraSPARC.
#	hp	Create code for HP/UX / HP-PA.
#	win32	Create code for Win32 / Pentium.
#	cygwin	Create code for Cygwin / Pentium.
#	mingw	Create code for Mingw / Pentium.

# All makefiles should have these.
#
SHELL = /bin/sh
MAKEFLAGS =

# HOSTTYPE is not defined in the native win32 Emacs.
#
ifeq ($(OS),Windows_NT)
	ifeq ($(OSTYPE),cygwin)
#		HOSTTYPE = cygwin
		HOSTTYPE = mingw
	else
		HOSTTYPE = win32
	endif
endif

# Set USE_OPTIMIZE to `true' if you want a fast executable.
#
USE_OPTIMIZE = true
#USE_OPTIMIZE = false

# Set USE_PROFILE if you want to profile the executable
#
#USE_PROFILE = true
USE_PROFILE = false

# Set COMPILER according to which compiler you use.
#	sun		Use the unbundled sun compiler.
#	gcc		Use the experimental GNU compiler.
#	borland		Use the Borland compiler.
#
ifeq ($(HOSTTYPE),sun4)
	COMPILER = gcc
#	COMPILER = sun	
endif
ifeq ($(HOSTTYPE),hp)
	COMPILER = gcc
endif
ifeq ($(HOSTTYPE),win32)
	COMPILER = borland
endif
ifeq ($(HOSTTYPE),cygwin)
	COMPILER = gcc
endif
ifeq ($(HOSTTYPE),mingw)
	COMPILER = gcc
endif

# On SPARC platforms we trap mathematical exception with some assembler code.
#
ifeq ($(HOSTTYPE),sun4) 
	SPARCSRC = set_exceptions.S
	SPARCOBJ = set_exceptions.o
endif

# Find the profile flags.
#
ifeq ($(USE_PROFILE),true)
	ifneq ($(COMPILER),borland)
		PROFILE = -pg
	endif
endif

# Find the optimize flags.
#
ifeq ($(USE_OPTIMIZE),true)
	ifeq ($(COMPILER),gcc)
		OPTIMIZE = -O3 -ffast-math 
		ifeq ($(HOSTTYPE),sun4)
			OPTIMIZE = -O3 -ffast-math -fno-inline
#`-mcpu=ultrasparc' breaks `IM::IM ()' with gcc 2.95.1.
		endif
		ifeq ($(HOSTTYPE),cygwin)
		  OPTIMIZE = -O3 -ffast-math -mcpu=pentiumpro -march=pentium
		endif
		ifeq ($(HOSTTYPE),mingw)
		  OPTIMIZE = -O3 -ffast-math -mcpu=pentiumpro -march=pentium
		endif
	endif
endif

# Create the right compile command.
#

ifeq ($(COMPILER),gcc)
	ifeq ($(HOSTTYPE),sun4)
		OSFLAGS = 
		DEBUG = -g
	endif
	ifeq ($(HOSTTYPE),cygwin)
		OSFLAGS =
		DEBUG = 
	endif
	ifeq ($(HOSTTYPE),mingw)
		OSFLAGS = -DMINGW -mno-cygwin \
		          -I/home/mingw/include -L/home/mingw/lib
		DEBUG =
	endif
	WARNING = -W -Wall -Wno-sign-compare -Wstrict-prototypes \
		   -Wconversion -Wno-uninitialized -Wmissing-prototypes 
	COMPILE = c++ $(WARNING) $(DEBUG) $(OSFLAGS)
	CCOMPILE = gcc -I/pack/f2c/include -g -Wall
endif
ifeq ($(COMPILER),sun)
	COMPILE = /pack/devpro/SUNWspro/bin/CC
	CCOMPILE = gcc -I/pack/f2c/include -g -Wall
endif
ifeq ($(COMPILER),borland)
	COMPILE = /bc5/bin/bcc32 -P -v -wdef -wnod -wamb -w-par -w-hid
	CCOMPILE = /bc5/bin/bcc32 -P- -v -wdef -wnod -wamb -w-par -w-hid
endif

# Construct the compile command.
#
CC = $(COMPILE) $(OPTIMIZE) $(PROFILE)

# Find the rigth math library.
#
ifeq ($(HOSTTYPE),sun4)
	MATHLIB = -lm
endif
ifeq ($(HOSTTYPE),hp)
	MATHLIB = -lM
endif
ifeq ($(HOSTTYPE),win32)
	MATHLIB =
endif
ifeq ($(HOSTTYPE),cygwin)
	MATHLIB =
endif
ifeq ($(HOSTTYPE),mingw)
	MATHLIB =
endif

# Locate the tk library.
#
TKINCLUDE	= -I/pack/tcl+tk-8/include -I/usr/openwin/include
TKLIB	  	= -L/pack/tcl+tk-8/lib -L/usr/openwin/lib \
		  -ltix4.1.8.0 -ltk8.0 -ltcl8.0 -lX11 -lsocket -lnsl -ldl

# Locate the Gtk-- library.
#
GTKMMINCLUDE	= -I$(HOME)/gtk/lib/Gtk--/include \
		  -I$(HOME)/gtk/lib/glib/include \
		  -I$(HOME)/gtk/include -I/usr/openwin/include
GTKMMLIB	= -L$(HOME)/gtk/lib -lgtkmm \
		  -L/usr/openwin/lib -R/usr/openwin/lib \
		  -lgtk -lgdk -lglib -lXext -lX11 -lsocket -lnsl -lm
GTKMMDRAWINCLUDE = -I$(HOME)/gtk/include/gtk--draw \
		   $(GTKMMINCLUDE)
GTKMMDRAWLIB	= -L$(HOME)/gtk/lib -lgtkmmdraw ${GTKMMLIB}

# Find the right file extension.
#
ifeq ($(HOSTTYPE),win32)
	OBJ = .obj
	EXT = .exe
else
	OBJ = .o
	ifeq ($(HOSTTYPE),cygwin)
		EXT = .exe
	else
		ifeq ($(HOSTTYPE),mingw)
			EXT = .exe
		else
			EXT =
		endif
	endif
endif

# Figure out how to link.
#
ifeq ($(COMPILER),borland)
	LINK =    /bc5/bin/tlink32 /v -Tpe -ap -c -x -e
	DLLLINK = /bc5/bin/bcc32 -P- -v -lw-dup -WDE -lTpd -lv -e
	NOLINK = -c
	CRTLIB = C:\BC5\LIB\c0x32.obj
else
	LINK = $(CC) -g -o
	NOLINK = -c
endif

# Select the C files that doesn't have a corresponding header file.
# These are all models of some componet.
#
MODELS = column_inorganic.C  vegetation_permanent.C \
	vegetation_crops.C crop_simple.C action_ridge.C groundwater_fixed.C \
	groundwater_deep.C action_heat.C hydraulic_M_vG_compact.C \
	action_crop.C groundwater_lysimeter.C select_min.C \
	select_max.C select_average.C action_message.C weather_std.C \
	select_flux_top.C select_flux_bottom.C groundwater_pipe.C \
	select_index.C select_content.C select_interval.C select_flux.C \
	select_number.C select_date.C select_array.C log_table.C \
	log_harvest.C action_while.C action_wait.C action_activity.C \
	average_arithmetic.C average_harmonic.C average_geometric.C \
	mactrans_std.C macro_std.C macro_none.C document_LaTeX.C \
	column_std.C  weather_simple.C uzrichard.C \
	hydraulic_yolo.C hydraulic_M_vG.C hydraulic_B_vG.C hydraulic_M_C.C \
	hydraulic_B_C.C hydraulic_M_BaC.C hydraulic_B_BaC.C \
	groundwater_static.C horizon_std.C \
	crop_std.C action_sow.C action_stop.C condition_time.C \
	condition_logic.C action_irrigate.C action_lisp.C \
	weather_none.C action_fertilize.C weather_file.C action_tillage.C \
	action_harvest.C hydraulic_old.C crop_old.C crop_sold.C \
	action_with.C hydraulic_old2.C nitrification_soil.C \
	nitrification_solute.C hydraulic_mod_C.C uzlr.C transport_cd.C \
	transport_none.C transport_convection.C adsorption_vS_S.C \
	adsorption_none.C tortuosity_M_Q.C tortuosity_linear.C \
	adsorption_freundlich.C adsorption_linear.C adsorption_langmuir.C \
	bioclimate_std.C condition_crop.C \
	condition_soil.C log_table1.C log_checkpoint.C weather_hourly.C \
	uznone.C condition_daisy.C chemical_std.C \
	hydraulic_M_BaC_Bimodal.C hydraulic_B_BaC_Bimodal.C \
	pet_makkink.C pet_weather.C svat_none.C action_spray.C pet_PM.C \
	svat_pmsw.C action_merge.C action_divide.C groundwater_file.C \
	action_surface.C

# A component isa common interface to a number of models.
#
COMPONENTS = select.C average.C mactrans.C macro.C \
	document.C parser.C log.C weather.C column.C crop.C \
	action.C condition.C horizon.C 	uzmodel.C hydraulic.C \
	bioclimate.C groundwater.C am.C transport.C \
	adsorption.C tortuosity.C printer.C chemical.C \
	pet.C net_radiation.C svat.C vegetation.C 

# Submodels are combined models and components.
#
SUBMODELS = canopy_simple.C canopy_std.C root_system.C \
	ridge.C soil.C surface.C soil_water.C soil_NH4.C soil_NO3.C \
	organic_matter.C nitrification.C denitrification.C soil_heat.C \
	snow.C im.C om.C harvest.C chemicals.C field.C \
	soil_chemical.C soil_chemicals.C bioincorporation.C


# Special or intermediate models with their own interface.
#
SPECIALS = weather_old.C log_extern.C log_select.C parser_file.C solute.C \
	geometry.C printer_file.C log_alist.C log_clone.C column_base.C

# Various utility code that are neither a component or a submodel.
#
OTHER = lexer_data.C lexer.C daisy.C alist.C syntax.C library.C plf.C \
	time.C mathlib.C librarian.C cdaisy.C common.C nrutil.C \
	submodel.C

# Support for Jeppe and Rino's GUI frontend.
#
# Everything that has an interface.
#
INTERFACES = $(COMPONENTS) $(SUBMODELS) $(SPECIALS) $(OTHER)

# Select the C files that are not part of the library.
#
MAIN = main.C tkmain.C gmain.C

# The object files used in the daisy library.
#
LIBOBJ = $(INTERFACES:.C=${OBJ}) $(MODELS:.C=${OBJ}) $(SPARCOBJ)

# Find all object files, header files, and source files.
#
OBJECTS = $(LIBOBJ) $(MAIN:.C=${OBJ}) cmain${OBJ} bugmain.o
SOURCES = $(INTERFACES) $(MODELS) $(SPARCSRC) $(MAIN) cmain.c bugmain.c
HEADERS = $(INTERFACES:.C=.h) version.h

# Find all printable files.
#
TEXT =  Makefile ChangeLog TODO $(HEADERS) $(SOURCES) tlink32.ini

# The executables.
#
EXECUTABLES = daisy${EXT} tkdaisy${EXT} cdaisy${EXT} gdaisy${EXT}

# Select files to be removed by the next cvs update.
#
REMOVE = pt.C pt.h pt_pmsw.C pt_std.C

# These are the file extensions we deal with.
# 
.SUFFIXES:	.C ${OBJ} .h .c ${EXT} .a

# Create all the executables.
#
all:	$(EXECUTABLES)

# Create the main executable.
#
daisy${EXT}:	main${OBJ} $(FORLIB) $(LIBOBJ) # $(INTERFACES:.C=${OBJ})
	$(LINK)daisy $(CRTLIB) $^ $(MATHLIB)

# Create manager test executable.
#
mandaisy${EXT}:	manmain${OBJ} $(FORLIB) $(LIBOBJ)
	$(LINK)mandaisy $(CRTLIB) $^ $(MATHLIB)

# Create bug test executable.
#
bugdaisy${EXT}:	bugmain${OBJ} $(FORLIB) $(LIBOBJ)
	$(LINK)bugdaisy $(CRTLIB) $^ $(MATHLIB)

# Create executable with embedded tcl/tk.
#
tkdaisy${EXT}:	tkmain${OBJ} $(FORLIB) $(LIBOBJ)
	$(LINK)tkdaisy $^ $(TKLIB) $(MATHLIB)

# Create executable with Gtk--.
#
gdaisy${EXT}:	gmain${OBJ} $(FORLIB) $(LIBOBJ)
	$(LINK)gdaisy $^ $(GTKMMLIB) $(MATHLIB)

# Create the C main executable.
#
cdaisy${EXT}:  cmain${OBJ} $(FORLIB) $(LIBOBJ)
	$(LINK)cdaisy $^ $(MATHLIB)

# Create the C main executable for testing.
#
cdaisy_test${EXT}:  cmain_test${OBJ} $(FORLIB) $(LIBOBJ)
	$(LINK)cdaisy_test $^ $(MATHLIB)

# Create a DLL.
#
daisy.dll:	$(LIBOBJ)
#	/bc5/bin/tlink32 /Tpd /v $^, daisy.dll,, cw32i.lib
	$(DLLLINK)daisy.dll $^ $(MATHLIB)

# Create daisy plot executable.
#
pdaisy${EXT}: pmain${OBJ} time.o
	$(LINK)pdaisy $^ $(GTKMMDRAWLIB) $(MATHLIB)


dlldaisy${EXT}:	cmain${OBJ} daisy.dll
	$(LINK)dlldaisy $^ $(MATHLIB)


# Count the size of daisy.
#
wc: $(HEADERS) $(SOURCES) 
	wc -l $(HEADERS) $(SOURCES) | sort -nr

wc-h: $(HEADERS)
	wc -l $(HEADERS)

wc-s: $(SOURCES)
	wc -l $(SOURCES)

wc-split: $(MODELS) $(INTERFACES)
	cat $(MODELS) | wc
	cat $(COMPONENTS) $(COMPONENTS:.C=.h) | wc
	cat $(SUBMODELS) $(SUBMODELS:.C=.h) | wc 
	cat $(SPECIALS) $(SPECIALS:.C=.h) | wc
	cat $(OTHER) $(OTHER:.C=.h) | wc

filecount: $(HEADERS) $(SOURCES) 
	ls $(TEXT) | wc

# Update the TAGS table.
#
tags: TAGS

TAGS: $(SOURCES) $(HEADERS)
	etags $(SOURCES) $(HEADERS)

# Fix DOS newline breakage.
#
dos2unix:
	perl -pi.bak -e 's/\r\n$$/\n/' $(TEXT)

# Print the current syntax for the Daisy input language.
#
dump:	daisy
	daisy -p

# Various test targets.
#
xtest:	test/test.dai daisy
	(cd test \
         && ../daisy test.dai \
	 && diff -u harvest_weather.log harvest.log)

ps:	txt/reference/reference.ps


txt/reference/reference.ps:	txt/reference/reference.dvi
	(cd txt/reference \
	 && dvips -f reference.dvi > reference.ps)

txt/reference/reference.dvi:	txt/reference/components.tex
	(cd txt/reference \
	 && latex reference.tex < /dev/null )

pdf:	txt/reference/reference.pdf

txt/reference/reference.pdf:	txt/reference/components.tex
	(cd txt/reference \
	 && pdflatex reference.tex < /dev/null )

txt/reference/components.tex:	daisy
	(cd txt/reference \
	 && ../../daisy -p LaTeX > components.tex)

# Remove all the temporary files.
#
clean:
	rm $(OBJECTS) *.rpo $(EXECUTABLES) *.obj *.exe *.o *~

# Update the Makefile when dependencies have changed.
#
depend: $(SOURCES) 
	rm -f Makefile.old
	mv Makefile Makefile.old
	sed -e '/^# AUTOMATIC/q' < Makefile.old > Makefile
	c++ -I. $(TKINCLUDE) $(GTKMMINCLUDE) \
	        -MM $(SOURCES) | sed -e 's/\.o:/$${OBJ}:/' >> Makefile

# Create a ZIP file with all the sources.
#
daisy-src.zip:	$(TEXT)
	rm -f daisy-src.zip
	zip daisy-src.zip $(TEXT) daisy.ide

# Move it to ftp.
#
dist:	cvs
	cp cdaisy.h cmain.c $(HOME)/.public_ftp/daisy
	$(MAKE) daisy-src.zip
	mv -f daisy-src.zip $(HOME)/.public_ftp/daisy
	(cd lib; $(MAKE) dist)
	$(MAKE) pdf
	mv -f txt/reference/reference.pdf \
		$(HOME)/.public_ftp/daisy/daisy-ref.pdf
	$(MAKE) ps
	mv -f txt/reference/reference.ps \
		$(HOME)/.public_ftp/daisy/daisy-ref.ps


# Update the CVS repository.
#
cvs: $(TEXT)
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	rm -f version.h
	echo "// version.h -- automatically generated file" > version.h
	echo " " >> version.h
	echo "static const char *const version = \"$(TAG)\";" >> version.h
	mv ChangeLog ChangeLog.old
	echo `date "+%Y-%m-%d "` \
	     " Per Abrahamsen  <abraham@dina.kvl.dk>" > ChangeLog
	echo >> ChangeLog
	echo "	* Version" $(TAG) released. >> ChangeLog
	echo >> ChangeLog
	cat ChangeLog.old >> ChangeLog
	(cd lib; $(MAKE) cvs);
	-cvs add $(TEXT)
	rm -f $(REMOVE) 
	-cvs remove $(REMOVE) 
	cvs commit -m "Version $(TAG)"
	cvs tag release_`echo $(TAG) | sed -e 's/[.]/_/g'`

# How to compile the assembler file.
#
set_exceptions${OBJ}: set_exceptions.S
	as -o set_exceptions${OBJ} set_exceptions.S

# How to compile a C++ file.
#
.C${OBJ}:
	$(CC) $(NOLINK) $<

# How to compile a C file.
#
.c${OBJ}:
	$(CCOMPILE) $(OPTIMIZE) $(PROFILE) $(NOLINK) $<

# There is a bug when gcc compile snow.C with optimization.
#
ifeq ($(COMPILER),gcc)
ifeq ($(USE_OPTIMIZE),true)

snow.o: snow.C
	$(COMPILE) $(PROFILE) $(NOLINK) $<

endif
endif

# Special rule for tkmain.o
#
tkmain${OBJ}: tkmain.C
	$(CC) $(TKINCLUDE) $(NOLINK) $<

# Special rule for gmain.o
#
gmain${OBJ}: gmain.C
	$(CC) $(GTKMMINCLUDE) $(NOLINK) $<

# Special rule for pmain.o
#
pmain${OBJ}: pmain.C
	$(CC) $(GTKMMDRAWINCLUDE) $(NOLINK) $<

############################################################
# AUTOMATIC -- DO NOT CHANGE THIS LINE OR ANYTHING BELOW IT!
select${OBJ}: select.C select.h condition.h librarian.h library.h common.h \
 alist.h syntax.h geometry.h
average${OBJ}: average.C average.h librarian.h library.h common.h alist.h \
 syntax.h
mactrans${OBJ}: mactrans.C mactrans.h librarian.h library.h common.h \
 alist.h syntax.h
macro${OBJ}: macro.C macro.h librarian.h library.h common.h alist.h \
 syntax.h
document${OBJ}: document.C document.h librarian.h library.h common.h \
 alist.h syntax.h submodel.h
parser${OBJ}: parser.C parser.h librarian.h library.h common.h alist.h \
 syntax.h
log${OBJ}: log.C log.h librarian.h library.h common.h alist.h syntax.h
weather${OBJ}: weather.C weather.h librarian.h library.h common.h alist.h \
 syntax.h im.h net_radiation.h log.h mathlib.h
column${OBJ}: column.C column.h librarian.h library.h common.h alist.h \
 syntax.h log.h
crop${OBJ}: crop.C crop.h time.h librarian.h library.h common.h alist.h \
 syntax.h chemicals.h om.h
action${OBJ}: action.C action.h librarian.h library.h common.h alist.h \
 syntax.h
condition${OBJ}: condition.C condition.h librarian.h library.h common.h \
 alist.h syntax.h
horizon${OBJ}: horizon.C horizon.h librarian.h library.h common.h alist.h \
 syntax.h plf.h hydraulic.h mathlib.h tortuosity.h log.h
uzmodel${OBJ}: uzmodel.C uzmodel.h librarian.h library.h common.h alist.h \
 syntax.h
hydraulic${OBJ}: hydraulic.C hydraulic.h librarian.h library.h common.h \
 alist.h syntax.h plf.h log.h
bioclimate${OBJ}: bioclimate.C bioclimate.h librarian.h library.h common.h \
 alist.h syntax.h weather.h im.h
groundwater${OBJ}: groundwater.C groundwater.h uzmodel.h librarian.h \
 library.h common.h alist.h syntax.h log.h
am${OBJ}: am.C am.h librarian.h library.h common.h alist.h syntax.h om.h \
 im.h log.h geometry.h mathlib.h
transport${OBJ}: transport.C transport.h librarian.h library.h common.h \
 alist.h syntax.h
adsorption${OBJ}: adsorption.C adsorption.h librarian.h library.h common.h \
 alist.h syntax.h
tortuosity${OBJ}: tortuosity.C tortuosity.h librarian.h library.h common.h \
 alist.h syntax.h
printer${OBJ}: printer.C printer.h librarian.h library.h common.h alist.h \
 syntax.h
chemical${OBJ}: chemical.C chemical.h librarian.h library.h common.h \
 alist.h syntax.h
pet${OBJ}: pet.C pet.h librarian.h library.h common.h alist.h syntax.h \
 log.h vegetation.h surface.h uzmodel.h
net_radiation${OBJ}: net_radiation.C net_radiation.h librarian.h library.h \
 common.h alist.h syntax.h log.h weather.h im.h
svat${OBJ}: svat.C svat.h librarian.h library.h common.h alist.h syntax.h \
 log.h
vegetation${OBJ}: vegetation.C vegetation.h librarian.h library.h common.h \
 alist.h syntax.h log.h
canopy_simple${OBJ}: canopy_simple.C canopy_simple.h plf.h submodel.h \
 log.h librarian.h library.h common.h alist.h syntax.h
canopy_std${OBJ}: canopy_std.C canopy_std.h canopy_simple.h plf.h \
 submodel.h log.h librarian.h library.h common.h alist.h syntax.h \
 mathlib.h
root_system${OBJ}: root_system.C root_system.h submodel.h soil_heat.h \
 soil_NH4.h solute.h adsorption.h librarian.h library.h common.h \
 alist.h syntax.h transport.h mactrans.h soil_NO3.h soil_water.h \
 macro.h soil.h horizon.h hydraulic.h tortuosity.h geometry.h log.h
ridge${OBJ}: ridge.C ridge.h soil.h horizon.h librarian.h library.h \
 common.h alist.h syntax.h hydraulic.h tortuosity.h geometry.h plf.h \
 submodel.h mathlib.h log.h soil_water.h macro.h
soil${OBJ}: soil.C soil.h horizon.h librarian.h library.h common.h alist.h \
 syntax.h hydraulic.h tortuosity.h geometry.h mathlib.h submodel.h \
 log.h
surface${OBJ}: surface.C surface.h uzmodel.h librarian.h library.h \
 common.h alist.h syntax.h soil_water.h macro.h soil.h horizon.h \
 hydraulic.h tortuosity.h geometry.h log.h am.h im.h mathlib.h \
 submodel.h chemicals.h soil_chemicals.h plf.h ridge.h
soil_water${OBJ}: soil_water.C soil_water.h macro.h librarian.h library.h \
 common.h alist.h syntax.h log.h uzmodel.h soil.h horizon.h \
 hydraulic.h tortuosity.h geometry.h surface.h groundwater.h mathlib.h \
 submodel.h
soil_NH4${OBJ}: soil_NH4.C soil_NH4.h solute.h adsorption.h librarian.h \
 library.h common.h alist.h syntax.h transport.h mactrans.h submodel.h
soil_NO3${OBJ}: soil_NO3.C soil_NO3.h solute.h adsorption.h librarian.h \
 library.h common.h alist.h syntax.h transport.h mactrans.h submodel.h
organic_matter${OBJ}: organic_matter.C organic_matter.h common.h syntax.h \
 alist.h log.h librarian.h library.h am.h om.h soil.h horizon.h \
 hydraulic.h tortuosity.h geometry.h soil_water.h macro.h soil_NH4.h \
 solute.h adsorption.h transport.h mactrans.h soil_NO3.h soil_heat.h \
 bioincorporation.h mathlib.h plf.h submodel.h
nitrification${OBJ}: nitrification.C nitrification.h librarian.h library.h \
 common.h alist.h syntax.h
denitrification${OBJ}: denitrification.C denitrification.h common.h plf.h \
 alist.h syntax.h soil.h horizon.h librarian.h library.h hydraulic.h \
 tortuosity.h geometry.h soil_water.h macro.h soil_heat.h \
 organic_matter.h soil_NO3.h solute.h adsorption.h transport.h \
 mactrans.h log.h submodel.h
soil_heat${OBJ}: soil_heat.C soil_heat.h alist.h common.h surface.h \
 uzmodel.h librarian.h library.h syntax.h weather.h im.h soil_water.h \
 macro.h soil.h horizon.h hydraulic.h tortuosity.h geometry.h \
 mathlib.h log.h submodel.h
snow${OBJ}: snow.C snow.h alist.h common.h syntax.h log.h librarian.h \
 library.h soil.h horizon.h hydraulic.h tortuosity.h geometry.h \
 soil_water.h macro.h soil_heat.h submodel.h mathlib.h
im${OBJ}: im.C im.h log.h librarian.h library.h common.h alist.h syntax.h \
 submodel.h
om${OBJ}: om.C om.h common.h syntax.h alist.h geometry.h log.h librarian.h \
 library.h mathlib.h submodel.h
harvest${OBJ}: harvest.C harvest.h chemicals.h syntax.h common.h log.h \
 librarian.h library.h alist.h submodel.h
chemicals${OBJ}: chemicals.C chemicals.h syntax.h common.h log.h \
 librarian.h library.h alist.h chemical.h submodel.h
field${OBJ}: field.C field.h common.h column.h librarian.h library.h \
 alist.h syntax.h log.h log_clone.h log_alist.h
soil_chemical${OBJ}: soil_chemical.C soil_chemical.h solute.h adsorption.h \
 librarian.h library.h common.h alist.h syntax.h transport.h \
 mactrans.h plf.h chemical.h soil.h horizon.h hydraulic.h tortuosity.h \
 geometry.h soil_heat.h soil_water.h macro.h organic_matter.h log.h \
 submodel.h
soil_chemicals${OBJ}: soil_chemicals.C soil_chemicals.h soil.h horizon.h \
 librarian.h library.h common.h alist.h syntax.h hydraulic.h \
 tortuosity.h geometry.h soil_water.h macro.h soil_heat.h \
 organic_matter.h chemical.h chemicals.h log.h soil_chemical.h \
 solute.h adsorption.h transport.h mactrans.h plf.h submodel.h
bioincorporation${OBJ}: bioincorporation.C bioincorporation.h common.h \
 alist.h syntax.h log.h librarian.h library.h soil.h horizon.h \
 hydraulic.h tortuosity.h geometry.h am.h submodel.h plf.h om.h \
 mathlib.h
weather_old${OBJ}: weather_old.C weather_old.h weather.h librarian.h \
 library.h common.h alist.h syntax.h im.h
log_extern${OBJ}: log_extern.C log_select.h log.h librarian.h library.h \
 common.h alist.h syntax.h select.h condition.h log_extern.h
log_select${OBJ}: log_select.C log_select.h log.h librarian.h library.h \
 common.h alist.h syntax.h select.h condition.h
parser_file${OBJ}: parser_file.C parser_file.h parser.h librarian.h \
 library.h common.h alist.h syntax.h lexer.h plf.h log.h
solute${OBJ}: solute.C solute.h adsorption.h librarian.h library.h \
 common.h alist.h syntax.h transport.h mactrans.h log.h soil.h \
 horizon.h hydraulic.h tortuosity.h geometry.h soil_water.h macro.h \
 mathlib.h
geometry${OBJ}: geometry.C geometry.h common.h syntax.h alist.h mathlib.h
printer_file${OBJ}: printer_file.C printer_file.h printer.h librarian.h \
 library.h common.h alist.h syntax.h plf.h
log_alist${OBJ}: log_alist.C log_alist.h log.h librarian.h library.h \
 common.h alist.h syntax.h
log_clone${OBJ}: log_clone.C log_clone.h log_alist.h log.h librarian.h \
 library.h common.h alist.h syntax.h
column_base${OBJ}: column_base.C column_base.h column.h librarian.h \
 library.h common.h alist.h syntax.h bioclimate.h surface.h uzmodel.h \
 soil.h horizon.h hydraulic.h tortuosity.h geometry.h soil_water.h \
 macro.h soil_heat.h soil_NH4.h solute.h adsorption.h transport.h \
 mactrans.h soil_NO3.h soil_chemicals.h organic_matter.h \
 nitrification.h denitrification.h plf.h groundwater.h log.h im.h am.h \
 weather.h vegetation.h
lexer_data${OBJ}: lexer_data.C lexer_data.h lexer.h common.h
lexer${OBJ}: lexer.C lexer.h common.h
daisy${OBJ}: daisy.C daisy.h time.h weather.h librarian.h library.h \
 common.h alist.h syntax.h im.h groundwater.h uzmodel.h horizon.h \
 log.h parser.h am.h nitrification.h bioclimate.h hydraulic.h field.h \
 harvest.h chemicals.h action.h condition.h column.h submodel.h
alist${OBJ}: alist.C plf.h library.h common.h alist.h syntax.h
syntax${OBJ}: syntax.C syntax.h common.h alist.h library.h
library${OBJ}: library.C library.h common.h alist.h syntax.h
plf${OBJ}: plf.C plf.h
time${OBJ}: time.C time.h
mathlib${OBJ}: mathlib.C mathlib.h common.h
librarian${OBJ}: librarian.C librarian.h library.h common.h alist.h \
 syntax.h
cdaisy${OBJ}: cdaisy.C syntax.h common.h alist.h daisy.h parser_file.h \
 parser.h librarian.h library.h field.h column.h weather.h im.h \
 action.h horizon.h printer_file.h printer.h version.h chemical.h \
 log_extern.h
common${OBJ}: common.C common.h parser_file.h parser.h librarian.h \
 library.h alist.h syntax.h document.h version.h
nrutil${OBJ}: nrutil.C
submodel${OBJ}: submodel.C submodel.h common.h
column_inorganic${OBJ}: column_inorganic.C column_base.h column.h \
 librarian.h library.h common.h alist.h syntax.h bioclimate.h \
 surface.h uzmodel.h soil.h horizon.h hydraulic.h tortuosity.h \
 geometry.h soil_water.h macro.h soil_heat.h soil_NH4.h solute.h \
 adsorption.h transport.h mactrans.h soil_NO3.h soil_chemicals.h \
 organic_matter.h nitrification.h denitrification.h plf.h \
 groundwater.h log.h im.h am.h weather.h vegetation.h
vegetation_permanent${OBJ}: vegetation_permanent.C vegetation.h \
 librarian.h library.h common.h alist.h syntax.h plf.h mathlib.h log.h \
 root_system.h canopy_simple.h soil.h horizon.h hydraulic.h \
 tortuosity.h geometry.h crop.h am.h om.h organic_matter.h
vegetation_crops${OBJ}: vegetation_crops.C vegetation.h librarian.h \
 library.h common.h alist.h syntax.h crop.h soil.h horizon.h \
 hydraulic.h tortuosity.h geometry.h plf.h mathlib.h harvest.h \
 chemicals.h log.h
crop_simple${OBJ}: crop_simple.C crop.h time.h librarian.h library.h \
 common.h alist.h syntax.h root_system.h canopy_simple.h plf.h log.h \
 bioclimate.h soil_water.h macro.h soil.h horizon.h hydraulic.h \
 tortuosity.h geometry.h om.h organic_matter.h soil_heat.h soil_NH4.h \
 solute.h adsorption.h transport.h mactrans.h soil_NO3.h am.h \
 harvest.h chemicals.h mathlib.h
action_ridge${OBJ}: action_ridge.C action.h librarian.h library.h common.h \
 alist.h syntax.h daisy.h field.h ridge.h
groundwater_fixed${OBJ}: groundwater_fixed.C groundwater.h uzmodel.h \
 librarian.h library.h common.h alist.h syntax.h
groundwater_deep${OBJ}: groundwater_deep.C groundwater.h uzmodel.h \
 librarian.h library.h common.h alist.h syntax.h
action_heat${OBJ}: action_heat.C action.h librarian.h library.h common.h \
 alist.h syntax.h daisy.h field.h
hydraulic_M_vG_compact${OBJ}: hydraulic_M_vG_compact.C hydraulic.h \
 librarian.h library.h common.h alist.h syntax.h plf.h
action_crop${OBJ}: action_crop.C action.h librarian.h library.h common.h \
 alist.h syntax.h daisy.h field.h crop.h am.h log.h harvest.h \
 chemicals.h im.h
groundwater_lysimeter${OBJ}: groundwater_lysimeter.C groundwater.h \
 uzmodel.h librarian.h library.h common.h alist.h syntax.h
select_min${OBJ}: select_min.C select.h condition.h librarian.h library.h \
 common.h alist.h syntax.h
select_max${OBJ}: select_max.C select.h condition.h librarian.h library.h \
 common.h alist.h syntax.h
select_average${OBJ}: select_average.C select.h condition.h librarian.h \
 library.h common.h alist.h syntax.h
action_message${OBJ}: action_message.C action.h librarian.h library.h \
 common.h alist.h syntax.h condition.h log.h daisy.h
weather_std${OBJ}: weather_std.C weather.h librarian.h library.h common.h \
 alist.h syntax.h im.h lexer_data.h lexer.h mathlib.h
select_flux_top${OBJ}: select_flux_top.C select.h condition.h librarian.h \
 library.h common.h alist.h syntax.h geometry.h
select_flux_bottom${OBJ}: select_flux_bottom.C select.h condition.h \
 librarian.h library.h common.h alist.h syntax.h geometry.h
groundwater_pipe${OBJ}: groundwater_pipe.C groundwater.h uzmodel.h \
 librarian.h library.h common.h alist.h syntax.h log.h soil.h \
 horizon.h hydraulic.h tortuosity.h geometry.h mathlib.h
select_index${OBJ}: select_index.C select.h condition.h librarian.h \
 library.h common.h alist.h syntax.h
select_content${OBJ}: select_content.C select.h condition.h librarian.h \
 library.h common.h alist.h syntax.h geometry.h
select_interval${OBJ}: select_interval.C select.h condition.h librarian.h \
 library.h common.h alist.h syntax.h geometry.h
select_flux${OBJ}: select_flux.C select.h condition.h librarian.h \
 library.h common.h alist.h syntax.h geometry.h
select_number${OBJ}: select_number.C select.h condition.h librarian.h \
 library.h common.h alist.h syntax.h
select_date${OBJ}: select_date.C select.h condition.h librarian.h \
 library.h common.h alist.h syntax.h
select_array${OBJ}: select_array.C select.h condition.h librarian.h \
 library.h common.h alist.h syntax.h
log_table${OBJ}: log_table.C log_select.h log.h librarian.h library.h \
 common.h alist.h syntax.h select.h condition.h geometry.h version.h
log_harvest${OBJ}: log_harvest.C log.h librarian.h library.h common.h \
 alist.h syntax.h daisy.h harvest.h chemicals.h version.h
action_while${OBJ}: action_while.C action.h librarian.h library.h common.h \
 alist.h syntax.h log.h
action_wait${OBJ}: action_wait.C action.h librarian.h library.h common.h \
 alist.h syntax.h condition.h log.h daisy.h
action_activity${OBJ}: action_activity.C action.h librarian.h library.h \
 common.h alist.h syntax.h log.h
average_arithmetic${OBJ}: average_arithmetic.C average.h librarian.h \
 library.h common.h alist.h syntax.h
average_harmonic${OBJ}: average_harmonic.C average.h librarian.h library.h \
 common.h alist.h syntax.h
average_geometric${OBJ}: average_geometric.C average.h librarian.h \
 library.h common.h alist.h syntax.h
mactrans_std${OBJ}: mactrans_std.C mactrans.h librarian.h library.h \
 common.h alist.h syntax.h soil_water.h macro.h soil.h horizon.h \
 hydraulic.h tortuosity.h geometry.h plf.h mathlib.h
macro_std${OBJ}: macro_std.C macro.h librarian.h library.h common.h \
 alist.h syntax.h soil.h horizon.h hydraulic.h tortuosity.h geometry.h \
 plf.h mathlib.h log.h uzmodel.h
macro_none${OBJ}: macro_none.C macro.h librarian.h library.h common.h \
 alist.h syntax.h
document_LaTeX${OBJ}: document_LaTeX.C document.h librarian.h library.h \
 common.h alist.h syntax.h version.h
column_std${OBJ}: column_std.C column_base.h column.h librarian.h \
 library.h common.h alist.h syntax.h bioclimate.h surface.h uzmodel.h \
 soil.h horizon.h hydraulic.h tortuosity.h geometry.h soil_water.h \
 macro.h soil_heat.h soil_NH4.h solute.h adsorption.h transport.h \
 mactrans.h soil_NO3.h soil_chemicals.h organic_matter.h \
 nitrification.h denitrification.h plf.h groundwater.h log.h im.h am.h \
 weather.h vegetation.h
weather_simple${OBJ}: weather_simple.C weather_old.h weather.h librarian.h \
 library.h common.h alist.h syntax.h im.h log.h
uzrichard${OBJ}: uzrichard.C uzmodel.h librarian.h library.h common.h \
 alist.h syntax.h soil.h horizon.h hydraulic.h tortuosity.h geometry.h \
 mathlib.h log.h average.h
hydraulic_yolo${OBJ}: hydraulic_yolo.C hydraulic.h librarian.h library.h \
 common.h alist.h syntax.h plf.h
hydraulic_M_vG${OBJ}: hydraulic_M_vG.C hydraulic.h librarian.h library.h \
 common.h alist.h syntax.h plf.h
hydraulic_B_vG${OBJ}: hydraulic_B_vG.C hydraulic.h librarian.h library.h \
 common.h alist.h syntax.h plf.h
hydraulic_M_C${OBJ}: hydraulic_M_C.C hydraulic.h librarian.h library.h \
 common.h alist.h syntax.h
hydraulic_B_C${OBJ}: hydraulic_B_C.C hydraulic.h librarian.h library.h \
 common.h alist.h syntax.h
hydraulic_M_BaC${OBJ}: hydraulic_M_BaC.C hydraulic.h librarian.h library.h \
 common.h alist.h syntax.h
hydraulic_B_BaC${OBJ}: hydraulic_B_BaC.C hydraulic.h librarian.h library.h \
 common.h alist.h syntax.h
groundwater_static${OBJ}: groundwater_static.C groundwater.h uzmodel.h \
 librarian.h library.h common.h alist.h syntax.h
horizon_std${OBJ}: horizon_std.C horizon.h librarian.h library.h common.h \
 alist.h syntax.h
crop_std${OBJ}: crop_std.C crop.h time.h librarian.h library.h common.h \
 alist.h syntax.h root_system.h canopy_std.h canopy_simple.h plf.h \
 log.h bioclimate.h soil_water.h macro.h soil.h horizon.h hydraulic.h \
 tortuosity.h geometry.h om.h organic_matter.h soil_heat.h soil_NH4.h \
 solute.h adsorption.h transport.h mactrans.h soil_NO3.h am.h \
 harvest.h chemicals.h mathlib.h
action_sow${OBJ}: action_sow.C action.h librarian.h library.h common.h \
 alist.h syntax.h daisy.h field.h crop.h
action_stop${OBJ}: action_stop.C action.h librarian.h library.h common.h \
 alist.h syntax.h daisy.h
condition_time${OBJ}: condition_time.C condition.h librarian.h library.h \
 common.h alist.h syntax.h daisy.h
condition_logic${OBJ}: condition_logic.C condition.h librarian.h library.h \
 common.h alist.h syntax.h
action_irrigate${OBJ}: action_irrigate.C action.h librarian.h library.h \
 common.h alist.h syntax.h daisy.h field.h am.h im.h
action_lisp${OBJ}: action_lisp.C action.h librarian.h library.h common.h \
 alist.h syntax.h daisy.h log.h condition.h
weather_none${OBJ}: weather_none.C weather_old.h weather.h librarian.h \
 library.h common.h alist.h syntax.h im.h
action_fertilize${OBJ}: action_fertilize.C action.h librarian.h library.h \
 common.h alist.h syntax.h daisy.h field.h am.h im.h
weather_file${OBJ}: weather_file.C weather_old.h weather.h librarian.h \
 library.h common.h alist.h syntax.h im.h log.h
action_tillage${OBJ}: action_tillage.C action.h librarian.h library.h \
 common.h alist.h syntax.h daisy.h field.h
action_harvest${OBJ}: action_harvest.C action.h librarian.h library.h \
 common.h alist.h syntax.h daisy.h field.h
hydraulic_old${OBJ}: hydraulic_old.C hydraulic.h librarian.h library.h \
 common.h alist.h syntax.h mathlib.h plf.h
crop_old${OBJ}: crop_old.C crop.h time.h librarian.h library.h common.h \
 alist.h syntax.h log.h bioclimate.h plf.h soil_water.h macro.h soil.h \
 horizon.h hydraulic.h tortuosity.h geometry.h om.h organic_matter.h \
 soil_heat.h soil_NH4.h solute.h adsorption.h transport.h mactrans.h \
 soil_NO3.h am.h harvest.h chemicals.h mathlib.h
crop_sold${OBJ}: crop_sold.C crop.h time.h librarian.h library.h common.h \
 alist.h syntax.h log.h bioclimate.h plf.h soil_water.h macro.h soil.h \
 horizon.h hydraulic.h tortuosity.h geometry.h organic_matter.h om.h \
 soil_heat.h soil_NH4.h solute.h adsorption.h transport.h mactrans.h \
 soil_NO3.h am.h harvest.h chemicals.h mathlib.h
action_with${OBJ}: action_with.C action.h librarian.h library.h common.h \
 alist.h syntax.h daisy.h field.h
hydraulic_old2${OBJ}: hydraulic_old2.C hydraulic.h librarian.h library.h \
 common.h alist.h syntax.h mathlib.h plf.h
nitrification_soil${OBJ}: nitrification_soil.C nitrification.h librarian.h \
 library.h common.h alist.h syntax.h soil.h horizon.h hydraulic.h \
 tortuosity.h geometry.h soil_water.h macro.h soil_heat.h soil_NH4.h \
 solute.h adsorption.h transport.h mactrans.h soil_NO3.h mathlib.h \
 log.h plf.h
nitrification_solute${OBJ}: nitrification_solute.C nitrification.h \
 librarian.h library.h common.h alist.h syntax.h soil.h horizon.h \
 hydraulic.h tortuosity.h geometry.h soil_water.h macro.h soil_heat.h \
 soil_NH4.h solute.h adsorption.h transport.h mactrans.h soil_NO3.h \
 log.h mathlib.h plf.h
hydraulic_mod_C${OBJ}: hydraulic_mod_C.C hydraulic.h librarian.h library.h \
 common.h alist.h syntax.h
uzlr${OBJ}: uzlr.C uzmodel.h librarian.h library.h common.h alist.h \
 syntax.h soil.h horizon.h hydraulic.h tortuosity.h geometry.h log.h \
 mathlib.h
transport_cd${OBJ}: transport_cd.C transport.h librarian.h library.h \
 common.h alist.h syntax.h soil.h horizon.h hydraulic.h tortuosity.h \
 geometry.h soil_water.h macro.h solute.h adsorption.h mactrans.h \
 log.h mathlib.h
transport_none${OBJ}: transport_none.C transport.h librarian.h library.h \
 common.h alist.h syntax.h soil.h horizon.h hydraulic.h tortuosity.h \
 geometry.h soil_water.h macro.h solute.h adsorption.h mactrans.h \
 log.h mathlib.h
transport_convection${OBJ}: transport_convection.C transport.h librarian.h \
 library.h common.h alist.h syntax.h soil.h horizon.h hydraulic.h \
 tortuosity.h geometry.h soil_water.h macro.h solute.h adsorption.h \
 mactrans.h log.h mathlib.h
adsorption_vS_S${OBJ}: adsorption_vS_S.C adsorption.h librarian.h \
 library.h common.h alist.h syntax.h soil.h horizon.h hydraulic.h \
 tortuosity.h geometry.h mathlib.h
adsorption_none${OBJ}: adsorption_none.C adsorption.h librarian.h \
 library.h common.h alist.h syntax.h
tortuosity_M_Q${OBJ}: tortuosity_M_Q.C tortuosity.h librarian.h library.h \
 common.h alist.h syntax.h hydraulic.h
tortuosity_linear${OBJ}: tortuosity_linear.C tortuosity.h librarian.h \
 library.h common.h alist.h syntax.h hydraulic.h
adsorption_freundlich${OBJ}: adsorption_freundlich.C adsorption.h \
 librarian.h library.h common.h alist.h syntax.h soil.h horizon.h \
 hydraulic.h tortuosity.h geometry.h mathlib.h
adsorption_linear${OBJ}: adsorption_linear.C adsorption.h librarian.h \
 library.h common.h alist.h syntax.h soil.h horizon.h hydraulic.h \
 tortuosity.h geometry.h
adsorption_langmuir${OBJ}: adsorption_langmuir.C adsorption.h librarian.h \
 library.h common.h alist.h syntax.h soil.h horizon.h hydraulic.h \
 tortuosity.h geometry.h mathlib.h
bioclimate_std${OBJ}: bioclimate_std.C bioclimate.h librarian.h library.h \
 common.h alist.h syntax.h surface.h uzmodel.h weather.h im.h plf.h \
 soil.h horizon.h hydraulic.h tortuosity.h geometry.h snow.h log.h \
 mathlib.h pet.h svat.h vegetation.h chemicals.h
condition_crop${OBJ}: condition_crop.C condition.h librarian.h library.h \
 common.h alist.h syntax.h crop.h field.h daisy.h
condition_soil${OBJ}: condition_soil.C condition.h librarian.h library.h \
 common.h alist.h syntax.h field.h daisy.h
log_table1${OBJ}: log_table1.C log.h librarian.h library.h common.h \
 alist.h syntax.h condition.h geometry.h
log_checkpoint${OBJ}: log_checkpoint.C log_alist.h log.h librarian.h \
 library.h common.h alist.h syntax.h condition.h daisy.h \
 printer_file.h printer.h
weather_hourly${OBJ}: weather_hourly.C weather_old.h weather.h librarian.h \
 library.h common.h alist.h syntax.h im.h log.h mathlib.h
uznone${OBJ}: uznone.C uzmodel.h librarian.h library.h common.h alist.h \
 syntax.h soil.h horizon.h hydraulic.h tortuosity.h geometry.h log.h \
 mathlib.h
condition_daisy${OBJ}: condition_daisy.C condition.h librarian.h library.h \
 common.h alist.h syntax.h daisy.h
chemical_std${OBJ}: chemical_std.C chemical.h librarian.h library.h \
 common.h alist.h syntax.h mathlib.h plf.h soil_chemical.h solute.h \
 adsorption.h transport.h mactrans.h
hydraulic_M_BaC_Bimodal${OBJ}: hydraulic_M_BaC_Bimodal.C hydraulic.h \
 librarian.h library.h common.h alist.h syntax.h
hydraulic_B_BaC_Bimodal${OBJ}: hydraulic_B_BaC_Bimodal.C hydraulic.h \
 librarian.h library.h common.h alist.h syntax.h mathlib.h
pet_makkink${OBJ}: pet_makkink.C pet.h librarian.h library.h common.h \
 alist.h syntax.h weather.h im.h
pet_weather${OBJ}: pet_weather.C pet.h librarian.h library.h common.h \
 alist.h syntax.h weather.h im.h
svat_none${OBJ}: svat_none.C svat.h librarian.h library.h common.h alist.h \
 syntax.h
action_spray${OBJ}: action_spray.C action.h librarian.h library.h common.h \
 alist.h syntax.h daisy.h field.h chemical.h
pet_PM${OBJ}: pet_PM.C pet.h librarian.h library.h common.h alist.h \
 syntax.h weather.h im.h soil.h horizon.h hydraulic.h tortuosity.h \
 geometry.h surface.h uzmodel.h soil_heat.h bioclimate.h \
 net_radiation.h vegetation.h log.h
svat_pmsw${OBJ}: svat_pmsw.C surface.h uzmodel.h librarian.h library.h \
 common.h alist.h syntax.h weather.h im.h soil.h horizon.h hydraulic.h \
 tortuosity.h geometry.h soil_water.h macro.h soil_heat.h vegetation.h \
 pet.h svat.h log.h nrutil.h
action_merge${OBJ}: action_merge.C action.h librarian.h library.h common.h \
 alist.h syntax.h daisy.h field.h
action_divide${OBJ}: action_divide.C action.h librarian.h library.h \
 common.h alist.h syntax.h daisy.h field.h
groundwater_file${OBJ}: groundwater_file.C groundwater.h uzmodel.h \
 librarian.h library.h common.h alist.h syntax.h
action_surface${OBJ}: action_surface.C action.h librarian.h library.h \
 common.h alist.h syntax.h daisy.h field.h
set_exceptions${OBJ}: set_exceptions.S
main${OBJ}: main.C daisy.h time.h syntax.h common.h alist.h library.h
tkmain${OBJ}: tkmain.C daisy.h time.h syntax.h common.h alist.h library.h
gmain${OBJ}: gmain.C daisy.h time.h syntax.h common.h alist.h library.h
cmain${OBJ}: cmain.c cdaisy.h
bugmain${OBJ}: bugmain.c cdaisy.h
