# Makefile --- For maintaining the Daisy project.
#
# Automatic creation of daisy on multiple platforms.
#
# You need GNU Make for using this file.
#
# The following envirnoment variables are used:
#
# HOSTTYPE
#	sun4		Create code for Solaris-2 / UltraSPARC.
#	i386-linux	Create code for ia32 / Linux.
#	hp		Create code for HP/UX / HP-PA.
#	win32		Create code for Win32 / Pentium.
#	cygwin		Create code for Cygwin / Pentium.
#	mingw		Create code for Mingw / Pentium.

# All makefiles should have these.
#
SHELL = /bin/sh
MAKEFLAGS =

# Some non-local files and directories.

FTPDIR = /home/ftp/pub/daisy
WWWINDEX = /home/user_3/daisy/.public_html/index.html

BORLAND = "e:/Program Files/Borland/CBuilder5/"

# HOSTTYPE is not defined in the native win32 Emacs.
#
ifeq ($(OS),Windows_NT)
	ifeq ($(OSTYPE),cygwin)
		HOSTTYPE = cygwin
#		HOSTTYPE = mingw
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
ifeq ($(HOSTTYPE),i386-linux)
	COMPILER = gcc
#	COMPILER = sun	
endif
ifeq ($(HOSTTYPE),hp)
	COMPILER = gcc
endif
ifeq ($(HOSTTYPE),win32)
	COMPILER = borland
endif
ifeq ($(OSTYPE),cygwin)
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
		OPTIMIZE = -O2 -ffast-math 
		ifeq ($(HOSTTYPE),sun4)
			OPTIMIZE = -O2 -ffast-math -mcpu=v8 -mtune=ultrasparc
#`-mcpu=ultrasparc' breaks `IM::IM ()' with gcc 2.95.1.
		endif
		ifeq ($(HOSTTYPE),i386-linux)
		  OPTIMIZE = -O2 -ffast-math -mcpu=pentiumpro -march=pentium
		endif
		ifeq ($(HOSTTYPE),cygwin)
		  OPTIMIZE = -O2 -ffast-math -mcpu=pentiumpro -march=pentium
		endif
		ifeq ($(HOSTTYPE),mingw)
		  OPTIMIZE = -O2 -ffast-math -mcpu=pentiumpro -march=pentium
		endif
	endif
endif

# Do we want to create a dynamic library?
#
ifeq ($(HOSTTYPE),i386-linux)
	USE_DYNLIB = false
endif
ifeq ($(HOSTTYPE),sun4)
	USE_DYNLIB = false
endif
ifeq ($(HOSTTYPE),hp)
	USE_DYNLIB = true
endif
ifeq ($(HOSTTYPE),win32)
	USE_DYNLIB = false
endif
ifeq ($(HOSTTYPE),cygwin)
	USE_DYNLIB = false
endif
ifeq ($(HOSTTYPE),mingw)
	USE_DYNLIB = false
endif

# Create the right compile command.
#
ifeq ($(USE_DYNLIB),true)
	DYNSEARCH = -R`pwd`
endif

ifeq ($(COMPILER),gcc)
	ifeq ($(HOSTTYPE),sun4)
		OSFLAGS = 
		DEBUG = -g
	endif
	ifeq ($(HOSTTYPE),i386-linux)
		OSFLAGS = 
		DEBUG = -g
	endif
	ifeq ($(HOSTTYPE),cygwin)
		OSFLAGS =
		DEBUG = 
	endif
	ifeq ($(HOSTTYPE),mingw)
		OSFLAGS = -DMINGW -mno-cygwin
#		          -I/home/mingw/include -L/home/mingw/lib
		DEBUG =
	endif
	WARNING = -W -Wall -Wno-sign-compare -Wstrict-prototypes \
		  -Wconversion -Wmissing-prototypes 
	COMPILE = "c++" -ansi -pedantic $(WARNING) $(DEBUG) $(OSFLAGS)
	CCOMPILE = gcc -I/pack/f2c/include -g -Wall
endif
ifeq ($(COMPILER),sun)
	COMPILE = /pack/devpro/SUNWspro/bin/CC
	CCOMPILE = gcc -I/pack/f2c/include -g -Wall
endif
ifeq ($(COMPILER),borland)
	WARNFLAGS = -w-csu -wdef -wnod -wamb -w-par -w-hid
	COMPILE = $(BORLAND)Bin/bcc32 -P -v $(WARNFLAGS)
	CCOMPILE = $(BORLAND)Bin/bcc32 -P- -v $(WARNFLAGS)
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
ifeq ($(HOSTTYPE),i386-linux)
	MATHLIB =
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

# Locate the Qt library.
#
QTINCLUDE	= -I/pack/qt/include -I/usr/openwin/include
QTLIB		= -L/pack/qt/lib -R/pack/qt/lib -lqt \
		  -L/usr/openwin/lib -R/usr/openwin/lib \
		  -lXext -lX11 -lsocket -lnsl -lm

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
	LINK = $(BORLAND)Bin/BCC32 -lw-dup -e
	DLLLINK = $(BORLAND)Bin/BCC32 -WD -lTpd -lw-dup -e
	NOLINK = -c
else
	LINK = $(CC) $(DYNSEARCH) $(DEBUG) -o
	NOLINK = -c
endif

# Select the C files that doesn't have a corresponding header file.
# These are all models of some componet.
#
MODELS = hydraulic_Cosby.C pedo_linear.C adsorption_full.C \
	equil_langmuir.C transform_equil.C condition_weather.C \
	rootdens_PLF.C rootdens_G_P.C groundwater_file.C action_fertilize.C \
	action_repeat.C column_inorganic.C  vegetation_permanent.C \
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
	weather_none.C action_tillage.C \
	action_harvest.C crop_old.C crop_sold.C \
	action_with.C nitrification_soil.C \
	nitrification_solute.C hydraulic_mod_C.C uzlr.C transport_cd.C \
	transport_none.C transport_convection.C adsorption_vS_S.C \
	adsorption_none.C tortuosity_M_Q.C tortuosity_linear.C \
	adsorption_freundlich.C adsorption_linear.C adsorption_langmuir.C \
	bioclimate_std.C condition_crop.C \
	condition_soil.C log_table1.C log_checkpoint.C \
	uznone.C condition_daisy.C chemical_std.C \
	hydraulic_M_BaC_Bimodal.C hydraulic_B_BaC_Bimodal.C \
	pet_makkink.C pet_weather.C svat_none.C action_spray.C pet_PM.C \
	svat_pmsw.C action_merge.C action_divide.C \
	action_surface.C

DISABLED = weather_file.C hydraulic_old.C hydraulic_old2.C weather_hourly.C


# A component is a common interface to a number of models.
#
COMPONENTS = equil.C pedo.C \
	transform.C rootdens.C select.C average.C mactrans.C macro.C \
	document.C parser.C log.C weather.C column.C crop.C \
	action.C condition.C horizon.C 	uzmodel.C hydraulic.C \
	bioclimate.C groundwater.C am.C transport.C \
	adsorption.C tortuosity.C printer.C chemical.C \
	pet.C net_radiation.C svat.C vegetation.C 

# Submodels are combined models and components.
#
SUBMODELS = photosynthesis.C crpn.C vernalization.C \
	partition.C development.C production.C \
	harvesting.C canopy_simple.C canopy_std.C root_system.C \
	ridge.C soil.C surface.C soil_water.C soil_NH4.C soil_NO3.C \
	organic_matter.C nitrification.C denitrification.C soil_heat.C \
	snow.C im.C om.C harvest.C chemicals.C field.C \
	soil_chemical.C soil_chemicals.C bioincorporation.C


# Special or intermediate models with their own interface.
#
SPECIALS = weather_old.C log_extern.C log_select.C parser_file.C solute.C \
	geometry.C printer_file.C log_alist.C log_clone.C column_base.C

# Various utility code that are neither a component or a (sub)model.
#
OTHER = units.C \
	check.C check_range.C path.C message.C options.C traverse_delete.C \
	depend.C traverse.C treelog.C treelog_stream.C tmpstream.C \
	lexer_data.C lexer.C daisy.C alist.C syntax.C library.C plf.C \
	time.C mathlib.C librarian.C cdaisy.C common.C nrutil.C \
	submodel.C

# Everything that has an interface.
#
INTERFACES = $(COMPONENTS) $(SUBMODELS) $(SPECIALS) $(OTHER)

# Select the Qt frontend files.
QTSOURCES = qmain_edit.C qmain_edit_moc.C qmain.C \
	qmain_moc.C qmain_tree.C qmain_item.C qmain_populate.C 	qmain_busy.C
QTOBJECTS = $(QTSOURCES:.C=${OBJ})

# Select the C files that are not part of the library.
#
MAIN = main.C tkmain.C gmain.C

# The object files used in the daisy library.
#
LIBOBJ = $(INTERFACES:.C=${OBJ}) $(MODELS:.C=${OBJ}) $(SPARCOBJ)

# Find all object files, header files, and source files.
#
OBJECTS = $(LIBOBJ) $(MAIN:.C=${OBJ}) cmain${OBJ} bugmain.o
SOURCES = $(INTERFACES) $(MODELS) $(SPARCSRC) $(MAIN) $(QTSOURCES) \
	cmain.c bugmain.c $(DISABLED)
HEADERS = $(INTERFACES:.C=.h) $(QTSOURCES:.C.h) version.h

# Find all printable files.
#
TEXT =  ChangeLog.1 Makefile ChangeLog TODO NEWS FILES COPYING COPYING.LIB \
	$(HEADERS) $(SOURCES) tlink32.ini daisy.bpr daisy.bpf daisy.bpg

# The executables.
#
EXECUTABLES = daisy${EXT} tkdaisy${EXT} cdaisy${EXT} gdaisy${EXT}

# Select files to be removed by the next cvs update.
#
REMOVE = check_std.C check_std.h

# These are the file extensions we deal with.
# 
.SUFFIXES:	.C ${OBJ} .h .c ${EXT} .a

# Create all the executables.
#
all:	#(EXECUTABLES)
	@echo Please be specific.

# Create the main executable.
#
daisy.exe:	main${OBJ} $(LIBOBJ)
	$(LINK)daisy.exe $^ $(MATHLIB)

daisy:	main${OBJ} $(LIBOBJ) #daisy.so
	$(LINK)daisy  $^ $(MATHLIB)

# Create manager test executable.
#
mandaisy${EXT}:	manmain${OBJ} daisy.so
	$(LINK)mandaisy  $^ $(MATHLIB)

# Create bug test executable.
#
bugdaisy${EXT}:	bugmain${OBJ} daisy.so
	$(LINK)bugdaisy  $^ $(MATHLIB)

# Create executable with embedded tcl/tk.
#
tkdaisy${EXT}:	tkmain${OBJ} daisy.so
	$(LINK)tkdaisy $^ $(TKLIB) $(MATHLIB)

# Create executable with Gtk--.
#
gdaisy${EXT}:	gmain${OBJ} daisy.so
	$(LINK)gdaisy $^ $(GTKMMLIB)

# Create executable with Qt.
#
qdaisy${EXT}:	$(QTOBJECTS) daisy.so
	$(LINK)qdaisy $^ $(QTLIB)

qmain_moc.C:	qmain.h
	/pack/qt/bin/moc $^ > qmain_moc.C

qmain_edit_moc.C:	qmain_edit.h
	/pack/qt/bin/moc $^ > qmain_edit_moc.C

# Create the C main executable.
#
cdaisy${EXT}:  cmain${OBJ} daisy.so
	$(LINK)cdaisy $^ $(MATHLIB)

# Create the C main executable for testing.
#
cdaisy_test${EXT}:  cmain_test${OBJ} daisy.so
	$(LINK)cdaisy_test $^ $(MATHLIB)

# Create a DLL.
#
daisy.dll:	$(LIBOBJ)
	$(DLLLINK)daisy.dll $^

# Create a shared library.
#
daisy.so: $(LIBOBJ)
	$(CC) -shared -o daisy.so $^ $(MATHLIB)

# Create daisy plot executable.
#
pdaisy${EXT}: pmain${OBJ} time.o
	$(LINK)pdaisy $^ $(GTKMMDRAWLIB) $(MATHLIB)


dlldaisy${EXT}:	cmain${OBJ} daisy.dll
	$(LINK)dlldaisy $^ $(MATHLIB)


# Create the MMM executable.

mmm${EXT}:	$(MOBJECTS)
	$(LINK)mmm  $^ $(MATHLIB)

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
	 && makeindex reference \
	 && latex reference.tex < /dev/null )

pdf:	txt/reference/reference.pdf

txt/reference/reference.pdf:	txt/reference/components.tex
	(cd txt/reference \
	 && makeindex reference \
	 && pdflatex reference.tex < /dev/null )

txt/reference/components.tex:	daisy
	(cd txt/reference \
	 && ../../daisy all.dai -p LaTeX > components.tex)

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
	c++ -I. $(TKINCLUDE) $(GTKMMINCLUDE)  $(QTINCLUDE) \
	        -MM $(SOURCES) | sed -e 's/\.o:/$${OBJ}:/' >> Makefile

# Create a ZIP file with all the sources.
#
daisy-src.zip:	$(TEXT)
	rm -f daisy-src.zip
	zip daisy-src.zip $(TEXT) daisy.ide

# Move it to ftp.
#
dist:	cvs
	mv -f $(WWWINDEX) $(WWWINDEX).old
	sed -e 's/version [1-9]\.[0-9][0-9]/version $(TAG)/' \
		< $(WWWINDEX).old > $(WWWINDEX)
	cp cdaisy.h cmain.c ChangeLog NEWS $(FTPDIR)
	$(MAKE) daisy-src.zip
	mv -f daisy-src.zip $(FTPDIR)
	(cd lib; $(MAKE) dist)
	$(MAKE) pdf
	mv -f txt/reference/reference.pdf $(FTPDIR)/daisy-ref.pdf
	$(MAKE) ps
	mv -f txt/reference/reference.ps $(FTPDIR)/daisy-ref.ps
	cp daisy $(FTPDIR)/daisy-$(TAG)-$(HOSTTYPE)
	strip $(FTPDIR)/daisy-$(TAG)-$(HOSTTYPE)
	rm -f $(FTPDIR)/daisy-$(HOSTTYPE)
	(cd $(FTPDIR); ln -s daisy-$(TAG)-$(HOSTTYPE) daisy-$(HOSTTYPE))

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

# Special rule for tkmain.o
#
tkmain${OBJ}: tkmain.C
	$(CC) $(TKINCLUDE) $(NOLINK) $<

# Special rule for gmain.o
#
gmain${OBJ}: gmain.C
	$(CC) $(GTKMMINCLUDE) $(NOLINK) $<

# Special rule for Qt frontend files.
#
$(QTOBJECTS):
	$(CC) $(QTINCLUDE) $(NOLINK) $<

# Special rule for pmain.o
#
pmain${OBJ}: pmain.C
	$(CC) $(GTKMMDRAWINCLUDE) $(NOLINK) $<

############################################################
# AUTOMATIC -- DO NOT CHANGE THIS LINE OR ANYTHING BELOW IT!
equil${OBJ}: equil.C equil.h librarian.h library.h common.h alist.h \
 syntax.h treelog.h
pedo${OBJ}: pedo.C pedo.h librarian.h library.h common.h alist.h syntax.h \
 treelog.h
transform${OBJ}: transform.C transform.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h
rootdens${OBJ}: rootdens.C rootdens.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h check.h
select${OBJ}: select.C select.h condition.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h geometry.h check.h
average${OBJ}: average.C average.h librarian.h library.h common.h alist.h \
 syntax.h treelog.h
mactrans${OBJ}: mactrans.C mactrans.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h
macro${OBJ}: macro.C macro.h librarian.h library.h common.h alist.h \
 syntax.h treelog.h
document${OBJ}: document.C document.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h submodel.h
parser${OBJ}: parser.C parser.h librarian.h library.h common.h alist.h \
 syntax.h treelog.h
log${OBJ}: log.C log.h librarian.h library.h common.h alist.h syntax.h \
 treelog.h daisy.h message.h
weather${OBJ}: weather.C weather.h librarian.h library.h common.h alist.h \
 syntax.h treelog.h im.h net_radiation.h log.h mathlib.h
column${OBJ}: column.C column.h librarian.h library.h common.h alist.h \
 syntax.h treelog.h log.h
crop${OBJ}: crop.C crop.h time.h librarian.h library.h common.h alist.h \
 syntax.h treelog.h chemicals.h om.h
action${OBJ}: action.C action.h librarian.h library.h common.h alist.h \
 syntax.h treelog.h
condition${OBJ}: condition.C condition.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h
horizon${OBJ}: horizon.C horizon.h librarian.h library.h common.h alist.h \
 syntax.h treelog.h plf.h hydraulic.h mathlib.h tortuosity.h log.h \
 check.h
uzmodel${OBJ}: uzmodel.C uzmodel.h librarian.h library.h common.h alist.h \
 syntax.h treelog.h
hydraulic${OBJ}: hydraulic.C hydraulic.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h plf.h log.h tmpstream.h check_range.h \
 check.h
bioclimate${OBJ}: bioclimate.C bioclimate.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h weather.h im.h
groundwater${OBJ}: groundwater.C groundwater.h uzmodel.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h log.h
am${OBJ}: am.C am.h librarian.h library.h common.h alist.h syntax.h \
 treelog.h om.h im.h log.h soil.h horizon.h hydraulic.h tortuosity.h \
 geometry.h check.h tmpstream.h message.h mathlib.h
transport${OBJ}: transport.C transport.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h
adsorption${OBJ}: adsorption.C adsorption.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h
tortuosity${OBJ}: tortuosity.C tortuosity.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h
printer${OBJ}: printer.C printer.h librarian.h library.h common.h alist.h \
 syntax.h treelog.h
chemical${OBJ}: chemical.C chemical.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h
pet${OBJ}: pet.C pet.h librarian.h library.h common.h alist.h syntax.h \
 treelog.h log.h vegetation.h surface.h uzmodel.h
net_radiation${OBJ}: net_radiation.C net_radiation.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h log.h weather.h im.h
svat${OBJ}: svat.C svat.h librarian.h library.h common.h alist.h syntax.h \
 treelog.h log.h
vegetation${OBJ}: vegetation.C vegetation.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h log.h
photosynthesis${OBJ}: photosynthesis.C photosynthesis.h bioclimate.h \
 librarian.h library.h common.h alist.h syntax.h treelog.h \
 canopy_std.h canopy_simple.h plf.h development.h message.h submodel.h \
 mathlib.h
crpn${OBJ}: crpn.C crpn.h production.h root_system.h rootdens.h \
 librarian.h library.h common.h alist.h syntax.h treelog.h log.h plf.h \
 mathlib.h submodel.h
vernalization${OBJ}: vernalization.C vernalization.h submodel.h log.h \
 librarian.h library.h common.h alist.h syntax.h treelog.h
partition${OBJ}: partition.C partition.h plf.h submodel.h syntax.h \
 common.h treelog.h alist.h
development${OBJ}: development.C development.h production.h \
 vernalization.h plf.h log.h librarian.h library.h common.h alist.h \
 syntax.h treelog.h message.h submodel.h
production${OBJ}: production.C production.h crpn.h partition.h \
 organic_matter.h common.h am.h librarian.h library.h alist.h syntax.h \
 treelog.h log.h plf.h message.h submodel.h
harvesting${OBJ}: harvesting.C harvesting.h time.h plf.h production.h am.h \
 librarian.h library.h common.h alist.h syntax.h treelog.h om.h crop.h \
 harvest.h chemicals.h log.h mathlib.h submodel.h
canopy_simple${OBJ}: canopy_simple.C canopy_simple.h plf.h submodel.h \
 log.h librarian.h library.h common.h alist.h syntax.h treelog.h
canopy_std${OBJ}: canopy_std.C canopy_std.h canopy_simple.h plf.h \
 submodel.h log.h librarian.h library.h common.h alist.h syntax.h \
 treelog.h mathlib.h
root_system${OBJ}: root_system.C root_system.h rootdens.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h submodel.h soil_heat.h \
 soil_NH4.h solute.h adsorption.h transport.h mactrans.h soil_NO3.h \
 soil_water.h macro.h soil.h horizon.h hydraulic.h tortuosity.h \
 geometry.h log.h check.h mathlib.h message.h
ridge${OBJ}: ridge.C ridge.h soil.h horizon.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h hydraulic.h tortuosity.h \
 geometry.h plf.h submodel.h mathlib.h log.h soil_water.h macro.h
soil${OBJ}: soil.C soil.h horizon.h librarian.h library.h common.h alist.h \
 syntax.h treelog.h hydraulic.h tortuosity.h geometry.h mathlib.h \
 submodel.h log.h message.h check.h
surface${OBJ}: surface.C surface.h uzmodel.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h soil_water.h macro.h soil.h \
 horizon.h hydraulic.h tortuosity.h geometry.h log.h am.h im.h \
 mathlib.h submodel.h chemicals.h soil_chemicals.h soil_chemical.h \
 solute.h adsorption.h transport.h mactrans.h plf.h ridge.h message.h
soil_water${OBJ}: soil_water.C soil_water.h macro.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h log.h uzmodel.h soil.h horizon.h \
 hydraulic.h tortuosity.h geometry.h surface.h groundwater.h mathlib.h \
 tmpstream.h submodel.h message.h
soil_NH4${OBJ}: soil_NH4.C soil_NH4.h solute.h adsorption.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h transport.h mactrans.h \
 soil.h horizon.h hydraulic.h tortuosity.h geometry.h soil_water.h \
 macro.h submodel.h
soil_NO3${OBJ}: soil_NO3.C soil_NO3.h solute.h adsorption.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h transport.h mactrans.h \
 soil.h horizon.h hydraulic.h tortuosity.h geometry.h submodel.h
organic_matter${OBJ}: organic_matter.C organic_matter.h common.h syntax.h \
 treelog.h alist.h log.h librarian.h library.h am.h om.h soil.h \
 horizon.h hydraulic.h tortuosity.h geometry.h soil_water.h macro.h \
 soil_NH4.h solute.h adsorption.h transport.h mactrans.h soil_NO3.h \
 soil_heat.h bioincorporation.h mathlib.h plf.h tmpstream.h submodel.h \
 message.h
nitrification${OBJ}: nitrification.C nitrification.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h
denitrification${OBJ}: denitrification.C denitrification.h common.h plf.h \
 alist.h syntax.h treelog.h soil.h horizon.h librarian.h library.h \
 hydraulic.h tortuosity.h geometry.h soil_water.h macro.h soil_heat.h \
 organic_matter.h soil_NO3.h solute.h adsorption.h transport.h \
 mactrans.h log.h submodel.h
soil_heat${OBJ}: soil_heat.C soil_heat.h alist.h common.h surface.h \
 uzmodel.h librarian.h library.h syntax.h treelog.h weather.h im.h \
 soil_water.h macro.h soil.h horizon.h hydraulic.h tortuosity.h \
 geometry.h mathlib.h log.h tmpstream.h submodel.h
snow${OBJ}: snow.C snow.h alist.h common.h syntax.h treelog.h log.h \
 librarian.h library.h soil.h horizon.h hydraulic.h tortuosity.h \
 geometry.h soil_water.h macro.h soil_heat.h submodel.h mathlib.h \
 message.h
im${OBJ}: im.C im.h am.h librarian.h library.h common.h alist.h syntax.h \
 treelog.h log.h submodel.h
om${OBJ}: om.C om.h common.h syntax.h treelog.h alist.h check.h geometry.h \
 log.h librarian.h library.h mathlib.h tmpstream.h submodel.h
harvest${OBJ}: harvest.C harvest.h chemicals.h syntax.h common.h treelog.h \
 log.h librarian.h library.h alist.h submodel.h
chemicals${OBJ}: chemicals.C chemicals.h syntax.h common.h treelog.h log.h \
 librarian.h library.h alist.h chemical.h submodel.h
field${OBJ}: field.C field.h common.h column.h librarian.h library.h \
 alist.h syntax.h treelog.h log.h log_clone.h log_alist.h
soil_chemical${OBJ}: soil_chemical.C soil_chemical.h solute.h adsorption.h \
 librarian.h library.h common.h alist.h syntax.h treelog.h transport.h \
 mactrans.h plf.h chemical.h soil.h horizon.h hydraulic.h tortuosity.h \
 geometry.h soil_heat.h soil_water.h macro.h organic_matter.h log.h \
 submodel.h
soil_chemicals${OBJ}: soil_chemicals.C soil_chemicals.h soil_chemical.h \
 solute.h adsorption.h librarian.h library.h common.h alist.h syntax.h \
 treelog.h transport.h mactrans.h plf.h soil.h horizon.h hydraulic.h \
 tortuosity.h geometry.h soil_water.h macro.h soil_heat.h \
 organic_matter.h chemical.h chemicals.h log.h submodel.h
bioincorporation${OBJ}: bioincorporation.C bioincorporation.h common.h \
 alist.h syntax.h treelog.h log.h librarian.h library.h soil.h \
 horizon.h hydraulic.h tortuosity.h geometry.h am.h submodel.h plf.h \
 om.h mathlib.h message.h
weather_old${OBJ}: weather_old.C weather_old.h weather.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h im.h tmpstream.h
log_extern${OBJ}: log_extern.C log_select.h log.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h select.h condition.h log_extern.h
log_select${OBJ}: log_select.C log_select.h log.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h select.h condition.h
parser_file${OBJ}: parser_file.C parser_file.h parser.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h lexer.h plf.h \
 tmpstream.h treelog_stream.h message.h path.h units.h
solute${OBJ}: solute.C solute.h adsorption.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h transport.h mactrans.h log.h \
 soil.h horizon.h hydraulic.h tortuosity.h geometry.h soil_water.h \
 macro.h mathlib.h message.h
geometry${OBJ}: geometry.C geometry.h common.h syntax.h treelog.h alist.h \
 tmpstream.h mathlib.h message.h check.h
printer_file${OBJ}: printer_file.C printer_file.h printer.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h plf.h parser.h
log_alist${OBJ}: log_alist.C log_alist.h log.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h
log_clone${OBJ}: log_clone.C log_clone.h log_alist.h log.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h
column_base${OBJ}: column_base.C column_base.h column.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h bioclimate.h surface.h \
 uzmodel.h soil.h horizon.h hydraulic.h tortuosity.h geometry.h \
 soil_water.h macro.h soil_heat.h soil_chemicals.h soil_chemical.h \
 solute.h adsorption.h transport.h mactrans.h plf.h transform.h \
 groundwater.h log.h weather.h im.h vegetation.h
units${OBJ}: units.C units.h mathlib.h common.h
check${OBJ}: check.C check.h
check_range${OBJ}: check_range.C check_range.h check.h tmpstream.h \
 common.h
path${OBJ}: path.C path.h common.h
message${OBJ}: message.C message.h common.h options.h
options${OBJ}: options.C options.h common.h parser_file.h parser.h \
 librarian.h library.h alist.h syntax.h treelog.h document.h \
 treelog_stream.h version.h message.h path.h
traverse_delete${OBJ}: traverse_delete.C traverse_delete.h traverse.h \
 library.h common.h syntax.h treelog.h alist.h
depend${OBJ}: depend.C depend.h traverse.h library.h common.h syntax.h \
 treelog.h alist.h tmpstream.h
traverse${OBJ}: traverse.C traverse.h library.h common.h syntax.h \
 treelog.h alist.h
treelog${OBJ}: treelog.C treelog.h
treelog_stream${OBJ}: treelog_stream.C treelog_stream.h treelog.h common.h
tmpstream${OBJ}: tmpstream.C tmpstream.h common.h
lexer_data${OBJ}: lexer_data.C lexer_data.h lexer.h
lexer${OBJ}: lexer.C lexer.h tmpstream.h common.h treelog.h path.h
daisy${OBJ}: daisy.C daisy.h time.h weather.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h im.h groundwater.h uzmodel.h \
 horizon.h log.h parser.h am.h nitrification.h bioclimate.h \
 hydraulic.h field.h harvest.h chemicals.h action.h condition.h \
 column.h submodel.h message.h
alist${OBJ}: alist.C plf.h library.h common.h alist.h syntax.h treelog.h \
 message.h
syntax${OBJ}: syntax.C syntax.h common.h treelog.h alist.h library.h \
 tmpstream.h check.h
library${OBJ}: library.C library.h common.h alist.h syntax.h treelog.h \
 tmpstream.h
plf${OBJ}: plf.C plf.h
time${OBJ}: time.C time.h
mathlib${OBJ}: mathlib.C mathlib.h common.h
librarian${OBJ}: librarian.C librarian.h library.h common.h alist.h \
 syntax.h treelog.h
cdaisy${OBJ}: cdaisy.C syntax.h common.h treelog.h alist.h daisy.h \
 parser_file.h parser.h librarian.h library.h field.h column.h \
 weather.h im.h action.h horizon.h printer_file.h printer.h version.h \
 chemical.h log_extern.h treelog_stream.h message.h
common${OBJ}: common.C common.h message.h
nrutil${OBJ}: nrutil.C
submodel${OBJ}: submodel.C submodel.h common.h
hydraulic_Cosby${OBJ}: hydraulic_Cosby.C hydraulic.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h horizon.h
pedo_linear${OBJ}: pedo_linear.C pedo.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h soil.h horizon.h hydraulic.h tortuosity.h \
 geometry.h
adsorption_full${OBJ}: adsorption_full.C adsorption.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h
equil_langmuir${OBJ}: equil_langmuir.C equil.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h pedo.h soil.h horizon.h \
 hydraulic.h tortuosity.h geometry.h check.h mathlib.h
transform_equil${OBJ}: transform_equil.C transform.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h soil.h horizon.h hydraulic.h \
 tortuosity.h geometry.h soil_water.h macro.h soil_chemicals.h \
 soil_chemical.h solute.h adsorption.h transport.h mactrans.h plf.h \
 pedo.h equil.h check.h log.h mathlib.h
condition_weather${OBJ}: condition_weather.C condition.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h field.h daisy.h check.h \
 log.h tmpstream.h
rootdens_PLF${OBJ}: rootdens_PLF.C rootdens.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h geometry.h plf.h check.h \
 mathlib.h
rootdens_G_P${OBJ}: rootdens_G_P.C rootdens.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h geometry.h check.h message.h
groundwater_file${OBJ}: groundwater_file.C groundwater.h uzmodel.h \
 librarian.h library.h common.h alist.h syntax.h treelog.h \
 lexer_data.h lexer.h
action_fertilize${OBJ}: action_fertilize.C action.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h daisy.h field.h am.h im.h \
 tmpstream.h message.h
action_repeat${OBJ}: action_repeat.C action.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h log.h
column_inorganic${OBJ}: column_inorganic.C column_base.h column.h \
 librarian.h library.h common.h alist.h syntax.h treelog.h \
 bioclimate.h surface.h uzmodel.h soil.h horizon.h hydraulic.h \
 tortuosity.h geometry.h soil_water.h macro.h soil_heat.h \
 soil_chemicals.h soil_chemical.h solute.h adsorption.h transport.h \
 mactrans.h plf.h transform.h groundwater.h log.h weather.h im.h \
 vegetation.h am.h
vegetation_permanent${OBJ}: vegetation_permanent.C vegetation.h \
 librarian.h library.h common.h alist.h syntax.h treelog.h plf.h \
 mathlib.h log.h root_system.h rootdens.h canopy_simple.h soil.h \
 horizon.h hydraulic.h tortuosity.h geometry.h crop.h am.h om.h \
 organic_matter.h
vegetation_crops${OBJ}: vegetation_crops.C vegetation.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h crop.h organic_matter.h \
 soil.h horizon.h hydraulic.h tortuosity.h geometry.h plf.h mathlib.h \
 harvest.h chemicals.h log.h message.h
crop_simple${OBJ}: crop_simple.C crop.h time.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h root_system.h rootdens.h \
 canopy_simple.h plf.h log.h bioclimate.h soil_water.h macro.h soil.h \
 horizon.h hydraulic.h tortuosity.h geometry.h om.h organic_matter.h \
 soil_heat.h soil_NH4.h solute.h adsorption.h transport.h mactrans.h \
 soil_NO3.h am.h harvest.h chemicals.h mathlib.h check.h message.h
action_ridge${OBJ}: action_ridge.C action.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h daisy.h field.h ridge.h message.h
groundwater_fixed${OBJ}: groundwater_fixed.C groundwater.h uzmodel.h \
 librarian.h library.h common.h alist.h syntax.h treelog.h
groundwater_deep${OBJ}: groundwater_deep.C groundwater.h uzmodel.h \
 librarian.h library.h common.h alist.h syntax.h treelog.h
action_heat${OBJ}: action_heat.C action.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h daisy.h field.h check.h message.h
hydraulic_M_vG_compact${OBJ}: hydraulic_M_vG_compact.C hydraulic.h \
 librarian.h library.h common.h alist.h syntax.h treelog.h plf.h
action_crop${OBJ}: action_crop.C action.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h daisy.h field.h crop.h am.h log.h \
 harvest.h chemicals.h check_range.h check.h im.h tmpstream.h \
 message.h
groundwater_lysimeter${OBJ}: groundwater_lysimeter.C groundwater.h \
 uzmodel.h librarian.h library.h common.h alist.h syntax.h treelog.h
select_min${OBJ}: select_min.C select.h condition.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h
select_max${OBJ}: select_max.C select.h condition.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h
select_average${OBJ}: select_average.C select.h condition.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h
action_message${OBJ}: action_message.C action.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h condition.h log.h daisy.h \
 message.h
weather_std${OBJ}: weather_std.C weather.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h im.h lexer_data.h lexer.h tmpstream.h \
 mathlib.h
select_flux_top${OBJ}: select_flux_top.C select.h condition.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h geometry.h
select_flux_bottom${OBJ}: select_flux_bottom.C select.h condition.h \
 librarian.h library.h common.h alist.h syntax.h treelog.h geometry.h
groundwater_pipe${OBJ}: groundwater_pipe.C groundwater.h uzmodel.h \
 librarian.h library.h common.h alist.h syntax.h treelog.h log.h \
 soil.h horizon.h hydraulic.h tortuosity.h geometry.h tmpstream.h \
 mathlib.h message.h
select_index${OBJ}: select_index.C select.h condition.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h
select_content${OBJ}: select_content.C select.h condition.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h geometry.h
select_interval${OBJ}: select_interval.C select.h condition.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h geometry.h
select_flux${OBJ}: select_flux.C select.h condition.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h geometry.h
select_number${OBJ}: select_number.C select.h condition.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h
select_date${OBJ}: select_date.C select.h condition.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h
select_array${OBJ}: select_array.C select.h condition.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h
log_table${OBJ}: log_table.C log_select.h log.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h select.h condition.h geometry.h \
 version.h daisy.h message.h
log_harvest${OBJ}: log_harvest.C log.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h daisy.h harvest.h chemicals.h version.h \
 message.h
action_while${OBJ}: action_while.C action.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h log.h
action_wait${OBJ}: action_wait.C action.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h condition.h log.h daisy.h tmpstream.h
action_activity${OBJ}: action_activity.C action.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h log.h
average_arithmetic${OBJ}: average_arithmetic.C average.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h
average_harmonic${OBJ}: average_harmonic.C average.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h
average_geometric${OBJ}: average_geometric.C average.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h
mactrans_std${OBJ}: mactrans_std.C mactrans.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h soil_water.h macro.h soil.h \
 horizon.h hydraulic.h tortuosity.h geometry.h plf.h mathlib.h \
 message.h
macro_std${OBJ}: macro_std.C macro.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h soil.h horizon.h hydraulic.h tortuosity.h \
 geometry.h plf.h mathlib.h log.h uzmodel.h message.h
macro_none${OBJ}: macro_none.C macro.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h
document_LaTeX${OBJ}: document_LaTeX.C document.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h plf.h tmpstream.h version.h
column_std${OBJ}: column_std.C column_base.h column.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h bioclimate.h surface.h \
 uzmodel.h soil.h horizon.h hydraulic.h tortuosity.h geometry.h \
 soil_water.h macro.h soil_heat.h soil_chemicals.h soil_chemical.h \
 solute.h adsorption.h transport.h mactrans.h plf.h transform.h \
 groundwater.h log.h weather.h im.h vegetation.h soil_NH4.h soil_NO3.h \
 organic_matter.h nitrification.h denitrification.h am.h
weather_simple${OBJ}: weather_simple.C weather_old.h weather.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h im.h log.h mathlib.h
uzrichard${OBJ}: uzrichard.C uzmodel.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h soil.h horizon.h hydraulic.h tortuosity.h \
 geometry.h mathlib.h log.h average.h message.h
hydraulic_yolo${OBJ}: hydraulic_yolo.C hydraulic.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h plf.h
hydraulic_M_vG${OBJ}: hydraulic_M_vG.C hydraulic.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h plf.h check.h
hydraulic_B_vG${OBJ}: hydraulic_B_vG.C hydraulic.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h plf.h
hydraulic_M_C${OBJ}: hydraulic_M_C.C hydraulic.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h check_range.h check.h
hydraulic_B_C${OBJ}: hydraulic_B_C.C hydraulic.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h check_range.h check.h
hydraulic_M_BaC${OBJ}: hydraulic_M_BaC.C hydraulic.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h message.h
hydraulic_B_BaC${OBJ}: hydraulic_B_BaC.C hydraulic.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h
groundwater_static${OBJ}: groundwater_static.C groundwater.h uzmodel.h \
 librarian.h library.h common.h alist.h syntax.h treelog.h
horizon_std${OBJ}: horizon_std.C horizon.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h
crop_std${OBJ}: crop_std.C crop.h time.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h chemicals.h root_system.h rootdens.h \
 canopy_std.h canopy_simple.h plf.h harvesting.h production.h \
 development.h partition.h vernalization.h photosynthesis.h crpn.h \
 log.h bioclimate.h soil_water.h macro.h soil.h horizon.h hydraulic.h \
 tortuosity.h geometry.h organic_matter.h soil_heat.h am.h message.h \
 mathlib.h
action_sow${OBJ}: action_sow.C action.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h daisy.h field.h crop.h message.h
action_stop${OBJ}: action_stop.C action.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h daisy.h
condition_time${OBJ}: condition_time.C condition.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h daisy.h
condition_logic${OBJ}: condition_logic.C condition.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h
action_irrigate${OBJ}: action_irrigate.C action.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h daisy.h field.h im.h check.h \
 message.h
action_lisp${OBJ}: action_lisp.C action.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h daisy.h log.h condition.h
weather_none${OBJ}: weather_none.C weather_old.h weather.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h im.h
action_tillage${OBJ}: action_tillage.C action.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h daisy.h field.h check.h message.h
action_harvest${OBJ}: action_harvest.C action.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h daisy.h field.h message.h
crop_old${OBJ}: crop_old.C crop.h time.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h log.h bioclimate.h plf.h soil_water.h \
 macro.h soil.h horizon.h hydraulic.h tortuosity.h geometry.h om.h \
 organic_matter.h soil_heat.h soil_NH4.h solute.h adsorption.h \
 transport.h mactrans.h soil_NO3.h am.h harvest.h chemicals.h \
 mathlib.h message.h
crop_sold${OBJ}: crop_sold.C crop.h time.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h log.h bioclimate.h plf.h soil_water.h \
 macro.h soil.h horizon.h hydraulic.h tortuosity.h geometry.h \
 organic_matter.h om.h soil_heat.h soil_NH4.h solute.h adsorption.h \
 transport.h mactrans.h soil_NO3.h am.h harvest.h chemicals.h \
 mathlib.h message.h
action_with${OBJ}: action_with.C action.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h daisy.h field.h log.h
nitrification_soil${OBJ}: nitrification_soil.C nitrification.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h soil.h horizon.h \
 hydraulic.h tortuosity.h geometry.h soil_water.h macro.h soil_heat.h \
 soil_NH4.h solute.h adsorption.h transport.h mactrans.h soil_NO3.h \
 mathlib.h log.h plf.h
nitrification_solute${OBJ}: nitrification_solute.C nitrification.h \
 librarian.h library.h common.h alist.h syntax.h treelog.h soil.h \
 horizon.h hydraulic.h tortuosity.h geometry.h soil_water.h macro.h \
 soil_heat.h soil_NH4.h solute.h adsorption.h transport.h mactrans.h \
 soil_NO3.h log.h mathlib.h plf.h
hydraulic_mod_C${OBJ}: hydraulic_mod_C.C hydraulic.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h check.h
uzlr${OBJ}: uzlr.C uzmodel.h librarian.h library.h common.h alist.h \
 syntax.h treelog.h soil.h horizon.h hydraulic.h tortuosity.h \
 geometry.h log.h mathlib.h
transport_cd${OBJ}: transport_cd.C transport.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h soil.h horizon.h hydraulic.h \
 tortuosity.h geometry.h soil_water.h macro.h solute.h adsorption.h \
 mactrans.h log.h mathlib.h message.h
transport_none${OBJ}: transport_none.C transport.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h soil.h horizon.h hydraulic.h \
 tortuosity.h geometry.h soil_water.h macro.h solute.h adsorption.h \
 mactrans.h log.h mathlib.h message.h
transport_convection${OBJ}: transport_convection.C transport.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h soil.h horizon.h \
 hydraulic.h tortuosity.h geometry.h soil_water.h macro.h solute.h \
 adsorption.h mactrans.h log.h mathlib.h
adsorption_vS_S${OBJ}: adsorption_vS_S.C adsorption.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h soil.h horizon.h \
 hydraulic.h tortuosity.h geometry.h mathlib.h
adsorption_none${OBJ}: adsorption_none.C adsorption.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h
tortuosity_M_Q${OBJ}: tortuosity_M_Q.C tortuosity.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h hydraulic.h
tortuosity_linear${OBJ}: tortuosity_linear.C tortuosity.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h hydraulic.h
adsorption_freundlich${OBJ}: adsorption_freundlich.C adsorption.h \
 librarian.h library.h common.h alist.h syntax.h treelog.h soil.h \
 horizon.h hydraulic.h tortuosity.h geometry.h check.h mathlib.h
adsorption_linear${OBJ}: adsorption_linear.C adsorption.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h check.h soil.h \
 horizon.h hydraulic.h tortuosity.h geometry.h
adsorption_langmuir${OBJ}: adsorption_langmuir.C adsorption.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h soil.h horizon.h \
 hydraulic.h tortuosity.h geometry.h check.h mathlib.h
bioclimate_std${OBJ}: bioclimate_std.C bioclimate.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h surface.h uzmodel.h weather.h \
 im.h plf.h soil.h horizon.h hydraulic.h tortuosity.h geometry.h \
 snow.h log.h mathlib.h pet.h svat.h vegetation.h chemicals.h
condition_crop${OBJ}: condition_crop.C condition.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h crop.h field.h daisy.h
condition_soil${OBJ}: condition_soil.C condition.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h field.h daisy.h check.h
log_table1${OBJ}: log_table1.C log.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h condition.h geometry.h message.h
log_checkpoint${OBJ}: log_checkpoint.C log_alist.h log.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h condition.h daisy.h \
 printer_file.h printer.h tmpstream.h
uznone${OBJ}: uznone.C uzmodel.h librarian.h library.h common.h alist.h \
 syntax.h treelog.h soil.h horizon.h hydraulic.h tortuosity.h \
 geometry.h log.h mathlib.h
condition_daisy${OBJ}: condition_daisy.C condition.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h daisy.h
chemical_std${OBJ}: chemical_std.C chemical.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h mathlib.h plf.h soil_chemical.h \
 solute.h adsorption.h transport.h mactrans.h
hydraulic_M_BaC_Bimodal${OBJ}: hydraulic_M_BaC_Bimodal.C hydraulic.h \
 librarian.h library.h common.h alist.h syntax.h treelog.h
hydraulic_B_BaC_Bimodal${OBJ}: hydraulic_B_BaC_Bimodal.C hydraulic.h \
 librarian.h library.h common.h alist.h syntax.h treelog.h mathlib.h
pet_makkink${OBJ}: pet_makkink.C pet.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h weather.h im.h log.h
pet_weather${OBJ}: pet_weather.C pet.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h weather.h im.h log.h
svat_none${OBJ}: svat_none.C svat.h librarian.h library.h common.h alist.h \
 syntax.h treelog.h
action_spray${OBJ}: action_spray.C action.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h daisy.h field.h chemical.h check.h \
 message.h
pet_PM${OBJ}: pet_PM.C pet.h librarian.h library.h common.h alist.h \
 syntax.h treelog.h weather.h im.h soil.h horizon.h hydraulic.h \
 tortuosity.h geometry.h surface.h uzmodel.h soil_heat.h bioclimate.h \
 net_radiation.h vegetation.h log.h message.h
svat_pmsw${OBJ}: svat_pmsw.C surface.h uzmodel.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h weather.h im.h soil.h horizon.h \
 hydraulic.h tortuosity.h geometry.h soil_water.h macro.h soil_heat.h \
 vegetation.h pet.h svat.h log.h nrutil.h
action_merge${OBJ}: action_merge.C action.h librarian.h library.h common.h \
 alist.h syntax.h treelog.h daisy.h field.h message.h
action_divide${OBJ}: action_divide.C action.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h daisy.h field.h message.h
action_surface${OBJ}: action_surface.C action.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h daisy.h field.h check.h message.h
main${OBJ}: main.C daisy.h time.h syntax.h common.h treelog.h alist.h \
 library.h treelog_stream.h options.h message.h
tkmain${OBJ}: tkmain.C daisy.h time.h syntax.h common.h treelog.h alist.h \
 library.h
gmain${OBJ}: gmain.C daisy.h time.h syntax.h common.h treelog.h alist.h \
 library.h
qmain_edit${OBJ}: qmain_edit.C qmain_edit.h alist.h common.h syntax.h \
 treelog.h library.h plf.h depend.h
qmain_edit_moc${OBJ}: qmain_edit_moc.C qmain_edit.h alist.h common.h
qmain${OBJ}: qmain.C qmain.h syntax.h common.h treelog.h alist.h \
 qmain_tree.h qmain_busy.h daisy.h library.h version.h parser_file.h \
 parser.h librarian.h printer_file.h printer.h tmpstream.h
qmain_moc${OBJ}: qmain_moc.C qmain.h syntax.h common.h treelog.h alist.h
qmain_tree${OBJ}: qmain_tree.C qmain_tree.h qmain_item.h qmain_populate.h \
 common.h
qmain_item${OBJ}: qmain_item.C qmain_item.h qmain_edit.h alist.h common.h \
 qmain_tree.h qmain_populate.h qmain_busy.h qmain.h syntax.h treelog.h \
 library.h tmpstream.h treelog_stream.h depend.h traverse_delete.h
qmain_populate${OBJ}: qmain_populate.C qmain_populate.h qmain_tree.h \
 qmain_item.h qmain.h syntax.h common.h treelog.h alist.h traverse.h \
 tmpstream.h plf.h library.h parser.h librarian.h
qmain_busy${OBJ}: qmain_busy.C qmain_busy.h
cmain${OBJ}: cmain.c cdaisy.h
bugmain${OBJ}: bugmain.c cdaisy.h
weather_file${OBJ}: weather_file.C weather_old.h weather.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h im.h log.h
hydraulic_old${OBJ}: hydraulic_old.C hydraulic.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h mathlib.h plf.h
hydraulic_old2${OBJ}: hydraulic_old2.C hydraulic.h librarian.h library.h \
 common.h alist.h syntax.h treelog.h mathlib.h plf.h
weather_hourly${OBJ}: weather_hourly.C weather_old.h weather.h librarian.h \
 library.h common.h alist.h syntax.h treelog.h im.h log.h mathlib.h
