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

SRCDIR = $(HOME)/daisy
OBJHOME = /usr/local/daisy
FTPDIR = /home/ftp/pub/daisy
WWWINDEX = /home/user_3/daisy/.public_html/index.html

BORLAND = "e:/Program Files/Borland/CBuilder5/"
TARGETTYPE = i586-mingw32msvc

# HOSTTYPE is not defined in the native win32 Emacs.
#
ifeq ($(OS),Windows_NT)
#	HOSTTYPE = cygwin
	HOSTTYPE = mingw
#	HOSTTYPE = win32
endif

# Set USE_OPTIMIZE to `true' if you want a fast executable.
#
#USE_OPTIMIZE = true
USE_OPTIMIZE = false

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
ifeq ($(HOSTTYPE),mingw)
	COMPILER = gcc
endif

# On SPARC platforms we trap mathematical exception with some assembler code.
#
ifeq ($(HOSTTYPE),sun4) 
	SPARCSRC = set_exceptions.S
	SPARCOBJ = set_exceptions.o
endif

ifeq ($(COMPILER),ms)
        MSSRC = win32_unistd.C
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
		  OPTIMIZE = -O3 -ffast-math -mcpu=v8 -mtune=ultrasparc
#`-mcpu=ultrasparc' breaks `IM::IM ()' with gcc 2.95.1.
		endif
		ifeq ($(HOSTTYPE),i386-linux)
		  OPTIMIZE = -O3 -ffast-math -mcpu=pentiumpro -march=pentium
	        endif
		ifeq ($(HOSTTYPE),cygwin)
		  OPTIMIZE = -O3 -ffast-math -mtune=pentiumpro -march=pentium
		endif
		ifeq ($(HOSTTYPE),mingw)
		  OPTIMIZE = -O3 -ffast-math -mtune=pentiumpro -march=pentium
		endif
	endif
	ifeq ($(COMPILER),icc)
		OPTIMIZE = -O2
	endif
else
	ifeq ($(COMPILER),icc)
		OPTIMIZE = -O0
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

GCC = gcc
CROSSGCC = "$(TARGETTYPE)-gcc"

STRIP = strip
CROSSSTRIP = "$(TARGETTYPE)-strip"

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
		DEBUG = -g
	endif
	ifeq ($(HOSTTYPE),mingw)
		OSFLAGS = -DMINGW -mno-cygwin
#		          -I/home/mingw/include -L/home/mingw/lib
		DEBUG = -g
	endif
	WARNING = -W -Wall -Wno-uninitialized \
		  -Wconversion -Woverloaded-virtual \
		  -Wsign-promo -Wundef -Wpointer-arith -Wwrite-strings \
                  -Wno-sign-compare -Wfloat-equal -Wundef -Wendif-labels \
		  -Wcast-qual -Wcast-align -Wmissing-format-attribute 
#  -Wold-style-cast: triggered by header files for 2.95/woody
#  -Wmissing-noreturn: triggered by some virtual functions.
#  -Wmissing-prototypes -Wstrict-prototypes: Not C++ flags.
#  -Wuninitialized: triggered in 3.4 in initializations!
#  -Wunreachable-code: triggered by header files
	COMPILE = $(GCC) -ansi -pedantic $(WARNING) $(DEBUG) $(OSFLAGS)
	CCOMPILE = gcc -I/pack/f2c/include -g -Wall
	CPPLIB = -lstdc++
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
ifeq ($(COMPILER),icc)
	COMPILE = /opt/intel/compiler70/ia32/bin/icc -Xc -x c++ -g -w1
	CCOMPILE = /opt/intel/compiler70/ia32/bin/icc -Xc -x c -g -w1
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

ifeq ($(HOSTTYPE),sun4)
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
MOC		= /pack/qt/bin/moc
endif

ifeq ($(HOSTTYPE),i386-linux)
# Locate the Qt library.
#
QTINCLUDE	= -I/usr/include/qt
QTLIB		= -lqt -L/usr/X11R6/lib -lX11 -lm
MOC		= moc
endif

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

ifeq ($(HOSTTYPE),mingw)
	DAISYEXE = /home/abraham/daisy/daisy.exe
else
	DAISYEXE = /usr/local/daisy/$(HOSTTYPE)/daisy
endif

# Figure out how to link.
#
ifeq ($(COMPILER),borland)
	LINK = $(BORLAND)Bin/BCC32 -lw-dup -e
	DLLLINK = $(BORLAND)Bin/BCC32 -WD -lTpd -lw-dup -e
endif
ifeq ($(COMPILER),gcc)
	LINK = $(CC) $(DYNSEARCH) $(DEBUG) -o
endif
ifeq ($(COMPILER),icc)
	LINK = /opt/intel/compiler70/ia32/bin/icc -g -o
endif
NOLINK = -c

# Select the C files that doesn't have a corresponding header file.
# These are all models of some component.
# 
MODELS = uz1d_richard.C difrad_DPF.C \
        difrad_weather.C number_lisp.C condition_extern.C condition_boolean.C \
	boolean_extern.C boolean_number.C boolean_string.C \
	number_extern.C movement_rect.C number_soil.C organic_none.C \
	organic_std.C movement_1D.C integer_arit.C \
	source_merge.C number_source.C program_file.C action_table.C \
	xysource_merge.C xysource_inline.C xysource_loop.C \
	xysource_combine.C gnuplot_xy.C xysource_expr.C gnuplot_multi.C \
	gnuplot_time.C source_combine.C number_arit.C source_expr.C \
	source_std.C action_markvand.C photo_Farquhar.C photo_GL.C \
	program_gnuplot.C \
	program_document.C program_batch.C summary_balance.C \
	rootdens_AP.C number_const.C equil_goal.C pedo_arit.C \
	domsorp_std.C chemistry_std.C equil_linear.C pedo_const.C \
	horizon_numeric.C horizon_system.C select_pF.C pet_FAO_PM.C \
	pet_Hargreaves.C hydraulic_M_vGp.C summary_simple.C \
	phenology_TSum.C phenology_std.C hydraulic_hypres.C clayom_biomod.C \
        clayom_old.C hydraulic_Cosby.C adsorption_full.C \
	equil_langmuir.C transform_equil.C condition_weather.C \
	rootdens_PLF.C rootdens_G_P.C groundwater_file.C action_fertilize.C \
	action_repeat.C vegetation_permanent.C \
	vegetation_crops.C crop_simple.C action_ridge.C groundwater_fixed.C \
	groundwater_deep.C action_heat.C hydraulic_M_vG_compact.C \
	action_crop.C groundwater_lysimeter.C action_message.C weather_std.C \
	select_flux_top.C select_flux_bottom.C groundwater_pipe.C \
	select_index.C select_content.C select_interval.C \
	select_number.C select_array.C log_table.C \
	log_harvest.C action_while.C action_wait.C action_activity.C \
	mactrans_std.C macro_std.C macro_none.C \
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
	condition_soil.C log_checkpoint.C \
	uznone.C condition_daisy.C chemical_std.C \
	hydraulic_M_BaC_Bimodal.C hydraulic_B_BaC_Bimodal.C \
	pet_makkink.C pet_weather.C svat_none.C action_spray.C pet_PM.C \
	svat_pmsw.C action_merge.C action_divide.C \
	action_surface.C

DISABLED = weather_file.C hydraulic_old.C hydraulic_old2.C weather_hourly.C


# A component is a common interface to a number of models.
#
COMPONENTS = uz1d.C difrad.C organic_matter.C movement.C integer.C \
	xysource.C gnuplot.C boolean.C stringer.C source.C photo.C \
	format.C depth.C wse.C program.C number.C domsorp.C chemistry.C \
	summary.C nitrification.C phenology.C clayom.C equil.C pedo.C \
	transform.C rootdens.C select.C average.C mactrans.C macro.C \
	parser.C log.C weather.C column.C crop.C \
	action.C condition.C horizon.C 	uzmodel.C hydraulic.C \
	bioclimate.C groundwater.C am.C transport.C \
	adsorption.C tortuosity.C printer.C chemical.C \
	pet.C net_radiation.C svat.C vegetation.C 

# Submodels are combined models and components.
#
SUBMODELS = geometry_rect.C element.C \
        geometry1d.C fetch.C horheat.C litter.C time.C \
	som.C smb.C aom.C dom.C crpn.C vernalization.C \
	partition.C production.C \
	harvesting.C canopy_simple.C canopy_std.C root_system.C \
	ridge.C soil.C surface.C soil_water.C soil_NH4.C soil_NO3.C \
	denitrification.C soil_heat.C \
	snow.C im.C harvest.C chemicals.C field.C \
	soil_chemical.C soil_chemicals.C bioincorporation.C


# Special or intermediate models with their own interface.
#
SPECIALS = geometry_vert.C select_flux.C gnuplot_base.C \
	source_file.C format_LaTeX.C log_all.C om.C select_value.C \
	weather_old.C log_extern.C log_select.C parser_file.C solute.C \
	geometry.C printer_file.C log_alist.C log_clone.C 

# Various utility code that are neither a component nor a (sub)model.
#
OTHER = scope_multi.C select_utils.C \
	gnuplot_utils.C scope_sources.C scope_table.C lexer_table.C \
	block.C dlf.C scope.C version.C texture.C destination.C symbol.C \
	fao.C gaussj.C vcheck.C assertion.C xref.C treelog_dual.C units.C \
	check.C check_range.C path.C options.C traverse_delete.C \
	depend.C traverse.C treelog.C treelog_stream.C \
	lexer_data.C lexer.C daisy.C alist.C syntax.C library.C plf.C \
	mathlib.C cdaisy.C nrutil.C \
	submodel.C

# Utilities in header alone.
HEADONLY = librarian.h submodeler.h border.h timestep.h memutils.h

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
	cmain.c bugmain.c $(DISABLED) $(MSSRC)
HEADERS = $(INTERFACES:.C=.h) $(QTSOURCES:.C.h) $(HEADONLY)

# Find all printable files.
#
TEXT =  ChangeLog.2 ChangeLog.1 \
	Makefile ChangeLog TODO NEWS COPYING COPYING.LIB \
	$(HEADERS) $(SOURCES) tlink32.ini daisy.bpr daisy.bpf daisy.bpg \
	Daisy.vcproj

# The executables.
#
EXECUTABLES = daisy${EXT} tkdaisy${EXT} cdaisy${EXT} gdaisy${EXT}

# Select files to be removed by the next cvs update.
#
REMOVE = photosynthesis.h photosynthesis.C

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
	$(LINK)daisy.exe $^ $(CPPLIB) $(MATHLIB)

daisy:	main${OBJ} $(LIBOBJ) #daisy.so
	$(LINK)daisy $^ $(CPPLIB) $(MATHLIB)

exp:	
	(cd $(OBJHOME)/exp \
         && $(MAKE) VPATH=$(SRCDIR) USE_PROFILE=true -f $(SRCDIR)/Makefile daisy)

native:	
	(cd $(OBJHOME)/$(HOSTTYPE) \
         && $(MAKE) VPATH=$(SRCDIR) -f $(SRCDIR)/Makefile daisy)

cross:
	(cd $(OBJHOME)/$(TARGETTYPE) \
         && $(MAKE) GCC=$(CROSSGCC) DEBUG= VPATH=$(SRCDIR) \
                    -f $(SRCDIR)/Makefile daisy)

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
	$(LINK)qdaisy $(QTOBJECTS) `pwd`/daisy.so $(QTLIB)

qmain_moc.C:	qmain.h
	$(MOC) $^ > qmain_moc.C

qmain_edit_moc.C:	qmain_edit.h
	$(MOC) $^ > qmain_edit_moc.C

# Create the C main executable.
#
cdaisy${EXT}:  cmain${OBJ} daisy.so
	$(LINK)cdaisy cmain${OBJ} `pwd`/daisy.so $(MATHLIB)

cdaisy-mshe${EXT}:  cmain-mshe${OBJ} daisy.so
	$(LINK)cdaisy-mshe cmain-mshe${OBJ} `pwd`/daisy.so $(MATHLIB)

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

TAGS: $(INTERFACES) $(MODELS) $(MAIN) $(HEADERS)
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

ps:	txt/reference.ps


txt/reference.ps:	txt/reference.dvi
	(cd txt \
	 && dvips -f reference.dvi > reference.ps)

txt/reference.dvi:	txt/components.tex
	(cd txt \
	 && makeindex reference \
	 && latex reference.tex < /dev/null )

pdf:	txt/reference.pdf

txt/reference.pdf:	txt/components.tex
	(cd txt \
	 && makeindex reference \
	 && pdflatex reference.tex < /dev/null )

txt/components.tex:	$(DAISYEXE)
	(cd txt && $(DAISYEXE) all.dai -p document > components.tex)

# Remove all the temporary files.
#
clean:
	rm $(OBJECTS) *.rpo $(EXECUTABLES) *.obj *.exe *.o *~

# Update the Makefile when dependencies have changed.
#
depend: #$(SOURCES) 
	rm -f Makefile.old
	mv Makefile Makefile.old
	sed -e '/^# AUTOMATIC/q' < Makefile.old > Makefile
	$(CC) -I. $(TKINCLUDE) $(GTKMMINCLUDE)  $(QTINCLUDE) \
              -MM $(SOURCES) | sed -e 's/\.o:/$${OBJ}:/' >> Makefile

# Create a ZIP file with all the sources.
#
daisy-src.zip:	$(TEXT)
	rm -f daisy-src.zip
	zip daisy-src.zip $(TEXT) daisy.ide

# Move it to ftp.
#

txtdist:
	(cd txt && $(MAKE) FTPDIR=$(FTPDIR) dist)

dist:	cvs
	$(MAKE) native cross
	mv -f $(WWWINDEX) $(WWWINDEX).old
	sed -e 's/Daisy version [1-9]\.[0-9][0-9]/Daisy version $(TAG)/' \
		< $(WWWINDEX).old > $(WWWINDEX)
	cp cdaisy.h cmain.c ChangeLog NEWS $(FTPDIR)
	$(MAKE) daisy-src.zip
	mv -f daisy-src.zip $(FTPDIR)
	(cd lib && $(MAKE) FTPDIR=$(FTPDIR) TAG=$(TAG) dist)
	(cd sample && $(MAKE) FTPDIR=$(FTPDIR) TAG=$(TAG) dist)
	(cd txt && $(MAKE) FTPDIR=$(FTPDIR) dist)
	(cd exercises && $(MAKE) FTPDIR=$(FTPDIR) dist)
	rm -f $(FTPDIR)/$(HOSTTYPE)/daisy-$(TAG)
	$(STRIP) -o $(FTPDIR)/$(HOSTTYPE)/daisy-$(TAG) \
		$(OBJHOME)/$(HOSTTYPE)/daisy
	rm -f $(FTPDIR)/daisy.exe $(FTPDIR)/$(TARGETTYPE)/daisy-$(TAG).exe
	$(CROSSSTRIP) -o $(FTPDIR)/$(TARGETTYPE)/daisy-$(TAG).exe \
		$(OBJHOME)/$(TARGETTYPE)/daisy
	(cd $(FTPDIR); ln -s $(TARGETTYPE)/daisy-$(TAG).exe daisy.exe)
	(cd exercises && $(MAKE) FTPDIR=$(FTPDIR) dist)
	./utils/update_index $(FTPDIR)
	./utils/update_index $(FTPDIR)/daisy-lib
	./utils/update_index $(FTPDIR)/$(TARGETTYPE)
	./utils/update_index $(FTPDIR)/$(HOSTTYPE)

version.C:
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	echo "// version.C -- automatically generated file" > version.C
	echo " " >> version.C
	echo "extern const char *const version = \"$(TAG)\";" >> version.C
	echo "extern const char *const version_date = __DATE__;" >> version.C

# Update the CVS repository.
#
cvs: $(TEXT)
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	rm -f version.C
	$(MAKE) version.C
	cp ChangeLog ChangeLog.old
	echo `date "+%Y-%m-%d "` \
	     " Per Abrahamsen  <abraham@dina.kvl.dk>" > ChangeLog
	echo >> ChangeLog
	echo "	* Version" $(TAG) released. >> ChangeLog
	echo >> ChangeLog
	cat ChangeLog.old >> ChangeLog
	(cd lib; $(MAKE) cvs);
	(cd sample; $(MAKE) cvs);
	(cd txt; $(MAKE) cvs);
	-cvs add $(TEXT)
	rm -f $(REMOVE) 
	-cvs remove $(REMOVE) 
	cvs commit -m "Version $(TAG)"
	cvs tag release_`echo $(TAG) | sed -e 's/[.]/_/g'`

add:
	cvs add $(TEXT)

cast:
	fgrep _cast $(INTERFACES) $(MODELS) $(MAIN)
	wc -l  $(INTERFACES) $(MODELS) $(MAIN)

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
uz1d${OBJ}: uz1d.C uz1d.h geometry_rect.h geometry_vert.h geometry.h syntax.h \
  treelog.h symbol.h mathlib.h assertion.h soil.h horizon.h librarian.h \
  library.h block.h plf.h alist.h soil_water.h soil_heat.h
difrad${OBJ}: difrad.C difrad.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h
organic_matter${OBJ}: organic_matter.C organic_matter.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h domsorp.h \
  clayom.h
movement${OBJ}: movement.C movement.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h uzmodel.h timestep.h \
  macro.h transport.h mactrans.h
integer${OBJ}: integer.C integer.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h boolean.h submodeler.h \
  memutils.h
xysource${OBJ}: xysource.C xysource.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h
gnuplot${OBJ}: gnuplot.C gnuplot.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h
boolean${OBJ}: boolean.C boolean.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h memutils.h
stringer${OBJ}: stringer.C stringer.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h boolean.h number.h \
  submodeler.h memutils.h
source${OBJ}: source.C source.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h time.h
photo${OBJ}: photo.C photo.h librarian.h library.h symbol.h block.h syntax.h \
  treelog.h plf.h alist.h assertion.h
format${OBJ}: format.C format.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h
depth${OBJ}: depth.C depth.h librarian.h library.h symbol.h block.h syntax.h \
  treelog.h plf.h alist.h assertion.h time.h lexer_data.h lexer.h check.h \
  vcheck.h
wse${OBJ}: wse.C wse.h librarian.h library.h symbol.h block.h syntax.h \
  treelog.h plf.h alist.h assertion.h program.h mathlib.h
program${OBJ}: program.C program.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h
number${OBJ}: number.C number.h symbol.h librarian.h library.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h
domsorp${OBJ}: domsorp.C domsorp.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h
chemistry${OBJ}: chemistry.C chemistry.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h
summary${OBJ}: summary.C summary.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h
nitrification${OBJ}: nitrification.C nitrification.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h soil.h \
  horizon.h soil_water.h soil_heat.h soil_NH4.h solute.h adsorption.h \
  timestep.h soil_NO3.h log.h border.h mathlib.h
phenology${OBJ}: phenology.C phenology.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h log.h border.h
clayom${OBJ}: clayom.C clayom.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h
equil${OBJ}: equil.C equil.h librarian.h library.h symbol.h block.h syntax.h \
  treelog.h plf.h alist.h assertion.h
pedo${OBJ}: pedo.C pedo.h librarian.h library.h symbol.h block.h syntax.h \
  treelog.h plf.h alist.h assertion.h soil.h horizon.h units.h
transform${OBJ}: transform.C transform.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h
rootdens${OBJ}: rootdens.C rootdens.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h check.h
select${OBJ}: select.C select.h destination.h symbol.h condition.h \
  librarian.h library.h block.h syntax.h treelog.h plf.h alist.h \
  assertion.h number.h units.h geometry.h mathlib.h scope.h check.h \
  vcheck.h format.h submodel.h
average${OBJ}: average.C average.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h mathlib.h
mactrans${OBJ}: mactrans.C mactrans.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h
macro${OBJ}: macro.C macro.h librarian.h library.h symbol.h block.h syntax.h \
  treelog.h plf.h alist.h assertion.h
parser${OBJ}: parser.C parser.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h
log${OBJ}: log.C log.h border.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h daisy.h program.h time.h
weather${OBJ}: weather.C weather.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h im.h fao.h time.h log.h \
  border.h mathlib.h
column${OBJ}: column.C column.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h log.h border.h
crop${OBJ}: crop.C crop.h time.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h chemicals.h om.h
action${OBJ}: action.C action.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h
condition${OBJ}: condition.C condition.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h
horizon${OBJ}: horizon.C horizon.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h horheat.h hydraulic.h \
  mathlib.h tortuosity.h texture.h nitrification.h log.h border.h \
  check_range.h check.h vcheck.h
uzmodel${OBJ}: uzmodel.C uzmodel.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h timestep.h
hydraulic${OBJ}: hydraulic.C hydraulic.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h log.h border.h \
  check_range.h check.h mathlib.h program.h vcheck.h
bioclimate${OBJ}: bioclimate.C bioclimate.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h weather.h im.h \
  mathlib.h
groundwater${OBJ}: groundwater.C groundwater.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h geometry.h \
  mathlib.h log.h border.h
am${OBJ}: am.C am.h librarian.h library.h symbol.h block.h syntax.h treelog.h \
  plf.h alist.h assertion.h aom.h om.h im.h submodeler.h time.h log.h \
  border.h geometry.h mathlib.h check.h vcheck.h program.h memutils.h
transport${OBJ}: transport.C transport.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h
adsorption${OBJ}: adsorption.C adsorption.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h
tortuosity${OBJ}: tortuosity.C tortuosity.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h
printer${OBJ}: printer.C printer.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h
chemical${OBJ}: chemical.C chemical.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h
pet${OBJ}: pet.C pet.h librarian.h library.h symbol.h block.h syntax.h \
  treelog.h plf.h alist.h assertion.h log.h border.h vegetation.h \
  surface.h uzmodel.h timestep.h
net_radiation${OBJ}: net_radiation.C net_radiation.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h log.h \
  border.h weather.h im.h mathlib.h
svat${OBJ}: svat.C svat.h librarian.h library.h symbol.h block.h syntax.h \
  treelog.h plf.h alist.h assertion.h log.h border.h
vegetation${OBJ}: vegetation.C vegetation.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h log.h border.h
geometry_rect${OBJ}: geometry_rect.C geometry_rect.h geometry_vert.h \
  geometry.h syntax.h treelog.h symbol.h mathlib.h assertion.h check.h \
  vcheck.h block.h plf.h alist.h submodel.h
element${OBJ}: element.C element.h log.h border.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  geometry.h mathlib.h adsorption.h submodel.h soil.h horizon.h \
  soil_water.h timestep.h
geometry1d${OBJ}: geometry1d.C geometry1d.h geometry_vert.h geometry.h \
  syntax.h treelog.h symbol.h mathlib.h assertion.h block.h plf.h alist.h \
  check.h vcheck.h submodel.h
fetch${OBJ}: fetch.C fetch.h destination.h symbol.h select.h condition.h \
  librarian.h library.h block.h syntax.h treelog.h plf.h alist.h \
  assertion.h number.h units.h mathlib.h
horheat${OBJ}: horheat.C horheat.h texture.h plf.h hydraulic.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h alist.h assertion.h \
  check.h mathlib.h submodel.h
litter${OBJ}: litter.C litter.h submodel.h syntax.h treelog.h symbol.h \
  alist.h check.h
time${OBJ}: time.C time.h assertion.h log.h border.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h vcheck.h submodel.h
som${OBJ}: som.C som.h om.h plf.h submodel.h alist.h symbol.h
smb${OBJ}: smb.C smb.h om.h plf.h dom.h adsorption.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h alist.h assertion.h submodel.h \
  check.h mathlib.h
aom${OBJ}: aom.C aom.h om.h plf.h submodel.h alist.h symbol.h syntax.h \
  treelog.h check.h assertion.h smb.h dom.h adsorption.h librarian.h \
  library.h block.h log.h border.h geometry.h mathlib.h
dom${OBJ}: dom.C dom.h adsorption.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h element.h smb.h om.h \
  geometry.h mathlib.h submodel.h soil.h horizon.h soil_water.h log.h \
  border.h check.h
crpn${OBJ}: crpn.C crpn.h production.h symbol.h root_system.h rootdens.h \
  librarian.h library.h block.h syntax.h treelog.h plf.h alist.h \
  assertion.h log.h border.h mathlib.h submodel.h check.h
vernalization${OBJ}: vernalization.C vernalization.h submodel.h log.h \
  border.h librarian.h library.h symbol.h block.h syntax.h treelog.h \
  plf.h alist.h assertion.h
partition${OBJ}: partition.C partition.h plf.h submodel.h syntax.h treelog.h \
  symbol.h alist.h check.h mathlib.h assertion.h
production${OBJ}: production.C production.h symbol.h crpn.h partition.h plf.h \
  organic_matter.h librarian.h library.h block.h syntax.h treelog.h \
  alist.h assertion.h domsorp.h clayom.h geometry.h mathlib.h am.h log.h \
  border.h time.h submodel.h
harvesting${OBJ}: harvesting.C harvesting.h time.h plf.h symbol.h \
  production.h am.h librarian.h library.h block.h syntax.h treelog.h \
  alist.h assertion.h aom.h om.h crop.h harvest.h chemicals.h geometry.h \
  mathlib.h log.h border.h submodel.h check_range.h check.h
canopy_simple${OBJ}: canopy_simple.C canopy_simple.h plf.h submodel.h log.h \
  border.h librarian.h library.h symbol.h block.h syntax.h treelog.h \
  alist.h assertion.h
canopy_std${OBJ}: canopy_std.C canopy_std.h canopy_simple.h plf.h submodel.h \
  log.h border.h librarian.h library.h symbol.h block.h syntax.h \
  treelog.h alist.h assertion.h mathlib.h
root_system${OBJ}: root_system.C root_system.h rootdens.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  submodel.h geometry.h mathlib.h soil_heat.h soil_NH4.h solute.h \
  adsorption.h timestep.h soil_NO3.h soil_water.h soil.h horizon.h log.h \
  border.h check.h
ridge${OBJ}: ridge.C ridge.h soil.h horizon.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h geometry1d.h \
  geometry_vert.h geometry.h mathlib.h submodel.h log.h border.h \
  soil_water.h check.h timestep.h
soil${OBJ}: soil.C soil.h horizon.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h geometry.h mathlib.h \
  hydraulic.h tortuosity.h groundwater.h submodel.h submodeler.h log.h \
  border.h check.h vcheck.h memutils.h
surface${OBJ}: surface.C surface.h uzmodel.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h timestep.h \
  geometry1d.h geometry_vert.h geometry.h mathlib.h soil.h horizon.h \
  soil_water.h log.h border.h im.h submodel.h chemicals.h \
  soil_chemicals.h soil_chemical.h solute.h adsorption.h ridge.h check.h
soil_water${OBJ}: soil_water.C soil_water.h geometry.h syntax.h treelog.h \
  symbol.h mathlib.h assertion.h soil.h horizon.h librarian.h library.h \
  block.h plf.h alist.h soil_heat.h groundwater.h timestep.h log.h \
  border.h submodel.h
soil_NH4${OBJ}: soil_NH4.C soil_NH4.h solute.h adsorption.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  timestep.h soil.h horizon.h soil_water.h submodel.h
soil_NO3${OBJ}: soil_NO3.C soil_NO3.h solute.h adsorption.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  timestep.h soil.h horizon.h submodel.h
denitrification${OBJ}: denitrification.C denitrification.h plf.h alist.h \
  symbol.h syntax.h treelog.h geometry.h mathlib.h assertion.h soil.h \
  horizon.h librarian.h library.h block.h soil_water.h soil_heat.h \
  organic_matter.h domsorp.h clayom.h soil_NO3.h solute.h adsorption.h \
  timestep.h log.h border.h submodel.h check.h
soil_heat${OBJ}: soil_heat.C soil_heat.h alist.h symbol.h geometry.h syntax.h \
  treelog.h mathlib.h assertion.h soil.h horizon.h librarian.h library.h \
  block.h plf.h soil_water.h weather.h im.h timestep.h log.h border.h \
  submodel.h
snow${OBJ}: snow.C snow.h alist.h symbol.h syntax.h treelog.h log.h border.h \
  librarian.h library.h block.h plf.h assertion.h geometry.h mathlib.h \
  soil.h horizon.h soil_water.h soil_heat.h movement.h uzmodel.h \
  timestep.h macro.h transport.h mactrans.h submodel.h
im${OBJ}: im.C im.h am.h librarian.h library.h symbol.h block.h syntax.h \
  treelog.h plf.h alist.h assertion.h log.h border.h submodel.h
harvest${OBJ}: harvest.C harvest.h chemicals.h symbol.h syntax.h treelog.h \
  time.h block.h plf.h log.h border.h librarian.h library.h alist.h \
  assertion.h submodel.h
chemicals${OBJ}: chemicals.C chemicals.h symbol.h syntax.h treelog.h log.h \
  border.h librarian.h library.h block.h plf.h alist.h assertion.h \
  chemical.h submodel.h mathlib.h timestep.h
field${OBJ}: field.C field.h border.h symbol.h column.h librarian.h library.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h log.h log_clone.h \
  log_alist.h memutils.h
soil_chemical${OBJ}: soil_chemical.C soil_chemical.h solute.h adsorption.h \
  librarian.h library.h symbol.h block.h syntax.h treelog.h plf.h alist.h \
  assertion.h timestep.h chemical.h geometry.h mathlib.h soil.h horizon.h \
  soil_heat.h soil_water.h organic_matter.h domsorp.h clayom.h log.h \
  border.h submodel.h
soil_chemicals${OBJ}: soil_chemicals.C soil_chemicals.h soil_chemical.h \
  solute.h adsorption.h librarian.h library.h symbol.h block.h syntax.h \
  treelog.h plf.h alist.h assertion.h timestep.h geometry.h mathlib.h \
  soil.h horizon.h soil_water.h soil_heat.h organic_matter.h domsorp.h \
  clayom.h chemical.h chemicals.h log.h border.h submodel.h memutils.h
bioincorporation${OBJ}: bioincorporation.C bioincorporation.h alist.h \
  symbol.h syntax.h treelog.h log.h border.h librarian.h library.h \
  block.h plf.h assertion.h geometry.h mathlib.h soil.h horizon.h am.h \
  submodel.h time.h aom.h om.h check.h vcheck.h timestep.h
geometry_vert${OBJ}: geometry_vert.C geometry_vert.h geometry.h syntax.h \
  treelog.h symbol.h mathlib.h assertion.h block.h plf.h
select_flux${OBJ}: select_flux.C select_flux.h select_value.h select.h \
  destination.h symbol.h condition.h librarian.h library.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h number.h units.h \
  geometry.h mathlib.h
gnuplot_base${OBJ}: gnuplot_base.C gnuplot_base.h gnuplot.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  vcheck.h
source_file${OBJ}: source_file.C source_file.h source.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h time.h \
  lexer_table.h gnuplot_utils.h vcheck.h mathlib.h
format_LaTeX${OBJ}: format_LaTeX.C format_LaTeX.h format.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  version.h
log_all${OBJ}: log_all.C log_all.h log_select.h log.h border.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  select.h destination.h condition.h number.h units.h
om${OBJ}: om.C om.h plf.h som.h smb.h dom.h adsorption.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h alist.h assertion.h \
  check.h vcheck.h geometry.h mathlib.h log.h border.h
select_value${OBJ}: select_value.C select_value.h select.h destination.h \
  symbol.h condition.h librarian.h library.h block.h syntax.h treelog.h \
  plf.h alist.h assertion.h number.h units.h mathlib.h
weather_old${OBJ}: weather_old.C weather_old.h weather.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  im.h fao.h time.h
log_extern${OBJ}: log_extern.C log_extern.h symbol.h log_select.h log.h \
  border.h librarian.h library.h block.h syntax.h treelog.h plf.h alist.h \
  assertion.h select.h destination.h condition.h number.h units.h scope.h
log_select${OBJ}: log_select.C log_select.h log.h border.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  select.h destination.h condition.h number.h units.h field.h format.h \
  memutils.h
parser_file${OBJ}: parser_file.C parser_file.h parser.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h lexer.h \
  submodel.h scope.h number.h integer.h time.h treelog_stream.h path.h \
  units.h mathlib.h memutils.h
solute${OBJ}: solute.C solute.h adsorption.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h timestep.h log.h \
  border.h geometry.h mathlib.h soil.h horizon.h soil_water.h
geometry${OBJ}: geometry.C geometry.h syntax.h treelog.h symbol.h mathlib.h \
  assertion.h alist.h check.h vcheck.h
printer_file${OBJ}: printer_file.C printer_file.h printer.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  time.h parser.h path.h
log_alist${OBJ}: log_alist.C log_alist.h log.h border.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h
log_clone${OBJ}: log_clone.C log_clone.h log_alist.h log.h border.h \
  librarian.h library.h symbol.h block.h syntax.h treelog.h plf.h alist.h \
  assertion.h
scope_multi${OBJ}: scope_multi.C scope_multi.h scope.h symbol.h assertion.h
select_utils${OBJ}: select_utils.C select_utils.h border.h block.h syntax.h \
  treelog.h symbol.h plf.h check.h mathlib.h assertion.h
gnuplot_utils${OBJ}: gnuplot_utils.C gnuplot_utils.h syntax.h treelog.h \
  symbol.h alist.h
scope_sources${OBJ}: scope_sources.C scope_sources.h scope.h symbol.h time.h \
  source.h librarian.h library.h block.h syntax.h treelog.h plf.h alist.h \
  assertion.h memutils.h
scope_table${OBJ}: scope_table.C scope_table.h scope.h symbol.h lexer_table.h \
  block.h syntax.h treelog.h plf.h assertion.h
lexer_table${OBJ}: lexer_table.C lexer_table.h block.h syntax.h treelog.h \
  symbol.h plf.h lexer_data.h lexer.h alist.h assertion.h mathlib.h \
  submodeler.h memutils.h time.h vcheck.h
block${OBJ}: block.C block.h syntax.h treelog.h symbol.h plf.h librarian.h \
  library.h alist.h assertion.h stringer.h number.h scope.h
dlf${OBJ}: dlf.C dlf.h symbol.h alist.h assertion.h version.h daisy.h \
  program.h librarian.h library.h block.h syntax.h treelog.h plf.h time.h
scope${OBJ}: scope.C scope.h symbol.h assertion.h
version${OBJ}: version.C
texture${OBJ}: texture.C texture.h plf.h assertion.h mathlib.h
destination${OBJ}: destination.C destination.h symbol.h
symbol${OBJ}: symbol.C symbol.h assertion.h
fao${OBJ}: fao.C fao.h net_radiation.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h mathlib.h
gaussj${OBJ}: gaussj.C gaussj.h mathlib.h assertion.h
vcheck${OBJ}: vcheck.C vcheck.h units.h symbol.h syntax.h treelog.h alist.h \
  time.h plf.h assertion.h mathlib.h
assertion${OBJ}: assertion.C assertion.h treelog.h symbol.h mathlib.h
xref${OBJ}: xref.C xref.h symbol.h traverse.h library.h syntax.h treelog.h \
  alist.h submodel.h assertion.h
treelog_dual${OBJ}: treelog_dual.C treelog_dual.h treelog.h symbol.h \
  assertion.h
units${OBJ}: units.C units.h symbol.h syntax.h treelog.h mathlib.h \
  assertion.h memutils.h
check${OBJ}: check.C check.h mathlib.h assertion.h
check_range${OBJ}: check_range.C check_range.h check.h
path${OBJ}: path.C path.h assertion.h
options${OBJ}: options.C options.h parser_file.h parser.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  program.h treelog_stream.h version.h path.h
traverse_delete${OBJ}: traverse_delete.C traverse_delete.h symbol.h \
  traverse.h library.h syntax.h treelog.h alist.h assertion.h
depend${OBJ}: depend.C depend.h symbol.h traverse.h library.h syntax.h \
  treelog.h alist.h assertion.h
traverse${OBJ}: traverse.C traverse.h symbol.h library.h syntax.h treelog.h \
  alist.h submodel.h assertion.h
treelog${OBJ}: treelog.C treelog.h symbol.h
treelog_stream${OBJ}: treelog_stream.C treelog_stream.h treelog.h symbol.h \
  assertion.h
lexer_data${OBJ}: lexer_data.C lexer_data.h lexer.h time.h mathlib.h \
  assertion.h
lexer${OBJ}: lexer.C lexer.h treelog.h symbol.h path.h
daisy${OBJ}: daisy.C daisy.h program.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h time.h weather.h im.h \
  groundwater.h horizon.h log_all.h log_select.h log.h border.h select.h \
  destination.h condition.h number.h units.h parser.h nitrification.h \
  bioclimate.h hydraulic.h field.h harvest.h chemicals.h action.h \
  submodeler.h column.h memutils.h
alist${OBJ}: alist.C plf.h library.h symbol.h alist.h syntax.h treelog.h \
  time.h memutils.h assertion.h
syntax${OBJ}: syntax.C syntax.h treelog.h symbol.h alist.h library.h check.h \
  vcheck.h assertion.h memutils.h
library${OBJ}: library.C library.h symbol.h alist.h syntax.h treelog.h \
  assertion.h memutils.h options.h
plf${OBJ}: plf.C plf.h assertion.h
mathlib${OBJ}: mathlib.C mathlib.h assertion.h
cdaisy${OBJ}: cdaisy.C syntax.h treelog.h symbol.h alist.h daisy.h program.h \
  librarian.h library.h block.h plf.h assertion.h time.h parser_file.h \
  parser.h field.h border.h column.h weather.h im.h action.h horizon.h \
  printer_file.h printer.h version.h options.h chemical.h log_extern.h \
  treelog_stream.h
nrutil${OBJ}: nrutil.C
submodel${OBJ}: submodel.C submodel.h syntax.h treelog.h symbol.h alist.h \
  assertion.h
uz1d_richard${OBJ}: uz1d_richard.C uz1d.h geometry_rect.h geometry_vert.h \
  geometry.h syntax.h treelog.h symbol.h mathlib.h assertion.h soil.h \
  horizon.h librarian.h library.h block.h plf.h alist.h soil_water.h \
  soil_heat.h average.h timestep.h
difrad_DPF${OBJ}: difrad_DPF.C difrad.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h weather.h im.h \
  fao.h mathlib.h check.h
difrad_weather${OBJ}: difrad_weather.C difrad.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h weather.h \
  im.h mathlib.h
number_lisp${OBJ}: number_lisp.C number.h symbol.h librarian.h library.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h scope_multi.h \
  scope.h submodeler.h memutils.h
condition_extern${OBJ}: condition_extern.C condition.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h boolean.h \
  log_extern.h scope.h
condition_boolean${OBJ}: condition_boolean.C condition.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  boolean.h scope.h
boolean_extern${OBJ}: boolean_extern.C boolean.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  log_extern.h scope_multi.h scope.h
boolean_number${OBJ}: boolean_number.C boolean.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h number.h \
  memutils.h
boolean_string${OBJ}: boolean_string.C boolean.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h
number_extern${OBJ}: number_extern.C number.h symbol.h librarian.h library.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h log_extern.h \
  scope_multi.h scope.h
movement_rect${OBJ}: movement_rect.C movement.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h uzmodel.h \
  timestep.h macro.h transport.h mactrans.h geometry_rect.h \
  geometry_vert.h geometry.h mathlib.h soil.h horizon.h soil_water.h \
  uz1d.h soil_heat.h solute.h adsorption.h element.h surface.h \
  groundwater.h weather.h im.h log.h border.h submodeler.h
number_soil${OBJ}: number_soil.C number.h symbol.h librarian.h library.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h column.h horizon.h \
  hydraulic.h weather.h im.h time.h units.h
organic_none${OBJ}: organic_none.C organic_matter.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h domsorp.h \
  clayom.h geometry.h mathlib.h
organic_std${OBJ}: organic_std.C organic_matter.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h domsorp.h \
  clayom.h submodeler.h log.h border.h am.h om.h som.h smb.h dom.h \
  adsorption.h aom.h soil.h horizon.h geometry.h mathlib.h soil_water.h \
  soil_NH4.h solute.h timestep.h soil_NO3.h soil_heat.h \
  bioincorporation.h time.h check_range.h check.h vcheck.h gaussj.h \
  memutils.h
movement_1D${OBJ}: movement_1D.C movement.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h uzmodel.h \
  timestep.h macro.h transport.h mactrans.h geometry1d.h geometry_vert.h \
  geometry.h mathlib.h soil.h horizon.h soil_water.h soil_heat.h \
  groundwater.h surface.h weather.h im.h solute.h adsorption.h element.h \
  log.h border.h submodeler.h
integer_arit${OBJ}: integer_arit.C integer.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h vcheck.h \
  memutils.h
source_merge${OBJ}: source_merge.C source.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h time.h \
  gnuplot_utils.h units.h vcheck.h mathlib.h memutils.h
number_source${OBJ}: number_source.C number.h symbol.h librarian.h library.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h source.h time.h
program_file${OBJ}: program_file.C program.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h path.h
action_table${OBJ}: action_table.C action.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h daisy.h program.h \
  time.h field.h border.h am.h im.h lexer_table.h mathlib.h
xysource_merge${OBJ}: xysource_merge.C xysource.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  gnuplot_utils.h number.h scope_sources.h scope.h time.h units.h \
  vcheck.h memutils.h
xysource_inline${OBJ}: xysource_inline.C xysource.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  gnuplot_utils.h number.h vcheck.h
xysource_loop${OBJ}: xysource_loop.C xysource.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  gnuplot_utils.h scope.h number.h check.h vcheck.h
xysource_combine${OBJ}: xysource_combine.C xysource.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  gnuplot_utils.h number.h scope_sources.h scope.h time.h
gnuplot_xy${OBJ}: gnuplot_xy.C gnuplot_base.h gnuplot.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  xysource.h mathlib.h memutils.h
xysource_expr${OBJ}: xysource_expr.C xysource.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  lexer_table.h scope_table.h scope.h gnuplot_utils.h number.h vcheck.h
gnuplot_multi${OBJ}: gnuplot_multi.C gnuplot.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h source.h time.h \
  memutils.h
gnuplot_time${OBJ}: gnuplot_time.C gnuplot_base.h gnuplot.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  source.h time.h mathlib.h memutils.h
source_combine${OBJ}: source_combine.C source.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h time.h \
  number.h scope_sources.h scope.h gnuplot_utils.h vcheck.h
number_arit${OBJ}: number_arit.C number.h symbol.h librarian.h library.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h units.h vcheck.h \
  mathlib.h memutils.h
source_expr${OBJ}: source_expr.C source_file.h source.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h time.h \
  lexer_table.h scope_table.h scope.h number.h
source_std${OBJ}: source_std.C source_file.h source.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h time.h \
  lexer_table.h units.h
action_markvand${OBJ}: action_markvand.C action.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h daisy.h \
  program.h time.h field.h border.h crop.h im.h fao.h log.h mathlib.h \
  check.h vcheck.h memutils.h
photo_Farquhar${OBJ}: photo_Farquhar.C photo.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h bioclimate.h \
  canopy_std.h canopy_simple.h phenology.h log.h border.h submodel.h \
  mathlib.h check.h
photo_GL${OBJ}: photo_GL.C photo.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h canopy_std.h \
  canopy_simple.h phenology.h submodel.h mathlib.h check.h
program_gnuplot${OBJ}: program_gnuplot.C program.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h gnuplot.h \
  path.h memutils.h
program_document${OBJ}: program_document.C program.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  submodel.h printer_file.h printer.h xref.h format.h
program_batch${OBJ}: program_batch.C program.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h path.h memutils.h
summary_balance${OBJ}: summary_balance.C summary.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h fetch.h \
  destination.h select.h condition.h number.h units.h memutils.h
rootdens_AP${OBJ}: rootdens_AP.C rootdens.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h geometry.h \
  mathlib.h log.h border.h check.h
number_const${OBJ}: number_const.C number.h symbol.h librarian.h library.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h scope.h units.h
equil_goal${OBJ}: equil_goal.C equil.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h soil_water.h pedo.h soil.h \
  horizon.h check.h mathlib.h
pedo_arit${OBJ}: pedo_arit.C pedo.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h soil.h horizon.h \
  treelog_stream.h submodeler.h units.h vcheck.h mathlib.h memutils.h
domsorp_std${OBJ}: domsorp_std.C domsorp.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h transform.h dom.h \
  adsorption.h som.h om.h soil.h horizon.h log.h border.h
chemistry_std${OBJ}: chemistry_std.C chemistry.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  transform.h soil_chemicals.h soil_chemical.h solute.h adsorption.h \
  timestep.h soil.h horizon.h log.h border.h
equil_linear${OBJ}: equil_linear.C equil.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h pedo.h soil.h \
  horizon.h check.h mathlib.h
pedo_const${OBJ}: pedo_const.C pedo.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h soil.h horizon.h units.h \
  vcheck.h
horizon_numeric${OBJ}: horizon_numeric.C horizon.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h texture.h \
  hydraulic.h check.h vcheck.h mathlib.h
horizon_system${OBJ}: horizon_system.C horizon.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h texture.h \
  hydraulic.h check.h mathlib.h
select_pF${OBJ}: select_pF.C select.h destination.h symbol.h condition.h \
  librarian.h library.h block.h syntax.h treelog.h plf.h alist.h \
  assertion.h number.h units.h mathlib.h check.h vcheck.h
pet_FAO_PM${OBJ}: pet_FAO_PM.C pet.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h fao.h weather.h im.h \
  soil.h horizon.h surface.h uzmodel.h timestep.h soil_heat.h \
  net_radiation.h vegetation.h log.h border.h
pet_Hargreaves${OBJ}: pet_Hargreaves.C pet.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h weather.h im.h \
  fao.h log.h border.h mathlib.h
hydraulic_M_vGp${OBJ}: hydraulic_M_vGp.C hydraulic.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h mathlib.h \
  check.h
summary_simple${OBJ}: summary_simple.C summary.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h fetch.h \
  destination.h select.h condition.h number.h units.h memutils.h \
  submodeler.h
phenology_TSum${OBJ}: phenology_TSum.C phenology.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  production.h vernalization.h
phenology_std${OBJ}: phenology_std.C phenology.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  production.h vernalization.h mathlib.h
hydraulic_hypres${OBJ}: hydraulic_hypres.C hydraulic.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h texture.h \
  mathlib.h
clayom_biomod${OBJ}: clayom_biomod.C clayom.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h check.h smb.h om.h \
  soil.h horizon.h mathlib.h
clayom_old${OBJ}: clayom_old.C clayom.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h smb.h om.h soil.h \
  horizon.h
hydraulic_Cosby${OBJ}: hydraulic_Cosby.C hydraulic.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h texture.h \
  mathlib.h
adsorption_full${OBJ}: adsorption_full.C adsorption.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h mathlib.h
equil_langmuir${OBJ}: equil_langmuir.C equil.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h pedo.h soil.h \
  horizon.h check.h mathlib.h
transform_equil${OBJ}: transform_equil.C transform.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h soil.h \
  horizon.h soil_water.h pedo.h equil.h check.h mathlib.h
condition_weather${OBJ}: condition_weather.C condition.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  field.h border.h daisy.h program.h time.h check.h log.h
rootdens_PLF${OBJ}: rootdens_PLF.C rootdens.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h geometry.h \
  mathlib.h submodeler.h check.h vcheck.h memutils.h
rootdens_G_P${OBJ}: rootdens_G_P.C rootdens.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h geometry.h \
  mathlib.h log.h border.h check.h
groundwater_file${OBJ}: groundwater_file.C groundwater.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  lexer_data.h lexer.h time.h
action_fertilize${OBJ}: action_fertilize.C action.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h daisy.h \
  program.h time.h field.h border.h am.h im.h check.h
action_repeat${OBJ}: action_repeat.C action.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h log.h border.h
vegetation_permanent${OBJ}: vegetation_permanent.C vegetation.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  mathlib.h log.h border.h litter.h root_system.h rootdens.h \
  canopy_simple.h time.h geometry.h soil.h horizon.h crop.h am.h aom.h \
  om.h organic_matter.h domsorp.h clayom.h submodeler.h check.h
vegetation_crops${OBJ}: vegetation_crops.C vegetation.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h crop.h \
  time.h organic_matter.h domsorp.h clayom.h geometry.h mathlib.h soil.h \
  horizon.h harvest.h chemicals.h log.h border.h
crop_simple${OBJ}: crop_simple.C crop.h time.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h root_system.h \
  rootdens.h canopy_simple.h log.h border.h bioclimate.h soil_water.h \
  geometry.h mathlib.h soil.h horizon.h aom.h om.h organic_matter.h \
  domsorp.h clayom.h soil_heat.h soil_NH4.h solute.h adsorption.h \
  timestep.h soil_NO3.h am.h harvest.h chemicals.h submodeler.h check.h
action_ridge${OBJ}: action_ridge.C action.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h daisy.h program.h \
  time.h field.h border.h ridge.h
groundwater_fixed${OBJ}: groundwater_fixed.C groundwater.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  check.h
groundwater_deep${OBJ}: groundwater_deep.C groundwater.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h
action_heat${OBJ}: action_heat.C action.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h daisy.h program.h \
  time.h field.h border.h check.h
hydraulic_M_vG_compact${OBJ}: hydraulic_M_vG_compact.C hydraulic.h \
  librarian.h library.h symbol.h block.h syntax.h treelog.h plf.h alist.h \
  assertion.h mathlib.h
action_crop${OBJ}: action_crop.C action.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h daisy.h program.h \
  time.h field.h border.h crop.h am.h log.h harvest.h chemicals.h \
  check_range.h check.h im.h submodeler.h vcheck.h memutils.h
groundwater_lysimeter${OBJ}: groundwater_lysimeter.C groundwater.h \
  librarian.h library.h symbol.h block.h syntax.h treelog.h plf.h alist.h \
  assertion.h geometry.h mathlib.h
action_message${OBJ}: action_message.C action.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  condition.h log.h border.h daisy.h program.h time.h
weather_std${OBJ}: weather_std.C weather.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h im.h fao.h \
  lexer_data.h lexer.h time.h mathlib.h units.h submodeler.h check.h \
  vcheck.h memutils.h
select_flux_top${OBJ}: select_flux_top.C select_flux.h select_value.h \
  select.h destination.h symbol.h condition.h librarian.h library.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h number.h units.h \
  border.h mathlib.h
select_flux_bottom${OBJ}: select_flux_bottom.C select_flux.h select_value.h \
  select.h destination.h symbol.h condition.h librarian.h library.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h number.h units.h \
  border.h mathlib.h
groundwater_pipe${OBJ}: groundwater_pipe.C groundwater.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  log.h border.h geometry.h mathlib.h soil.h horizon.h soil_heat.h \
  soil_water.h depth.h check.h
select_index${OBJ}: select_index.C select_value.h select.h destination.h \
  symbol.h condition.h librarian.h library.h block.h syntax.h treelog.h \
  plf.h alist.h assertion.h number.h units.h
select_content${OBJ}: select_content.C select_value.h select.h destination.h \
  symbol.h condition.h librarian.h library.h block.h syntax.h treelog.h \
  plf.h alist.h assertion.h number.h units.h geometry.h mathlib.h soil.h \
  horizon.h check.h
select_interval${OBJ}: select_interval.C select_value.h select.h \
  destination.h symbol.h condition.h librarian.h library.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h number.h units.h \
  select_utils.h geometry.h mathlib.h soil.h horizon.h
select_number${OBJ}: select_number.C select_value.h select.h destination.h \
  symbol.h condition.h librarian.h library.h block.h syntax.h treelog.h \
  plf.h alist.h assertion.h number.h units.h
select_array${OBJ}: select_array.C select.h destination.h symbol.h \
  condition.h librarian.h library.h block.h syntax.h treelog.h plf.h \
  alist.h assertion.h number.h units.h mathlib.h
log_table${OBJ}: log_table.C log_select.h log.h border.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  select.h destination.h condition.h number.h units.h summary.h \
  geometry.h mathlib.h dlf.h daisy.h program.h time.h vcheck.h memutils.h
log_harvest${OBJ}: log_harvest.C log.h border.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h daisy.h \
  program.h time.h harvest.h chemicals.h dlf.h vcheck.h version.h
action_while${OBJ}: action_while.C action.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h log.h border.h \
  memutils.h
action_wait${OBJ}: action_wait.C action.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h condition.h log.h \
  border.h daisy.h program.h time.h
action_activity${OBJ}: action_activity.C action.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h log.h \
  border.h memutils.h
mactrans_std${OBJ}: mactrans_std.C mactrans.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h soil_water.h \
  geometry1d.h geometry_vert.h geometry.h mathlib.h timestep.h
macro_std${OBJ}: macro_std.C macro.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h geometry1d.h \
  geometry_vert.h geometry.h mathlib.h soil.h horizon.h surface.h \
  uzmodel.h timestep.h log.h border.h check.h vcheck.h
macro_none${OBJ}: macro_none.C macro.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h
column_std${OBJ}: column_std.C column.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h surface.h \
  uzmodel.h timestep.h soil_heat.h soil_chemicals.h soil_chemical.h \
  solute.h adsorption.h movement.h macro.h transport.h mactrans.h \
  groundwater.h geometry.h mathlib.h soil.h horizon.h soil_water.h \
  vegetation.h bioclimate.h weather.h im.h chemistry.h chemicals.h \
  soil_NH4.h soil_NO3.h organic_matter.h domsorp.h clayom.h \
  denitrification.h am.h dom.h time.h log.h border.h submodeler.h \
  memutils.h
weather_simple${OBJ}: weather_simple.C weather_old.h weather.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  im.h time.h log.h border.h mathlib.h
uzrichard${OBJ}: uzrichard.C uzmodel.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h timestep.h groundwater.h \
  surface.h geometry_vert.h geometry.h mathlib.h soil.h horizon.h \
  soil_heat.h log.h border.h average.h
hydraulic_yolo${OBJ}: hydraulic_yolo.C hydraulic.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h mathlib.h
hydraulic_M_vG${OBJ}: hydraulic_M_vG.C hydraulic.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h mathlib.h
hydraulic_B_vG${OBJ}: hydraulic_B_vG.C hydraulic.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h mathlib.h
hydraulic_M_C${OBJ}: hydraulic_M_C.C hydraulic.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h check.h \
  mathlib.h
hydraulic_B_C${OBJ}: hydraulic_B_C.C hydraulic.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h check.h \
  mathlib.h
hydraulic_M_BaC${OBJ}: hydraulic_M_BaC.C hydraulic.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h check.h \
  mathlib.h
hydraulic_B_BaC${OBJ}: hydraulic_B_BaC.C hydraulic.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h mathlib.h
groundwater_static${OBJ}: groundwater_static.C groundwater.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h
horizon_std${OBJ}: horizon_std.C horizon.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h texture.h \
  hydraulic.h check.h mathlib.h
crop_std${OBJ}: crop_std.C crop.h time.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h chemicals.h \
  root_system.h rootdens.h canopy_std.h canopy_simple.h harvesting.h \
  production.h phenology.h partition.h vernalization.h photo.h crpn.h \
  wse.h log.h border.h bioclimate.h soil_water.h geometry.h mathlib.h \
  soil.h horizon.h organic_matter.h domsorp.h clayom.h soil_heat.h am.h \
  submodeler.h
action_sow${OBJ}: action_sow.C action.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h daisy.h program.h \
  time.h field.h border.h crop.h
action_stop${OBJ}: action_stop.C action.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h daisy.h program.h \
  time.h
condition_time${OBJ}: condition_time.C condition.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h time.h \
  daisy.h program.h vcheck.h
condition_logic${OBJ}: condition_logic.C condition.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  memutils.h
action_irrigate${OBJ}: action_irrigate.C action.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h daisy.h \
  program.h time.h field.h border.h im.h check.h
action_lisp${OBJ}: action_lisp.C action.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h daisy.h program.h \
  time.h log.h border.h memutils.h submodeler.h condition.h
weather_none${OBJ}: weather_none.C weather_old.h weather.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  im.h
action_tillage${OBJ}: action_tillage.C action.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h daisy.h \
  program.h time.h field.h border.h check.h
action_harvest${OBJ}: action_harvest.C action.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h daisy.h \
  program.h time.h field.h border.h
crop_old${OBJ}: crop_old.C crop.h time.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h log.h border.h \
  bioclimate.h soil_water.h soil.h horizon.h geometry.h mathlib.h aom.h \
  om.h organic_matter.h domsorp.h clayom.h soil_heat.h soil_NH4.h \
  solute.h adsorption.h timestep.h soil_NO3.h am.h harvest.h chemicals.h
crop_sold${OBJ}: crop_sold.C crop.h time.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h log.h border.h \
  bioclimate.h soil_water.h soil.h horizon.h geometry.h mathlib.h \
  organic_matter.h domsorp.h clayom.h aom.h om.h soil_heat.h soil_NH4.h \
  solute.h adsorption.h timestep.h soil_NO3.h am.h harvest.h chemicals.h
action_with${OBJ}: action_with.C action.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h daisy.h program.h \
  time.h field.h border.h log.h memutils.h
nitrification_soil${OBJ}: nitrification_soil.C nitrification.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  mathlib.h check.h timestep.h
nitrification_solute${OBJ}: nitrification_solute.C nitrification.h \
  librarian.h library.h symbol.h block.h syntax.h treelog.h plf.h alist.h \
  assertion.h soil.h horizon.h soil_water.h soil_heat.h soil_NH4.h \
  solute.h adsorption.h timestep.h soil_NO3.h mathlib.h check.h
hydraulic_mod_C${OBJ}: hydraulic_mod_C.C hydraulic.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h check.h \
  mathlib.h
uzlr${OBJ}: uzlr.C uzmodel.h librarian.h library.h symbol.h block.h syntax.h \
  treelog.h plf.h alist.h assertion.h timestep.h surface.h groundwater.h \
  geometry_vert.h geometry.h mathlib.h soil.h horizon.h soil_heat.h
transport_cd${OBJ}: transport_cd.C transport.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h geometry1d.h \
  geometry_vert.h geometry.h mathlib.h soil.h horizon.h soil_water.h \
  adsorption.h log.h border.h timestep.h
transport_none${OBJ}: transport_none.C transport.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  geometry1d.h geometry_vert.h geometry.h mathlib.h soil.h horizon.h \
  soil_water.h adsorption.h log.h border.h timestep.h
transport_convection${OBJ}: transport_convection.C transport.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  geometry1d.h geometry_vert.h geometry.h mathlib.h soil.h horizon.h \
  soil_water.h adsorption.h log.h border.h timestep.h
adsorption_vS_S${OBJ}: adsorption_vS_S.C adsorption.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h soil.h \
  horizon.h mathlib.h
adsorption_none${OBJ}: adsorption_none.C adsorption.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h
tortuosity_M_Q${OBJ}: tortuosity_M_Q.C tortuosity.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  hydraulic.h mathlib.h
tortuosity_linear${OBJ}: tortuosity_linear.C tortuosity.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  hydraulic.h
adsorption_freundlich${OBJ}: adsorption_freundlich.C adsorption.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  soil.h horizon.h check.h mathlib.h
adsorption_linear${OBJ}: adsorption_linear.C adsorption.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  check.h soil.h horizon.h
adsorption_langmuir${OBJ}: adsorption_langmuir.C adsorption.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  soil.h horizon.h check.h mathlib.h
bioclimate_std${OBJ}: bioclimate_std.C bioclimate.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h surface.h \
  uzmodel.h timestep.h weather.h im.h geometry.h mathlib.h soil.h \
  horizon.h soil_heat.h snow.h log.h border.h pet.h difrad.h svat.h \
  vegetation.h chemicals.h time.h
condition_crop${OBJ}: condition_crop.C condition.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h crop.h \
  time.h field.h border.h daisy.h program.h check_range.h check.h
condition_soil${OBJ}: condition_soil.C condition.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h field.h \
  border.h daisy.h program.h time.h check.h
log_checkpoint${OBJ}: log_checkpoint.C log_alist.h log.h border.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  condition.h daisy.h program.h time.h printer_file.h printer.h
uznone${OBJ}: uznone.C uzmodel.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h timestep.h soil.h \
  horizon.h mathlib.h
condition_daisy${OBJ}: condition_daisy.C condition.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h daisy.h \
  program.h time.h
chemical_std${OBJ}: chemical_std.C chemical.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h mathlib.h \
  soil_chemical.h solute.h adsorption.h timestep.h check.h
hydraulic_M_BaC_Bimodal${OBJ}: hydraulic_M_BaC_Bimodal.C hydraulic.h \
  librarian.h library.h symbol.h block.h syntax.h treelog.h plf.h alist.h \
  assertion.h check.h mathlib.h
hydraulic_B_BaC_Bimodal${OBJ}: hydraulic_B_BaC_Bimodal.C hydraulic.h \
  librarian.h library.h symbol.h block.h syntax.h treelog.h plf.h alist.h \
  assertion.h check.h mathlib.h
pet_makkink${OBJ}: pet_makkink.C pet.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h weather.h im.h fao.h log.h \
  border.h
pet_weather${OBJ}: pet_weather.C pet.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h weather.h im.h log.h \
  border.h
svat_none${OBJ}: svat_none.C svat.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h
action_spray${OBJ}: action_spray.C action.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h daisy.h program.h \
  time.h field.h border.h chemical.h check.h
pet_PM${OBJ}: pet_PM.C pet.h librarian.h library.h symbol.h block.h syntax.h \
  treelog.h plf.h alist.h assertion.h fao.h weather.h im.h soil.h \
  horizon.h surface.h uzmodel.h timestep.h soil_heat.h net_radiation.h \
  vegetation.h log.h border.h
svat_pmsw${OBJ}: svat_pmsw.C mathlib.h assertion.h svat.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h surface.h \
  uzmodel.h timestep.h weather.h im.h time.h soil.h horizon.h \
  soil_water.h soil_heat.h vegetation.h pet.h log.h border.h fao.h \
  gaussj.h nrutil.h
action_merge${OBJ}: action_merge.C action.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h daisy.h program.h \
  time.h field.h border.h
action_divide${OBJ}: action_divide.C action.h librarian.h library.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h daisy.h program.h \
  time.h field.h border.h
action_surface${OBJ}: action_surface.C action.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h daisy.h \
  program.h time.h field.h border.h check.h
main${OBJ}: main.C daisy.h program.h librarian.h library.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h time.h parser.h \
  treelog_dual.h options.h
qmain_edit_moc${OBJ}: qmain_edit_moc.C
cmain${OBJ}: cmain.c cdaisy.h
bugmain${OBJ}: bugmain.c cdaisy.h
weather_file${OBJ}: weather_file.C weather_old.h weather.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  im.h time.h log.h border.h
hydraulic_old${OBJ}: hydraulic_old.C hydraulic.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h mathlib.h
hydraulic_old2${OBJ}: hydraulic_old2.C hydraulic.h librarian.h library.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h mathlib.h
weather_hourly${OBJ}: weather_hourly.C weather_old.h weather.h librarian.h \
  library.h symbol.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  im.h time.h log.h border.h mathlib.h
