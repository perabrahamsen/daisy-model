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

# HOSTTYPE is not defined in the native win32 Emacs.
#
ifeq ($(OS),Windows_NT)
#	HOSTTYPE = cygwin
	HOSTTYPE = mingw
#	HOSTTYPE = win32
endif

# Some non-local files and directories.

ifeq ($(HOSTTYPE),i386-linux)
SRCDIR = $(HOME)/daisy
OBJHOME = /usr/local/daisy
NATIVEHOME = $(OBJHOME)/$(HOSTTYPE)
BOOSTINC = -isystem $(HOME)/boost/include/boost-1_35/
FTPDIR = /home/ftp/pub/daisy
WWWINDEX = /home/user_3/daisy/.public_html/index.html
else
SRCDIR = ..
OBJHOME = obj
NATIVEHOME = $(OBJHOME)
BOOSTINC = -isystem /usr/include/boost-1_33_1/
SETUPDIR = $(HOME)/daisy/install
MAKENSIS = /cygdrive/c/Programmer/NSIS/makensis.exe
MINGWHOME = /cygdrive/c/MinGW
endif

TARGETTYPE = i586-mingw32msvc

# Set USE_GUI to Q4 or none, depending on what GUI you want.
#USE_GUI = none
USE_GUI = Q4

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
#	gcc		Use the GNU compiler.
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
SPARCSRC = set_exceptions.S
SPARCOBJ = set_exceptions.o

# Microsoft lacks some common Unix functions.
#
MSSRC = win32_unistd.C

WINSRC = w32reg.c
WINOBJ = w32reg.o
WINHDR = w32reg.h

ifeq ($(HOSTTYPE),sun4) 
SYSSOURCES = $(SPARCSRC)
SYSOBJECTS = $(SPARCOBJ)
endif

ifeq ($(COMPILER),ms)
SYSSOURCES = $(MSSRC)
endif

ifeq ($(HOSTTYPE),mingw)
SYSSOURCES = $(WINSRC)
SYSOBJECTS = $(WINOBJ)
SYSHEADERS = $(WINHDR)
endif

ALLSYSSRC = $(SPARCSRC) $(MSSRC) $(WINSRC)
ALLSYSHDR = $(WINHDR)

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
		  OPTIMIZE = -O3 -ffast-math -mtune=pentium-m -march=pentium
		endif
		ifeq ($(HOSTTYPE),mingw)
		  OPTIMIZE = -O3 -ffast-math -mtune=pentium-m -march=pentium
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
	WARNING = -Wextra -Wall -Wno-uninitialized \
		  -Wconversion -Woverloaded-virtual \
		  -Wsign-promo -Wundef -Wpointer-arith -Wwrite-strings \
                  -Wno-sign-compare  -Wundef -Wendif-labels \
		  -Wcast-qual -Wcast-align -Wmissing-format-attribute \
		  -Wold-style-cast
# -Wfloat-equal
#  : triggered by header files for 2.95/woody
#  -Wmissing-noreturn: triggered by some virtual functions.
#  -Wmissing-prototypes -Wstrict-prototypes: Not C++ flags.
#  -Wuninitialized: triggered in 3.4 in initializations!
#  -Wunreachable-code: triggered by header files
	COMPILE = $(GCC) -ansi -pedantic $(WARNING) $(DEBUG) $(OSFLAGS) $(BOOSTINC)
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

CSHARP = /cygdrive/C/WINDOWS/Microsoft.NET/Framework/v2.0.50727/csc.exe

csdaisy.exe:	csmain.cs csdaisy.netmodule
	$(CSHARP) /out:csdaisy.exe /addmodule:csdaisy.netmodule csmain.cs 

csdaisydll.exe:	csmain.cs csdaisy.dll
	$(CSHARP) /out:csdaisy.exe /r:csdaisy.dll csmain.cs 

csdaisy.dll: csdaisy.cs
	$(CSHARP) /target:library csdaisy.cs

csdaisy.netmodule: csdaisy.cs
	$(CSHARP) /target:module csdaisy.cs

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
ifeq ($(HOSTTYPE),i386-linux)
# Locate the Qt library.
#
QTINCLUDE	= -I/usr/include/qt
QTLIB		= -lqt -L/usr/X11R6/lib -lX11 -lm
QTMOC		= moc
else
QTINCLUDE	= -I/pack/qt/include -I/usr/openwin/include
QTLIB		= -L/pack/qt/lib -R/pack/qt/lib -lqt \
		  -L/usr/openwin/lib -R/usr/openwin/lib \
		  -lXext -lX11 -lsocket -lnsl -lm
QTMOC		= /pack/qt/bin/moc
endif

ifeq ($(HOSTTYPE),mingw)
Q4HOME = /cygdrive/c/Qt/4.2.3
Q4INCLUDE	= -isystem $(Q4HOME)/include
ifeq (false,true)
Q4SYS		= -lGDI32 -lole32 -lOleaut32 -luuid -lImm32 -lwinmm \
		  -lWinspool -lWs2_32 -lcomdlg32
Q4LIB		= -L$(Q4HOME)/lib -lQtGui -lQtCore $(Q4SYS) 
else
Q4LIB		= -L$(Q4HOME)/lib -lQtGui4 -lQtCore4
endif
Q4MOC		= $(Q4HOME)/bin/moc
endif

# Find the right file extension.
#
ifeq ($(HOSTTYPE),win32)
	OBJ = .obj
	EXE = .exe
else
	OBJ = .o
	ifeq ($(HOSTTYPE),cygwin)
		EXE = .exe
	else
		ifeq ($(HOSTTYPE),mingw)
			EXE = .exe
		else
			EXE =
		endif
	endif
endif

ifeq ($(HOSTTYPE),mingw)
	DAISYEXE = daisy.exe
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
LATER = msoltranrect_Mollerup.C
MODELS = reaction_std.C chemistry_std.C groundwater_extern.C scope_exchange.C \
	msoltranrect_none.C uzrect_Mollerup.C groundwater_flux.C \
	msoltranrect_2x1.C photo_FCC4.C ABAeffect_exp.C cropNdist_uniform.C \
	uzrect_2x1.C select_flow.C volume_box.C \
	select_volume.C uz1d_none.C condition_walltime.C uz1d_richard.C \
	cropNdist_DPF.C raddist_DPF.C raddist_std.C difrad_DPF.C \
        difrad_weather.C number_lisp.C condition_extern.C condition_boolean.C \
	boolean_number.C boolean_string.C \
	movement_rect.C number_soil.C organic_none.C \
	organic_std.C movement_1D.C integer_arit.C \
	source_merge.C number_source.C program_file.C action_table.C \
	xysource_merge.C xysource_inline.C xysource_loop.C \
	xysource_combine.C gnuplot_xy.C xysource_expr.C gnuplot_multi.C \
	gnuplot_time.C source_combine.C number_arit.C source_expr.C \
	source_std.C action_markvand.C photo_Farquhar.C photo_GL.C \
	program_gnuplot.C \
	program_document.C program_batch.C summary_balance.C \
	rootdens_AP.C number_const.C equil_goal.C pedo_arit.C \
	domsorp_std.C equil_linear.C pedo_const.C \
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
	groundwater_pipe.C \
	select_index.C select_content.C \
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
	svat_pmsw.C action_surface.C

DISABLED = log_clone.C action_merge.C action_divide.C \
	weather_file.C hydraulic_old.C hydraulic_old2.C weather_hourly.C 
# A component is a common interface to a number of models.
#
COMPONENTS = ui.C reaction.C scopesel.C scope.C \
	ABAeffect.C msoltranrect.C uzrect.C bound.C volume.C uz1d.C \
	cropNdist.C raddist.C difrad.C organic_matter.C movement.C integer.C \
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
SUBMODELS = toplevel.C timestep.C geometry_rect.C element.C \
        geometry1d.C fetch.C horheat.C litter.C time.C \
	som.C smb.C aom.C dom.C crpn.C vernalization.C \
	partition.C production.C \
	harvesting.C canopy_simple.C canopy_std.C root_system.C \
	ridge.C soil.C surface.C soil_water.C soil_NH4.C soil_NO3.C \
	denitrification.C soil_heat.C \
	snow.C im.C harvest.C field.C \
	bioincorporation.C 

# Special or intermediate models with their own interface.
#
SPECIALS = scope_multi.C scope_id.C geometry_vert.C gnuplot_base.C \
	source_file.C format_LaTeX.C log_all.C om.C select_value.C \
	weather_old.C log_extern.C log_select.C parser_file.C solute.C \
	geometry.C printer_file.C log_alist.C

# Various utility code that are neither a component nor a (sub)model.
#
OTHER = run.C treelog_text.C treelog_store.C \
	intrinsics.C metalib.C model.C output.C scope_block.C librarian.C \
	gnuplot_utils.C scope_sources.C scope_table.C lexer_table.C \
	block.C dlf.C texture.C destination.C symbol.C \
	fao.C gaussj.C vcheck.C assertion.C xref.C units.C \
	check.C check_range.C path.C traverse_delete.C \
	depend.C traverse.C treelog.C \
	lexer_data.C lexer.C daisy.C alist.C syntax.C library.C plf.C \
	mathlib.C cdaisy.C nrutil.C submodel.C version.C

# Utilities in header alone.
HEADONLY = submodeler.h border.h memutils.h 

# Everything that has an interface.
#
INTERFACES = $(COMPONENTS) $(SUBMODELS) $(SPECIALS) $(OTHER)

# Select the Qt frontend files.

QTMOCHDR = qmain_edit.h qmain.h
QTMOCSRC = $(QTMOCHDR:.h=_moc.C)
QTSOURCES = $(QTMOCHDR:.h=.C) \
	qmain_tree.C qmain_item.C qmain_populate.C qmain_busy.C
QTHEADERS = $(QTSOURCES:.C=.h) 
QTOBJECTS = $(QTSOURCES:.C=${OBJ}) $(QTMOCHDR:.h=_moc${OBJ}) 

# Select the Qt4 frontend files

Q4MOCHDR = run_Qt.h vis_Qt.h log_Qt.h
Q4MOCSRC = $(Q4MOCHDR:.h=_moc.C)
Q4HEADERS = $(Q4MOCHDR) ui_Qt.h
Q4SOURCES = $(Q4HEADERS:.h=.C) ui_Qt_run.C
Q4OBJECTS = $(Q4SOURCES:.C=${OBJ}) $(Q4MOCHDR:.h=_moc${OBJ}) 

ifeq ($(USE_GUI),Q4)
GUISOURCES = $(Q4SOURCES) 
GUIOBJECTS = $(Q4OBJECTS)
GUILIB = $(Q4LIB)
GUIINCLUDE = $(Q4INCLUDE)
MOC = $(Q4MOC)
else
GUISOURCES =
GUIOBJECTS = 
GUILIB = 
GUIINCLUDE = 
endif

ALLGUISRC = tkmain.C gmain.C $(QTSOURCES) $(Q4SOURCES)
ALLGUIHDR = $(QTHEADERS) $(Q4HEADERS)

LOSTGUISRC = pmain.C

# Select the C files that are not part of the library.
#
MAIN = main.C 

# The object files used in the daisy library.
#
LIBOBJ = $(INTERFACES:.C=${OBJ}) $(MODELS:.C=${OBJ}) $(SYSOBJECTS)

# Find all object files, header files, and source files.
#
OBJECTS = $(LIBOBJ) $(MAIN:.C=${OBJ}) cmain${OBJ} bugmain.o
SOURCES = $(INTERFACES) $(MODELS)  $(MAIN) \
	 cmain.c bugmain.c 
HEADERS = $(INTERFACES:.C=.h) $(HEADONLY)

# Find all printable files.
#
TEXT =  ChangeLog.2 ChangeLog.1 \
	Makefile ChangeLog TODO NEWS COPYING COPYING.LIB  $(DISABLED) \
	$(HEADERS) $(SOURCES) $(ALLSYSHDR) $(ALLSYSSRC) \
	$(ALLGUIHDR) $(ALLGUISRC)

# The executables.
#
EXECUTABLES = daisy${EXE} tkdaisy${EXE} cdaisy${EXE} gdaisy${EXE}

# Select files to be removed by the next cvs update.
#
REMOVE = tlink32.ini daisy.bpr daisy.bpf daisy.bpg Daisy.vcproj q4main.C \
	treelog_stream.C treelog_stream.h treelog_dual.C treelog_dual.h


REMOVED = soil_chemical.C soil_chemicals.C chemicals.C soil_chemical.h soil_chemicals.h chemicals.h boolean_extern.C number_extern.C options.C options.h select_interval.C select_utils.h select_utils.C select_flux_top.C select_flux_bottom.C select_flux.C select_flux.h column_base.h

# These are the file extensions we deal with.
# 
.SUFFIXES:	.C ${OBJ} .h .c ${EXE} .a

# Create all the executables.
#
all:	#(EXECUTABLES)
	@echo 'Use "make native" to create a native Daisy executable.'

# Create a DLL.
#
daisy.dll: $(LIBOBJ) $(GUIOBJECTS) 
	$(CC) -shared -o $@ $^ $(GUILIB) $(CPPLIB) $(MATHLIB) -Wl,--out-implib,libdaisy.a 

# Create the main executable.
#
daisy.exe:	main${OBJ} daisy.dll
	$(LINK)$@ $^ $(CPPLIB) $(MATHLIB) -mwindows

daisy:	main${OBJ} $(GUIOBJECTS) $(LIBOBJ)
	$(LINK)$@ $^ $(GUILIB) $(CPPLIB) $(MATHLIB)

exp:	
	(cd $(OBJHOME)/exp \
         && $(MAKE) VPATH=$(SRCDIR) USE_PROFILE=true -f $(SRCDIR)/Makefile daisy)

native:	
	(mkdir -p $(NATIVEHOME) \
	 && cd $(NATIVEHOME) \
         && $(MAKE) VPATH=$(SRCDIR) -f $(SRCDIR)/Makefile daisy${EXE})

cnative:
	(mkdir -p $(NATIVEHOME) \
	 && cd $(NATIVEHOME) \
         && $(MAKE) VPATH=$(SRCDIR) -f $(SRCDIR)/Makefile cdaisy${EXE})

cross:
	(cd $(OBJHOME)/$(TARGETTYPE) \
         && $(MAKE) GCC=$(CROSSGCC) DEBUG= VPATH=$(SRCDIR) \
                    -f $(SRCDIR)/Makefile daisy${EXE})

# Create manager test executable.
#
mandaisy${EXE}:	manmain${OBJ} daisy.so
	$(LINK)$@  $^ $(MATHLIB)

# Create bug test executable.
#
bugdaisy${EXE}:	bugmain${OBJ} daisy.so
	$(LINK)$@  $^ $(MATHLIB)

# Create executable with embedded tcl/tk.
#
tkdaisy${EXE}:	tkmain${OBJ} daisy.so
	$(LINK)$@ $^ $(TKLIB) $(MATHLIB)

# Create executable with Gtk--.
#
gdaisy${EXE}:	gmain${OBJ} daisy.so
	$(LINK)$@ $^ $(GTKMMLIB)

# Create executable with Qt 3.
#
qdaisy${EXE}:	$(QTOBJECTS) daisy.so
	$(LINK)$@ $(QTOBJECTS) `pwd`/daisy.so $(QTLIB)

# Create the C main executable.
#
cdaisy${EXE}:  cmain${OBJ} $(DAISYDYN)
	gcc -o $@ $^ 

ddaisy${EXE}:  main${OBJ} $(DAISYDYN) $(GUIOBJECTS) 
	$(LINK)$@ $^ $(GUILIB) $(CPPLIB) $(MATHLIB)

cdaisy-mshe${EXE}:  cmain-mshe${OBJ} daisy.so
	$(LINK)$@ cmain-mshe${OBJ} `pwd`/daisy.so $(MATHLIB)

# Create the C main executable for testing.
#
cdaisy_test${EXE}:  cmain_test${OBJ} daisy.so
	$(LINK)$@ $^ $(MATHLIB)

# Create a shared library.
#
daisy.so: $(LIBOBJ)
	$(CC) -shared -o daisy.so $^ $(MATHLIB)

# toplevel.o cdaisy.o:
#	 $(CC) $(NOLINK) -DBUILD_DLL $<

# Create daisy plot executable.
#
pdaisy${EXE}: pmain${OBJ} time.o
	$(LINK)$@ $^ $(GTKMMDRAWLIB) $(MATHLIB)


# Boost test

btest${EXE}: btest.C
	$(LINK)$@ -isystem /usr/include/boost-1_33_1/ $< $(CPPLIB) $(MATHLIB)

# Create the MMM executable.

mmm${EXE}:	$(MOBJECTS)
	$(LINK)$@  $^ $(MATHLIB)

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

txt/components.tex:
	(cd txt && $(DAISYEXE) -nw all.dai -p document

# Remove all the temporary files.
#
clean:
	rm $(OBJECTS) *.rpo $(EXECUTABLES) *.obj *.exe *.o *~

# Update the Makefile when dependencies have changed.
#
depend: $(GUISOURCES) $(SOURCES) 
	rm -f Makefile.old
	mv Makefile Makefile.old
	sed -e '/^# AUTOMATIC/q' < Makefile.old > Makefile
	$(CC) -I. $(GUIINCLUDE) -MM $^ | sed -e 's/\.o:/$${OBJ}:/' >> Makefile

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

setup:	cvs
	$(MAKE) setupnocvs

setupnocvs: 
	$(MAKE) native 
	rm -rf $(SETUPDIR)
	mkdir $(SETUPDIR)
	cp ChangeLog NEWS $(SETUPDIR)
	mkdir $(SETUPDIR)/src
	cp $(TEXT) $(SETUPDIR)/src
	(cd lib && $(MAKE) SETUPDIR=$(SETUPDIR) TAG=$(TAG) setup)
	(cd sample && $(MAKE) SETUPDIR=$(SETUPDIR) TAG=$(TAG) setup)
	(cd txt && $(MAKE) PATH="$(PATH):$(Q4HOME)/bin" DAISYEXE=$(SRCDIR)/$(OBJHOME)/$(DAISYEXE) SETUPDIR=$(SETUPDIR) DAISYPATH=".;$(SRCDIR)/lib;$(SRCDIR)/sample" setup)
	(cd exercises && $(MAKE) SETUPDIR=$(SETUPDIR) setup)
	mkdir $(SETUPDIR)/bin
	$(STRIP) -o $(SETUPDIR)/bin/daisy.exe $(OBJHOME)/daisy.exe
	$(STRIP) -o $(SETUPDIR)/bin/daisy.dll $(OBJHOME)/daisy.dll
	cp $(Q4HOME)/bin/QtCore4.dll $(SETUPDIR)/bin
	cp $(Q4HOME)/bin/QtGui4.dll $(SETUPDIR)/bin
	cp $(MINGWHOME)/bin/mingwm10.dll $(SETUPDIR)/bin
	$(MAKENSIS) /V2 /DVERSION=$(TAG) setup.nsi

# How to compile the assembler file.
#
set_exceptions${OBJ}: set_exceptions.S
	as -o set_exceptions${OBJ} set_exceptions.S

# How to compile a C++ file.
#
.C${OBJ}:
	$(CC) $(GUIINCLUDE) $(NOLINK) $<

# How to compile a C file.
#
.c${OBJ}:
	$(CCOMPILE) $(OPTIMIZE) $(PROFILE) $(NOLINK) $<

# How to mock a Qt file.
#
%_moc.C: %.h
	$(MOC) $< -o $@

# Special rule for tkmain.o
#
tkmain${OBJ}: tkmain.C
	$(CC) $(TKINCLUDE) $(NOLINK) $<

# Special rule for gmain.o
#
gmain${OBJ}: gmain.C
	$(CC) $(GTKMMINCLUDE) $(NOLINK) $<

# Special rule for Qt frontend files.

$(QTOBJECTS):
	$(CC) $(QTINCLUDE) $(NOLINK) $<

# Special rule for pmain.o
#
pmain${OBJ}: pmain.C
	$(CC) $(GTKMMDRAWINCLUDE) $(NOLINK) $<

############################################################
# AUTOMATIC -- DO NOT CHANGE THIS LINE OR ANYTHING BELOW IT!
run_Qt${OBJ}: run_Qt.C run_Qt.h run.h model.h toplevel.h treelog_text.h \
  treelog.h symbol.h vis_Qt.h time.h program.h
vis_Qt${OBJ}: vis_Qt.C vis_Qt.h toplevel.h time.h log_Qt.h log_extern.h \
  log_select.h log.h border.h model.h alist.h symbol.h memutils.h \
  destination.h scope.h mathlib.h assertion.h
log_Qt${OBJ}: log_Qt.C log_Qt.h log_extern.h log_select.h log.h border.h \
  model.h alist.h symbol.h memutils.h destination.h scope.h librarian.h \
  syntax.h treelog.h
ui_Qt${OBJ}: ui_Qt.C ui_Qt.h ui.h model.h symbol.h toplevel.h librarian.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h
ui_Qt_run${OBJ}: ui_Qt_run.C ui_Qt.h ui.h model.h symbol.h run_Qt.h run.h \
  toplevel.h treelog_text.h treelog.h vis_Qt.h time.h log_Qt.h \
  log_extern.h log_select.h log.h border.h alist.h memutils.h \
  destination.h scope.h program.h metalib.h library.h librarian.h block.h \
  syntax.h plf.h assertion.h
ui${OBJ}: ui.C ui.h model.h symbol.h toplevel.h treelog_text.h treelog.h \
  librarian.h block.h syntax.h plf.h alist.h
reaction${OBJ}: reaction.C reaction.h model.h alist.h symbol.h block.h \
  syntax.h treelog.h plf.h librarian.h
scopesel${OBJ}: scopesel.C scopesel.h model.h scope.h symbol.h assertion.h \
  output.h condition.h memutils.h block.h syntax.h treelog.h plf.h \
  alist.h librarian.h
scope${OBJ}: scope.C scope.h symbol.h model.h block.h syntax.h treelog.h \
  plf.h assertion.h librarian.h
ABAeffect${OBJ}: ABAeffect.C ABAeffect.h model.h alist.h symbol.h mathlib.h \
  assertion.h block.h syntax.h treelog.h plf.h librarian.h
msoltranrect${OBJ}: msoltranrect.C msoltranrect.h model.h symbol.h block.h \
  syntax.h treelog.h plf.h librarian.h
uzrect${OBJ}: uzrect.C uzrect.h model.h symbol.h block.h syntax.h treelog.h \
  plf.h librarian.h
bound${OBJ}: bound.C bound.h model.h symbol.h block.h syntax.h treelog.h \
  plf.h alist.h mathlib.h assertion.h librarian.h
volume${OBJ}: volume.C volume.h model.h symbol.h block.h syntax.h treelog.h \
  plf.h assertion.h librarian.h
uz1d${OBJ}: uz1d.C uz1d.h model.h geometry_rect.h geometry_vert.h geometry.h \
  syntax.h treelog.h symbol.h mathlib.h assertion.h soil.h soil_water.h \
  soil_heat.h block.h plf.h librarian.h
cropNdist${OBJ}: cropNdist.C cropNdist.h model.h alist.h symbol.h block.h \
  syntax.h treelog.h plf.h mathlib.h assertion.h librarian.h
raddist${OBJ}: raddist.C raddist.h model.h alist.h symbol.h block.h syntax.h \
  treelog.h plf.h mathlib.h assertion.h librarian.h
difrad${OBJ}: difrad.C difrad.h model.h alist.h symbol.h block.h syntax.h \
  treelog.h plf.h librarian.h
organic_matter${OBJ}: organic_matter.C organic_matter.h model.h symbol.h \
  block.h syntax.h treelog.h plf.h librarian.h
movement${OBJ}: movement.C movement.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h librarian.h
integer${OBJ}: integer.C integer.h model.h symbol.h boolean.h submodeler.h \
  block.h syntax.h treelog.h plf.h assertion.h alist.h memutils.h \
  librarian.h
xysource${OBJ}: xysource.C xysource.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h assertion.h librarian.h
gnuplot${OBJ}: gnuplot.C gnuplot.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h librarian.h
boolean${OBJ}: boolean.C boolean.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h alist.h assertion.h memutils.h librarian.h
stringer${OBJ}: stringer.C stringer.h model.h symbol.h boolean.h number.h \
  submodeler.h block.h syntax.h treelog.h plf.h assertion.h alist.h \
  memutils.h librarian.h
source${OBJ}: source.C source.h model.h time.h symbol.h block.h syntax.h \
  treelog.h plf.h librarian.h
photo${OBJ}: photo.C photo.h model.h symbol.h block.h syntax.h treelog.h \
  plf.h librarian.h
format${OBJ}: format.C format.h model.h symbol.h assertion.h block.h syntax.h \
  treelog.h plf.h librarian.h
depth${OBJ}: depth.C depth.h model.h symbol.h block.h syntax.h treelog.h \
  plf.h alist.h time.h lexer_data.h lexer.h check.h vcheck.h assertion.h \
  librarian.h
wse${OBJ}: wse.C wse.h model.h symbol.h block.h syntax.h treelog.h plf.h \
  alist.h program.h run.h mathlib.h assertion.h librarian.h
program${OBJ}: program.C program.h model.h symbol.h run.h block.h syntax.h \
  treelog.h plf.h librarian.h
number${OBJ}: number.C number.h symbol.h model.h block.h syntax.h treelog.h \
  plf.h librarian.h
domsorp${OBJ}: domsorp.C domsorp.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h librarian.h
chemistry${OBJ}: chemistry.C chemistry.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h librarian.h
summary${OBJ}: summary.C summary.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h librarian.h
nitrification${OBJ}: nitrification.C nitrification.h model.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h soil.h soil_water.h soil_heat.h \
  soil_NH4.h solute.h adsorption.h soil_NO3.h log.h border.h mathlib.h \
  assertion.h librarian.h
phenology${OBJ}: phenology.C phenology.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h log.h border.h alist.h librarian.h
clayom${OBJ}: clayom.C clayom.h model.h symbol.h block.h syntax.h treelog.h \
  plf.h librarian.h
equil${OBJ}: equil.C equil.h model.h symbol.h block.h syntax.h treelog.h \
  plf.h librarian.h
pedo${OBJ}: pedo.C pedo.h model.h symbol.h soil.h units.h block.h syntax.h \
  treelog.h plf.h librarian.h
transform${OBJ}: transform.C transform.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h librarian.h
rootdens${OBJ}: rootdens.C rootdens.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h alist.h check.h librarian.h
select${OBJ}: select.C select.h destination.h symbol.h model.h units.h \
  volume.h condition.h block.h syntax.h treelog.h plf.h geometry.h \
  mathlib.h assertion.h number.h scope_id.h scope.h metalib.h library.h \
  alist.h check.h vcheck.h format.h submodel.h submodeler.h librarian.h
average${OBJ}: average.C average.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h alist.h mathlib.h assertion.h librarian.h
mactrans${OBJ}: mactrans.C mactrans.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h librarian.h
macro${OBJ}: macro.C macro.h model.h syntax.h treelog.h symbol.h block.h \
  plf.h librarian.h
parser${OBJ}: parser.C parser.h model.h symbol.h block.h syntax.h treelog.h \
  plf.h librarian.h
log${OBJ}: log.C log.h border.h model.h alist.h symbol.h library.h metalib.h \
  block.h syntax.h treelog.h plf.h daisy.h program.h run.h time.h \
  memutils.h assertion.h librarian.h
weather${OBJ}: weather.C weather.h model.h im.h symbol.h block.h syntax.h \
  treelog.h plf.h fao.h time.h log.h border.h alist.h mathlib.h \
  assertion.h librarian.h
column${OBJ}: column.C column.h model.h alist.h symbol.h block.h syntax.h \
  treelog.h plf.h log.h border.h librarian.h
crop${OBJ}: crop.C crop.h time.h alist.h symbol.h model.h om.h plf.h block.h \
  syntax.h treelog.h mathlib.h assertion.h librarian.h
action${OBJ}: action.C action.h model.h alist.h symbol.h block.h syntax.h \
  treelog.h plf.h librarian.h
condition${OBJ}: condition.C condition.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h librarian.h
horizon${OBJ}: horizon.C horizon.h model.h symbol.h library.h block.h \
  syntax.h treelog.h plf.h alist.h horheat.h hydraulic.h mathlib.h \
  assertion.h tortuosity.h texture.h nitrification.h log.h border.h \
  check_range.h check.h vcheck.h librarian.h
uzmodel${OBJ}: uzmodel.C uzmodel.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h librarian.h
hydraulic${OBJ}: hydraulic.C hydraulic.h model.h syntax.h treelog.h symbol.h \
  library.h block.h plf.h log.h border.h alist.h check_range.h check.h \
  mathlib.h assertion.h program.h run.h vcheck.h librarian.h
bioclimate${OBJ}: bioclimate.C bioclimate.h model.h alist.h symbol.h \
  weather.h im.h block.h syntax.h treelog.h plf.h mathlib.h assertion.h \
  librarian.h
groundwater${OBJ}: groundwater.C groundwater.h model.h symbol.h geometry.h \
  syntax.h treelog.h mathlib.h assertion.h log.h border.h alist.h block.h \
  plf.h librarian.h
am${OBJ}: am.C am.h model.h alist.h symbol.h aom.h om.h plf.h im.h metalib.h \
  library.h submodeler.h block.h syntax.h treelog.h assertion.h time.h \
  log.h border.h geometry.h mathlib.h check.h vcheck.h program.h run.h \
  memutils.h librarian.h
transport${OBJ}: transport.C transport.h model.h syntax.h treelog.h symbol.h \
  block.h plf.h librarian.h
adsorption${OBJ}: adsorption.C adsorption.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h librarian.h
tortuosity${OBJ}: tortuosity.C tortuosity.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h librarian.h
printer${OBJ}: printer.C printer.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h librarian.h
chemical${OBJ}: chemical.C chemical.h model.h solute.h adsorption.h symbol.h \
  alist.h block.h syntax.h treelog.h plf.h librarian.h
pet${OBJ}: pet.C pet.h model.h alist.h symbol.h syntax.h treelog.h block.h \
  plf.h log.h border.h vegetation.h surface.h uzmodel.h librarian.h
net_radiation${OBJ}: net_radiation.C net_radiation.h model.h symbol.h log.h \
  border.h alist.h weather.h im.h block.h syntax.h treelog.h plf.h \
  mathlib.h assertion.h librarian.h
svat${OBJ}: svat.C svat.h model.h symbol.h log.h border.h alist.h block.h \
  syntax.h treelog.h plf.h librarian.h
vegetation${OBJ}: vegetation.C vegetation.h model.h symbol.h log.h border.h \
  alist.h syntax.h treelog.h block.h plf.h librarian.h
toplevel${OBJ}: toplevel.C toplevel.h metalib.h symbol.h daisy.h program.h \
  model.h run.h time.h memutils.h ui.h library.h parser_file.h parser.h \
  submodel.h block.h syntax.h treelog.h plf.h alist.h path.h version.h \
  assertion.h treelog_text.h treelog_store.h librarian.h
timestep${OBJ}: timestep.C timestep.h time.h vcheck.h syntax.h treelog.h \
  symbol.h alist.h block.h plf.h assertion.h mathlib.h
geometry_rect${OBJ}: geometry_rect.C geometry_rect.h geometry_vert.h \
  geometry.h syntax.h treelog.h symbol.h mathlib.h assertion.h volume.h \
  model.h check.h vcheck.h block.h plf.h alist.h submodel.h
element${OBJ}: element.C element.h log.h border.h model.h alist.h symbol.h \
  geometry.h syntax.h treelog.h mathlib.h assertion.h adsorption.h \
  submodel.h soil.h soil_water.h
geometry1d${OBJ}: geometry1d.C geometry1d.h geometry_vert.h geometry.h \
  syntax.h treelog.h symbol.h mathlib.h assertion.h volume.h model.h \
  block.h plf.h alist.h check.h vcheck.h submodel.h
fetch${OBJ}: fetch.C fetch.h destination.h symbol.h select.h model.h units.h \
  volume.h treelog.h alist.h syntax.h mathlib.h assertion.h
horheat${OBJ}: horheat.C horheat.h texture.h plf.h hydraulic.h model.h \
  syntax.h treelog.h symbol.h alist.h check.h mathlib.h assertion.h \
  submodel.h
litter${OBJ}: litter.C litter.h submodel.h syntax.h treelog.h symbol.h \
  alist.h check.h
time${OBJ}: time.C time.h assertion.h log.h border.h model.h alist.h symbol.h \
  syntax.h treelog.h vcheck.h submodel.h
som${OBJ}: som.C som.h om.h plf.h submodel.h alist.h symbol.h
smb${OBJ}: smb.C smb.h om.h plf.h dom.h adsorption.h model.h symbol.h \
  submodel.h syntax.h treelog.h alist.h assertion.h check.h mathlib.h
aom${OBJ}: aom.C aom.h om.h plf.h submodel.h alist.h symbol.h syntax.h \
  treelog.h check.h assertion.h smb.h dom.h adsorption.h model.h log.h \
  border.h geometry.h mathlib.h
dom${OBJ}: dom.C dom.h adsorption.h model.h symbol.h plf.h element.h smb.h \
  om.h geometry.h syntax.h treelog.h mathlib.h assertion.h submodel.h \
  block.h alist.h soil.h soil_water.h log.h border.h check.h librarian.h
crpn${OBJ}: crpn.C crpn.h production.h symbol.h root_system.h rootdens.h \
  model.h plf.h syntax.h treelog.h alist.h log.h border.h mathlib.h \
  assertion.h submodel.h check.h
vernalization${OBJ}: vernalization.C vernalization.h submodel.h log.h \
  border.h model.h alist.h symbol.h syntax.h treelog.h
partition${OBJ}: partition.C partition.h plf.h submodel.h syntax.h treelog.h \
  symbol.h alist.h check.h mathlib.h assertion.h
production${OBJ}: production.C production.h symbol.h crpn.h partition.h plf.h \
  organic_matter.h model.h geometry.h syntax.h treelog.h mathlib.h \
  assertion.h am.h alist.h log.h border.h time.h submodel.h
harvesting${OBJ}: harvesting.C harvesting.h time.h plf.h symbol.h \
  production.h am.h model.h alist.h aom.h om.h crop.h harvest.h block.h \
  syntax.h treelog.h geometry.h mathlib.h assertion.h log.h border.h \
  timestep.h vcheck.h submodel.h check_range.h check.h
canopy_simple${OBJ}: canopy_simple.C canopy_simple.h plf.h submodel.h log.h \
  border.h model.h alist.h symbol.h syntax.h treelog.h
canopy_std${OBJ}: canopy_std.C canopy_std.h canopy_simple.h plf.h submodel.h \
  log.h border.h model.h alist.h symbol.h syntax.h treelog.h mathlib.h \
  assertion.h
root_system${OBJ}: root_system.C root_system.h rootdens.h model.h symbol.h \
  plf.h submodel.h geometry.h syntax.h treelog.h mathlib.h assertion.h \
  soil_heat.h soil_NH4.h solute.h adsorption.h soil_NO3.h soil_water.h \
  soil.h log.h border.h alist.h check.h block.h librarian.h
ridge${OBJ}: ridge.C ridge.h soil.h geometry1d.h geometry_vert.h geometry.h \
  syntax.h treelog.h symbol.h mathlib.h assertion.h plf.h submodel.h \
  log.h border.h model.h alist.h soil_water.h check.h
soil${OBJ}: soil.C soil.h horizon.h model.h symbol.h geometry.h syntax.h \
  treelog.h mathlib.h assertion.h hydraulic.h tortuosity.h groundwater.h \
  metalib.h library.h alist.h submodel.h submodeler.h block.h plf.h log.h \
  border.h check.h vcheck.h memutils.h librarian.h
surface${OBJ}: surface.C surface.h uzmodel.h model.h symbol.h syntax.h \
  treelog.h alist.h geometry1d.h geometry_vert.h geometry.h mathlib.h \
  assertion.h soil.h soil_water.h log.h border.h im.h submodel.h plf.h \
  ridge.h check.h
soil_water${OBJ}: soil_water.C soil_water.h geometry.h syntax.h treelog.h \
  symbol.h mathlib.h assertion.h soil.h soil_heat.h groundwater.h model.h \
  log.h border.h alist.h submodel.h block.h plf.h
soil_NH4${OBJ}: soil_NH4.C soil_NH4.h solute.h adsorption.h model.h symbol.h \
  soil.h soil_water.h submodel.h alist.h assertion.h
soil_NO3${OBJ}: soil_NO3.C soil_NO3.h solute.h adsorption.h model.h symbol.h \
  soil.h submodel.h alist.h assertion.h
denitrification${OBJ}: denitrification.C denitrification.h plf.h alist.h \
  symbol.h syntax.h treelog.h geometry.h mathlib.h assertion.h soil.h \
  soil_water.h soil_heat.h organic_matter.h model.h soil_NO3.h solute.h \
  adsorption.h log.h border.h submodel.h check.h
soil_heat${OBJ}: soil_heat.C soil_heat.h block.h syntax.h treelog.h symbol.h \
  plf.h alist.h geometry.h mathlib.h assertion.h soil.h soil_water.h \
  weather.h model.h im.h log.h border.h submodel.h
snow${OBJ}: snow.C snow.h alist.h symbol.h syntax.h treelog.h log.h border.h \
  model.h geometry.h mathlib.h assertion.h soil.h soil_water.h \
  soil_heat.h movement.h submodel.h
im${OBJ}: im.C im.h am.h model.h alist.h symbol.h log.h border.h syntax.h \
  treelog.h submodel.h
harvest${OBJ}: harvest.C harvest.h time.h symbol.h block.h syntax.h treelog.h \
  plf.h log.h border.h model.h alist.h submodel.h
field${OBJ}: field.C field.h border.h symbol.h column.h model.h alist.h log.h \
  log_clone.h log_alist.h treelog.h library.h block.h syntax.h plf.h \
  memutils.h assertion.h librarian.h
bioincorporation${OBJ}: bioincorporation.C bioincorporation.h alist.h \
  symbol.h syntax.h treelog.h log.h border.h model.h geometry.h mathlib.h \
  assertion.h soil.h am.h submodel.h plf.h time.h aom.h om.h check.h \
  vcheck.h
scope_multi${OBJ}: scope_multi.C scope_multi.h scope.h symbol.h model.h \
  syntax.h treelog.h alist.h assertion.h librarian.h
scope_id${OBJ}: scope_id.C scope_id.h scope.h symbol.h model.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h librarian.h
geometry_vert${OBJ}: geometry_vert.C geometry_vert.h geometry.h syntax.h \
  treelog.h symbol.h mathlib.h assertion.h block.h plf.h
gnuplot_base${OBJ}: gnuplot_base.C gnuplot_base.h gnuplot.h model.h symbol.h \
  vcheck.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  librarian.h
source_file${OBJ}: source_file.C source_file.h source.h model.h time.h \
  symbol.h lexer_table.h block.h syntax.h treelog.h plf.h alist.h \
  gnuplot_utils.h vcheck.h mathlib.h assertion.h
format_LaTeX${OBJ}: format_LaTeX.C format_LaTeX.h format.h model.h symbol.h \
  syntax.h treelog.h alist.h version.h assertion.h librarian.h
log_all${OBJ}: log_all.C log_all.h log_select.h log.h border.h model.h \
  alist.h symbol.h memutils.h select.h destination.h units.h volume.h \
  block.h syntax.h treelog.h plf.h assertion.h
om${OBJ}: om.C om.h plf.h som.h smb.h dom.h adsorption.h model.h symbol.h \
  syntax.h treelog.h alist.h check.h vcheck.h geometry.h mathlib.h \
  assertion.h log.h border.h
select_value${OBJ}: select_value.C select_value.h select.h destination.h \
  symbol.h model.h units.h volume.h block.h syntax.h treelog.h plf.h \
  alist.h mathlib.h assertion.h
weather_old${OBJ}: weather_old.C weather_old.h weather.h model.h im.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h fao.h time.h
log_extern${OBJ}: log_extern.C log_extern.h log_select.h log.h border.h \
  model.h alist.h symbol.h memutils.h destination.h scope.h select.h \
  units.h volume.h scope_block.h block.h syntax.h treelog.h plf.h \
  assertion.h librarian.h submodeler.h
log_select${OBJ}: log_select.C log_select.h log.h border.h model.h alist.h \
  symbol.h memutils.h select.h destination.h units.h volume.h condition.h \
  metalib.h library.h block.h syntax.h treelog.h plf.h field.h format.h \
  assertion.h librarian.h
parser_file${OBJ}: parser_file.C parser_file.h parser.h model.h symbol.h \
  metalib.h library.h block.h syntax.h treelog.h plf.h alist.h lexer.h \
  submodel.h scope.h number.h integer.h time.h treelog_text.h path.h \
  units.h mathlib.h assertion.h memutils.h librarian.h
solute${OBJ}: solute.C solute.h adsorption.h model.h symbol.h log.h border.h \
  alist.h block.h syntax.h treelog.h plf.h geometry.h mathlib.h \
  assertion.h soil.h soil_water.h librarian.h
geometry${OBJ}: geometry.C geometry.h syntax.h treelog.h symbol.h mathlib.h \
  assertion.h volume.h model.h alist.h check.h vcheck.h
printer_file${OBJ}: printer_file.C printer_file.h printer.h model.h symbol.h \
  metalib.h library.h block.h syntax.h treelog.h plf.h alist.h time.h \
  parser.h path.h assertion.h librarian.h
log_alist${OBJ}: log_alist.C log_alist.h log.h border.h model.h alist.h \
  symbol.h library.h syntax.h treelog.h assertion.h
run${OBJ}: run.C run.h model.h
treelog_text${OBJ}: treelog_text.C treelog_text.h treelog.h symbol.h \
  assertion.h
treelog_store${OBJ}: treelog_store.C treelog_store.h treelog.h symbol.h \
  assertion.h memutils.h
intrinsics${OBJ}: intrinsics.C intrinsics.h symbol.h assertion.h library.h \
  memutils.h
metalib${OBJ}: metalib.C metalib.h symbol.h intrinsics.h librarian.h model.h \
  library.h block.h syntax.h treelog.h plf.h alist.h assertion.h \
  memutils.h
model${OBJ}: model.C model.h
output${OBJ}: output.C output.h condition.h model.h symbol.h memutils.h \
  daisy.h program.h run.h time.h log_all.h log_select.h log.h border.h \
  alist.h log_extern.h destination.h scope.h treelog.h timestep.h \
  vcheck.h syntax.h assertion.h librarian.h
scope_block${OBJ}: scope_block.C scope_block.h scope.h symbol.h model.h \
  block.h syntax.h treelog.h plf.h library.h number.h stringer.h alist.h \
  assertion.h librarian.h
librarian${OBJ}: librarian.C librarian.h model.h symbol.h library.h metalib.h \
  intrinsics.h block.h syntax.h treelog.h plf.h alist.h treelog_text.h \
  assertion.h
gnuplot_utils${OBJ}: gnuplot_utils.C gnuplot_utils.h syntax.h treelog.h \
  symbol.h alist.h
scope_sources${OBJ}: scope_sources.C scope_sources.h scope.h symbol.h model.h \
  time.h source.h treelog.h assertion.h memutils.h
scope_table${OBJ}: scope_table.C scope_table.h scope.h symbol.h model.h \
  lexer_table.h block.h syntax.h treelog.h plf.h assertion.h
lexer_table${OBJ}: lexer_table.C lexer_table.h block.h syntax.h treelog.h \
  symbol.h plf.h lexer_data.h lexer.h alist.h assertion.h mathlib.h \
  submodeler.h memutils.h time.h vcheck.h
block${OBJ}: block.C block.h syntax.h treelog.h symbol.h plf.h metalib.h \
  library.h alist.h librarian.h model.h stringer.h number.h assertion.h \
  scope_block.h scope.h
dlf${OBJ}: dlf.C dlf.h symbol.h volume.h model.h alist.h assertion.h \
  version.h daisy.h program.h run.h time.h memutils.h
texture${OBJ}: texture.C texture.h plf.h assertion.h mathlib.h
destination${OBJ}: destination.C destination.h symbol.h
symbol${OBJ}: symbol.C symbol.h assertion.h
fao${OBJ}: fao.C fao.h alist.h symbol.h syntax.h treelog.h net_radiation.h \
  model.h assertion.h mathlib.h librarian.h
gaussj${OBJ}: gaussj.C gaussj.h mathlib.h assertion.h
vcheck${OBJ}: vcheck.C vcheck.h units.h symbol.h syntax.h treelog.h alist.h \
  time.h plf.h assertion.h mathlib.h
assertion${OBJ}: assertion.C assertion.h treelog.h symbol.h mathlib.h
xref${OBJ}: xref.C xref.h symbol.h traverse.h metalib.h library.h syntax.h \
  treelog.h alist.h submodel.h assertion.h
units${OBJ}: units.C units.h symbol.h syntax.h treelog.h mathlib.h \
  assertion.h memutils.h
check${OBJ}: check.C check.h mathlib.h assertion.h
check_range${OBJ}: check_range.C check_range.h check.h
path${OBJ}: path.C path.h assertion.h
traverse_delete${OBJ}: traverse_delete.C traverse_delete.h symbol.h \
  traverse.h metalib.h library.h syntax.h treelog.h alist.h assertion.h
depend${OBJ}: depend.C depend.h symbol.h traverse.h library.h metalib.h \
  syntax.h treelog.h alist.h assertion.h
traverse${OBJ}: traverse.C traverse.h symbol.h metalib.h library.h syntax.h \
  treelog.h alist.h submodel.h assertion.h
treelog${OBJ}: treelog.C treelog.h symbol.h
lexer_data${OBJ}: lexer_data.C lexer_data.h lexer.h time.h mathlib.h \
  assertion.h
lexer${OBJ}: lexer.C lexer.h treelog.h symbol.h path.h
daisy${OBJ}: daisy.C daisy.h program.h model.h symbol.h run.h time.h \
  memutils.h weather.h im.h groundwater.h horizon.h output.h condition.h \
  log.h border.h alist.h parser.h nitrification.h bioclimate.h \
  hydraulic.h syntax.h treelog.h field.h harvest.h block.h plf.h action.h \
  timestep.h vcheck.h library.h submodeler.h assertion.h column.h \
  mathlib.h librarian.h
alist${OBJ}: alist.C plf.h library.h symbol.h alist.h syntax.h treelog.h \
  time.h mathlib.h assertion.h memutils.h
syntax${OBJ}: syntax.C syntax.h treelog.h symbol.h alist.h library.h \
  metalib.h check.h vcheck.h assertion.h memutils.h
library${OBJ}: library.C library.h symbol.h block.h syntax.h treelog.h plf.h \
  alist.h assertion.h memutils.h
plf${OBJ}: plf.C plf.h assertion.h mathlib.h
mathlib${OBJ}: mathlib.C mathlib.h assertion.h
cdaisy${OBJ}: cdaisy.C scope.h symbol.h model.h block.h syntax.h treelog.h \
  plf.h metalib.h library.h alist.h daisy.h program.h run.h time.h \
  memutils.h output.h condition.h toplevel.h parser_file.h parser.h \
  field.h border.h column.h weather.h im.h action.h horizon.h \
  printer_file.h printer.h version.h chemical.h solute.h adsorption.h \
  assertion.h
nrutil${OBJ}: nrutil.C
submodel${OBJ}: submodel.C submodel.h syntax.h treelog.h symbol.h alist.h \
  assertion.h
version${OBJ}: version.C
reaction_std${OBJ}: reaction_std.C reaction.h model.h alist.h symbol.h \
  block.h syntax.h treelog.h plf.h transform.h chemistry.h chemical.h \
  solute.h adsorption.h soil.h log.h border.h assertion.h librarian.h
chemistry_std${OBJ}: chemistry_std.C chemistry.h model.h symbol.h chemical.h \
  solute.h adsorption.h alist.h reaction.h movement.h geometry.h syntax.h \
  treelog.h mathlib.h assertion.h soil.h soil_water.h block.h plf.h \
  vcheck.h log.h border.h memutils.h librarian.h
groundwater_extern${OBJ}: groundwater_extern.C groundwater.h model.h symbol.h \
  output.h condition.h memutils.h scopesel.h scope.h number.h block.h \
  syntax.h treelog.h plf.h alist.h units.h assertion.h librarian.h
scope_exchange${OBJ}: scope_exchange.C scope.h symbol.h model.h block.h \
  syntax.h treelog.h plf.h alist.h assertion.h memutils.h librarian.h
msoltranrect_none${OBJ}: msoltranrect_none.C msoltranrect.h model.h symbol.h \
  geometry_rect.h geometry_vert.h geometry.h syntax.h treelog.h mathlib.h \
  assertion.h soil.h soil_water.h solute.h adsorption.h element.h alist.h \
  submodeler.h block.h plf.h memutils.h librarian.h
uzrect_Mollerup${OBJ}: uzrect_Mollerup.C uzrect.h model.h symbol.h \
  geometry_rect.h geometry_vert.h geometry.h syntax.h treelog.h mathlib.h \
  assertion.h soil.h soil_water.h soil_heat.h groundwater.h surface.h \
  uzmodel.h log.h border.h alist.h block.h plf.h librarian.h
groundwater_flux${OBJ}: groundwater_flux.C groundwater.h model.h symbol.h \
  syntax.h treelog.h alist.h block.h plf.h check.h librarian.h
msoltranrect_2x1${OBJ}: msoltranrect_2x1.C msoltranrect.h model.h symbol.h \
  geometry_rect.h geometry_vert.h geometry.h syntax.h treelog.h mathlib.h \
  assertion.h transport.h soil.h soil_water.h solute.h adsorption.h \
  element.h alist.h submodeler.h block.h plf.h memutils.h librarian.h
photo_FCC4${OBJ}: photo_FCC4.C photo.h model.h symbol.h cropNdist.h alist.h \
  ABAeffect.h bioclimate.h canopy_std.h canopy_simple.h plf.h phenology.h \
  log.h border.h syntax.h treelog.h block.h submodel.h mathlib.h \
  assertion.h check.h librarian.h
ABAeffect_exp${OBJ}: ABAeffect_exp.C ABAeffect.h model.h alist.h symbol.h \
  mathlib.h assertion.h check.h block.h syntax.h treelog.h plf.h \
  librarian.h
cropNdist_uniform${OBJ}: cropNdist_uniform.C cropNdist.h model.h alist.h \
  symbol.h mathlib.h assertion.h check.h block.h syntax.h treelog.h plf.h \
  librarian.h
uzrect_2x1${OBJ}: uzrect_2x1.C uzrect.h model.h symbol.h uzmodel.h uz1d.h \
  geometry_rect.h geometry_vert.h geometry.h syntax.h treelog.h mathlib.h \
  assertion.h soil.h soil_water.h soil_heat.h groundwater.h surface.h \
  alist.h memutils.h librarian.h
select_flow${OBJ}: select_flow.C select_value.h select.h destination.h \
  symbol.h model.h units.h volume.h block.h syntax.h treelog.h plf.h \
  alist.h border.h geometry.h mathlib.h assertion.h librarian.h
volume_box${OBJ}: volume_box.C volume.h model.h symbol.h syntax.h treelog.h \
  alist.h bound.h border.h mathlib.h assertion.h librarian.h
select_volume${OBJ}: select_volume.C select_value.h select.h destination.h \
  symbol.h model.h units.h volume.h block.h syntax.h treelog.h plf.h \
  alist.h geometry.h mathlib.h assertion.h soil.h librarian.h
uz1d_none${OBJ}: uz1d_none.C uz1d.h model.h geometry_rect.h geometry_vert.h \
  geometry.h syntax.h treelog.h symbol.h mathlib.h assertion.h soil.h \
  soil_water.h soil_heat.h alist.h librarian.h
condition_walltime${OBJ}: condition_walltime.C condition.h model.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h librarian.h
uz1d_richard${OBJ}: uz1d_richard.C uz1d.h model.h geometry_rect.h \
  geometry_vert.h geometry.h syntax.h treelog.h symbol.h mathlib.h \
  assertion.h soil.h soil_water.h soil_heat.h block.h plf.h alist.h \
  average.h librarian.h
cropNdist_DPF${OBJ}: cropNdist_DPF.C cropNdist.h model.h alist.h symbol.h \
  mathlib.h assertion.h block.h syntax.h treelog.h plf.h check.h \
  librarian.h
raddist_DPF${OBJ}: raddist_DPF.C raddist.h model.h alist.h symbol.h block.h \
  syntax.h treelog.h plf.h vegetation.h weather.h im.h mathlib.h \
  assertion.h check.h librarian.h
raddist_std${OBJ}: raddist_std.C raddist.h model.h alist.h symbol.h syntax.h \
  treelog.h vegetation.h mathlib.h assertion.h librarian.h
difrad_DPF${OBJ}: difrad_DPF.C difrad.h model.h alist.h symbol.h block.h \
  syntax.h treelog.h plf.h weather.h im.h fao.h mathlib.h assertion.h \
  check.h librarian.h
difrad_weather${OBJ}: difrad_weather.C difrad.h model.h alist.h symbol.h \
  syntax.h treelog.h weather.h im.h mathlib.h assertion.h librarian.h
number_lisp${OBJ}: number_lisp.C number.h symbol.h model.h alist.h \
  scope_multi.h scope.h submodeler.h block.h syntax.h treelog.h plf.h \
  assertion.h memutils.h librarian.h
condition_extern${OBJ}: condition_extern.C condition.h model.h symbol.h \
  daisy.h program.h run.h time.h memutils.h block.h syntax.h treelog.h \
  plf.h alist.h boolean.h output.h scope.h scopesel.h librarian.h
condition_boolean${OBJ}: condition_boolean.C condition.h model.h symbol.h \
  syntax.h treelog.h alist.h boolean.h scope.h librarian.h
boolean_number${OBJ}: boolean_number.C boolean.h model.h symbol.h syntax.h \
  treelog.h alist.h number.h memutils.h librarian.h
boolean_string${OBJ}: boolean_string.C boolean.h model.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h librarian.h
movement_rect${OBJ}: movement_rect.C movement.h model.h symbol.h \
  geometry_rect.h geometry_vert.h geometry.h syntax.h treelog.h mathlib.h \
  assertion.h soil.h soil_water.h soil_heat.h msoltranrect.h \
  groundwater.h surface.h uzmodel.h weather.h im.h uzrect.h check.h \
  alist.h submodeler.h block.h plf.h memutils.h librarian.h
number_soil${OBJ}: number_soil.C number.h symbol.h model.h metalib.h \
  library.h block.h syntax.h treelog.h plf.h column.h alist.h horizon.h \
  hydraulic.h weather.h im.h output.h condition.h memutils.h time.h \
  units.h librarian.h
organic_none${OBJ}: organic_none.C organic_matter.h model.h symbol.h alist.h \
  geometry.h syntax.h treelog.h mathlib.h assertion.h librarian.h
organic_std${OBJ}: organic_std.C organic_matter.h model.h symbol.h syntax.h \
  treelog.h alist.h submodeler.h block.h plf.h assertion.h log.h border.h \
  am.h om.h som.h smb.h dom.h adsorption.h domsorp.h aom.h clayom.h \
  soil.h geometry.h mathlib.h soil_water.h soil_NH4.h solute.h soil_NO3.h \
  soil_heat.h bioincorporation.h time.h check_range.h check.h vcheck.h \
  gaussj.h memutils.h librarian.h
movement_1D${OBJ}: movement_1D.C movement.h model.h symbol.h geometry1d.h \
  geometry_vert.h geometry.h syntax.h treelog.h mathlib.h assertion.h \
  soil.h soil_water.h soil_heat.h macro.h groundwater.h surface.h \
  uzmodel.h weather.h im.h solute.h adsorption.h element.h transport.h \
  mactrans.h log.h border.h alist.h submodeler.h block.h plf.h memutils.h \
  librarian.h
integer_arit${OBJ}: integer_arit.C integer.h model.h symbol.h syntax.h \
  treelog.h alist.h vcheck.h assertion.h memutils.h librarian.h
source_merge${OBJ}: source_merge.C source.h model.h time.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h gnuplot_utils.h units.h vcheck.h \
  mathlib.h assertion.h memutils.h librarian.h
number_source${OBJ}: number_source.C number.h symbol.h model.h block.h \
  syntax.h treelog.h plf.h alist.h source.h time.h assertion.h \
  librarian.h
program_file${OBJ}: program_file.C program.h model.h symbol.h run.h block.h \
  syntax.h treelog.h plf.h alist.h path.h librarian.h
action_table${OBJ}: action_table.C action.h model.h alist.h symbol.h \
  metalib.h library.h daisy.h program.h run.h time.h memutils.h field.h \
  border.h am.h im.h lexer_table.h block.h syntax.h treelog.h plf.h \
  mathlib.h assertion.h librarian.h
xysource_merge${OBJ}: xysource_merge.C xysource.h model.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h gnuplot_utils.h number.h \
  scope_sources.h scope.h time.h units.h vcheck.h memutils.h librarian.h
xysource_inline${OBJ}: xysource_inline.C xysource.h model.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h gnuplot_utils.h number.h vcheck.h \
  assertion.h librarian.h
xysource_loop${OBJ}: xysource_loop.C xysource.h model.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h gnuplot_utils.h scope_id.h scope.h \
  number.h check.h vcheck.h assertion.h librarian.h
xysource_combine${OBJ}: xysource_combine.C xysource.h model.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h gnuplot_utils.h number.h \
  scope_sources.h scope.h time.h source.h assertion.h librarian.h
gnuplot_xy${OBJ}: gnuplot_xy.C gnuplot_base.h gnuplot.h model.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h xysource.h mathlib.h \
  assertion.h memutils.h librarian.h
xysource_expr${OBJ}: xysource_expr.C xysource.h model.h symbol.h alist.h \
  lexer_table.h block.h syntax.h treelog.h plf.h scope_table.h scope.h \
  gnuplot_utils.h number.h vcheck.h assertion.h librarian.h
gnuplot_multi${OBJ}: gnuplot_multi.C gnuplot.h model.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h source.h time.h memutils.h librarian.h
gnuplot_time${OBJ}: gnuplot_time.C gnuplot_base.h gnuplot.h model.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h source.h time.h mathlib.h \
  assertion.h memutils.h librarian.h
source_combine${OBJ}: source_combine.C source.h model.h time.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h number.h scope_sources.h \
  scope.h gnuplot_utils.h vcheck.h assertion.h librarian.h
number_arit${OBJ}: number_arit.C number.h symbol.h model.h syntax.h treelog.h \
  alist.h units.h vcheck.h mathlib.h assertion.h memutils.h librarian.h
source_expr${OBJ}: source_expr.C source_file.h source.h model.h time.h \
  symbol.h lexer_table.h block.h syntax.h treelog.h plf.h scope_table.h \
  scope.h number.h alist.h librarian.h
source_std${OBJ}: source_std.C source_file.h source.h model.h time.h symbol.h \
  lexer_table.h block.h syntax.h treelog.h plf.h units.h alist.h \
  librarian.h
action_markvand${OBJ}: action_markvand.C action.h model.h alist.h symbol.h \
  block.h syntax.h treelog.h plf.h daisy.h program.h run.h time.h \
  memutils.h field.h border.h crop.h im.h fao.h log.h mathlib.h \
  assertion.h check.h vcheck.h librarian.h
photo_Farquhar${OBJ}: photo_Farquhar.C photo.h model.h symbol.h block.h \
  syntax.h treelog.h plf.h cropNdist.h alist.h ABAeffect.h bioclimate.h \
  canopy_std.h canopy_simple.h phenology.h log.h border.h submodel.h \
  mathlib.h assertion.h check.h librarian.h
photo_GL${OBJ}: photo_GL.C photo.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h canopy_std.h canopy_simple.h phenology.h alist.h \
  submodel.h mathlib.h assertion.h check.h librarian.h
program_gnuplot${OBJ}: program_gnuplot.C program.h model.h symbol.h run.h \
  block.h syntax.h treelog.h plf.h alist.h gnuplot.h path.h memutils.h \
  librarian.h
program_document${OBJ}: program_document.C program.h model.h symbol.h run.h \
  library.h metalib.h block.h syntax.h treelog.h plf.h alist.h submodel.h \
  printer_file.h printer.h xref.h format.h assertion.h librarian.h
program_batch${OBJ}: program_batch.C program.h model.h symbol.h run.h block.h \
  syntax.h treelog.h plf.h alist.h path.h assertion.h memutils.h \
  librarian.h
summary_balance${OBJ}: summary_balance.C summary.h model.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h fetch.h destination.h select.h units.h \
  volume.h memutils.h librarian.h
rootdens_AP${OBJ}: rootdens_AP.C rootdens.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h geometry.h mathlib.h assertion.h log.h border.h alist.h \
  check.h librarian.h
number_const${OBJ}: number_const.C number.h symbol.h model.h block.h syntax.h \
  treelog.h plf.h alist.h scope.h units.h assertion.h librarian.h
equil_goal${OBJ}: equil_goal.C equil.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h alist.h soil_water.h pedo.h soil.h check.h mathlib.h \
  assertion.h librarian.h
pedo_arit${OBJ}: pedo_arit.C pedo.h model.h symbol.h alist.h soil.h \
  treelog_text.h treelog.h submodeler.h block.h syntax.h plf.h \
  assertion.h units.h vcheck.h mathlib.h memutils.h librarian.h
domsorp_std${OBJ}: domsorp_std.C domsorp.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h transform.h dom.h adsorption.h som.h om.h soil.h log.h \
  border.h alist.h assertion.h librarian.h
equil_linear${OBJ}: equil_linear.C equil.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h alist.h pedo.h soil.h check.h mathlib.h assertion.h \
  librarian.h
pedo_const${OBJ}: pedo_const.C pedo.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h alist.h soil.h units.h vcheck.h assertion.h librarian.h
horizon_numeric${OBJ}: horizon_numeric.C horizon.h model.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h texture.h hydraulic.h check.h vcheck.h \
  mathlib.h assertion.h librarian.h
horizon_system${OBJ}: horizon_system.C horizon.h model.h symbol.h library.h \
  block.h syntax.h treelog.h plf.h alist.h texture.h hydraulic.h check.h \
  mathlib.h assertion.h librarian.h
select_pF${OBJ}: select_pF.C select.h destination.h symbol.h model.h units.h \
  volume.h block.h syntax.h treelog.h plf.h alist.h mathlib.h assertion.h \
  check.h vcheck.h librarian.h
pet_FAO_PM${OBJ}: pet_FAO_PM.C pet.h model.h alist.h symbol.h syntax.h \
  treelog.h fao.h weather.h im.h soil.h surface.h uzmodel.h soil_heat.h \
  vegetation.h log.h border.h librarian.h
pet_Hargreaves${OBJ}: pet_Hargreaves.C pet.h model.h alist.h symbol.h \
  syntax.h treelog.h weather.h im.h fao.h log.h border.h mathlib.h \
  assertion.h librarian.h
hydraulic_M_vGp${OBJ}: hydraulic_M_vGp.C hydraulic.h model.h syntax.h \
  treelog.h symbol.h block.h plf.h alist.h mathlib.h assertion.h check.h \
  librarian.h
summary_simple${OBJ}: summary_simple.C summary.h model.h symbol.h alist.h \
  fetch.h destination.h select.h units.h volume.h treelog.h memutils.h \
  submodeler.h block.h syntax.h plf.h assertion.h librarian.h
phenology_TSum${OBJ}: phenology_TSum.C phenology.h model.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h production.h vernalization.h \
  assertion.h librarian.h
phenology_std${OBJ}: phenology_std.C phenology.h model.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h production.h vernalization.h mathlib.h \
  assertion.h librarian.h
hydraulic_hypres${OBJ}: hydraulic_hypres.C hydraulic.h model.h syntax.h \
  treelog.h symbol.h block.h plf.h alist.h texture.h mathlib.h \
  assertion.h librarian.h
clayom_biomod${OBJ}: clayom_biomod.C clayom.h model.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h check.h smb.h om.h soil.h mathlib.h \
  assertion.h librarian.h
clayom_old${OBJ}: clayom_old.C clayom.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h alist.h smb.h om.h soil.h assertion.h librarian.h
hydraulic_Cosby${OBJ}: hydraulic_Cosby.C hydraulic.h model.h syntax.h \
  treelog.h symbol.h alist.h texture.h plf.h mathlib.h assertion.h \
  librarian.h
adsorption_full${OBJ}: adsorption_full.C adsorption.h model.h symbol.h \
  syntax.h treelog.h alist.h mathlib.h assertion.h librarian.h
equil_langmuir${OBJ}: equil_langmuir.C equil.h model.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h pedo.h soil.h check.h mathlib.h \
  assertion.h librarian.h
transform_equil${OBJ}: transform_equil.C transform.h model.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h soil.h soil_water.h pedo.h equil.h \
  check.h mathlib.h assertion.h librarian.h
condition_weather${OBJ}: condition_weather.C condition.h model.h symbol.h \
  block.h syntax.h treelog.h plf.h field.h border.h daisy.h program.h \
  run.h time.h memutils.h check.h log.h alist.h librarian.h
rootdens_PLF${OBJ}: rootdens_PLF.C rootdens.h model.h symbol.h alist.h \
  geometry.h syntax.h treelog.h mathlib.h assertion.h plf.h submodeler.h \
  block.h check.h vcheck.h memutils.h librarian.h
rootdens_G_P${OBJ}: rootdens_G_P.C rootdens.h model.h symbol.h block.h \
  syntax.h treelog.h plf.h geometry.h mathlib.h assertion.h log.h \
  border.h alist.h check.h librarian.h
groundwater_file${OBJ}: groundwater_file.C groundwater.h model.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h lexer_data.h lexer.h \
  assertion.h time.h librarian.h
action_fertilize${OBJ}: action_fertilize.C action.h model.h alist.h symbol.h \
  block.h syntax.h treelog.h plf.h daisy.h program.h run.h time.h \
  memutils.h field.h border.h am.h im.h check.h assertion.h librarian.h
action_repeat${OBJ}: action_repeat.C action.h model.h alist.h symbol.h \
  daisy.h program.h run.h time.h memutils.h block.h syntax.h treelog.h \
  plf.h log.h border.h librarian.h
vegetation_permanent${OBJ}: vegetation_permanent.C vegetation.h model.h \
  symbol.h plf.h mathlib.h assertion.h log.h border.h alist.h litter.h \
  root_system.h rootdens.h canopy_simple.h time.h geometry.h syntax.h \
  treelog.h soil.h crop.h am.h aom.h om.h organic_matter.h submodeler.h \
  block.h check.h librarian.h
vegetation_crops${OBJ}: vegetation_crops.C vegetation.h model.h symbol.h \
  crop.h time.h alist.h organic_matter.h geometry.h syntax.h treelog.h \
  mathlib.h assertion.h soil.h plf.h harvest.h block.h log.h border.h \
  librarian.h
crop_simple${OBJ}: crop_simple.C crop.h time.h alist.h symbol.h model.h \
  root_system.h rootdens.h plf.h canopy_simple.h log.h border.h \
  bioclimate.h soil_water.h geometry.h syntax.h treelog.h mathlib.h \
  assertion.h soil.h aom.h om.h organic_matter.h soil_heat.h soil_NH4.h \
  solute.h adsorption.h soil_NO3.h am.h harvest.h block.h submodeler.h \
  check.h librarian.h
action_ridge${OBJ}: action_ridge.C action.h model.h alist.h symbol.h block.h \
  syntax.h treelog.h plf.h daisy.h program.h run.h time.h memutils.h \
  field.h border.h ridge.h librarian.h
groundwater_fixed${OBJ}: groundwater_fixed.C groundwater.h model.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h check.h assertion.h \
  librarian.h
groundwater_deep${OBJ}: groundwater_deep.C groundwater.h model.h symbol.h \
  syntax.h treelog.h alist.h assertion.h librarian.h
action_heat${OBJ}: action_heat.C action.h model.h alist.h symbol.h block.h \
  syntax.h treelog.h plf.h daisy.h program.h run.h time.h memutils.h \
  field.h border.h check.h librarian.h
hydraulic_M_vG_compact${OBJ}: hydraulic_M_vG_compact.C hydraulic.h model.h \
  syntax.h treelog.h symbol.h block.h plf.h alist.h mathlib.h assertion.h \
  librarian.h
action_crop${OBJ}: action_crop.C action.h model.h alist.h symbol.h daisy.h \
  program.h run.h time.h memutils.h field.h border.h crop.h am.h log.h \
  harvest.h block.h syntax.h treelog.h plf.h check_range.h check.h im.h \
  submodeler.h assertion.h vcheck.h mathlib.h librarian.h
groundwater_lysimeter${OBJ}: groundwater_lysimeter.C groundwater.h model.h \
  symbol.h alist.h geometry.h syntax.h treelog.h mathlib.h assertion.h \
  librarian.h
action_message${OBJ}: action_message.C action.h model.h alist.h symbol.h \
  block.h syntax.h treelog.h plf.h condition.h log.h border.h daisy.h \
  program.h run.h time.h memutils.h librarian.h
weather_std${OBJ}: weather_std.C weather.h model.h im.h symbol.h alist.h \
  fao.h lexer_data.h lexer.h time.h plf.h mathlib.h assertion.h units.h \
  submodeler.h block.h syntax.h treelog.h check.h vcheck.h memutils.h \
  librarian.h
groundwater_pipe${OBJ}: groundwater_pipe.C groundwater.h model.h symbol.h \
  block.h syntax.h treelog.h plf.h log.h border.h alist.h geometry.h \
  mathlib.h assertion.h soil.h soil_heat.h soil_water.h depth.h check.h \
  librarian.h
select_index${OBJ}: select_index.C select_value.h select.h destination.h \
  symbol.h model.h units.h volume.h block.h syntax.h treelog.h plf.h \
  alist.h librarian.h
select_content${OBJ}: select_content.C select_value.h select.h destination.h \
  symbol.h model.h units.h volume.h block.h syntax.h treelog.h plf.h \
  alist.h geometry.h mathlib.h assertion.h soil.h check.h librarian.h
select_number${OBJ}: select_number.C select_value.h select.h destination.h \
  symbol.h model.h units.h volume.h syntax.h treelog.h alist.h \
  librarian.h
select_array${OBJ}: select_array.C select.h destination.h symbol.h model.h \
  units.h volume.h block.h syntax.h treelog.h plf.h alist.h mathlib.h \
  assertion.h librarian.h
log_table${OBJ}: log_table.C log_select.h log.h border.h model.h alist.h \
  symbol.h memutils.h library.h block.h syntax.h treelog.h plf.h select.h \
  destination.h units.h volume.h summary.h geometry.h mathlib.h \
  assertion.h dlf.h daisy.h program.h run.h time.h timestep.h vcheck.h \
  librarian.h
log_harvest${OBJ}: log_harvest.C log.h border.h model.h alist.h symbol.h \
  daisy.h program.h run.h time.h memutils.h harvest.h block.h syntax.h \
  treelog.h plf.h dlf.h vcheck.h version.h assertion.h librarian.h
action_while${OBJ}: action_while.C action.h model.h alist.h symbol.h syntax.h \
  treelog.h log.h border.h assertion.h memutils.h librarian.h
action_wait${OBJ}: action_wait.C action.h model.h alist.h symbol.h block.h \
  syntax.h treelog.h plf.h condition.h log.h border.h daisy.h program.h \
  run.h time.h memutils.h assertion.h librarian.h
action_activity${OBJ}: action_activity.C action.h model.h alist.h symbol.h \
  syntax.h treelog.h log.h border.h memutils.h librarian.h
mactrans_std${OBJ}: mactrans_std.C mactrans.h model.h symbol.h alist.h \
  soil_water.h geometry1d.h geometry_vert.h geometry.h syntax.h treelog.h \
  mathlib.h assertion.h plf.h librarian.h
macro_std${OBJ}: macro_std.C macro.h model.h syntax.h treelog.h symbol.h \
  block.h plf.h geometry1d.h geometry_vert.h geometry.h mathlib.h \
  assertion.h soil.h surface.h uzmodel.h log.h border.h alist.h check.h \
  vcheck.h librarian.h
macro_none${OBJ}: macro_none.C macro.h model.h syntax.h treelog.h symbol.h \
  alist.h librarian.h
column_std${OBJ}: column_std.C column.h model.h alist.h symbol.h library.h \
  surface.h uzmodel.h soil_heat.h macro.h syntax.h treelog.h movement.h \
  groundwater.h geometry.h mathlib.h assertion.h soil.h soil_water.h \
  vegetation.h bioclimate.h weather.h im.h chemistry.h soil_NH4.h \
  solute.h adsorption.h soil_NO3.h organic_matter.h denitrification.h \
  plf.h am.h dom.h time.h log.h border.h submodeler.h block.h memutils.h \
  librarian.h
weather_simple${OBJ}: weather_simple.C weather_old.h weather.h model.h im.h \
  symbol.h block.h syntax.h treelog.h plf.h time.h log.h border.h alist.h \
  mathlib.h assertion.h librarian.h
uzrichard${OBJ}: uzrichard.C uzmodel.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h groundwater.h surface.h geometry_vert.h geometry.h \
  mathlib.h assertion.h soil.h soil_heat.h alist.h log.h border.h \
  average.h librarian.h
hydraulic_yolo${OBJ}: hydraulic_yolo.C hydraulic.h model.h syntax.h treelog.h \
  symbol.h block.h plf.h alist.h mathlib.h assertion.h librarian.h
hydraulic_M_vG${OBJ}: hydraulic_M_vG.C hydraulic.h model.h syntax.h treelog.h \
  symbol.h block.h plf.h alist.h mathlib.h assertion.h librarian.h
hydraulic_B_vG${OBJ}: hydraulic_B_vG.C hydraulic.h model.h syntax.h treelog.h \
  symbol.h block.h plf.h alist.h mathlib.h assertion.h librarian.h
hydraulic_M_C${OBJ}: hydraulic_M_C.C hydraulic.h model.h syntax.h treelog.h \
  symbol.h block.h plf.h alist.h check.h mathlib.h assertion.h \
  librarian.h
hydraulic_B_C${OBJ}: hydraulic_B_C.C hydraulic.h model.h syntax.h treelog.h \
  symbol.h block.h plf.h alist.h check.h mathlib.h assertion.h \
  librarian.h
hydraulic_M_BaC${OBJ}: hydraulic_M_BaC.C hydraulic.h model.h syntax.h \
  treelog.h symbol.h block.h plf.h alist.h check.h mathlib.h assertion.h \
  librarian.h
hydraulic_B_BaC${OBJ}: hydraulic_B_BaC.C hydraulic.h model.h syntax.h \
  treelog.h symbol.h block.h plf.h alist.h mathlib.h assertion.h \
  librarian.h
groundwater_static${OBJ}: groundwater_static.C groundwater.h model.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h assertion.h librarian.h
horizon_std${OBJ}: horizon_std.C horizon.h model.h symbol.h block.h syntax.h \
  treelog.h plf.h alist.h texture.h hydraulic.h check.h mathlib.h \
  assertion.h librarian.h
crop_std${OBJ}: crop_std.C crop.h time.h alist.h symbol.h model.h chemistry.h \
  root_system.h rootdens.h plf.h canopy_std.h canopy_simple.h \
  harvesting.h production.h phenology.h partition.h vernalization.h \
  photo.h crpn.h wse.h log.h border.h timestep.h vcheck.h bioclimate.h \
  soil_water.h geometry.h syntax.h treelog.h mathlib.h assertion.h soil.h \
  organic_matter.h soil_heat.h am.h submodeler.h block.h librarian.h
action_sow${OBJ}: action_sow.C action.h model.h alist.h symbol.h block.h \
  syntax.h treelog.h plf.h daisy.h program.h run.h time.h memutils.h \
  field.h border.h crop.h librarian.h
action_stop${OBJ}: action_stop.C action.h model.h alist.h symbol.h syntax.h \
  treelog.h daisy.h program.h run.h time.h memutils.h librarian.h
condition_time${OBJ}: condition_time.C condition.h model.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h time.h daisy.h program.h run.h \
  memutils.h vcheck.h librarian.h
condition_logic${OBJ}: condition_logic.C condition.h model.h symbol.h \
  syntax.h treelog.h alist.h memutils.h librarian.h
action_irrigate${OBJ}: action_irrigate.C action.h model.h alist.h symbol.h \
  block.h syntax.h treelog.h plf.h daisy.h program.h run.h time.h \
  memutils.h field.h border.h im.h check.h mathlib.h assertion.h \
  librarian.h
action_lisp${OBJ}: action_lisp.C action.h model.h alist.h symbol.h daisy.h \
  program.h run.h time.h memutils.h log.h border.h submodeler.h block.h \
  syntax.h treelog.h plf.h assertion.h librarian.h condition.h
weather_none${OBJ}: weather_none.C weather_old.h weather.h model.h im.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h librarian.h
action_tillage${OBJ}: action_tillage.C action.h model.h alist.h symbol.h \
  block.h syntax.h treelog.h plf.h daisy.h program.h run.h time.h \
  memutils.h field.h border.h check.h librarian.h
action_harvest${OBJ}: action_harvest.C action.h model.h alist.h symbol.h \
  daisy.h program.h run.h time.h memutils.h field.h border.h harvest.h \
  block.h syntax.h treelog.h plf.h librarian.h
crop_old${OBJ}: crop_old.C crop.h time.h alist.h symbol.h model.h log.h \
  border.h bioclimate.h plf.h soil_water.h soil.h geometry.h syntax.h \
  treelog.h mathlib.h assertion.h aom.h om.h organic_matter.h soil_heat.h \
  soil_NH4.h solute.h adsorption.h soil_NO3.h am.h harvest.h block.h \
  librarian.h
crop_sold${OBJ}: crop_sold.C crop.h time.h alist.h symbol.h model.h log.h \
  border.h bioclimate.h plf.h soil_water.h soil.h geometry.h syntax.h \
  treelog.h mathlib.h assertion.h organic_matter.h aom.h om.h soil_heat.h \
  soil_NH4.h solute.h adsorption.h soil_NO3.h am.h harvest.h block.h \
  librarian.h
action_with${OBJ}: action_with.C action.h model.h alist.h symbol.h block.h \
  syntax.h treelog.h plf.h daisy.h program.h run.h time.h memutils.h \
  field.h border.h log.h librarian.h
nitrification_soil${OBJ}: nitrification_soil.C nitrification.h model.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h mathlib.h assertion.h \
  check.h librarian.h
nitrification_solute${OBJ}: nitrification_solute.C nitrification.h model.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h soil.h soil_water.h \
  soil_heat.h soil_NH4.h solute.h adsorption.h soil_NO3.h mathlib.h \
  assertion.h check.h librarian.h
hydraulic_mod_C${OBJ}: hydraulic_mod_C.C hydraulic.h model.h syntax.h \
  treelog.h symbol.h block.h plf.h alist.h check.h mathlib.h assertion.h \
  librarian.h
uzlr${OBJ}: uzlr.C uzmodel.h model.h symbol.h block.h syntax.h treelog.h \
  plf.h alist.h surface.h groundwater.h geometry_vert.h geometry.h \
  mathlib.h assertion.h soil.h soil_heat.h librarian.h
transport_cd${OBJ}: transport_cd.C transport.h model.h syntax.h treelog.h \
  symbol.h block.h plf.h geometry1d.h geometry_vert.h geometry.h \
  mathlib.h assertion.h soil.h soil_water.h adsorption.h log.h border.h \
  alist.h librarian.h
transport_none${OBJ}: transport_none.C transport.h model.h syntax.h treelog.h \
  symbol.h geometry1d.h geometry_vert.h geometry.h mathlib.h assertion.h \
  soil.h soil_water.h adsorption.h log.h border.h alist.h librarian.h
transport_convection${OBJ}: transport_convection.C transport.h model.h \
  syntax.h treelog.h symbol.h block.h plf.h geometry1d.h geometry_vert.h \
  geometry.h mathlib.h assertion.h soil.h soil_water.h adsorption.h log.h \
  border.h alist.h librarian.h
adsorption_vS_S${OBJ}: adsorption_vS_S.C adsorption.h model.h symbol.h \
  syntax.h treelog.h alist.h soil.h mathlib.h assertion.h librarian.h
adsorption_none${OBJ}: adsorption_none.C adsorption.h model.h symbol.h \
  syntax.h treelog.h alist.h librarian.h
tortuosity_M_Q${OBJ}: tortuosity_M_Q.C tortuosity.h model.h symbol.h syntax.h \
  treelog.h alist.h hydraulic.h mathlib.h assertion.h librarian.h
tortuosity_linear${OBJ}: tortuosity_linear.C tortuosity.h model.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h hydraulic.h librarian.h
adsorption_freundlich${OBJ}: adsorption_freundlich.C adsorption.h model.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h soil.h check.h \
  mathlib.h assertion.h librarian.h
adsorption_linear${OBJ}: adsorption_linear.C adsorption.h model.h symbol.h \
  block.h syntax.h treelog.h plf.h alist.h check.h soil.h librarian.h
adsorption_langmuir${OBJ}: adsorption_langmuir.C adsorption.h model.h \
  symbol.h block.h syntax.h treelog.h plf.h alist.h soil.h check.h \
  mathlib.h assertion.h librarian.h
bioclimate_std${OBJ}: bioclimate_std.C bioclimate.h model.h alist.h symbol.h \
  metalib.h library.h block.h syntax.h treelog.h plf.h surface.h \
  uzmodel.h weather.h im.h geometry.h mathlib.h assertion.h soil.h \
  soil_heat.h snow.h log.h border.h net_radiation.h pet.h difrad.h \
  raddist.h svat.h vegetation.h time.h check.h fao.h librarian.h
condition_crop${OBJ}: condition_crop.C condition.h model.h symbol.h block.h \
  syntax.h treelog.h plf.h crop.h time.h alist.h field.h border.h daisy.h \
  program.h run.h memutils.h check_range.h check.h mathlib.h assertion.h \
  librarian.h
condition_soil${OBJ}: condition_soil.C condition.h model.h symbol.h block.h \
  syntax.h treelog.h plf.h alist.h field.h border.h daisy.h program.h \
  run.h time.h memutils.h check.h librarian.h
log_checkpoint${OBJ}: log_checkpoint.C log_alist.h log.h border.h model.h \
  alist.h symbol.h metalib.h block.h syntax.h treelog.h plf.h condition.h \
  daisy.h program.h run.h time.h memutils.h printer_file.h printer.h \
  assertion.h librarian.h
uznone${OBJ}: uznone.C uzmodel.h model.h symbol.h syntax.h treelog.h alist.h \
  soil.h mathlib.h assertion.h librarian.h
condition_daisy${OBJ}: condition_daisy.C condition.h model.h symbol.h \
  syntax.h treelog.h alist.h daisy.h program.h run.h time.h memutils.h \
  librarian.h
chemical_std${OBJ}: chemical_std.C chemical.h model.h solute.h adsorption.h \
  symbol.h alist.h organic_matter.h soil_heat.h soil_water.h soil.h \
  geometry.h syntax.h treelog.h mathlib.h assertion.h log.h border.h \
  block.h plf.h check.h librarian.h
hydraulic_M_BaC_Bimodal${OBJ}: hydraulic_M_BaC_Bimodal.C hydraulic.h model.h \
  syntax.h treelog.h symbol.h block.h plf.h alist.h check.h mathlib.h \
  assertion.h librarian.h
hydraulic_B_BaC_Bimodal${OBJ}: hydraulic_B_BaC_Bimodal.C hydraulic.h model.h \
  syntax.h treelog.h symbol.h block.h plf.h alist.h check.h mathlib.h \
  assertion.h librarian.h
pet_makkink${OBJ}: pet_makkink.C pet.h model.h alist.h symbol.h syntax.h \
  treelog.h weather.h im.h fao.h log.h border.h librarian.h
pet_weather${OBJ}: pet_weather.C pet.h model.h alist.h symbol.h syntax.h \
  treelog.h weather.h im.h log.h border.h librarian.h
svat_none${OBJ}: svat_none.C svat.h model.h symbol.h syntax.h treelog.h \
  alist.h librarian.h
action_spray${OBJ}: action_spray.C action.h model.h alist.h symbol.h \
  metalib.h library.h block.h syntax.h treelog.h plf.h daisy.h program.h \
  run.h time.h memutils.h field.h border.h chemical.h solute.h \
  adsorption.h check.h librarian.h
pet_PM${OBJ}: pet_PM.C pet.h model.h alist.h symbol.h syntax.h treelog.h \
  fao.h weather.h im.h soil.h surface.h uzmodel.h soil_heat.h \
  vegetation.h log.h border.h librarian.h
svat_pmsw${OBJ}: svat_pmsw.C svat.h model.h symbol.h mathlib.h assertion.h \
  block.h syntax.h treelog.h plf.h surface.h uzmodel.h weather.h im.h \
  time.h soil.h soil_water.h soil_heat.h vegetation.h pet.h alist.h log.h \
  border.h fao.h gaussj.h librarian.h nrutil.h
action_surface${OBJ}: action_surface.C action.h model.h alist.h symbol.h \
  block.h syntax.h treelog.h plf.h daisy.h program.h run.h time.h \
  memutils.h field.h border.h check.h librarian.h
main${OBJ}: main.C toplevel.h
cmain${OBJ}: cmain.c cdaisy.h
bugmain${OBJ}: bugmain.c cdaisy.h
