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
NATIVEEXE = daisy
USE_GUI = none
BOOSTINC = -isystem $(HOME)/boost/include/boost-1_35/
FTPDIR = /home/ftp/pub/daisy
WWWINDEX = /home/user_3/daisy/.public_html/index.html
else
SRCDIR = ..
OBJHOME = obj
NATIVEHOME = $(OBJHOME)
NATIVEEXE = daisy.exe daisyw.exe
USE_GUI = Q4
BOOSTINC = -isystem $(CYGHOME)/usr/include/boost-1_33_1
#BOOSTINC = -isystem /cygdrive/c/boostcvs
#BOOSTINC = -isystem /cygdrive/c/boostsvn/boost-sandbox
SETUPDIR = /home/abraham/daisy/install
MAKENSIS = "/cygdrive/c/Program Files/NSIS/makensis.exe"
MINGWHOME = /cygdrive/c/MinGW
endif

TARGETTYPE = i586-mingw32msvc
SVNROOT = https://daisy-model.googlecode.com/svn

# Set USE_GUI to Q4 or none, depending on what GUI you want.
#

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
#MSSRC = win32_unistd.c
MSSRC =

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
CROSSGCC = $(GCC) # "$(TARGETTYPE)-gcc"

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
	WARNING = -Wall -Wno-uninitialized \
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
#  -Wextra: Triggers dllexport/inline: -Wno-attributes is not in GCC 3
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

#csdaisydll.exe:	csmain.cs csdaisy.dll
#	$(CSHARP) /out:csdaisy.exe /r:csdaisy.dll csmain.cs 

#csdaisy.dll: csdaisy.cs
#	$(CSHARP) /target:library csdaisy.cs

.cs.netmodule:
	$(CSHARP) /target:module $<

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

# Locate the CSSparse lib
CXSPARSELIB = -L../libdeps -lcxsparse
CXSPARSEHEAD = ublas_cxsparse.h cs.h UFconfig.h

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
Q4HOME = /cygdrive/c/Qt/4.3.1
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
LATER = 
MODELS = groundwater_aquitard.C \
	heatrect_Mollerup.C heatrect_linear.C heatrect_none.C \
	msoltranrect_convection.C \
	ABAprod_uptake.C ABAprod_soil.C ABAprod_root.C \
	solver_ublas.C solver_cxsparse.C solver_none.C \
	movement_rect.C chemistry_multi.C \
	equil_goal.C equil_linear.C equil_langmuir.C transform_equil.C \
	reaction_nit.C reaction_denit.C \
	reaction_adsorption.C reaction_equil.C rootdens_GP2D.C \
	rootdens_GP1D.C number_plf.C rubiscoNdist_forced.C action_extern.C \
	rubiscoNdist_expr.C uzrect_const.C photo_FCC3.C photo_FCC4.C \
	msoltranrect_Mollerup.C reaction_std.C chemistry_std.C \
	groundwater_extern.C \
	msoltranrect_none.C uzrect_Mollerup.C groundwater_flux.C \
	ABAeffect_exp.C rubiscoNdist_uniform.C \
	uzrect_2x1.C select_flow.C volume_box.C \
	select_volume.C uz1d_none.C condition_walltime.C uz1d_richard.C \
	rubiscoNdist_DPF.C raddist_DPF.C raddist_std.C difrad_DPF.C \
        difrad_weather.C number_lisp.C condition_extern.C condition_boolean.C \
	boolean_number.C boolean_string.C \
	number_soil.C organic_none.C \
	organic_std.C movement_1D.C integer_arit.C \
	source_merge.C number_source.C program_file.C action_table.C \
	xysource_merge.C xysource_inline.C xysource_loop.C \
	xysource_combine.C gnuplot_xy.C xysource_expr.C gnuplot_multi.C \
	gnuplot_time.C source_combine.C number_arit.C source_expr.C \
	source_std.C action_markvand.C  photo_GL.C \
	program_gnuplot.C \
	program_document.C program_batch.C summary_balance.C \
	rootdens_AP.C number_const.C \
	domsorp_std.C \
	horizon_numeric.C horizon_system.C select_pF.C pet_FAO_PM.C \
	pet_Hargreaves.C hydraulic_M_vGp.C summary_simple.C \
	phenology_TSum.C phenology_std.C hydraulic_hypres.C clayom_biomod.C \
        clayom_old.C hydraulic_Cosby.C \
	condition_weather.C \
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
	tortuosity_M_Q.C tortuosity_linear.C \
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
COMPONENTS = mobsol.C heatrect.C unit.C \
	ABAprod.C solver.C element.C ui.C reaction.C scopesel.C scope.C \
	ABAeffect.C msoltranrect.C uzrect.C bound.C volume.C uz1d.C \
	rubiscoNdist.C raddist.C difrad.C organic_matter.C movement.C integer.C\
	xysource.C gnuplot.C boolean.C stringer.C source.C photo.C \
	format.C depth.C wse.C program.C number.C domsorp.C chemistry.C \
	summary.C nitrification.C phenology.C clayom.C equil.C \
	transform.C rootdens.C select.C average.C mactrans.C macro.C \
	parser.C log.C weather.C column.C crop.C \
	action.C condition.C horizon.C 	uzmodel.C hydraulic.C \
	bioclimate.C groundwater.C am.C transport.C \
	adsorption.C tortuosity.C printer.C chemical.C \
	pet.C net_radiation.C svat.C vegetation.C 

# Submodels are combined models and components.
#
SUBMODELS = toplevel.C timestep.C geometry_rect.C doe.C \
        geometry1d.C fetch.C horheat.C litter.C time.C \
	som.C smb.C aom.C dom.C crpn.C vernalization.C \
	partition.C production.C \
	harvesting.C canopy_simple.C canopy_std.C root_system.C \
	ridge.C soil.C surface.C soil_water.C \
	soil_heat.C \
	snow.C im.C harvest.C field.C \
	bioincorporation.C 

# Special or intermediate models with their own interface.
#
SPECIALS = scope_exchange.C photo_Farquhar.C \
	scope_multi.C scope_id.C geometry_vert.C gnuplot_base.C \
	source_file.C format_LaTeX.C log_all.C om.C select_value.C \
	weather_old.C log_extern.C log_select.C parser_file.C \
	geometry.C printer_file.C log_alist.C

# Various utility code that are neither a component nor a (sub)model.
# 
OTHER = frame.C \
	bdconv.C abiotic.C scope_soil.C run.C treelog_text.C treelog_store.C \
	intrinsics.C metalib.C model.C output.C scope_block.C librarian.C \
	gnuplot_utils.C scope_sources.C scope_table.C lexer_table.C \
	block.C dlf.C texture.C destination.C symbol.C \
	fao.C gaussj.C vcheck.C assertion.C xref.C units.C \
	check.C check_range.C path.C traverse_delete.C \
	depend.C traverse.C treelog.C \
	lexer_data.C lexer.C daisy.C alist.C syntax.C library.C plf.C \
	mathlib.C cdaisy.C nrutil.C submodel.C version.C

# Utilities in header or source alone.
HEADONLY = submodeler.h border.h memutils.h iterative.h $(CXSPARSEHEAD)
SRCONLY = 

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

Q4MOCHDR = run_Qt.h vis_Qt.h log_Qt.h ui_Qt_run.h
Q4MOCSRC = $(Q4MOCHDR:.h=_moc.C)
Q4HEADERS = $(Q4MOCHDR) ui_Qt.h
Q4SOURCES = $(Q4HEADERS:.h=.C) main_Qt.C
Q4OBJECTS = $(Q4SOURCES:.C=${OBJ}) $(Q4MOCHDR:.h=_moc${OBJ}) 

ifeq ($(USE_GUI),Q4)
GUISOURCES = $(Q4SOURCES) 
GUIOBJECTS = $(Q4OBJECTS)
GUILIB = $(Q4LIB)
GUIINCLUDE = $(Q4INCLUDE)
GUIDLL = daisy_Qt.dll
MOC = $(Q4MOC)
else
GUISOURCES =
GUIOBJECTS = 
GUILIB = 
GUIINCLUDE = 
GUIDLL =
endif

ALLGUISRC = tkmain.C gmain.C $(QTSOURCES) $(Q4SOURCES)
ALLGUIHDR = $(QTHEADERS) $(Q4HEADERS)

LOSTGUISRC = pmain.C

# Select the C files that are not part of the library.
#
MAIN = main.C 

# The object files used in the daisy library.
#
LIBOBJ = $(INTERFACES:.C=${OBJ}) $(MODELS:.C=${OBJ}) $(SYSOBJECTS) \
	$(SRCONLY:.C=$(OBJ))

# Find all object files, header files, and source files.
#
OBJECTS = $(LIBOBJ) $(MAIN:.C=${OBJ}) cmain${OBJ} bugmain.o
SOURCES = $(INTERFACES) $(MODELS)  $(MAIN) \
	 cmain.c bugmain.c $(SRCONLY)
HEADERS = $(INTERFACES:.C=.h) $(HEADONLY)

# Find all printable files.
#
TEXT =  ChangeLog.3 ChangeLog.2 ChangeLog.1 setup.nsi \
	Makefile ChangeLog TODO NEWS COPYING COPYING.LIB  $(DISABLED) \
	$(HEADERS) $(SOURCES) $(ALLSYSHDR) $(ALLSYSSRC) \
	$(ALLGUIHDR) $(ALLGUISRC) $(UTESTSRC)

# The executables.
#
EXECUTABLES = daisy${EXE} tkdaisy${EXE} cdaisy${EXE} gdaisy${EXE}

# Select files to be removed by the next svn update.
#
REMOVE = msoltranrect_2x1.C

REMOVED = select_soil.C adsorption_none.C adsorption_full.C ABAprod_expr.C \
	solute.C solute.h pedo.C pedo.h pedo_arit.C pedo_const.C \
	denitrification.C soil_NH4.C soil_NO3.C \
	denitrification.h soil_NH4.h soil_NO3.h \
	soil_heat1d.h soil_heat1d.C soil_heat_rect.h soil_heat_rect.C \
	ui_Qt_read.h ui_Qt_read.C \
	tlink32.ini daisy.bpr daisy.bpf daisy.bpg Daisy.vcproj q4main.C treelog_stream.C treelog_stream.h treelog_dual.C treelog_dual.h soil_chemical.C soil_chemicals.C chemicals.C soil_chemical.h soil_chemicals.h chemicals.h boolean_extern.C number_extern.C options.C options.h select_interval.C select_utils.h select_utils.C select_flux_top.C select_flux_bottom.C select_flux.C select_flux.h column_base.h

# These are the file extensions we deal with.
# 
.SUFFIXES:	.C ${OBJ} .h .c ${EXE} .a .cs .netmodule

# Create all the executables.
#
all:	#(EXECUTABLES)
	@echo 'Use "make native" to create a native Daisy executable.'

# Create a DLL.
#
daisy.dll: $(LIBOBJ) 
	$(CC) -shared -o $@ $^ $(CPPLIB) $(MATHLIB) $(CXSPARSELIB) -Wl,--out-implib,libdaisy.a 

daisy_Qt.dll: $(Q4OBJECTS) daisy.dll
	$(CC) -shared -o $@ $^ $(GUILIB) $(CPPLIB) $(MATHLIB) -Wl,--out-implib,libdaisy_Qt.a 


# Create the main executable.
#
daisy.exe:	main${OBJ} daisy.dll
	$(LINK)$@ $^ $(CPPLIB) $(MATHLIB)

daisyw.exe:	$(GUIOBJECTS) daisy.dll
	$(LINK)$@ $^ $(GUILIB) $(CPPLIB) $(MATHLIB) -Wl,--enable-runtime-pseudo-reloc -mwindows

daisy:	main${OBJ} $(GUIOBJECTS) $(LIBOBJ)
	$(LINK)$@ $^ $(GUILIB) $(CPPLIB) $(MATHLIB)

exp:	
	(cd $(OBJHOME)/exp \
         && $(MAKE) VPATH=$(SRCDIR) USE_PROFILE=true -f $(SRCDIR)/Makefile daisy)

native:	
	(mkdir -p $(NATIVEHOME) \
	 && cd $(NATIVEHOME) \
         && time $(MAKE) VPATH=$(SRCDIR) -f $(SRCDIR)/Makefile $(NATIVEEXE))

cnative:
	(mkdir -p $(NATIVEHOME) \
	 && cd $(NATIVEHOME) \
         && $(MAKE) VPATH=$(SRCDIR) -f $(SRCDIR)/Makefile cdaisy.exe)

cross:
	(cd $(TARGETTYPE) \
         && $(MAKE) "PATH=/cygdrive/c/MinGW/bin:$(PATH)" \
		    "CYGHOME=C:/cygwin" Q4HOME=c:/Qt/4.3.0\
	            GCC=$(CROSSGCC) VPATH=$(SRCDIR) \
                    -f $(SRCDIR)/Makefile daisy${EXE} daisyw.exe)

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

# Various test targets.
#

UTESTSRC = ut_scope_exchange.C
UTESTOBJ = $(UTESTSRC:.C=${OBJ})

utest$(EXE): $(LIBOBJ) $(UTESTOBJ)
	$(CC) -o $@ $^ $(CPPLIB) $(MATHLIB) 

unittest:	
	(mkdir -p $(NATIVEHOME) \
	 && cd $(NATIVEHOME) \
         && $(MAKE) VPATH=$(SRCDIR) -f $(SRCDIR)/Makefile utest${EXE} \
	 && ./utest${EXE})

utest: utest.exe

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

txt/components.tex: $(DAISYEXE)
	(cd txt && $(DAISYEXE) -nw all.dai -p document )

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

dist:	svnci
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

# Update the SVN repository.
#
svnci: $(TEXT)
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
	(cd OpenMI; $(MAKE) svnci);
	(cd lib; $(MAKE) svnci);
	(cd sample; $(MAKE) svnci);
	(cd txt; $(MAKE) svnci);
	-svn add $(TEXT)
	rm -f $(REMOVE) 
	-svn remove $(REMOVE) 
	svn commit -m "Version $(TAG)"
	svn copy $(SVNROOT)/trunk \
	  $(SVNROOT)/tags/release_`echo $(TAG) | sed -e 's/[.]/_/g'` -m "New release"

.IGNORE: add

add:
	svn add $(TEXT)

update:
	svn update

commit:
	svn commit -m make

done:	update add commit


cast:
	fgrep _cast $(INTERFACES) $(MODELS) $(MAIN)
	wc -l  $(INTERFACES) $(MODELS) $(MAIN)

setup:	svnci
	$(MAKE) setupnosvn
	$(MAKE) upload

setupdocs:
	(cd txt && $(MAKE) PATH="$(PATH):$(Q4HOME)/bin" \
		           DAISYEXE=$(SRCDIR)/$(OBJHOME)/$(DAISYEXE) \
			   SETUPDIR=$(SETUPDIR) \
			   DAISYPATH=".;$(SRCDIR)/lib;$(SRCDIR)/sample" setup)

setupnosvn: 
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	$(MAKE) native 
	rm -rf $(SETUPDIR)
	mkdir $(SETUPDIR)
	cp ChangeLog NEWS $(SETUPDIR)
	mkdir $(SETUPDIR)/src
	cp $(TEXT) $(SETUPDIR)/src
	(cd lib && $(MAKE) SETUPDIR=$(SETUPDIR) TAG=$(TAG) setup)
	(cd sample && $(MAKE) SETUPDIR=$(SETUPDIR) TAG=$(TAG) setup)
	$(MAKE) setupdocs
	(cd exercises && $(MAKE) SETUPDIR=$(SETUPDIR) setup)
	mkdir $(SETUPDIR)/bin
	$(STRIP) -o $(SETUPDIR)/bin/daisy.exe $(OBJHOME)/daisy.exe
	$(STRIP) -o $(SETUPDIR)/bin/daisyw.exe $(OBJHOME)/daisyw.exe
	$(STRIP) -o $(SETUPDIR)/bin/daisy.dll $(OBJHOME)/daisy.dll
	cp $(Q4HOME)/bin/QtCore4.dll $(SETUPDIR)/bin
	cp $(Q4HOME)/bin/QtGui4.dll $(SETUPDIR)/bin
	cp $(MINGWHOME)/bin/mingwm10.dll $(SETUPDIR)/bin
	(cd OpenMI && $(MAKE) SETUPDIR=$(SETUPDIR) TAG=$(TAG) setup)
	$(MAKENSIS) /V2 /DVERSION=$(TAG) setup.nsi

upload:
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	./libdeps/googlecode_upload.py -p daisy-model \
		-s "Daisy version $(TAG) installer for MS Windows" \
		-l Type-Installer,OpSys-Windows,Featured \
		daisy-$(TAG)-setup.exe

daisysetup:
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
run_Qt${OBJ}: run_Qt.C run_Qt.h run.h model.h symbol.h alist.h toplevel.h \
  treelog_text.h treelog.h vis_Qt.h time.h program.h
vis_Qt${OBJ}: vis_Qt.C vis_Qt.h toplevel.h time.h log_Qt.h log_extern.h \
  log_select.h log.h border.h model.h symbol.h alist.h memutils.h \
  destination.h scope.h mathlib.h assertion.h
log_Qt${OBJ}: log_Qt.C log_Qt.h log_extern.h log_select.h log.h time.h \
  border.h model.h symbol.h alist.h memutils.h destination.h scope.h \
  librarian.h syntax.h treelog.h
ui_Qt_run${OBJ}: ui_Qt_run.C ui_Qt_run.h ui_Qt.h ui.h model.h symbol.h \
  alist.h vis_Qt.h toplevel.h time.h memutils.h log_Qt.h log_extern.h \
  log_select.h log.h border.h destination.h scope.h run_Qt.h run.h \
  treelog_text.h treelog.h program.h metalib.h library.h librarian.h \
  block.h syntax.h plf.h assertion.h path.h
ui_Qt${OBJ}: ui_Qt.C ui_Qt.h ui.h model.h symbol.h alist.h toplevel.h \
  librarian.h block.h syntax.h treelog.h plf.h assertion.h
main_Qt${OBJ}: main_Qt.C ui_Qt.h ui.h model.h symbol.h alist.h toplevel.h
mobsol${OBJ}: mobsol.C mobsol.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h librarian.h assertion.h
heatrect${OBJ}: heatrect.C heatrect.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h librarian.h
unit${OBJ}: unit.C unit.h model.h symbol.h alist.h check.h treelog.h \
  metalib.h library.h librarian.h syntax.h block.h plf.h
ABAprod${OBJ}: ABAprod.C ABAprod.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h librarian.h
solver${OBJ}: solver.C solver.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h librarian.h
element${OBJ}: element.C element.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h mathlib.h assertion.h librarian.h
ui${OBJ}: ui.C ui.h model.h symbol.h alist.h toplevel.h treelog_text.h \
  treelog.h librarian.h block.h syntax.h plf.h assertion.h
reaction${OBJ}: reaction.C reaction.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h librarian.h
scopesel${OBJ}: scopesel.C scopesel.h model.h symbol.h alist.h scope.h \
  assertion.h output.h condition.h memutils.h time.h block.h syntax.h \
  treelog.h plf.h librarian.h
scope${OBJ}: scope.C scope.h symbol.h model.h alist.h block.h syntax.h \
  treelog.h plf.h assertion.h librarian.h
ABAeffect${OBJ}: ABAeffect.C ABAeffect.h model.h symbol.h alist.h mathlib.h \
  assertion.h block.h syntax.h treelog.h plf.h librarian.h
msoltranrect${OBJ}: msoltranrect.C msoltranrect.h model.h symbol.h alist.h \
  chemical.h doe.h geometry_rect.h geometry_vert.h geometry.h syntax.h \
  treelog.h mathlib.h assertion.h adsorption.h block.h plf.h librarian.h
uzrect${OBJ}: uzrect.C uzrect.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h librarian.h
bound${OBJ}: bound.C bound.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h mathlib.h assertion.h librarian.h
volume${OBJ}: volume.C volume.h model.h symbol.h alist.h geometry.h syntax.h \
  treelog.h mathlib.h assertion.h block.h plf.h librarian.h
uz1d${OBJ}: uz1d.C uz1d.h model.h symbol.h alist.h geometry_rect.h \
  geometry_vert.h geometry.h syntax.h treelog.h mathlib.h assertion.h \
  soil.h soil_water.h soil_heat.h block.h plf.h librarian.h
rubiscoNdist${OBJ}: rubiscoNdist.C rubiscoNdist.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h mathlib.h assertion.h librarian.h
raddist${OBJ}: raddist.C raddist.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h mathlib.h assertion.h librarian.h
difrad${OBJ}: difrad.C difrad.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h librarian.h
organic_matter${OBJ}: organic_matter.C organic_matter.h model.h symbol.h \
  alist.h block.h syntax.h treelog.h plf.h librarian.h
movement${OBJ}: movement.C movement.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h librarian.h
integer${OBJ}: integer.C integer.h model.h symbol.h alist.h boolean.h \
  submodeler.h block.h syntax.h treelog.h plf.h assertion.h memutils.h \
  librarian.h
xysource${OBJ}: xysource.C xysource.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h assertion.h librarian.h
gnuplot${OBJ}: gnuplot.C gnuplot.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h librarian.h
boolean${OBJ}: boolean.C boolean.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h assertion.h memutils.h librarian.h
stringer${OBJ}: stringer.C stringer.h model.h symbol.h alist.h boolean.h \
  number.h submodeler.h block.h syntax.h treelog.h plf.h assertion.h \
  memutils.h librarian.h
source${OBJ}: source.C source.h model.h symbol.h alist.h time.h block.h \
  syntax.h treelog.h plf.h librarian.h
photo${OBJ}: photo.C photo.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h librarian.h
format${OBJ}: format.C format.h model.h symbol.h alist.h assertion.h block.h \
  syntax.h treelog.h plf.h librarian.h
depth${OBJ}: depth.C depth.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h time.h lexer_data.h lexer.h output.h condition.h \
  memutils.h number.h units.h check.h vcheck.h assertion.h librarian.h \
  mathlib.h path.h
wse${OBJ}: wse.C wse.h model.h symbol.h alist.h block.h syntax.h treelog.h \
  plf.h program.h run.h mathlib.h assertion.h librarian.h
program${OBJ}: program.C program.h model.h symbol.h alist.h run.h block.h \
  syntax.h treelog.h plf.h librarian.h
number${OBJ}: number.C number.h symbol.h model.h alist.h block.h syntax.h \
  treelog.h plf.h librarian.h units.h
domsorp${OBJ}: domsorp.C domsorp.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h librarian.h
chemistry${OBJ}: chemistry.C chemistry.h model.h symbol.h alist.h im.h \
  syntax.h treelog.h chemical.h block.h plf.h librarian.h vcheck.h
summary${OBJ}: summary.C summary.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h librarian.h
nitrification${OBJ}: nitrification.C nitrification.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h soil.h soil_water.h soil_heat.h log.h \
  time.h border.h mathlib.h assertion.h librarian.h
phenology${OBJ}: phenology.C phenology.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h log.h time.h border.h librarian.h
clayom${OBJ}: clayom.C clayom.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h librarian.h
equil${OBJ}: equil.C equil.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h librarian.h
transform${OBJ}: transform.C transform.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h librarian.h
rootdens${OBJ}: rootdens.C rootdens.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h check.h librarian.h
select${OBJ}: select.C select.h destination.h symbol.h model.h alist.h \
  units.h volume.h condition.h block.h syntax.h treelog.h plf.h \
  geometry.h mathlib.h assertion.h number.h scope_id.h scope.h metalib.h \
  library.h check.h vcheck.h format.h submodel.h submodeler.h librarian.h
average${OBJ}: average.C average.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h mathlib.h assertion.h librarian.h
mactrans${OBJ}: mactrans.C mactrans.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h librarian.h
macro${OBJ}: macro.C macro.h model.h symbol.h alist.h syntax.h treelog.h \
  block.h plf.h librarian.h
parser${OBJ}: parser.C parser.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h librarian.h
log${OBJ}: log.C log.h time.h border.h model.h symbol.h alist.h library.h \
  metalib.h block.h syntax.h treelog.h plf.h daisy.h program.h run.h \
  timestep.h vcheck.h memutils.h assertion.h librarian.h
weather${OBJ}: weather.C weather.h model.h symbol.h alist.h im.h syntax.h \
  treelog.h block.h plf.h fao.h time.h log.h border.h units.h mathlib.h \
  assertion.h librarian.h
column${OBJ}: column.C column.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h log.h time.h border.h librarian.h submodeler.h \
  assertion.h
crop${OBJ}: crop.C crop.h model.h symbol.h alist.h time.h om.h plf.h block.h \
  syntax.h treelog.h mathlib.h assertion.h librarian.h
action${OBJ}: action.C action.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h librarian.h
condition${OBJ}: condition.C condition.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h librarian.h
horizon${OBJ}: horizon.C horizon.h model.h symbol.h alist.h library.h block.h \
  syntax.h treelog.h plf.h horheat.h hydraulic.h mathlib.h assertion.h \
  tortuosity.h texture.h nitrification.h log.h time.h border.h \
  check_range.h check.h vcheck.h librarian.h mobsol.h
uzmodel${OBJ}: uzmodel.C uzmodel.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h librarian.h
hydraulic${OBJ}: hydraulic.C hydraulic.h model.h symbol.h alist.h syntax.h \
  treelog.h library.h block.h plf.h log.h time.h border.h check_range.h \
  check.h mathlib.h assertion.h program.h run.h vcheck.h librarian.h
bioclimate${OBJ}: bioclimate.C bioclimate.h model.h symbol.h alist.h \
  weather.h im.h syntax.h treelog.h block.h plf.h mathlib.h assertion.h \
  librarian.h
groundwater${OBJ}: groundwater.C groundwater.h model.h symbol.h alist.h \
  geometry.h syntax.h treelog.h mathlib.h assertion.h log.h time.h \
  border.h block.h plf.h librarian.h
am${OBJ}: am.C am.h model.h symbol.h alist.h im.h syntax.h treelog.h aom.h \
  om.h plf.h chemical.h metalib.h library.h submodeler.h block.h \
  assertion.h time.h log.h border.h geometry.h mathlib.h check.h vcheck.h \
  program.h run.h memutils.h librarian.h
transport${OBJ}: transport.C transport.h model.h symbol.h alist.h syntax.h \
  treelog.h block.h plf.h librarian.h
adsorption${OBJ}: adsorption.C adsorption.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h librarian.h mathlib.h assertion.h
tortuosity${OBJ}: tortuosity.C tortuosity.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h librarian.h
printer${OBJ}: printer.C printer.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h librarian.h
chemical${OBJ}: chemical.C chemical.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h librarian.h vcheck.h assertion.h
pet${OBJ}: pet.C pet.h model.h symbol.h alist.h syntax.h treelog.h block.h \
  plf.h log.h time.h border.h vegetation.h surface.h uzmodel.h \
  librarian.h
net_radiation${OBJ}: net_radiation.C net_radiation.h model.h symbol.h alist.h \
  log.h time.h border.h weather.h im.h syntax.h treelog.h block.h plf.h \
  mathlib.h assertion.h librarian.h
svat${OBJ}: svat.C svat.h model.h symbol.h alist.h log.h time.h border.h \
  block.h syntax.h treelog.h plf.h librarian.h
vegetation${OBJ}: vegetation.C vegetation.h model.h symbol.h alist.h log.h \
  time.h border.h syntax.h treelog.h block.h plf.h librarian.h
toplevel${OBJ}: toplevel.C toplevel.h metalib.h symbol.h daisy.h program.h \
  model.h alist.h run.h time.h timestep.h vcheck.h memutils.h ui.h \
  library.h parser_file.h parser.h submodel.h block.h syntax.h treelog.h \
  plf.h path.h version.h assertion.h treelog_text.h treelog_store.h \
  librarian.h
timestep${OBJ}: timestep.C timestep.h time.h vcheck.h symbol.h syntax.h \
  treelog.h alist.h block.h plf.h assertion.h mathlib.h
geometry_rect${OBJ}: geometry_rect.C geometry_rect.h geometry_vert.h \
  geometry.h syntax.h treelog.h symbol.h mathlib.h assertion.h volume.h \
  model.h alist.h check.h vcheck.h block.h plf.h submodel.h
doe${OBJ}: doe.C doe.h log.h time.h border.h model.h symbol.h alist.h \
  geometry.h syntax.h treelog.h mathlib.h assertion.h submodel.h soil.h \
  soil_water.h
geometry1d${OBJ}: geometry1d.C geometry1d.h geometry_vert.h geometry.h \
  syntax.h treelog.h symbol.h mathlib.h assertion.h volume.h model.h \
  alist.h block.h plf.h check.h vcheck.h submodel.h
fetch${OBJ}: fetch.C fetch.h destination.h symbol.h select.h model.h alist.h \
  units.h volume.h treelog.h syntax.h mathlib.h assertion.h
horheat${OBJ}: horheat.C horheat.h texture.h plf.h hydraulic.h model.h \
  symbol.h alist.h syntax.h treelog.h check.h mathlib.h assertion.h \
  submodel.h
litter${OBJ}: litter.C litter.h submodel.h syntax.h treelog.h symbol.h \
  alist.h check.h
time${OBJ}: time.C time.h assertion.h log.h border.h model.h symbol.h alist.h \
  syntax.h treelog.h vcheck.h submodel.h block.h plf.h
som${OBJ}: som.C som.h om.h plf.h submodel.h alist.h symbol.h
smb${OBJ}: smb.C smb.h om.h plf.h dom.h submodel.h syntax.h treelog.h \
  symbol.h alist.h assertion.h check.h mathlib.h
aom${OBJ}: aom.C aom.h om.h plf.h submodel.h alist.h symbol.h syntax.h \
  treelog.h check.h assertion.h smb.h dom.h log.h time.h border.h model.h \
  geometry.h mathlib.h
dom${OBJ}: dom.C dom.h plf.h doe.h smb.h om.h geometry.h syntax.h treelog.h \
  symbol.h mathlib.h assertion.h submodel.h block.h alist.h soil.h \
  soil_water.h log.h time.h border.h model.h check.h librarian.h
crpn${OBJ}: crpn.C crpn.h production.h symbol.h root_system.h rootdens.h \
  model.h alist.h ABAprod.h plf.h syntax.h treelog.h log.h time.h \
  border.h mathlib.h assertion.h submodel.h check.h
vernalization${OBJ}: vernalization.C vernalization.h submodel.h log.h time.h \
  border.h model.h symbol.h alist.h syntax.h treelog.h
partition${OBJ}: partition.C partition.h plf.h submodel.h syntax.h treelog.h \
  symbol.h alist.h check.h mathlib.h assertion.h
production${OBJ}: production.C production.h symbol.h crpn.h partition.h plf.h \
  organic_matter.h model.h alist.h geometry.h syntax.h treelog.h \
  mathlib.h assertion.h am.h im.h log.h time.h border.h submodel.h
harvesting${OBJ}: harvesting.C harvesting.h time.h plf.h symbol.h \
  production.h am.h model.h alist.h im.h syntax.h treelog.h aom.h om.h \
  crop.h harvest.h block.h geometry.h mathlib.h assertion.h log.h \
  border.h timestep.h vcheck.h submodel.h check_range.h check.h \
  submodeler.h
canopy_simple${OBJ}: canopy_simple.C canopy_simple.h plf.h submodel.h log.h \
  time.h border.h model.h symbol.h alist.h syntax.h treelog.h
canopy_std${OBJ}: canopy_std.C canopy_std.h canopy_simple.h plf.h submodel.h \
  log.h time.h border.h model.h symbol.h alist.h syntax.h treelog.h \
  mathlib.h assertion.h
root_system${OBJ}: root_system.C root_system.h rootdens.h model.h symbol.h \
  alist.h ABAprod.h plf.h submodel.h geometry.h syntax.h treelog.h \
  mathlib.h assertion.h soil_heat.h soil_water.h soil.h chemical.h \
  chemistry.h log.h time.h border.h check.h block.h librarian.h
ridge${OBJ}: ridge.C ridge.h soil.h symbol.h geometry1d.h geometry_vert.h \
  geometry.h syntax.h treelog.h mathlib.h assertion.h plf.h submodel.h \
  log.h time.h border.h model.h alist.h soil_water.h check.h
soil${OBJ}: soil.C soil.h symbol.h horizon.h model.h alist.h geometry.h \
  syntax.h treelog.h mathlib.h assertion.h hydraulic.h tortuosity.h \
  groundwater.h metalib.h library.h submodel.h submodeler.h block.h plf.h \
  log.h time.h border.h check.h vcheck.h memutils.h librarian.h volume.h
surface${OBJ}: surface.C surface.h uzmodel.h model.h symbol.h alist.h \
  syntax.h treelog.h geometry1d.h geometry_vert.h geometry.h mathlib.h \
  assertion.h soil.h soil_water.h log.h time.h border.h submodel.h plf.h \
  ridge.h check.h
soil_water${OBJ}: soil_water.C soil_water.h geometry.h syntax.h treelog.h \
  symbol.h mathlib.h assertion.h soil.h soil_heat.h groundwater.h model.h \
  alist.h log.h time.h border.h submodel.h block.h plf.h mobsol.h
soil_heat${OBJ}: soil_heat.C soil_heat.h block.h syntax.h treelog.h symbol.h \
  plf.h alist.h geometry.h mathlib.h assertion.h soil.h soil_water.h \
  surface.h uzmodel.h model.h movement.h weather.h im.h log.h time.h \
  border.h submodel.h
snow${OBJ}: snow.C snow.h alist.h symbol.h syntax.h treelog.h log.h time.h \
  border.h model.h geometry.h mathlib.h assertion.h soil.h soil_water.h \
  soil_heat.h movement.h submodel.h
im${OBJ}: im.C im.h symbol.h syntax.h treelog.h chemical.h model.h alist.h \
  units.h am.h log.h time.h border.h block.h plf.h check.h submodel.h \
  assertion.h
harvest${OBJ}: harvest.C harvest.h time.h symbol.h block.h syntax.h treelog.h \
  plf.h log.h border.h model.h alist.h submodel.h
field${OBJ}: field.C field.h border.h symbol.h column.h model.h alist.h log.h \
  time.h log_clone.h log_alist.h treelog.h library.h block.h syntax.h \
  plf.h memutils.h assertion.h librarian.h
bioincorporation${OBJ}: bioincorporation.C bioincorporation.h alist.h \
  symbol.h syntax.h treelog.h log.h time.h border.h model.h geometry.h \
  mathlib.h assertion.h soil.h am.h im.h submodel.h plf.h aom.h om.h \
  check.h vcheck.h
scope_exchange${OBJ}: scope_exchange.C scope_exchange.h model.h symbol.h \
  alist.h scope.h memutils.h block.h syntax.h treelog.h plf.h assertion.h \
  librarian.h
photo_Farquhar${OBJ}: photo_Farquhar.C photo_Farquhar.h photo.h model.h \
  symbol.h alist.h block.h syntax.h treelog.h plf.h rubiscoNdist.h \
  ABAeffect.h bioclimate.h canopy_std.h canopy_simple.h phenology.h log.h \
  time.h border.h submodel.h mathlib.h assertion.h check.h librarian.h
scope_multi${OBJ}: scope_multi.C scope_multi.h scope.h symbol.h model.h \
  alist.h syntax.h treelog.h assertion.h librarian.h
scope_id${OBJ}: scope_id.C scope_id.h scope.h symbol.h model.h alist.h \
  block.h syntax.h treelog.h plf.h assertion.h librarian.h
geometry_vert${OBJ}: geometry_vert.C geometry_vert.h geometry.h syntax.h \
  treelog.h symbol.h mathlib.h assertion.h block.h plf.h
gnuplot_base${OBJ}: gnuplot_base.C gnuplot_base.h gnuplot.h model.h symbol.h \
  alist.h vcheck.h block.h syntax.h treelog.h plf.h assertion.h \
  librarian.h
source_file${OBJ}: source_file.C source_file.h source.h model.h symbol.h \
  alist.h time.h lexer_table.h block.h syntax.h treelog.h plf.h \
  gnuplot_utils.h vcheck.h mathlib.h assertion.h
format_LaTeX${OBJ}: format_LaTeX.C format_LaTeX.h format.h model.h symbol.h \
  alist.h syntax.h treelog.h version.h assertion.h librarian.h
log_all${OBJ}: log_all.C log_all.h log_select.h log.h time.h border.h model.h \
  symbol.h alist.h memutils.h select.h destination.h units.h volume.h \
  metalib.h library.h block.h syntax.h treelog.h plf.h assertion.h
om${OBJ}: om.C om.h plf.h som.h smb.h dom.h syntax.h treelog.h symbol.h \
  alist.h check.h vcheck.h geometry.h mathlib.h assertion.h log.h time.h \
  border.h model.h
select_value${OBJ}: select_value.C select_value.h select.h destination.h \
  symbol.h model.h alist.h units.h volume.h block.h syntax.h treelog.h \
  plf.h mathlib.h assertion.h
weather_old${OBJ}: weather_old.C weather_old.h weather.h model.h symbol.h \
  alist.h im.h syntax.h treelog.h block.h plf.h fao.h time.h units.h
log_extern${OBJ}: log_extern.C log_extern.h log_select.h log.h time.h \
  border.h model.h symbol.h alist.h memutils.h destination.h scope.h \
  select.h units.h volume.h scope_block.h block.h syntax.h treelog.h \
  plf.h assertion.h librarian.h submodeler.h
log_select${OBJ}: log_select.C log_select.h log.h time.h border.h model.h \
  symbol.h alist.h memutils.h select.h destination.h units.h volume.h \
  condition.h metalib.h library.h block.h syntax.h treelog.h plf.h \
  field.h format.h scope.h assertion.h librarian.h
parser_file${OBJ}: parser_file.C parser_file.h parser.h model.h symbol.h \
  alist.h metalib.h library.h block.h syntax.h treelog.h plf.h lexer.h \
  submodel.h scope.h number.h integer.h time.h treelog_text.h path.h \
  units.h mathlib.h assertion.h memutils.h librarian.h
geometry${OBJ}: geometry.C geometry.h syntax.h treelog.h symbol.h mathlib.h \
  assertion.h volume.h model.h alist.h check.h vcheck.h
printer_file${OBJ}: printer_file.C printer_file.h printer.h model.h symbol.h \
  alist.h metalib.h library.h block.h syntax.h treelog.h plf.h time.h \
  parser.h path.h assertion.h librarian.h
log_alist${OBJ}: log_alist.C log_alist.h log.h time.h border.h model.h \
  symbol.h alist.h library.h syntax.h treelog.h assertion.h
bdconv${OBJ}: bdconv.C bdconv.h units.h symbol.h geometry.h syntax.h \
  treelog.h mathlib.h assertion.h soil.h volume.h model.h alist.h
abiotic${OBJ}: abiotic.C abiotic.h mathlib.h assertion.h
scope_soil${OBJ}: scope_soil.C scope_soil.h scope.h symbol.h model.h alist.h \
  soil.h soil_water.h soil_heat.h units.h syntax.h treelog.h assertion.h \
  librarian.h
run${OBJ}: run.C run.h model.h symbol.h alist.h
treelog_text${OBJ}: treelog_text.C treelog_text.h treelog.h symbol.h \
  assertion.h
treelog_store${OBJ}: treelog_store.C treelog_store.h treelog.h symbol.h \
  assertion.h memutils.h
intrinsics${OBJ}: intrinsics.C intrinsics.h symbol.h assertion.h library.h \
  memutils.h
metalib${OBJ}: metalib.C metalib.h symbol.h intrinsics.h librarian.h model.h \
  alist.h library.h block.h syntax.h treelog.h plf.h assertion.h \
  memutils.h path.h
model${OBJ}: model.C model.h symbol.h alist.h log.h time.h border.h
output${OBJ}: output.C output.h condition.h model.h symbol.h alist.h \
  memutils.h time.h daisy.h program.h run.h timestep.h vcheck.h log_all.h \
  log_select.h log.h border.h log_extern.h destination.h scope.h \
  treelog.h block.h syntax.h plf.h assertion.h librarian.h
scope_block${OBJ}: scope_block.C scope_block.h scope.h symbol.h model.h \
  alist.h block.h syntax.h treelog.h plf.h library.h number.h stringer.h \
  assertion.h librarian.h
librarian${OBJ}: librarian.C librarian.h model.h symbol.h alist.h library.h \
  metalib.h intrinsics.h block.h syntax.h treelog.h plf.h treelog_text.h \
  assertion.h
gnuplot_utils${OBJ}: gnuplot_utils.C gnuplot_utils.h syntax.h treelog.h \
  symbol.h alist.h
scope_sources${OBJ}: scope_sources.C scope_sources.h scope.h symbol.h model.h \
  alist.h time.h source.h treelog.h assertion.h memutils.h
scope_table${OBJ}: scope_table.C scope_table.h scope.h symbol.h model.h \
  alist.h lexer_table.h block.h syntax.h treelog.h plf.h assertion.h
lexer_table${OBJ}: lexer_table.C lexer_table.h block.h syntax.h treelog.h \
  symbol.h plf.h lexer_data.h lexer.h alist.h assertion.h mathlib.h \
  submodeler.h memutils.h time.h vcheck.h path.h
block${OBJ}: block.C block.h syntax.h treelog.h symbol.h plf.h metalib.h \
  library.h alist.h librarian.h model.h stringer.h number.h assertion.h \
  scope_block.h scope.h
dlf${OBJ}: dlf.C dlf.h symbol.h volume.h model.h alist.h assertion.h \
  version.h daisy.h program.h run.h time.h timestep.h vcheck.h memutils.h \
  toplevel.h
texture${OBJ}: texture.C texture.h plf.h assertion.h mathlib.h
destination${OBJ}: destination.C destination.h symbol.h
symbol${OBJ}: symbol.C symbol.h assertion.h
fao${OBJ}: fao.C fao.h alist.h symbol.h syntax.h treelog.h net_radiation.h \
  model.h assertion.h mathlib.h librarian.h
gaussj${OBJ}: gaussj.C gaussj.h mathlib.h assertion.h
vcheck${OBJ}: vcheck.C vcheck.h symbol.h units.h metalib.h library.h syntax.h \
  treelog.h alist.h time.h plf.h assertion.h mathlib.h
assertion${OBJ}: assertion.C assertion.h treelog.h symbol.h mathlib.h
xref${OBJ}: xref.C xref.h symbol.h traverse.h metalib.h library.h syntax.h \
  treelog.h alist.h submodel.h assertion.h
units${OBJ}: units.C units.h symbol.h syntax.h treelog.h mathlib.h \
  assertion.h memutils.h
check${OBJ}: check.C check.h mathlib.h assertion.h
check_range${OBJ}: check_range.C check_range.h check.h
path${OBJ}: path.C path.h assertion.h w32reg.h version.h
traverse_delete${OBJ}: traverse_delete.C traverse_delete.h symbol.h \
  traverse.h metalib.h library.h syntax.h treelog.h alist.h assertion.h
depend${OBJ}: depend.C depend.h symbol.h traverse.h library.h metalib.h \
  syntax.h treelog.h alist.h assertion.h
traverse${OBJ}: traverse.C traverse.h symbol.h metalib.h library.h syntax.h \
  treelog.h alist.h submodel.h assertion.h
treelog${OBJ}: treelog.C treelog.h symbol.h
lexer_data${OBJ}: lexer_data.C lexer_data.h lexer.h time.h mathlib.h \
  assertion.h
lexer${OBJ}: lexer.C lexer.h treelog.h symbol.h
daisy${OBJ}: daisy.C daisy.h program.h model.h symbol.h alist.h run.h time.h \
  timestep.h vcheck.h memutils.h weather.h im.h syntax.h treelog.h \
  groundwater.h horizon.h output.h condition.h log.h border.h parser.h \
  nitrification.h bioclimate.h hydraulic.h field.h harvest.h block.h \
  plf.h action.h library.h submodeler.h assertion.h column.h scope.h \
  scopesel.h mathlib.h librarian.h
alist${OBJ}: alist.C plf.h library.h symbol.h alist.h syntax.h treelog.h \
  time.h mathlib.h assertion.h memutils.h
syntax${OBJ}: syntax.C syntax.h treelog.h symbol.h alist.h library.h \
  metalib.h check.h vcheck.h assertion.h memutils.h
library${OBJ}: library.C library.h symbol.h block.h syntax.h treelog.h plf.h \
  alist.h assertion.h memutils.h
plf${OBJ}: plf.C plf.h assertion.h mathlib.h
mathlib${OBJ}: mathlib.C mathlib.h assertion.h
cdaisy${OBJ}: cdaisy.C scope.h symbol.h model.h alist.h block.h syntax.h \
  treelog.h plf.h metalib.h library.h daisy.h program.h run.h time.h \
  timestep.h vcheck.h memutils.h output.h condition.h toplevel.h \
  parser_file.h parser.h field.h border.h column.h weather.h im.h \
  action.h horizon.h printer_file.h printer.h version.h chemical.h \
  assertion.h
nrutil${OBJ}: nrutil.C
submodel${OBJ}: submodel.C submodel.h syntax.h treelog.h symbol.h alist.h \
  assertion.h
version${OBJ}: version.C
groundwater_aquitard${OBJ}: groundwater_aquitard.C groundwater.h model.h \
  symbol.h alist.h syntax.h treelog.h block.h plf.h librarian.h check.h \
  assertion.h depth.h geometry.h mathlib.h soil_water.h log.h time.h \
  border.h
heatrect_Mollerup${OBJ}: heatrect_Mollerup.C heatrect.h model.h symbol.h \
  alist.h solver.h geometry_rect.h geometry_vert.h geometry.h syntax.h \
  treelog.h mathlib.h assertion.h plf.h block.h librarian.h
heatrect_linear${OBJ}: heatrect_linear.C heatrect.h model.h symbol.h alist.h \
  geometry_rect.h geometry_vert.h geometry.h syntax.h treelog.h mathlib.h \
  assertion.h plf.h librarian.h
heatrect_none${OBJ}: heatrect_none.C heatrect.h model.h symbol.h alist.h \
  syntax.h treelog.h librarian.h
msoltranrect_convection${OBJ}: msoltranrect_convection.C msoltranrect.h \
  model.h symbol.h alist.h geometry_rect.h geometry_vert.h geometry.h \
  syntax.h treelog.h mathlib.h assertion.h soil.h soil_water.h \
  adsorption.h submodeler.h block.h plf.h memutils.h librarian.h
ABAprod_uptake${OBJ}: ABAprod_uptake.C ABAprod.h model.h symbol.h alist.h \
  number.h scope_id.h scope.h geometry.h syntax.h treelog.h mathlib.h \
  assertion.h soil_water.h units.h librarian.h
ABAprod_soil${OBJ}: ABAprod_soil.C ABAprod.h model.h symbol.h alist.h \
  number.h scope_exchange.h scope.h memutils.h geometry.h syntax.h \
  treelog.h mathlib.h assertion.h soil_water.h units.h librarian.h
ABAprod_root${OBJ}: ABAprod_root.C ABAprod.h model.h symbol.h alist.h \
  number.h scope_id.h scope.h geometry.h syntax.h treelog.h mathlib.h \
  assertion.h soil_water.h units.h librarian.h
solver_ublas${OBJ}: solver_ublas.C solver.h model.h symbol.h alist.h \
  assertion.h syntax.h treelog.h librarian.h
solver_cxsparse${OBJ}: solver_cxsparse.C solver.h model.h symbol.h alist.h \
  syntax.h treelog.h librarian.h ublas_cxsparse.h
solver_none${OBJ}: solver_none.C solver.h model.h symbol.h alist.h syntax.h \
  treelog.h librarian.h
movement_rect${OBJ}: movement_rect.C movement.h model.h symbol.h alist.h \
  geometry_rect.h geometry_vert.h geometry.h syntax.h treelog.h mathlib.h \
  assertion.h heatrect.h soil.h soil_water.h soil_heat.h msoltranrect.h \
  chemical.h groundwater.h surface.h uzmodel.h weather.h im.h uzrect.h \
  adsorption.h log.h time.h border.h check.h submodeler.h block.h plf.h \
  memutils.h librarian.h
chemistry_multi${OBJ}: chemistry_multi.C chemistry.h model.h symbol.h alist.h \
  chemical.h log.h time.h border.h block.h syntax.h treelog.h plf.h \
  assertion.h memutils.h librarian.h vcheck.h
equil_goal${OBJ}: equil_goal.C equil.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h scope_soil.h scope.h number.h check.h \
  mathlib.h assertion.h librarian.h
equil_linear${OBJ}: equil_linear.C equil.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h number.h check.h mathlib.h assertion.h \
  librarian.h
equil_langmuir${OBJ}: equil_langmuir.C equil.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h number.h soil.h check.h mathlib.h \
  assertion.h librarian.h
transform_equil${OBJ}: transform_equil.C transform.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h soil.h soil_water.h equil.h \
  scope_soil.h scope.h number.h units.h check.h mathlib.h assertion.h \
  librarian.h
reaction_nit${OBJ}: reaction_nit.C reaction.h model.h symbol.h alist.h \
  geometry.h syntax.h treelog.h mathlib.h assertion.h soil.h soil_water.h \
  soil_heat.h chemistry.h chemical.h organic_matter.h log.h time.h \
  border.h librarian.h
reaction_denit${OBJ}: reaction_denit.C reaction.h model.h symbol.h alist.h \
  abiotic.h librarian.h block.h syntax.h treelog.h plf.h geometry.h \
  mathlib.h assertion.h soil.h soil_water.h soil_heat.h organic_matter.h \
  chemistry.h chemical.h log.h time.h border.h check.h
reaction_adsorption${OBJ}: reaction_adsorption.C reaction.h model.h symbol.h \
  alist.h block.h syntax.h treelog.h plf.h number.h adsorption.h \
  chemistry.h chemical.h geometry.h mathlib.h assertion.h soil.h \
  soil_water.h scope_soil.h scope.h units.h log.h time.h border.h \
  librarian.h
reaction_equil${OBJ}: reaction_equil.C reaction.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h number.h equil.h chemistry.h \
  chemical.h geometry.h mathlib.h assertion.h soil.h scope_soil.h scope.h \
  log.h time.h border.h librarian.h
rootdens_GP2D${OBJ}: rootdens_GP2D.C rootdens.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h geometry.h mathlib.h assertion.h log.h \
  time.h border.h check.h librarian.h iterative.h
rootdens_GP1D${OBJ}: rootdens_GP1D.C rootdens.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h geometry.h mathlib.h assertion.h log.h \
  time.h border.h check.h librarian.h iterative.h
number_plf${OBJ}: number_plf.C number.h symbol.h model.h alist.h syntax.h \
  treelog.h plf.h units.h memutils.h block.h librarian.h submodeler.h \
  assertion.h
rubiscoNdist_forced${OBJ}: rubiscoNdist_forced.C rubiscoNdist.h model.h \
  symbol.h alist.h mathlib.h assertion.h block.h syntax.h treelog.h plf.h \
  check.h librarian.h number.h scope_exchange.h scope.h memutils.h
action_extern${OBJ}: action_extern.C action.h model.h symbol.h alist.h \
  scope_multi.h scope.h scopesel.h number.h daisy.h program.h run.h \
  time.h timestep.h vcheck.h memutils.h field.h border.h am.h im.h \
  syntax.h treelog.h chemical.h log.h librarian.h block.h plf.h check.h \
  assertion.h units.h
rubiscoNdist_expr${OBJ}: rubiscoNdist_expr.C rubiscoNdist.h model.h symbol.h \
  alist.h mathlib.h assertion.h block.h syntax.h treelog.h plf.h check.h \
  librarian.h number.h scope_exchange.h scope.h memutils.h
uzrect_const${OBJ}: uzrect_const.C uzrect.h model.h symbol.h alist.h \
  geometry_rect.h geometry_vert.h geometry.h syntax.h treelog.h mathlib.h \
  assertion.h soil_water.h block.h plf.h librarian.h
photo_FCC3${OBJ}: photo_FCC3.C photo_Farquhar.h photo.h model.h symbol.h \
  alist.h block.h syntax.h treelog.h plf.h rubiscoNdist.h ABAeffect.h \
  bioclimate.h canopy_std.h canopy_simple.h phenology.h log.h time.h \
  border.h submodel.h mathlib.h assertion.h check.h librarian.h
photo_FCC4${OBJ}: photo_FCC4.C photo_Farquhar.h photo.h model.h symbol.h \
  alist.h rubiscoNdist.h ABAeffect.h bioclimate.h canopy_std.h \
  canopy_simple.h plf.h phenology.h log.h time.h border.h syntax.h \
  treelog.h block.h submodel.h mathlib.h assertion.h check.h librarian.h
msoltranrect_Mollerup${OBJ}: msoltranrect_Mollerup.C msoltranrect.h model.h \
  symbol.h alist.h geometry_rect.h geometry_vert.h geometry.h syntax.h \
  treelog.h mathlib.h assertion.h soil.h soil_water.h adsorption.h \
  solver.h log.h time.h border.h submodeler.h block.h plf.h memutils.h \
  librarian.h
reaction_std${OBJ}: reaction_std.C reaction.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h transform.h chemistry.h chemical.h \
  soil.h log.h time.h border.h assertion.h librarian.h
chemistry_std${OBJ}: chemistry_std.C chemistry.h model.h symbol.h alist.h \
  chemical.h reaction.h movement.h geometry.h syntax.h treelog.h \
  mathlib.h assertion.h soil.h soil_water.h block.h plf.h log.h time.h \
  border.h memutils.h librarian.h vcheck.h
groundwater_extern${OBJ}: groundwater_extern.C groundwater.h model.h symbol.h \
  alist.h output.h condition.h memutils.h time.h number.h block.h \
  syntax.h treelog.h plf.h units.h check.h assertion.h librarian.h
msoltranrect_none${OBJ}: msoltranrect_none.C msoltranrect.h model.h symbol.h \
  alist.h geometry_rect.h geometry_vert.h geometry.h syntax.h treelog.h \
  mathlib.h assertion.h soil.h soil_water.h adsorption.h submodeler.h \
  block.h plf.h memutils.h librarian.h
uzrect_Mollerup${OBJ}: uzrect_Mollerup.C uzrect.h model.h symbol.h alist.h \
  geometry_rect.h geometry_vert.h geometry.h syntax.h treelog.h mathlib.h \
  assertion.h soil.h soil_water.h soil_heat.h groundwater.h surface.h \
  uzmodel.h solver.h log.h time.h border.h block.h plf.h librarian.h
groundwater_flux${OBJ}: groundwater_flux.C groundwater.h model.h symbol.h \
  alist.h syntax.h treelog.h block.h plf.h check.h librarian.h
msoltranrect_2x1${OBJ}: msoltranrect_2x1.C msoltranrect.h model.h symbol.h \
  alist.h geometry_rect.h geometry_vert.h geometry.h syntax.h treelog.h \
  mathlib.h assertion.h transport.h soil.h soil_water.h adsorption.h \
  submodeler.h block.h plf.h memutils.h librarian.h
ABAeffect_exp${OBJ}: ABAeffect_exp.C ABAeffect.h model.h symbol.h alist.h \
  mathlib.h assertion.h check.h block.h syntax.h treelog.h plf.h \
  librarian.h
rubiscoNdist_uniform${OBJ}: rubiscoNdist_uniform.C rubiscoNdist.h model.h \
  symbol.h alist.h mathlib.h assertion.h check.h block.h syntax.h \
  treelog.h plf.h librarian.h
uzrect_2x1${OBJ}: uzrect_2x1.C uzrect.h model.h symbol.h alist.h uzmodel.h \
  uz1d.h geometry_rect.h geometry_vert.h geometry.h syntax.h treelog.h \
  mathlib.h assertion.h soil.h soil_water.h soil_heat.h groundwater.h \
  surface.h memutils.h librarian.h
select_flow${OBJ}: select_flow.C select_value.h select.h destination.h \
  symbol.h model.h alist.h units.h volume.h block.h syntax.h treelog.h \
  plf.h border.h geometry.h mathlib.h assertion.h librarian.h
volume_box${OBJ}: volume_box.C volume.h model.h symbol.h alist.h syntax.h \
  treelog.h bound.h border.h mathlib.h assertion.h librarian.h
select_volume${OBJ}: select_volume.C select_value.h select.h destination.h \
  symbol.h model.h alist.h units.h volume.h bdconv.h block.h syntax.h \
  treelog.h plf.h geometry.h mathlib.h assertion.h soil.h vegetation.h \
  check.h librarian.h
uz1d_none${OBJ}: uz1d_none.C uz1d.h model.h symbol.h alist.h geometry_rect.h \
  geometry_vert.h geometry.h syntax.h treelog.h mathlib.h assertion.h \
  soil.h soil_water.h soil_heat.h librarian.h
condition_walltime${OBJ}: condition_walltime.C condition.h model.h symbol.h \
  alist.h block.h syntax.h treelog.h plf.h librarian.h
uz1d_richard${OBJ}: uz1d_richard.C uz1d.h model.h symbol.h alist.h \
  geometry_rect.h geometry_vert.h geometry.h syntax.h treelog.h mathlib.h \
  assertion.h soil.h soil_water.h soil_heat.h block.h plf.h average.h \
  librarian.h
rubiscoNdist_DPF${OBJ}: rubiscoNdist_DPF.C rubiscoNdist.h model.h symbol.h \
  alist.h mathlib.h assertion.h block.h syntax.h treelog.h plf.h check.h \
  librarian.h
raddist_DPF${OBJ}: raddist_DPF.C raddist.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h vegetation.h weather.h im.h mathlib.h \
  assertion.h check.h librarian.h
raddist_std${OBJ}: raddist_std.C raddist.h model.h symbol.h alist.h syntax.h \
  treelog.h vegetation.h mathlib.h assertion.h librarian.h
difrad_DPF${OBJ}: difrad_DPF.C difrad.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h weather.h im.h fao.h mathlib.h assertion.h \
  check.h librarian.h
difrad_weather${OBJ}: difrad_weather.C difrad.h model.h symbol.h alist.h \
  syntax.h treelog.h weather.h im.h mathlib.h assertion.h librarian.h
number_lisp${OBJ}: number_lisp.C number.h symbol.h model.h alist.h \
  scope_multi.h scope.h submodeler.h block.h syntax.h treelog.h plf.h \
  assertion.h memutils.h librarian.h
condition_extern${OBJ}: condition_extern.C condition.h model.h symbol.h \
  alist.h daisy.h program.h run.h time.h timestep.h vcheck.h memutils.h \
  block.h syntax.h treelog.h plf.h boolean.h output.h scope_multi.h \
  scope.h scopesel.h librarian.h assertion.h
condition_boolean${OBJ}: condition_boolean.C condition.h model.h symbol.h \
  alist.h syntax.h treelog.h boolean.h scope.h librarian.h assertion.h
boolean_number${OBJ}: boolean_number.C boolean.h model.h symbol.h alist.h \
  syntax.h treelog.h number.h memutils.h librarian.h
boolean_string${OBJ}: boolean_string.C boolean.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h librarian.h
number_soil${OBJ}: number_soil.C number.h symbol.h model.h alist.h metalib.h \
  library.h block.h syntax.h treelog.h plf.h column.h horizon.h \
  hydraulic.h weather.h im.h output.h condition.h memutils.h time.h \
  units.h librarian.h scope.h
organic_none${OBJ}: organic_none.C organic_matter.h model.h symbol.h alist.h \
  geometry.h syntax.h treelog.h mathlib.h assertion.h librarian.h
organic_std${OBJ}: organic_std.C organic_matter.h model.h symbol.h alist.h \
  syntax.h treelog.h submodeler.h block.h plf.h assertion.h log.h time.h \
  border.h am.h im.h om.h som.h smb.h dom.h domsorp.h aom.h clayom.h \
  soil.h geometry.h mathlib.h soil_water.h soil_heat.h chemistry.h \
  chemical.h bioincorporation.h abiotic.h check_range.h check.h vcheck.h \
  gaussj.h memutils.h librarian.h
movement_1D${OBJ}: movement_1D.C movement.h model.h symbol.h alist.h \
  geometry1d.h geometry_vert.h geometry.h syntax.h treelog.h mathlib.h \
  assertion.h soil.h soil_water.h soil_heat.h macro.h groundwater.h \
  surface.h uzmodel.h weather.h im.h chemical.h doe.h transport.h \
  mactrans.h adsorption.h log.h time.h border.h submodeler.h block.h \
  plf.h memutils.h librarian.h
integer_arit${OBJ}: integer_arit.C integer.h model.h symbol.h alist.h \
  syntax.h treelog.h vcheck.h assertion.h memutils.h librarian.h
source_merge${OBJ}: source_merge.C source.h model.h symbol.h alist.h time.h \
  block.h syntax.h treelog.h plf.h gnuplot_utils.h units.h vcheck.h \
  mathlib.h assertion.h memutils.h librarian.h
number_source${OBJ}: number_source.C number.h symbol.h model.h alist.h \
  block.h syntax.h treelog.h plf.h source.h time.h assertion.h \
  librarian.h
program_file${OBJ}: program_file.C program.h model.h symbol.h alist.h run.h \
  block.h syntax.h treelog.h plf.h path.h librarian.h
action_table${OBJ}: action_table.C action.h model.h symbol.h alist.h \
  metalib.h library.h daisy.h program.h run.h time.h timestep.h vcheck.h \
  memutils.h field.h border.h am.h im.h syntax.h treelog.h units.h \
  lexer_table.h block.h plf.h mathlib.h assertion.h librarian.h
xysource_merge${OBJ}: xysource_merge.C xysource.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h gnuplot_utils.h number.h \
  scope_sources.h scope.h time.h units.h vcheck.h memutils.h librarian.h
xysource_inline${OBJ}: xysource_inline.C xysource.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h gnuplot_utils.h number.h vcheck.h \
  assertion.h librarian.h
xysource_loop${OBJ}: xysource_loop.C xysource.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h gnuplot_utils.h scope_id.h scope.h \
  number.h check.h vcheck.h assertion.h librarian.h
xysource_combine${OBJ}: xysource_combine.C xysource.h model.h symbol.h \
  alist.h block.h syntax.h treelog.h plf.h gnuplot_utils.h number.h \
  scope_sources.h scope.h time.h source.h assertion.h librarian.h
gnuplot_xy${OBJ}: gnuplot_xy.C gnuplot_base.h gnuplot.h model.h symbol.h \
  alist.h block.h syntax.h treelog.h plf.h xysource.h mathlib.h \
  assertion.h memutils.h librarian.h
xysource_expr${OBJ}: xysource_expr.C xysource.h model.h symbol.h alist.h \
  lexer_table.h block.h syntax.h treelog.h plf.h scope_table.h scope.h \
  gnuplot_utils.h number.h vcheck.h assertion.h librarian.h
gnuplot_multi${OBJ}: gnuplot_multi.C gnuplot.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h source.h time.h memutils.h librarian.h
gnuplot_time${OBJ}: gnuplot_time.C gnuplot_base.h gnuplot.h model.h symbol.h \
  alist.h block.h syntax.h treelog.h plf.h source.h time.h mathlib.h \
  assertion.h memutils.h librarian.h
source_combine${OBJ}: source_combine.C source.h model.h symbol.h alist.h \
  time.h block.h syntax.h treelog.h plf.h number.h scope_sources.h \
  scope.h gnuplot_utils.h vcheck.h assertion.h librarian.h
number_arit${OBJ}: number_arit.C number.h symbol.h model.h alist.h syntax.h \
  treelog.h units.h vcheck.h mathlib.h assertion.h memutils.h block.h \
  plf.h librarian.h submodeler.h
source_expr${OBJ}: source_expr.C source_file.h source.h model.h symbol.h \
  alist.h time.h lexer_table.h block.h syntax.h treelog.h plf.h \
  scope_table.h scope.h number.h librarian.h
source_std${OBJ}: source_std.C source_file.h source.h model.h symbol.h \
  alist.h time.h lexer_table.h block.h syntax.h treelog.h plf.h units.h \
  librarian.h
action_markvand${OBJ}: action_markvand.C action.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h daisy.h program.h run.h time.h \
  timestep.h vcheck.h memutils.h field.h border.h crop.h im.h fao.h log.h \
  mathlib.h assertion.h check.h librarian.h vegetation.h
photo_GL${OBJ}: photo_GL.C photo.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h canopy_std.h canopy_simple.h phenology.h submodel.h \
  mathlib.h assertion.h check.h librarian.h
program_gnuplot${OBJ}: program_gnuplot.C program.h model.h symbol.h alist.h \
  run.h block.h syntax.h treelog.h plf.h gnuplot.h path.h memutils.h \
  librarian.h
program_document${OBJ}: program_document.C program.h model.h symbol.h alist.h \
  run.h library.h metalib.h block.h syntax.h treelog.h plf.h submodel.h \
  printer_file.h printer.h xref.h format.h assertion.h librarian.h
program_batch${OBJ}: program_batch.C program.h model.h symbol.h alist.h run.h \
  block.h syntax.h treelog.h plf.h path.h assertion.h memutils.h \
  librarian.h
summary_balance${OBJ}: summary_balance.C summary.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h fetch.h destination.h select.h units.h \
  volume.h memutils.h librarian.h
rootdens_AP${OBJ}: rootdens_AP.C rootdens.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h geometry.h mathlib.h assertion.h log.h time.h \
  border.h check.h librarian.h
number_const${OBJ}: number_const.C number.h symbol.h model.h alist.h block.h \
  syntax.h treelog.h plf.h scope.h units.h assertion.h librarian.h
domsorp_std${OBJ}: domsorp_std.C domsorp.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h transform.h dom.h som.h om.h soil.h log.h \
  time.h border.h assertion.h librarian.h
horizon_numeric${OBJ}: horizon_numeric.C horizon.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h texture.h hydraulic.h check.h vcheck.h \
  mathlib.h assertion.h librarian.h
horizon_system${OBJ}: horizon_system.C horizon.h model.h symbol.h alist.h \
  library.h block.h syntax.h treelog.h plf.h texture.h hydraulic.h \
  check.h mathlib.h assertion.h librarian.h
select_pF${OBJ}: select_pF.C select.h destination.h symbol.h model.h alist.h \
  units.h volume.h block.h syntax.h treelog.h plf.h mathlib.h assertion.h \
  check.h vcheck.h librarian.h
pet_FAO_PM${OBJ}: pet_FAO_PM.C pet.h model.h symbol.h alist.h syntax.h \
  treelog.h fao.h weather.h im.h soil.h surface.h uzmodel.h soil_heat.h \
  vegetation.h log.h time.h border.h librarian.h
pet_Hargreaves${OBJ}: pet_Hargreaves.C pet.h model.h symbol.h alist.h \
  syntax.h treelog.h weather.h im.h fao.h log.h time.h border.h mathlib.h \
  assertion.h librarian.h
hydraulic_M_vGp${OBJ}: hydraulic_M_vGp.C hydraulic.h model.h symbol.h alist.h \
  syntax.h treelog.h block.h plf.h mathlib.h assertion.h check.h \
  librarian.h
summary_simple${OBJ}: summary_simple.C summary.h model.h symbol.h alist.h \
  fetch.h destination.h select.h units.h volume.h treelog.h memutils.h \
  submodeler.h block.h syntax.h plf.h assertion.h librarian.h
phenology_TSum${OBJ}: phenology_TSum.C phenology.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h production.h vernalization.h \
  assertion.h librarian.h
phenology_std${OBJ}: phenology_std.C phenology.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h production.h vernalization.h mathlib.h \
  assertion.h librarian.h
hydraulic_hypres${OBJ}: hydraulic_hypres.C hydraulic.h model.h symbol.h \
  alist.h syntax.h treelog.h block.h plf.h texture.h mathlib.h \
  assertion.h librarian.h
clayom_biomod${OBJ}: clayom_biomod.C clayom.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h check.h smb.h om.h soil.h mathlib.h \
  assertion.h librarian.h
clayom_old${OBJ}: clayom_old.C clayom.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h smb.h om.h soil.h assertion.h librarian.h
hydraulic_Cosby${OBJ}: hydraulic_Cosby.C hydraulic.h model.h symbol.h alist.h \
  syntax.h treelog.h texture.h plf.h mathlib.h assertion.h librarian.h
condition_weather${OBJ}: condition_weather.C condition.h model.h symbol.h \
  alist.h block.h syntax.h treelog.h plf.h field.h border.h daisy.h \
  program.h run.h time.h timestep.h vcheck.h memutils.h check.h log.h \
  librarian.h
rootdens_PLF${OBJ}: rootdens_PLF.C rootdens.h model.h symbol.h alist.h \
  geometry.h syntax.h treelog.h mathlib.h assertion.h plf.h submodeler.h \
  block.h check.h vcheck.h memutils.h librarian.h
rootdens_G_P${OBJ}: rootdens_G_P.C rootdens.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h geometry.h mathlib.h assertion.h log.h \
  time.h border.h check.h librarian.h
groundwater_file${OBJ}: groundwater_file.C groundwater.h model.h symbol.h \
  alist.h block.h syntax.h treelog.h plf.h lexer_data.h lexer.h \
  assertion.h time.h librarian.h path.h
action_fertilize${OBJ}: action_fertilize.C action.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h daisy.h program.h run.h time.h \
  timestep.h vcheck.h memutils.h field.h border.h am.h im.h check.h \
  assertion.h librarian.h volume.h
action_repeat${OBJ}: action_repeat.C action.h model.h symbol.h alist.h \
  daisy.h program.h run.h time.h timestep.h vcheck.h memutils.h block.h \
  syntax.h treelog.h plf.h log.h border.h librarian.h
vegetation_permanent${OBJ}: vegetation_permanent.C vegetation.h model.h \
  symbol.h alist.h plf.h mathlib.h assertion.h log.h time.h border.h \
  litter.h root_system.h rootdens.h ABAprod.h canopy_simple.h geometry.h \
  syntax.h treelog.h soil.h crop.h am.h im.h aom.h om.h organic_matter.h \
  submodeler.h block.h check.h librarian.h
vegetation_crops${OBJ}: vegetation_crops.C vegetation.h model.h symbol.h \
  alist.h crop.h time.h organic_matter.h geometry.h syntax.h treelog.h \
  mathlib.h assertion.h soil.h plf.h harvest.h block.h log.h border.h \
  librarian.h
crop_simple${OBJ}: crop_simple.C crop.h model.h symbol.h alist.h time.h \
  root_system.h rootdens.h ABAprod.h plf.h canopy_simple.h log.h border.h \
  bioclimate.h soil_water.h geometry.h syntax.h treelog.h mathlib.h \
  assertion.h soil.h aom.h om.h organic_matter.h soil_heat.h am.h im.h \
  harvest.h block.h submodeler.h check.h librarian.h
action_ridge${OBJ}: action_ridge.C action.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h daisy.h program.h run.h time.h timestep.h \
  vcheck.h memutils.h field.h border.h ridge.h librarian.h
groundwater_fixed${OBJ}: groundwater_fixed.C groundwater.h model.h symbol.h \
  alist.h block.h syntax.h treelog.h plf.h check.h assertion.h \
  librarian.h
groundwater_deep${OBJ}: groundwater_deep.C groundwater.h model.h symbol.h \
  alist.h syntax.h treelog.h assertion.h librarian.h
action_heat${OBJ}: action_heat.C action.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h daisy.h program.h run.h time.h timestep.h \
  vcheck.h memutils.h field.h border.h check.h librarian.h
hydraulic_M_vG_compact${OBJ}: hydraulic_M_vG_compact.C hydraulic.h model.h \
  symbol.h alist.h syntax.h treelog.h block.h plf.h mathlib.h assertion.h \
  librarian.h
action_crop${OBJ}: action_crop.C action.h model.h symbol.h alist.h daisy.h \
  program.h run.h time.h timestep.h vcheck.h memutils.h field.h border.h \
  crop.h am.h im.h syntax.h treelog.h log.h harvest.h block.h plf.h \
  check_range.h check.h submodeler.h assertion.h mathlib.h librarian.h \
  vegetation.h
groundwater_lysimeter${OBJ}: groundwater_lysimeter.C groundwater.h model.h \
  symbol.h alist.h geometry.h syntax.h treelog.h mathlib.h assertion.h \
  librarian.h
action_message${OBJ}: action_message.C action.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h condition.h log.h time.h border.h \
  daisy.h program.h run.h timestep.h vcheck.h memutils.h librarian.h
weather_std${OBJ}: weather_std.C weather.h model.h symbol.h alist.h im.h \
  syntax.h treelog.h chemical.h fao.h lexer_data.h lexer.h time.h plf.h \
  mathlib.h assertion.h units.h submodeler.h block.h check.h vcheck.h \
  memutils.h librarian.h path.h
groundwater_pipe${OBJ}: groundwater_pipe.C groundwater.h model.h symbol.h \
  alist.h block.h syntax.h treelog.h plf.h log.h time.h border.h \
  geometry.h mathlib.h assertion.h soil.h soil_heat.h soil_water.h \
  depth.h check.h librarian.h
select_index${OBJ}: select_index.C select_value.h select.h destination.h \
  symbol.h model.h alist.h units.h volume.h block.h syntax.h treelog.h \
  plf.h librarian.h
select_content${OBJ}: select_content.C select_value.h select.h destination.h \
  symbol.h model.h alist.h units.h volume.h block.h syntax.h treelog.h \
  plf.h geometry.h mathlib.h assertion.h soil.h check.h librarian.h
select_number${OBJ}: select_number.C select_value.h select.h destination.h \
  symbol.h model.h alist.h units.h volume.h syntax.h treelog.h \
  librarian.h
select_array${OBJ}: select_array.C select.h destination.h symbol.h model.h \
  alist.h units.h volume.h soil.h bdconv.h block.h syntax.h treelog.h \
  plf.h mathlib.h assertion.h librarian.h
log_table${OBJ}: log_table.C log_select.h log.h time.h border.h model.h \
  symbol.h alist.h memutils.h library.h block.h syntax.h treelog.h plf.h \
  select.h destination.h units.h volume.h summary.h geometry.h mathlib.h \
  assertion.h dlf.h daisy.h program.h run.h timestep.h vcheck.h \
  librarian.h scope_block.h scope.h
log_harvest${OBJ}: log_harvest.C log.h time.h border.h model.h symbol.h \
  alist.h daisy.h program.h run.h timestep.h vcheck.h memutils.h \
  harvest.h block.h syntax.h treelog.h plf.h dlf.h version.h assertion.h \
  librarian.h
action_while${OBJ}: action_while.C action.h model.h symbol.h alist.h syntax.h \
  treelog.h log.h time.h border.h assertion.h memutils.h librarian.h
action_wait${OBJ}: action_wait.C action.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h condition.h log.h time.h border.h daisy.h \
  program.h run.h timestep.h vcheck.h memutils.h assertion.h librarian.h
action_activity${OBJ}: action_activity.C action.h model.h symbol.h alist.h \
  syntax.h treelog.h log.h time.h border.h memutils.h librarian.h
mactrans_std${OBJ}: mactrans_std.C mactrans.h model.h symbol.h alist.h \
  soil_water.h geometry1d.h geometry_vert.h geometry.h syntax.h treelog.h \
  mathlib.h assertion.h plf.h librarian.h
macro_std${OBJ}: macro_std.C macro.h model.h symbol.h alist.h syntax.h \
  treelog.h block.h plf.h geometry1d.h geometry_vert.h geometry.h \
  mathlib.h assertion.h soil.h surface.h uzmodel.h log.h time.h border.h \
  check.h vcheck.h librarian.h
macro_none${OBJ}: macro_none.C macro.h model.h symbol.h alist.h syntax.h \
  treelog.h librarian.h
column_std${OBJ}: column_std.C column.h model.h symbol.h alist.h library.h \
  surface.h uzmodel.h soil_heat.h macro.h syntax.h treelog.h movement.h \
  groundwater.h geometry.h mathlib.h assertion.h soil.h soil_water.h \
  vegetation.h bioclimate.h weather.h im.h chemistry.h chemical.h \
  organic_matter.h am.h dom.h plf.h time.h units.h log.h border.h \
  submodeler.h block.h memutils.h librarian.h scope_multi.h scope.h \
  scopesel.h
weather_simple${OBJ}: weather_simple.C weather_old.h weather.h model.h \
  symbol.h alist.h im.h syntax.h treelog.h block.h plf.h time.h log.h \
  border.h mathlib.h assertion.h librarian.h
uzrichard${OBJ}: uzrichard.C uzmodel.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h groundwater.h surface.h geometry_vert.h \
  geometry.h mathlib.h assertion.h soil.h soil_heat.h log.h time.h \
  border.h average.h librarian.h
hydraulic_yolo${OBJ}: hydraulic_yolo.C hydraulic.h model.h symbol.h alist.h \
  syntax.h treelog.h block.h plf.h mathlib.h assertion.h librarian.h
hydraulic_M_vG${OBJ}: hydraulic_M_vG.C hydraulic.h model.h symbol.h alist.h \
  syntax.h treelog.h block.h plf.h mathlib.h assertion.h librarian.h
hydraulic_B_vG${OBJ}: hydraulic_B_vG.C hydraulic.h model.h symbol.h alist.h \
  syntax.h treelog.h block.h plf.h mathlib.h assertion.h librarian.h
hydraulic_M_C${OBJ}: hydraulic_M_C.C hydraulic.h model.h symbol.h alist.h \
  syntax.h treelog.h block.h plf.h check.h mathlib.h assertion.h \
  librarian.h
hydraulic_B_C${OBJ}: hydraulic_B_C.C hydraulic.h model.h symbol.h alist.h \
  syntax.h treelog.h block.h plf.h check.h mathlib.h assertion.h \
  librarian.h
hydraulic_M_BaC${OBJ}: hydraulic_M_BaC.C hydraulic.h model.h symbol.h alist.h \
  syntax.h treelog.h block.h plf.h check.h mathlib.h assertion.h \
  librarian.h
hydraulic_B_BaC${OBJ}: hydraulic_B_BaC.C hydraulic.h model.h symbol.h alist.h \
  syntax.h treelog.h block.h plf.h mathlib.h assertion.h librarian.h
groundwater_static${OBJ}: groundwater_static.C groundwater.h model.h symbol.h \
  alist.h block.h syntax.h treelog.h plf.h assertion.h librarian.h
horizon_std${OBJ}: horizon_std.C horizon.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h texture.h hydraulic.h check.h mathlib.h \
  assertion.h librarian.h
crop_std${OBJ}: crop_std.C crop.h model.h symbol.h alist.h time.h chemistry.h \
  root_system.h rootdens.h ABAprod.h plf.h canopy_std.h canopy_simple.h \
  harvesting.h production.h phenology.h partition.h vernalization.h \
  photo.h crpn.h wse.h log.h border.h timestep.h vcheck.h bioclimate.h \
  soil_water.h geometry.h syntax.h treelog.h mathlib.h assertion.h soil.h \
  organic_matter.h soil_heat.h am.h im.h submodeler.h block.h librarian.h \
  memutils.h
action_sow${OBJ}: action_sow.C action.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h daisy.h program.h run.h time.h timestep.h \
  vcheck.h memutils.h field.h border.h crop.h librarian.h check.h
action_stop${OBJ}: action_stop.C action.h model.h symbol.h alist.h syntax.h \
  treelog.h daisy.h program.h run.h time.h timestep.h vcheck.h memutils.h \
  librarian.h
condition_time${OBJ}: condition_time.C condition.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h time.h daisy.h program.h run.h \
  timestep.h vcheck.h memutils.h librarian.h log.h border.h submodeler.h \
  assertion.h
condition_logic${OBJ}: condition_logic.C condition.h model.h symbol.h alist.h \
  log.h time.h border.h syntax.h treelog.h memutils.h librarian.h
action_irrigate${OBJ}: action_irrigate.C action.h model.h symbol.h alist.h \
  scope.h block.h syntax.h treelog.h plf.h daisy.h program.h run.h time.h \
  timestep.h vcheck.h memutils.h chemical.h number.h units.h field.h \
  border.h im.h check.h mathlib.h assertion.h librarian.h volume.h
action_lisp${OBJ}: action_lisp.C action.h model.h symbol.h alist.h daisy.h \
  program.h run.h time.h timestep.h vcheck.h memutils.h log.h border.h \
  submodeler.h block.h syntax.h treelog.h plf.h assertion.h librarian.h \
  condition.h
weather_none${OBJ}: weather_none.C weather_old.h weather.h model.h symbol.h \
  alist.h im.h syntax.h treelog.h block.h plf.h librarian.h
action_tillage${OBJ}: action_tillage.C action.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h daisy.h program.h run.h time.h \
  timestep.h vcheck.h memutils.h field.h border.h check.h librarian.h
action_harvest${OBJ}: action_harvest.C action.h model.h symbol.h alist.h \
  daisy.h program.h run.h time.h timestep.h vcheck.h memutils.h field.h \
  border.h harvest.h block.h syntax.h treelog.h plf.h librarian.h \
  vegetation.h
crop_old${OBJ}: crop_old.C crop.h model.h symbol.h alist.h time.h log.h \
  border.h bioclimate.h plf.h soil_water.h soil.h geometry.h syntax.h \
  treelog.h mathlib.h assertion.h aom.h om.h organic_matter.h soil_heat.h \
  chemistry.h chemical.h am.h im.h harvest.h block.h librarian.h
crop_sold${OBJ}: crop_sold.C crop.h model.h symbol.h alist.h time.h log.h \
  border.h bioclimate.h plf.h soil_water.h soil.h geometry.h syntax.h \
  treelog.h mathlib.h assertion.h organic_matter.h aom.h om.h soil_heat.h \
  chemistry.h chemical.h am.h im.h harvest.h block.h librarian.h
action_with${OBJ}: action_with.C action.h model.h symbol.h alist.h block.h \
  syntax.h treelog.h plf.h daisy.h program.h run.h time.h timestep.h \
  vcheck.h memutils.h field.h border.h log.h librarian.h
nitrification_soil${OBJ}: nitrification_soil.C nitrification.h model.h \
  symbol.h alist.h abiotic.h block.h syntax.h treelog.h plf.h mathlib.h \
  assertion.h check.h librarian.h
nitrification_solute${OBJ}: nitrification_solute.C nitrification.h model.h \
  symbol.h alist.h abiotic.h block.h syntax.h treelog.h plf.h soil.h \
  soil_water.h soil_heat.h mathlib.h assertion.h check.h librarian.h
hydraulic_mod_C${OBJ}: hydraulic_mod_C.C hydraulic.h model.h symbol.h alist.h \
  syntax.h treelog.h block.h plf.h check.h mathlib.h assertion.h \
  librarian.h
uzlr${OBJ}: uzlr.C uzmodel.h model.h symbol.h alist.h block.h syntax.h \
  treelog.h plf.h surface.h groundwater.h geometry_vert.h geometry.h \
  mathlib.h assertion.h soil.h soil_heat.h librarian.h
transport_cd${OBJ}: transport_cd.C transport.h model.h symbol.h alist.h \
  syntax.h treelog.h block.h plf.h geometry1d.h geometry_vert.h \
  geometry.h mathlib.h assertion.h soil.h soil_water.h adsorption.h log.h \
  time.h border.h librarian.h
transport_none${OBJ}: transport_none.C transport.h model.h symbol.h alist.h \
  syntax.h treelog.h geometry1d.h geometry_vert.h geometry.h mathlib.h \
  assertion.h soil.h soil_water.h adsorption.h log.h time.h border.h \
  librarian.h
transport_convection${OBJ}: transport_convection.C transport.h model.h \
  symbol.h alist.h syntax.h treelog.h block.h plf.h geometry1d.h \
  geometry_vert.h geometry.h mathlib.h assertion.h soil.h soil_water.h \
  adsorption.h log.h time.h border.h librarian.h
adsorption_vS_S${OBJ}: adsorption_vS_S.C adsorption.h model.h symbol.h \
  alist.h syntax.h treelog.h soil.h mathlib.h assertion.h librarian.h
tortuosity_M_Q${OBJ}: tortuosity_M_Q.C tortuosity.h model.h symbol.h alist.h \
  syntax.h treelog.h hydraulic.h mathlib.h assertion.h librarian.h
tortuosity_linear${OBJ}: tortuosity_linear.C tortuosity.h model.h symbol.h \
  alist.h block.h syntax.h treelog.h plf.h hydraulic.h librarian.h
adsorption_freundlich${OBJ}: adsorption_freundlich.C adsorption.h model.h \
  symbol.h alist.h block.h syntax.h treelog.h plf.h soil.h check.h \
  mathlib.h assertion.h librarian.h
adsorption_linear${OBJ}: adsorption_linear.C adsorption.h model.h symbol.h \
  alist.h block.h syntax.h treelog.h plf.h check.h soil.h librarian.h
adsorption_langmuir${OBJ}: adsorption_langmuir.C adsorption.h model.h \
  symbol.h alist.h block.h syntax.h treelog.h plf.h soil.h check.h \
  mathlib.h assertion.h librarian.h
bioclimate_std${OBJ}: bioclimate_std.C bioclimate.h model.h symbol.h alist.h \
  metalib.h library.h block.h syntax.h treelog.h plf.h surface.h \
  uzmodel.h weather.h im.h geometry.h mathlib.h assertion.h soil.h \
  soil_heat.h chemistry.h chemical.h snow.h log.h time.h border.h \
  net_radiation.h pet.h difrad.h raddist.h svat.h vegetation.h units.h \
  check.h fao.h librarian.h
condition_crop${OBJ}: condition_crop.C condition.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h crop.h time.h field.h border.h daisy.h \
  program.h run.h timestep.h vcheck.h memutils.h check_range.h check.h \
  mathlib.h assertion.h librarian.h
condition_soil${OBJ}: condition_soil.C condition.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h field.h border.h daisy.h program.h \
  run.h time.h timestep.h vcheck.h memutils.h check.h librarian.h
log_checkpoint${OBJ}: log_checkpoint.C log_alist.h log.h time.h border.h \
  model.h symbol.h alist.h metalib.h block.h syntax.h treelog.h plf.h \
  condition.h daisy.h program.h run.h timestep.h vcheck.h memutils.h \
  printer_file.h printer.h scope.h assertion.h librarian.h
uznone${OBJ}: uznone.C uzmodel.h model.h symbol.h alist.h syntax.h treelog.h \
  soil.h mathlib.h assertion.h librarian.h
condition_daisy${OBJ}: condition_daisy.C condition.h model.h symbol.h alist.h \
  syntax.h treelog.h daisy.h program.h run.h time.h timestep.h vcheck.h \
  memutils.h librarian.h
chemical_std${OBJ}: chemical_std.C chemical.h model.h symbol.h alist.h \
  organic_matter.h soil_heat.h soil_water.h soil.h geometry.h syntax.h \
  treelog.h mathlib.h assertion.h abiotic.h adsorption.h chemistry.h \
  log.h time.h border.h block.h plf.h check.h librarian.h number.h \
  scope_soil.h scope.h vcheck.h memutils.h submodeler.h
hydraulic_M_BaC_Bimodal${OBJ}: hydraulic_M_BaC_Bimodal.C hydraulic.h model.h \
  symbol.h alist.h syntax.h treelog.h block.h plf.h check.h mathlib.h \
  assertion.h librarian.h
hydraulic_B_BaC_Bimodal${OBJ}: hydraulic_B_BaC_Bimodal.C hydraulic.h model.h \
  symbol.h alist.h syntax.h treelog.h block.h plf.h check.h mathlib.h \
  assertion.h librarian.h
pet_makkink${OBJ}: pet_makkink.C pet.h model.h symbol.h alist.h syntax.h \
  treelog.h weather.h im.h fao.h log.h time.h border.h librarian.h
pet_weather${OBJ}: pet_weather.C pet.h model.h symbol.h alist.h syntax.h \
  treelog.h weather.h im.h log.h time.h border.h librarian.h
svat_none${OBJ}: svat_none.C svat.h model.h symbol.h alist.h syntax.h \
  treelog.h librarian.h
action_spray${OBJ}: action_spray.C action.h model.h symbol.h alist.h \
  metalib.h library.h block.h syntax.h treelog.h plf.h daisy.h program.h \
  run.h time.h timestep.h vcheck.h memutils.h field.h border.h chemical.h \
  check.h librarian.h
pet_PM${OBJ}: pet_PM.C pet.h model.h symbol.h alist.h syntax.h treelog.h \
  fao.h weather.h im.h soil.h surface.h uzmodel.h soil_heat.h \
  vegetation.h log.h time.h border.h librarian.h
svat_pmsw${OBJ}: svat_pmsw.C svat.h model.h symbol.h alist.h mathlib.h \
  assertion.h block.h syntax.h treelog.h plf.h surface.h uzmodel.h \
  weather.h im.h time.h soil.h soil_water.h soil_heat.h vegetation.h \
  pet.h log.h border.h fao.h gaussj.h librarian.h nrutil.h
action_surface${OBJ}: action_surface.C action.h model.h symbol.h alist.h \
  block.h syntax.h treelog.h plf.h daisy.h program.h run.h time.h \
  timestep.h vcheck.h memutils.h field.h border.h check.h librarian.h
main${OBJ}: main.C toplevel.h treelog.h symbol.h
cmain${OBJ}: cmain.c cdaisy.h
bugmain${OBJ}: bugmain.c cdaisy.h
