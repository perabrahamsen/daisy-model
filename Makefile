# Makefile -- DAISY 

SHELL = /bin/sh
MAKEFLAGS =

# change these to enable/disable mike she connection
#FORHOME = /pack/f2c
#FORHOME = /usr1/jaj0kvl
#FOROBJ = $(FORTRAN:.f=.o) $(MIKESRC:.c=.o) 
#FOROBJ = mshe/mshe.a
#FORLIB =  -L$(FORHOME)/lib -lI77 -lF77 
#MIKESHE=mike_she.C
#MIKEONLY=
#MIKEFLAGS=-I$(FORHOME)/include -DMIKE_SHE
FOROBJ=
FORLIB=
MIKESRC=
MIKEFLAGS= 

#Uncomment these on SPARC
SPARCSRC = set_exceptions.S
SPARCOBJ = set_exceptions.o
CC = /pack/egcs/bin/c++ -Wall -fno-exceptions -DEGCS -g -pipe $(MIKEFLAGS) -frepo -pg -O3 -ffast-math -mcpu=ultrasparc
#CC = /pack/gcc-2.7.1/bin/c++ -Wall -g -pipe $(MIKEFLAGS) -frepo -O3 -ffast-math -mv8 -pg
# CC = /pack/devpro/SUNWspro/bin/CC $(MIKEFLAGS)
MATHLIB = -lm

#Use these on HPUX (repo doesn't work)
#MATHLLIB = -lM
#CC = c++ -Wall -Wcast-qual -pipe $(MIKEFLAGS) -O2 

#Use these on Borland C++ 5.01
#CC = /bc5/bin/bcc32 -P -v -WC

SRCONLY = filter_array.C filter_all.C filter_none.C filter_some.C \
	column_std.C  weather_simple.C uzrichard.C \
	hydraulic_yolo.C hydraulic_M_vG.C hydraulic_B_vG.C hydraulic_M_C.C \
	hydraulic_B_C.C hydraulic_M_BaC.C hydraulic_B_BaC.C \
	groundwater_static.C horizon_std.C \
	crop_std.C action_sow.C action_stop.C condition_time.C \
	condition_logic.C log_file.C action_irrigate.C action_lisp.C \
	weather_none.C action_fertilize.C weather_file.C action_tillage.C \
	action_harvest.C hydraulic_old.C $(MIKEONLY) crop_old.C crop_sold.C \
	action_with.C hydraulic_old2.C nitrification_soil.C \
	nitrification_solute.C hydraulic_mod_C.C
OBJECTS = main.C daisy.C parser.C log.C weather.C column.C crop.C \
	alist.C syntax.C library.C action.C condition.C horizon.C \
	filter.C csmp.C time.C uzmodel.C parser_file.C hydraulic.C \
	soil.C mathlib.C bioclimate.C surface.C soil_water.C \
	soil_NH4.C soil_NO3.C organic_matter.C nitrification.C \
	denitrification.C soil_heat.C groundwater.C snow.C solute.C \
	am.C im.C om.C harvest.C $(MIKESHE) options.C geometry.C
OBJ = $(SRCONLY:.C=.o) $(OBJECTS:.C=.o) $(SPARCOBJ)
SRC = $(SRCONLY) $(OBJECTS) $(SPARCSRC) 
HEAD = $(OBJECTS:.C=.h) common.h librarian.h
TEXT =  Makefile $(HEAD) $(SRC) 

# To be removed by the next cvs update.
REMOVE = none 

.SUFFIXES:	.C .o .h .c

daisy:	$(OBJ) $(FOROBJ)
	$(CC) -o daisy $(OBJ) $(FOROBJ) $(FORLIB) -lm

#	/bc5/bin/bcc32 -v -WC -edaisy $(OBJ:.o=.OBJ) $(FOROBJ) $(FORLIB) 



mshe/mshe.a:
	(cd mshe; $(MAKE) mshe.a) 

set_exceptions.o: set_exceptions.S
	as -o set_exceptions.o set_exceptions.S

wc: $(TEXT)
	wc -l $(TEXT)

wc-h: $(HEAD)
	wc -l $(HEAD)

wc-s: $(SRC)
	wc -l $(SRC)

tags: TAGS

TAGS: $(SRC) $(HEAD)
	etags $(SRC) $(HEAD)

dos2unix:
	perl -pi.bak -e 's/\r\n$$/\n/' $(TEXT)

print:
	mp -p /home/user_13/fischer/bin/mp.pro.none -a4 $(TEXT) | parr -s | up -n pup | lpr -Pduplex

dump:	daisy
	daisy -p

test:	crop-test water-test evapo-test

bless:
	(cd test; make bless )

water-test:	daisy 
	(cd test; make water-test )

crop-test:	daisy 
	(cd test; make crop-test )

evapo-test:	daisy 
	(cd test; make evapo-test )

balance:	daisy
	(cd test; make balance)

check:	daisy
	(cd exp; make test )


clean:
	rm $(OBJ) *.rpo daisy *~

depend: $(SRC) 
	rm -f Makefile.old
	mv Makefile Makefile.old
	sed -e '/^# AUTOMATIC/q' < Makefile.old > Makefile
	g++ -DMIKE_SHE -I. -MM $(SRC) >> Makefile

daisy.zip:	$(TEXT)
	zip daisy.zip $(TEXT)

cvs: $(TEXT)
	(cd lib; $(MAKE) cvs);
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	-cvs add $(TEXT)
	rm -f $(REMOVE) 
	-cvs remove $(REMOVE) 
	cvs commit -m "$(TAG)" # "Version $(TAG)"

.C.o:
	$(CC) -c $<

#	touch ${<:.C=.o} 

.c.o:
	gcc -I/pack/f2c/include -c $<

############################################################
# AUTOMATIC -- DO NOT CHANGE THIS LINE OR ANYTHING BELOW IT!
filter_array.o: filter_array.C filter.h librarian.h library.h common.h \
 alist.h syntax.h geometry.h
filter_all.o: filter_all.C filter.h librarian.h library.h common.h \
 alist.h syntax.h
filter_none.o: filter_none.C filter.h librarian.h library.h common.h \
 alist.h syntax.h
filter_some.o: filter_some.C filter.h librarian.h library.h common.h \
 alist.h syntax.h
column_std.o: column_std.C column.h librarian.h library.h common.h \
 alist.h syntax.h crop.h bioclimate.h surface.h uzmodel.h im.h soil.h \
 horizon.h hydraulic.h geometry.h soil_water.h soil_heat.h soil_NH4.h \
 solute.h soil_NO3.h organic_matter.h nitrification.h \
 denitrification.h log.h filter.h am.h
weather_simple.o: weather_simple.C weather.h librarian.h library.h \
 common.h alist.h syntax.h im.h log.h filter.h
uzrichard.o: uzrichard.C uzmodel.h common.h soil.h horizon.h \
 hydraulic.h geometry.h mathlib.h alist.h syntax.h filter.h \
 librarian.h library.h log.h
hydraulic_yolo.o: hydraulic_yolo.C hydraulic.h common.h syntax.h \
 alist.h csmp.h
hydraulic_M_vG.o: hydraulic_M_vG.C hydraulic.h common.h syntax.h \
 alist.h csmp.h
hydraulic_B_vG.o: hydraulic_B_vG.C hydraulic.h common.h syntax.h \
 alist.h csmp.h
hydraulic_M_C.o: hydraulic_M_C.C hydraulic.h common.h syntax.h alist.h
hydraulic_B_C.o: hydraulic_B_C.C hydraulic.h common.h syntax.h alist.h
hydraulic_M_BaC.o: hydraulic_M_BaC.C hydraulic.h common.h syntax.h \
 alist.h
hydraulic_B_BaC.o: hydraulic_B_BaC.C hydraulic.h common.h syntax.h \
 alist.h
groundwater_static.o: groundwater_static.C groundwater.h time.h \
 common.h uzmodel.h syntax.h alist.h
horizon_std.o: horizon_std.C horizon.h common.h syntax.h alist.h
crop_std.o: crop_std.C crop.h time.h common.h log.h filter.h \
 librarian.h library.h alist.h syntax.h csmp.h bioclimate.h column.h \
 soil_water.h soil.h horizon.h hydraulic.h geometry.h om.h \
 organic_matter.h soil_heat.h soil_NH4.h solute.h soil_NO3.h am.h \
 harvest.h mathlib.h
action_sow.o: action_sow.C action.h common.h daisy.h column.h \
 librarian.h library.h alist.h syntax.h crop.h
action_stop.o: action_stop.C action.h common.h syntax.h alist.h \
 daisy.h
condition_time.o: condition_time.C condition.h librarian.h library.h \
 common.h alist.h syntax.h daisy.h
condition_logic.o: condition_logic.C condition.h librarian.h library.h \
 common.h alist.h syntax.h
log_file.o: log_file.C log.h filter.h librarian.h library.h common.h \
 alist.h syntax.h condition.h csmp.h
action_irrigate.o: action_irrigate.C action.h common.h daisy.h \
 weather.h librarian.h library.h alist.h syntax.h im.h column.h am.h
action_lisp.o: action_lisp.C action.h common.h daisy.h column.h \
 librarian.h library.h alist.h syntax.h condition.h
weather_none.o: weather_none.C weather.h librarian.h library.h \
 common.h alist.h syntax.h im.h
action_fertilize.o: action_fertilize.C action.h common.h daisy.h \
 column.h librarian.h library.h alist.h syntax.h am.h im.h
weather_file.o: weather_file.C weather.h librarian.h library.h \
 common.h alist.h syntax.h im.h options.h log.h filter.h mike_she.h
action_tillage.o: action_tillage.C action.h common.h daisy.h weather.h \
 librarian.h library.h alist.h syntax.h im.h column.h
action_harvest.o: action_harvest.C action.h common.h daisy.h column.h \
 librarian.h library.h alist.h syntax.h
hydraulic_old.o: hydraulic_old.C hydraulic.h common.h options.h \
 syntax.h alist.h mathlib.h csmp.h
crop_old.o: crop_old.C crop.h time.h common.h log.h filter.h \
 librarian.h library.h alist.h syntax.h csmp.h bioclimate.h column.h \
 soil_water.h soil.h horizon.h hydraulic.h geometry.h om.h \
 organic_matter.h soil_heat.h soil_NH4.h solute.h soil_NO3.h am.h \
 harvest.h mathlib.h
crop_sold.o: crop_sold.C crop.h time.h common.h log.h filter.h \
 librarian.h library.h alist.h syntax.h csmp.h bioclimate.h column.h \
 soil_water.h soil.h horizon.h hydraulic.h geometry.h organic_matter.h \
 om.h soil_heat.h soil_NH4.h solute.h soil_NO3.h am.h harvest.h \
 mathlib.h
action_with.o: action_with.C action.h common.h daisy.h syntax.h \
 alist.h column.h librarian.h library.h
hydraulic_old2.o: hydraulic_old2.C hydraulic.h common.h options.h \
 syntax.h alist.h mathlib.h csmp.h
nitrification_soil.o: nitrification_soil.C nitrification.h librarian.h \
 library.h common.h alist.h syntax.h soil.h horizon.h hydraulic.h \
 geometry.h soil_water.h soil_heat.h soil_NH4.h solute.h soil_NO3.h \
 csmp.h mathlib.h log.h filter.h
nitrification_solute.o: nitrification_solute.C nitrification.h \
 librarian.h library.h common.h alist.h syntax.h soil.h horizon.h \
 hydraulic.h geometry.h soil_water.h soil_heat.h soil_NH4.h solute.h \
 soil_NO3.h csmp.h log.h filter.h mathlib.h
hydraulic_mod_C.o: hydraulic_mod_C.C hydraulic.h common.h syntax.h \
 alist.h
main.o: main.C daisy.h common.h parser_file.h parser.h syntax.h \
 alist.h
daisy.o: daisy.C daisy.h common.h weather.h librarian.h library.h \
 alist.h syntax.h im.h groundwater.h uzmodel.h horizon.h log.h \
 filter.h parser.h am.h nitrification.h hydraulic.h crop.h column.h \
 harvest.h action.h condition.h mike_she.h
parser.o: parser.C parser.h common.h alist.h library.h syntax.h
log.o: log.C log.h filter.h librarian.h library.h common.h alist.h \
 syntax.h
weather.o: weather.C weather.h librarian.h library.h common.h alist.h \
 syntax.h im.h mathlib.h
column.o: column.C column.h librarian.h library.h common.h alist.h \
 syntax.h
crop.o: crop.C crop.h time.h common.h library.h alist.h syntax.h
alist.o: alist.C alist.h common.h
syntax.o: syntax.C syntax.h common.h alist.h library.h
library.o: library.C library.h common.h alist.h syntax.h
action.o: action.C action.h common.h alist.h library.h syntax.h
condition.o: condition.C condition.h librarian.h library.h common.h \
 alist.h syntax.h
horizon.o: horizon.C horizon.h common.h library.h alist.h syntax.h \
 csmp.h hydraulic.h mathlib.h
filter.o: filter.C filter.h librarian.h library.h common.h alist.h \
 syntax.h
csmp.o: csmp.C csmp.h common.h log.h filter.h librarian.h library.h \
 alist.h syntax.h
time.o: time.C time.h common.h
uzmodel.o: uzmodel.C uzmodel.h common.h library.h alist.h syntax.h
parser_file.o: parser_file.C parser_file.h parser.h common.h options.h \
 syntax.h alist.h library.h csmp.h log.h filter.h librarian.h
hydraulic.o: hydraulic.C hydraulic.h common.h library.h alist.h \
 syntax.h csmp.h
soil.o: soil.C soil.h horizon.h common.h hydraulic.h geometry.h \
 alist.h syntax.h mathlib.h
mathlib.o: mathlib.C mathlib.h common.h
bioclimate.o: bioclimate.C bioclimate.h column.h librarian.h library.h \
 common.h alist.h syntax.h surface.h uzmodel.h im.h weather.h crop.h \
 csmp.h soil.h horizon.h hydraulic.h geometry.h snow.h log.h filter.h \
 mike_she.h
surface.o: surface.C surface.h uzmodel.h common.h im.h syntax.h \
 alist.h soil_water.h log.h filter.h librarian.h library.h am.h \
 mathlib.h mike_she.h
soil_water.o: soil_water.C soil_water.h common.h log.h filter.h \
 librarian.h library.h alist.h syntax.h uzmodel.h soil.h horizon.h \
 hydraulic.h geometry.h surface.h im.h groundwater.h mathlib.h \
 mike_she.h
soil_NH4.o: soil_NH4.C soil_NH4.h solute.h common.h soil_water.h \
 soil.h horizon.h hydraulic.h geometry.h mathlib.h
soil_NO3.o: soil_NO3.C soil_NO3.h solute.h common.h soil_water.h \
 soil.h horizon.h hydraulic.h geometry.h mike_she.h
organic_matter.o: organic_matter.C organic_matter.h syntax.h common.h \
 alist.h log.h filter.h librarian.h library.h am.h om.h soil.h \
 horizon.h hydraulic.h geometry.h soil_water.h soil_NH4.h solute.h \
 soil_NO3.h soil_heat.h mathlib.h csmp.h
nitrification.o: nitrification.C nitrification.h librarian.h library.h \
 common.h alist.h syntax.h
denitrification.o: denitrification.C denitrification.h common.h \
 alist.h syntax.h soil.h horizon.h hydraulic.h geometry.h soil_water.h \
 soil_heat.h organic_matter.h soil_NO3.h solute.h csmp.h log.h \
 filter.h librarian.h library.h
soil_heat.o: soil_heat.C soil_heat.h alist.h common.h surface.h \
 uzmodel.h im.h groundwater.h soil_water.h soil.h horizon.h \
 hydraulic.h geometry.h syntax.h mathlib.h log.h filter.h librarian.h \
 library.h
groundwater.o: groundwater.C groundwater.h time.h common.h uzmodel.h \
 library.h alist.h syntax.h
snow.o: snow.C snow.h alist.h common.h syntax.h log.h filter.h \
 librarian.h library.h soil.h horizon.h hydraulic.h geometry.h \
 soil_water.h soil_heat.h mathlib.h mike_she.h
solute.o: solute.C solute.h common.h log.h filter.h librarian.h \
 library.h alist.h syntax.h soil.h horizon.h hydraulic.h geometry.h \
 soil_water.h mathlib.h
am.o: am.C am.h common.h om.h im.h library.h alist.h syntax.h log.h \
 filter.h librarian.h geometry.h mathlib.h
im.o: im.C im.h log.h filter.h librarian.h library.h common.h alist.h \
 syntax.h
om.o: om.C om.h common.h syntax.h alist.h geometry.h log.h filter.h \
 librarian.h library.h mathlib.h
harvest.o: harvest.C harvest.h time.h common.h syntax.h log.h filter.h \
 librarian.h library.h alist.h
options.o: options.C options.h common.h
geometry.o: geometry.C geometry.h common.h syntax.h alist.h mathlib.h
set_exceptions.o: set_exceptions.S
