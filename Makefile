# Makefile -- DAISY 

SHELL = /bin/sh
MAKEFLAGS =
CC = /pack/gcc-2.7.1/bin/c++ -Wall -Wcast-qual -g -frepo -pipe #-O2 # -fhandle-exceptions -pipe -fno-implicit-templates
SRCONLY = column_std.o  weather_simple.o uzrichard.o \
	hydraulic_yolo.o hydraulic_M_vG.o hydraulic_B_vG.o hydraulic_M_C.o \
	hydraulic_B_C.o hydraulic_M_BaC.o hydraulic_B_BaC.o \
	groundwater_static.o horizon_std.o \
	crop_std.o action_sow.o action_stop.o condition_time.o \
	condition_logic.o log_file.o action_irrigate.o action_lisp.o \
	weather_none.o action_fertilize.o weather_file.o action_tillage.o \
	action_harvest.o hydraulic_old.o
OBJECTS = main.o daisy.o parser.o log.o weather.o column.o crop.o \
	alist.o syntax.o library.o action.o condition.o horizon.o ftable.o \
	filter.o csmp.o time.o uzmodel.o parser_file.o hydraulic.o \
	soil.o mathlib.o bioclimate.o surface.o soil_water.o \
	soil_NH4.o soil_NO3.o organic_matter.o nitrification.o \
	denitrification.o soil_heat.o groundwater.o snow.o solute.o \
	am.o im.o om.o
OBJ = $(OBJECTS) $(SRCONLY)
SRC = $(OBJ:.o=.C) set_exceptions.S
HEAD = $(OBJECTS:.o=.h) common.h 
TEXT =  Makefile $(HEAD) $(SRC) ftable.t

# To be removed by the next cvs update.
REMOVE = none

.SUFFIXES:	.C .o .h

daisy:	$(OBJ)
	$(CC) -o daisy $(OBJ) set_exceptions.o -lm

set_exceptions.o: set_exceptions.S
	as set_exceptions.S

wc: $(TEXT)
	wc -l $(TEXT)

wc-h: $(HEAD)
	wc -l $(HEAD)

wc-s: $(SRC)
	wc -l $(SRC)

tags: TAGS

TAGS: $(SRC) $(HEAD)
	etags $(SRC) $(HEAD)

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
	g++ -I. -MM $(SRC) >> Makefile

cvs: $(TEXT)
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	-cvs add $(TEXT)
	(cd test ; make cvs )
	rm -f $(REMOVE) 
	-cvs remove $(REMOVE) 
	cvs commit -m "$(TAG)" # "Version $(TAG)"

.C.o:
	$(CC) -c $<

############################################################
# AUTOMATIC -- DO NOT CHANGE THIS LINE OR ANYTHING BELOW IT!
main.o: main.C daisy.h time.h parser_file.h parser.h syntax.h alist.h \
 common.h
daisy.o: daisy.C daisy.h time.h weather.h groundwater.h uzmodel.h \
 common.h horizon.h log.h parser.h am.h om.h hydraulic.h crop.h \
 column.h action.h filter.h library.h syntax.h condition.h alist.h
parser.o: parser.C parser.h alist.h common.h library.h syntax.h
log.o: log.C log.h alist.h common.h library.h syntax.h
weather.o: weather.C weather.h time.h library.h alist.h common.h \
 syntax.h
column.o: column.C column.h library.h alist.h common.h syntax.h
crop.o: crop.C crop.h library.h alist.h common.h syntax.h
alist.o: alist.C alist.h common.h time.h
syntax.o: syntax.C syntax.h alist.h common.h library.h
library.o: library.C library.h alist.h common.h syntax.h
action.o: action.C action.h alist.h common.h library.h syntax.h
condition.o: condition.C condition.h alist.h common.h library.h \
 syntax.h
horizon.o: horizon.C horizon.h library.h alist.h common.h syntax.h \
 csmp.h hydraulic.h
ftable.o: ftable.C ftable.h
filter.o: filter.C filter.h common.h
csmp.o: csmp.C csmp.h log.h
time.o: time.C time.h
uzmodel.o: uzmodel.C uzmodel.h common.h library.h alist.h syntax.h
parser_file.o: parser_file.C parser_file.h parser.h syntax.h alist.h \
 common.h library.h csmp.h time.h log.h filter.h
hydraulic.o: hydraulic.C hydraulic.h library.h alist.h common.h \
 syntax.h csmp.h
soil.o: soil.C soil.h horizon.h hydraulic.h alist.h common.h syntax.h
mathlib.o: mathlib.C mathlib.h
bioclimate.o: bioclimate.C bioclimate.h column.h surface.h uzmodel.h \
 common.h im.h weather.h time.h crop.h csmp.h alist.h soil.h horizon.h \
 hydraulic.h syntax.h snow.h log.h filter.h
surface.o: surface.C surface.h uzmodel.h common.h im.h syntax.h \
 alist.h soil_water.h log.h filter.h am.h time.h om.h
soil_water.o: soil_water.C soil_water.h log.h alist.h common.h \
 uzmodel.h soil.h horizon.h hydraulic.h surface.h im.h groundwater.h \
 time.h syntax.h mathlib.h
soil_NH4.o: soil_NH4.C soil_NH4.h solute.h common.h soil_water.h \
 soil.h horizon.h hydraulic.h mathlib.h
soil_NO3.o: soil_NO3.C soil_NO3.h solute.h common.h soil_water.h
organic_matter.o: organic_matter.C organic_matter.h syntax.h alist.h \
 common.h log.h filter.h am.h time.h om.h soil.h horizon.h hydraulic.h \
 soil_water.h soil_NH4.h solute.h soil_NO3.h soil_heat.h mathlib.h \
 csmp.h
nitrification.o: nitrification.C nitrification.h alist.h common.h \
 syntax.h soil.h horizon.h hydraulic.h soil_water.h soil_heat.h \
 soil_NH4.h solute.h soil_NO3.h csmp.h
denitrification.o: denitrification.C denitrification.h alist.h \
 common.h syntax.h soil.h horizon.h hydraulic.h soil_water.h \
 soil_heat.h organic_matter.h soil_NO3.h solute.h csmp.h
soil_heat.o: soil_heat.C soil_heat.h alist.h common.h surface.h \
 uzmodel.h im.h groundwater.h time.h soil_water.h soil.h horizon.h \
 hydraulic.h syntax.h mathlib.h log.h
groundwater.o: groundwater.C groundwater.h time.h uzmodel.h common.h \
 library.h alist.h syntax.h
snow.o: snow.C snow.h alist.h common.h syntax.h log.h filter.h soil.h \
 horizon.h hydraulic.h soil_water.h soil_heat.h
solute.o: solute.C solute.h common.h log.h filter.h syntax.h alist.h \
 soil.h horizon.h hydraulic.h soil_water.h mathlib.h
am.o: am.C am.h time.h om.h im.h library.h alist.h common.h syntax.h \
 log.h soil.h horizon.h hydraulic.h mathlib.h
im.o: im.C im.h log.h alist.h common.h syntax.h
om.o: om.C om.h syntax.h alist.h common.h soil.h horizon.h hydraulic.h \
 log.h mathlib.h
column_std.o: column_std.C column.h crop.h bioclimate.h surface.h \
 uzmodel.h common.h im.h soil.h horizon.h hydraulic.h soil_water.h \
 soil_heat.h soil_NH4.h solute.h soil_NO3.h organic_matter.h \
 nitrification.h denitrification.h alist.h syntax.h library.h log.h \
 filter.h am.h time.h om.h
weather_simple.o: weather_simple.C weather.h time.h syntax.h alist.h \
 common.h log.h filter.h
uzrichard.o: uzrichard.C uzmodel.h common.h soil.h horizon.h \
 hydraulic.h mathlib.h alist.h syntax.h filter.h log.h
hydraulic_yolo.o: hydraulic_yolo.C hydraulic.h syntax.h alist.h \
 common.h csmp.h
hydraulic_M_vG.o: hydraulic_M_vG.C hydraulic.h syntax.h alist.h \
 common.h csmp.h
hydraulic_B_vG.o: hydraulic_B_vG.C hydraulic.h syntax.h alist.h \
 common.h csmp.h
hydraulic_M_C.o: hydraulic_M_C.C hydraulic.h syntax.h alist.h common.h
hydraulic_B_C.o: hydraulic_B_C.C hydraulic.h syntax.h alist.h common.h
hydraulic_M_BaC.o: hydraulic_M_BaC.C hydraulic.h syntax.h alist.h \
 common.h
hydraulic_B_BaC.o: hydraulic_B_BaC.C hydraulic.h syntax.h alist.h \
 common.h
groundwater_static.o: groundwater_static.C groundwater.h time.h \
 uzmodel.h common.h syntax.h alist.h
horizon_std.o: horizon_std.C horizon.h syntax.h alist.h common.h
crop_std.o: crop_std.C crop.h log.h time.h csmp.h bioclimate.h \
 column.h common.h ftable.h ftable.t syntax.h alist.h filter.h \
 soil_water.h soil.h horizon.h hydraulic.h soil_heat.h soil_NH4.h \
 solute.h soil_NO3.h am.h om.h mathlib.h
action_sow.o: action_sow.C action.h daisy.h time.h column.h crop.h \
 syntax.h alist.h common.h
action_stop.o: action_stop.C action.h syntax.h alist.h common.h \
 daisy.h time.h
condition_time.o: condition_time.C condition.h time.h syntax.h alist.h \
 common.h daisy.h
condition_logic.o: condition_logic.C condition.h syntax.h alist.h \
 common.h
log_file.o: log_file.C log.h condition.h filter.h csmp.h time.h \
 alist.h common.h syntax.h
action_irrigate.o: action_irrigate.C action.h daisy.h time.h weather.h \
 column.h syntax.h alist.h common.h am.h om.h im.h
action_lisp.o: action_lisp.C action.h daisy.h time.h column.h \
 condition.h syntax.h alist.h common.h
weather_none.o: weather_none.C weather.h time.h syntax.h alist.h \
 common.h
action_fertilize.o: action_fertilize.C action.h daisy.h time.h \
 column.h syntax.h alist.h common.h am.h om.h im.h library.h
weather_file.o: weather_file.C weather.h time.h syntax.h alist.h \
 common.h log.h filter.h
action_tillage.o: action_tillage.C action.h daisy.h time.h weather.h \
 column.h syntax.h alist.h common.h
action_harvest.o: action_harvest.C action.h daisy.h time.h column.h \
 syntax.h alist.h common.h library.h
hydraulic_old.o: hydraulic_old.C hydraulic.h syntax.h alist.h common.h \
 csmp.h
