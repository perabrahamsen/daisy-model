# Makefile -- DAISY 

SHELL = /bin/sh
MAKEFLAGS =

# change these to enable/disable mike she connection
#FOROBJ = $(FORTRAN:.f=.o) $(MIKESRC:.c=.o) 
FOROBJ = mshe/mshe.a
FORLIB =  -L/pack/f2c/lib -lI77 -lF77 
MIKESHE=mike_she.C
MIKEONLY=
MIKEFLAGS=-I/pack/f2c/include -DMIKE_SHE
#FOROBJ=
#FORLIB=
#MIKESRC=
#MIKEFLAGS= 

CC = /pack/gcc-2.7.1/bin/c++ -Wall -Wcast-qual -g -frepo -pipe $(MIKEFLAGS) -O2
# CC = /pack/devpro/SUNWspro/bin/CC $(MIKEFLAGS)
SRCONLY = column_std.C  weather_simple.C uzrichard.C \
	hydraulic_yolo.C hydraulic_M_vG.C hydraulic_B_vG.C hydraulic_M_C.C \
	hydraulic_B_C.C hydraulic_M_BaC.C hydraulic_B_BaC.C \
	groundwater_static.C horizon_std.C \
	crop_std.C action_sow.C action_stop.C condition_time.C \
	condition_logic.C log_file.C action_irrigate.C action_lisp.C \
	weather_none.C action_fertilize.C weather_file.C action_tillage.C \
	action_harvest.C hydraulic_old.C $(MIKEONLY) crop_old.C crop_sold.C \
	action_with.C
OBJECTS = main.C daisy.C parser.C log.C weather.C column.C crop.C \
	alist.C syntax.C library.C action.C condition.C horizon.C ftable.C \
	filter.C csmp.C time.C uzmodel.C parser_file.C hydraulic.C \
	soil.C mathlib.C bioclimate.C surface.C soil_water.C \
	soil_NH4.C soil_NO3.C organic_matter.C nitrification.C \
	denitrification.C soil_heat.C groundwater.C snow.C solute.C \
	am.C im.C om.C harvest.C $(MIKESHE)
OBJ = $(OBJECTS:.C=.o) $(SRCONLY:.C=.o) set_exceptions.o 
SRC = $(OBJECTS) $(SRCONLY) set_exceptions.S
HEAD = $(OBJECTS:.C=.h) common.h 
TEXT =  Makefile $(HEAD) $(SRC) ftable.t

# To be removed by the next cvs update.
REMOVE = none

.SUFFIXES:	.C .o .h .c

daisy:	$(OBJ) $(FOROBJ)
	$(CC) -o daisy $(OBJ) $(FOROBJ) $(FORLIB) -lm

mshe/mshe.a:
	(cd mshe; $(MAKE) mshe.a) 

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
	g++ -DMIKE_SHE -I. -MM $(SRC) >> Makefile

daisy.zip:	$(TEXT)
	zip daisy.zip $(TEXT)

cvs: $(TEXT)
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	-cvs add $(TEXT)
	rm -f $(REMOVE) 
	-cvs remove $(REMOVE) 
	cvs commit -m "$(TAG)" # "Version $(TAG)"

.C.o:
	$(CC) -c $<

.c.o:
	gcc -I/pack/f2c/include -c $<

############################################################
# AUTOMATIC -- DO NOT CHANGE THIS LINE OR ANYTHING BELOW IT!
main.o: main.C daisy.h time.h parser_file.h parser.h syntax.h alist.h \
 common.h
daisy.o: daisy.C daisy.h time.h weather.h groundwater.h uzmodel.h \
 common.h horizon.h log.h parser.h am.h om.h hydraulic.h crop.h \
 column.h action.h filter.h library.h syntax.h condition.h alist.h \
 mike_she.h
parser.o: parser.C parser.h alist.h common.h library.h syntax.h
log.o: log.C log.h alist.h common.h library.h syntax.h
weather.o: weather.C weather.h time.h library.h alist.h common.h \
 syntax.h
column.o: column.C column.h library.h alist.h common.h syntax.h
crop.o: crop.C crop.h time.h library.h alist.h common.h syntax.h
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
soil.o: soil.C soil.h horizon.h hydraulic.h alist.h common.h syntax.h \
 mathlib.h
mathlib.o: mathlib.C mathlib.h
bioclimate.o: bioclimate.C bioclimate.h column.h surface.h uzmodel.h \
 common.h im.h weather.h time.h crop.h csmp.h alist.h soil.h horizon.h \
 hydraulic.h syntax.h snow.h log.h filter.h mike_she.h
surface.o: surface.C surface.h uzmodel.h common.h im.h syntax.h \
 alist.h soil_water.h log.h filter.h am.h time.h om.h mathlib.h \
 mike_she.h
soil_water.o: soil_water.C soil_water.h log.h alist.h common.h \
 uzmodel.h soil.h horizon.h hydraulic.h surface.h im.h groundwater.h \
 time.h syntax.h mathlib.h mike_she.h
soil_NH4.o: soil_NH4.C soil_NH4.h solute.h common.h soil_water.h \
 soil.h horizon.h hydraulic.h mathlib.h
soil_NO3.o: soil_NO3.C soil_NO3.h solute.h common.h soil_water.h \
 soil.h horizon.h hydraulic.h mike_she.h
organic_matter.o: organic_matter.C organic_matter.h syntax.h alist.h \
 common.h log.h filter.h am.h time.h om.h soil.h horizon.h hydraulic.h \
 soil_water.h soil_NH4.h solute.h soil_NO3.h soil_heat.h mathlib.h \
 csmp.h
nitrification.o: nitrification.C nitrification.h alist.h common.h \
 syntax.h soil.h horizon.h hydraulic.h soil_water.h soil_heat.h \
 soil_NH4.h solute.h soil_NO3.h csmp.h log.h filter.h
denitrification.o: denitrification.C denitrification.h alist.h \
 common.h syntax.h soil.h horizon.h hydraulic.h soil_water.h \
 soil_heat.h organic_matter.h soil_NO3.h solute.h csmp.h log.h \
 filter.h
soil_heat.o: soil_heat.C soil_heat.h alist.h common.h surface.h \
 uzmodel.h im.h groundwater.h time.h soil_water.h soil.h horizon.h \
 hydraulic.h syntax.h mathlib.h log.h
groundwater.o: groundwater.C groundwater.h time.h uzmodel.h common.h \
 library.h alist.h syntax.h
snow.o: snow.C snow.h alist.h common.h syntax.h log.h filter.h soil.h \
 horizon.h hydraulic.h soil_water.h soil_heat.h mathlib.h mike_she.h
solute.o: solute.C solute.h common.h log.h filter.h syntax.h alist.h \
 soil.h horizon.h hydraulic.h soil_water.h mathlib.h
am.o: am.C am.h time.h om.h im.h library.h alist.h common.h syntax.h \
 log.h soil.h horizon.h hydraulic.h mathlib.h
im.o: im.C im.h log.h alist.h common.h syntax.h
om.o: om.C om.h syntax.h alist.h common.h soil.h horizon.h hydraulic.h \
 log.h mathlib.h
mike_she.o: mike_she.C mike_she.h common.h mshe/ff_write_read.P \
 mshe/mshedaisycoup.P mshe/prdebug.P
column_std.o: column_std.C column.h crop.h time.h bioclimate.h \
 surface.h uzmodel.h common.h im.h soil.h horizon.h hydraulic.h \
 soil_water.h soil_heat.h soil_NH4.h solute.h soil_NO3.h \
 organic_matter.h nitrification.h denitrification.h alist.h syntax.h \
 library.h log.h filter.h am.h om.h
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
crop_std.o: crop_std.C crop.h time.h log.h csmp.h bioclimate.h \
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
 common.h log.h filter.h mike_she.h
action_tillage.o: action_tillage.C action.h daisy.h time.h weather.h \
 column.h syntax.h alist.h common.h
action_harvest.o: action_harvest.C action.h daisy.h time.h column.h \
 syntax.h alist.h common.h library.h
hydraulic_old.o: hydraulic_old.C hydraulic.h syntax.h alist.h common.h \
 csmp.h
set_exceptions.o: set_exceptions.S
