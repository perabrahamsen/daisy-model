# Makefile -- DAISY 

SHELL = /bin/sh
CC = /pack/gcc-2.7.1/bin/c++ -Wall -g -frepo # -O2 -fhandle-exceptions -pipe -fno-implicit-templates
SRCONLY = column_std.o manager_rule.o weather_simple.o uzrichard.o \
	horizon_yolo.o horizon_M_vG.o horizon_B_vG.o horizon_M_C.o \
	horizon_B_C.o horizon_M_BaC.o horizon_B_BaC.o groundwater_static.o \
	crop_std.o 
OBJECTS = main.o daisy.o input.o log.o weather.o manager.o column.o crop.o \
	alist.o syntax.o library.o action.o condition.o horizon.o ftable.o \
	filter.o csmp.o rules.o time.o uzmodel.o \
	soil.o mathlib.o bioclimate.o surface.o soil_water.o \
	soil_NH4.o soil_NO3.o organic_matter.o nitrification.o \
	denitrification.o soil_heat.o groundwater.o snow.o
OBJ = $(OBJECTS) $(SRCONLY)
SRC = $(OBJ:.o=.C)
HEAD = $(OBJECTS:.o=.h) common.h
TEST = crop.dai old_crop.chp old_crop.log \
	water.dai old_water.log water.gen water.plot
TEXT =  Makefile $(HEAD) $(SRC) ftable.t

# To be removed by the next cvs update.
REMOVE = uzrichard.h

.SUFFIXES:	.C .o .h

daisy:	$(OBJ)
	$(CC) -o daisy $(OBJ) -lm

bug: bug.o
	$(CC) -o bug bug.o
	bug

bug.o:	bug.C
	$(CC) -v -c bug.C

wc: $(TEXT)
	wc -l $(TEXT)

tags: TAGS

TAGS: $(SRC) $(HEAD)
	etags $(SRC) $(HEAD)

print:
	mp -p /home/user_13/fischer/bin/mp.pro.none -a4 $(TEXT) | parr -s | up -n pup | lpr -Pduplex

water-print:	water.log
	water.gen water.log
	gnuplot water.plot

water.log:	daisy water.dai
	daisy water.dai

crop.log:	daisy crop.dai
	daisy crop.dai

crop.chp:	daisy crop.dai
	daisy crop.dai

crop-test:	crop.log crop.chp
	diff old_crop.chp crop.chp
	diff old_crop.log crop.log

water-test:	water.log
	diff old_water.log water.log

check:	daisy test.dai
	daisy test.dai

# diff old_water.chp water.chp

test:	crop-test water-test

clean:
	rm $(OBJ) *.rpo daisy *~

depend: $(SRC) 
	rm -f Makefile.old
	mv Makefile Makefile.old
	sed -e '/^# AUTOMATIC/q' < Makefile.old > Makefile
	g++ -I. -MM $(SRC) >> Makefile

cvs: $(TEXT) $(TEST)
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	-cvs add $(TEXT) $(TEST)
	rm -f $(REMOVE) 
	-cvs remove $(REMOVE) 
	cvs commit -m "$(TAG)" # "Version $(TAG)"

.C.o:
	$(CC) -c $<

$(OBJ):	daisy.h

pcrop.c: crop.pas
	/pack/p2c/bin/p2c -a $<
	mv crop.c pcrop.c

fsnow.c: fsnow.f
	/pack/f2c/bin/f2c $<

pcrop: pcrop.c
	gcc -L/pack/p2c/lib -I/pack/p2c/include pcrop.c -g -o pcrop -lm -lp2c

ptest: pcrop
	pcrop < /dev/null

############################################################
# AUTOMATIC -- DO NOT CHANGE THIS LINE OR ANYTHING BELOW IT!
main.o: main.C daisy.h time.h input.h syntax.h log.h alist.h common.h
daisy.o: daisy.C daisy.h time.h input.h manager.h weather.h \
 groundwater.h uzmodel.h horizon.h log.h crop.h column.h action.h \
 filter.h library.h syntax.h condition.h alist.h common.h
input.o: input.C input.h log.h alist.h common.h csmp.h rules.h \
 library.h syntax.h action.h condition.h time.h filter.h crop.h
log.o: log.C log.h condition.h time.h filter.h csmp.h daisy.h
weather.o: weather.C weather.h time.h library.h alist.h common.h \
 syntax.h
manager.o: manager.C manager.h library.h alist.h common.h syntax.h
column.o: column.C column.h library.h alist.h common.h syntax.h
crop.o: crop.C crop.h library.h alist.h common.h syntax.h
alist.o: alist.C alist.h common.h action.h condition.h time.h
syntax.o: syntax.C syntax.h alist.h common.h log.h library.h
library.o: library.C library.h alist.h common.h
action.o: action.C action.h column.h alist.h common.h
condition.o: condition.C condition.h time.h
horizon.o: horizon.C horizon.h library.h alist.h common.h syntax.h
ftable.o: ftable.C ftable.h
filter.o: filter.C filter.h
csmp.o: csmp.C csmp.h log.h
rules.o: rules.C rules.h daisy.h time.h action.h
time.o: time.C time.h
uzmodel.o: uzmodel.C uzmodel.h library.h alist.h common.h syntax.h
soil.o: soil.C soil.h horizon.h alist.h common.h syntax.h
mathlib.o: mathlib.C mathlib.h
bioclimate.o: bioclimate.C bioclimate.h surface.h uzmodel.h weather.h \
 time.h crop.h csmp.h alist.h common.h soil.h horizon.h syntax.h \
 snow.h
surface.o: surface.C surface.h uzmodel.h
soil_water.o: soil_water.C soil_water.h log.h alist.h common.h \
 uzmodel.h soil.h horizon.h surface.h groundwater.h time.h syntax.h
soil_NH4.o: soil_NH4.C soil_NH4.h
soil_NO3.o: soil_NO3.C soil_NO3.h
organic_matter.o: organic_matter.C organic_matter.h
nitrification.o: nitrification.C nitrification.h
denitrification.o: denitrification.C denitrification.h
soil_heat.o: soil_heat.C soil_heat.h alist.h common.h bioclimate.h \
 syntax.h
groundwater.o: groundwater.C groundwater.h time.h uzmodel.h library.h \
 alist.h common.h syntax.h
uzrichard.o: uzrichard.C uzrichard.h uzmodel.h soil.h horizon.h \
 mathlib.h alist.h common.h syntax.h
snow.o: snow.C snow.h alist.h common.h syntax.h
column_std.o: column_std.C column.h crop.h bioclimate.h surface.h \
 uzmodel.h soil.h horizon.h soil_water.h soil_heat.h soil_NH4.h \
 soil_NO3.h organic_matter.h nitrification.h denitrification.h alist.h \
 common.h syntax.h library.h log.h filter.h
manager_rule.o: manager_rule.C manager.h syntax.h rules.h alist.h \
 common.h
weather_simple.o: weather_simple.C weather.h time.h syntax.h alist.h \
 common.h
horizon_yolo.o: horizon_yolo.C horizon.h syntax.h alist.h common.h
horizon_M_vG.o: horizon_M_vG.C horizon.h syntax.h alist.h common.h
horizon_B_vG.o: horizon_B_vG.C horizon.h syntax.h alist.h common.h
horizon_M_C.o: horizon_M_C.C horizon.h syntax.h alist.h common.h
horizon_B_C.o: horizon_B_C.C horizon.h syntax.h alist.h common.h
horizon_M_BaC.o: horizon_M_BaC.C horizon.h syntax.h alist.h common.h
horizon_B_BaC.o: horizon_B_BaC.C horizon.h syntax.h alist.h common.h
groundwater_static.o: groundwater_static.C groundwater.h time.h \
 uzmodel.h syntax.h alist.h common.h
crop_std.o: crop_std.C crop.h log.h time.h column.h csmp.h \
 bioclimate.h common.h ftable.h syntax.h alist.h filter.h ftable.t
