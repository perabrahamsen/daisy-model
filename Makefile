# Makefile -- DAISY 

SHELL = /bin/sh
CC = /pack/gcc-2.7.1/bin/c++ -Wall -g -frepo # -O2 -fhandle-exceptions -pipe -fno-implicit-templates
OBJ = main.o daisy.o input.o log.o weather.o manager.o column.o crop.o \
	alist.o syntax.o library.o action.o condition.o horizon.o ftable.o \
	crop_impl.o filter.o csmp.o rules.o time.o column_std.o uzmodel.o \
	soil.o mathlib.o template.o bioclimate.o surface.o soil_water.o \
	soil_NH4.o soil_NO3.o organic_matter.o nitrification.o \
	denitrification.o soil_heat.o groundwater.o
SRC = $(OBJ:.o=.C)
HEAD = $(OBJ:.o=.h)
TEST = crop.dai old_crop.chp old_crop.log water.dai old_water.log
TEXT =  Makefile $(HEAD) $(SRC) ftable.t

.SUFFIXES:	.C .o .h

daisy:	$(OBJ)
	$(CC) -o daisy $(OBJ) -lm

bug: bug.o
	$(CC) -v -o bug bug.o
	bug

bug.o:	bug.C
	$(CC) -c bug.C

wc: $(TEXT)
	wc -l $(TEXT)

tags: TAGS

TAGS: $(SRC) $(HEAD)
	etags $(SRC) $(HEAD)

print:
	mp -p /home/user_13/fischer/bin/mp.pro.none -a4 $(TEXT) | parr -s | up -n pup | lpr -Pduplex

crop-test:	daisy
	daisy crop.dai
	diff old_crop.chp crop.chp
	diff old_crop.log crop.log

water-test:	daisy
	daisy water.dai
	diff old_water.log water.log

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
	cvs commit -m "$(TAG)" # "Version $(TAG)"
.C.o:
	$(CC) -c $<

$(OBJ):	daisy.h

pcrop.c: crop.pas
	../ftp/p2c-1.20/p2c -a $<
	mv crop.c pcrop.c

pcrop: pcrop.c
	gcc -L../ftp/p2c-1.20/home -I../ftp/p2c-1.20/home pcrop.c -g -o pcrop -lm -lp2c

ptest: pcrop
	pcrop < /dev/null

############################################################
# AUTOMATIC -- DO NOT CHANGE THIS LINE OR ANYTHING BELOW IT!
main.o: main.C daisy.h time.h input.h
daisy.o: daisy.C daisy.h time.h input.h manager.h weather.h log.h \
 column.h action.h filter.h library.h syntax.h condition.h
input.o: input.C input.h daisy.h time.h manager.h weather.h \
 groundwater.h uzmodel.h log.h horizon.h column_std.h column.h \
 bioclimate.h surface.h soil.h soil_water.h soil_heat.h soil_NH4.h \
 soil_NO3.h organic_matter.h nitrification.h denitrification.h crop.h \
 alist.h csmp.h rules.h library.h syntax.h action.h condition.h \
 filter.h
log.o: log.C log.h condition.h daisy.h time.h filter.h csmp.h
weather.o: weather.C weather.h time.h syntax.h alist.h daisy.h
manager.o: manager.C manager.h daisy.h time.h syntax.h rules.h alist.h
column.o: column.C column.h
crop.o: crop.C crop_impl.h crop.h daisy.h time.h ftable.h csmp.h log.h \
 column.h bioclimate.h
alist.o: alist.C alist.h daisy.h time.h action.h condition.h
syntax.o: syntax.C syntax.h alist.h daisy.h time.h
library.o: library.C library.h alist.h daisy.h time.h syntax.h
action.o: action.C action.h daisy.h time.h column.h
condition.o: condition.C condition.h daisy.h time.h
horizon.o: horizon.C horizon.h daisy.h time.h syntax.h alist.h
ftable.o: ftable.C ftable.h
crop_impl.o: crop_impl.C crop_impl.h crop.h daisy.h time.h ftable.h \
 csmp.h syntax.h alist.h filter.h log.h bioclimate.h
filter.o: filter.C filter.h
csmp.o: csmp.C csmp.h log.h
rules.o: rules.C rules.h daisy.h time.h action.h
time.o: time.C time.h
column_std.o: column_std.C column_std.h daisy.h time.h column.h \
 bioclimate.h surface.h uzmodel.h soil.h horizon.h soil_water.h \
 soil_heat.h soil_NH4.h soil_NO3.h organic_matter.h nitrification.h \
 denitrification.h alist.h syntax.h library.h log.h filter.h crop.h
uzmodel.o: uzmodel.C uzmodel.h soil.h horizon.h daisy.h time.h \
 mathlib.h
soil.o: soil.C soil.h horizon.h daisy.h time.h alist.h
mathlib.o: mathlib.C mathlib.h
template.o: template.C ftable.h crop_impl.h crop.h daisy.h time.h \
 csmp.h ftable.t
bioclimate.o: bioclimate.C bioclimate.h daisy.h time.h surface.h \
 uzmodel.h weather.h crop.h csmp.h alist.h
surface.o: surface.C surface.h uzmodel.h
soil_water.o: soil_water.C soil_water.h log.h alist.h daisy.h time.h \
 uzmodel.h soil.h horizon.h surface.h groundwater.h syntax.h
soil_NH4.o: soil_NH4.C soil_NH4.h
soil_NO3.o: soil_NO3.C soil_NO3.h
organic_matter.o: organic_matter.C organic_matter.h
nitrification.o: nitrification.C nitrification.h
denitrification.o: denitrification.C denitrification.h
soil_heat.o: soil_heat.C soil_heat.h daisy.h time.h alist.h \
 bioclimate.h
groundwater.o: groundwater.C groundwater.h uzmodel.h syntax.h
