# Makefile -- DAISY 

SHELL = /bin/sh
CC = c++ -Wall -g # -fhandle-exceptions -pipe -fno-implicit-templates
OBJ = main.o daisy.o input.o log.o bioclimate.o manager.o column.o crop.o \
	alist.o syntax.o library.o action.o condition.o horizon.o ftable.o \
	crop_impl.o template.o filter.o csmp.o rules.o time.o
SRC = $(OBJ:.o=.C)
HEAD = $(OBJ:.o=.h)
TEXT = ftable.t $(SRC) $(HEAD) Makefile 

.SUFFIXES:	.C .o .h

daisy:	$(OBJ)
	$(CC) -o daisy $(OBJ) -lm

bug: bug.C
	$(CC) -v -c bug.C

wc: $(TEXT)
	wc -l $(TEXT)

tags: TAGS

TAGS: $(SRC) $(HEAD)
	etags $(SRC) $(HEAD)

print:
	mp -p /home/user_13/fischer/bin/mp.pro.none -a4 $(TEXT) | parr -s | up -n pup | lpr -Pduplex

test:	daisy
	daisy test.dai

clean:
	rm $(OBJ) daisy *~

depend: $(SRC) 
	rm -f Makefile.old
	mv Makefile Makefile.old
	sed -e '/^# AUTOMATIC/q' < Makefile.old > Makefile
	g++ -I. -MM $(SRC) >> Makefile

cvs: $(TEXT)
	@if [ "X$(TAG)" = "X" ]; then echo "*** No tag ***"; exit 1; fi
	-cvs add $(TEXT)
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
main.o: main.C daisy.h time.h input.h column.h
daisy.o: daisy.C daisy.h time.h input.h column.h manager.h \
 bioclimate.h log.h action.h filter.h library.h syntax.h condition.h
input.o: input.C input.h column.h daisy.h time.h manager.h \
 bioclimate.h log.h horizon.h crop.h alist.h csmp.h rules.h library.h \
 syntax.h action.h condition.h filter.h
log.o: log.C log.h condition.h daisy.h time.h filter.h
bioclimate.o: bioclimate.C bioclimate.h time.h syntax.h alist.h \
 daisy.h
manager.o: manager.C manager.h daisy.h time.h syntax.h rules.h alist.h
column.o: column.C column.h daisy.h time.h crop.h syntax.h log.h \
 filter.h library.h bioclimate.h crop_impl.h ftable.h alist.h csmp.h
crop.o: crop.C crop_impl.h crop.h daisy.h time.h ftable.h log.h \
 column.h csmp.h bioclimate.h
alist.o: alist.C alist.h daisy.h time.h action.h condition.h
syntax.o: syntax.C syntax.h alist.h daisy.h time.h
library.o: library.C library.h alist.h daisy.h time.h syntax.h
action.o: action.C action.h daisy.h time.h column.h
condition.o: condition.C condition.h daisy.h time.h
horizon.o: horizon.C horizon.h daisy.h time.h syntax.h
ftable.o: ftable.C ftable.h
crop_impl.o: crop_impl.C crop_impl.h crop.h daisy.h time.h ftable.h \
 syntax.h alist.h csmp.h filter.h log.h bioclimate.h
template.o: template.C ftable.h ftable.t crop_impl.h crop.h daisy.h \
 time.h
filter.o: filter.C filter.h
csmp.o: csmp.C csmp.h
rules.o: rules.C rules.h daisy.h time.h action.h
time.o: time.C time.h
