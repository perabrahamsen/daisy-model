# Makefile -- DAISY 

SHELL = /bin/sh
CC = c++ -Wall -g # -fhandle-exceptions -pipe -fno-implicit-templates
OBJ = main.o daisy.o input.o log.o wheather.o manager.o column.o crop.o \
	alist.o syntax.o library.o action.o condition.o horizon.o ftable.o \
	crop_impl.o template.o filter.o csmp.o rules.o
SRC = $(OBJ:.o=.C)
HEAD = $(OBJ:.o=.h)
TEXT = ftable.t $(SRC) $(HEAD) Makefile 

.SUFFIXES:	.C .o .h

daisy:	$(OBJ)
	$(CC) -o daisy $(OBJ)

bug: bug.C
	$(CC) -v -c bug.C

wc: $(TEXT)
	wc $(TEXT)

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

############################################################
# AUTOMATIC -- DO NOT CHANGE THIS LINE OR ANYTHING BELOW IT!
main.o: main.C daisy.h input.h column.h
daisy.o: daisy.C daisy.h input.h column.h manager.h wheather.h log.h \
 action.h filter.h library.h syntax.h condition.h
input.o: input.C input.h column.h daisy.h manager.h wheather.h log.h \
 horizon.h crop.h alist.h csmp.h rules.h library.h syntax.h action.h \
 condition.h filter.h
log.o: log.C log.h condition.h daisy.h filter.h
wheather.o: wheather.C wheather.h daisy.h
manager.o: manager.C manager.h daisy.h syntax.h rules.h alist.h
column.o: column.C column.h daisy.h crop.h syntax.h log.h filter.h \
 library.h
crop.o: crop.C crop_impl.h crop.h daisy.h ftable.h log.h
alist.o: alist.C alist.h daisy.h action.h condition.h
syntax.o: syntax.C syntax.h alist.h daisy.h
library.o: library.C library.h alist.h daisy.h syntax.h
action.o: action.C action.h daisy.h column.h
condition.o: condition.C condition.h daisy.h
horizon.o: horizon.C horizon.h daisy.h syntax.h
ftable.o: ftable.C ftable.h
crop_impl.o: crop_impl.C crop_impl.h crop.h daisy.h ftable.h syntax.h \
 alist.h csmp.h filter.h log.h
template.o: template.C ftable.h ftable.t crop_impl.h crop.h daisy.h
filter.o: filter.C filter.h
csmp.o: csmp.C csmp.h
rules.o: rules.C rules.h daisy.h action.h
