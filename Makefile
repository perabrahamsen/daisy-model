
CC = c++ -Wall
OBJ = main.o daisy.o input.o log.o wheather.o manager.o column.o crop.o
SRC = $(OBJ:.o=.C)
TEXT = $(SRC) daisy.h Makefile

.SUFFIXES:	.C .o

daisy:	$(OBJ)
	$(CC) -o daisy $(OBJ)

wc:
	wc $(SRC)

print:
	mp -p /home/user_13/fischer/bin/mp.pro.none -a4 $(TEXT) | parr -s | up -n pup | lpr -Pduplex


test:	daisy
	daisy

clean:
	rm $(OBJ) daisy *~

.C.o:
	$(CC) -c $<

$(OBJ):	daisy.h
