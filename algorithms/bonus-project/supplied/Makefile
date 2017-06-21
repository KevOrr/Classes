CC=g++
CFLAGS=-std=c++11
INC=
LIB=

all:  dksort

dksort:  driver.o dunningkruger.o sort.o
	$(CC) $(CFLAGS) -o $@ $^ $(LIB)

.cpp.o:
	$(CC) $(CFLAGS) $(INC) -c -o $@ $^ 

clean:
	rm -f *.o dksort
