CC := gcc
CFLAGS := -O0 -Wall -g

all: mystat-orr myls-orr mytail-orr mysearch-orr

mystat-orr: src/mystat-orr.c build/common.o
	$(CC) $(CFLAGS) $^ -o $@

myls-orr: src/myls-orr.c build/common.o
	$(CC) $(CFLAGS) $^ -o $@

mytail-orr: src/mytail-orr.c build/common.o
	$(CC) $(CFLAGS) $^ -o $@

mysearch-orr: src/mysearch-orr.c build/common.o
	$(CC) $(CFLAGS) $^ -o $@

build/common.o: src/common.c src/common.h build
	$(CC) $(CFLAGS) -c $< -o $@

build:
	mkdir build

clean:
	rm -rf build mystat-orr myls-orr mytail-orr mysearch-orr

.PHONY: clean all
