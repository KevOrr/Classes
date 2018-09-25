CC := gcc
CFLAGS := -g3 -O0 -std=gnu11 -Wall

SOURCES := $(wildcard src/*.c)
OBJECTS := $(patsubst src/%.c, build/%.o, $(SOURCES))


memsim: build $(OBJECTS)
	$(CC) $(CFLAGS) $(filter %.o,$^) -o $@

build:
	mkdir build

$(OBJECTS): build/%.o : src/%.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -rf build memsim

.PHONY: clean
