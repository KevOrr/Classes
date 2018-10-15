CC := gcc
CFLAGS := -std=gnu11 -lpthread

main: main.c
	$(CC) $(CFLAGS) $^ -o $@

.PHONY: clean
clean:
	rm -rf main
