CC := gcc
CFLAGS := -m64 -O5 -std=gnu11 -Wall
CFLAGS += -D OPTIMIZE_IO_SYSCALLS

make: system_call context_switch

runsc: system_call
	./system_call

runcs: context_switch
	./context_switch

clean:
	rm -f system_call context_switch

.PHONY: make runsc runcs clean
