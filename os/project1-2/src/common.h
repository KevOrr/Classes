#ifndef _COMMON_H_
#define _COMMON_H_

#include <stdlib.h>
#include <stdio.h>

#define FNUM_BITS 20
#define PAGE_BITS 12
#define exit_error() (error_at_line(errno, errno, __FILE__, __LINE__, argv0))

#define debugf(fmt, ...) do { if (debug) printf(fmt, ##__VA_ARGS__); } while (0)

typedef unsigned long long addr_t;

char *argv0;
int debug;

struct frame {
    char dirty;
    char cached;
};

struct queue {
    addr_t *buffer;
    size_t front;
    size_t length;
    size_t maxlength;
};

struct queue *make_queue(size_t n);
void queue_push(struct queue *q, addr_t value);
addr_t queue_pop(struct queue *q);

enum mem_op {READ, WRITE};

struct frame* make_table(size_t n);

addr_t addr_to_framenum(addr_t addr);

void test_algorithm(const char *tracef_path, addr_t nframes,
                    void (*init)(addr_t),
                    void (*run)(addr_t frame_n, enum mem_op,
                                addr_t *disk_writes, addr_t *disk_reads));

#endif
