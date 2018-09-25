#include <stdlib.h>

#include "fifo.h"
#include "common.h"

static struct frame *ptable;
static struct queue *queue;

static void fifo_init(addr_t n) {
    ptable = make_table(1 << FNUM_BITS);
    queue = malloc(sizeof(struct queue));
    queue->buffer = malloc(n * sizeof(addr_t));
    queue->front = 0;
    queue->length = 0;
    queue->maxlength = n;
}

static void fifo_run(addr_t addr, enum mem_op oper,
             addr_t *disk_writes, addr_t *disk_reads) {

    addr_t framenum = addr_to_framenum(addr);
    debugf("%05llx %c ", framenum, oper == READ ? 'R' : 'W');

    if (ptable[framenum].cached) {
        // Already in memory, mark dirty if READ, no disk accesses
        debugf("hit\n");
        if (oper == WRITE)
            ptable[framenum].dirty = 1;

    } else if (queue->length < queue->maxlength) {
        // Not in memory, but we have room for a new page
        debugf("miss, just load\n");

        queue_push(queue, framenum);
        (*disk_reads)++;
        ptable[framenum].cached = 1;

    } else {
        // No room in memory, evict first-accessed page and fetch new page
        addr_t victim_framenum = queue_pop(queue);
        if (ptable[victim_framenum].dirty) {
            debugf("miss, write and evict %05llx\n", victim_framenum);
            (*disk_writes)++;
        } else {
            debugf("miss, evict %05llx\n", victim_framenum);
        }

        ptable[victim_framenum].dirty = 0;
        ptable[victim_framenum].cached = 0;

        queue_push(queue, framenum);
        (*disk_reads)++;
        ptable[framenum].cached = 1;
    }
}

void fifo(const char *tracef_path, addr_t nframes) {
    debugf("FIFO init\n");
    test_algorithm(tracef_path, nframes, fifo_init, fifo_run);
}
