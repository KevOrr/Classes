#include <stdlib.h>

#include "vms.h"
#include "common.h"

static struct frame *ptable;
static struct queue *cleanq;
static struct queue *dirtyq;
static struct queue *procsq[2];

static void vms_init(addr_t n) {
    ptable = make_table(1 << FNUM_BITS);
    cleanq = make_queue(n);
    dirtyq = make_queue(n);
    procsq[0] = make_queue(n);
    procsq[1] = make_queue(n);
}

static void vms_run(addr_t addr, enum mem_op oper,
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

void vms(const char *tracef_path, addr_t nframes) {
    debugf("VMS init\n");
    test_algorithm(tracef_path, nframes, vms_init, vms_run);
}
