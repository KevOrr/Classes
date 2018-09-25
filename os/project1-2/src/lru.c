#include <stdlib.h>

#include "lru.h"
#include "common.h"

struct rulist {
    addr_t *front;
    size_t length;
    size_t maxlength;
};

static struct frame *ptable;
static struct rulist *rulist;

static struct rulist *make_rulist(size_t maxlength) {
    struct rulist *list = malloc(sizeof(struct rulist));
    list->front = malloc(maxlength * sizeof(struct rulist));
    list->length = 0;
    list->maxlength = maxlength;

    return list;
}

static void lru_init(addr_t n) {
    ptable = make_table(1 << FNUM_BITS);
    /* rulist = make_dll(); */
    rulist = make_rulist(n);
}

static void rulist_move_front_helper(addr_t *rest, size_t togo, addr_t framenum) {
    if (togo > 0) {
        rulist_move_front_helper(rest+1, togo-1, framenum);
        if (*rest == framenum) {
            *rest = *(rest-1);
            *(rest-1) = framenum;
        }
    }
}

static void rulist_move_front(struct rulist *list, addr_t framenum) {
    if (list->front[0] != framenum)
        rulist_move_front_helper(list->front + 1, list->length - 1, framenum);
}

static void rulist_push_front_make_room(addr_t *rest, size_t togo) {
    if (togo > 0) {
        rulist_push_front_make_room(rest+1, togo-1);
        *(rest+1) = *rest;
    }
}

static void rulist_push_front(struct rulist *list, addr_t framenum) {
    rulist_push_front_make_room(list->front, list->length);
    list->front[0] = framenum;
    list->length++;
}

static addr_t rulist_pop_back(struct rulist *list) {
    if (list->length) {
        list->length--;
        return list->front[list->length];
    } else {
        return -1;
    }
}

static void lru_run(addr_t addr, enum mem_op oper,
             addr_t *disk_writes, addr_t *disk_reads) {

    addr_t framenum = addr_to_framenum(addr);
    debugf("%05llx %c ", framenum, oper == READ ? 'R' : 'W');

    if (ptable[framenum].cached) {
        // Already in memory, mark dirty if READ, no disk accesses
        debugf("hit\n");

        rulist_move_front(rulist, framenum);
        if (oper == WRITE)
            ptable[framenum].dirty = 1;

    } else if (rulist->length < rulist->maxlength) {
        // Not in memory, but we have room for a new page
        debugf("miss, just load\n");

        rulist_push_front(rulist, framenum);
        (*disk_reads)++;
        ptable[framenum].cached = 1;

    } else {
        // No room in memory, evict LRU page and fetch new page
        addr_t victim_framenum = rulist_pop_back(rulist);
        if (ptable[victim_framenum].dirty) {
            debugf("miss, write and evict %05llx\n", victim_framenum);
            (*disk_writes)++;
        } else {
            debugf("miss, evict %05llx\n", victim_framenum);
        }

        ptable[victim_framenum].dirty = 0;
        ptable[victim_framenum].cached = 0;

        rulist_push_front(rulist, framenum);
        (*disk_reads)++;
        ptable[framenum].cached = 1;
    }
}

void lru(const char *tracef_path, addr_t nframes) {
    debugf("LRU init\n");
    test_algorithm(tracef_path, nframes, lru_init, lru_run);
}
