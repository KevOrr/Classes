#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#include "lru.h"
#include "fifo.h"
#include "common.h"

static void usage() {
    fprintf(stderr, "USAGE: %s <TRACEFILE> <NFRAMES> { lru | fifo | vms } { debug | quiet }\n", argv0);
    exit(1);
}

int main(int argc, char *argv[]) {
    argv0 = argv[0];

    if (argc != 5)
        usage();

    if (strcasecmp(argv[1], "--help") == 0 || strcasecmp(argv[1], "-h") == 0)
        usage();

    for (const char *c=argv[2]; *c; c++)
        if (!isdigit(*c))
            usage();

    addr_t nframes = atoll(argv[2]);

    if (strcasecmp(argv[4], "debug") == 0)
        debug = 1;
    else if (strcasecmp(argv[4], "quiet") == 0)
        debug = 0;
    else
        usage();

    if (strcasecmp(argv[3], "lru") == 0)
        lru(argv[1], nframes);
    else if (strcasecmp(argv[3], "fifo") == 0)
        fifo(argv[1], nframes);
    else if (strcasecmp(argv[3], "lru") == 0)
        lru(argv[1], nframes);
    else
        usage();


}
