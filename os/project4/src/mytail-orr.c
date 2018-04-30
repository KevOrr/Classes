#include <sys/mman.h>
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <fcntl.h>

#include "common.h"

char *argv0;

void usage() {
    fprintf(stderr, "USAGE: %s -n <NUM> <FILE>", argv0);
    exit(1);
}

void print_tail(const char *mapped, size_t size, unsigned long lines) {
    const char *cur = mapped + (size - 1);

    while (lines > 0 && cur > mapped) {
        cur--;
        if (*cur == '\n') {
            lines--;
        }
    }

    cur++;

    fwrite(cur, sizeof(char), mapped + (size - 1) - cur + 1, stdout);
}

int main(int argc, char *argv[]) {
    argv0 = argv[0];

    if (argc != 4)
        usage();

    errno = 0;
    unsigned long n = strtoul(argv[2], NULL, 10);
    if (errno != 0)
        exit_error();

    struct stat info;
    if (stat(argv[3], &info) != 0)
        exit_error();

    int fd = open(argv[3], O_RDONLY);
    if (fd == -1)
        exit_error();

    char *mapped_file = mmap(NULL, info.st_size, PROT_READ, MAP_SHARED, fd, 0);
    if (mapped_file == MAP_FAILED)
        exit_error();

    print_tail(mapped_file, info.st_size, n);
}
