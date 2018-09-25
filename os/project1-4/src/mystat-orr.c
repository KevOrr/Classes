#include <sys/stat.h>
#include <stdlib.h>
#include <errno.h>
#include <error.h>
#include <stdio.h>
#include <string.h>

#include "common.h"

#define exit_error() (error_at_line(errno, errno, __FILE__, __LINE__, argv0))

const char *argv0;

void print_info(const char *path) {
    struct stat info;
    char perms[11];

    if (stat(path, &info) != 0)
        exit_error();

    get_perms(info, perms);

    printf("  File: %s\n"
           "  Size: %-10llu\t" "Blocks: %-10llu" "IO Block: %llu\n"
           " Inode: %-15llu Links: %llu\n"
           "Access: (%04llo/%s)\n",
           path,
           (unsigned long long)info.st_size,
           (unsigned long long)info.st_blocks,
           (unsigned long long)info.st_blksize,
           (unsigned long long)info.st_ino,
           (unsigned long long)info.st_nlink,
           (unsigned long long)(info.st_mode & 07777),
           perms);
}

void usage() {
    fprintf(stderr, "USAGE: %s <FILE>\n", argv0);
    exit(1);
}

int main(int argc, char *argv[]) {
    argv0 = argv[0];

    if (argc != 2)
        usage();

    if (strcmp(argv[1], "-h") == 0 || strcmp(argv[1], "--help") == 0)
        usage();

    print_info(argv[1]);
}
