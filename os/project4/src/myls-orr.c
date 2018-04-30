#define _GNU_SOURCE
#include <sys/stat.h>
#include <dirent.h>
#include <stdlib.h>
#include <stdio.h>
#include <pwd.h>
#include <grp.h>
#include <time.h>
#include <string.h>

#include "common.h"

const unsigned int MAX_WIDTH = 80;

const char *argv0;

// Adapted from http://programanddesign.com/cpp/human-readable-file-size-in-c/
char* readable_fs(double size/*in bytes*/) {
    const char units[] = "BKMGTPEZY";
    unsigned int len = strlen(units);

    unsigned int i;
    for (i=0; size > 1024 && i < len - 1; i++) {
        size /= 1024;
    }

    char* res;
    if (i > 0) {
        asprintf(&res, "%6.1f%c", size, units[i]);
    } else {
        asprintf(&res, "%6u%c", (unsigned)size, units[i]);
    }
    return res;
}

void *print_long_info(struct fold_dir_arg arg) {
    struct stat info;
    if (lstat(arg.relpath, &info) != 0)
        exit_error();

    char perms[11];
    get_perms(info, perms);

    struct passwd *user = getpwuid(info.st_uid);
    struct group *group = getgrgid(info.st_gid);

    char *size = readable_fs(info.st_size);

    char time[20];
    strftime(time, sizeof(time), "%b %d %Y %H:%M", localtime(&info.st_mtim.tv_sec));

    printf("%s %llu %s %s %s %s %s\n",
           perms, (unsigned long long)info.st_nlink,
           user->pw_name, group->gr_name,
           size, time, arg.basename);

    free(size);

    return NULL;
}

void *print_short_info(struct fold_dir_arg arg) {
    size_t len = strlen(arg.basename);
    if (len + 1 <= *(size_t*)arg.acc) {
        printf("%s ", arg.basename);
        *(size_t*)arg.acc -= len + 1;
        return arg.acc;
    } else {
        printf("\n%s ", arg.basename);
        *(size_t*)arg.acc = MAX_WIDTH - 1 - len;
        return arg.acc;
    }
}

void usage() {
    fprintf(stderr, "USAGE: %s [-l | --] [FILE]...\n", argv0);
    exit(1);
}

int main(int argc, char *argv[]) {
    argv0 = argv[0];
    argv++; argc--;

    int long_format = 0;
    if (argc > 0) {
        if (strcmp(argv[0], "-h") == 0 || strcmp(argv[0], "--help") == 0) {
            usage();
        } else if (strcmp(argv[0], "--") == 0) {
            argv++; argc--;
        } else if (strcmp(argv[0], "-l") == 0) {
            long_format = 1;
            argv++; argc--;
        }
    }

    char *path = (argc == 0) ? NULL : argv[0];
    char *actualpath = (path == NULL) ? agetcwd() : path;

    struct stat info;
    if (stat(actualpath, &info) != 0)
        exit_error();

    if (long_format) {
        if (S_ISDIR(info.st_mode))
            fold_dir(path, 0, print_long_info, NULL);
        else {
            struct fold_dir_arg arg;
            arg.basename = arg.relpath = path;
            print_long_info(arg);
        }
    } else {
        if (S_ISDIR(info.st_mode)) {
            size_t acc = MAX_WIDTH;
            fold_dir(path, 0, print_short_info, &acc);
            puts("");
        } else {
            puts(path);
        }
    }
}
