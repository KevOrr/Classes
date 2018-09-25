#define _GNU_SOURCE
#include <sys/stat.h>
#include <dirent.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>

#include "common.h"

extern char *argv0;

void get_perms(struct stat info, char perms[11]) {
    if (S_ISREG(info.st_mode))
        perms[0] = '-';
    else if (S_ISDIR(info.st_mode))
        perms[0] = 'd';
    else if (S_ISBLK(info.st_mode))
        perms[0] = 'b';
    else if (S_ISCHR(info.st_mode))
        perms[0] = 'c';
    else if (S_ISLNK(info.st_mode))
        perms[0] = 'l';
    else if (S_ISSOCK(info.st_mode))
        perms[0] = 's';
    else
        perms[0] = '?';

    perms[1] = info.st_mode & S_IRUSR ? 'r' : '-';
    perms[2] = info.st_mode & S_IWUSR ? 'w' : '-';
    switch(((!!(info.st_mode & S_IXUSR)) << 1) | (!!(info.st_mode & S_ISUID))) {
    case (0b00): perms[3] = '-'; break;
    case (0b01): perms[3] = 'S'; break;
    case (0b10): perms[3] = 'x'; break;
    case (0b11): perms[3] = 's'; break;
    }

    perms[4] = info.st_mode & S_IRGRP ? 'r' : '-';
    perms[5] = info.st_mode & S_IWGRP ? 'w' : '-';
    switch(((!!(info.st_mode & S_IXGRP)) << 1) | (!!(info.st_mode & S_ISGID))) {
    case (0b00): perms[6] = '-'; break;
    case (0b01): perms[6] = 'l'; break;
    case (0b10): perms[6] = 'x'; break;
    case (0b11): perms[6] = 's'; break;
    }

    perms[7] = info.st_mode & S_IROTH ? 'r' : '-';
    perms[8] = info.st_mode & S_IWOTH ? 'w' : '-';
    // sticky bit
    switch(((!!(info.st_mode & S_IXOTH)) << 1) | (!!(info.st_mode & S_ISVTX))) {
    case (0b00): perms[9] = '-'; break;
    case (0b01): perms[9] = 'T'; break;
    case (0b10): perms[9] = 'x'; break;
    case (0b11): perms[9] = 't'; break;
    }

    perms[10] = '\0';
}

// NULL means current directory
void fold_dir(const char *path, int ignore_special, void* fp(struct fold_dir_arg), void *acc) {
    int curdir = path == NULL;
    char *cwd = agetcwd();

    struct fold_dir_arg arg;
    arg.dirname = curdir ? cwd : path;
    arg.acc = acc;
    arg.entry_pos = -1;

    DIR *dir = opendir(arg.dirname);

    errno = 0;
    struct dirent *cur = readdir(dir);
    if (errno != 0)
        exit_error();
    while (ignore_special && cur != NULL && strchr(cur->d_name, '.') == cur->d_name) {
        cur = readdir(dir);
        if (errno != 0)
            exit_error();
    }

    char *buf = NULL;
    while(cur) {
        errno = 0;
        struct dirent *next = readdir(dir);
        if (errno != 0)
            exit_error();

        while (ignore_special && next != NULL && strchr(next->d_name, '.') == next->d_name) {
            next = readdir(dir);
            if (errno != 0)
                exit_error();
        }

        if (curdir) {
            buf = cur->d_name;
        } else {
            free(buf);
            asprintf(&buf, "%s/%s", path, cur->d_name);
        }

        arg.basename = cur->d_name;
        arg.relpath = buf;
        arg.entry_pos = (next == NULL) ? 1 : 0;

        arg.acc = fp(arg);

        cur = next;
    }

    if (curdir)
        free(cwd);

}

char *agetcwd() {
    size_t buflen = 1024;
    char *buf = NULL;

    while (1) {
        free(buf);
        buf = malloc(buflen);
        if (buf == NULL)
            exit_error();

        if(getcwd(buf, buflen) == NULL) {
            if (errno = ERANGE) {
                buflen *= 2;
                continue;
            } else {
                exit_error();
            }
        } else {
            return buf;
        }
    }
}
