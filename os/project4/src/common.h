#ifndef _COMMON_H_
#define _COMMON_H_

#include <sys/stat.h>
#include <error.h>
#include <errno.h>

#define exit_error() (error_at_line(errno, errno, __FILE__, __LINE__, argv0))

struct fold_dir_arg {
    const char *basename;
    const char *dirname;
    const char *relpath;
    int entry_pos;
    void *acc;
};

void get_perms(struct stat info, char perms[11]);

void fold_dir(const char *path, int ignore_special, void* fp(struct fold_dir_arg), void *acc);

char *agetcwd();

#endif
