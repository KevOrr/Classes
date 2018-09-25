#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <locale.h>
#include <wchar.h>
#include <string.h>

#include "common.h"

const wchar_t *ENTRY = L"├── ";
const wchar_t *SEP = L"│   ";
const wchar_t *LAST_CHILD = L"└── ";

char *argv0;

void usage() {
    fprintf(stderr, "USAGE: %s [DIR]", argv0);
    exit(1);
}

void print_sep(size_t depth) {
    for (; depth > 0; depth--)
        wprintf(L"%ls", SEP);
}

void *print_tree(struct fold_dir_arg arg) {
    print_sep(*(size_t*)arg.acc); //depth

    if (arg.entry_pos <= 0)
        wprintf(L"%ls", ENTRY);
    else
        wprintf(L"%ls", LAST_CHILD);

    wprintf(L"%s\n", arg.basename);

    struct stat info;
    if (stat(arg.relpath, &info) != 0)
        exit_error();
    if (S_ISDIR(info.st_mode)) {
        size_t newarg = *(size_t*)arg.acc + 1;
        fold_dir(arg.relpath, 1, print_tree, &newarg);
    }

    return arg.acc;
}

int main(int argc, char *argv[]) {
    argv0 = argv[0];

    setlocale(LC_ALL, "");

    char *path = NULL;
    if (argc == 2) {
        struct stat info;
        if (stat(argv[1], &info) != 0)
            exit_error();
        path = argv[1];
        puts(argv[1]);
    } else {
        wprintf(L"%s\n", ".");
    }

    size_t depth = 0;
    fold_dir(path, 1, print_tree, &depth);
}
