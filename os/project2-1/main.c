#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <error.h>
#include <errno.h>
#include <sys/wait.h>
#include <sys/mman.h>

static unsigned long *total;

#define EXITERROR() error_at_line(errno, errno, __FILE__, __LINE__, "pid %llu", (long long unsigned)getpid())

static void process1() {
    for (unsigned int i=0; i<100000; i++)
        (*total)++;
    printf("From child 1: counter = %lu\n", *total);
}

static void process2() {
    for (unsigned int i=0; i<200000; i++)
        (*total)++;
    printf("From child 2: counter = %lu\n", *total);
}

static void process3() {
    for (unsigned int i=0; i<300000; i++)
        (*total)++;
    printf("From child 3: counter = %lu\n", *total);
}

static void process4() {
    for (unsigned int i=0; i<500000; i++)
        (*total)++;
    printf("From child 4: counter = %lu\n", *total);
}

struct {
    void (*func)();
    pid_t pid;
} tasks[] = {
    {.func = process1},
    {.func = process2},
    {.func = process3},
    {.func = process4}
};

int main() {
    total = mmap(NULL, sizeof(*total), PROT_READ|PROT_WRITE, MAP_SHARED|MAP_ANONYMOUS, -1, 0);

    for (size_t i = 0; i < sizeof(tasks)/sizeof(tasks[0]); i++) {
        pid_t pid = fork();
        if (pid < 0)
            EXITERROR();
        else if (pid > 0)
            tasks[i].pid = pid;
        else {
            tasks[i].func();
            return 0;
        }
    }

    for (size_t i = 0; i < sizeof(tasks)/sizeof(tasks[0]); i++) {
        pid_t pid = waitpid(tasks[i].pid, NULL, 0);
        if (pid < 0)
            EXITERROR();
        printf("Child %llu with pid %llu has just exited\n", (long long unsigned)(i+1), (long long unsigned)pid);
    }

    puts("End of simulation");
}
