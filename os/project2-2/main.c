#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <error.h>
#include <errno.h>
#include <semaphore.h>
#include <sys/mman.h>
#include <sys/wait.h>

#define EXITERROR() error_at_line(errno, errno, __FILE__, __LINE__, "pid %llu", (long long unsigned)getpid())

static struct {
    unsigned long total;
    sem_t mutex;
} *shared;

static void task(int task_num, unsigned int arg) {
    sem_wait(&shared->mutex);
    for (unsigned int i=0; i<arg; i++)
        shared->total++;
    sem_post(&shared->mutex);
    printf("From child %d: counter = %lu\n", task_num, shared->total);
}

struct {
    void (*func)(int task_num, unsigned int arg);
    unsigned int arg;
    pid_t pid;
} tasks[] = {
    {.func = task, .arg = 100000},
    {.func = task, .arg = 200000},
    {.func = task, .arg = 300000},
    {.func = task, .arg = 500000},
};

int main() {
    shared = mmap(NULL, sizeof(shared), PROT_READ|PROT_WRITE, MAP_SHARED|MAP_ANONYMOUS, -1, 0);
    shared->total = 0;
    sem_init(&shared->mutex, 1, 1);

    for (size_t i = 0; i < sizeof(tasks)/sizeof(tasks[0]); i++) {
        pid_t pid = fork();
        if (pid < 0)
            EXITERROR();
        else if (pid > 0)
            tasks[i].pid = pid;
        else {
            tasks[i].func(i+1, tasks[i].arg);
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

    sem_destroy(&shared->mutex);
}
