#define _GNU_SOURCE
#include <unistd.h>
#include <stdint.h>
#include <errno.h>
#include <error.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <signal.h>
#include <sched.h>

#define MILLION 1000000
unsigned long N = MILLION;

int mosi[2]; // parent -> child pipe
int miso[2]; // child -> parent pipe

char * argv0; // name of program

void exiterror(unsigned int lineno) {
    error_at_line(errno, errno, __FILE__, lineno, argv0);
}

/* If OPTIMIZE_IO_SYSCALLS is defined, use inline assembly to invoke
   system call through linux's entry point through the `syscall`
   instruction. Only works on x86-64 */
inline void optim_read(int64_t fd, char *buf, uint64_t count) {
#if defined(OPTIMIZE_IO_SYSCALLS) && defined(__x86_64__)
    asm ("xor %%rax, %%rax\n\t"
         "mov %1, %%rbx\n\t"
         "mov %0, %%rcx\n\t"
         "mov %2, %%rdx\n\t"
         "syscall\n\t"

         : "+rm" (*buf)
         : "0rm" (buf), "irm" (fd), "irm" (count)
         : "rax", "ebx", "rcx", "rdx");

#elif defined(OPTIMIZE_IO_SYSCALLS)
#exiterror "OPTIMIZE_IO_SYSCALLS set, but architecture isn't supported."

#else
    read(fd, buf, count);
#endif
}

inline void optim_write(int64_t fd, char *buf, uint64_t count) {
#if defined(OPTIMIZE_IO_SYSCALLS) && defined(__x86_64__)
    asm ("mov $1, %%rax\n\t"
         "mov %1, %%rbx\n\t"
         "mov %0, %%rcx\n\t"
         "mov %2, %%rdx\n\t"
         "syscall\n\t"

         :
         : "irm" (buf), "irm" (fd), "irm" (count)
         : "rax", "ebx", "rcx", "rdx");

#elif defined(OPTIMIZE_IO_SYSCALLS)
#exiterror "OPTIMIZE_IO_SYSCALLS set, but architecture isn't supported."

#else
    write(fd, buf, count);
#endif
}

void parent(unsigned long long cpuid) {
    char output = 'a';
    char input;

    int outfd = mosi[1]; // write side of parent -> child pipe
    int infd = miso[0]; // read side of child -> parent pipe

    // Close fds that we don't use in this process
    close(mosi[0]);
    close(miso[1]);

    struct timeval start, end;
    gettimeofday(&start, NULL);

    // Begin parent side of pipe read/write loop
    for (unsigned long i=0; i<N; i++) {
        optim_write(outfd, &output, 1);
        optim_read(infd, &input, 1);
    }

    gettimeofday(&end, NULL);
    double delta = (end.tv_usec - start.tv_usec) / (double)MILLION + (end.tv_sec - start.tv_sec);
    printf("%ld million context switches on cpu %llu took %lf seconds.\n", 2*N/MILLION, cpuid, delta);
}

void child() {
    char output = 'b';
    char input;

    int outfd = miso[1]; // write side of child -> parent pipe
    int infd = mosi[0]; // read side of parent -> child pipe

    // Close fds that we don't use in this process
    close(miso[1]);
    close(mosi[0]);
    fclose(stdin);
    fclose(stdout);
    fclose(stderr);

    // Begin child side of pipe read/write loop
    for (unsigned long i=0; i<N; i++) {
        optim_read(infd, &input, 1);
        optim_write(outfd, &output, 1);
    }
}

int main(int argc, char * argv[]) {
    argv0 = argv[0];

    // for each cpu
    unsigned long long n = sysconf(_SC_NPROCESSORS_CONF);
    for (unsigned long long i=0; i<n; i++){

        // try to set cpu affinity to just `i`
        cpu_set_t mask;
        CPU_ZERO(&mask);
        CPU_SET(i, &mask);
        if (sched_setaffinity(0, sizeof(mask), &mask) < 0) {
            if (errno == EINVAL)
                continue;
            else
                exiterror(__LINE__);
        }

        // Make pair of pipes
        if (pipe(mosi) < 0)
            exiterror(__LINE__);
        if (pipe(miso) < 0)
            exiterror(__LINE__);

        switch(fork()) {
        case 0:
            child();
            exit(0);
        case -1:
            exiterror(__LINE__);
            break;
        default:
            parent(i);
            break;
        }

        // close old pipe fds before continuing onto next cpu
        close(miso[1]);
        close(mosi[0]);
    }
}
