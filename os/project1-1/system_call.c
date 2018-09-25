#include <stdio.h>
#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>
#include <sys/time.h>
#include <fcntl.h>
#include <errno.h>

#define MILLION 1000000

unsigned long N = 10 * MILLION;

char * argv0;

uint64_t dummy_read_n(uint32_t fd, unsigned long n) {
    struct timeval start, end;
    gettimeofday(&start, NULL);

#if defined(OPTIMIZE_READ_SYSCALL) && defined(__x86_64__)
    for (; n>0; n--)
        asm ("xor %%rax, %%rax\n\t"
             "mov %0, %%ebx\n\t"
             "xor %%rcx, %%rcx\n\t"
             "xor %%rdx, %%rdx\n\t"
             "syscall\n\t"

             :
             : "irm" (fd)
             : "rax", "ebx", "rcx", "rdx");

#elif defined(OPTIMIZE_READ_SYSCALL)
#error "OPTIMIZE_READ_SYSCALL set, but architecture isn't supported."

#else
    for (; n>0; n--)
        read(fd, NULL, 0);
#endif

    gettimeofday(&end, NULL);
    return (end.tv_usec - start.tv_usec) + (end.tv_sec - start.tv_sec) * (uint64_t)MILLION;
}

void error() {
    perror(argv0);
    fflush(stdout);
    fflush(stderr);
    exit(errno);
}

int main(int argc, char *argv[]) {
    argv0 = argv[0];

    int fd = open("/dev/zero", O_RDONLY);
    if (fd < 0)
        error();

    double total = dummy_read_n(fd, N) / (double)MILLION;
    printf("%lu million calls to read(2) took %lf seconds.\n", N/MILLION, total);
}
