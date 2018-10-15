// Kevin Orr

#define _REENTRANT
#include <stdlib.h>
#include <pthread.h>
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <error.h>
#include <unistd.h>
#include <semaphore.h>
#include <unistd.h>

// Should we sleep 1 sec in consumer?
static const int SLEEP = 1;
static const size_t BUFFER_SIZE = 15;

// Macro to print out an error message for library/syscall errors
#define EXITERROR() error_at_line(errno, errno, __FILE__, __LINE__, "pid %llu", (long long unsigned)getpid())

typedef struct {
    char *buf;
    size_t size;
    size_t next_full;
    size_t next_empty;
    sem_t empty;
    sem_t full;
    sem_t mutex;
    int closed;
} BUFFER;

// Allocate a buffer on heap and initialize it
BUFFER* buffer_make(size_t size) {
    BUFFER *buf = malloc(sizeof(BUFFER));
    if (buf == NULL) EXITERROR();

    if ((buf->buf = calloc(size, 1)) == NULL) EXITERROR();
    buf->size = size;
    buf->next_full = buf->next_empty = 0;
    if (sem_init(&buf->empty, 0, size) == -1) EXITERROR();
    if (sem_init(&buf->full, 0, 0) == -1) EXITERROR();
    if (sem_init(&buf->mutex, 0, 1) == -1) EXITERROR();
    buf->closed = 0;

    return buf;
}

// Insert a character into buffer (MT-safe)
void buffer_put(BUFFER *buf, char c) {
    sem_wait(&buf->mutex);
    int closed = buf->closed;
    sem_post(&buf->mutex);
    if (closed)
        return;

    sem_wait(&buf->empty);
    sem_wait(&buf->mutex);

    buf->buf[buf->next_empty] = c;
    buf->next_empty = (buf->next_empty + 1) % buf->size;

    sem_post(&buf->mutex);
    sem_post(&buf->full);
}

// Get the next character from a buffer (MT-safe)
int buffer_get(BUFFER *buf) {
    sem_wait(&buf->mutex);
    int is_empty = buf->closed && buf->next_full == buf->next_empty;
    sem_post(&buf->mutex);
    if (is_empty)
        return EOF;

    char c;
    sem_wait(&buf->full);
    sem_wait(&buf->mutex);

    if (SLEEP)
        sleep(1);

    c = buf->buf[buf->next_full];
    buf->next_full = (buf->next_full + 1) % buf->size;

    sem_post(&buf->mutex);
    sem_post(&buf->empty);

    return c;
}

// Signal a buffer to not accept any more input (MT-safe)
int buffer_close(BUFFER *buf) {
    sem_wait(&buf->mutex);
    int old_val = buf->closed;
    buf->closed = 1;
    sem_post(&buf->mutex);

    return old_val;
}

// Read from mytest.dat and insert into (BUFFER*)buf
void* producer(void *buf) {
    int c;
    FILE* f = fopen("mytest.dat", "r");
    if (f == NULL) EXITERROR();

    while ((c = fgetc(f)) != EOF)
        buffer_put((BUFFER*)buf, c);

    buffer_close((BUFFER*)buf);
    fclose(f);
    return NULL;
}

// Get characters from (BUFFER*)buf and print them out
void* consumer(void *buf) {
    int c;
    while ((c = buffer_get((BUFFER*)buf)) != EOF)
        putchar(c);

    return NULL;
}

int main() {
    errno = 0;

    // Make stdout unbuffered, so we can see each character when printed
    if (setvbuf(stdout, NULL, _IONBF, 0) != 0) EXITERROR();

    BUFFER *buf = buffer_make(BUFFER_SIZE);

    pthread_attr_t thread_attrs;
    if (pthread_attr_init(&thread_attrs) != 0) EXITERROR();

    // Start producer and consumer
    pthread_t t_producer, t_consumer;
    if (pthread_create(&t_producer, &thread_attrs, producer, buf) != 0) EXITERROR();
    if (pthread_create(&t_consumer, &thread_attrs, consumer, buf) != 0) EXITERROR();

    // Wait for producer and consumer to exit
    if (pthread_join(t_producer, NULL) != 0) EXITERROR();
    if (pthread_join(t_consumer, NULL) != 0) EXITERROR();
}
