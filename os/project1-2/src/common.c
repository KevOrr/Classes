#include "common.h"
#include "errno.h"
#include "error.h"
#include "ctype.h"

#define BUFFER_INIT_SIZE 512

char *input_buffer = NULL;
size_t input_buffer_length;

struct queue *make_queue(size_t n) {
    struct queue *q = malloc(sizeof(struct queue));
    q->buffer = malloc(n * sizeof(addr_t));
    q->front = 0;
    q->length = 0;
    q->maxlength = n;
    return q;
}

void queue_push(struct queue *q, addr_t value) {
    q->buffer[(q->front + q->length) % q->maxlength] = value;
    q->length++;
}

addr_t queue_pop(struct queue *q) {
    addr_t temp = q->buffer[q->front];
    q->length--;
    q->front = (q->front + 1) % q->maxlength;
    return temp;
}

struct frame* make_table(size_t n) {
    struct frame *new = calloc(n, sizeof(struct frame));
    if (new == NULL)
        exit_error();
    return new;
}

addr_t addr_to_framenum(addr_t addr) {
    return addr >> PAGE_BITS;
}

int isempty(const char *s) {
    while (*s)
        if (!isspace(s))
            return 0;
    return 1;
}

int line_terminated(const char *s) {
    while (*s)
        if (*s++ == '\n')
            return 1;
    return 0;
}

int parse_next_line(FILE* tracef, const char *tracef_path, addr_t *lineno,
                     addr_t *next_address, enum mem_op *next_op) {

    char next_op_char;

    if (input_buffer == NULL) {
        input_buffer = malloc(BUFFER_INIT_SIZE);
        if (input_buffer == NULL)
            exit_error();
        else
            input_buffer_length = BUFFER_INIT_SIZE;
    }

    if (fgets(input_buffer, input_buffer_length, tracef)) {
        *lineno += 1;

        while (1) {
            if (line_terminated(input_buffer))
                break;
            else if (isempty(input_buffer)) {
                fgets(input_buffer, input_buffer_length, tracef);
            } else {
                input_buffer = realloc(input_buffer, input_buffer_length*2);
                fgets(input_buffer + input_buffer_length, input_buffer_length, tracef);
                input_buffer_length *= 2;
            }
        }

        if (isempty(input_buffer))
            return -1;

        sscanf(input_buffer, "%llx %c", next_address, &next_op_char);
        switch (next_op_char) {
        case 'r':
        case 'R':
            *next_op = READ;
            return 1;
        case 'w':
        case 'W':
            *next_op = WRITE;
            return 1;
        default:
            fprintf(stderr, "%s:%lld '%c' is not a valid memory operation\n",
                    tracef_path, *lineno, next_op_char);
            exit(1);
        }
    } else {
        return 0;
    }
}

void test_algorithm(const char *tracef_path, addr_t nframes,
                    void (*init)(addr_t),
                    void (*run)(addr_t frame_n, enum mem_op,
                                addr_t *disk_writes, addr_t *disk_reads)) {

    FILE *tracef = fopen(tracef_path, "r");
    if (tracef == NULL)
        exit_error();

    addr_t event_counter = 0;
    addr_t disk_reads = 0;
    addr_t disk_writes = 0;

    addr_t lineno;
    addr_t next_address;
    enum mem_op next_op;

    init(nframes);
    int last_line;
    while ((last_line = parse_next_line(tracef, tracef_path, &lineno, &next_address, &next_op))) {
        if (last_line == -1)
            continue;

        run(next_address, next_op, &disk_writes, &disk_reads);
        event_counter++;
        if (debug)
            getc(stdin);
    }

    printf("total memory frames: %llu\n"
           "events in trace: %llu\n"
           "total disk reads: %llu\n"
           "total disk writes: %llu\n",
           nframes, event_counter, disk_reads, disk_writes);
}
