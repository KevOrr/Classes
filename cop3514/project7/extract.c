// Kevin Orr
// Prof. Jing Wang MW 12:30-1:45

// Extracts /(www\..+\.edu)/ from stdin, or quits if it doesn't find a match

#include <stdio.h>
#include <string.h>

void extract(char *s1, char *s2);
int read_line(char *str, int n);

int main(void) {
    char input[1000] = {0};
    char output[1000] = {0};

    return 0;
}

void extract(char *s1, char *s2) {
    char *beg = strstr(s1, "www.");
    char *end = strstr(s1, ".edu");
    if (beg == NULL || end == NULL) {
        // substr not found
        s2 = NULL;
        return;
    }

    beg += 4; // Skips 4 chars of "www."
    int length = (end - 1) - beg; // Rewind 1 character on `end` (points to '.')
    strncpy(beg, s2, length);
}

int read_line(char *str, int n)
{
    int ch, i = 0; 
    while ((ch = getchar()) != '\n') {
        if (i < n) {
            *str++ = ch;
            i++;
        }
    }
    *str = '\0';   /* terminates string */
    return i;        /* number of characters stored */
}
