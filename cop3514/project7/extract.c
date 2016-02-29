// Kevin Orr
// Prof. Jing Wang MW 12:30-1:45

// Extracts /(www\..+\.edu)/ from stdin, or quits if it doesn't find a match

#include <stdio.h>
#include <string.h>

#define MAX_LEN 1000

void extract(char *s1, char *s2);
int read_line(char *str, int n);

int main(void) {
    char input[MAX_LEN] = {0};
    char output[MAX_LEN] = {0};

    // Get input
    fputs("Input: ", stdout);
    read_line(input, MAX_LEN);

    // Extract /(www\..+\.edu)/
    extract(input, output);

    if (output[0] == '\0') {
        // Couldn't find /(www\..+\.edu)/
        puts("Web address starting with www. and ending with .edu not found");
        return 1;
    } else {
        // Success
        printf("%s\n", output);
    }

    return 0;
}

void extract(char *s1, char *s2) {
    char *beg = strstr(s1, "www.");
    char *end = strstr(s1, ".edu");
    if (beg == NULL || end == NULL || end <= beg) {
        // substr not found
        *s2 = '\0';
        return;
    }

    end += 4; // Include ".edu"
    int length = end - beg;
    strncpy(s2, beg, length);
}

// From lecture notes
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
