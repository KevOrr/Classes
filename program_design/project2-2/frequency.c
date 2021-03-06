// Kevin Orr
// Prof. Jing Wang MW 12:30-1:45

// Counts occurences of each word in one line of stdin

#include <stdio.h>
#include <string.h>

#define DELIM " ,.!"
#define MAX_INPUT 1000
#define MAX_WORDS 1000
#define MAX_WORD_LEN 50

int read_line(char *str, int n);

int main(void) {
    char str[MAX_INPUT + 1] = {0};
    char words[MAX_WORDS][MAX_WORD_LEN + 1] = {{0}};
    int word_counts[MAX_WORDS] = {0};
    int unique_words = 0;

    // Get input string
    fputs("Enter the sentence: ", stdout);
    read_line(str, MAX_INPUT);

    // Get word counts
    char *strp = str;
    for (strp = strtok(strp, DELIM); strp != NULL; strp = strtok(NULL, DELIM)) {

        // Try to find word in words
        int words_index = -1;
        int i;
        for (i=0; i<unique_words; i++) {
            if (!strcmp(words[i], strp)) {
                words_index = i;
                break;
            }
        }

        if (words_index == -1) {
            // make new entry in words
            words_index = unique_words++;
            strcpy(words[words_index], strp);
        }
        word_counts[words_index]++;
    }

    // Output results
    fputs("\n", stdout);
    int i;
    for (i=0; i<unique_words; i++) {
        printf("%s\t%d\n", words[i], word_counts[i]);
    }

    return 0;
}

// From lecture notes
int read_line(char *str, int n) {
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
