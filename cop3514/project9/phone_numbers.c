// Kevin Orr
// Prof. Jing Wang MW 12:30-1:45

// Translate seven-letter words in a file to their corresponding phone numbers

#include <stdio.h>
#include <string.h>
#include <ctype.h>

void translate(char *word, char *phone_number);
int read_line(char *str, int n);

int main(void) {
    char input_path[101];
    fputs("Enter the file name: ", stdout);
    read_line(input_path, 100);
    
    FILE *input_file = fopen(input_path, "r");
    
    if (input_file == NULL) {
        printf("Could not open file %s for reading", input_path);
        return 1;
    }
    
    char output_path[101 + 4];
    strncpy(output_path, input_path, 100);
    strncat(output_path, ".cvt", 4);
    
    FILE *output_file = fopen(output_path, "w");
    
    if (output_file == NULL) {
        printf("Could not open file %s for writing", output_path);
        return 1;
    }
    
    char word[9];
    char phone_number[9];
    while (fgets(word, 8, input_file) != NULL) {
        translate(word, phone_number);
        //printf("%s -> %s\n", word, phone_number);
        fputs(phone_number, output_file);
    }
    
    fclose(input_file);
    fclose(output_file);

    return 0;
}

void translate(char *word, char *phone_number) {
    int i;
    for (i=0; word[i] != '\0'; i++) {
        word[i] = tolower(word[i]);
        
        // maps ('A', 'B', 'C' -> '2'), ('D', 'E', 'F' -> '3'), ...
        if (word[i] >= 'a' && word[i] <= 'o')
            phone_number[i] = '2' + (word[i] - 'a') / 3;
        else if (word[i] >= 'p' && word[i] <= 's')
            phone_number[i] = '7';
        else if (word[i] >= 't' && word[i] <= 'v')
            phone_number[i] = '8';
        else if (word[i] >= 'w' && word[i] <= 'z')
            phone_number[i] = '9';
        else
            phone_number[i] = word[i];
    }
    phone_number[i] = '\0';
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
