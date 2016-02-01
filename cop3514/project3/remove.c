// Kevin Orr
// Prof. Jing Wang MW 12:30-1:45

// Removes all nonalpha characters (besides spaces) from input

#include <stdio.h>
#include <ctype.h>

int main(void) {
    fputs("Enter message: ", stdout);
    char input[256];
    fgets(input, sizeof(input), stdin);
    
    char output[256];
    int out_i = 0;
    for (int in_i=0; in_i<sizeof(input); in_i++) {
        char c = input[in_i];
        if (c == '\0') {
            output[out_i] = '\0';
            break;
        } else if (c == ' ' || isalpha(c)) {
            output[out_i] = toupper(c);
            out_i++;
        }
    }
    
    printf("%s\n", output);

    return 0;
}
