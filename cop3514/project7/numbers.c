// Kevin Orr
// Prof. Jing Wang MW 12:30-1:45

// Finds the largest or smallest integer from cli args
// USAGE: ./find_largest_smallest (-l | -s) number [number ...]

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char *argv[]) {
    if (argc != 12) { // Should have argv[0], mode switch, 10 numbers
        puts("Invalid option. -l for largest number or -s for smallest\n"
             "number followed by ten numbers.");
        return 1;
    }

    if (strcmp(argv[1], "-l") == 0) {
        // Find largest
        int max = atoi(argv[2]);
        int i;
        for (i=3; i<argc; i++) {
            int temp = atoi(argv[i]);
            if (temp > max)
                max = temp;
        }
        printf("The largest number is %d\n", max);

    } else if (strcmp(argv[1], "-s") == 0) {
        // Find smallest
        int min = atoi(argv[2]);
        int i;
        for (i=3; i<argc; i++) {
            int temp = atoi(argv[i]);
            if (temp < min)
                min = temp;
        }
        printf("The smallest number is %d\n", min);

    } else {
        puts("Invalid option");
        return 1;
    }

    return 0;
}
