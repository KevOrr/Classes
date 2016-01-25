// Kevin Orr
// Prof. Jing Wang MW 12:30-1:45

// Converts a length between one of four conversions

#include <stdio.h>

int main(void) {
    fputs(
        "1  mi -> km\n"
        "2  km -> mi\n"
        "3  in -> cm\n"
        "4  cm -> in\n\n"
        "Enter selection: ",
        stdout);

    int choice;
    scanf("%d", &choice);

    double multiplier;
    char *from_units, *to_units;
    switch (choice) {
        case 1:
            multiplier = 1.6093;
            from_units = "mi";
            to_units = "km";
            break;
        case 2:
            multiplier = 0.6214;
            from_units = "km";
            to_units = "mi";
            break;
        case 3:
            multiplier = 2.54;
            from_units = "in";
            to_units = "cm";
            break;
        case 4:
            multiplier = 0.3937;
            from_units = "cm";
            to_units = "in";
            break;
        default:
            puts("Invalid input, please select  between 1 and 4");
            return 1;
    }

    printf("Enter length (%s): ", from_units);
    double input;
    scanf("%lf", &input);

    printf("\n%.2f %s\n", input * multiplier, to_units);

    return 0;
}
