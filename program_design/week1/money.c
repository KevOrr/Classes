// Kevin Orr
// Prof. Jing Wang MW 12:30-1:45

#include <stdio.h>

int main(void) {
    int pennies, nickles, dimes, quarters;
    printf("Pennies: ");
    scanf("%d", &pennies);
    printf("Nickles: ");
    scanf("%d", &nickles);
    printf("Dimes: ");
    scanf("%d", &dimes);
    printf("Quarters: ");
    scanf("%d", &quarters);

    int total = pennies + 5*nickles + 10*dimes + 25*quarters;
    printf("Total: $%.2f\n", total / 100.0);

    return 0;
}

