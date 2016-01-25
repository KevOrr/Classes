// Kevin Orr
// Prof. Jing Wang MW 12:30-1:45

// Replaces a 3 digit number with (d+6)%10 for each digit d

#include <stdio.h>

int main(void) {
    int a, b, c;

    printf("Enter number: ");
    scanf("%1d", &a);
    scanf("%1d", &b);
    scanf("%1d", &c);
    
    printf("%d%d%d\n", (a+6)%10, (b+6)%10, (c+6)%10);

    return 0;
}

