// Kevin Orr
// Prof. Jing Wang MW 12:30-1:45

// Produces n multiples of 7

#include <stdio.h>

int main(void) {
    int n;
    fputs("Enter the number of multiples of 7: ", stdout);
    scanf("%d", &n);

    for (int i=1; i<=n; i++) {
        printf("%d\n", i * 7);
    }

    return 0;
}

