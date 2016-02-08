// Kevin Orr
// Prof. Jing Wang MW 12:30-1:45

// Detects edges on an 8-bit barcode

#include <stdio.h>
#include <stdint.h>

int main(void) {
    int i;

    uint8_t barcode = 0;
    printf("Enter 8-bit barcode: ");
    
    // Insert into lsb and shift left
    for (i=0; i<8; i++) {
        int d;
        scanf("%1d", &d);
        barcode <<= 1;
        barcode |= d;
    }

    // Create right-shifted barcode, with first bit same as original barcode
    uint8_t shifted = barcode >> 1 | (barcode & (1 << 7));
    // XOR is logically equivalent to "is different than"
    uint8_t output = barcode ^ shifted;
    
    uint8_t mask = 1 << 7;
    uint8_t offset = 7;
    printf("Output: ");

    // Print out bit by bit
    for (i=0; i<8; i++) {
        printf("%d", (output & mask) >> offset);
        mask >>= 1;
        offset--;
    }
    printf("\n");

    return 0;
}
