// Kevin Orr
// Prof. Jing Wang MW 12:30-1:45

// Calculates (n+6)%10 for each digit on stdin, then swaps first and last digits

#include <stdio.h>
#include <stdint.h>

void replace(int *input, int *output, int n); // Calculates each digit
void swap(int *p, int *q); // Swaps first and last digits

int main(void) {

    // Get target size
    int n; // number of digits
    fputs("Enter the number of digits of the number: ", stdout);
    scanf("%d", &n);

    // Create arrays
    int input[n],  // array to hold input digits
        output[n]; // holds output digits

    // Input
    fputs("Enter the number: ", stdout);
    int *inp; // points to elem in input
    for (inp = &input[0]; inp < &input[n]; inp++)
        scanf("%1d", inp);

    // Replace and swap
    replace(input, output, n);
    swap(&output[0], &output[n-1]);

    // Output
    fputs("Output: ", stdout);
    int *outp; // points to elem in output
    for (outp = &output[0]; outp < &output[n]; outp++)
        printf("%d", *outp);
    printf("\n");

    return 0;
}

void replace(int *input, int *output, int n) {
    int *inp, *outp; // inp points to element in input, outp to element in output
    for (inp = &input[0], outp = &output[0]; inp < &input[n]; inp++, outp++)
        *outp = (*inp + 6) % 10;
}

void swap(int *p, int *q) {
    int old_p = *p;
    *p = *q;
    *q = old_p;
}
