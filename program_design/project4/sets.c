// Kevin Orr
// Prof. Jing Wang MW 12:30-1:45

// Computes the intersection and union of two sets

#include <stdio.h>

#define LENGTH 10

void fill_array(char array[], int num, char name[]) {
    printf("Enter the numbers in set %s: ", name);
    int i;
    for (i=0; i<num; i++) {
        int d;
        scanf("%d", &d);
        array[d] = 1;
    }
}

void print_union(char a[], char b[], int len) {
    printf("The union of set A and B is: ");
    int i;
    for (i=0; i<len; i++) {
        if (a[i] || b[i]) {
            printf("%d ", i);
        }
    }
    printf("\n");
}

void print_intersection(char a[], char b[], int len) {
    printf("The union of set A and B is: ");
    int i;
    for (i=0; i<len; i++) {
        if (a[i] && b[i]) {
            printf("%d ", i);
        }
    }
    printf("\n");
}

int main(void) {
    char a[LENGTH] = {0};
    char b[LENGTH] = {0};

    printf("Please enter the number of elements in set A: ");
    int num_a;
    scanf("%d", &num_a);
    fill_array(a, num_a, "A");
    
    printf("Please enter the number of elements in set B: ");
    int num_b;
    scanf("%d", &num_b);
    fill_array(b, num_b, "B");

    print_union(a, b, LENGTH);
    print_intersection(a, b, LENGTH);

    return 0;
}
