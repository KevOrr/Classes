/*****************************************************************
 * This program finds the edges of light and dark regions of the
 * input binary bit pattern.
 * ***************************************************************/

#include <stdio.h>

void edge(int n, int a[], int b[]);
int main(void)
{
	int input[8]={0};
	int output[8];

	int i;
	printf("Please enter the 8 bit bar code: ");
	for(i=0;i<8;i++)
		scanf("%1d", &input[i]);

	edge(8, input, output);

	for(i=0;i<8;i++)
		printf("%d", output[i]);
    printf("\n");
	return 0;
}

void edge(int n, int a[], int b[])
{
	int *ap, *bp; // `ap` points to element in a, `bp` to an element in b
	b[0]=0;
	for (ap = &a[1], bp = &b[1]; ap < &a[n]; ap++, bp++)
        *bp = *ap ^ *(ap-1); // XOR eaxh neighboring bit pair
}
