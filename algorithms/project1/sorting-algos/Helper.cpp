#include "Helper.hpp"
#include <stdlib.h>
#include <string.h>
using namespace std;

/**
  Fills an allocated int array with sorted values

  @param data the array to fill
  @param n the size of the array
 */
void sortedArray(int* data, int n)
{
  for (int i = 0; i < n; i++)
    data[i] = i+1;
}

/**
  Fills an allocated int array with n zeros

  @param data the array to fill
  @param n the size of the array
 */
void constArray(int* data, int n)
{
  memset(data, 0, n*sizeof(int));
}

/**
  Fills an allocated int array with a random permutation of the integers 1 to n

  @param n the size of the array
  @return the array
 */
void randomArray(int* data, int n)
{
  sortedArray(data, n);
  for (int i = n; i > 1; i--)
  {
    int swap = rand() % i;
    Swap(data[swap], data[i-1]);
  }
}
