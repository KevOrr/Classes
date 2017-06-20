#ifndef __SORTING_HELPER_H
#define __SORTING_HELPER_H

#include <iostream>
using namespace std;

//Functions for generating various integer arrays of length n
void sortedArray(int* data, int n);
void constArray(int* data, int n);
void randomArray(int* data, int n);

/**
Swaps two values
Name capitalized to avoid potential naming conflicts

@param a,b  values to swap
*/
template <class T> void Swap(T& a, T& b)
{
  T t = a;
  a = b;
  b = t;
}

/**
Compares 3 values and returns which is the median
When two values are equal, one of the two will be indicated
Note:  these values must be comparable with <; compilation will fail otherwise.

@param a,b,c  3 values
@return 1, 2, or 3, depending on whether a, b, or c is the median
*/
template <class T> int medianof3(T a, T b, T c)
{
  if (a < c)
  {
    if (b < a)
      return 1;
    else if (c < b)
      return 3;
    else
      return 2;
  }
  else if (b < c)
    return 3;
  else if (a < b)
    return 1;
  else
    return 2;
}

/**
 Returns whether an array is in sorted order
 Note:  the values in the array must be comparable with <; compilation will fail otherwise.

 @param arr the array to test
 @param n the length of the array
 @return whether the array is sorted
 */
template <class T> bool isSorted(T* arr, int n)
{
  for (int i = 0; i < n-1; i++)
    if (arr[i+1] < arr[i])
      return false;
  return true;
}

/**
  Prints an array of size n, with spaces in between and a newline at the end

  @param arr the array to print
  @param n the length of the array
  */
template <class T> void printArray(T* arr, int n)
{
  for (int i = 0; i < n; i++)
    cout << arr[i] << ' ';
  cout << endl;
}
#endif
