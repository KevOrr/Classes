#include <iostream>
#include <iomanip>
#include <ctime>
#include <cstdlib>
#include <exception>
#include <cstdio>
using namespace std;

#include "Sorting.hpp"
#include "Helper.hpp"

enum sort_t  {SELECTION, INSERTION, MERGE, QUICK};
enum input_t {SORTED, CONSTANT, RANDOM};

#define DEFAULT_N     10000
#define DEFAULT_ALG   MERGE
#define DEFAULT_INPUT RANDOM
#define DEF_STR_ALG   "MergeSort"
#define DEF_STR_INPUT "Random"

#define MIN_N         1
#define MAX_N         1000000000

#define N_ARG         1
#define ALG_ARG       2
#define INPUT_ARG     3
#define MIN_ARGS      1
#define MAX_ARGS      4
#define PRINT_USAGE() cout << argv[0] << " [n] [algorithm] [input type]" << endl << endl << \
                              "n (optional):  size of array (default:  " << DEFAULT_N << ')' << endl << \
                              "algorithm (optional):  one of selectionsort, insertionsort, mergesort, or quicksort (simq, default " << DEF_STR_ALG << ')' << endl << \
                              "input type (optional):  one of increasing, decreasing, constant, or random (idcr, default " << DEF_STR_INPUT << ')' << endl;

int main(int argc, char** argv)
{
  int n = DEFAULT_N;
  sort_t alg = DEFAULT_ALG;
  input_t intype = DEFAULT_INPUT;
  int* data;
  int* temp;
  clock_t start;
  clock_t timing[3];

  //Parse input arguments
  if (argc < MIN_ARGS || argc > MAX_ARGS)
  {
    PRINT_USAGE();
    return -1;
  }

  if (argc > N_ARG)
    n = atoi(argv[N_ARG]);

  if (n < MIN_N || n > MAX_N)
  {
    cout << "Value of n (argument " << N_ARG << ") out of range\nValid range is " << MIN_N << " to " << MAX_N << endl;
    return -1;
  }

  if (argc > ALG_ARG)
  {
    switch (argv[ALG_ARG][0])
    {
    case 's':
    case 'S':
      alg = SELECTION;
      break;
    case 'i':
    case 'I':
      alg = INSERTION;
      break;
    case 'm':
    case 'M':
      alg = MERGE;
      break;
    case 'q':
    case 'Q':
      alg = QUICK;
      break;
    default:
      cout << "Sorting algorithm (arg " << ALG_ARG << ") not recognized, using default of " << DEF_STR_INPUT << '\n';
    }
  }

  if (argc > INPUT_ARG)
  {
    switch (argv[INPUT_ARG][0])
    {
    case 's': //sorted
    case 'S':
    case 'i': //increasing
    case 'I':
    case 'a': //ascending
    case 'A':
      intype = SORTED;
      break;
    case 'c':  //constant
    case 'C':
    case 'z':  //zero
    case 'Z':
      intype = CONSTANT;
      break;
    case 'r':  //random
    case 'R':
      intype = RANDOM;
      break;
    default:
      cout << "Input array type (arg " << INPUT_ARG << ") not recognized, using default of '" << DEF_STR_INPUT << "'\n";
    }
  }

  //Run sorting algorithm 3 times
  data = new int[n];
  temp = new int[n];

  for (int i = 0; i < 3; i++)
  {
    // Initialize data
    switch (intype)
    {
    case SORTED:
      sortedArray(data, n);
      break;
    case CONSTANT:
      constArray(data, n);
      break;
    case RANDOM:
      randomArray(data, n);
    }

    //Sort data
    //printArray(arr, n);
    start = clock();
    switch (alg)
    {
    case SELECTION:
      selectionsort(data, n);
      break;
    case INSERTION:
      insertionsort(data, n);
      break;
    case MERGE:
      mergesort(data, n, temp);
      break;
    case QUICK:
      quicksort(data, n);
    }
    timing[i] = clock() - start;
    //printArray(arr, n);

    //Verify data is sorted
    if (isSorted(data, n))
      cout << "Array successfully sorted.\n";
    else
    {
      cout << "Array incorrectly sorted.\n";
      //Time to debug...
      return -1;
    }
  }

  //Output timing results
  for (int i = 0; i < 3; i++)
    cout << "Attempt " << i + 1 << ":  " << setw(8) << (int) (timing[i] * 1000.0 / CLOCKS_PER_SEC) << " ms\n";
  cout << "Median time:     " << setw(8) << (int) (timing[medianof3(timing[0], timing[1], timing[2]) - 1] * 1000.0 / CLOCKS_PER_SEC) << " ms" << endl;

  delete[] data;
  delete[] temp;

  return 0;
}
