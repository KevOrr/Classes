#include "dunningkruger.h"
#include "sort.h"

#include <random>
#include <ctime>
#include <iostream>
using namespace std;

#define CLASS_TO_TEST SlightlyOff
const int POP_SIZE = 10000;

const int NTRIALS = 5;
const unsigned int RANDOM_SEED[] = {0xDEADBEEF, 0xC0C0ABEE, 0xFEEDFACE, 0xDEBEDDED, 0xABEADED1};

int main()
{
  clock_t start, end, timesum;
  int n;
  double scoresum;
  Subject* arr;

  n = POP_SIZE;

  arr = new CLASS_TO_TEST[n];
  for (int i = 0; i < n; i++)
    arr[i].setCompetence(100.0 * i / n);

  shuffle(arr, n);

  scoresum = 0;
  timesum = 0;
  for (int i = 0; i < NTRIALS; i++)
  {
    cout << "Trial " << i + 1 << endl;
    srand(RANDOM_SEED[i]);
    shuffle(arr, n);

    start = clock();
    dksort(arr, n);
    end = clock();

    scoresum += score(arr, n);
    timesum += end - start;
  }
  cout << "Average score:  " << scoresum / NTRIALS << endl;
  cout << "Average time:  " << (double) timesum / NTRIALS / CLOCKS_PER_SEC << " seconds" << endl;

  delete[] arr;

  return 0;
}
