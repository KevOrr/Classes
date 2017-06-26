#include <cstddef>
#include <cstdint>
#include <utility>
#include <algorithm>
#include <iostream>

#include "sort.h"
#include "dunningkruger.h"

void print_array(Subject* arr, size_t n) {
    std::cout << "(";
    for (size_t i=0; i<n; i++) {
        std::cout << "(" << i << " . " << arr[i].getCompetence() << ")" << std::endl;
    }
    std::cout << ")" << std::endl << std::endl;
}

template<typename T>
uint_fast8_t median3(T a, T b, T c) {
    if (a < b) {
        if (b < c)
            return b;
        else if (a < c)
            return c;
        else
            return a;
    } else {
        if (c < b)
            return b;
        else if (a < c)
            return a;
        else
            return c;
    }
}

template<typename T>
size_t partition(T* arr, size_t low, size_t high) {
    T p = (median3(arr[low], arr[(low + high)/2], arr[high]));

    size_t left = low, right = high;
    while (true) {
        do {left++;} while (arr[left] < p);

        do {right--;} while (arr[right] > p);

        if (left >= right)
            return right;
        std::swap(arr[left], arr[right]);
    }
}

template<typename T>
void quicksort(T* arr, size_t low, size_t high) {
    if (low < high) {
        size_t p = partition(arr, low, high);
        quicksort(arr, low, p);
        quicksort(arr, p+1, high);
    }
}

// int main() {
//     int A[] = {1,6,7,5,3,3,4,6,8,9,7,6,3,4,67,357,34,256,46,24,36,23,246,34,34,1,143,1345,134,51345,3145,342,
//                223,35,326,47,25,62,234,243,5,2,34,45,356,56,586,679,985,87,87,7,6,65,263,364,5,5,12,15,5,5326,
//                6635,6,43,34,76,87,56,74,874,895,7089,978,86,7,6,341,45,32,32,65,65,76,748,78,7,7,643,652,532};

//     size_t n = sizeof A / sizeof A[0];
//     quicksort(A, 0, n-1);
//     for (size_t i=0; i<n; i++) {
//         std::cout << A[i] << " ";
//     }

//     std::cout << std::endl;
// }

void sort_relative(Subject* arr, size_t n) {
    std::pair<size_t, Subject*> counts[n];

    for (size_t i=0; i<n; i++) {
        size_t count = 0;
        for (size_t j=0; j<n; j++) {
            if (i != j) {
                count += (arr[j] < arr[i]);
            }
        }
        counts[i] = std::pair<size_t, Subject*>(count, arr + i);
    }

    std::sort(counts, counts+n, [](std::pair<size_t, Subject*> &a, std::pair<size_t, Subject*> &b){
            return a.first < b.first;
        });

    Subject temp[n];
    for (size_t i=0; i<n; i++)
        temp[i] = *counts[i].second;
    std::copy(temp, temp+n, arr);
}

void sort_extremes(Subject* arr, size_t n) {
    size_t high = ceil(HI_COMPETENCE * n / 100.0);
    size_t low = floor(1 + (LOW_COMPETENCE * n / 100.0));
    std::pair<size_t, Subject*> high_counts[n - high];
    std::pair<size_t, Subject*> low_counts[1 + low];

    for (size_t i=0; i<=low; i++) {
        size_t count = 0;
        for (size_t j = 1 + low; j < high; j++) {
            count += (arr[i] > arr[j]);
        }
        low_counts[i] = std::pair<size_t, Subject*>(count, arr+i);
    }

    for (size_t i=high; i<n; i++) {
        size_t count = 0;
        for (size_t j=1+low; j<n; j++) {
            count += (arr[i] > arr[j]);
        }
        high_counts[i - high] = std::pair<size_t, Subject*>(count, arr+i);
    }

    std::sort(low_counts, low_counts + low + 1,
              [](std::pair<size_t, Subject*> &a, std::pair<size_t, Subject*> &b){
                  return a.first > b.first;
              });

    std::sort(high_counts, high_counts + (n - high),
              [](std::pair<size_t, Subject*> &a, std::pair<size_t, Subject*> &b){
                  return a.first < b.first;
              });

    Subject low_temp[low + 1];
    for (size_t i = 0; i <= low; i++) {
        low_temp[i] = *low_counts[i].second;
    }
    std::copy(low_temp, low_temp + low + 1, arr);

    Subject high_temp[n - high];
    for (size_t i = 0; i < n - high; i++) {
        high_temp[i] = *high_counts[i].second;
    }
    std::copy(high_temp, high_temp + n - high, arr + high);
}

void dksort(Subject* arr, int n) {
    for (int i = 0; i < n; i++)
        arr[i].setCompetence(100.0 * i / n);
    sort_relative(arr, n);
    sort_extremes(arr, n);
}
