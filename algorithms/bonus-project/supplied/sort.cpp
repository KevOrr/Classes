#include <cstddef>
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

    std::cout << high << " " << low << std::endl;

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

    for (size_t i=0; i<5; i++) {
        std::cout << high_counts[i].first << " " << high_counts[i].second->getCompetence() << std::endl;
    }

    std::cout << std::endl;

    for (size_t i=n - high - 6; i<n - high; i++) {
        std::cout << high_counts[i].first << " " << high_counts[i].second->getCompetence() << std::endl;
    }

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
    // print_array(arr, n);
}

void dksort(Subject* arr, int n) {
    // print_array(arr, n);
    sort_relative(arr, n);
    // print_array(arr, n);
    sort_extremes(arr, n);
    // print_array(arr, n);
}
