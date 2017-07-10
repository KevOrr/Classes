#include <cstddef>
#include <cstdint>
#include <tuple>
#include <vector>
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
    std::vector<std::tuple<size_t, size_t, Subject*>> counts;

    size_t high_bound = ceil(HI_COMPETENCE * n / 100);

    for (size_t i=0; i<n; i++) {
        size_t count1 = 0;
        size_t count2 = 0;
        for (size_t j=0; j<n; j++) {
            if (i != j) {
                count1 += (arr[j] < arr[i]);
                count2 += (arr[i] > arr[j]);
            }
        }
        counts.push_back(std::tuple<size_t, size_t, Subject*>(count1, count2, arr + i));
    }

    std::sort(counts.begin(), counts.end(),
              [](std::tuple<size_t, size_t, Subject*> &a, std::tuple<size_t, size_t, Subject*> &b){
                  return std::get<0>(a) < std::get<0>(b);
              });

    size_t value = std::get<1>(counts[0]);
    size_t idx = 0;
    for (size_t i=0; i<n; i++) {
        if (std::get<1>(counts[i]) > value) {
            value = std::get<1>(counts[i]);
            idx = i;
        }
    }

    std::swap(counts[high_bound - 1], counts[idx]);

    Subject temp[n];
    for (size_t i=0; i<n; i++)
        temp[i] = *std::get<2>(counts[i]);
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
    sort_relative(arr, n);
    sort_extremes(arr, n);
}
