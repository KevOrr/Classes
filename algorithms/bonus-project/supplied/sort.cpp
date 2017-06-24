#include <cstddef>
#include <utility>
#include <algorithm>

#include "sort.h"
#include "dunningkruger.h"

void sort_relative(Subject* arr, int n) {
    std::pair<size_t, Subject*> counts[n];

    for (size_t i=0; i<n; i++) {
        size_t count = 0;
        for (size_t j=0; j<n; j++) {
            if (i != j) {
                count += (arr[i] < arr[j]);
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

void sort_extremes(Subject* arr, int n) {
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
        for (size_t j=i+low; j<n; j++) {
            count += (arr[i] > arr[j]);
        }
        high_counts[i - high] = std::pair<size_t, Subject*>(count, arr+i);
    }


    std::sort(low_counts, low_counts + low + 1, [](std::pair<size_t, Subject*> &a, std::pair<size_t, Subject*> &b){
            return a.first > b.first;
        });
    std::sort(high_counts, high_counts + n - high, [](std::pair<size_t, Subject*> &a, std::pair<size_t, Subject*> &b){
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
    std::copy(high_temp, high_temp + n - high, arr);
}

void dksort(Subject* arr, int n) {
    sort_relative(arr, n);
    sort_extremes(arr, n);
}
