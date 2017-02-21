#include <vector>
#include <random>
#include <iostream>
#include <iomanip>

#include "line.h"

const int NUM_LINES = 10;

int main() {
    std::vector<Line> lines;
    std::uniform_int_distribution<int> uni_rand(-5, 5);
    std::default_random_engine gen;

    // Create NUM_LINES random lines
    std::cout << "Creating " << NUM_LINES << " random lines:" << std::endl << std::setprecision(1);
    for (int i=0; i<NUM_LINES; ++i) {
        Line line(uni_rand(gen), uni_rand(gen));
        lines.push_back(line);

        std::cout << "line " << i << ": y = ";

        double slope = line.getSlope();
        double intercept = line.getIntercept();
        if (slope) {
            std::cout << slope << "x";
            if (intercept > 0)
                std::cout << " + " << intercept;
            else if (intercept < 0)
                std::cout << " - " << -intercept;
        } else {
            std::cout << intercept;
        }

        std::cout << std::endl;
    }
    std::cout << std::endl;

    // Test intersection of each pair of lines, including self-pairs
    std::cout << "Testing " << NUM_LINES*(NUM_LINES + 1)/2 << " pairs of lines: " << std::endl
              << std::setprecision(3);
    for (int i=0; i<NUM_LINES; ++i) {
        for (int j=0; j<=i; ++j) {
            std::cout << i << " + " << j << ": ";
            try {
                double x = lines[i].intersect(lines[j]);
                std::cout << "x = " << x << std::endl;
            } catch (ParallelLines e) {
                std::cout << e.getMessage() << std::endl;
            } catch (EqualLines e) {
                std::cout << e.getMessage() << std::endl;
            }
        }
    }
}
