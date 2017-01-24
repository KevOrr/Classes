// Programmer: Kevin Orr
// Last modification date: 2017-01-16

#include <iostream>
#include "line.h"

double Line::intersect(const Line L) const throw(ParallelLines, EqualLines) {
    // Returns the x coordinate where both lines intersect
    // Throws ParallelLines if they're parallel and EqualLines if they're equal

    if (this->a == L.a) { // Same slope
        if (this->b == L.b) // Same intercept
            throw EqualLines();
        else
            throw ParallelLines();
    }

    // a_1 x + b_1 = a_2 x + b_2 => x = (b_2 - b_1)/(a_1 - a_2)
    return (L.b - this->b) / (this->a - L.a);
}
