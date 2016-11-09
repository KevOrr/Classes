// Kevin Orr
// Prof. Jing Wang MW 12:30-1:45

// Prints voltage vs. time for an experiment involving various ac signals

#include <stdio.h>
#include <math.h>

int main(void) {
    puts("time(sec)\tvoltage");
    
    for (double t=0; t<1; t+=0.5) {
        printf("%.2f\t\t%.2f\n", t, 0.5*sin(2*t));
    }
    
    for (double t=1; t<10; t+=0.5) {
        printf("%.2f\t\t%.2f\n", t, sin(t));
    }
    
    for (double t=10; t<=12; t+=0.5) {
        printf("%.2f\t\t%.2f\n", t, sin(10));
    }

    return 0;
}
