#include <string>

class RuntimeException { // generic run-time exception
private:
    std::string errorMsg;
public:
    RuntimeException(const std::string& err) { errorMsg = err; }
    std::string getMessage() const { return errorMsg; }
};

class EqualLines: public RuntimeException {
public:
    EqualLines(): RuntimeException("The lines are equal: infinite intersection") {};
};

class ParallelLines: public RuntimeException {
public:
    ParallelLines(): RuntimeException("The lines are parallel: no intersection") {};
};

class Line {
public:
    Line(double slope, double y_intercept): a(slope), b(y_intercept) {};
    double intersect(const Line L) const throw(ParallelLines, EqualLines);
    double getSlope() const {return a;};
    double getIntercept() const {return b;};

private:
    double a;
    double b;
};
