#include <ctime>
#include <iostream>

#include "Student.hpp"
#include "Person.hpp"

int main() {
    Student student("Kevin Orr", time(NULL), Gender::MALE);
    std::cout << student;

    return 0;
}

