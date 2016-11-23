#ifndef _OOD_PROJECT_PERSON_H_
#define _OOD_PROJECT_PERSON_H_

#include <string>
#include <ctime>

#include "util.hpp"

// Abstract class
class Person {
protected:
    unsigned int id;
    std::string name;
    time_t birthdate;
    Gender gender;

    Person(std::string name, time_t birthdate, Gender gender);

public:
    unsigned int get_id() const;
    std::string get_name() const;
    time_t get_birthdate() const;
    Gender get_gender() const;
};

#endif

