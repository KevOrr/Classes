#include <string>
#include <ctime>

#include "Person.hpp"
#include "util.hpp"

Person::Person(std::string name, time_t birthdate, Gender gender)
    : name(name), birthdate(birthdate), gender(gender)
    {}

unsigned int Person::get_id() const {
    return id;
}

std::string Person::get_name() const {
    return name;
}

time_t Person::get_birthdate() const {
    return birthdate;
}

Gender Person::get_gender() const {
    return gender;
}

