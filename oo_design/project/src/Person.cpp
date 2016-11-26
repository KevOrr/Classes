#include <string>
#include <ctime>
#include <ostream>

#include "Person.hpp"

extern std::string time_to_string(std::string, time_t);

Person::Person(std::string name, time_t birthdate, Gender gender)
    : id(0), name(name), birthdate(birthdate), gender(gender)
    {}

unsigned int Person::get_id() const { return id; }

std::string Person::get_name() const { return name; }

time_t Person::get_birthdate() const { return birthdate; }

Gender Person::get_gender() const { return gender; }

std::ostream& operator<<(std::ostream& os, const Person& person) {
    std::map<Gender, const char*> genders{{Gender::MALE, "male"}, {Gender::FEMALE, "female"},
                                                                  {Gender::OTHER, "other"}};

    os << "Name: " << person.name << "\nID: " << person.id << std::endl;
    os << "Birthdate: " << time_to_string("%F", person.birthdate) << std::endl;
    os << "Gender: " << genders[person.gender] << std::endl;

    return os;
}
