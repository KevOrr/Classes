#include <string>
#include <ctime>
#include <ostream>

#include "Person.hpp"

extern std::string time_to_string(std::string, time_t);

Person::Person(std::string name, struct tm birthdate, Gender gender)
    : id(0), name(name), birthdate(birthdate), gender(gender)
    {}

unsigned int Person::get_id() const { return id; }

std::string Person::get_name() const { return name; }

struct tm Person::get_birthdate() const { return birthdate; }

Gender Person::get_gender() const { return gender; }

std::ostream& operator<<(std::ostream& os, const Person& person) {
    std::map<Gender, const char*> genders{{Gender::MALE, "male"}, {Gender::FEMALE, "female"},
                                                                  {Gender::OTHER, "other"}};
    char date_str[16];
    strftime(date_str, 16, "%Y-%m-%d", &person.birthdate);

    os << "Name: " << person.name << "\nID: " << person.id << std::endl;
    os << "Birthdate: " << date_str << std::endl;
    os << "Gender: " << genders[person.gender] << std::endl;

    return os;
}
