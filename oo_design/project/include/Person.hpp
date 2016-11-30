#ifndef _OOD_PROJECT_PERSON_H_
#define _OOD_PROJECT_PERSON_H_

#include <string>
#include <ctime>
#include <map>
#include <ostream>

enum class Gender { MALE, FEMALE, OTHER };

// Abstract class
class Person {
    friend std::ostream& operator<<(std::ostream&, const Person&);

protected:
    unsigned int id;
    std::string name;
    struct tm birthdate;
    Gender gender;

    // declaring constructor as protected makes this class abstract
    Person(std::string name, struct tm birthdate, Gender gender);

public:

    unsigned int get_id() const;
    std::string get_name() const;
    struct tm get_birthdate() const;
    Gender get_gender() const;
};

#endif
