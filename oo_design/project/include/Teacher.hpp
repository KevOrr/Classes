#ifndef _OOD_PROJECT_TEACHER_H_
#define _OOD_PROJECT_TEACHER_H_

#include "Person.hpp"

class University;

enum class TeachingRole {LECTURER, ADJUNCT, PROFESSOR};

class Teacher : public Person {
    using Person::Person;
    friend class University;

private:
    TeachingRole role;

public:
    TeachingRole get_role() const;
    TeachingRole set_role(TeachingRole role);
};

#endif

