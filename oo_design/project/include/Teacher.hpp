#ifndef _OOD_PROJECT_TEACHER_H_
#define _OOD_PROJECT_TEACHER_H_

#include "Person.hpp"

enum class TeachingRole {LECTURER, ADJUNCT, PROFESSOR};

class Teacher : public Person {
using Person::Person;

private:
    TeachingRole role;
    
};

#endif

