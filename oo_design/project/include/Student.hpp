#ifndef _OOD_PROJECT_STUDENT_H_
#define _OOD_PROJECT_STUDENT_H_

#include <ctime>

#include "Person.hpp"
#include "util.hpp"

class Student : public Person {
friend class University;

private:
    StudyLevel student_level;

public:
    Student(std::string name, time_t birthdate, Gender gender,
            bool is_teaching_assistant=false, unsigned int teaching_section_id=0u,
            bool is_research_assistant=false);

    StudyLevel get_level();

};

#endif

