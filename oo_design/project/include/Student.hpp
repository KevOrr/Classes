#ifndef _OOD_PROJECT_STUDENT_H_
#define _OOD_PROJECT_STUDENT_H_

#include <ctime>

#include "Person.hpp"
#include "util.hpp"

class Student : public Person {
friend class University;

private:
    StudyLevel student_level;

    bool is_teaching_assistant;
    unsigned int teach_section_id;

    bool is_research_assistant;

public:
    Student(std::string name, time_t birthdate, Gender gender);

    StudyLevel get_level();

};

#endif

