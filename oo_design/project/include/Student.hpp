#ifndef _OOD_PROJECT_STUDENT_H_
#define _OOD_PROJECT_STUDENT_H_

#include <ctime>
#include <ostream>

#include "Person.hpp"
#include "CourseSection.hpp"

class University;

class Student : public Person {
friend class University;
friend std::ostream& operator<<(std::ostream&, const Student&);

private:
    StudyLevel student_level;
    bool _is_teaching_assistant;
    unsigned int _teaching_section_id;
    bool _is_research_assistant;

public:
    Student(std::string name, struct tm birthdate, Gender gender,
            bool is_teaching_assistant=false, unsigned int teaching_section_id=0u,
            bool is_research_assistant=false);

    StudyLevel get_level() const;
    StudyLevel set_level(StudyLevel);

    bool is_teaching_assistant() const;
    bool is_teaching_assistant(bool);

    unsigned int get_teaching_section_id() const;
    unsigned int set_teaching_section_id(unsigned int);

    bool is_research_assistant() const;
    bool is_research_assistant(bool);

};

#endif

