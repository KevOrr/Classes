#ifndef _OOD_PROJECT_DEPARTMENT_H_
#define _OOD_PROJECT_DEPARTMENT_H_

#include <string>
#include <vector>

class University;

class Department {
    friend class University;

private:
    unsigned int id;
    std::string name;
    std::vector<unsigned int> _course_section_ids;
    std::vector<unsigned int> _student_ids;
    std::vector<unsigned int> _teacher_ids;

public:
    Department(std::string name);

    const std::string & get_name();
    void add_person(unsigned int person_id);
    void remove_person(unsigned int person_id);
};

#endif
