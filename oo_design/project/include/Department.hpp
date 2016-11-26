#ifndef _OOD_PROJECT_DEPARTMENT_H_
#define _OOD_PROJECT_DEPARTMENT_H_

#include <string>
#include <set>

class University;

class Department {
    friend class University;

private:
    unsigned int id;
    std::string name;
    std::set<unsigned int> _course_section_ids;
    std::set<unsigned int> _student_ids;
    std::set<unsigned int> _teacher_ids;

public:
    Department(std::string name);

    std::string get_name() const;

    bool add_student(unsigned int student_id);
    bool remove_student(unsigned int student_id);

    bool add_teacher(unsigned int teacher_id);
    bool remove_teacher(unsigned int teacher_id);
};

#endif

