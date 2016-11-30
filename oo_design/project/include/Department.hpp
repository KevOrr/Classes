#ifndef _OOD_PROJECT_DEPARTMENT_H_
#define _OOD_PROJECT_DEPARTMENT_H_

#include <string>
#include <set>
#include <ostream>

class University;

class Department {
    friend class University;
    friend std::ostream& operator<<(std::ostream&, const Department&);

private:
    unsigned int id;
    std::string name;
    std::set<unsigned int> _course_section_ids;
    std::set<unsigned int> _student_ids;
    std::set<unsigned int> _teacher_ids;

public:
    Department(std::string name);

    unsigned int get_id() const;
    std::string get_name() const;
    const std::set<unsigned int>& course_section_ids() const;
    const std::set<unsigned int>& student_ids() const;
    const std::set<unsigned int>& teacher_ids() const;

    bool add_course(unsigned int course_id);
    bool remove_course(unsigned int course_id);

    bool add_student(unsigned int student_id);
    bool remove_student(unsigned int student_id);

    bool add_teacher(unsigned int teacher_id);
    bool remove_teacher(unsigned int teacher_id);
};

#endif

