#ifndef _OOD_PROJECT_UNIVERSITY_H_
#define _OOD_PROJECT_UNIVERSITY_H_

#include <set>
#include <utility>
#include <vector>

#include "CourseSection.hpp"
#include "Student.hpp"
#include "Teacher.hpp"
#include "Department.hpp"

class University {
private:
    // student/class and teacher/class relationships
    std::set<std::pair<unsigned int, unsigned int> > students_in_classes;
    std::set<std::pair<unsigned int, unsigned int> > teachers_in_classes;

    std::vector<CourseSection> _course_sections;
    std::vector<Student> _students;
    std::vector<Teacher> _teachers;
    std::vector<Department> _departments;

public:
    // const iterators for each internal container
    std::vector<CourseSection>::const_iterator course_sections_cbegin() const;
    std::vector<CourseSection>::const_iterator course_sections_cend() const;
    std::vector<Student>::const_iterator students_cbegin() const;
    std::vector<Student>::const_iterator students_cend() const;
    std::vector<Teacher>::const_iterator teachers_cbegin() const;
    std::vector<Teacher>::const_iterator teachers_cend() const;
    std::vector<Department>::const_iterator departments_cbegin() const;
    std::vector<Department>::const_iterator departments_cend() const;

    University();

    // Modify relationships
    bool enroll_student(unsigned int student_id, unsigned int section_id);
    bool remove_student(unsigned int student_id, unsigned int section_id);
    bool assign_teacher(unsigned int teacher_id, unsigned int section_id);
    bool remove_teacher(unsigned int teacher_id, unsigned int section_id);

    // Convenience for Department::add_person and Department::remove_person
    void add_to_department(unsigned int person_id, unsigned int department_id);
    void remove_from_department(unsigned int person_id, unsigned int department_id);
};

#endif

