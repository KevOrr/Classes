#ifndef _OOD_PROJECT_UNIVERSITY_H_
#define _OOD_PROJECT_UNIVERSITY_H_

#include <set>
#include <utility>
#include <vector>
#include <ostream>
#include <fstream>

#include "CourseSection.hpp"
#include "Student.hpp"
#include "Teacher.hpp"
#include "Department.hpp"

class University {
    friend std::ostream& operator<<(std::ostream&, const University&);

private:
    std::string name;

    // student/class and teacher/class relationships
    std::set<std::pair<unsigned int, unsigned int> > students_in_classes;
    std::set<std::pair<unsigned int, unsigned int> > teachers_in_classes;

    std::vector<CourseSection> _course_sections;
    std::vector<Student> _students;
    std::vector<Teacher> _teachers;
    std::vector<Department> _departments;

    bool read_departments(std::ifstream&);
    bool read_courses(std::ifstream&);
    bool read_students(std::ifstream&);
    bool read_teachers(std::ifstream&);
    bool read_grades(std::ifstream&);

public:
    University(const std::string&);

    bool read_in(std::ifstream&, std::ifstream&, std::ifstream&, std::ifstream&, std::ifstream&);

    Student* get_student(unsigned int id) const;
    Teacher* get_teacher(unsigned int id) const;
    CourseSection* get_course_section(unsigned int id) const;
    Department* get_department(unsigned int id) const;

    // const references for each internal container
    const std::vector<CourseSection>& course_sections() const;
    const std::vector<Student>& students() const;
    const std::vector<Teacher>& teachers() const;
    const std::vector<Department>& departments() const;
    const std::set<std::pair<unsigned int, unsigned int> >& students_and_classes() const;
    const std::set<std::pair<unsigned int, unsigned int> >& teachers_and_classes() const;

    // Modify relationships
    bool enroll_student(unsigned int student_id, unsigned int section_id);
    bool remove_student(unsigned int student_id, unsigned int section_id);
    bool assign_teacher(unsigned int teacher_id, unsigned int section_id);
    bool remove_teacher(unsigned int teacher_id, unsigned int section_id);
};

#endif
