#ifndef _OOD_PROJECT_UNIVERSITY_H_
#define _OOD_PROJECT_UNIVERSITY_H_

#include <set>
#include <utility>
#include <vector>

#include "Course.hpp"
#include "CourseSection.hpp"
#include "Student.hpp"
#include "Teacher.hpp"

class University {
private:
    std::set<std::pair<unsigned int, unsigned int> > studentsInClasses;
    std::set<std::pair<unsigned int, unsigned int> > teachersInClasses;

    std::vector<Course> _courses;
    std::vector<CourseSection> _course_sections;
    std::vector<Student> _students;
    std::vector<Teacher> _teachers;

public:
    University();

    const std::vector<Course> & courses;
    const std::vector<CourseSection> & course_sections;
    const std::vector<Student> & students;
    const std::vector<Teacher> teachers;

    void enrollStudent(unsigned int student_id, unsigned int section_id);
    void assignTeacher(unsigned int teacher_id, unsigned int section_id);
};

#endif

