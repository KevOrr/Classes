#ifndef _OOD_PROJECT_UNIVERSITY_H_
#define _OOD_PROJECT_UNIVERSITY_H_

#include <set>
#include <utility>

#include "Course.hpp"
#include "CourseSection.hpp"
#include "Student.hpp"
#include "Teacher.hpp"

class University {
private:
    std::set<std::pair<unsigned int, unsigned int> > studentsInClasses;
    std::set<std::pair<unsigned int, unsigned int> > teachersInClasses;

    std::set<unsigned int, Course> courses;
    std::set<unsigned int, CourseSection> courseSections;
    std::set<unsigned int, Student> students;
    std::set<unsigned int, Teacher> teachers;

public:
    University();

    const Course & getCourse(unsigned int id);
    const CourseSection & getCourseSection(unsigned int id);
    const Student & getStudent(unsigned int id);
    const Teacher & getTeacher(unsigned int id);



    void enrollStudent(unsigned int student_id, unsigned int section_id);
    void assignTeacher(unsigned int teacher_id, unsigned int section_id);
}

#endif
