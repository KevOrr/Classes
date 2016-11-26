#include <utility>
#include <ostream>

#include "University.hpp"
#include "CourseSection.hpp"
#include "Student.hpp"
#include "Teacher.hpp"
#include "Department.hpp"

University::University(const std::string& name) : name(name) {}

std::vector<CourseSection>::const_iterator University::course_sections_cbegin() const {
    return _course_sections.cbegin();
}

std::vector<CourseSection>::const_iterator University::course_sections_cend() const {
    return _course_sections.cbegin();
}

std::vector<Student>::const_iterator University::students_cbegin() const {
    return _students.cbegin();
}

std::vector<Student>::const_iterator University::students_cend() const {
    return _students.cend();
}

std::vector<Teacher>::const_iterator University::teachers_cbegin() const {
    return _teachers.cbegin();
}

std::vector<Teacher>::const_iterator University::teachers_cend() const {
    return _teachers.cend();
}

std::vector<Department>::const_iterator University::departments_cbegin() const {
    return _departments.cbegin();
}

std::vector<Department>::const_iterator University::departments_cend() const {
    return _departments.cend();
}

// return true if student successfully added, false if already enrolled
bool University::enroll_student(unsigned int student_id, unsigned int section_id) {
    return students_in_classes.insert(std::pair<unsigned int, unsigned int>(student_id, section_id)).second;
}

// return true if student successfully removed, false if not in class section
bool University::remove_student(unsigned int student_id, unsigned int section_id) {
    auto position = students_in_classes.find(std::pair<unsigned int, unsigned int>(student_id, section_id));
    if (position == students_in_classes.end())
        return false;

    students_in_classes.erase(position);
    return true;
}

// return true if teacher successfully added, false if already assigned
bool University::assign_teacher(unsigned int teacher_id, unsigned int section_id) {
    return teachers_in_classes.insert(std::pair<unsigned int, unsigned int>(teacher_id, section_id)).second;
}

// return true if teacher successfully removed, false if not in class section
bool University::remove_teacher(unsigned int teacher_id, unsigned int section_id) {
    auto position = teachers_in_classes.find(std::pair<unsigned int, unsigned int>(teacher_id, section_id));
    if (position == teachers_in_classes.end())
        return false;

    teachers_in_classes.erase(position);
    return true;
}

std::ostream& operator<<(std::ostream& os, const University& university) {
    os << "Name: " << university.name << std::endl
       << "Courses: " << university._course_sections.size() << std::endl
       << "Students: " << university._students.size() << std::endl
       << "Teachers: " << university._teachers.size() << std::endl
       << "Departments: " << university._departments.size() << std::endl;

    return os;

}
