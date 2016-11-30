#include "Department.hpp"

#include <string>
#include <ostream>

Department::Department(std::string name)
    : name(name)
{}

// Getters
unsigned int Department::get_id() const { return id; }
std::string Department::get_name() const { return name; }
const std::set<unsigned int>& Department::course_section_ids() const { return _course_section_ids; }
const std::set<unsigned int>& Department::student_ids() const { return _student_ids; }
const std::set<unsigned int>& Department::teacher_ids() const { return _teacher_ids; }

// Returns true if course didn't exist before, false if it did
bool Department::add_course(unsigned int course_id) {
    auto result = _course_section_ids.insert(course_id);
    return result.second;
}

// Returns true if course was successfully removed, flase if it didn't exist
bool Department::remove_course(unsigned int course_id) {
    auto position = _course_section_ids.find(course_id);
    if (position == _course_section_ids.end())
        return false;

    _course_section_ids.erase(position);
    return true;
}

// Returns true if student didn't exist before, false if it did
bool Department::add_student(unsigned int student_id) {
    auto result = _student_ids.insert(student_id);
    return result.second;
}

// Returns true if student was successfully removed, flase if it didn't exist
bool Department::remove_student(unsigned int student_id) {
    auto position = _student_ids.find(student_id);
    if (position == _student_ids.end())
        return false;

    _student_ids.erase(position);
    return true;
}

// Returns true if teacher didn't exist before, false if it did
bool Department::add_teacher(unsigned int teacher_id) {
    auto result = _teacher_ids.insert(teacher_id);
    return result.second;
}

// Returns true if teacher was successfully removed, flase if it didn't exist
bool Department::remove_teacher(unsigned int teacher_id) {
    auto position = _teacher_ids.find(teacher_id);
    if (position == _teacher_ids.end())
        return false;

    _teacher_ids.erase(position);
    return true;
}

std::ostream& operator<<(std::ostream& os, const Department& department) {
    os << "Name: " << department.name << std::endl
       << "ID: " << department.id << std::endl
       << "Courses: " << department._course_section_ids.size() << std::endl
       << "Students: " << department._student_ids.size() << std::endl
       << "Teachers: " << department._teacher_ids.size() << std::endl;

    return os;
}
