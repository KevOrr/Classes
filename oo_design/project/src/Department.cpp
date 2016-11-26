#include "Department.hpp"

#include <string>

Department::Department(std::string name)
    : name(name)
{}

std::string Department::get_name() const {
    return name;
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

