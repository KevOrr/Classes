#include <stdexcept>
#include <ostream>

#include "CourseSection.hpp"

CourseSection::CourseSection(const std::string& name, StudyLevel level)
    : name(name),
      level(level)
{}

unsigned int CourseSection::get_id() const {
    return id;
}

// std::vector::at throws std::out_of_range if key is not found
float CourseSection::get_grade(unsigned int student_id) const {
    if (grades.find(student_id) != grades.end())
        return grades.at(student_id);
    else
        return -1.0f;
}

float CourseSection::set_grade(unsigned int student_id, float new_grade) {
    float old_grade = get_grade(student_id);
    grades[student_id] = new_grade;
    return old_grade;
}
std::ostream& operator<<(std::ostream& os, const CourseSection& section) {
    os << "Name: " << section.name << std::endl << "ID: " << section.id << std::endl;

    return os;
}
