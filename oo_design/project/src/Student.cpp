#include <ostream>
#include <cstdio>
#include <string>
#include <ctime>

#include "Student.hpp"

extern std::string time_to_string(std::string, time_t);

Student::Student(std::string name, time_t birthdate, Gender gender,
                 bool is_teaching_assistant, unsigned int teaching_section_id,
                 bool is_research_assistant)
    : Person(name, birthdate, gender),
      student_level(StudyLevel::UNDERGRADUATE),
      _is_teaching_assistant(is_teaching_assistant),
      _teaching_section_id(teaching_section_id),
      _is_research_assistant(is_research_assistant)
{}

StudyLevel Student::get_level() const {
    return student_level;
}

StudyLevel Student::set_level(StudyLevel new_level) {
    StudyLevel old_level = student_level;
    student_level = new_level;
    return old_level;
}

bool Student::is_teaching_assistant() const {
    return _is_teaching_assistant;
}

bool Student::is_teaching_assistant(bool new_value) {
    bool old_value = _is_teaching_assistant;
    _is_teaching_assistant = new_value;
    return old_value;
}

unsigned int Student::get_teaching_section_id() const {
    return _teaching_section_id;
}

unsigned int Student::set_teaching_section_id(unsigned int new_value) {
    unsigned int old_value = _teaching_section_id;
    _teaching_section_id = new_value;
    return old_value;
}

bool Student::is_research_assistant() const {
    return _is_research_assistant;
}

bool Student::is_research_assistant(bool new_value) {
    bool old_value = _is_teaching_assistant;
    _is_teaching_assistant = new_value;
    return old_value;
}

std::ostream& operator<<(std::ostream& os, Student student) {
    std::map<Gender, const char*> genders{{Gender::MALE, "male"}, {Gender::FEMALE, "female"},
                                          {Gender::OTHER, "other"}};
    std::map<StudyLevel, const char*> levels{{StudyLevel::UNDERGRADUATE, "undergrad"},
                                             {StudyLevel::GRADUATE, "graduate"}};
    
    os << "Name: " << student.name << "\nID: " << student.id << std::endl;
    os << "Birthdate: " << time_to_string("%F", student.birthdate) << std::endl;
    os << "Gender: " << genders[student.gender] << std::endl;
    os << "Level: " << levels[student.student_level] << std::endl;

    os << "Teaching assistant: " << (student._is_teaching_assistant ? "true" : "false") << std::endl;
    if (student._is_teaching_assistant)
        os << "Teaching assistant course section id: " << student._teaching_section_id << std::endl;

    os << "Research assistant: " << (student._is_research_assistant ? "true" : "false") << std::endl;

    return os;
}

