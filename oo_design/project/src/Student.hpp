#include "Student.hpp"
#include "util.hpp"

Student::Student(std::string name, time_t birthdate, Gender gender,
                 bool is_teaching_assistant, unsigned int teaching_section_id,
                 bool is_research_assistant)
    : Person(name, birthdate, gender),
      is_teaching_assistant(is_teaching_assistant),
      teaching_section_id(teaching_section_id)
    {}

StudyLevel Student::get_level() {
    return student_level
}

