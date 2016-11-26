#include "Student.hpp"

Student::Student(std::string name, time_t birthdate, Gender gender,
                 bool is_teaching_assistant, unsigned int teaching_section_id,
                 bool is_research_assistant)
    : Person(name, birthdate, gender),
      _is_teaching_assistant(is_teaching_assistant),
      _teaching_section_id(teaching_section_id)
    {}

StudyLevel Student::get_level() const {
    return student_level;
}
