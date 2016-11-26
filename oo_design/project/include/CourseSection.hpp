#ifndef _OOD_PROJECT_COURSESECTION_H_
#define _OOD_PROJECT_COURSESECTION_H_

#include <map>
#include <ostream>

class University;

enum class StudyLevel { UNDERGRADUATE, GRADUATE };

class CourseSection {
    friend class University;
    friend std::ostream& operator<<(std::ostream&, const CourseSection&);

private:
    unsigned int id;
    std::string name;
    StudyLevel level;
    std::map<unsigned int, float> grades;

public:
    CourseSection(const std::string& name, StudyLevel level);

    unsigned int get_id() const;
    float get_grade(unsigned int student_id) const;
    float set_grade(unsigned int student_id, float new_grade);
};

#endif
