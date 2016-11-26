#ifndef _OOD_PROJECT_COURSESECTION_H_
#define _OOD_PROJECT_COURSESECTION_H_

#include <map>

class University;

class CourseSection {
    friend class University;

private:
    unsigned int id;
    std::map<unsigned int, float> grades;

public:
    CourseSection();

    unsigned int get_id() const;
    float get_grade(unsigned int student_id) const;
    float set_grade(unsigned int student_id, float new_grade);
};

#endif
