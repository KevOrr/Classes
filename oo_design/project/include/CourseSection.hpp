#ifndef _OOD_PROJECT_COURSESECTION_H_
#define _OOD_PROJECT_COURSESECTION_H_

#include <map>

class CourseSection {
private:
    unsigned int id;
    unsigned int course_id;
    std::map<unsigned int, float> grades;

public:
    CourseSection(unsigned int course_id);

    unsigned int get_course() const;
    unsigned int get_grade(unsigned int student_id) const;
};

#endif

