#ifndef _OOD_PROJECT_UNIVERSITY_H_
#define _OOD_PROJECT_UNIVERSITY_H_

enum class CourseLevel { UNDERGRADUATE, GRADUATE };

class Course {
private:
    unsigned int id;
    CourseLevel level;

public:
    Course(unsigned int id, CourseLevel level);
    Course(Course);
    unsigned int getID();
    CourseLevel getLevel();
}

#endif
