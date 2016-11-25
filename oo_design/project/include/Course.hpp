#ifndef _OOD_PROJECT_COURSE_H_
#define _OOD_PROJECT_COURSE_H_

enum class StudyLevel { UNDERGRADUATE, GRADUATE };

class Course {
friend class University;

private:
    unsigned int id;
    StudyLevel level;

public:
    Course(StudyLevel level);

    unsigned int get_id() const;
    StudyLevel get_level() const;
};

#endif

