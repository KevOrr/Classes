#include <utility>
#include <algorithm>
#include <string>
#include <limits>
#include <ostream>
#include <fstream>
#include <sstream>
#include <ios>
#include <iomanip>
#include <cctype>
#include <iostream>
#include <time.h>

#include "University.hpp"
#include "CourseSection.hpp"
#include "Student.hpp"
#include "Teacher.hpp"
#include "Department.hpp"

University::University(const std::string& name) : name(name) {}

std::string& University::ltrim(std::string& str) {
    // http://stackoverflow.com/a/217605/1529586
    // left trim line
    str.erase(str.begin(), std::find_if(str.begin(), str.end(),
                                      std::not1(std::ptr_fun<int, int>(std::isspace))));
    return str;
}

bool University::read_in(std::ifstream& departments_file, std::ifstream& courses_file,
                         std::ifstream& students_file, std::ifstream& teachers_file,
                         std::ifstream& grades_file) {

    bool result = true;

    result &= read_departments(departments_file);
    result &= read_courses(courses_file);
    result &= read_students(students_file);
    result &= read_teachers(teachers_file);
    result &= read_grades(grades_file);

    return result;
}

bool University::read_departments(std::ifstream& file) {
    std::string line;

    while (std::getline(file, line)) {
        std::string name;
        unsigned int id;

        // Skip initial whitespace
        ltrim(line);

        // Ignore lines starting with '#'
        if (line[0] == '#')
            continue;

        // Get name
        auto name_end = line.begin() + line.find(':');
        name.assign(line.begin(), name_end); // store until ':' in `name`
        line.erase(line.begin(), name_end+1);
        ltrim(line);

        std::istringstream rest(line);

        // Get ID
        rest >> id;

        // Add department
        Department department(name);
        department.id = id;
        _departments.push_back(department);
    }

    return file.eof();
}

bool University::read_courses(std::ifstream& file) {
    std::string line;
    while (std::getline(file, line)) {
        std::string name;
        unsigned int course_id;
        StudyLevel level;
        unsigned int department_id;

        ltrim(line);

        if (line[0] == '#')
            continue;

        // Get name
        auto name_end = line.begin() + line.find(':');
        name.assign(line.begin(), name_end);
        line.erase(line.begin(), name_end+1);

        std::istringstream rest(line);

        // Get id
        rest >> std::ws >> course_id;

        // Get level
        char clevel;
        rest >> std::ws >> clevel;
        clevel = std::tolower(clevel);
        if (clevel != 'u' && clevel != 'g')
            continue;
        level = clevel == 'u' ? StudyLevel::UNDERGRADUATE : StudyLevel::GRADUATE;

        // Get department id
        rest >> std::ws >> department_id;

        // Add course
        CourseSection section(name, level);
        section.id = course_id;
        _course_sections.push_back(section);

        // Add course to its department
        for (auto it=_departments.begin(), end=_departments.end(); it != end; ++it)
            if (it->get_id() == department_id)
                it->add_course(course_id);

    }

    return file.eof();
}

bool University::read_students(std::ifstream& file) {
    std::string line;
    while (std::getline(file, line)) {
        ltrim(line);
        if (line[0] == '#')
            continue;

        // Get name
        std::string name;
        auto name_end = line.begin() + line.find(':');
        name.assign(line.begin(), name_end);
        line.erase(line.begin(), name_end+1);

        std::istringstream rest(line);

        // Get id
        unsigned int student_id;
        rest >> std::ws >> student_id;

        // Get birthdate
        struct tm birthdate;
        rest >> std::ws;
        std::string bd;
        std::getline(rest, bd, ' ');
        strptime(bd.c_str(), "%Y-%m-%d", &birthdate);

        // Get gender
        Gender gender;
        char cgender;
        std::map<char, Gender> genders{
            {'m', Gender::MALE}, {'f', Gender::FEMALE}, {'o', Gender::OTHER}
        };
        rest >> std::ws >> cgender;
        cgender = std::tolower(cgender);
        if (!genders.count(cgender))
            continue;
        gender = genders[cgender];

        // Get level
        StudyLevel level;
        char clevel;
        rest >> std::ws >> clevel;
        clevel = std::tolower(clevel);
        if (clevel != 'u' && clevel != 'g')
            continue;
        level = clevel == 'u' ? StudyLevel::UNDERGRADUATE : StudyLevel::GRADUATE;

        // Get department
        unsigned int department_id;
        rest >> std::ws >> department_id;

        // Get courses
        std::set<unsigned int> course_ids;
        unsigned int next_course;
        rest >> std::ws;
        while (rest >> next_course) {
            course_ids.insert(next_course);
            if (rest.peek() == ',')
                rest.ignore();
            if (rest.peek() == ' ')
                break;
        }

        // Get TA/research info
        bool is_ta;
        char cis_ta;
        rest >> std::ws >> cis_ta;
        cis_ta = tolower(cis_ta);
        if (cis_ta != 't' && cis_ta != 'f')
            continue;
        is_ta = cis_ta == 't';

        unsigned int ta_course;
        rest >> std::ws >> ta_course;

        bool is_research;
        char cis_research;
        rest >> std::ws >> cis_research;
        if (cis_research != 't' && cis_research != 'f')
            continue;
        is_research = cis_research == 't';

        // Add student
        Student student(name, timegm(&birthdate), gender, is_ta, ta_course, is_research);
        student.id = student_id;
        student.set_level(level);
        _students.push_back(student);

        // Add student/course relationships
        for (auto it=course_ids.begin(), end=course_ids.end(); it != end; ++it)
            students_in_classes.insert(std::pair<unsigned int, unsigned int>(student_id, *it));

        // Add student to its department
        for (auto it=_departments.begin(), end=_departments.end(); it != end; ++it)
            if (it->get_id() == department_id)
                it->add_student(student_id);
    }

    return file.eof();
}

bool University::read_teachers(std::ifstream& file) {
    std::string line;
    while (std::getline(file, line)) {
        ltrim(line);
        if (line[0] == '#')
            continue;

        // Get name
        std::string name;
        auto name_end = line.begin() + line.find(':');
        name.assign(line.begin(), name_end);
        line.erase(line.begin(), name_end+1);

        std::istringstream rest(line);

        // Get id
        unsigned int teacher_id;
        rest >> std::ws >> teacher_id;

        // Get birthdate
        struct tm birthdate;
        rest >> std::ws;
        std::string bd;
        std::getline(rest, bd, ' ');
        strptime(bd.c_str(), "%Y-%m-%d", &birthdate);

        // Get gender
        std::map<char, Gender> genders{
            {'m', Gender::MALE}, {'f', Gender::FEMALE}, {'o', Gender::OTHER}
        };
        char cgender;
        rest >> std::ws >> cgender;
        cgender = std::tolower(cgender);
        if (!genders.count(cgender))
            continue;
        Gender gender = genders[cgender];

        // Get department
        unsigned int department_id;
        rest >> std::ws >> department_id;

        // Get role
        char crole;
        std::map<char, TeachingRole> roles{
            {'p', TeachingRole::PROFESSOR},
            {'a', TeachingRole::PROFESSOR},
            {'t', TeachingRole::LECTURER}
        };
        rest >> std::ws >> crole;
        crole = std::tolower(crole);
        if (!roles.count(crole))
            continue;
        std::cout << crole;
        TeachingRole role = roles[crole];

        // Get courses
        std::set<unsigned int> course_ids;
        unsigned int next_course;
        rest >> std::ws;
        while (rest >> next_course) {
            course_ids.insert(next_course);
            if (rest.peek() == ',')
                rest.ignore();
            if (rest.peek() == ' ')
                break;
        }

        // Add teacher
        Teacher teacher(name, timegm(&birthdate), gender);
        teacher.id = teacher_id;
        teacher.set_role(role);
        _teachers.push_back(teacher);

        // Add teacher/course relationships
        for (auto it=course_ids.begin(), end=course_ids.end(); it != end; ++it)
            teachers_in_classes.insert(std::pair<unsigned int, unsigned int>(teacher_id, *it));

        // Add teacher to its department
        for (auto it=_departments.begin(), end=_departments.end(); it != end; ++it)
            if (it->get_id() == department_id)
                it->add_teacher(teacher_id);
    }

    return file.eof();
}

bool University::read_grades(std::ifstream& file) {
    std::string line;
    while (std::getline(file, line)) {
        std::istringstream ssline(line);

        ssline >> std::ws;
        if (ssline.eof() || ssline.peek() == '#')
            continue;

        unsigned int student_id, course_id;
        float grade;

        ssline >> std::skipws >> student_id >> course_id >> grade;

        for (auto it=_course_sections.begin(), end=_course_sections.end(); it != end; ++it)
            if (it->id == course_id)
                it->set_grade(student_id, grade);
    }

    return file.eof();
}

std::vector<CourseSection>::const_iterator University::course_sections_cbegin() const {
    return _course_sections.cbegin();
}

std::vector<CourseSection>::const_iterator University::course_sections_cend() const {
    return _course_sections.cend();
}

std::vector<Student>::const_iterator University::students_cbegin() const {
    return _students.cbegin();
}

std::vector<Student>::const_iterator University::students_cend() const {
    return _students.cend();
}

std::vector<Teacher>::const_iterator University::teachers_cbegin() const {
    return _teachers.cbegin();
}

std::vector<Teacher>::const_iterator University::teachers_cend() const {
    return _teachers.cend();
}

std::vector<Department>::const_iterator University::departments_cbegin() const {
    return _departments.cbegin();
}

std::vector<Department>::const_iterator University::departments_cend() const {
    return _departments.cend();
}

// return true if student successfully added, false if already enrolled
bool University::enroll_student(unsigned int student_id, unsigned int section_id) {
    return students_in_classes.insert(std::pair<unsigned int, unsigned int>(student_id, section_id)).second;
}

// return true if student successfully removed, false if not in class section
bool University::remove_student(unsigned int student_id, unsigned int section_id) {
    auto position = students_in_classes.find(std::pair<unsigned int, unsigned int>(student_id, section_id));
    if (position == students_in_classes.end())
        return false;

    students_in_classes.erase(position);
    return true;
}

// return true if teacher successfully added, false if already assigned
bool University::assign_teacher(unsigned int teacher_id, unsigned int section_id) {
    return teachers_in_classes.insert(std::pair<unsigned int, unsigned int>(teacher_id, section_id)).second;
}

// return true if teacher successfully removed, false if not in class section
bool University::remove_teacher(unsigned int teacher_id, unsigned int section_id) {
    auto position = teachers_in_classes.find(std::pair<unsigned int, unsigned int>(teacher_id, section_id));
    if (position == teachers_in_classes.end())
        return false;

    teachers_in_classes.erase(position);
    return true;
}

const Student* University::get_student(unsigned int id) const {
    auto pos = std::find_if(_students.begin(), _students.end(), [id](const Student& it){
            return it.id == id;
        });

    if (pos != _students.end())
        return &*pos;
    else
        return NULL;
}

const Teacher* University::get_teacher(unsigned int id) const {
    auto pos = std::find_if(_teachers.begin(), _teachers.end(), [id](const Teacher& it){
            return it.id == id;
        });

    if (pos != _teachers.end())
        return &*pos;
    else
        return NULL;
}

const CourseSection* University::get_course_section(unsigned int id) const {
    auto pos = std::find_if(_course_sections.begin(), _course_sections.end(), [id](const CourseSection& it){
            return it.id == id;
        });

    if (pos != _course_sections.end())
        return &*pos;
    else
        return NULL;
}

const Department* University::get_department(unsigned int id) const {
    auto pos = std::find_if(_departments.begin(), _departments.end(), [id](const Department& it){
            return it.id == id;
        });

    if (pos != _departments.end())
        return &*pos;
    else
        return NULL;
}

std::set<std::pair<unsigned int, unsigned int> >::const_iterator
University::students_in_classes_cbegin() const {
    return students_in_classes.cbegin();
}

std::set<std::pair<unsigned int, unsigned int> >::const_iterator
University::students_in_classes_cend() const {
    return students_in_classes.cend();
}

std::set<std::pair<unsigned int, unsigned int> >::const_iterator
University::teachers_in_classes_cbegin() const {
    return teachers_in_classes.cbegin();
}

std::set<std::pair<unsigned int, unsigned int> >::const_iterator
University::teachers_in_classes_cend() const {
    return teachers_in_classes.cend();
}

std::ostream& operator<<(std::ostream& os, const University& university) {
    os << "Name: " << university.name << std::endl
       << "Courses: " << university._course_sections.size() << std::endl
       << "Students: " << university._students.size() << std::endl
       << "Teachers: " << university._teachers.size() << std::endl
       << "Departments: " << university._departments.size() << std::endl;

    return os;
}