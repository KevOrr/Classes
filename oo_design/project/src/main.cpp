#include <fstream>
#include <iostream>
#include <ios>
#include <limits>

#include "University.hpp"
#include "Student.hpp"
#include "Person.hpp"

const char * MENU =
    "\n"
    "1)  Show students\n"
    "2)  Show teachers\n"
    "3)  Show departments\n"
    "4)  Show courses\n"
    "5)  Show students enrolled in class\n"
    "6)  Show classes student is enrolled in\n"
    "7)  Show teachers assigned to class\n"
    "8)  Show classes assigned to teacher\n"
    "9)  Show students in department\n"
    "10) Show teachers in department\n"
    "11) Show courses in department\n"
    "0)  Exit\n"
    "> ";

bool cin_reset() {
    if (std::cin.fail()) {
        std::cin.clear();
        std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
        return true;
    }
    return false;
}

int main() {
    University university("USF");

    std::ifstream departments_file, courses_file, students_file, teachers_file, grades_file;
    departments_file.open("test_files/departments.txt");
    courses_file.open("test_files/courses.txt");
    students_file.open("test_files/students.txt");
    teachers_file.open("test_files/teachers.txt");
    grades_file.open("test_files/grades.txt");

    university.read_in(departments_file, courses_file, students_file, teachers_file, grades_file);

    while (true) {
        // Get menu choice
        std::cout << MENU;
        int choice;
        std::cin >> choice;
        if (cin_reset()) {
            std::cerr << "Please enter one of the numbers shown on the menu\n";
            continue;
        }

        switch (choice) {
        case 0:
            return 0;

        // Show students
        case 1:
        {
            std::cout << "STUDENTS" << std::endl;
            for (auto it=university.students().cbegin(), end=university.students().cend();
                 it != end; ++it)
                std::cout << *it << std::endl;
            break;
        }

        // Show teachers
        case 2:
        {
            std::cout << "TEACHERS" << std::endl;
            for (auto it=university.teachers().cbegin(), end=university.teachers().cend();
                 it != end; ++it)
                std::cout << *it << std::endl;
            break;
        }

        // Show departments
        case 3:
        {
            std::cout << "DEPARTMENTS" << std::endl;
            for (auto it=university.departments().cbegin(), end=university.departments().cend();
                 it != end; ++it)
                std::cout << *it << std::endl;
            break;
        }

        // Show courses
        case 4:
        {
            std::cout << "COURSES" << std::endl;
            for (auto it=university.course_sections().cbegin(), end=university.course_sections().cend();
                 it != end; ++it)
                std::cout << *it << std::endl;
            break;
        }

        // Show students in class
        case 5:
        {
            std::cout << "Enter course ID: ";
            unsigned int course_id;
            std::cin >> course_id;
            if (cin_reset())
                continue;
            if (!university.get_course_section(course_id)) {
                std::cerr << course_id << " is not a valid course" << std::endl;
                continue;
            }

            for (auto it=university.students_and_classes().cbegin(),
                     end=university.students_and_classes().cend(); it != end; ++it) {
                if (it->second == course_id) {
                    const Student* student = university.get_student(it->first);
                    if (!student) continue;
                    std::cout << "ID: " << student->get_id() << ", Name: " << student->get_name()
                              << std::endl;
                }
            }

            break;
        }

        // Show classes student is in
        case 6:
        {
            std::cout << "Enter student ID: ";
            unsigned int student_id;
            std::cin >> student_id;
            if (cin_reset())
                continue;
            if (!university.get_student(student_id)) {
                std::cerr << student_id << " is not a valid student" << std::endl;
                continue;
            }

            for (auto it=university.students_and_classes().cbegin(),
                     end=university.students_and_classes().cend(); it != end; ++it) {
                if (it->first == student_id) {
                    const CourseSection* section = university.get_course_section(it->second);
                    if (!section) continue;
                    std::cout << "ID: " << section->get_id() << ", Name: " << section->get_name()
                              << std::endl;
                }
            }

            break;
        }

        // Show teachers in class
        case 7:
        {
            std::cout << "Enter course ID: ";
            unsigned int course_id;
            std::cin >> course_id;
            if (cin_reset())
                continue;
            if (!university.get_course_section(course_id)) {
                std::cerr << course_id << " is not a valid course" << std::endl;
                continue;
            }

            for (auto it=university.teachers_and_classes().cbegin(),
                     end=university.teachers_and_classes().cend(); it != end; ++it) {
                if (it->second == course_id) {
                    const Teacher* teacher = university.get_teacher(it->first);
                    if (!teacher) continue;
                    std::cout << "ID: " << teacher->get_id() << ", Name: " << teacher->get_name()
                              << std::endl;
                }
            }

            break;
        }

        // Show classes teacher is in
        case 8:
        {
            std::cout << "Enter teacher ID: ";
            unsigned int teacher_id;
            std::cin >> teacher_id;
            if (cin_reset())
                continue;
            if (!university.get_teacher(teacher_id)) {
                std::cerr << teacher_id << " is not a valid teacher" << std::endl;
                continue;
            }

            for (auto it=university.teachers_and_classes().cbegin(),
                     end=university.teachers_and_classes().cend(); it != end; ++it) {
                if (it->first == teacher_id) {
                    const CourseSection* section = university.get_course_section(it->second);
                    if (!section) continue;
                    std::cout << "ID: " << section->get_id() << ", Name: " << section->get_name()
                              << std::endl;
                }
            }

            break;
        }

        // Show students in department
        case 9:
        {
            std::cout << "Enter department ID: ";
            unsigned int department_id;
            std::cin >> department_id;
            if (cin_reset())
                continue;
            const Department* department = university.get_department(department_id);
            if (!department) {
                std::cerr << department_id << " is not a valid department";
                continue;
            }

            for (auto it=department->student_ids().begin(), end=department->student_ids().end();
                 it != end; ++it) {
                const Student* student = university.get_student(*it);
                if (!student) continue;
                std::cout << "ID: " << student->get_id() << ", Name" << student->get_name() << std::endl;
            }
        }
        }
    }
}
