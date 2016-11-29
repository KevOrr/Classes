#include <fstream>
#include <iostream>
#include <ios>

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
    "6)  Show class student is enrolled in\n"
    "7)  Show teachers assigned to class\n"
    "8)  Show classes assigned to teacher\n"
    "9)  Show students in department\n"
    "10) Show teachers in department\n"
    "11) Show courses in department\n"
    "0)  Exit\n"
    "> ";

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
        if (std::cin.fail()) {
            std::cin.clear();
            std::cout << "Please enter one of the numbers shown on the menu\n";
            continue;
        }

        switch (choice) {
        case 0:
            return 0;

        // Show students
        case 1:
            std::cout << "STUDENTS" << std::endl;
            for (auto it=university.students_cbegin(), end=university.students_cend(); it != end; ++it)
                std::cout << *it << std::endl;
            break;

        // Show teachers
        case 2:
            std::cout << "TEACHERS" << std::endl;
            for (auto it=university.teachers_cbegin(), end=university.teachers_cend(); it != end; ++it)
                std::cout << *it << std::endl;
            break;

        // Show departments
        case 3:
            std::cout << "DEPARTMENTS" << std::endl;
            for (auto it=university.departments_cbegin(), end=university.departments_cend();
                 it != end; ++it)
                std::cout << *it << std::endl;
            break;

        // Show courses
        case 4:
            std::cout << "COURSES" << std::endl;
            for (auto it=university.course_sections_cbegin(), end=university.course_sections_cend();
                 it != end; ++it)
                std::cout << *it << std::endl;
            break;

        // Show students in class
        case 5:
            std::cout << "Enter course ID: ";
            unsigned int course_id;
            std::cin >> course_id;

            for (auto it=university.students_in_classes_cbegin(),
                     end=university.students_in_classes_cend(); it != end; ++it) {
                if (it->second == course_id) {
                    const Student* student = university.get_student(it->first);
                    if (!student) continue;
                    std::cout << "ID: " << student->get_id() << ", Name: " << student->get_name();
                }
            }
            break;
        }
    }
}
