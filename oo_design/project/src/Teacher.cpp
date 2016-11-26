#include <ostream>
#include <map>

#include "Teacher.hpp"

TeachingRole Teacher::get_role() const { return role; }

TeachingRole Teacher::set_role(TeachingRole new_role) {
    TeachingRole old_role = role;
    role = new_role;
    return old_role;
}

std::ostream& operator<<(std::ostream& os, const Teacher& teacher) {
    // Print generic Person stuff
    os << static_cast<const Person&>(teacher);

    std::map<TeachingRole, const char*> roles {
        {TeachingRole::LECTURER, "lecturer"},
        {TeachingRole::ADJUNCT, "adjunct"},
        {TeachingRole::PROFESSOR, "professor"}
    };
    os << "Teaching role: " << roles[teacher.role] << std::endl;

    return os;
}
