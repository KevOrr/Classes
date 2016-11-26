#include "Teacher.hpp"

TeachingRole Teacher::get_role() const { return role; }

TeachingRole Teacher::set_role(TeachingRole new_role) {
    TeachingRole old_role = role;
    role = new_role;
    return old_role;
}
