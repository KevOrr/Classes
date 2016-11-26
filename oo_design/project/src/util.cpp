#include <string>
#include <ctime>

std::string time_to_string(std::string fmt, time_t t) {
    tm *timeinfo;
    timeinfo = gmtime(&t);

    fmt += '\a';
    std::string buffer;
    buffer.resize(fmt.size());

    int len = strftime(&buffer[0], buffer.size(), fmt.c_str(), timeinfo);
    while (len == 0) {
        buffer.resize(buffer.size()*2);
        len = strftime(&buffer[0], buffer.size(), fmt.c_str(), timeinfo);
    }

    buffer.resize(len-1);
    return buffer;
}

