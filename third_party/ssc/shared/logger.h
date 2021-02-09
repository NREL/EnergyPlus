#ifndef SAM_SIMULATION_CORE_LOGGER_H
#define SAM_SIMULATION_CORE_LOGGER_H

#include <ostream>
#include <iostream>

class nullstream : public std::ostream {
public:
    nullstream() : std::ostream(nullptr) {}
    nullstream(const nullstream &) : std::ostream(nullptr) {}
};

template <class T>
const nullstream &operator<<(nullstream &&os, const T &value) {
    return os;
}

static nullstream ns;

class logger {
private:
    std::ostream& _out_stream;

public:
    logger(): _out_stream(ns) {}
    explicit logger(std::ostream& stream): _out_stream(stream) {}

    explicit operator std::ostream& () {
        return _out_stream;
    }

    template<typename T>
    logger& operator<< (const T& data)
    {
        _out_stream << data;
        return *this;
    }
};

#endif //SAM_SIMULATION_CORE_LOGGER_H
