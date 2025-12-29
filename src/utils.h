#pragma once
#ifndef UTILS_H
#define UTILS_H

#include <cstring>
#include <iostream>
#include <string>

#ifndef NDEBUG
#define ASSERT(condition, message)                                             \
    do {                                                                       \
        if (!(condition)) {                                                    \
            std::cerr << "Assertion `" #condition "` failed in " << __FILE__   \
                      << " line " << __LINE__ << ": " << message << std::endl; \
            std::terminate();                                                  \
        }                                                                      \
    } while (false)
#else
#define ASSERT(condition, message) \
    do {                           \
    } while (false)
#endif

void prettyError(int line, int col, const std::string &msg, const std::string &source);

void fail(const std::string &message);
void warn(const std::string &message);
void info(const std::string &message);

template <typename T_in, typename T_out>
T_out bitcast(const T_in &in) {
    static_assert(sizeof(T_in) == sizeof(T_out), "bitcast: size mismatch");
    T_out out;
    memcpy(&out, &in, sizeof(T_in));
    return out;
}

#endif
