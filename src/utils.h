#ifndef UTILS_H
#define UTILS_H
#include <string>

void prettyError(int line, int col, const std::string &msg,const std::string &source);

void fail(const std::string &message);
void warn(const std::string &message);
void info(const std::string &message);


#endif
