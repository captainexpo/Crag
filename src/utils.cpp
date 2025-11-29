#include "utils.h"
#include <iostream>
#include <sstream>


void fail(const std::string &message) {
  std::cerr << "Fatal Error: " << message << "\n";
  exit(1);
}

void warn(const std::string &message) {
  std::cout << "Warning: " << message << "\n";
}

void info(const std::string &message) {
  std::cout << "Info: " << message << "\n";
}

void prettyError(int line, int col, const std::string &msg,const std::string &source) {
  std::istringstream srcStream(source);
  std::string srcLine;
  int currentLine = 1;

  while (std::getline(srcStream, srcLine)) {
    if (currentLine == line) {
      std::cerr << srcLine << "\n";
      std::cerr << std::string(col - 1, ' ') << "^\n";
      std::cerr << "Error at " << line << ":" << col << " - " << msg << "\n";
      return;
    }
    currentLine++;
  }
} // TODO: Put this in a header file
