#include "utils.h"
#include <iostream>
#include <sstream>


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
