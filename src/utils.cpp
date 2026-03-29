#include "utils.h"
#include <algorithm>
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

void prettyError(int line, int col, const std::string &msg, const std::string &source) {
    if (line <= 0 || col <= 0) {
        std::cerr << "error: " << msg << "\n";
        return;
    }

    std::istringstream srcStream(source);
    std::string srcLine;
    int currentLine = 1;

    while (std::getline(srcStream, srcLine)) {
        if (currentLine == line) {
            int clamped_col = std::max(1, std::min(col, static_cast<int>(srcLine.size()) + 1));
            std::cerr << "error: " << msg << "\n";
            std::cerr << " --> " << line << ":" << clamped_col << "\n";
            std::cerr << "  |\n";
            std::cerr << line << " | " << srcLine << "\n";
            std::cerr << "  | " << std::string(clamped_col - 1, ' ') << "^\n";
            return;
        }
        currentLine++;
    }

    std::cerr << "error: " << msg << " (at " << line << ":" << col << ")\n";
}
