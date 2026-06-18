#include "utils.h"
#include <unistd.h>
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

std::string colorCodes[] = {
    "\033[31m", // Red
    "\033[32m", // Green
    "\033[33m", // Yellow
    "\033[34m", // Blue
    "\033[35m", // Magenta
    "\033[36m", // Cyan
    "\033[37m"  // White
};

std::string colorString(const std::string &message, Color color) {
    if (color == Color::Default) {
        return message;
    }
    return colorCodes[static_cast<int>(color)] + message + "\033[0m";
}

void prettyError(int line, int col, const std::string &msg, const std::string &source, const std::string &filename) {
    bool doColor = true;
    if (isatty(fileno(stderr))) {
        doColor = true;
    } else {
        doColor = false;
    }

    std::istringstream srcStream(source);
    std::string srcLine;
    int currentLine = 1;

    while (std::getline(srcStream, srcLine)) {
        if (currentLine == line) {
            int clamped_col = std::max(1, std::min(col, static_cast<int>(srcLine.size()) + 1));
            std::cerr << filename << ":" << line << ":" << col << ": " << colorString("error: ", Color::Red) << msg << "\n";
            std::cerr << srcLine << "\n";
            std::cerr << std::string(clamped_col - 1, ' ') << "^\n";

            return;
        }
        currentLine++;
    }

    int clamped_col = std::max(1, std::min(col, static_cast<int>(srcLine.size()) + 1));
    std::cerr << filename << ":" << line << ":" << col << ": " << colorString("error: ", Color::Red) << msg << "\n";
    std::cerr << srcLine << "\n";
    std::cerr << std::string(clamped_col - 1, ' ') << "^\n";
}
