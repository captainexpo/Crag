#pragma once
#ifndef DIAGNOSTIC_H
#define DIAGNOSTIC_H

#include "utils.h"
#include <string>
#include <vector>

struct Diagnostic {
    int line;
    int col;
    std::string message;
    std::string module_name;
    std::string source_code;
};

struct DiagnosticSink {
    std::vector<Diagnostic> diags;

    bool empty() const { return diags.empty(); }
    size_t size() const { return diags.size(); }

    void report() const {
        for (const auto &d : diags) {
            prettyError(d.line, d.col, d.message, d.source_code, d.module_name);
        }
    }
};

#endif // DIAGNOSTIC_H
