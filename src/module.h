#pragma once
#ifndef MODULE_H
#define MODULE_H

#include "ast/ast.h"

#include <string>
#include <unordered_map>
#include <unordered_set>
#include "typechecking/tables.h"

struct Module {
  public:
    std::string canon_name;
    std::string path;
    std::string name;

    uint32_t id;

    std::shared_ptr<Program> ast;

    std::unordered_map<std::string, std::shared_ptr<ASTNode>> exports;
    std::unordered_map<std::string, std::shared_ptr<Module>> imports;

    std::string source_code;

    std::unordered_set<std::string> externLinkage; // Probably an inaccurate name, but who cares

    SymbolTable symbols;
    bool typechecking = false;
    bool typechecked = false;

    std::string canonicalizeName(const std::string &s) const {
        return canon_name + "." + s;
    }

    std::shared_ptr<Module> getImportedModule(std::vector<std::string> path) const {
        if (path.empty()) {
            return nullptr;
        }
        // Remove first element
        auto cur_find = path[0];

        if (path.size() == 1) {
            if (auto it = imports.find(cur_find); it != imports.end()) {
                return it->second;
            } else {
                return nullptr;
            }
        }

        // Recursive resolution of imports
        if (auto it = imports.find(cur_find); it != imports.end()) {
            auto next_module = it->second;
            path.erase(path.begin());
            return next_module->getImportedModule(path);
        }
        return nullptr;
    }
};

#endif
