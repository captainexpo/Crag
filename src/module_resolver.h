#pragma once
#ifndef MODULE_RESOLVER_H
#define MODULE_RESOLVER_H

#include "ast/ast.h"
#include "parser.h" // assume you have a Parser class that can parse source text
#include "utils.h"
#include <algorithm>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iostream>
#include <string>
#include <unordered_map>
#include <unordered_set>

struct Module {
  public:
    std::string canon_name;
    std::string path;
    std::shared_ptr<Program> ast;
    std::unordered_map<std::string, std::shared_ptr<ASTNode>> exports;
    std::unordered_map<std::string, std::shared_ptr<Module>> imports;

    std::string source_code;

    std::unordered_set<std::string> externLinkage; // Probably an inaccurate name, but who cares

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

inline std::string canonicalModuleName(const std::filesystem::path &abs_path) {
    // example: /a/b/c/foo.cr → a.b.c.foo
    std::filesystem::path p = abs_path;
    p.replace_extension();

    std::string out;
    for (auto it = p.begin(); it != p.end(); ++it) {
        if (!out.empty())
            out += ".";
        out += it->string();
    }
    return out;
}

class ModuleResolver {
  public:
    explicit ModuleResolver(std::string base_path)
        : m_base_path(std::move(base_path)) {}

    // adjacency list: module -> list of modules it depends on
    std::unordered_map<std::string, std::vector<std::string>> dependencyGraph;

    std::shared_ptr<Module> loadRoot(const std::string &entry) {
        return loadModule(entry, std::filesystem::path(m_base_path));
    }

    std::shared_ptr<Module> loadModule(
        const std::string &import_path,
        const std::filesystem::path &from_dir) {
        auto abs_path =
            std::filesystem::absolute(from_dir / import_path);

        abs_path = std::filesystem::canonical(abs_path);

        // reuse cache
        if (auto it = m_module_cache.find(abs_path.string());
            it != m_module_cache.end()) {
            return it->second;
        }

        std::string source = readFile(abs_path.string());
        if (source.empty()) {
            std::cerr << "Error: Could not read module file: "
                      << abs_path << "\n";
            exit(1);
        }

        Lexer lexer(source);
        auto tokens = lexer.tokenize();
        Parser parser(tokens);
        auto ast = parser.parse();

        if (!parser.ok()) {
            for (const auto &err : parser.errors()) {
                prettyError(err.line, err.col, err.message, source);
            }
            exit(1);
        }

        auto module = std::make_shared<Module>();
        module->path = abs_path.string();
        module->canon_name = canonicalModuleName(abs_path);
        module->ast = ast;
        module->source_code = source;

        dependencyGraph[module->canon_name];
        m_module_cache[module->path] = module;

        auto module_dir = abs_path.parent_path();

        for (const auto &decl : ast->declarations) {
            if (auto importDecl =
                    std::dynamic_pointer_cast<ImportDeclaration>(decl)) {

                auto imported =
                    loadModule(importDecl->path, module_dir);

                module->imports[importDecl->alias] = imported;
                dependencyGraph[module->canon_name]
                    .push_back(imported->canon_name);
                continue;
            }

            // exports
            if (auto f = std::dynamic_pointer_cast<FunctionDeclaration>(decl)) {
                if (f->is_pub)
                    module->exports[f->name] = f;
                if (f->is_extern || f->attributes.count("noprefix"))
                    module->externLinkage.insert(f->name);
            } else if (auto s =
                           std::dynamic_pointer_cast<StructDeclaration>(decl)) {
                if (s->is_pub)
                    module->exports[s->name] = s;
            } else if (auto e =
                           std::dynamic_pointer_cast<EnumDeclaration>(decl)) {
                if (e->is_pub)
                    module->exports[e->name] = e;
            } else if (auto v =
                           std::dynamic_pointer_cast<VariableDeclaration>(decl)) {
                if (v->is_pub)
                    module->exports[v->name] = v;
                if (v->is_extern)
                    module->externLinkage.insert(v->name);
            }
        }

        return module;
    }
    std::string resolveModulePath(const std::string &import_path) const {
        return m_base_path + "/" + import_path;
    }

    const std::unordered_map<std::string, std::shared_ptr<Module>> &moduleCache() const {
        return m_module_cache;
    }

    bool hasDependencyCycle() {
        // Simple DFS to detect cycles
        std::unordered_set<std::string> visited;
        std::unordered_set<std::string> recStack;

        std::function<bool(const std::string &)> dfs = [&](const std::string &mod_name) {
            if (recStack.find(mod_name) != recStack.end())
                return true; // cycle detected
            if (visited.find(mod_name) != visited.end())
                return false; // already processed

            visited.insert(mod_name);
            recStack.insert(mod_name);

            for (const auto &dep : dependencyGraph[mod_name]) {
                if (dfs(dep))
                    return true;
            }

            recStack.erase(mod_name);
            return false;
        };

        for (const auto &[mod_name, _] : dependencyGraph) {
            if (dfs(mod_name))
                return true;
        }
        return false;
    }

    std::vector<std::string> getBestOrder() {
        std::vector<std::string> order;
        std::unordered_set<std::string> visited;

        std::function<void(const std::string &)> dfs = [&](const std::string &mod_name) {
            if (visited.find(mod_name) != visited.end())
                return;

            visited.insert(mod_name);

            for (const auto &dep : dependencyGraph[mod_name]) {
                dfs(dep);
            }

            order.push_back(mod_name);
        };

        for (const auto &[mod_name, _] : dependencyGraph) {
            dfs(mod_name);
        }

        return order;
    }
    std::shared_ptr<Module> getModule(const std::string &module_name) const {
        for (const auto &[path, module] : m_module_cache) {
            if (module->canon_name == module_name) {
                return module;
            }
        }
        return nullptr;
    }

    std::string getRootModulePath() const {
        if (m_module_cache.empty())
            return "";
        return m_module_cache.begin()->second->canon_name;
    }
  private:
    std::string m_base_path;
    std::unordered_map<std::string, std::shared_ptr<Module>> m_module_cache;

    static std::string readFile(const std::string &path) {
        std::ifstream file(path);
        if (!file) {
            std::cerr << "Could not open file: " << path << "\n";
            return "";
        }
        std::stringstream buffer;
        buffer << file.rdbuf();
        return buffer.str();
    }
};
#endif
