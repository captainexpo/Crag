#pragma once
#include <optional>
#ifndef MODULE_RESOLVER_H
#define MODULE_RESOLVER_H

#include "ast/ast.h"
#include "parser.h" // assume you have a Parser class that can parse source text
#include "utils.h"
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
        // if (!out.empty())
        //     out += ".";
        out += it->string();
    }
    return out;
}

class ModuleResolver {
  public:
    explicit ModuleResolver(std::string base_path)
        : m_base_path(std::move(base_path)) {
        initializeBuiltinShortcuts();
        loadUserShortcuts();
    }

    // adjacency list: module -> list of modules it depends on
    std::unordered_map<std::string, std::vector<std::string>> dependencyGraph;

    std::shared_ptr<Module> loadRoot(const std::string &entry) {
        return loadModule(entry, std::filesystem::path(m_base_path));
    }

    std::shared_ptr<Module> loadModule(
        const std::string &import_path,
        const std::filesystem::path &from_dir) {
        // First try to resolve as a shortcut
        std::string resolved_path = resolveShortcut(import_path, from_dir);

        auto abs_path =
            std::filesystem::absolute(from_dir / resolved_path);

        if (!std::filesystem::exists(abs_path)) {
            std::cerr << "Error: Module file not found: " << abs_path << "\n";
            exit(1);
        }
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

    void addShortcut(const std::string &shortcut, const std::string &path) {
        m_shortcuts[shortcut] = path;
    }

    const std::unordered_map<std::string, std::string> &getShortcuts() const {
        return m_shortcuts;
    }

  private:
    std::string m_base_path;
    std::unordered_map<std::string, std::shared_ptr<Module>> m_module_cache;
    std::unordered_map<std::string, std::string> m_shortcuts;
    std::unordered_map<std::string, std::filesystem::path> m_shortcut_base_dirs; // Base directory for each shortcut

    void initializeBuiltinShortcuts() {
        m_shortcuts["stdlib"] = "<STDLIB>/std.crag";
        m_shortcuts["string"] = "<STDLIB>/string.crag";
        m_shortcuts["vector"] = "<STDLIB>/vector.crag";
        m_shortcuts["random"] = "<STDLIB>/random.crag";
        m_shortcuts["libc"] = "<STDLIB>/libc.crag";
    }

    void loadUserShortcuts() {
        loadShortcutsFromFile(".cragrc");
    }

    void loadShortcutsFromFile(const std::string &filename) {
        std::filesystem::path config_path = std::filesystem::path(m_base_path) / filename;
        std::cout << "Looking for user shortcuts in: " << config_path << std::endl;

        if (!std::filesystem::exists(config_path)) {
            return;
        }

        try {
            std::ifstream file(config_path);
            if (!file.is_open()) {
                return;
            }

            std::string line;
            while (std::getline(file, line)) {
                if (line.empty() || line[0] == '#')
                    continue;

                size_t eq_pos = line.find('=');
                if (eq_pos != std::string::npos) {
                    std::string key = line.substr(0, eq_pos);
                    std::string value = line.substr(eq_pos + 1);

                    key.erase(0, key.find_first_not_of(" \t"));
                    key.erase(key.find_last_not_of(" \t") + 1);
                    value.erase(0, value.find_first_not_of(" \t"));
                    value.erase(value.find_last_not_of(" \t") + 1);

                    if (value.size() >= 2 && value.front() == '"' && value.back() == '"') {
                        value = value.substr(1, value.size() - 2);
                    }

                    m_shortcuts[key] = value;
                    m_shortcut_base_dirs[key] = config_path.parent_path();
                }
            }
        } catch (const std::exception &e) {
            std::cerr << "Warning: Error loading shortcuts from " << config_path
                      << ": " << e.what() << std::endl;
        }
    }

    std::string resolveShortcut(const std::string &import_path, const std::filesystem::path &from_dir) const {

        if (import_path.find('.') != std::string::npos ||
            import_path.find('/') != std::string::npos ||
            import_path.find('\\') != std::string::npos) {
            return import_path;
        }

        if (auto it = m_shortcuts.find(import_path); it != m_shortcuts.end()) {
            std::string shortcut_path = it->second;

            if (shortcut_path.substr(0, 9) == "<STDLIB>/") {
                auto stdlib_path_opt = findStdlibPath();
                if (stdlib_path_opt) {
                    std::filesystem::path stdlib_path = *stdlib_path_opt;
                    info("stdlib path: " + stdlib_path.string());
                    std::string relative_path = shortcut_path.substr(9);
                    return (stdlib_path / relative_path).string();
                } else {
                    warn("Warning: Could not find stdlib. Make sure CRAGSTD is set or the stdlib is installed in a common location.");
                }
                fail("Warning: <STDLIB> shortcut used but stdlib not found. Have you set CRAGSTD or installed the stdlib? Falling back to default relative path.");
            }

            if (auto base_it = m_shortcut_base_dirs.find(import_path); base_it != m_shortcut_base_dirs.end()) {
                std::filesystem::path abs_path = base_it->second / shortcut_path;
                return abs_path.lexically_normal().string();
            }

            return shortcut_path;
        }

        return import_path;
    }

    std::optional<std::filesystem::path> findStdlibPath() const {
        // Get env variable for CRAGSTD=...
        // If it exists, check if stdlib is there
        if (const char *env_p = std::getenv("CRAGSTD")) {
            std::filesystem::path stdlib_path = std::filesystem::path(env_p) / "std.crag";
            // Expand ~ to home directory if present
            if (stdlib_path.string().find("~") == 0) {
                if (const char *home_p = std::getenv("HOME")) {
                    stdlib_path = std::filesystem::path(home_p) / stdlib_path.string().substr(1);
                }
            }
            if (std::filesystem::exists(stdlib_path)) {
                info("Found stdlib at CRAGSTD path: " + stdlib_path.string());
                return stdlib_path.parent_path();
            }
        }

        // Otherwise, check for common install dir
        std::filesystem::path common_paths[] = {
            "/usr/local/lib/crag/stdlib",
            "/usr/lib/crag/stdlib",
            std::filesystem::path(std::getenv("HOME") ? std::getenv("HOME") : "") / ".local/lib/crag/stdlib"};
        for (const auto &path : common_paths) {
            std::filesystem::path stdlib_path = path / "std.crag";
            if (stdlib_path.string().find("~") == 0) {
                if (const char *home_p = std::getenv("HOME")) {
                    stdlib_path = std::filesystem::path(home_p) / stdlib_path.string().substr(1);
                }
            }
            info("Checking for stdlib at: " + stdlib_path.string());
            if (std::filesystem::exists(stdlib_path)) {
                info("Found stdlib at common path: " + stdlib_path.string());
                return stdlib_path.parent_path();
            }
        }
        warn("Could not find stdlib in common paths. Checked /usr/local/lib/crag/stdlib, /usr/lib/crag/stdlib, and ~/.local/lib/crag/stdlib");
        return std::nullopt;
    }

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
