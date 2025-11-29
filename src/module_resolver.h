#ifndef MODULE_RESOLVER_H
#define MODULE_RESOLVER_H

#include "ast.h"
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
};

inline std::string canonicalModuleName(const std::string &root_path, const std::string &path) {
  auto abspath = std::filesystem::absolute(std::filesystem::path(root_path) / path);

  // remove extension
  auto base = abspath.stem().string();

  // replace disallowed separators with "_"
  for (auto &ch : base) {
    if (ch == '/' || ch == '\\' || ch == '-') {
      ch = '_';
    }
  }
  return base;
}
class ModuleResolver {
public:
  explicit ModuleResolver(std::string base_path)
      : m_base_path(std::move(base_path)) {}

  // adjacency list: module -> list of modules it depends on
  std::unordered_map<std::string, std::vector<std::string>> dependencyGraph;

  std::shared_ptr<Module> loadModule(const std::string &import_path) {
    auto abs_path = resolveModulePath(import_path);
    auto module_canonical_name = canonicalModuleName(m_base_path, import_path);

    // Reuse cached module
    if (auto it = m_module_cache.find(abs_path); it != m_module_cache.end()) {
      return it->second;
    }

    // ---- Load & parse source ----
    std::string source = readFile(abs_path);
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

    // ---- Create module ----
    auto module = std::make_shared<Module>();
    module->canon_name = module_canonical_name;
    module->path = abs_path;
    module->ast = ast;
    module->source_code = source;

    dependencyGraph[module->canon_name];

    m_module_cache[abs_path] = module;

    for (const auto &decl : ast->declarations) {
      if (auto importDecl = std::dynamic_pointer_cast<ImportDeclaration>(decl)) {
        auto importedMod = loadModule(importDecl->path);
        module->imports[importDecl->alias] = importedMod;

        dependencyGraph[module->canon_name].push_back(importedMod->canon_name);
        continue;
      }

      // exports
      if (auto funcDecl = std::dynamic_pointer_cast<FunctionDeclaration>(decl)) {
        if (funcDecl->is_pub)
          module->exports[funcDecl->name] = funcDecl;
        if (funcDecl->is_extern || funcDecl->attributes.count("noprefix"))
          module->externLinkage.insert(funcDecl->name);
      } else if (auto structDecl = std::dynamic_pointer_cast<StructDeclaration>(decl)) {
        if (structDecl->is_pub)
          module->exports[structDecl->name] = structDecl;
      } else if (auto enumDecl = std::dynamic_pointer_cast<EnumDeclaration>(decl)) {
        if (enumDecl->is_pub)
          module->exports[enumDecl->name] = enumDecl;
      } else if (auto varDecl = std::dynamic_pointer_cast<VariableDeclaration>(decl)) {
        if (varDecl->is_pub)
          module->exports[varDecl->name] = varDecl;
        if (varDecl->is_extern)
          module->externLinkage.insert(varDecl->name);
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
