#ifndef MODULE_RESOLVER_H
#define MODULE_RESOLVER_H

#include "ast.h"
#include <filesystem>
#include <fstream>
#include <iostream>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include "utils.h"
#include "parser.h" // assume you have a Parser class that can parse source text




struct Module {
public:
  std::string canon_name;
  std::string path;
  std::shared_ptr<Program> ast;
  std::unordered_map<std::string, std::shared_ptr<ASTNode>> exports;
  std::unordered_map<std::string, std::shared_ptr<Module>> imports;

  std::string source_code;

  std::unordered_set<std::string> externFunctions;

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

  // Retrieve or load a module AST
  std::shared_ptr<Module> loadModule(const std::string &import_path) {
    // Check cache first
    auto abs_path = resolveModulePath(import_path);
    auto filename = abs_path.substr(abs_path.find_last_of("/\\") + 1);

    auto module_canonical_name = canonicalModuleName(m_base_path, import_path);

    std::cout << "Resolving module: " << import_path << " -> " << filename << "\n";
    if (auto it = m_module_cache.find(abs_path); it != m_module_cache.end()) {
      return it->second;
    }

    // Read file
    std::string source = readFile(abs_path);

    std::cout << "Loading module from: " << abs_path << "\n";
    std::cout << source << "\n";

    // Parse it
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

    module->canon_name = module_canonical_name;
    module->path = abs_path;
    module->ast = ast;
    module->source_code = source;

    // Cache it
    m_module_cache[abs_path] = module;

    // Collect exports
    for (const auto &imp : ast->declarations) {
      if (auto importDecl = std::dynamic_pointer_cast<ImportDeclaration>(imp)) {
        module->imports[importDecl->alias] = loadModule(importDecl->path);
        continue;
      }

      // Should check for pub-ness in the future, but for now, export all
      if (auto varDecl = std::dynamic_pointer_cast<VariableDeclaration>(imp)) {
        module->exports[varDecl->name] = varDecl;
      } else if (auto funcDecl = std::dynamic_pointer_cast<FunctionDeclaration>(imp)) {
        module->exports[funcDecl->name] = funcDecl;
        if (funcDecl->is_extern)
          module->externFunctions.insert(funcDecl->name);
      } else if (auto structDecl = std::dynamic_pointer_cast<StructDeclaration>(imp)) {
        module->exports[structDecl->name] = structDecl;
      } else if (auto enumDecl = std::dynamic_pointer_cast<EnumDeclaration>(imp)) {
        module->exports[enumDecl->name] = enumDecl;
      }
    }

    return module;
  }

  std::string resolveModulePath(const std::string &import_path) const {
    // TODO: handle relative paths, extensions, etc.
    return m_base_path + "/" + import_path;
  }

  const std::unordered_map<std::string, std::shared_ptr<Module>> &moduleCache() const {
    return m_module_cache;
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
