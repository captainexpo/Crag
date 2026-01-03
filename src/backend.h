#pragma once
#ifndef BACKEND_H
#define BACKEND_H

#include "ast/ast.h"
#include "module_resolver.h"
#include <memory>

typedef enum {
    LLVM,
    // TODO: QBE,
} BackendType;

enum OptLevel {
    Debug,
    Release,
};

typedef struct {
    BackendType backend;
    OptLevel opt_level;
    bool do_runtime_safety;
} CompilerOptions;

class Backend {
  public:
    virtual ~Backend() = default;
    virtual void generate(std::shared_ptr<Module> module) = 0;
    virtual void emitIrToFile(const std::string &filepath) = 0;
    virtual void emitObjectToFile(const std::string &filepath) = 0;
    virtual void compileObjectFileToExecutable(const std::string &object_filepath,
                                               const std::filesystem::path &executable_filepath,
                                               const std::filesystem::path &runtime_path,
                                               bool no_runtime, std::string additional_compiler_args = "") = 0;
};

#endif
