#ifndef COMPILER_H
#define COMPILER_H

#include "backend.h"
#include <llvm/IR/Module.h>
#include <memory>
#include <unordered_map>

typedef enum {
  LLVM,
  // TODO: QBE,
} BackendType;

typedef struct {
  BackendType backend;
  OptLevel opt_level;
} CompilerOptions;

void initializeLLVM();
std::shared_ptr<llvm::Module> compileModule(const std::string &raw_filepath, llvm::LLVMContext &context, CompilerOptions options);

#endif // COMPILER_H
