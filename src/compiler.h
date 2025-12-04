#pragma once
#ifndef COMPILER_H
#define COMPILER_H

#include "backend.h"
#include "backends/llvmcodegen.h"
#include <llvm/IR/Module.h>
#include <memory>
#include <unordered_map>


void initializeLLVM();
std::shared_ptr<Backend> compileModule(const std::string &raw_filepath, llvm::LLVMContext &context, CompilerOptions options);

#endif // COMPILER_H
