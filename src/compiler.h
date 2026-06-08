#pragma once
#ifndef COMPILER_H
#define COMPILER_H

#include "backend.h"
#ifdef NO_LLVM
#include <llvm/IR/Module.h>
#endif
#include <memory>

void initializeLLVM();
std::shared_ptr<Backend> compileModule(const std::string &raw_filepath, CompilerOptions options);

#endif // COMPILER_H
