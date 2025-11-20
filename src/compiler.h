#ifndef COMPILER_H
#define COMPILER_H

#include <llvm/IR/Module.h>
#include <memory>

void initializeLLVM();
std::shared_ptr<llvm::Module> compileModule(const std::string &raw_filepath, llvm::LLVMContext &context);

#endif // COMPILER_H
