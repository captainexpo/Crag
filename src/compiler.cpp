#include "compiler.h"
#include "codegen.h"
#include "module_resolver.h"
#include "typecheck.h"
#include "utils.h"
#include "llvm/TargetParser/Host.h"
#include <filesystem>
#include <llvm/Support/TargetSelect.h>
#include <memory>

void initializeLLVM() {
  static bool initialized = false;
  if (!initialized) {
    llvm::InitializeNativeTarget();
    llvm::InitializeNativeTargetAsmPrinter();
    llvm::InitializeNativeTargetAsmParser();
    initialized = true;
  }
}
std::shared_ptr<llvm::Module> compileModule(const std::string &raw_filepath, llvm::LLVMContext &context) {

  auto moduleResolver = ModuleResolver(std::filesystem::path(raw_filepath).parent_path());
  auto mod = moduleResolver.loadModule(std::filesystem::path(raw_filepath).filename().string());

  auto typeChecker = TypeChecker();

  typeChecker.check(mod);

  if (!typeChecker.ok()) {
    // report errors per module
    std::cout << typeChecker.errors().size() << " errors during type checking:\n";
    for (const auto &err : typeChecker.errors()) {
      prettyError(err.first ? err.first->line : -1,
                  err.first ? err.first->col : -1, err.second, mod->source_code);
    }
    return nullptr;
  }

  if (moduleResolver.hasDependencyCycle()) {
    std::cerr << "Error: Cyclic dependencies detected among modules.\n";
    return nullptr;
  }

  auto codegen = LLVMCodegen("mainmod", context, moduleResolver);
  bool has_errors = false;
  for (std::string path : moduleResolver.getBestOrder()) {
    auto module = moduleResolver.getModule(path);
    std::cout << "Generating code for module: " << path << "\n";
    codegen.generate(module);
    // Check for errors after each module
    if (!codegen.ok()) {
      std::cerr << "In module '" << module->canon_name << "'\n";
      std::cerr << codegen.errors().size() << " errors during code generation:\n";
      for (const auto &err : codegen.errors()) {
        std::cerr << err.second << "\n";
        prettyError(err.first ? err.first->line : -1,
                    err.first ? err.first->col : -1, err.second, module->source_code);
      }
      has_errors = true;
    }
    // Set insert point back to main module after each compiled module
    codegen.prepareForNewModule();
    codegen.clearErrors();
  }
  if (has_errors) {
    std::cerr << "Code generation failed due to previous errors.\n";
    return nullptr;
  }
  std::cout << "Compilation succeeded.\n";

  auto llvmmod = codegen.takeModule();
  llvmmod->setTargetTriple(llvm::sys::getDefaultTargetTriple());

  return llvmmod;
}
