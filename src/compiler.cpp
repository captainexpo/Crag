#include "compiler.h"
#include "codegen.h"
#include "module_resolver.h"
#include "utils.h"
#include "typecheck.h"
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

  // Print ast for debugging
  std::cout << mod->ast->toString();

  auto codegen = IRGenerator("mainmod", context, moduleResolver);
  for (auto &[path, module] : moduleResolver.moduleCache()) {
    std::cout << "Generating code for module: " << path << "\n";
    codegen.generate(module);
    // Check for errors after each module
    if (!codegen.ok())
      break;
    // Set insert point back to main module after each compiled module
    codegen.prepareForNewModule();
  }
  if (!codegen.ok()) {
    std::cout << codegen.errors().size() << " errors during code generation:\n";
    for (const auto &err : codegen.errors()) {
      prettyError(err.first ? err.first->line : -1,
                  err.first ? err.first->col : -1, err.second, mod->source_code);
    }
    std::cout << "Code generation failed due to previous errors.\n";
    return nullptr;
  }
  std::cout << "Compilation succeeded.\n";
  return codegen.takeModule();
}
