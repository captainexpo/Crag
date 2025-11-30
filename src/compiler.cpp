#include "compiler.h"
#include "module_resolver.h"
#include "typecheck.h"
#include "utils.h"
#include "backend.h"
#include "backends/llvmcodegen.h"
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

std::string backendToString(BackendType backend) {
  switch (backend) {
    case LLVM:
      return "LLVM";
    default:
      return "Unknown";
  }
}

std::shared_ptr<llvm::Module> compileModule(const std::string &raw_filepath, llvm::LLVMContext &context, CompilerOptions options) {

  if (options.backend != LLVM) {
    fail("Unsupported backend: " + backendToString(options.backend));
  }

  // See if file is empty or doesn't exist
  if (raw_filepath.empty()) {
    fail("No input file specified.");
  }
  if (!std::filesystem::exists(raw_filepath)) {
    fail("Input file does not exist: " + raw_filepath);
  }
  if (std::filesystem::is_empty(raw_filepath)) {
    warn("Input file is empty: " + raw_filepath);
    info("An empty program has no code to compile. Try writing a main function.");
    return nullptr;
  }

  auto path = std::filesystem::path(raw_filepath);
  path = std::filesystem::absolute(path);
  auto moduleResolver = ModuleResolver(path.parent_path());
  auto mod = moduleResolver.loadModule(path.filename().string());

  auto typeChecker = TypeChecker();

  typeChecker.check(mod);

  if (!typeChecker.ok()) {
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

  // Call llvm codegen directly here, should probably make it more modular later
  auto codegen = LLVMCodegen("mainmod", context, moduleResolver, options);
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
        std::cerr << err.what() << "\n";
        prettyError(err.node()? err.node()->line : -1,
                    err.node()? err.node()->col : -1, err.what(), module->source_code);
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
  llvmmod->setDarwinTargetVariantTriple(llvm::sys::getDefaultTargetTriple());

  return llvmmod;
}
