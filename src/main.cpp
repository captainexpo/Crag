
#include "compiler.h"

#include <iostream>
#include <llvm/ADT/StringRef.h>

#include <llvm/Support/CommandLine.h>
#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/raw_ostream.h>
#include <system_error>

#define PRGM_NAME "Toy Compiler"

std::string stripped_file_name(const std::string &path) {
  size_t last_slash = path.find_last_of("/\\");
  size_t last_dot = path.find_last_of('.');
  if (last_dot == std::string::npos || last_dot < last_slash)
    last_dot = path.size();
  return path.substr(last_slash + 1, last_dot - last_slash - 1);
}

std::string dirname(const std::string &path) {
  size_t last_slash = path.find_last_of("/\\");
  if (last_slash == std::string::npos)
    return ".";
  return path.substr(0, last_slash);
}

int main(int argc, char **argv) {
  llvm::InitLLVM X(argc, argv);

  llvm::LLVMContext context;

  // --- Define CLI options ---
  llvm::cl::opt<std::string> inputFilepath(llvm::cl::Positional,
                                           llvm::cl::desc("<source file>"),
                                           llvm::cl::Required);

  llvm::cl::opt<std::string> outputFilepath(
      "o", llvm::cl::desc("Specify output filename"),
      llvm::cl::value_desc("filename"), llvm::cl::init("a.o"));

  llvm::cl::opt<bool> EmitIR(
      "emit-ir", llvm::cl::desc("Emit LLVM IR instead of object code"),
      llvm::cl::init(false));

  llvm::cl::ParseCommandLineOptions(argc, argv, std::string(PRGM_NAME) + "\n");

  auto mod = compileModule(inputFilepath, context);
  if (!mod) {
    std::cerr << "Compilation failed.\n";
    return 1;
  }

  std::string irFilePath = outputFilepath+".tmp.ll";
  std::error_code ec;
  llvm::raw_fd_ostream os(llvm::StringRef(irFilePath), ec);
  if (ec) {
    std::cerr << "Could not open output file: " << ec.message() << "\n";
    return 1;
  }
  mod->print(os, nullptr);

  if (EmitIR) {
    std::cout << "Emitted LLVM IR to " << outputFilepath << "\n";
    return 0;
  }

  // Run clang to compile output_filepath to executable
  std::string exe_name = dirname(outputFilepath) + "/" + stripped_file_name(outputFilepath);
  std::string clang_cmd = "clang " + irFilePath + " -o " + exe_name;
  int ret = system(clang_cmd.c_str());
  if (ret != 0) {
    std::cerr << "Failed to invoke clang to create executable.\n";
    return 1;
  }
  std::cout << "Emitted executable to " << exe_name << "\n";

  return 0;
}
