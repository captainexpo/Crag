
#include "ir.h"
#include "lexer.h"
#include "parser.h"
#include "typecheck.h"
#include <fstream>
#include <iostream>
#include <sstream>

#include <fstream>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/raw_ostream.h>
#include <sstream>

int main(int argc, char **argv) {
  llvm::InitLLVM X(argc, argv);

  // --- Define CLI options ---
  llvm::cl::opt<std::string> inputFilename(llvm::cl::Positional,
                                           llvm::cl::desc("<source file>"),
                                           llvm::cl::Required);

  llvm::cl::opt<std::string> outputFilename(
      "o", llvm::cl::desc("Specify output filename"),
      llvm::cl::value_desc("filename"), llvm::cl::init("a.o"));

  llvm::cl::opt<bool> EmitIR(
      "emit-ir", llvm::cl::desc("Emit LLVM IR instead of object code"),
      llvm::cl::init(false));

  llvm::cl::ParseCommandLineOptions(argc, argv, "Toy compiler\n");

  // --- Read source file ---
  std::ifstream file(inputFilename);
  if (!file) {
    llvm::errs() << "Could not open file: " << inputFilename << "\n";
    return 1;
  }
  std::stringstream buffer;
  buffer << file.rdbuf();
  std::string src = buffer.str();

  try {
    Lexer lexer(src);
    auto tokens = lexer.tokenize();
    auto ast = parse(tokens);

    auto typeChecker = TypeChecker();
    typeChecker.check(ast);
    if (!typeChecker.ok()) {
      for (const auto &err : typeChecker.errors()) {
        llvm::errs() << "Type error: " << err << "\n";
      }
      return 1;
    }

    IRGenerator codegen("mainmod");
    codegen.generate(ast);

    if (EmitIR) {
      codegen.printIR(outputFilename);
    } else {
      codegen.outputObjFile(outputFilename);
    }

  } catch (const ParseError &e) {
    llvm::errs() << "Parse error: " << e.what() << "\n";
    return 1;
  } catch (const std::exception &e) {
    llvm::errs() << "Error: " << e.what() << "\n";
    return 1;
  }

  return 0;
}
