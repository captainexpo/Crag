
#include "codegen.h"
#include "const_eval.h"
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

void prettyError(int line, int col, const std::string &msg,
                 const std::string &source) {
  std::istringstream srcStream(source);
  std::string srcLine;
  int currentLine = 1;

  while (std::getline(srcStream, srcLine)) {
    if (currentLine == line) {
      std::cout << srcLine << "\n";
      std::cout << std::string(col - 1, ' ') << "^\n";
      std::cout << "Error at " << line << ":" << col << " - " << msg << "\n";
      return;
    }
    currentLine++;
  }
}

#define PRGM_NAME "Toy Compiler"

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

  llvm::cl::ParseCommandLineOptions(argc, argv, std::string(PRGM_NAME) + "\n");

  // --- Read source file ---
  std::ifstream file(inputFilename);
  if (!file) {
    std::cerr << "Could not open file: " << inputFilename << "\n";
    return 1;
  }
  std::stringstream buffer;
  buffer << file.rdbuf();
  std::string src = buffer.str();

  Lexer lexer(src);
  auto tokens = lexer.tokenize();
  auto parser = Parser(tokens);
  std::shared_ptr<Program> ast;
  try {
    ast = parser.parse();
  } catch (const ParseError &e) {
    prettyError(e.line, e.col, e.message, src);
    std::cout << "Parsing failed due to previous errors.\n";
    return 1;
  }
  if (!parser.ok()) {
    for (const auto &err : parser.errors()) {
      prettyError(err.line, err.col, err.message, src);
    }
    std::cout << "Parsing failed due to previous errors.\n";
    return 1;
  }

  auto typeChecker = TypeChecker();
  typeChecker.check(ast);
  if (!typeChecker.ok()) {
    for (const auto &err : typeChecker.errors()) {
      prettyError(err.first->line, err.first->col, err.second, src);
    }
    std::cout << "Type checking failed due to previous errors.\n";
    return 1;
  }

  IRGenerator codegen("mainmod");
  codegen.generate(ast);
  if (!codegen.ok()) {
    std::cout << codegen.errors().size() << " errors during code generation:\n";
    for (const auto &err : codegen.errors()) {
      prettyError(err.first ? err.first->line : -1,
                  err.first ? err.first->col : -1, err.second, src);
    }
    std::cout << "Code generation failed due to previous errors.\n";
    return 1;
  }

  if (codegen.ok()) {
    if (EmitIR) {
      codegen.printIR(outputFilename);
    } else {
      codegen.outputObjFile(outputFilename);
    }
  } else {
    std::cerr << "Code generation failed due to previous errors.\n";
    return 1;
  }

  return 0;
}
