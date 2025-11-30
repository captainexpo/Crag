#include "backend.h"
#include "compiler.h"

#include <argparse/argparse.hpp>
#include <filesystem>
#include <iostream>
#include <memory>

#define PRGM_NAME "Crag Compiler"

namespace fs = std::filesystem;

int main(int argc, char **argv) {
  argparse::ArgumentParser program(PRGM_NAME);

  program.add_argument("input")
      .help("Source file to compile");

  program.add_argument("-o", "--output")
      .default_value(std::string("a.out"))
      .help("Specify output filename");

  program.add_argument("--emit-ir")
      .default_value(false)
      .implicit_value(true)
      .help("Emit LLVM IR instead of object code");

  program.add_argument("--runtime-path")
      .default_value(std::string("lib/libruntime.a"))
      .help("Path to runtime library");

  program.add_argument("--backend")
      .default_value(std::string("llvm"))
      .help("Specify backend (currently only 'llvm' is supported)");

  program.add_argument("--opt-level")
      .default_value(std::string("debug"))
      .help("Optimization level (debug or release)");

  program.add_argument("--unsafe")
      .default_value(false)
      .implicit_value(true)
      .help("Disable safety checks (bounds checking, null pointer checks)");

  program.add_argument("--no-runtime")
      .default_value(false)
      .implicit_value(true)
      .help("Do not link against the runtime library");

  try {
    program.parse_args(argc, argv);
  } catch (const std::runtime_error &err) {
    std::cerr << err.what() << "\n";
    std::cerr << program;
    return 1;
  }

  CompilerOptions options;

  std::string backend = program.get<std::string>("--backend");
  if (backend == "llvm") {
    options.backend = LLVM;
  } else {
    std::cerr << "Unsupported backend: " << backend << "\n";
    return 1;
  }

  std::string optLevel = program.get<std::string>("--opt-level");
  if (optLevel == "debug") {
    options.opt_level = Debug;
  } else if (optLevel == "release") {
    options.opt_level = Release;
  } else {
    std::cerr << "Unsupported optimization level: " << optLevel << "\n";
    return 1;
  }

  options.do_runtime_safety = !program.get<bool>("--unsafe");

  auto inputFilepath = program.get<std::string>("input");
  auto outputFilepath = program.get<std::string>("--output");
  auto EmitIR = program.get<bool>("--emit-ir");
  auto pathToRuntime = program.get<std::string>("--runtime-path");
  auto noRuntime = program.get<bool>("--no-runtime");

  auto context = llvm::LLVMContext();
  std::shared_ptr<Backend> codegen = compileModule(inputFilepath, context, options);

  if (codegen == nullptr) {
    std::cerr << "Compilation failed.\n";
    return 1;
  }

  fs::path outputPath(outputFilepath);

  if (EmitIR) {
    fs::path irFilePath = outputPath;
    irFilePath.replace_extension(".ll");

    try {
      codegen->emitIrToFile(irFilePath.string());
    } catch (const CodeGenError &err) {
      std::cerr << "Error emitting ir: " << err.what() << "\n";
      return 1;
    }
    std::cout << "Emitted IR to " << irFilePath << "\n";
    return 0;
  }

  std::string outputExt = outputPath.extension().string();
  if (outputExt == ".o") {
    try {
      codegen->emitObjectToFile(outputPath.string());
      std::cout << "Emitted object file to " << outputPath << "\n";
    } catch (const CodeGenError &err) {
      std::cerr << err.what() << "\n";
      return 1;
    }
  } else {
    fs::path objectFilePath = outputPath;
    objectFilePath.replace_extension(".o");

    try {

      codegen->emitObjectToFile(objectFilePath.string());
      std::cout << "Emitted object file to " << objectFilePath << "\n";

      codegen->compileObjectFileToExecutable(objectFilePath.string(),
                                             outputPath,
                                             fs::path(pathToRuntime),
                                             noRuntime);
      std::cout << "Emitted executable to " << outputPath << "\n";
    } catch (const CodeGenError &err) {
      std::cerr << err.what() << "\n";
      return 1;
    }
  }

  return 0;
}
