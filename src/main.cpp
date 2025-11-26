#include "compiler.h"

#include <iostream>
#include <filesystem>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/InitLLVM.h>
#include <llvm/Support/raw_ostream.h>
#include <ratio>
#include <system_error>

#define PRGM_NAME "Toy Compiler"

namespace fs = std::filesystem;

int main(int argc, char **argv) {
    llvm::InitLLVM X(argc, argv);
    llvm::LLVMContext context;

    // --- Define CLI options ---
    llvm::cl::opt<std::string> inputFilepath(llvm::cl::Positional,
                                             llvm::cl::desc("<source file>"),
                                             llvm::cl::Required);

    llvm::cl::opt<std::string> outputFilepath(
        "o", llvm::cl::desc("Specify output filename"),
        llvm::cl::value_desc("filename"), llvm::cl::init("a.out"));

    llvm::cl::opt<bool> EmitIR(
        "emit-ir", llvm::cl::desc("Emit LLVM IR instead of object code"),
        llvm::cl::init(false));

    llvm::cl::opt<std::string> pathToRuntime(
        "runtime-path", llvm::cl::desc("Path to runtime library"),
        llvm::cl::value_desc("path"), llvm::cl::init("lib/libruntime.a"));

    // For future use, hopefully with the QBE backend
    llvm::cl::opt<std::string> backend(
        "backend", llvm::cl::desc("Specify backend (currently only 'llvm' is supported)"),
        llvm::cl::value_desc("backend"), llvm::cl::init("llvm"));

    llvm::cl::opt<std::string> optLevel(
        "opt-level", llvm::cl::desc("Optimization level (debug or release)"),
        llvm::cl::value_desc("level"), llvm::cl::init("debug"));

    llvm::cl::opt<bool> unsafe(
        "unsafe", llvm::cl::desc("Disable safety checks (bounds checking, null pointer checks)"),
        llvm::cl::init(false));


    llvm::cl::opt<bool> noRuntime(
        "no-runtime", llvm::cl::desc("Do not link against the runtime library"),
        llvm::cl::init(false));

    llvm::cl::ParseCommandLineOptions(argc, argv, std::string(PRGM_NAME) + "\n");

    CompilerOptions options;
    if (backend == "llvm") {
        options.backend = LLVM;
    } else {
        std::cerr << "Unsupported backend: " << backend << "\n";
        return 1;
    }

    if (optLevel == "debug") {
        options.opt_level = Debug;
    } else if (optLevel == "release") {
        options.opt_level = Release;
    } else {
        std::cerr << "Unsupported optimization level: " << optLevel << "\n";
        return 1;
    }

    options.do_runtime_safety = !unsafe;


    std::chrono::steady_clock::time_point start = std::chrono::steady_clock::now();

    auto mod = compileModule(inputFilepath, context, options);

    std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
    std::cout << "Compilation finished in " << duration << " micros.\n";
    if (!mod) {
        std::cerr << "Compilation failed.\n";
        return 1;
    }

    fs::path outputPath(outputFilepath.c_str());
    std::string outputExt = outputPath.extension().string();
    fs::path irFilePath = outputPath;
    irFilePath.replace_extension(".ll");

    std::error_code ec;
    llvm::raw_fd_ostream os(irFilePath.string(), ec);
    if (ec) {
        std::cerr << "Could not open output file: " << ec.message() << "\n";
        return 1;
    }
    mod->print(os, nullptr);

    if (EmitIR) {
        std::cout << "Emitted LLVM IR to " << irFilePath << "\n";
        return 0;
    }

    std::string opt;
    if (options.opt_level == Release) {
        opt = " -O3 ";
    } else {
        opt = " -O0 ";
    }

    // Run clang to compile IR to executable
    fs::path finalOutputFilePath = outputPath.parent_path() / outputPath.stem();
    std::string clangCmd = "clang " + irFilePath.string()  + opt;
    if (!noRuntime) {
        clangCmd += " " + pathToRuntime;
    }

    if (outputExt == ".o") {
        finalOutputFilePath += ".o";
        clangCmd += " -c -o " + finalOutputFilePath.string();
    } else {
        clangCmd += " -o " + finalOutputFilePath.string();
    }

    start = std::chrono::steady_clock::now();
    int ret = system(clangCmd.c_str());
    end = std::chrono::steady_clock::now();
    duration = std::chrono::duration_cast<std::chrono::microseconds>(end - start).
count();
    std::cout << "Linking finished in " << duration << " micros.\n";
    if (ret != 0) {
        std::cerr << "Failed to invoke clang to create executable.\n";
        return 1;
    }

    std::cout << "Emitted executable to " << finalOutputFilePath << "\n";


    // Remove the intermediate IR file
  if (!EmitIR && fs::exists(irFilePath))
    fs::remove(irFilePath);
  return 0;
}
