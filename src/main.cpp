#include "compiler.h"

#include <iostream>
#include <filesystem>
#include <system_error>
#include <argparse/argparse.hpp>

#define PRGM_NAME "Toy Compiler"

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

    std::chrono::steady_clock::time_point start = std::chrono::steady_clock::now();

    auto context = llvm::LLVMContext();
    auto mod = compileModule(inputFilepath, context, options);

    std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
    std::cout << "Compilation finished in " << duration << " micros.\n";

    if (!mod) {
        std::cerr << "Compilation failed.\n";
        return 1;
    }

    fs::path outputPath(outputFilepath);
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

    std::string opt = (options.opt_level == Release) ? " -O3 " : " -O0 ";

    // Run clang to compile IR to executable
    fs::path finalOutputFilePath = outputPath.parent_path() / outputPath.stem();
    std::string clangCmd = "clang " + irFilePath.string() + opt;

    if (!noRuntime) {
        clangCmd += " " + pathToRuntime;
    }

    std::string outputExt = outputPath.extension().string();
    if (outputExt == ".o") {
        finalOutputFilePath += ".o";
        clangCmd += " -c -o " + finalOutputFilePath.string();
    } else {
        clangCmd += " -o " + finalOutputFilePath.string();
    }

    start = std::chrono::steady_clock::now();
    int ret = system(clangCmd.c_str());
    end = std::chrono::steady_clock::now();
    duration = std::chrono::duration_cast<std::chrono::microseconds>(end - start).count();
    std::cout << "Linking finished in " << duration << " micros.\n";

    if (ret != 0) {
        std::cerr << "Failed to invoke clang to create executable.\n";
        return 1;
    }

    std::cout << "Emitted executable to " << finalOutputFilePath << "\n";

    if (!EmitIR && fs::exists(irFilePath)) {
        fs::remove(irFilePath);
    }

    return 0;
}
