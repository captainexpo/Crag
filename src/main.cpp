#include "backend.h"
#include "compiler.h"

#include <argparse/argparse.hpp>
#include <filesystem>
#include <iostream>
#include <memory>

#ifdef __APPLE__
    #include <mach-o/dyld.h>
#endif

#define PRGM_NAME "Crag Compiler"

namespace fs = std::filesystem;

std::string defaultRuntimePath() {
#if defined(__APPLE__)
    // find lib/libruntime.a relative to executable
    char execPath[1024];
    uint32_t size = sizeof(execPath);
    if (_NSGetExecutablePath(execPath, &size) != 0) {
        std::cerr << "Error getting executable path\n";
        return "libruntime.a";
    }
    fs::path runtimePath = fs::canonical(fs::path(execPath)).parent_path() / "lib" / "libruntime.a";
    return runtimePath.string();
#elif defined(__linux__)
    // find lib/libruntime.a relative to executable
    fs::path execPath = fs::canonical(fs::path("/proc/self/exe"));
    fs::path runtimePath = execPath.parent_path() / "lib" / "libruntime.a";
    return runtimePath.string();
#else
    return "libruntime.a";
#endif
}

struct Arguments {
    std::string input;
    std::string output;
    bool emit_ir;
    std::string runtime_path;
    std::string stdlib_path;
    std::string backend;
    OptLevel opt;
    bool unsafe;
    bool no_runtime;
    bool dump_ast;
    std::vector<std::string> backend_args;
};

void printHelp(int argc, char** argv) {
    std::cout << "Usage: " << argv[0] << "[options] <input_file>\n";
    std::cout << "Options:\n";
    std::cout << "  -o, --output <file>       Specify output filename (default: main)\n";
    std::cout << "  --emit-ir                 Emit LLVM IR instead of object code\n";
    std::cout << "  --runtime-path <path>     Path to runtime library (default: relative to executable)\n";
    std::cout << "  --stdlib-path <path>      Manually specify path to standard library\n";
    std::cout << "  --backend <backend>       Specify backend (currently only 'llvm' is supported)\n";
    std::cout << "  -ODebug                   Set optimization level to Debug\n";
    std::cout << "  -ORelease                 Set optimization level to Release\n";
    std::cout << "  --unsafe                  Disable safety checks (bounds checking, null pointer checks)\n";
    std::cout << "  --no-runtime              Do not link against the runtime library\n";
    std::cout << "  --dump-ast               Dump the AST and exit\n";
}

Arguments parseArguments(int argc, char** argv) {
    Arguments args;
    args.output = "main";
    args.emit_ir = false;
    args.runtime_path = defaultRuntimePath();
    args.stdlib_path = "";
    args.backend = "llvm";
    args.opt = Debug;
    args.unsafe = false;
    args.no_runtime = false;
    args.dump_ast = false;

    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];

        if (arg == "-o" || arg == "--output") {
            if (i + 1 < argc) {
                args.output = argv[++i];
            } else {
                std::cerr << "Error: Missing value for " << arg << "\n";
                printHelp(argc, argv);
                exit(1);
            }
        } else if (arg == "--emit-ir") {
            args.emit_ir = true;
        }
        else if (arg == "--runtime-path") {
            if (i + 1 < argc) {
                args.runtime_path = argv[++i];
            } else {
                std::cerr << "Error: Missing value for " << arg << "\n";
                printHelp(argc, argv);
                exit(1);
            }
        } else if (arg == "--stdlib-path") {
            if (i + 1 < argc) {
                args.stdlib_path = argv[++i];
            } else {
                std::cerr << "Error: Missing value for " << arg << "\n";
                printHelp(argc, argv);
                exit(1);
            }
        } else if (arg == "--backend") {
            if (i + 1 < argc) {
                args.backend = argv[++i];
            } else {
                std::cerr << "Error: Missing value for " << arg << "\n";
                printHelp(argc, argv);
                exit(1);
            }
        } else if (arg == "-ODebug") {
            args.opt = Debug;
        } else if (arg == "-ORelease") {
            args.opt = Release;
        } else if (arg == "--unsafe") {
            args.unsafe = true;
        } else if (arg == "--no-runtime") {
            args.no_runtime = true;
        } else if (arg == "--dump-ast") {
            args.dump_ast = true;
        } else if (arg[0] == '-') {
            if (arg[1] == 'l' || arg[1] == 'L' || arg[1] == 'W') {
                args.backend_args.push_back(arg);
            } else {
                std::cerr << "Error: Unknown option " << arg << "\n";
                printHelp(argc, argv);
                exit(1);
            }
        } else if (!args.input.empty()) {
            std::cerr << "Error: Multiple input files specified\n";
            printHelp(argc, argv);
            exit(1);
        } else {
            args.input = arg;
        }
    }
    return args;
}

int main(int argc, char **argv) {
    Arguments args = parseArguments(argc, argv);

    CompilerOptions options;

    if (args.backend == "llvm") {
        options.backend = LLVM;
    } else {
        std::cerr << "Unsupported backend: " << args.backend << "\n";
        return 1;
    }

    options.dump_ast = args.dump_ast;
    options.opt_level = args.opt;
    options.do_runtime_safety = !args.unsafe;

    auto inputFilepath = args.input;
    auto outputFilepath = args.output;
    auto EmitIR = args.emit_ir;
    auto pathToRuntime = args.runtime_path;
    auto noRuntime = args.no_runtime;

    auto context = llvm::LLVMContext();
    std::shared_ptr<Backend> codegen = compileModule(inputFilepath, context, options);

    if (codegen == nullptr) {
        std::cerr << "Compilation failed.\n";
        return 1;
    }

    fs::path outputPath(outputFilepath);

    if (EmitIR) {
        fs::path irFilePath = outputPath;
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
        try {
            objectFilePath.replace_extension(".o");
            codegen->emitObjectToFile(objectFilePath.string());
            std::cout << "Emitted object file to " << objectFilePath << "\n";

            codegen->compileObjectFileToExecutable(objectFilePath.string(),
                                                   outputPath,
                                                   fs::path(pathToRuntime),
                                                   noRuntime, args.backend_args);
            std::cout << "Emitted executable to " << outputPath << "\n";
        } catch (const CodeGenError &err) {
            std::cerr << err.what() << "\n";
            return 1;
        }
    }

    return 0;
}
