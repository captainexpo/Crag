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
    std::string runtime_path;
    std::string stdlib_path;
    std::string backend;
    std::vector<std::string> backend_args;
    OptLevel opt;
    bool emit_ir;
    bool unsafe;
    bool no_runtime;
    bool dump_ast_bsa;
    bool dump_ast_asa;
    OSTarget os;
    ArchTarget arch;
    EnvironmentTarget environment;
    VendorTarget vendor;
};

void printHelp(int argc, char **argv) {
    std::cout << "Usage: " << argv[0] << "[options] <input_file>\n";
    std::cout << "Options:\n";
    std::cout << "  -o, --output <file>          Specify output filename (default: main)\n";
    std::cout << "  --emit-ir                    Emit LLVM IR instead of object code\n";
    std::cout << "  --runtime-path <path>        Path to runtime library (default: relative to executable)\n";
    std::cout << "  --stdlib-path <path>         Manually specify path to standard library\n";
    std::cout << "  --backend <backend>          Specify backend (currently only 'llvm' is supported)\n";
    std::cout << "  -ODebug                      Set optimization level to Debug\n";
    std::cout << "  -ORelease                    Set optimization level to Release\n";
    std::cout << "  --unsafe                     Disable safety checks (bounds checking, null pointer checks)\n";
    std::cout << "  --no-runtime                 Do not link against the runtime library\n";
    std::cout << "  --dump-ast [bsa],[asa]       Dump the AST and exit. Provide 'bsa' to dump before semantic analysis and 'asa' to dump after semantic analysis\n";
    std::cout << "  --target-os <os>             Target operating system (linux, macos, windows)\n";
    std::cout << "  --target-arch <arch>         Target architecture (x86_64, x86, arm64)\n";
    std::cout << "  --target-vendor <vendor>     Target vendor (pc, apple, unkown)\n";
    std::cout << "  --target-environment <env>   Target environment (gnu, msvc, elf, eabi)\n";
    std::cout << "  -l<arg>, -L<arg>, -W<arg>    Pass <arg> to the backend as a linker or compiler flag\n";
    std::cout << "  --help or -h                 Show this help message\n";
    std::cout << "  --nocstdlib                  Do not link against the standard library (implies --no-runtime)\n";
}

Arguments parseArguments(int argc, char **argv) {
    Arguments args;
    args.output = "main";
    args.emit_ir = false;
    args.runtime_path = defaultRuntimePath();
    args.stdlib_path = "";
    args.backend = "llvm";
    args.opt = Debug;
    args.unsafe = false;
    args.no_runtime = false;
    args.dump_ast_asa = false;
    args.dump_ast_bsa = false;
    args.os = defaultOS();
    args.arch = defaultArch();

    for (int i = 1; i < argc; ++i) {
        std::string arg = argv[i];
        if (arg == "--help" || arg == "-h") {
            printHelp(argc, argv);
            exit(0);
        } else if (arg == "-o" || arg == "--output") {
            if (i + 1 < argc) {
                args.output = argv[++i];
            } else {
                std::cerr << "Error: Missing value for " << arg << "\n";
                printHelp(argc, argv);
                exit(1);
            }
        } else if (arg == "--emit-ir") {
            args.emit_ir = true;
        } else if (arg == "--runtime-path") {
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
            if (i + 1 < argc) {
                std::string dump_arg = argv[++i];
                if (dump_arg == "bsa") {
                    args.dump_ast_bsa = true;
                } else if (dump_arg == "asa") {
                    args.dump_ast_asa = true;
                } else {
                    std::cerr << "Error: Invalid value for --dump-ast. Expected 'bsa' or 'asa'.\n";
                    printHelp(argc, argv);
                    exit(1);
                }
            } else {
                std::cerr << "Error: Missing value for " << arg << "\n";
                printHelp(argc, argv);
                exit(1);
            }
        } else if (arg == "--target-os") {
            if (i + 1 < argc) {
                std::string os_arg = argv[++i];
                if (os_arg == "linux") {
                    args.os = Linux;
                } else if (os_arg == "macos") {
                    args.os = MacOS;
                } else if (os_arg == "windows") {
                    args.os = Windows;
                } else {
                    std::cerr << "Error: Invalid value for --target-os. Expected 'linux', 'macos', or 'windows'.\n";
                    printHelp(argc, argv);
                    exit(1);
                }
            } else {
                std::cerr << "Error: Missing value for " << arg << "\n";
                printHelp(argc, argv);
                exit(1);
            }
        } else if (arg == "--target-arch") {
            if (i + 1 < argc) {
                std::string arch_arg = argv[++i];
                if (arch_arg == "x86_64") {
                    args.arch = X86_64;
                } else if (arch_arg == "x86") {
                    args.arch = X86;
                } else if (arch_arg == "arm64") {
                    args.arch = ARM64;
                } else {
                    std::cerr << "Error: Invalid value for --target-arch. Expected 'x86_64', 'x86', or 'arm64'.\n";
                    printHelp(argc, argv);
                    exit(1);
                }
            } else {
                std::cerr << "Error: Missing value for " << arg << "\n";
                printHelp(argc, argv);
                exit(1);
            }
        } else if (arg == "--target-vendor") {
            if (i + 1 < argc) {
                std::string vendor_arg = argv[++i];
                if (vendor_arg == "pc") {
                    args.vendor = PC;
                } else if (vendor_arg == "apple") {
                    args.vendor = Apple;
                } else if (vendor_arg == "unknown") {
                    args.vendor = UnknownVendor;
                } else {
                    std::cerr << "Error: Invalid value for --target-vendor. Expected 'pc', 'apple', or 'unknown'.\n";
                    printHelp(argc, argv);
                    exit(1);
                }
            } else {
                std::cerr << "Error: Missing value for " << arg << "\n";
                printHelp(argc, argv);
                exit(1);
            }
        } else if (arg == "--target-environment") {
            if (i + 1 < argc) {
                std::string env_arg = argv[++i];
                if (env_arg == "gnu") {
                    args.environment = GNU;
                } else if (env_arg == "msvc") {
                    args.environment = MSVC;
                } else if (env_arg == "elf") {
                    args.environment = ELF;
                } else if (env_arg == "eabi") {
                    args.environment = EABI;
                } else if (env_arg == "unknown") {
                    args.environment = UnknownEnvironment;
                } else {
                    std::cerr << "Error: Invalid value for --target-environment. Expected 'gnu', 'msvc', 'elf', 'eabi', or 'unknown'.\n";
                    printHelp(argc, argv);
                    exit(1);
                }
            } else {
                std::cerr << "Error: Missing value for " << arg << "\n";
                printHelp(argc, argv);
                exit(1);
            }
        }  else if (arg == "--nocstdlib") {
            // Pass --nostdlib to the backend to avoid linking against the C standard library (assumes the backend is clang or gcc)
            args.backend_args.push_back("-nostdlib");
        }
        else if (arg[0] == '-') {
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

    options.dump_ast_bsa = args.dump_ast_bsa;
    options.dump_ast_asa = args.dump_ast_asa;
    options.opt_level = args.opt;
    options.do_runtime_safety = !args.unsafe;
    options.target = Target(args.os, args.arch);

    auto inputFilepath = args.input;
    auto outputFilepath = args.output;
    auto EmitIR = args.emit_ir;
    auto pathToRuntime = args.runtime_path;
    auto noRuntime = args.no_runtime;

    std::shared_ptr<Backend> codegen = compileModule(inputFilepath, options);

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
