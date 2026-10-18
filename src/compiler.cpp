#include "compiler.h"
#include "backend.h"

// #define NO_LLVM
#ifndef NO_LLVM
#include "backends/llvmcodegen.h"
#endif

#include "diagnostic.h"
#include "module_resolver.h"

#include "typechecking/typecheck.h"
#include "utils.h"
#include <filesystem>
#include <llvm/Support/TargetSelect.h>
#include <memory>

// #define DBG_PRINT_AST

static DiagnosticSink toDiagnostics(const std::vector<TypeCheckError> &errs) {
    DiagnosticSink sink;
    for (const auto &err : errs) {
        sink.diags.push_back({err.node ? err.node->line : -1,
                              err.node ? err.node->col : -1,
                              err.what(),
                              err.module ? err.module->name : "<unknown>",
                              err.module ? err.module->source_code : ""});
    }
    return sink;
}

#ifndef NO_LLVM
static DiagnosticSink toDiagnostics(const std::vector<AttributedCodeGenError> &errs) {
    DiagnosticSink sink;
    for (const auto &err : errs) {
        sink.diags.push_back({err.error.node() ? err.error.node()->line : -1,
                              err.error.node() ? err.error.node()->col : -1,
                              err.error.what(),
                              err.module ? err.module->name : "<unknown>",
                              err.module ? err.module->source_code : ""});
    }
    return sink;
}
#endif

void initializeLLVM() {
    static bool initialized = false;
    if (!initialized) {
        llvm::InitializeAllTargets();
        llvm::InitializeAllAsmPrinters();
        llvm::InitializeAllAsmParsers();
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

std::shared_ptr<Backend> compileModule(const std::string &raw_filepath, CompilerOptions options) {
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
    auto moduleResolver = ModuleResolver(path.parent_path(), options);
    auto mod = moduleResolver.loadRoot(path.filename().string());

#ifdef DBG_PRINT_AST
    std::cout << mod->ast->toString();
#endif

    auto typeChecker = new TypeChecker(options);
    typeChecker->check(mod);

    if (options.dump_ast_bsa) {
        std::cout << "Dumping AST and exiting as requested by --dump-ast:\n";
        std::cout << mod->ast->toString();
        return nullptr;
    }
    if (!typeChecker->ok()) {
        std::cout << typeChecker->errors().size() << " errors during type checking:\n";
        toDiagnostics(typeChecker->errors()).report();
        return nullptr;
    }

#if defined(DBG_PRINT_AST) || defined(NO_LLVM)
    std::cout << "after type checking:\n";
    std::cout << mod->ast->toString();
#else
    if (options.dump_ast_asa) {
        std::cout << "Dumping AST and exiting as requested by --dump-ast:\n";
        std::cout << mod->ast->toString();
        return nullptr;
    }
#endif

    if (moduleResolver.hasDependencyCycle()) {
        std::cerr << "Error: Cyclic dependencies detected among modules.\n";
        return nullptr;
    }
#ifndef NO_LLVM
    // Call llvm codegen directly here, should probably make it more modular later
    auto codegen = std::make_shared<LLVMCodegen>("mainmod", moduleResolver, options);
    codegen->setGlobalSymbols(&typeChecker->globalSymbols());


    std::vector<std::shared_ptr<Module>> allModules;
    for (const auto &[path, mod] : moduleResolver.moduleCache()) {
        allModules.push_back(mod);
    }
    for (const auto &[origin, synth] : typeChecker->instantiationModules()) {
        allModules.push_back(synth);
    }

    for (const auto &mod : allModules) codegen->generateTypeSkeletons(mod);
    for (const auto &mod : allModules) codegen->generateStructBodies(mod);
    for (const auto &mod : allModules) codegen->generateUnionAndEnumBodies(mod);

    if (!codegen->ok()) {
        std::cerr << codegen->errors().size() << " errors during code generation:\n";
        toDiagnostics(codegen->errors()).report();
        std::cerr << "Code generation failed due to previous errors.\n";
        return nullptr;
    }

    std::vector<std::string> order = moduleResolver.getBestOrder();
    std::vector<std::shared_ptr<Module>> functionPhaseOrder;
    for (const std::string &path : order) {
        functionPhaseOrder.push_back(moduleResolver.getModule(path));
    }
    for (const auto &[origin, synth] : typeChecker->instantiationModules()) {
        functionPhaseOrder.push_back(synth);
    }

    for (const auto &mod : functionPhaseOrder) codegen->generateSignatures(mod);
    for (const auto &mod : functionPhaseOrder) codegen->generateBodies(mod);
    codegen->finished();

    if (!codegen->ok()) {
        std::cerr << codegen->errors().size() << " errors during code generation:\n";
        toDiagnostics(codegen->errors()).report();
        std::cerr << "Code generation failed due to previous errors.\n";
        return nullptr;
    }

    codegen->verifyModule();

    std::cout << "Compilation succeeded.\n";
    return codegen;
#else
    std::cout << "Compilation succeeded (no code generated due to NO_LLVM flag).\n";
    return nullptr;
#endif
}
