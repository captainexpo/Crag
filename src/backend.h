#pragma once
#ifndef BACKEND_H
#define BACKEND_H

#include "ast/ast.h"
#include "module_resolver.h"
#include <memory>

typedef enum {
    LLVM,
    // TODO: QBE,
} BackendType;

enum OptLevel {
    Debug,
    Release,
};

enum OSTarget {
    Linux,
    MacOS,
    Windows,
    UnknownOS
};

enum ArchTarget {
    X86_64,
    X86,
    ARM64,
    UnknownArch
};

inline ArchTarget defaultArch() {
    // Current arch detection
#if defined(__x86_64__) || defined(_M_X64)
    return X86_64;
#elif defined(__i386__) || defined(_M_IX86)
    return X86;
#elif defined(__aarch64__)
    return ARM64;
#else
    return UnknownArch;
#endif
}

inline OSTarget defaultOS() {
    // Current OS detection
#if defined(_WIN32) || defined(_WIN64)
    return Windows;
#elif defined(__APPLE__) || defined(__MACH__)
    return MacOS;
#elif defined(__linux__)
    return Linux;
#else
    return UnknownOS;
#endif
}

struct Target {
    OSTarget os;
    ArchTarget arch;

    Target() : os(UnknownOS), arch(UnknownArch) {}
    Target(OSTarget os, ArchTarget arch) : os(os), arch(arch) {}

    static Target defaultTarget() {
        return Target(defaultOS(), defaultArch());
    }

    std::string osToString() {
        switch (os) {
            case Linux: return "linux";
            case MacOS: return "macos";
            case Windows: return "windows";
            default: return "unknown";
        }
    }

    std::string archToString() {
        switch (arch) {
            case X86_64: return "x86_64";
            case X86: return "x86";
            case ARM64: return "arm64";
            default: return "unknown";
        }
    }
};

typedef struct {
    BackendType backend;
    OptLevel opt_level;
    bool do_runtime_safety;
    bool dump_ast_bsa;
    bool dump_ast_asa;
    Target target;
} CompilerOptions;



class Backend {
  public:
    virtual ~Backend() = default;
    virtual void generate(std::shared_ptr<Module> module) = 0;
    virtual void emitIrToFile(const std::string &filepath) = 0;
    virtual void emitObjectToFile(const std::string &filepath) = 0;
    virtual void compileObjectFileToExecutable(const std::string &object_filepath,
                                               const std::filesystem::path &executable_filepath,
                                               const std::filesystem::path &runtime_path,
                                               bool no_runtime, std::optional<std::vector<std::string>> backend_args = std::nullopt) = 0;
};

class CodeGenError : public std::exception {
  public:
    CodeGenError(ASTNodePtr node, const std::string &msg)
        : m_node(node), m_msg(msg) {}
    const char *what() const noexcept override { return m_msg.c_str(); }
    ASTNodePtr node() const { return m_node; }

  private:
    ASTNodePtr m_node;
    std::string m_msg;
};

#endif
