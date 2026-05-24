#include "llvmcodegen.h"
#include "../module_resolver.h"
#include "src/ast/ast.h"
#include <filesystem>
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/BinaryFormat/Dwarf.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/DebugInfoMetadata.h>
#include <llvm/IR/DebugLoc.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/InlineAsm.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/CommandLine.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/MathExtras.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>
#include <llvm/TargetParser/Host.h>
#include <memory>
#include <unordered_map>

int globalVals = 0;

#define IS_INSTANCE(obj, type) (std::dynamic_pointer_cast<type>(obj) != nullptr)
#define CUR_SCOPE m_scopeStack.back()

static Scope::Symbol *lookupSymbolInScopes(std::vector<Scope> &scopes, const std::string &name) {
    for (auto scopeIt = scopes.rbegin(); scopeIt != scopes.rend(); ++scopeIt) {
        auto entryIt = scopeIt->table.find(name);
        if (entryIt != scopeIt->table.end()) {
            return &entryIt->second;
        }
    }
    return nullptr;
}

static Scope::Symbol *lookupSymbolInScopes(std::vector<Scope> &scopes,
                                           const std::string &canonicalName,
                                           const std::string &rawName) {
    if (auto *sym = lookupSymbolInScopes(scopes, canonicalName)) {
        return sym;
    }
    if (rawName != canonicalName) {
        return lookupSymbolInScopes(scopes, rawName);
    }
    return nullptr;
}

std::string runtimePanicTypeToString(RuntimePanicType type) {
    switch (type) {
        case OutOfBounds:
            return "Out of Bounds";
        case DivisionByZero:
            return "Division by Zero";
        case NullPointerDereference:
            return "Null Pointer Dereference";
        default:
            return "Unknown";
    }
}

void registerGlobalCtor(llvm::Module &mod, llvm::Function *initFn, int priority = 65535) {
    llvm::LLVMContext &ctx = mod.getContext();

    llvm::Type *i32Ty = llvm::Type::getInt32Ty(ctx);
    llvm::Type *voidPtrTy = llvm::PointerType::getUnqual(ctx);

    llvm::StructType *ctorStructTy = llvm::StructType::get(i32Ty, initFn->getType(), voidPtrTy);

    llvm::Constant *ctorStruct = llvm::ConstantStruct::get(
        ctorStructTy,
        llvm::ConstantInt::get(i32Ty, priority),
        initFn,
        llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(voidPtrTy)));

    llvm::ArrayType *arrTy = llvm::ArrayType::get(ctorStructTy, 1);

    new llvm::GlobalVariable(
        mod,
        arrTy,
        false,
        llvm::GlobalValue::AppendingLinkage,
        llvm::ConstantArray::get(arrTy, ctorStruct),
        "llvm.global_ctors");
}

void LLVMCodegen::initDebugInfo() {
    if (!m_emit_debug || m_di_builder) {
        return;
    }

    m_llvm_module->addModuleFlag(llvm::Module::Warning, "Debug Info Version",
                                 llvm::DEBUG_METADATA_VERSION);
    m_llvm_module->addModuleFlag(llvm::Module::Warning, "Dwarf Version", 5);

    m_di_builder = std::make_unique<llvm::DIBuilder>(*m_llvm_module);
    m_di_file = getDIFileForPath(m_current_module ? m_current_module->path : "");
    m_llvm_module->setSourceFileName(m_current_module ? m_current_module->path : "");

    m_di_compile_unit = m_di_builder->createCompileUnit(
        llvm::dwarf::DW_LANG_C,
        m_di_file,
        "Crag",
        false,
        "",
        0);
    m_di_scope = m_di_compile_unit;
}

void LLVMCodegen::finalizeDebugInfo() {
    if (!m_emit_debug || m_debug_finalized || !m_di_builder) {
        return;
    }
    m_di_builder->finalize();
    m_di_builder.reset();
    m_debug_finalized = true;
}

llvm::DIFile *LLVMCodegen::getDIFileForPath(const std::string &path) {
    if (!m_di_builder) {
        return nullptr;
    }

    std::filesystem::path filePath = path.empty() ? std::filesystem::path("<unknown>")
                                                  : std::filesystem::path(path);
    std::string filename = filePath.filename().string();
    std::string directory = filePath.parent_path().string();
    std::string key = directory + "/" + filename;

    auto it = m_di_file_cache.find(key);
    if (it != m_di_file_cache.end()) {
        return it->second;
    }

    llvm::DIFile *file = m_di_builder->createFile(filename, directory);
    m_di_file_cache[key] = file;
    return file;
}

void LLVMCodegen::setDebugLocation(const ASTNodePtr &node) {
    if (!m_emit_debug || !m_di_builder) {
        return;
    }

    if (!node) {
        m_builder.SetCurrentDebugLocation(llvm::DebugLoc());
        return;
    }

    llvm::DIScope *scope = m_di_scope;
    if (scope && llvm::isa<llvm::DICompileUnit>(scope)) {
        scope = nullptr;
    }
    if (!scope) {
        auto *bb = m_builder.GetInsertBlock();
        auto *func = bb ? bb->getParent() : nullptr;
        if (func) {
            scope = func->getSubprogram();
        }
    }

    if (!scope) {
        m_builder.SetCurrentDebugLocation(llvm::DebugLoc());
        return;
    }

    unsigned line = node->line > 0 ? static_cast<unsigned>(node->line) : 1;
    unsigned col = node->col > 0 ? static_cast<unsigned>(node->col) : 1;
    auto *loc = llvm::DILocation::get(context, line, col, scope);
    m_builder.SetCurrentDebugLocation(loc);
}

llvm::DIType *LLVMCodegen::getDIType(const std::shared_ptr<Type> &type, const ASTNodePtr &node) {
    if (!m_di_builder || !type) {
        return nullptr;
    }

    std::string key = type->str();
    auto it = m_di_type_cache.find(key);
    if (it != m_di_type_cache.end()) {
        return it->second;
    }

    llvm::DIType *diType = nullptr;
    if (IS_INSTANCE(type, Void)) {
        diType = nullptr;
    } else if (IS_INSTANCE(type, I32)) {
        diType = m_di_builder->createBasicType("i32", 32, llvm::dwarf::DW_ATE_signed);
    } else if (IS_INSTANCE(type, I16)) {
        diType = m_di_builder->createBasicType("i16", 16, llvm::dwarf::DW_ATE_signed);
    } else if (IS_INSTANCE(type, I8)) {
        diType = m_di_builder->createBasicType("i8", 8, llvm::dwarf::DW_ATE_signed);
    } else if (IS_INSTANCE(type, I64)) {
        diType = m_di_builder->createBasicType("i64", 64, llvm::dwarf::DW_ATE_signed);
    } else if (IS_INSTANCE(type, U8)) {
        diType = m_di_builder->createBasicType("u8", 8, llvm::dwarf::DW_ATE_unsigned);
    } else if (IS_INSTANCE(type, U32)) {
        diType = m_di_builder->createBasicType("u32", 32, llvm::dwarf::DW_ATE_unsigned);
    } else if (IS_INSTANCE(type, U16)) {
        diType = m_di_builder->createBasicType("u16", 16, llvm::dwarf::DW_ATE_unsigned);
    } else if (IS_INSTANCE(type, U64)) {
        diType = m_di_builder->createBasicType("u64", 64, llvm::dwarf::DW_ATE_unsigned);
    } else if (IS_INSTANCE(type, F32)) {
        diType = m_di_builder->createBasicType("f32", 32, llvm::dwarf::DW_ATE_float);
    } else if (IS_INSTANCE(type, F64)) {
        diType = m_di_builder->createBasicType("f64", 64, llvm::dwarf::DW_ATE_float);
    } else if (IS_INSTANCE(type, Boolean)) {
        diType = m_di_builder->createBasicType("bool", 1, llvm::dwarf::DW_ATE_boolean);
    } else if (IS_INSTANCE(type, USize)) {
        unsigned bits = m_llvm_module->getDataLayout().getPointerSizeInBits();
        diType = m_di_builder->createBasicType("usize", bits, llvm::dwarf::DW_ATE_unsigned);
    } else if (IS_INSTANCE(type, ISize)) {
        unsigned bits = m_llvm_module->getDataLayout().getPointerSizeInBits();
        diType = m_di_builder->createBasicType("isize", bits, llvm::dwarf::DW_ATE_signed);
    } else if (IS_INSTANCE(type, PointerType)) {
        auto ptrType = std::dynamic_pointer_cast<PointerType>(type);
        llvm::DIType *pointee = getDIType(ptrType->base, node);
        unsigned bits = m_llvm_module->getDataLayout().getPointerSizeInBits();
        diType = m_di_builder->createPointerType(pointee, bits);
    } else if (IS_INSTANCE(type, StructType)) {
        auto structType = std::dynamic_pointer_cast<StructType>(type);
        llvm::DIFile *file = m_di_file ? m_di_file : getDIFileForPath(m_current_module ? m_current_module->path : "");
        llvm::Type *llvmTy = getLLVMType(type, node);
        uint64_t size = m_llvm_module->getDataLayout().getTypeAllocSize(llvmTy);
        uint32_t align = m_llvm_module->getDataLayout().getABITypeAlign(llvmTy).value();
        diType = m_di_builder->createStructType(
            m_di_compile_unit,
            structType->name,
            file,
            node ? node->line : 1,
            size,
            align,
            llvm::DINode::FlagZero,
            nullptr,
            llvm::DINodeArray());
    } else if (IS_INSTANCE(type, UnionType)) {
        auto unionType = std::dynamic_pointer_cast<UnionType>(type);
        llvm::DIFile *file = m_di_file ? m_di_file : getDIFileForPath(m_current_module ? m_current_module->path : "");
        llvm::Type *llvmTy = getLLVMType(type, node);
        uint64_t size = m_llvm_module->getDataLayout().getTypeAllocSize(llvmTy);
        uint32_t align = m_llvm_module->getDataLayout().getABITypeAlign(llvmTy).value();
        diType = m_di_builder->createUnionType(
            m_di_compile_unit,
            unionType->name,
            file,
            node ? node->line : 1,
            size,
            align,
            llvm::DINode::FlagZero,
            llvm::DINodeArray());
    } else if (IS_INSTANCE(type, EnumType)) {
        auto enumType = std::dynamic_pointer_cast<EnumType>(type);
        diType = m_di_builder->createUnspecifiedType("enum " + enumType->name);
    } else {
        diType = m_di_builder->createUnspecifiedType(type->str());
    }

    if (diType) {
        m_di_type_cache[key] = diType;
    }
    return diType;
}

llvm::AllocaInst *LLVMCodegen::createEntryBlockAlloca(llvm::Function *function, llvm::Type *type, const llvm::Twine &name) {
    if (!function) {
        return nullptr;
    }
    llvm::IRBuilder<> entryBuilder(&function->getEntryBlock(), function->getEntryBlock().begin());
    entryBuilder.SetCurrentDebugLocation(llvm::DebugLoc());
    return entryBuilder.CreateAlloca(type, nullptr, name);
}

void LLVMCodegen::emitBuiltinDeclarations() {
    llvm::FunctionType *panicType = llvm::FunctionType::get(
        llvm::Type::getVoidTy(context),
        {
            llvm::PointerType::getUnqual(context), // Error message
            llvm::Type::getInt32Ty(context),       // Line number
            llvm::Type::getInt32Ty(context)        // Column number
        },
        false);
    llvm::Function::Create(panicType, llvm::Function::ExternalLinkage, RUNTIME_PANIC_FUNC_NAME, m_llvm_module.get());

    // Prepare runtime panic strings
    m_runtime_panic_strings.clear();
    m_runtime_panic_strings.resize(3, nullptr);
    for (const auto &type : {OutOfBounds, DivisionByZero, NullPointerDereference}) {
        // String literal → create global string
        auto strVal = "Runtime Panic: " + runtimePanicTypeToString(type) + "\n";
        auto strName = "g" + std::to_string(globalVals++);
        auto strType = llvm::ArrayType::get(llvm::Type::getInt8Ty(context),
                                            strVal.size() + 1);
        auto strConstant =
            llvm::ConstantDataArray::getString(context, strVal, true);
        auto globalStr = new llvm::GlobalVariable(
            *m_llvm_module, strType, true, llvm::GlobalValue::PrivateLinkage,
            strConstant, strName);
        m_runtime_panic_strings[type] = globalStr;
    }
}

// Public API stubs
void LLVMCodegen::generate(std::shared_ptr<Module> module) {

    std::vector<std::pair<llvm::Function *, std::shared_ptr<FunctionDeclaration>>> funcDecls;

    m_current_module = module;
    if (m_emit_debug) {
        if (!m_di_builder) {
            initDebugInfo();
        }
        if (m_di_builder) {
            m_di_file = getDIFileForPath(m_current_module ? m_current_module->path : "");
        }
    }

    try {
        for (const auto &decl : module->ast->declarations) {
            if (IS_INSTANCE(decl, StructDeclaration)) {
                auto sd = std::dynamic_pointer_cast<StructDeclaration>(decl);
                // Skip template declarations - only generate instantiations
                if (!sd->generic_params.empty()) {
                    continue;
                }
                generateStructDeclaration(sd);
                auto sms = generateStructMethods(sd);
                for (const auto &pair : sms) {
                    funcDecls.push_back(pair);
                }
                continue;
            }
            if (IS_INSTANCE(decl, UnionDeclaration)) {
                auto ud = std::dynamic_pointer_cast<UnionDeclaration>(decl);
                generateUnionDeclaration(ud);
                continue;
            }
            if (IS_INSTANCE(decl, EnumDeclaration)) {
                generateEnumDeclaration(
                    std::dynamic_pointer_cast<EnumDeclaration>(decl));
                continue;
            }
        }

        for (const auto &decl : module->ast->declarations) {
            if (IS_INSTANCE(decl, FunctionDeclaration)) {
                auto fd = std::dynamic_pointer_cast<FunctionDeclaration>(decl);
                // Skip template declarations - only generate instantiations
                if (!fd->generic_params.empty()) {
                    continue;
                }
                llvm::Function *fn = generateFunctionDefinition(fd);
                funcDecls.push_back({fn, fd});
                continue;
            }
            if (IS_INSTANCE(decl, VariableDeclaration)) {
                generateVariableDeclaration(
                    std::dynamic_pointer_cast<VariableDeclaration>(decl));
                continue;
            }
            if (IS_INSTANCE(decl, StructDeclaration) || IS_INSTANCE(decl, UnionDeclaration) || IS_INSTANCE(decl, EnumDeclaration)) {
                // Already handled in first pass
                continue;
            }
            if (IS_INSTANCE(decl, ImportDeclaration)) {
                continue; // Handled elsewhere
            }
            if (IS_INSTANCE(decl, TypeAliasDeclaration)) {
                continue; // No codegen needed
            }
            throw CodeGenError(decl, "Unknown top-level declaration: " + decl->str());
        }
        for (const auto &fnPair : funcDecls) {
            generateFunctionBody(fnPair.second, fnPair.first);
        }
    } catch (const CodeGenError &e) {
        m_errors.push_back(e);
    }

    addGlobalVarInitializer(nullptr, nullptr);
}

void LLVMCodegen::emitObjectToFile(const std::string &filename) {
    // REF:
    // https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl08.html

    auto targetTriple = llvm::sys::getDefaultTargetTriple();
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    std::string errorStr;
    auto Target = llvm::TargetRegistry::lookupTarget(targetTriple, errorStr);

    // Print an error and exit if we couldn't find the requested target.
    // This generally occurs if we've forgotten to initialise the
    // TargetRegistry or we have a bogus target triple.
    if (!Target) {
        throw CodeGenError(nullptr, "Failed to lookup target: " + errorStr);
    }
    auto CPU = "generic";
    auto Features = "";

    // Verify the module before emitting object code
    // if (llvm::verifyModule(*m_llvm_module, &llvm::errs())) {
    //     throw CodeGenError(nullptr, "Module verification failed - invalid IR");
    // }

    const llvm::TargetOptions opt{};

    llvm::CodeGenOptLevel opt_level = llvm::CodeGenOptLevel::Default;

    m_options.opt_level == Release
        ? opt_level = llvm::CodeGenOptLevel::Aggressive
        : opt_level = llvm::CodeGenOptLevel::None;

    const auto Triple = llvm::Triple(targetTriple);

    auto targetMachine = Target->createTargetMachine(Triple, CPU, Features, opt, llvm::Reloc::Model::PIC_, std::nullopt, opt_level);

    m_llvm_module->setDataLayout(targetMachine->createDataLayout());
    m_llvm_module->setTargetTriple(Triple);

    std::error_code ec;
    llvm::raw_fd_ostream dest(filename, ec, llvm::sys::fs::OF_None);

    if (ec) {
        throw CodeGenError(nullptr, "Could not open file: " + ec.message());
    }
    llvm::legacy::PassManager pass;
    auto FileType = llvm::CodeGenFileType::ObjectFile;

    if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
        throw CodeGenError(nullptr, "TargetMachine can't emit a file of this type");
    }

    finalizeDebugInfo();

    pass.run(*m_llvm_module);
    dest.flush();
}

void LLVMCodegen::emitIrToFile(const std::string &filepath) {
    if (m_llvm_module) {
        finalizeDebugInfo();
        std::error_code ec;
        llvm::raw_fd_ostream out(filepath, ec, llvm::sys::fs::OF_None);
        if (ec) {
            throw CodeGenError(nullptr, "Could not open file: " + ec.message());
        }
        m_llvm_module->print(out, nullptr);
    }
}

void LLVMCodegen::compileObjectFileToExecutable(
    const std::string &object_filepath,
    const std::filesystem::path &executable_filepath,
    const std::filesystem::path &runtime_path,
    bool no_runtime,
    std::optional<std::vector<std::string>> backend_args) {
    std::string linker = "cc";
    std::string cmd = linker + " " + object_filepath + " -o " + executable_filepath.string() + " -lm ";
    if (backend_args) {
        for (const auto &arg : *backend_args) {
            cmd += arg + " ";
        }
    }

    if (!no_runtime) {
        std::string runtimeLib = runtime_path.string();
        cmd += " " + runtimeLib;
    }
    int result = std::system(cmd.c_str());
    if (result != 0) {
        throw CodeGenError(nullptr, "Linking failed with command: " + cmd);
    }
}

llvm::Type *LLVMCodegen::getLLVMType(const std::shared_ptr<Type> &type, const ASTNodePtr &node) {
    if (IS_INSTANCE(type, I32))
        return llvm::Type::getInt32Ty(context);
    if (IS_INSTANCE(type, I16))
        return llvm::Type::getInt16Ty(context);
    if (IS_INSTANCE(type, I8))
        return llvm::Type::getInt8Ty(context);
    if (IS_INSTANCE(type, I64))
        return llvm::Type::getInt64Ty(context);
    if (IS_INSTANCE(type, U8))
        return llvm::Type::getInt8Ty(context);
    if (IS_INSTANCE(type, U16))
        return llvm::Type::getInt16Ty(context);
    if (IS_INSTANCE(type, U32))
        return llvm::Type::getInt32Ty(context);
    if (IS_INSTANCE(type, U64))
        return llvm::Type::getInt64Ty(context);
    if (IS_INSTANCE(type, F32))
        return llvm::Type::getFloatTy(context);
    if (IS_INSTANCE(type, F64))
        return llvm::Type::getDoubleTy(context);
    if (IS_INSTANCE(type, Boolean))
        return llvm::Type::getInt1Ty(context);
    if (IS_INSTANCE(type, Void))
        return llvm::Type::getVoidTy(context);
    if (IS_INSTANCE(type, USize)) {
        if (sizeof(size_t) == 4) {
            return llvm::Type::getInt32Ty(context);
        } else if (sizeof(size_t) == 8) {
            return llvm::Type::getInt64Ty(context);
        } else {
            throw CodeGenError(node, "Unsupported platform: size_t is neither 32-bit nor 64-bit");
        }
    }
    if (IS_INSTANCE(type, ISize)) {
        if (sizeof(size_t) == 4) {
            return llvm::Type::getInt32Ty(context);
        } else if (sizeof(size_t) == 8) {
            return llvm::Type::getInt64Ty(context);
        } else {
            throw CodeGenError(node, "Unsupported platform: size_t is neither 32-bit nor 64-bit");
        }
    }
    if (IS_INSTANCE(type, StructType)) {
        auto structType = std::dynamic_pointer_cast<StructType>(type);
        auto it = m_structTypes.find(structType->name);
        if (it != m_structTypes.end()) {
            return it->second;
        } else {
            throw CodeGenError(nullptr, "Unknown struct type: " + structType->name);
        }
    }
    if (IS_INSTANCE(type, UnionType)) {
        auto unionType = std::dynamic_pointer_cast<UnionType>(type);
        auto it = m_unionTypes.find(unionType->name);
        if (it != m_unionTypes.end()) {
            return it->second;
        } else {
            throw CodeGenError(nullptr, "Unknown union type: " + unionType->name);
        }
    }
    if (IS_INSTANCE(type, PointerType)) {
        auto ptrType = std::dynamic_pointer_cast<PointerType>(type);
        return llvm::PointerType::getUnqual(context);
    }
    if (IS_INSTANCE(type, ArrayType)) {
        auto arrType = std::dynamic_pointer_cast<ArrayType>(type);
        // As struct { length (usize), data_ptr (element_type*) }
        std::vector<llvm::Type *> elements = {
            llvm::PointerType::getUnqual(context),
            llvm::Type::getInt64Ty(context)};
        auto st = llvm::StructType::get(context, elements);
        return st;
    }
    if (IS_INSTANCE(type, FunctionType)) {
        auto funcType = std::dynamic_pointer_cast<FunctionType>(type);
        std::vector<llvm::Type *> paramTypes;
        for (const auto &paramType : funcType->params) {
            paramTypes.push_back(getLLVMType(paramType, node));
        }
        return llvm::FunctionType::get(getLLVMType(funcType->ret, node), paramTypes,
                                       funcType->variadic);
    }
    if (IS_INSTANCE(type, ErrorUnionType)) {
        auto eut = std::dynamic_pointer_cast<ErrorUnionType>(type);
        if (IS_INSTANCE(eut->valueType, Void)) {
            // Represent as a struct { errorType, isError (i1) }
            std::vector<llvm::Type *> elements = {
                getLLVMType(eut->errorType, node),
                llvm::Type::getInt1Ty(context)};
            auto st = llvm::StructType::get(context, elements);
            return st;
        }
        // Represent as a struct { valueType, errorType, isError (i1) }
        std::vector<llvm::Type *> elements = {
            getLLVMType(eut->valueType, node),
            getLLVMType(eut->errorType, node),
            llvm::Type::getInt1Ty(context)};
        auto st = llvm::StructType::get(context, elements);
        return st;
    }
    if (IS_INSTANCE(type, EnumType)) {
        return getLLVMType(std::dynamic_pointer_cast<EnumType>(type)->base_type, node);
    }
    if (!type) {
        throw CodeGenError(node, "Type is null");
    } else {
        throw CodeGenError(node, "Unsupported type: " + type->str());
    }
    return nullptr;
}

llvm::Value *LLVMCodegen::generateAddress(const std::shared_ptr<Expression> &expr) {
    if (IS_INSTANCE(expr, VarAccess)) {
        auto var = std::dynamic_pointer_cast<VarAccess>(expr);
        auto *sym = lookupSymbolInScopes(m_scopeStack, canonicalizeNonexternName(var->name), var->name);
        return sym ? sym->value : nullptr;
    } else if (IS_INSTANCE(expr, OffsetAccess)) {
        return generateOffsetAccess(std::dynamic_pointer_cast<OffsetAccess>(expr),
                                    false);
    } else if (IS_INSTANCE(expr, FieldAccess)) {
        return generateFieldAccess(std::dynamic_pointer_cast<FieldAccess>(expr),
                                   false);
    } else if (IS_INSTANCE(expr, UnaryOperation)) {
        auto unOp = std::dynamic_pointer_cast<UnaryOperation>(expr);
        return generateUnaryOp(unOp, false);
    } else if (IS_INSTANCE(expr, TypeCast)) {
        auto typeCast = std::dynamic_pointer_cast<TypeCast>(expr);
        return generateAddress(typeCast->expr);
    } else if (IS_INSTANCE(expr, BinaryOperation)) {
        auto binOp = std::dynamic_pointer_cast<BinaryOperation>(expr);
        if (binOp->op == "=") {
            return generateAddress(binOp->left);
        } else {
            throw CodeGenError(expr,
                               "Left operand of binary operation is not an lvalue: " +
                                   expr->str());
        }
    } else if (IS_INSTANCE(expr, Dereference)) {
        auto deref = std::dynamic_pointer_cast<Dereference>(expr);

        auto base = generateExpression(deref->pointer, true);
        // Check for null ptr
        if (m_options.opt_level == Debug && m_options.do_runtime_safety) {
            llvm::Value *zero = llvm::ConstantPointerNull::get(
                llvm::PointerType::getUnqual(context));
            llvm::Value *isZero = m_builder.CreateICmpNE(base, zero, "isnullptrtmp");
            conditionOrPanic(isZero, NullPointerDereference, expr->line, expr->col);
        }
        return base;
    } else if (IS_INSTANCE(expr, ModuleAccess)) {
        return generateModuleAccess(std::dynamic_pointer_cast<ModuleAccess>(expr),
                                    false);
    } else if (IS_INSTANCE(expr, MethodCall)) {
        throw CodeGenError(expr,
                           "Cannot take address of method call result");
    }
    throw CodeGenError(expr, "Expression is not an lvalue: " + expr->str());
}

llvm::Value *
LLVMCodegen::generateExpression(const std::shared_ptr<Expression> &expr,
                                bool loadValue) {
    setDebugLocation(expr);
    if (IS_INSTANCE(expr, BinaryOperation)) {
        auto binOp = std::dynamic_pointer_cast<BinaryOperation>(expr);
        return generateBinaryOp(binOp->left, binOp->right, binOp->op, loadValue);
    }
    if (IS_INSTANCE(expr, UnaryOperation)) {
        auto unOp = std::dynamic_pointer_cast<UnaryOperation>(expr);
        return generateUnaryOp(unOp, loadValue);
    }
    if (IS_INSTANCE(expr, Literal)) {
        return generateLiteral(std::dynamic_pointer_cast<Literal>(expr), loadValue);
    }
    if (IS_INSTANCE(expr, VarAccess)) {
        return generateVarAccess(std::dynamic_pointer_cast<VarAccess>(expr),
                                 loadValue);
    }
    if (IS_INSTANCE(expr, EnumAccess)) {
        return generateEnumAccess(std::dynamic_pointer_cast<EnumAccess>(expr),
                                  loadValue);
    }
    if (IS_INSTANCE(expr, FuncCall)) {
        return generateFuncCall(std::dynamic_pointer_cast<FuncCall>(expr),
                                loadValue);
    }
    if (IS_INSTANCE(expr, FieldAccess)) {
        return generateFieldAccess(std::dynamic_pointer_cast<FieldAccess>(expr),
                                   loadValue);
    }
    if (IS_INSTANCE(expr, StructInitializer)) {
        return generateStructInitializer(
            std::dynamic_pointer_cast<StructInitializer>(expr), loadValue);
    }
    if (IS_INSTANCE(expr, OffsetAccess)) {
        return generateOffsetAccess(std::dynamic_pointer_cast<OffsetAccess>(expr),
                                    loadValue);
    }
    if (IS_INSTANCE(expr, TypeCast)) {
        return generateCast(std::dynamic_pointer_cast<TypeCast>(expr), loadValue);
    }
    if (IS_INSTANCE(expr, Dereference)) {
        auto deref = std::dynamic_pointer_cast<Dereference>(expr);
        llvm::Value *ptr = generateExpression(deref->pointer, true);
        if (m_options.opt_level == Debug && m_options.do_runtime_safety) {
            llvm::Value *zero = llvm::ConstantPointerNull::get(
                llvm::PointerType::getUnqual(context));
            llvm::Value *isZero = m_builder.CreateICmpNE(ptr, zero, "isnullptrtmp");
            conditionOrPanic(isZero, NullPointerDereference, expr->line, expr->col);
        }
        if (loadValue)
            return m_builder.CreateLoad(getLLVMType(deref->inferred_type, expr), ptr,
                                        "derefloadtmp");
        return ptr;
    }
    if (IS_INSTANCE(expr, MethodCall)) {
        return generateMethodCall(std::dynamic_pointer_cast<MethodCall>(expr),
                                  loadValue);
    }
    if (IS_INSTANCE(expr, ModuleAccess)) {
        return generateModuleAccess(std::dynamic_pointer_cast<ModuleAccess>(expr),
                                    loadValue);
    }
    if (IS_INSTANCE(expr, ArrayLiteral)) {
        return generateArrayLiteral(std::dynamic_pointer_cast<ArrayLiteral>(expr),
                                    loadValue);
    }
    throw CodeGenError(expr, "Unknown expression type: " + expr->str());
}
std::string llvmTypeToString(llvm::Type *ty) {
    std::string str;
    llvm::raw_string_ostream rso(str);
    ty->print(rso);
    return rso.str();
}

llvm::Value *
LLVMCodegen::generateCast(const std::shared_ptr<TypeCast> &typeCast,
                          bool loadValue) {
    llvm::Value *val = generateExpression(typeCast->expr);
    llvm::Type *destType = getLLVMType(typeCast->target_type, typeCast);
    if (!val || !destType) {
        throw CodeGenError(typeCast, "Invalid cast operation");
    }
    llvm::Type *srcType = val->getType();
    if (srcType == destType) {
        return val; // No cast needed
    }
    if (srcType->isIntegerTy() && destType->isIntegerTy()) {
        unsigned srcBits = srcType->getIntegerBitWidth();
        unsigned destBits = destType->getIntegerBitWidth();
        if (srcBits < destBits) {
            if (typeCast->expr->inferred_type->isUnsigned() || typeCast->expr->inferred_type->kind() == TypeKind::Bool)
                return m_builder.CreateZExt(val, destType, "zexttmp");
            else
                return m_builder.CreateSExt(val, destType, "sexttmp");
        } else {
            return m_builder.CreateTrunc(val, destType, "trunctmp");
        }
    } else if (srcType->isFloatingPointTy() && destType->isFloatingPointTy()) {
        unsigned srcBits = srcType->getPrimitiveSizeInBits();
        unsigned destBits = destType->getPrimitiveSizeInBits();
        if (srcBits < destBits) {
            return m_builder.CreateFPExt(val, destType, "fpexttmp");
        } else {
            return m_builder.CreateFPTrunc(val, destType, "fptrunctmp");
        }
    } else if (srcType->isIntegerTy() && destType->isFloatingPointTy()) {
        if (typeCast->expr->inferred_type->isUnsigned()) {
            return m_builder.CreateUIToFP(val, destType, "uitofptmp");
        }
        return m_builder.CreateSIToFP(val, destType, "sitofptmp");
    } else if (srcType->isFloatingPointTy() && destType->isIntegerTy()) {
        if (typeCast->target_type->isUnsigned()) {
            return m_builder.CreateFPToUI(val, destType, "fptouitmp");
        }
        return m_builder.CreateFPToSI(val, destType, "fptositmp");
    } else if (srcType->isPointerTy() && destType->isPointerTy()) {
        return m_builder.CreateBitCast(val, destType, "ptrcasttmp");
    } else if (srcType->isPointerTy() && destType->isIntegerTy()) {
        return m_builder.CreatePtrToInt(val, destType, "ptrtointtmp");
    } else if (srcType->isIntegerTy() && destType->isPointerTy()) {
        return m_builder.CreateIntToPtr(val, destType, "inttoptrtmp");
    }
    throw CodeGenError(typeCast, "Unsupported cast from " + llvmTypeToString(srcType) +
                                     " to " + llvmTypeToString(destType) + " at " + typeCast->str());
}

llvm::Value *
LLVMCodegen::generateStatement(const std::shared_ptr<Statement> &stmt) {
    setDebugLocation(stmt);
    if (IS_INSTANCE(stmt, VariableDeclaration)) {
        return generateVariableDeclaration(
            std::dynamic_pointer_cast<VariableDeclaration>(stmt));
    } else if (IS_INSTANCE(stmt, Block)) {
        return generateBlock(std::dynamic_pointer_cast<Block>(stmt));
    } else if (IS_INSTANCE(stmt, IfStatement)) {
        return generateIfStatement(std::dynamic_pointer_cast<IfStatement>(stmt));
    } else if (IS_INSTANCE(stmt, WhileStatement)) {
        return generateWhileStatement(
            std::dynamic_pointer_cast<WhileStatement>(stmt));
    } else if (IS_INSTANCE(stmt, ForStatement)) {
        return generateForStatement(std::dynamic_pointer_cast<ForStatement>(stmt));
    } else if (IS_INSTANCE(stmt, ReturnStatement)) {
        return generateReturnStatement(
            std::dynamic_pointer_cast<ReturnStatement>(stmt));
    } else if (IS_INSTANCE(stmt, ExpressionStatement)) {
        return generateExpressionStatement(
            std::dynamic_pointer_cast<ExpressionStatement>(stmt));
    } else if (IS_INSTANCE(stmt, ReturnStatement)) {
        return generateReturnStatement(
            std::dynamic_pointer_cast<ReturnStatement>(stmt));
    } else if (IS_INSTANCE(stmt, BreakStatement)) {
        llvm::Value *val = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
        m_builder.CreateBr(m_loop_stack.back().breakBB);
        return val;
    } else if (IS_INSTANCE(stmt, ContinueStatement)) {
        llvm::Value *val = llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
        m_builder.CreateBr(m_loop_stack.back().continueBB);
        return val;
    } else if (IS_INSTANCE(stmt, AsmStmt)) {
        return generateAsmStatement(std::dynamic_pointer_cast<AsmStmt>(stmt));
    } else if (IS_INSTANCE(stmt, SwitchStmt)) {
        return generateSwitchStatement(std::dynamic_pointer_cast<SwitchStmt>(stmt));
    }

    throw CodeGenError(stmt, "Unknown statement type: " + stmt->str());
}

llvm::Function *LLVMCodegen::generateFunctionDefinition(std::shared_ptr<FunctionDeclaration> func) {
    llvm::FunctionType *fType =
        llvm::cast<llvm::FunctionType>(this->getLLVMType(func->type, func));

    std::string fname = func->is_extern ? func->name : canonicalizeNonexternName(func->name);
    if (m_llvm_module->getFunction(fname)) {
        if (func->is_extern) {
            return m_llvm_module->getFunction(fname);
        }
        throw CodeGenError(func, "Duplicate defition of function: " + func->name);
    }

    // For extern functions, we need to apply ABI coercion to struct parameters and return types
    llvm::FunctionType *abiType = fType;
    if (func->is_extern) {
        std::vector<llvm::Type *> abiParamTypes;
        for (llvm::Type *paramType : fType->params()) {
            if (shouldCoerceForABI(paramType)) {
                abiParamTypes.push_back(getABICoercionType(paramType));
            } else {
                abiParamTypes.push_back(paramType);
            }
        }

        llvm::Type *abiRetType = fType->getReturnType();
        if (shouldCoerceForABI(abiRetType)) {
            abiRetType = getABICoercionType(abiRetType);
        }

        abiType = llvm::FunctionType::get(abiRetType, abiParamTypes, fType->isVarArg());
    }

    llvm::Function::LinkageTypes linkage = func->is_extern ? llvm::Function::ExternalLinkage : llvm::Function::ExternalLinkage;

    llvm::Function *function = llvm::Function::Create(
        abiType, linkage, fname, m_llvm_module.get());
    CUR_SCOPE.set(fname, function, fType, func->type);
    // Set names for all arguments
    unsigned int idx = 0;
    for (auto &arg : function->args()) {
        arg.setName(func->param_names[idx++]);
    }
    return function;
}

llvm::Function *LLVMCodegen::generateFunctionBody(std::shared_ptr<FunctionDeclaration> func, llvm::Function *function) {
    if (func->is_extern || !func->body) {
        return function; // No body to generate
    }

    m_scopeStack.push_back(Scope(CUR_SCOPE));
    struct ScopePopper {
        std::vector<Scope> &stack;
        ~ScopePopper() {
            if (stack.size() > 1) {
                stack.pop_back();
            }
        }
    } scopePopper{m_scopeStack};

    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(context, "entry", function);
    m_builder.SetInsertPoint(entry);

    if (m_emit_debug && m_di_builder) {
        llvm::DIFile *file = m_di_file ? m_di_file : getDIFileForPath(m_current_module ? m_current_module->path : "");
        unsigned line = func->line > 0 ? static_cast<unsigned>(func->line) : 1;
        unsigned col = func->col > 0 ? static_cast<unsigned>(func->col) : 1;
        std::vector<llvm::Metadata *> di_params;
        di_params.push_back(getDIType(func->type->ret, func));
        for (const auto &paramType : func->type->params) {
            di_params.push_back(getDIType(paramType, func));
        }
        auto funcType = m_di_builder->createSubroutineType(
            m_di_builder->getOrCreateTypeArray(di_params));
        auto subprogram = m_di_builder->createFunction(
            file,
            func->name,
            function->getName(),
            file,
            line,
            funcType,
            line,
            llvm::DINode::FlagPrototyped,
            llvm::DISubprogram::SPFlagDefinition);
        function->setSubprogram(subprogram);
        m_di_scope = subprogram;
        m_builder.SetCurrentDebugLocation(llvm::DILocation::get(context, line, col, m_di_scope));
    }
    // Allocate space for arguments and store them
    unsigned int idx = 0;
    for (auto &arg : function->args()) {
        llvm::AllocaInst *alloca =
            m_builder.CreateAlloca(arg.getType(), nullptr, arg.getName() + ".addr");
        m_builder.CreateStore(&arg, alloca);
        auto argname = arg.getName().str();
        CUR_SCOPE.set(canonicalizeNonexternName(argname), alloca, arg.getType(),
                      func->type->params[idx]);

        if (m_emit_debug && m_di_builder && m_di_scope) {
            llvm::DIFile *file = m_di_file ? m_di_file : getDIFileForPath(m_current_module ? m_current_module->path : "");
            unsigned line = func->line > 0 ? static_cast<unsigned>(func->line) : 1;
            unsigned col = func->col > 0 ? static_cast<unsigned>(func->col) : 1;
            llvm::DIType *diType = getDIType(func->type->params[idx], func);
            auto *var = m_di_builder->createParameterVariable(
                llvm::cast<llvm::DIScope>(m_di_scope),
                argname,
                idx + 1,
                file,
                line,
                diType,
                true);
            m_di_builder->insertDeclare(
                alloca,
                var,
                m_di_builder->createExpression(),
                llvm::DILocation::get(context, line, col, m_di_scope),
                m_builder.GetInsertBlock());
        }
        idx++;
    }

    if (IS_INSTANCE(func->type->ret, ErrorUnionType)) {
        m_error_union_return_type =
            std::dynamic_pointer_cast<ErrorUnionType>(func->type->ret);
    } else {
        m_error_union_return_type = nullptr;
    }
    generateStatement(func->body);
    if (m_builder.GetInsertBlock()->getTerminator() == nullptr) {
        emitDefaultReturn(func);
    }
    m_error_union_return_type = nullptr;

    if (m_emit_debug) {
        m_di_scope = m_di_compile_unit;
        m_builder.SetCurrentDebugLocation(llvm::DebugLoc());
    }

    return function;
}
//
// llvm::Function *IRGenerator::generateFunction(
//     const std::shared_ptr<FunctionDeclaration> &func) {
//   llvm::FunctionType *fType =
//       llvm::cast<llvm::FunctionType>(this->getLLVMType(func->type));
//
//   std::string fname = canonicalizeNonexternName(func->name);
//
//   if (m_llvm_module->getFunction(fname)) {
//     if (func->is_extern) {
//       return m_llvm_module->getFunction(fname);
//     }
//     throw CodeGenError(func, "Duplicate defition of function: " + func->name);
//   }
//
//   llvm::Function *function = llvm::Function::Create(
//       fType, llvm::Function::ExternalLinkage, fname, m_llvm_module.get());
//   CUR_SCOPE.set(fname, function, fType, func->type);
//   // Set names for all arguments
//   unsigned idx = 0;
//   for (auto &arg : function->args()) {
//     arg.setName(func->param_names[idx++]);
//   }
//
//   if (!func->body)
//     return function;
//
//   m_scopeStack.push_back(Scope(CUR_SCOPE));
//
//   llvm::BasicBlock *entry =
//       llvm::BasicBlock::Create(context, "entry", function);
//   m_builder.SetInsertPoint(entry);
//   // Allocate space for arguments and store them
//   idx = 0;
//   for (auto &arg : function->args()) {
//     llvm::AllocaInst *alloca =
//         m_builder.CreateAlloca(arg.getType(), nullptr, arg.getName() + ".addr");
//     m_builder.CreateStore(&arg, alloca);
//     auto argname = arg.getName().str();
//     CUR_SCOPE.set(canonicalizeNonexternName(argname), alloca, arg.getType(),
//                   func->type->params[idx]);
//     idx++;
//   }
//
//   if (IS_INSTANCE(func->type->ret, ErrorUnionType)) {
//     m_error_union_return_type =
//         std::dynamic_pointer_cast<ErrorUnionType>(func->type->ret);
//   } else {
//     m_error_union_return_type = nullptr;
//   }
//   generateStatement(func->body);
//   if (m_builder.GetInsertBlock()->getTerminator() == nullptr) {
//     if (IS_INSTANCE(func->type->ret, Void)) {
//       m_builder.CreateRetVoid();
//     } else {
//       throw CodeGenError(func, "Non-void function missing return: " + func->name);
//     }
//   }
//   m_error_union_return_type = nullptr;
//
//   return function;
// }
//

void LLVMCodegen::finished(bool is_final_module) {
    if (!is_final_module) {
        return;
    }
    // Finalize global variable initialization function
    llvm::Function *globalVarInitFunc = m_llvm_module->getFunction(GLOBAL_VAR_INIT_FUNC_NAME);
    if (globalVarInitFunc) {
        llvm::BasicBlock *initBB = nullptr;
        for (auto &bb : *globalVarInitFunc) {
            if (bb.getName() == "init") {
                initBB = &bb;
                break;
            }
        }
        if (initBB) {
            llvm::BasicBlock *currentBB = m_builder.GetInsertBlock();
            m_builder.SetInsertPoint(initBB);
            m_builder.CreateRetVoid();
            m_builder.SetInsertPoint(currentBB);
        }
    }

    finalizeDebugInfo();
}
llvm::Value *LLVMCodegen::addGlobalVarInitializer(llvm::Value *var, std::shared_ptr<Expression> initializer) {
    if (!var || !initializer) {
        return nullptr; // No initializer to add
    }

    static llvm::BasicBlock *initBB;
    if (!initBB) {

        llvm::FunctionType *initFuncType =
            llvm::FunctionType::get(llvm::Type::getVoidTy(context), false);
        llvm::Function *initFunc = llvm::Function::Create(initFuncType,
                                                          llvm::Function::InternalLinkage,
                                                          GLOBAL_VAR_INIT_FUNC_NAME, m_llvm_module.get());
        registerGlobalCtor(*m_llvm_module, initFunc, 65535);

        initBB = llvm::BasicBlock::Create(context, "init", initFunc);
    }
    llvm::BasicBlock *currentBB = m_builder.GetInsertBlock();
    m_builder.SetInsertPoint(initBB);
    llvm::Value *initVal = nullptr;
    initVal = generateExpression(std::dynamic_pointer_cast<Expression>(initializer));

    if (initVal == nullptr) {
        throw CodeGenError(initializer, "Failed to generate initializer for global variable");
        initVal = llvm::Constant::getNullValue(var->getType());
    }
    m_builder.CreateStore(initVal, var);
    m_builder.SetInsertPoint(currentBB);
    return initVal;
}

llvm::Constant *LLVMCodegen::getConstantLiteralValue(const std::shared_ptr<ASTNode> &literal) {
    auto resolveSymbolConstant = [&](const std::string &name,
                                     const std::shared_ptr<Module> &module = nullptr) -> llvm::Constant * {
        std::string full_name = name;
        if (module) {
            if (module->externLinkage.find(name) == module->externLinkage.end()) {
                full_name = module->canonicalizeName(name);
            }
        } else if (m_current_module && m_current_module->externLinkage.find(name) == m_current_module->externLinkage.end()) {
            full_name = m_current_module->canonicalizeName(name);
        }

        auto *sym = lookupSymbolInScopes(m_scopeStack, full_name, name);
        if (!sym || !sym->value) {
            return nullptr;
        }

        if (auto *constant = llvm::dyn_cast<llvm::Constant>(sym->value)) {
            if (auto *globalVar = llvm::dyn_cast<llvm::GlobalVariable>(constant)) {
                if (globalVar->hasInitializer()) {
                    return globalVar->getInitializer();
                }
            }
            return constant;
        }

        return nullptr;
    };

    if (IS_INSTANCE(literal, Literal)) {
        auto lit = generateLiteral(std::dynamic_pointer_cast<Literal>(literal), false);
        if (llvm::isa<llvm::Constant>(lit)) {
            return llvm::cast<llvm::Constant>(lit);
        } else {
            return nullptr;
        }
    }
    if (IS_INSTANCE(literal, VarAccess)) {
        auto var = std::dynamic_pointer_cast<VarAccess>(literal);
        return resolveSymbolConstant(var->name);
    }
    if (IS_INSTANCE(literal, ModuleAccess)) {
        auto moduleAccess = std::dynamic_pointer_cast<ModuleAccess>(literal);
        auto mod = m_current_module->getImportedModule(moduleAccess->module_path);
        if (!mod) {
            return nullptr;
        }
        return resolveSymbolConstant(moduleAccess->member_name, mod);
    }
    if (IS_INSTANCE(literal, StructInitializer)) {
        auto structInit = std::dynamic_pointer_cast<StructInitializer>(literal);
        std::vector<llvm::Constant *> fieldValues;
        auto structTy = std::dynamic_pointer_cast<StructType>(structInit->inferred_type);
        if (!structTy) {
            return nullptr;
        }

        for (const auto &declField : structTy->fields) {
            auto it = structInit->field_values.find(declField.first);
            if (it == structInit->field_values.end()) {
                return nullptr;
            }
            llvm::Constant *fieldVal = getConstantLiteralValue(it->second);
            if (!fieldVal) {
                return nullptr;
            }
            fieldValues.push_back(fieldVal);
        }
        llvm::StructType *structType = llvm::cast<llvm::StructType>(getLLVMType(structInit->inferred_type, literal));
        return llvm::ConstantStruct::get(structType, fieldValues);
    }
    if (IS_INSTANCE(literal, TypeCast)) {
        auto typeCast = std::dynamic_pointer_cast<TypeCast>(literal);
        auto innerConst = getConstantLiteralValue(typeCast->expr);
        if (!innerConst) {
            return nullptr;
        }
        llvm::Type *targetTy = getLLVMType(typeCast->target_type, literal);
        if (innerConst->getType() == targetTy) {
            return innerConst;
        }
        llvm::Type *sourceTy = innerConst->getType();
        llvm::Instruction::CastOps castOp;
        bool hasCast = true;

        if (sourceTy->isIntegerTy() && targetTy->isIntegerTy()) {
            unsigned srcBits = sourceTy->getIntegerBitWidth();
            unsigned dstBits = targetTy->getIntegerBitWidth();
            if (srcBits < dstBits) {
                bool isSigned = !typeCast->expr->inferred_type || !typeCast->expr->inferred_type->isUnsigned();
                castOp = isSigned ? llvm::Instruction::SExt : llvm::Instruction::ZExt;
            } else {
                castOp = llvm::Instruction::Trunc;
            }
        } else if (sourceTy->isFloatingPointTy() && targetTy->isFloatingPointTy()) {
            castOp = sourceTy->getPrimitiveSizeInBits() < targetTy->getPrimitiveSizeInBits()
                         ? llvm::Instruction::FPExt
                         : llvm::Instruction::FPTrunc;
        } else if (sourceTy->isIntegerTy() && targetTy->isFloatingPointTy()) {
            bool isSigned = !typeCast->expr->inferred_type || !typeCast->expr->inferred_type->isUnsigned();
            castOp = isSigned ? llvm::Instruction::SIToFP : llvm::Instruction::UIToFP;
        } else if (sourceTy->isFloatingPointTy() && targetTy->isIntegerTy()) {
            bool isUnsigned = typeCast->target_type && typeCast->target_type->isUnsigned();
            castOp = isUnsigned ? llvm::Instruction::FPToUI : llvm::Instruction::FPToSI;
        } else if (sourceTy->isPointerTy() && targetTy->isPointerTy()) {
            castOp = llvm::Instruction::BitCast;
        } else if (sourceTy->isPointerTy() && targetTy->isIntegerTy()) {
            castOp = llvm::Instruction::PtrToInt;
        } else if (sourceTy->isIntegerTy() && targetTy->isPointerTy()) {
            castOp = llvm::Instruction::IntToPtr;
        } else {
            hasCast = false;
        }

        if (hasCast) {
            return llvm::ConstantExpr::getCast(castOp, innerConst, targetTy);
        }
        return nullptr;
    }
    if (IS_INSTANCE(literal, UnaryOperation)) {
        auto unOp = std::dynamic_pointer_cast<UnaryOperation>(literal);
        if (unOp->op == "&") {
            ExprPtr opd = unOp->operand;
            if (auto va = std::dynamic_pointer_cast<VarAccess>(opd)) {
                return resolveSymbolConstant(va->name);
            } else if (auto ma = std::dynamic_pointer_cast<ModuleAccess>(opd)) {
                auto mod = m_current_module->getImportedModule(ma->module_path);
                if (!mod)
                    return nullptr;
                return resolveSymbolConstant(ma->member_name, mod);
            }
            return nullptr;
        }
    }
    if (IS_INSTANCE(literal, ArrayLiteral)) {
        auto arrayLit = std::dynamic_pointer_cast<ArrayLiteral>(literal);
        std::vector<llvm::Constant *> elementValues;
        for (const auto &elem : arrayLit->elements) {
            llvm::Constant *elemVal = getConstantLiteralValue(elem);
            if (!elemVal) {
                return nullptr;
            }
            elementValues.push_back(elemVal);
        }
        llvm::StructType *arrayType = llvm::cast<llvm::StructType>(getLLVMType(arrayLit->inferred_type, literal));
        llvm::Constant *dataPtr = llvm::ConstantArray::get(llvm::ArrayType::get(elementValues[0]->getType(), elementValues.size()), elementValues);
        return llvm::ConstantStruct::get(arrayType, {dataPtr, llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), elementValues.size())});
    }
    return nullptr;
}

llvm::Value *LLVMCodegen::generateVariableDeclaration(
    const std::shared_ptr<VariableDeclaration> &varDecl) {
    static int varCounter = 0;
    bool is_global = m_scopeStack.size() == 1;

    if (is_global) {
        if (varDecl->is_extern) {
            // Extern global variable declaration
            // Don't canonicalize name due to extern linkage
            llvm::Type *varType = getLLVMType(varDecl->var_type, varDecl);
            llvm::GlobalVariable *gVar = new llvm::GlobalVariable(
                *m_llvm_module, varType, varDecl->is_const,
                llvm::GlobalValue::ExternalLinkage, nullptr,
                varDecl->name);
            CUR_SCOPE.set(varDecl->name, gVar, varType, varDecl->var_type);
            return gVar;
        }
        llvm::Type *varType = getLLVMType(varDecl->var_type, varDecl);
        llvm::Constant *initVal = nullptr;
        bool has_const_init = false;
        if (varDecl->initializer->constant_evaluated) {
            auto ret = getConstantLiteralValue(varDecl->initializer);
            if (ret) {
                initVal = ret;
                has_const_init = true;
            }
        }

        if (!initVal) {
            initVal = llvm::Constant::getNullValue(varType);
        }

        llvm::GlobalVariable *gVar = new llvm::GlobalVariable(
            *m_llvm_module, varType, false,
            llvm::GlobalValue::ExternalLinkage, initVal, canonicalizeNonexternName(varDecl->name));
        CUR_SCOPE.set(canonicalizeNonexternName(varDecl->name), gVar, varType, varDecl->var_type);
        if (m_emit_debug && m_di_builder && m_di_compile_unit) {
            llvm::DIFile *file = m_di_file ? m_di_file : getDIFileForPath(m_current_module ? m_current_module->path : "");
            unsigned line = varDecl->line > 0 ? static_cast<unsigned>(varDecl->line) : 1;
            llvm::DIType *diType = getDIType(varDecl->var_type, varDecl);
            auto *expr = m_di_builder->createGlobalVariableExpression(
                m_di_compile_unit,
                varDecl->name,
                gVar->getName(),
                file,
                line,
                diType,
                false);
            gVar->addDebugInfo(expr);
        }
        if (varDecl->initializer && !has_const_init) {
            if (auto expr = std::dynamic_pointer_cast<Expression>(varDecl->initializer)) {
                addGlobalVarInitializer(gVar, expr);
            } else {
                throw CodeGenError(varDecl, "Unsupported initializer type for global variable");
            }
        }
        return gVar;
    }

    llvm::Type *varType = getLLVMType(varDecl->var_type, varDecl);
    llvm::Function *currentFunction = m_builder.GetInsertBlock() ? m_builder.GetInsertBlock()->getParent() : nullptr;
    llvm::Value *alloca = createEntryBlockAlloca(currentFunction, varType, varDecl->name);
    if (!alloca) {
        alloca = m_builder.CreateAlloca(varType, nullptr, varDecl->name);
    }
    CUR_SCOPE.set(canonicalizeNonexternName(varDecl->name), alloca, varType, varDecl->var_type);
    if (m_emit_debug && m_di_builder && m_di_scope) {
        llvm::DIFile *file = m_di_file ? m_di_file : getDIFileForPath(m_current_module ? m_current_module->path : "");
        unsigned line = varDecl->line > 0 ? static_cast<unsigned>(varDecl->line) : 1;
        unsigned col = varDecl->col > 0 ? static_cast<unsigned>(varDecl->col) : 1;
        llvm::DIType *diType = getDIType(varDecl->var_type, varDecl);
        auto *var = m_di_builder->createAutoVariable(
            llvm::cast<llvm::DIScope>(m_di_scope),
            varDecl->name,
            file,
            line,
            diType);
        m_di_builder->insertDeclare(
            alloca,
            var,
            m_di_builder->createExpression(),
            llvm::DILocation::get(context, line, col, m_di_scope),
            m_builder.GetInsertBlock());
    }
    if (varDecl->initializer) {
        llvm::Value *initVal = nullptr;
        if (IS_INSTANCE(varDecl->initializer, Expression)) {
            initVal = generateExpression(
                std::dynamic_pointer_cast<Expression>(varDecl->initializer));
        } else {
            throw CodeGenError(varDecl, "Unsupported initializer type");
        }
        if (initVal == nullptr) {
            throw CodeGenError(varDecl, "Failed to generate initializer for variable: " +
                                            varDecl->name);
            initVal = llvm::Constant::getNullValue(varType);
        }
        m_builder.CreateStore(initVal, alloca);
    }
    return alloca;
}

std::vector<std::pair<llvm::Function *, std::shared_ptr<FunctionDeclaration>>> LLVMCodegen::generateStructMethods(
    const std::shared_ptr<StructDeclaration> &structDecl) {
    std::vector<std::pair<llvm::Function *, std::shared_ptr<FunctionDeclaration>>> methods;
    for (std::unordered_map<std::string, std::shared_ptr<FunctionDeclaration>>::iterator iter = structDecl->methods.begin();
         iter != structDecl->methods.end(); ++iter) {
        auto method = iter->second;
        std::string mangledName = structDecl->name + "." + method->name;
        method->name = mangledName;

        // Mark struct methods as having external linkage so they're not namespaced
        // This allows methods to be called across modules
        m_current_module->externLinkage.insert(mangledName);

        // HACK: Just get this done for now, should probably be better somehow
        llvm::Function *fn = generateFunctionDefinition(method);
        methods.push_back({fn, method});
    }
    return methods;
}

void LLVMCodegen::generateStructDeclaration(
    const std::shared_ptr<StructDeclaration> &structDecl) {

    llvm::StructType *llvmStruct =
        llvm::StructType::create(context, structDecl->name);

    m_structTypes[structDecl->name] = llvmStruct;

    std::vector<llvm::Type *> fieldTypes;
    for (const auto &field : structDecl->fields) {
        llvm::Type *ty = getLLVMType(field.second, structDecl);
        fieldTypes.push_back(ty);
    }

    llvmStruct->setBody(fieldTypes, /*packed=*/false);
}

void LLVMCodegen::generateUnionDeclaration(
    const std::shared_ptr<UnionDeclaration> &unionDecl) {

    llvm::StructType *llvmUnion =
        llvm::StructType::create(context, unionDecl->name);

    m_unionTypes[unionDecl->name] = llvmUnion;

    // Unions in LLVM are represented as a struct with a single field
    // that is the size of the largest field in the union.
    // We'll use an array of bytes (i8) to represent the union storage.

    size_t maxSize = 0;
    size_t maxAlign = 1;

    for (const auto &field : unionDecl->fields) {
        llvm::Type *ty = getLLVMType(field.second, unionDecl);
        size_t fieldSize = m_llvm_module->getDataLayout().getTypeAllocSize(ty);
        size_t fieldAlign = m_llvm_module->getDataLayout().getABITypeAlign(ty).value();

        if (fieldSize > maxSize) {
            maxSize = fieldSize;
        }
        if (fieldAlign > maxAlign) {
            maxAlign = fieldAlign;
        }
    }

    // Create a byte array of the appropriate size to hold the largest field
    llvm::Type *storageType = llvm::ArrayType::get(
        llvm::Type::getInt8Ty(context), maxSize);

    llvmUnion->setBody({storageType}, /*packed=*/false);
}

void LLVMCodegen::generateEnumDeclaration(
    const std::shared_ptr<EnumDeclaration> &enumDecl) {
    // No actual code-gen here, just putting the type in global scope
    m_enumTypes[enumDecl->name] =
        std::dynamic_pointer_cast<EnumType>(enumDecl->inferred_type);
}

llvm::Value *LLVMCodegen::generateBinaryOp(
    const std::shared_ptr<Expression> &left,
    const std::shared_ptr<Expression> &right,
    std::string op, bool loadValue) {

    if (!left || !right) {
        throw CodeGenError(nullptr, "Invalid binary operation: null operand expression");
    }

    if (op == "&&" || op == "||") { // These are special :)
        return generateLogicalOp(left, right, op);
    }

    llvm::Value *l = nullptr;
    if (op == "=") {
        l = generateAddress(left);
    } else {
        l = generateExpression(left);
    }

    llvm::Value *r = nullptr;
    if (op == "=") {

        if (right->inferred_type && right->inferred_type->kind() == TypeKind::Function) {
            throw CodeGenError(right, "Cannot assign function to variable, try using '&' to get function pointer");
        }
        r = generateExpression(right);
    } else {
        r = generateExpression(right);
    }

    // Handle assignment separately
    if (op == "=") {
        if (!l) {
            throw CodeGenError(left, "Left operand of assignment is not an lvalue");
        }
        m_builder.CreateStore(r, l);
        return r;
    }

    if (!l || !r) {
        throw CodeGenError(nullptr, "Failed to generate LLVM value for binary operands");
    }

    llvm::Type *ty = l->getType();
    if (!ty) {
        throw CodeGenError(left, "Left operand of binary op has no type");
    }

    static const std::map<std::string, OpInfo> ops = {
        {"+",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateAdd(a, b, "add");
          },
          [this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateFAdd(a, b, "add");
          }}},
        {"-",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateSub(a, b, "sub");
          },
          [this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateFSub(a, b, "sub");
          }}},
        {"*",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateMul(a, b, "mul");
          },
          [this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateFMul(a, b, "mul");
          }}},
        {"/",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateSDiv(a, b, "div");
          },
          [this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateFDiv(a, b, "div");
          }}},
        {"%",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateSRem(a, b, "mod");
          },
          [this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateFRem(a, b, "mod");
          }}},

        {"==",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateICmpEQ(a, b, "eq");
          },
          [this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateFCmpUEQ(a, b, "eq");
          }}},
        {"!=",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateICmpNE(a, b, "ne");
          },
          [this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateFCmpUNE(a, b, "ne");
          }}},
        {"<",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateICmpSLT(a, b, "lt");
          },
          [this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateFCmpULT(a, b, "lt");
          }}},
        {"<=",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateICmpSLE(a, b, "le");
          },
          [this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateFCmpULE(a, b, "le");
          }}},
        {">",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateICmpSGT(a, b, "gt");
          },
          [this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateFCmpUGT(a, b, "gt");
          }}},
        {">=",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateICmpSGE(a, b, "ge");
          },
          [this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateFCmpUGE(a, b, "ge");
          }}},
        {"<<",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateShl(a, b, "shl");
          },
          nullptr}},
        {">>",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateAShr(a, b, "shr");
          },
          nullptr}},
        {"&",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateAnd(a, b, "and");
          },
          nullptr}},
        {"|",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateOr(a, b, "or");
          },
          nullptr}},
        {"^",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateXor(a, b, "xor");
          },
          nullptr}},
    };

    // Pointer arithmetic handling
    if (ty->isPointerTy()) {
        if (op == "+" || op == "-") {
            // For pointer arithmetic, use GEP instruction
            // If right operand is a pointer (from cast), convert to integer
            if (r->getType()->isPointerTy()) {
                r = m_builder.CreatePtrToInt(r, llvm::Type::getInt64Ty(context), "ptrtoint_offset");
            }
            // Ensure right operand is integer type
            if (!r->getType()->isIntegerTy()) {
                throw CodeGenError(right, "Cannot add/subtract non-integer to pointer");
            }
            // For subtraction, negate the offset
            if (op == "-") {
                r = m_builder.CreateNeg(r, "neg_offset");
            }

            // Use GEP for pointer arithmetic (returns pointer)
            return m_builder.CreateGEP(llvm::Type::getInt8Ty(context), l, r, "ptrarith");
        }

        // For other operations, convert both to integers
        ty = llvm::Type::getInt64Ty(context);
        l = m_builder.CreatePtrToInt(l, ty, "ptrtoint_lhs");
        r = m_builder.CreatePtrToInt(r, ty, "ptrtoint_rhs");
        if (!l || !r) {
            throw CodeGenError(nullptr, "Failed to convert pointer operands to integer");
        }
    }

    // Ensure both operands have the same type
    if (l->getType() != r->getType()) {
        throw CodeGenError(left, "Type mismatch in binary op: lhs=" +
                                     llvmTypeToString(l->getType()) +
                                     " rhs=" + llvmTypeToString(r->getType()));
    }

    // Check operator support
    auto it = ops.find(op);
    if (it == ops.end()) {
        throw CodeGenError(right, "Unsupported binary operator: " + op);
    }

    const OpInfo &info = it->second;
    if (m_options.opt_level == Debug && m_options.do_runtime_safety) {
        if (op == "/" && r->getType()->isIntegerTy()) {
            llvm::Value *zero = llvm::ConstantInt::get(r->getType(), 0);
            llvm::Value *isZero = m_builder.CreateICmpNE(r, zero, "divbyzerotmp");
            conditionOrPanic(isZero, DivisionByZero, left->line, left->col);
        } else if ((op == "/" || op == "%") && r->getType()->isFloatingPointTy()) {
            llvm::Value *zero = llvm::ConstantFP::get(r->getType(), 0.0);
            llvm::Value *isZero = m_builder.CreateFCmpUNE(r, zero, "fpdivbyzerotmp");
            conditionOrPanic(isZero, DivisionByZero, left->line, left->col);
        }
    }
    if (ty->isIntegerTy())
        return info.intOp(l, r);
    else if (ty->isFloatingPointTy())
        return info.floatOp(l, r);
    else {
        throw CodeGenError(right,
                           "Unsupported type for binary operator: " + llvmTypeToString(ty));
    }
}

llvm::Value *
LLVMCodegen::generateUnaryOp(const std::shared_ptr<UnaryOperation> &operation,
                             bool load_value) {
    auto val = generateExpression(operation->operand);
    auto op = operation->op;
    if (op == "-") {
        if (val->getType()->isFloatingPointTy()) {
            return m_builder.CreateFNeg(val, "fnegtmp");
        }
        return m_builder.CreateNeg(val, "negtmp");
    } else if (op == "+")
        return val; // Unary plus is a no-op
    else if (op == "!")
        return m_builder.CreateNot(val, "nottmp");
    else if (op == "&")
        return generateAddress(operation->operand);
    else if (op == "~") {
        return m_builder.CreateNot(val, "bwnottmp");
    } else if (op == "++") {
        if (operation->is_prefix) {
            llvm::Value *one = llvm::ConstantInt::get(val->getType(), 1);
            llvm::Value *inc = m_builder.CreateAdd(val, one, "inc");
            llvm::Value *addr = generateAddress(operation->operand);
            m_builder.CreateStore(inc, addr);
            return inc;
        } else {
            llvm::Value *one = llvm::ConstantInt::get(val->getType(), 1);
            llvm::Value *inc = m_builder.CreateAdd(val, one, "inc");
            llvm::Value *addr = generateAddress(operation->operand);
            m_builder.CreateStore(inc, addr);
            return val;
        }
    } else if (op == "--") {
        if (operation->is_prefix) {
            llvm::Value *one = llvm::ConstantInt::get(val->getType(), 1);
            llvm::Value *dec = m_builder.CreateSub(val, one, "dec");
            llvm::Value *addr = generateAddress(operation->operand);
            m_builder.CreateStore(dec, addr);
            return dec;
        } else {
            llvm::Value *one = llvm::ConstantInt::get(val->getType(), 1);
            llvm::Value *dec = m_builder.CreateSub(val, one, "dec");
            llvm::Value *addr = generateAddress(operation->operand);
            m_builder.CreateStore(dec, addr);
            return val;
        }
    }

    throw CodeGenError(operation, "Unsupported unary operator: " + op);
}

llvm::Value *LLVMCodegen::generateLogicalOp(std::shared_ptr<Expression> left, std::shared_ptr<Expression> right, std::string op) {
    llvm::Value *valLeft = generateExpression(left);

    llvm::BasicBlock *startBlock = m_builder.GetInsertBlock();
    llvm::Function *currentFunc = startBlock->getParent();
    llvm::BasicBlock *rhs = llvm::BasicBlock::Create(context, op == "&&" ? "land.rhs" : "lor.rhs", currentFunc);
    llvm::BasicBlock *end = llvm::BasicBlock::Create(context, op == "&&" ? "land.end" : "lor.end", currentFunc);

    llvm::Value *valRight = nullptr;
    if (op == "&&") {
        m_builder.CreateCondBr(valLeft, rhs, end);

        m_builder.SetInsertPoint(rhs);
        valRight = generateExpression(right);

        m_builder.CreateBr(end);
    } else {

        m_builder.CreateCondBr(valLeft, end, rhs);

        m_builder.SetInsertPoint(rhs);
        valRight = generateExpression(right);

        m_builder.CreateBr(end);
    }

    m_builder.SetInsertPoint(end);
    llvm::PHINode *phi = m_builder.CreatePHI(llvm::Type::getInt1Ty(context), 2, "phitmp");

    phi->addIncoming(llvm::ConstantInt::get(context, llvm::APInt(1, op == "||" ? 1 : 0)), startBlock);
    phi->addIncoming(valRight, rhs);

    return phi;
}

template <typename T>
T tryGetConstValue(std::shared_ptr<Literal> lit) {
    auto val = lit->value;
    if (std::holds_alternative<T>(val)) {
        return std::get<T>(val);
    }
    std::cerr << "Failed to get constant value of requested type (" << lit->lit_type->str() << ")\n";
    std::cerr << "Actual type: ";
    if (std::holds_alternative<int64_t>(val)) {
        std::cerr << "int64_t\n";
    } else if (std::holds_alternative<uint64_t>(val)) {
        std::cerr << "uint64_t\n";
    } else if (std::holds_alternative<double>(val)) {
        std::cerr << "double\n";
    } else if (std::holds_alternative<bool>(val)) {
        std::cerr << "bool\n";
    } else if (std::holds_alternative<std::string>(val)) {
        std::cerr << "string\n";
    } else {
        std::cerr << "unknown\n";
    }
    throw std::bad_variant_access();
}

llvm::Value *LLVMCodegen::generateLiteral(const std::shared_ptr<Literal> &lit,
                                          bool loadValue) {

    if (IS_INSTANCE(lit->inferred_type, I32)) {
        return llvm::ConstantInt::get(context,
                                      llvm::APInt(32, tryGetConstValue<int64_t>(lit)));
    } else if (IS_INSTANCE(lit->inferred_type, I16)) {
        return llvm::ConstantInt::get(context,
                                      llvm::APInt(16, tryGetConstValue<int64_t>(lit)));
    } else if (IS_INSTANCE(lit->inferred_type, I8)) {
        return llvm::ConstantInt::get(context,
                                      llvm::APInt(8, tryGetConstValue<int64_t>(lit)));
    } else if (IS_INSTANCE(lit->inferred_type, I64)) {
        return llvm::ConstantInt::get(context,
                                      llvm::APInt(64, tryGetConstValue<int64_t>(lit)));
    } else if (IS_INSTANCE(lit->inferred_type, U8)) {
        return llvm::ConstantInt::get(context,
                                      llvm::APInt(8, tryGetConstValue<uint64_t>(lit)));
    } else if (IS_INSTANCE(lit->inferred_type, U16)) {
        return llvm::ConstantInt::get(context,
                                      llvm::APInt(16, tryGetConstValue<uint64_t>(lit)));
    } else if (IS_INSTANCE(lit->inferred_type, U32)) {
        return llvm::ConstantInt::get(context,
                                      llvm::APInt(32, tryGetConstValue<uint64_t>(lit)));
    } else if (IS_INSTANCE(lit->inferred_type, U64)) {
        return llvm::ConstantInt::get(context,
                                      llvm::APInt(64, tryGetConstValue<uint64_t>(lit)));
    } else if (IS_INSTANCE(lit->inferred_type, F32)) {
        return llvm::ConstantFP::get(m_builder.getFloatTy(),
                                     tryGetConstValue<double>(lit));
    } else if (IS_INSTANCE(lit->inferred_type, F64)) {
        return llvm::ConstantFP::get(m_builder.getDoubleTy(),
                                     tryGetConstValue<double>(lit));
    } else if (IS_INSTANCE(lit->inferred_type, Boolean)) {
        return llvm::ConstantInt::get(
            context, llvm::APInt(1, tryGetConstValue<bool>(lit) ? 1 : 0));
    } else if (IS_INSTANCE(lit->inferred_type, Void)) {
        throw CodeGenError(lit, "Cannot generate literal for void type");
    } else if (IS_INSTANCE(lit->inferred_type, USize)) {
        unsigned bits = m_llvm_module->getDataLayout().getPointerSizeInBits();
        return llvm::ConstantInt::get(
            context,
            llvm::APInt(bits, tryGetConstValue<uint64_t>(lit)));
    } else if (IS_INSTANCE(lit->inferred_type, ISize)) {
        unsigned bits = m_llvm_module->getDataLayout().getPointerSizeInBits();
        return llvm::ConstantInt::get(
            context,
            llvm::APInt(bits, tryGetConstValue<int64_t>(lit)));
    } else if (IS_INSTANCE(lit->inferred_type, PointerType)) {
        if (std::holds_alternative<uint64_t>(lit->value)) {
            int intVal = tryGetConstValue<uint64_t>(lit);
            if (intVal == 0) {
                // Null pointer literal
                return llvm::ConstantPointerNull::get(
                    llvm::cast<llvm::PointerType>(getLLVMType(lit->inferred_type, lit)));
            } else {
                throw CodeGenError(lit, "Only null (0) is allowed as integer pointer literal");
            }
        } else if (std::holds_alternative<std::string>(lit->value)) {
            // String literal → create global string
            auto strVal = tryGetConstValue<std::string>(lit);
            auto strName = "g" + std::to_string(globalVals++);
            auto strType = llvm::ArrayType::get(llvm::Type::getInt8Ty(context),
                                                strVal.size() + 1);
            auto strConstant =
                llvm::ConstantDataArray::getString(context, strVal, true);
            auto globalStr = new llvm::GlobalVariable(
                *m_llvm_module, strType, true, llvm::GlobalValue::PrivateLinkage,
                strConstant, strName);
            llvm::Constant *zero =
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0);
            llvm::Constant *indices[] = {zero, zero};
            return llvm::ConstantExpr::getGetElementPtr(strType, globalStr, indices,
                                                        true);
        } else {
            throw CodeGenError(lit, "Unsupported pointer literal value type");
        }
    } else if (IS_INSTANCE(lit->inferred_type, NullType)) {
        return llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(
            getLLVMType(std::make_shared<PointerType>(std::make_shared<Void>()), lit)));
    } else if (IS_INSTANCE(lit->inferred_type, ArrayType)) {
        return generateArrayLiteral(std::dynamic_pointer_cast<ArrayLiteral>(lit), loadValue);
    }
    if (!lit->inferred_type) {
        throw CodeGenError(lit, "Literal " + lit->str() + " has no inferred type");
    }
    throw CodeGenError(lit, "Unsupported literal type: " + lit->inferred_type->str());
}

llvm::Value *LLVMCodegen::generateArrayLiteral(
    const std::shared_ptr<ArrayLiteral> &arrayLit, bool loadValue) {

    auto arrayType = std::dynamic_pointer_cast<ArrayType>(arrayLit->inferred_type);
    auto elemType = getLLVMType(arrayType->element_type, arrayLit);
    int numElements = arrayLit->len;

    // Allocate the raw array: [N x elemType]
    auto arrayLLVMType = getLLVMType(arrayType, arrayLit); // struct { ptr, i64 }
    auto arrayLitType = llvm::ArrayType::get(elemType, numElements);
    auto rawArrAlloc = m_builder.CreateAlloca(arrayLitType, nullptr, "arraylit");
    rawArrAlloc->setAlignment(llvm::Align(alignof(void *)));

    // Store elements in array using correct GEP (2 indices for array)
    for (int i = 0; i < numElements; i++) {
        llvm::Value *elemVal = generateExpression(arrayLit->elements[i]);
        llvm::Value *elemPtr = m_builder.CreateGEP(
            arrayLitType,
            rawArrAlloc,
            {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0),
             llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), i)});
        m_builder.CreateStore(elemVal, elemPtr);
    }

    // Allocate struct { ptr, i64 }
    llvm::Value *arrayStructAlloc = m_builder.CreateAlloca(arrayLLVMType, nullptr, "arraystruct");

    // Store array length into struct
    llvm::Value *lengthPtr = m_builder.CreateGEP(
        arrayLLVMType, arrayStructAlloc,
        {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0),
         llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 1)});
    m_builder.CreateStore(llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), numElements), lengthPtr);

    // Store pointer to raw array into struct (cast to i8* or element pointer type)
    llvm::Value *dataPtr = m_builder.CreateGEP(
        arrayLLVMType, arrayStructAlloc,
        {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0),
         llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0)});
    llvm::Value *castedPtr = m_builder.CreateBitCast(rawArrAlloc, llvm::PointerType::getUnqual(context));
    m_builder.CreateStore(castedPtr, dataPtr);

    if (loadValue) {
        return m_builder.CreateLoad(arrayLLVMType, arrayStructAlloc, "arrload");
    }
    return arrayStructAlloc;
}

llvm::Value *
LLVMCodegen::generateVarAccess(const std::shared_ptr<VarAccess> &varAccess,
                               bool loadValue) {
    auto lookupName = canonicalizeNonexternName(varAccess->name);
    auto *sym = lookupSymbolInScopes(m_scopeStack, lookupName, varAccess->name);
    if (!sym) {
        throw CodeGenError(varAccess, "Unknown variable name: " + varAccess->name);
    }

    llvm::Value *v = sym->value;

    if (llvm::isa<llvm::Function>(v))
        return v;

    if (!loadValue)
        return v;
    // if (auto ar = std::dynamic_pointer_cast<ArrayType>(varAccess->inferred_type)) {
    //     return m_builder.CreateInBoundsGEP(getLLVMType(ar), v, {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0), llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0)}, "arraydecay");
    // }
    return m_builder.CreateLoad(sym->type, v, varAccess->name);
}

llvm::Value *LLVMCodegen::generateEnumAccess(const std::shared_ptr<EnumAccess> &ea, bool loadValue) {
    auto enumType = std::dynamic_pointer_cast<EnumType>(ea->inferred_type);
    if (!enumType) {
        throw CodeGenError(ea, "Enum access on non-enum type: " + ea->inferred_type->str());
    }

    auto valIt = enumType->variant_map.find(ea->variant);
    if (valIt == enumType->variant_map.end()) {
        throw CodeGenError(ea, "Unknown enum variant: " + ea->variant);
    }
    std::shared_ptr<Literal> literal = valIt->second;
    // Assuming enums are represented as i32
    return generateLiteral(literal, loadValue);
}

llvm::Value *LLVMCodegen::generateMethodCall(
    const std::shared_ptr<MethodCall> &methodCall,
    bool /*loadValue*/
) {
    auto structAccess = methodCall->object;
    llvm::Value *obj = generateAddress(
        std::static_pointer_cast<Expression>(structAccess));

    if (!obj) {
        throw CodeGenError(structAccess,
                           "Failed to generate address for method call object");
    }

    auto structType =
        std::dynamic_pointer_cast<StructType>(structAccess->inferred_type);

    if (!structType) {
        auto ptrType =
            std::dynamic_pointer_cast<PointerType>(structAccess->inferred_type);
        if (ptrType) {
            structType =
                std::dynamic_pointer_cast<StructType>(ptrType->base);
            obj = m_builder.CreateLoad(
                llvm::PointerType::getUnqual(context),
                obj,
                "load_ptr_for_method");
        }
    }

    if (!structType) {
        throw CodeGenError(structAccess,
                           "Method call on non-struct type");
    }

    std::string mangledName =
        structType->name + "." + methodCall->method;

    llvm::Function *callee =
        m_llvm_module->getFunction(mangledName);

    if (!callee) {
        throw CodeGenError(methodCall,
                           "Unknown method: " + mangledName);
    }

    std::vector<llvm::Value *> args;

    // If method expects struct by value, load it
    if (callee->getArg(0)->getType()->isStructTy()) {
        obj = m_builder.CreateLoad(
            callee->getArg(0)->getType(),
            obj,
            "load_this");
    }

    args.push_back(obj);

    for (const auto &arg : methodCall->args) {
        args.push_back(generateExpression(arg));
    }

    llvm::CallInst *call =
        m_builder.CreateCall(callee, args, callee->getFunctionType()->getReturnType()->isVoidTy() ? "" : "methodcalltmp");

    return call;
}
llvm::Value *
LLVMCodegen::generateFuncCall(const std::shared_ptr<FuncCall> &funcCall,
                              bool loadValue) {
    std::vector<llvm::Value *> argsV;

    llvm::Value *calleeValue = generateExpression(std::static_pointer_cast<Expression>(funcCall->func));
    if (!calleeValue) {
        throw CodeGenError(funcCall, "Failed to generate callee for function call");
    }

    llvm::FunctionType *funcTy = nullptr;
    bool isExtern = false;
    if (auto ft = std::dynamic_pointer_cast<FunctionType>(funcCall->func->inferred_type)) {
        isExtern = ft->is_extern;
    } else if (auto fpt = std::dynamic_pointer_cast<PointerType>(funcCall->func->inferred_type)) {
        if (auto ft = std::dynamic_pointer_cast<FunctionType>(fpt->base)) {
            isExtern = ft->is_extern;
        }
    } else {
        throw CodeGenError(funcCall, "Unable to determine function type for call: " + funcCall->func->inferred_type->str());
    }

    if (auto *func = llvm::dyn_cast<llvm::Function>(calleeValue)) {
        funcTy = func->getFunctionType();
    } else if (calleeValue->getType()->isPointerTy()) {
        // Indirect call via function pointer
        auto it = funcCall->func->inferred_type;
        if (!IS_INSTANCE(it, PointerType)) {
            throw CodeGenError(funcCall, "Callee is a pointer, but inferred type is not a pointer");
        }
        auto ptrType = std::dynamic_pointer_cast<PointerType>(it);
        if (!IS_INSTANCE(ptrType->base, FunctionType)) {
            throw CodeGenError(funcCall, "Callee is a pointer, but not to a function type");
        }
        auto funcType = std::dynamic_pointer_cast<FunctionType>(ptrType->base);
        funcTy = llvm::cast<llvm::FunctionType>(getLLVMType(funcType, funcCall));
    } else {
        throw CodeGenError(funcCall, "Callee is not a function or function pointer: " +
                                         funcCall->func->str());
    }

    // Generate argument values and apply ABI coercion if needed
    unsigned int argIdx = 0;
    for (const auto &arg : funcCall->args) {
        llvm::Value *argVal = generateExpression(arg);

        // Apply ABI coercion for extern functions
        if (isExtern && argIdx < funcTy->getNumParams()) {
            llvm::Type *paramType = funcTy->getParamType(argIdx);
            llvm::Type *argType = argVal->getType();

            // If the actual param type in the function is different (due to ABI coercion),
            // we need to coerce the argument
            if (argType->isStructTy() && shouldCoerceForABI(argType)) {
                argVal = coerceToABI(argVal, argType);
            }
        }

        argsV.push_back(argVal);
        argIdx++;
    }

    // Apply default argument promotions for varargs
    if (funcTy->isVarArg()) {
        unsigned numFixedArgs = funcTy->getNumParams();
        for (size_t i = numFixedArgs; i < argsV.size(); ++i) {
            llvm::Value *arg = argsV[i];
            if (!arg) {
                throw CodeGenError(funcCall, "Null argument value for vararg " + std::to_string(i));
            }
            llvm::Type *ty = arg->getType();
            if (ty->isIntegerTy(1) || ty->isIntegerTy(8) || ty->isIntegerTy(16)) {
                argsV[i] = m_builder.CreateZExt(arg, llvm::Type::getInt32Ty(context), "vapromotetmp");
            } else if (ty->isFloatTy()) {
                argsV[i] = m_builder.CreateFPExt(arg, llvm::Type::getDoubleTy(context), "vapromotetmp");
            }
        }
    }

    llvm::Value *result = m_builder.CreateCall(funcTy, calleeValue, argsV,
                                               funcTy->getReturnType() != llvm::Type::getVoidTy(context) ? "calltmp" : "");

    // If calling an extern function that returns a coerced struct, convert it back
    if (isExtern && result && shouldCoerceForABI(funcTy->getReturnType())) {
        if (funcCall->inferred_type && IS_INSTANCE(funcCall->inferred_type, StructType)) {
            llvm::Type *structType = getLLVMType(funcCall->inferred_type, funcCall);
            if (structType->isStructTy()) {
                result = coerceFromABI(result, structType);
            }
        }
    }

    return result;
}
void LLVMCodegen::conditionOrPanic(llvm::Value *condition, RuntimePanicType panicType, int line, int col) {

    llvm::Function *curFunc = m_builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context, "good_check", curFunc);
    llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(context, "bad_check", curFunc);
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "post_check", curFunc);

    m_builder.CreateCondBr(condition, thenBB, elseBB);

    m_builder.SetInsertPoint(thenBB);
    m_builder.CreateBr(mergeBB);
    m_builder.SetInsertPoint(elseBB);

    llvm::Function *panicFunc = m_llvm_module->getFunction(RUNTIME_PANIC_FUNC_NAME);
    llvm::FunctionType *panicFuncTy = panicFunc->getFunctionType();

    std::vector<llvm::Value *> panicArgs = {
        m_runtime_panic_strings[panicType],
        llvm::ConstantInt::get(context, llvm::APInt(32, line)),
        llvm::ConstantInt::get(context, llvm::APInt(32, col)),
    };
    m_builder.CreateCall(panicFuncTy, panicFunc, panicArgs);

    m_builder.CreateBr(mergeBB);
    m_builder.SetInsertPoint(mergeBB);
}
llvm::Value *LLVMCodegen::generateArrayAccess(
    const std::shared_ptr<OffsetAccess> &arrayAccess, bool loadValue) {
    if (!arrayAccess || !arrayAccess->base || !arrayAccess->index) {
        throw CodeGenError(nullptr, "Invalid array access: missing base or index expression");
    }
    llvm::Value *basePtr = generateAddress(std::static_pointer_cast<Expression>(arrayAccess->base));
    if (!basePtr) {
        throw CodeGenError(arrayAccess, "Failed to generate base address for array access");
    }
    llvm::Value *index = generateExpression(arrayAccess->index);
    if (!index) {
        throw CodeGenError(arrayAccess->index, "Failed to generate index for array access");
    }
    if (!arrayAccess->base->inferred_type ||
        arrayAccess->base->inferred_type->kind() != TypeKind::Array) {
        throw CodeGenError(arrayAccess->base, "Base expression is not of array type: " +
                                                  (arrayAccess->base->inferred_type ? arrayAccess->base->inferred_type->str() : "null"));
    }
    llvm::Type *elementType = getLLVMType(
        std::dynamic_pointer_cast<ArrayType>(arrayAccess->base->inferred_type)->element_type, arrayAccess);
    if (!elementType) {
        throw CodeGenError(arrayAccess, "Unknown element type for array access: " +
                                            arrayAccess->base->inferred_type->str());
    }
    // Condition check for index within bounds
    if (index->getType() != llvm::Type::getInt64Ty(context)) {
        index = m_builder.CreateZExt(index, llvm::Type::getInt64Ty(context), "index_to_i64");
    }
    llvm::Value *arrLoad = m_builder.CreateLoad(
        getLLVMType(arrayAccess->base->inferred_type, arrayAccess->base), basePtr);
    if (m_options.opt_level == Debug && m_options.do_runtime_safety) {
        llvm::Value *arraySizeVal = m_builder.CreateExtractValue(
            arrLoad,
            {1}, "array_size");

        // Zext index to i64 if needed
        if (index->getType() != llvm::Type::getInt64Ty(context)) {
            index = m_builder.CreateZExt(index, llvm::Type::getInt64Ty(context), "index_to_i64");
        }
        llvm::Value *indexInBounds = m_builder.CreateICmpULT(
            index, arraySizeVal, "index_in_bounds");
        conditionOrPanic(indexInBounds, OutOfBounds, arrayAccess->line, arrayAccess->col);
    }

    llvm::Value *dataPtr = m_builder.CreateExtractValue(
        arrLoad,
        {0}, "array_data_ptr");

    llvm::Value *gep = m_builder.CreateGEP(
        elementType, dataPtr, index, "array_elem_ptr");

    if (loadValue)
        return m_builder.CreateLoad(elementType, gep, "load_array_elem");
    else
        return gep;
}

llvm::Value *LLVMCodegen::generateOffsetAccess(
    const std::shared_ptr<OffsetAccess> &offsetAccess, bool loadValue) {

    if (!offsetAccess || !offsetAccess->base || !offsetAccess->index) {
        throw CodeGenError(nullptr, "Invalid offset access: missing base or index expression");
    }
    if (offsetAccess->base->inferred_type->kind() == TypeKind::Array) {
        return generateArrayAccess(offsetAccess, loadValue);
    }

    llvm::Value *basePtr = generateExpression(
        std::static_pointer_cast<Expression>(offsetAccess->base));
    if (!basePtr) {
        throw CodeGenError(offsetAccess, "Failed to generate base for offset access");
    }
    llvm::Value *index = generateExpression(offsetAccess->index);
    if (!index) {
        throw CodeGenError(offsetAccess->index, "Failed to generate index for offset access");
    }
    llvm::Type *resultType = getLLVMType(offsetAccess->inferred_type, offsetAccess);
    if (!resultType) {
        throw CodeGenError(offsetAccess, "Unknown type for offset access: " +
                                             offsetAccess->inferred_type->str());
    }
    llvm::Value *gep =
        m_builder.CreateGEP(resultType, basePtr, index, "offset_access");

    if (loadValue)
        return m_builder.CreateLoad(resultType, gep, "load_offset");
    else
        return gep;
}

llvm::Value *LLVMCodegen::generateArrayFieldAccess(const std::shared_ptr<FieldAccess> &fieldAccess, bool loadValue) {
    bool baseIsAddressable = isLValue(fieldAccess->base);

    llvm::Value *basePtr = nullptr;

    if (baseIsAddressable) {
        basePtr = generateAddress(fieldAccess->base);
    } else {
        if (!loadValue) {
            throw CodeGenError(fieldAccess,
                               "Cannot assign to field of temporary value");
        }

        llvm::Value *baseVal = generateExpression(fieldAccess->base);
        llvm::StructType *structType = llvm::cast<llvm::StructType>(getLLVMType(fieldAccess->base->inferred_type, fieldAccess->base));
        basePtr = materializeAggregate(baseVal, structType);
    }
    if (!basePtr) {
        throw CodeGenError(fieldAccess->base, "Failed to generate address for array field base");
    }
    auto arr_type = std::dynamic_pointer_cast<ArrayType>(fieldAccess->base->inferred_type);
    if (!arr_type) {
        throw CodeGenError(fieldAccess->base, "Field access on non-array type: " + fieldAccess->base->inferred_type->str());
    }
    llvm::StructType *arrStructType =
        llvm::cast<llvm::StructType>(getLLVMType(arr_type, fieldAccess->base));

    if (fieldAccess->field == "len") {
        // Return index 1
        llvm::Type *length_type = llvm::Type::getInt64Ty(context);
        auto gep = m_builder.CreateStructGEP(arrStructType, basePtr, 1, "len_access");
        return loadValue
                   ? m_builder.CreateLoad(length_type, gep, "load_len")
                   : gep;

    } else if (fieldAccess->field == "ptr") {
        // Return index 0
        llvm::Type *data_ptr_type = llvm::PointerType::getUnqual(context);
        auto gep = m_builder.CreateStructGEP(arrStructType, basePtr, 0, "ptr_access");
        return loadValue
                   ? m_builder.CreateLoad(data_ptr_type, gep, "load_ptr")
                   : gep;

    } else {
        throw CodeGenError(fieldAccess, "Unknown field on array: " + fieldAccess->field);
    }
}

llvm::Value *LLVMCodegen::generateErrorUnionFieldAccess(const std::shared_ptr<FieldAccess> &fieldAccess, bool loadValue) {
    bool baseIsAddressable = isLValue(fieldAccess->base);

    llvm::Value *basePtr = nullptr;

    if (baseIsAddressable) {
        basePtr = generateAddress(fieldAccess->base);
    } else {
        if (!loadValue) {
            throw CodeGenError(fieldAccess,
                               "Cannot assign to field of temporary value");
        }
        llvm::Value *baseVal = generateExpression(fieldAccess->base);
        llvm::StructType *structType = llvm::cast<llvm::StructType>(getLLVMType(fieldAccess->base->inferred_type, fieldAccess->base));
        basePtr = materializeAggregate(baseVal, structType);
    }
    if (!basePtr) {
        throw CodeGenError(fieldAccess->base, "Failed to generate address for error union field base");
    }
    assert(basePtr->getType()->isPointerTy());

    auto eu_type = std::dynamic_pointer_cast<ErrorUnionType>(fieldAccess->base->inferred_type);
    if (!eu_type) {
        throw CodeGenError(fieldAccess->base, "Field access on non-error-union type: " + fieldAccess->base->inferred_type->str());
    }
    llvm::StructType *euStructType =
        llvm::cast<llvm::StructType>(getLLVMType(eu_type, fieldAccess->base));
    auto layout = getErrorUnionLayout(eu_type);

    if (fieldAccess->field == "ok") {
        if (layout.okIsVoid) {
            if (!loadValue) {
                throw CodeGenError(fieldAccess, "Cannot take address of void ok field");
            }
            return nullptr;
        }
        llvm::Type *ok_type = getLLVMType(eu_type->valueType, fieldAccess);
        return createStructFieldAccess(euStructType, basePtr, layout.okIndex, ok_type, "ok", loadValue);

    } else if (fieldAccess->field == "err") {
        llvm::Type *err_type = getLLVMType(eu_type->errorType, fieldAccess);
        return createStructFieldAccess(euStructType, basePtr, layout.errIndex, err_type, "err", loadValue);

    } else if (fieldAccess->field == "is_err") {
        llvm::Type *bool_type = llvm::Type::getInt1Ty(context);
        return createStructFieldAccess(euStructType, basePtr, layout.isErrIndex, bool_type, "is_err", loadValue);
    } else {
        throw CodeGenError(fieldAccess, "Unknown field on error union: " + fieldAccess->field);
    }
}

bool LLVMCodegen::isLValue(const std::shared_ptr<Expression> &expr) {
    if (!expr)
        return false;

    if (IS_INSTANCE(expr, VarAccess)) {
        return true;
    }
    if (IS_INSTANCE(expr, FieldAccess)) {
        auto fa = std::dynamic_pointer_cast<FieldAccess>(expr);
        return isLValue(fa->base);
    }
    if (IS_INSTANCE(expr, OffsetAccess)) {
        auto oa = std::dynamic_pointer_cast<OffsetAccess>(expr);
        return isLValue(oa->base);
    }
    if (IS_INSTANCE(expr, Dereference)) {
        return true;
    }
    if (IS_INSTANCE(expr, TypeCast)) {
        auto tc = std::dynamic_pointer_cast<TypeCast>(expr);
        return isLValue(tc->expr);
    }
    if (IS_INSTANCE(expr, BinaryOperation)) {
        auto bin = std::dynamic_pointer_cast<BinaryOperation>(expr);
        return bin->op == "=" && isLValue(bin->left);
    }
    if (IS_INSTANCE(expr, MethodCall)) {
        // Only a pointer-returning method can be an lvalue
        auto mc = std::dynamic_pointer_cast<MethodCall>(expr);
        return mc->inferred_type && mc->inferred_type->kind() == TypeKind::Pointer;
    }
    if (IS_INSTANCE(expr, FuncCall)) {
        // Only a pointer-returning function can be an lvalue
        auto fc = std::dynamic_pointer_cast<FuncCall>(expr);
        return fc->inferred_type && fc->inferred_type->kind() == TypeKind::Pointer;
    }

    // Literals, computations, temporaries, etc. are never lvalues
    return false;
}

llvm::Value *LLVMCodegen::generateFieldAccess(
    const std::shared_ptr<FieldAccess> &fieldAccess, bool loadValue) {

    if (!fieldAccess || !fieldAccess->base) {
        throw CodeGenError(nullptr, "Invalid field access: missing base expression");
    }

    if (!fieldAccess->base->inferred_type) {
        throw CodeGenError(fieldAccess->base, "Base expression of field access has no inferred type");
    }

    if (fieldAccess->base->inferred_type->kind() == TypeKind::Array) {
        return generateArrayFieldAccess(fieldAccess, loadValue);
    } else if (fieldAccess->base->inferred_type->kind() == TypeKind::ErrorUnion) {
        return generateErrorUnionFieldAccess(fieldAccess, loadValue);
    }

    bool baseIsAddressable = isLValue(fieldAccess->base);

    llvm::Value *basePtr = nullptr;

    if (baseIsAddressable) {
        basePtr = generateAddress(fieldAccess->base);
    } else {
        if (!loadValue) {
            throw CodeGenError(fieldAccess,
                               "Cannot assign to field of temporary value");
        }

        llvm::Value *baseVal = generateExpression(fieldAccess->base);
        llvm::StructType *structType = llvm::cast<llvm::StructType>(getLLVMType(fieldAccess->base->inferred_type, fieldAccess->base));
        basePtr = materializeAggregate(baseVal, structType);
    }
    if (!basePtr) {
        throw CodeGenError(fieldAccess->base, "Failed to generate address for field base");
    }

    // Check if base type is a struct or pointer-to-struct
    std::shared_ptr<StructType> structType =
        std::dynamic_pointer_cast<StructType>(fieldAccess->base->inferred_type);
    std::shared_ptr<UnionType> unionType =
        std::dynamic_pointer_cast<UnionType>(fieldAccess->base->inferred_type);

    if (!structType && !unionType) {
        auto ptrType =
            std::dynamic_pointer_cast<PointerType>(fieldAccess->base->inferred_type);
        if (ptrType) {
            structType = std::dynamic_pointer_cast<StructType>(ptrType->base);
            unionType = std::dynamic_pointer_cast<UnionType>(ptrType->base);

            if (!ptrType->base) {
                throw CodeGenError(fieldAccess->base,
                                   "Pointer type in field access has no base type");
            }

            if (structType || unionType) {
                // Adjust basePtr to dereference the pointer
                basePtr = m_builder.CreateLoad(
                    llvm::PointerType::getUnqual(context), basePtr,
                    "load_ptr_for_field");
                if (!basePtr) {
                    throw CodeGenError(fieldAccess->base,
                                       "Failed to load pointer value for field access");
                }
            }
        }

        if (!structType && !unionType) {
            throw CodeGenError(fieldAccess->base,
                               "Field access on non-struct/union type: " +
                                   fieldAccess->base->inferred_type->str());
        }
    }

    // Handle union field access
    if (unionType) {
        auto accessee = m_unionTypes.find(unionType->name);
        if (accessee == m_unionTypes.end() || !accessee->second) {
            throw CodeGenError(fieldAccess->base,
                               "Unknown or unregistered union type in field access: " +
                                   unionType->name);
        }

        llvm::StructType *llvmUnion = accessee->second;

        // Validate field existence
        const auto &fieldName = fieldAccess->field;
        auto fieldType = unionType->getFieldType(fieldName);
        if (!fieldType) {
            throw CodeGenError(fieldAccess->base,
                               "Union '" + unionType->name +
                                   "' has no field named '" + fieldName + "'");
        }

        // For unions, all fields are at offset 0 in the byte array (field 0)
        // Get pointer to the storage (byte array)
        llvm::Value *storagePtr = m_builder.CreateGEP(
            llvmUnion, basePtr,
            {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0),
             llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0)},
            "union_storage_ptr");

        // Cast the byte array pointer to the desired field type
        llvm::Type *targetType = getLLVMType(fieldType, fieldAccess);
        llvm::Value *fieldPtr = m_builder.CreateBitCast(
            storagePtr,
            llvm::PointerType::getUnqual(context),
            "union_field_cast");

        if (loadValue) {
            return m_builder.CreateLoad(targetType, fieldPtr, fieldName + "_load");
        } else {
            return fieldPtr;
        }
    }

    // Confirm struct type is registered in LLVM mapping
    auto accessee = m_structTypes.find(structType->name);
    if (accessee == m_structTypes.end() || !accessee->second) {
        throw CodeGenError(fieldAccess->base,
                           "Unknown or unregistered struct type in field access: " +
                               structType->name);
    }

    llvm::StructType *llvmStruct = accessee->second;

    // Validate field existence
    const auto &fieldName = fieldAccess->field;
    int fieldIndex = structType->getFieldIndex(fieldName);
    if (fieldIndex < 0) {
        throw CodeGenError(fieldAccess->base,
                           "Struct '" + structType->name +
                               "' has no field named '" + fieldName + "'");
    }

    if (!fieldAccess->inferred_type) {
        throw CodeGenError(fieldAccess, "Field '" + fieldName +
                                            "' has no inferred type for load");
    }
    return createStructFieldAccess(llvmStruct, basePtr, static_cast<unsigned>(fieldIndex),
                                   getLLVMType(fieldAccess->inferred_type, fieldAccess),
                                   fieldName, loadValue);
}

llvm::Value *LLVMCodegen::generateModuleAccess(const std::shared_ptr<ModuleAccess> &moduleAccess,
                                               bool loadValue) {

    auto mod = m_current_module->getImportedModule(moduleAccess->module_path);
    auto var_name = moduleAccess->member_name;
    // make sure full name isn't an extern name
    auto full_name = var_name;
    if (mod->externLinkage.find(var_name) == mod->externLinkage.end()) {
        full_name = mod->canonicalizeName(var_name);
    }

    auto *sym = lookupSymbolInScopes(m_scopeStack, full_name, var_name);
    if (!sym) {
        throw CodeGenError(moduleAccess, "Unknown variable name: " + full_name);
    }
    llvm::Value *v = sym->value;
    if (llvm::isa<llvm::Function>(v))
        return v;
    if (!loadValue)
        return v;
    return m_builder.CreateLoad(sym->type, v, var_name);
}

llvm::Value *LLVMCodegen::generateUnionInitializer(std::shared_ptr<StructInitializer> unionInit, bool loadValue) {
    auto _if_type = unionInit->inferred_type;
    auto union_type = std::dynamic_pointer_cast<UnionType>(_if_type);
    if (!union_type) {
        throw CodeGenError(unionInit, "Union initializer has non-union inferred type: " +
                                          (_if_type ? _if_type->str() : "null"));
    }

    // Handle union initialization
    auto it = m_unionTypes.find(union_type->name);
    if (it == m_unionTypes.end()) {
        throw CodeGenError(unionInit, "Unknown union type: " + union_type->name);
    }
    llvm::StructType *llvmUnion = it->second;
    llvm::Value *alloca = nullptr;
    if (loadValue) {
        llvm::Function *currentFunction = m_builder.GetInsertBlock()
                                              ? m_builder.GetInsertBlock()->getParent()
                                              : nullptr;
        alloca = createEntryBlockAlloca(currentFunction, llvmUnion, "uniontmp");
    }
    if (!alloca) {
        alloca = m_builder.CreateAlloca(llvmUnion, nullptr, "uniontmp");
    }

    // Unions can only initialize one field at a time
    if (unionInit->field_values.size() != 1) {
        throw CodeGenError(unionInit, "Union initializer must have exactly one field, got " +
                                          std::to_string(unionInit->field_values.size()));
    }

    const auto &field = *unionInit->field_values.begin();
    auto fieldType = union_type->getFieldType(field.first);
    if (!fieldType) {
        throw CodeGenError(unionInit, "Union '" + union_type->name +
                                          "' has no field named '" + field.first + "'");
    }

    // Get pointer to the storage (byte array at field 0)
    llvm::Value *storagePtr = m_builder.CreateGEP(
        llvmUnion, alloca,
        {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0),
         llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0)},
        "union_storage_ptr");

    // Cast to the field type pointer
    llvm::Type *targetType = getLLVMType(fieldType, unionInit);
    llvm::Value *fieldPtr = m_builder.CreateBitCast(
        storagePtr,
        llvm::PointerType::getUnqual(context),
        "union_field_ptr");

    // Store the value
    llvm::Value *fieldVal = generateExpression(field.second);
    m_builder.CreateStore(fieldVal, fieldPtr);

    if (!loadValue)
        return alloca;
    return m_builder.CreateLoad(llvmUnion, alloca, "loadunion");
}

llvm::Value *LLVMCodegen::generateStructInitializer(
    const std::shared_ptr<StructInitializer> &structInit, bool loadValue) {
    auto _if_type = structInit->inferred_type;
    auto if_type = std::dynamic_pointer_cast<StructType>(_if_type);
    auto union_type = std::dynamic_pointer_cast<UnionType>(_if_type);

    if (union_type) {
        // If it's a union initializer, delegate to the union initializer generator
        return generateUnionInitializer(structInit, loadValue);
    }

    if (!if_type) {
        throw CodeGenError(structInit, "Struct initializer has non-struct/union inferred type: " +
                                           (_if_type ? _if_type->str() : "null"));
    }
    auto it = m_structTypes.find(if_type->name);
    if (it == m_structTypes.end()) {
        throw CodeGenError(structInit, "Unknown struct type: " + if_type->name);
    }
    llvm::StructType *llvmStruct = it->second;
    if (loadValue) {
        llvm::Value *agg = llvm::UndefValue::get(llvmStruct);
        for (const auto &field : structInit->field_values) {
            auto fieldIndex = if_type->getFieldIndex(field.first);
            if (fieldIndex < 0) {
                throw CodeGenError(structInit, "Struct '" + if_type->name +
                                                   "' has no field named '" + field.first + "'");
            }
            llvm::Value *fieldVal = generateExpression(field.second);
            agg = m_builder.CreateInsertValue(agg, fieldVal, fieldIndex,
                                              field.first + "_insert");
        }
        return agg;
    }

    llvm::Value *alloca =
        m_builder.CreateAlloca(llvmStruct, nullptr, "structtmp");
    for (const auto &field : structInit->field_values) {
        // Find field index
        auto fieldIndex = if_type->getFieldIndex(field.first);
        if (fieldIndex < 0) {
            throw CodeGenError(structInit, "Struct '" + if_type->name +
                                               "' has no field named '" + field.first + "'");
        }
        // Generate GEP for field
        llvm::Value *fieldPtr = m_builder.CreateGEP(
            llvmStruct, alloca,
            {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0),
             llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), fieldIndex)},
            field.first + "_ptr");
        llvm::Value *fieldVal = generateExpression(field.second);
        m_builder.CreateStore(fieldVal, fieldPtr);
    }
    return alloca;
}

llvm::Value *
LLVMCodegen::generateBlock(const std::shared_ptr<Block> &blockNode) {
    auto parent_scope = !m_scopeStack.empty() ? std::make_shared<Scope>(CUR_SCOPE)
                                              : std::make_shared<Scope>(nullptr);
    m_scopeStack.push_back(*parent_scope);
    llvm::Value *lastValue = nullptr;
    for (const auto &stmt : blockNode->statements) {
        if (std::dynamic_pointer_cast<ReturnStatement>(stmt)) {
            // If it's a return statement, we generate it and then break out of the block
            lastValue = generateStatement(stmt);
            break;
        }
        lastValue = generateStatement(stmt);
    }
    m_scopeStack.pop_back();
    return lastValue;
}

llvm::Value *
LLVMCodegen::generateIfStatement(const std::shared_ptr<IfStatement> &ifStmt) {

    // Condition generation
    llvm::Value *condV = generateExpression(ifStmt->condition);
    condV = m_builder.CreateICmpNE(
        condV, llvm::ConstantInt::get(condV->getType(), 0), "ifcond");
    llvm::Function *theFunction = m_builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *thenBB =
        llvm::BasicBlock::Create(context, "then", theFunction);
    llvm::BasicBlock *elseBB =
        llvm::BasicBlock::Create(context, "else", theFunction);
    llvm::BasicBlock *mergeBB =
        llvm::BasicBlock::Create(context, "ifcont", theFunction);
    m_builder.CreateCondBr(condV, thenBB, elseBB);
    m_builder.SetInsertPoint(thenBB);
    llvm::Value *thenV = generateStatement(ifStmt->then_branch);
    // Only branch to merge if the then block is not already terminated
    if (!m_builder.GetInsertBlock()->getTerminator()) {
        m_builder.CreateBr(mergeBB);
    }
    thenBB = m_builder.GetInsertBlock();
    m_builder.SetInsertPoint(elseBB);
    if (ifStmt->else_branch)
        llvm::Value *elseV = generateStatement(ifStmt->else_branch);
    // Only branch to merge if the else block is not already terminated
    if (!m_builder.GetInsertBlock()->getTerminator()) {
        m_builder.CreateBr(mergeBB);
    }
    m_builder.SetInsertPoint(mergeBB);
    return nullptr;
}

llvm::Value *LLVMCodegen::generateWhileStatement(
    const std::shared_ptr<WhileStatement> &whileStmt) {
    emitLoop(
        nullptr,
        [&]() -> llvm::Value * { return generateExpression(whileStmt->condition); },
        [&]() { generateStatement(whileStmt->body); },
        nullptr,
        false,
        "while");
    return nullptr;
}

llvm::Value *LLVMCodegen::generateForStatement(
    const std::shared_ptr<ForStatement> &forStmt) {
    emitLoop(
        [&]() {
            if (forStmt->init)
                generateStatement(forStmt->init);
        },
        [&]() -> llvm::Value * {
            if (!forStmt->condition)
                return nullptr;
            return generateExpression(forStmt->condition);
        },
        [&]() { generateStatement(forStmt->body); },
        [&]() {
            if (forStmt->increment)
                generateStatement(forStmt->increment);
        },
        true,
        "for");
    return nullptr;
}

llvm::Value *LLVMCodegen::generateReturnStatement(
    const std::shared_ptr<ReturnStatement> &retStmt) {
    if (retStmt->value == nullptr) {
        if (m_error_union_return_type != nullptr) {
            bool okIsVoid = IS_INSTANCE(m_error_union_return_type->valueType, Void);
            if (!okIsVoid) {
                throw CodeGenError(retStmt, "Return type mismatch: expected value");
            }
            if (retStmt->is_error) {
                throw CodeGenError(retStmt, "Error return requires a value");
            }
            llvm::Value *okReturn = buildErrorUnionValue(m_error_union_return_type, nullptr, nullptr, false, retStmt);
            auto retInstr = m_builder.CreateRet(okReturn);
            return retInstr;
        }
        auto retInstr = m_builder.CreateRetVoid();
        return retInstr;
    }
    auto retVal = generateExpression(retStmt->value);
    if (m_error_union_return_type != nullptr) {
        bool okIsVoid = IS_INSTANCE(m_error_union_return_type->valueType, Void);
        if (okIsVoid && !retStmt->is_error) {
            throw CodeGenError(retStmt, "Return type mismatch: expected void ok");
        }
        if (retStmt->is_error) {
            retVal = buildErrorUnionValue(m_error_union_return_type, nullptr, retVal, true, retStmt);
        } else {
            retVal = buildErrorUnionValue(m_error_union_return_type, retVal, nullptr, false, retStmt);
        }
    }

    auto retInstr = m_builder.CreateRet(retVal);
    return retInstr;
}

llvm::Value *LLVMCodegen::generateExpressionStatement(
    const std::shared_ptr<ExpressionStatement> &exprStmt) {
    return generateExpression(exprStmt->expression);
}

llvm::Value *LLVMCodegen::buildErrorUnionValue(const std::shared_ptr<ErrorUnionType> &euType,
                                               llvm::Value *okVal,
                                               llvm::Value *errVal,
                                               bool isErr,
                                               const ASTNodePtr &node) {
    if (!euType) {
        throw CodeGenError(node, "Error union type is null");
    }
    bool okIsVoid = IS_INSTANCE(euType->valueType, Void);
    llvm::StructType *errUnionStruct = llvm::cast<llvm::StructType>(getLLVMType(euType, node));
    llvm::Value *zeroVal = llvm::Constant::getNullValue(errUnionStruct);

    if (okIsVoid) {
        if (!isErr && okVal != nullptr) {
            throw CodeGenError(node, "Void ok error union cannot carry a value");
        }
        if (isErr && errVal == nullptr) {
            throw CodeGenError(node, "Error return requires a value");
        }
        if (!errVal) {
            errVal = llvm::Constant::getNullValue(getLLVMType(euType->errorType, node));
        }
        auto withErr = m_builder.CreateInsertValue(zeroVal, errVal, 0, "insert_err");
        return m_builder.CreateInsertValue(withErr,
                                           llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), isErr ? 1 : 0),
                                           1, "insert_is_err");
    }

    if (isErr && errVal == nullptr) {
        throw CodeGenError(node, "Error return requires a value");
    }
    if (!okVal) {
        okVal = llvm::Constant::getNullValue(getLLVMType(euType->valueType, node));
    }
    if (!errVal) {
        errVal = llvm::Constant::getNullValue(getLLVMType(euType->errorType, node));
    }

    auto withOk = m_builder.CreateInsertValue(zeroVal, okVal, 0, "insert_ok");
    auto withErr = m_builder.CreateInsertValue(withOk, errVal, 1, "insert_err");
    return m_builder.CreateInsertValue(withErr,
                                       llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), isErr ? 1 : 0),
                                       2, "insert_is_err");
}

void LLVMCodegen::emitDefaultReturn(const std::shared_ptr<FunctionDeclaration> &funcDecl) {
    if (IS_INSTANCE(funcDecl->type->ret, Void)) {
        m_builder.CreateRetVoid();
        return;
    }
    if (IS_INSTANCE(funcDecl->type->ret, ErrorUnionType)) {
        auto expected_error = std::dynamic_pointer_cast<ErrorUnionType>(funcDecl->type->ret);
        if (expected_error && IS_INSTANCE(expected_error->valueType, Void)) {
            llvm::Value *okReturn = buildErrorUnionValue(expected_error, nullptr, nullptr, false, funcDecl);
            m_builder.CreateRet(okReturn);
            return;
        }
    }
    throw CodeGenError(funcDecl, "Non-void function missing return: " + funcDecl->name);
}

LLVMCodegen::ErrorUnionLayout LLVMCodegen::getErrorUnionLayout(
    const std::shared_ptr<ErrorUnionType> &euType) const {
    ErrorUnionLayout layout{};
    layout.okIsVoid = IS_INSTANCE(euType->valueType, Void);
    if (layout.okIsVoid) {
        layout.okIndex = 0;
        layout.errIndex = 0;
        layout.isErrIndex = 1;
    } else {
        layout.okIndex = 0;
        layout.errIndex = 1;
        layout.isErrIndex = 2;
    }
    return layout;
}

llvm::Value *LLVMCodegen::createStructFieldAccess(llvm::StructType *structType,
                                                  llvm::Value *basePtr,
                                                  unsigned fieldIndex,
                                                  llvm::Type *fieldType,
                                                  const std::string &name,
                                                  bool loadValue) {
    llvm::Value *gep = m_builder.CreateGEP(
        structType, basePtr,
        {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0),
         llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), fieldIndex)},
        name + "_ptr");

    if (!gep) {
        throw CodeGenError(nullptr, "Failed to create GEP for field '" + name + "'");
    }

    if (loadValue) {
        return m_builder.CreateLoad(fieldType, gep, "load_" + name);
    }
    return gep;
}

void LLVMCodegen::emitLoop(const std::function<void()> &emitInit,
                           const std::function<llvm::Value *()> &emitCondition,
                           const std::function<void()> &emitBody,
                           const std::function<void()> &emitIncrement,
                           bool hasIncrement,
                           const std::string &labelPrefix) {
    llvm::Function *theFunction = m_builder.GetInsertBlock()->getParent();
    if (emitInit) {
        emitInit();
    }

    auto cond_block =
        llvm::BasicBlock::Create(context, labelPrefix + ".cond", theFunction);
    auto body_block =
        llvm::BasicBlock::Create(context, labelPrefix + ".body", theFunction);
    auto inc_block =
        llvm::BasicBlock::Create(context, labelPrefix + ".inc", theFunction);
    auto after_block =
        llvm::BasicBlock::Create(context, labelPrefix + ".end", theFunction);

    m_loop_stack.push_back(LoopInfo{
        .breakBB = after_block,
        .continueBB = hasIncrement ? inc_block : cond_block,
    });

    m_builder.CreateBr(cond_block);
    m_builder.SetInsertPoint(cond_block);

    llvm::Value *condition = nullptr;
    if (emitCondition) {
        condition = emitCondition();
    }
    if (condition) {
        condition = m_builder.CreateICmpNE(
            condition, llvm::ConstantInt::get(condition->getType(), 0), labelPrefix + "cond");
    } else {
        condition = llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), 1);
    }

    m_builder.CreateCondBr(condition, body_block, after_block);
    m_builder.SetInsertPoint(body_block);
    if (emitBody) {
        emitBody();
    }
    if (!m_builder.GetInsertBlock()->getTerminator()) {
        m_builder.CreateBr(hasIncrement ? inc_block : cond_block);
    }

    m_builder.SetInsertPoint(inc_block);
    if (hasIncrement && emitIncrement) {
        emitIncrement();
    }
    m_builder.CreateBr(cond_block);

    m_builder.SetInsertPoint(after_block);
    m_loop_stack.pop_back();
}

llvm::Value *LLVMCodegen::materializeAggregate(llvm::Value *abiValue, llvm::StructType *langType) {
    assert(abiValue);
    assert(langType);

    llvm::Type *abiTy = abiValue->getType();

    // If abiValue is already a pointer, dereference to get the struct type
    if (abiTy->isPointerTy()) {
        // abiValue is already an address, return it directly
        return abiValue;
    }

    assert(abiTy->isStructTy());

    llvm::IRBuilder<> &B = m_builder;
    llvm::LLVMContext &C = context;

    // Allocate ABI-shaped temporary
    llvm::AllocaInst *abiTmp =
        B.CreateAlloca(abiTy, nullptr, "abi.tmp");

    // ABI alignment
    llvm::DataLayout DL = m_llvm_module->getDataLayout();
    unsigned abiAlign = DL.getABITypeAlign(abiTy).value();
    abiTmp->setAlignment(llvm::Align(abiAlign));
    // Store ABI value
    B.CreateStore(abiValue, abiTmp);

    // Allocate language-level struct
    llvm::AllocaInst *langTmp =
        B.CreateAlloca(langType, nullptr, "agg.tmp");

    unsigned langAlign = DL.getABITypeAlign(langType).value();
    langTmp->setAlignment(llvm::Align(langAlign));

    // Compute copy size (language layout!)
    uint64_t copySize = DL.getTypeAllocSize(langType);

    // memcpy ABI → language object
    B.CreateMemCpy(
        langTmp,
        llvm::Align(langAlign),
        abiTmp,
        llvm::Align(abiAlign),
        copySize);

    // Return pointer to materialized object
    return langTmp;
}

// ABI coercion helper functions
bool LLVMCodegen::shouldCoerceForABI(llvm::Type *type) {
    // Only coerce structs for extern functions
    if (!type->isStructTy()) {
        return false;
    }

    llvm::StructType *structType = llvm::cast<llvm::StructType>(type);
    llvm::DataLayout DL = m_llvm_module->getDataLayout();
    uint64_t size = DL.getTypeAllocSize(structType);

    // Coerce structs that are 1, 2, 4, 8, or 16 bytes
    // (following System V AMD64 ABI rules)
    return size == 1 || size == 2 || size == 4 || size == 8 || size == 16;
}

llvm::Type *LLVMCodegen::getABICoercionType(llvm::Type *structType) {
    if (!structType->isStructTy()) {
        return structType;
    }

    llvm::DataLayout DL = m_llvm_module->getDataLayout();
    uint64_t size = DL.getTypeAllocSize(structType);

    // Map struct size to appropriate integer type
    switch (size) {
        case 1:
            return llvm::Type::getInt8Ty(context);
        case 2:
            return llvm::Type::getInt16Ty(context);
        case 4:
            return llvm::Type::getInt32Ty(context);
        case 8:
            return llvm::Type::getInt64Ty(context);
        case 16:
            // Use <2 x i64> for 16-byte structs
            return llvm::VectorType::get(llvm::Type::getInt64Ty(context), 2, false);
        default:
            return structType;
    }
}

llvm::Value *LLVMCodegen::coerceToABI(llvm::Value *structValue, llvm::Type *structType) {
    if (!shouldCoerceForABI(structType)) {
        return structValue;
    }

    llvm::Type *abiType = getABICoercionType(structType);
    // If the structValue originates from a load of a global variable,
    // avoid materializing a temporary and instead load the ABI-shaped
    // value directly from the global by bitcasting the global pointer.
    if (auto *LI = llvm::dyn_cast<llvm::LoadInst>(structValue)) {
        llvm::Value *ptrOp = LI->getPointerOperand();
        if (ptrOp) {
            if (auto *GV = llvm::dyn_cast<llvm::GlobalVariable>(ptrOp->stripPointerCasts())) {
                llvm::PointerType *abiPtrTy = llvm::PointerType::getUnqual(context);
                llvm::Value *abiPtr = m_builder.CreateBitCast(ptrOp, abiPtrTy, "abi.ptr");
                return m_builder.CreateLoad(abiType, abiPtr, "abi.val");
            }
        }
    }

    // If it's a constant aggregate (e.g., global constant initializer),
    // try to produce a constant ABI value via bitcast of the constant.
    if (auto *C = llvm::dyn_cast<llvm::Constant>(structValue)) {
        if (auto *CE = llvm::ConstantExpr::getBitCast(C, abiType)) {
            return CE;
        }
    }

    // Fallback: allocate temporary for struct and perform the usual ABI coercion.
    llvm::AllocaInst *structAlloca = m_builder.CreateAlloca(structType, nullptr, "struct.tmp");
    m_builder.CreateStore(structValue, structAlloca);

    llvm::Value *abiPtr = m_builder.CreateBitCast(structAlloca,
                                                  llvm::PointerType::getUnqual(context),
                                                  "abi.ptr");
    llvm::Value *abiValue = m_builder.CreateLoad(abiType, abiPtr, "abi.val");

    return abiValue;
}

llvm::Value *LLVMCodegen::coerceFromABI(llvm::Value *abiValue, llvm::Type *structType) {
    if (!shouldCoerceForABI(structType)) {
        return abiValue;
    }

    llvm::Type *abiType = getABICoercionType(structType);

    // Allocate temporary for ABI value
    llvm::AllocaInst *abiAlloca = m_builder.CreateAlloca(abiType, nullptr, "abi.tmp");
    m_builder.CreateStore(abiValue, abiAlloca);

    // Bitcast to struct type pointer and load
    llvm::Value *structPtr = m_builder.CreateBitCast(abiAlloca,
                                                     llvm::PointerType::getUnqual(context),
                                                     "struct.ptr");
    llvm::Value *structValue = m_builder.CreateLoad(structType, structPtr, "struct.val");

    return structValue;
}
static std::shared_ptr<AsmStmt> lowerTemplateString(std::shared_ptr<AsmStmt> stmt) {
    // Replace template string placeholders like {0}, {1} with $0, $1
    std::string output;
    for (size_t i = 0; i < stmt->template_str.size(); ++i) {
        if (stmt->template_str[i] == '{' && i + 1 < stmt->template_str.size()) {
            size_t end = stmt->template_str.find('}', i);
            if (end != std::string::npos) {
                output += "$" + stmt->template_str.substr(i + 1, end - i - 1);
                i = end;
            } else {
                output += stmt->template_str[i];
            }
        } else {
            output += stmt->template_str[i];
        }
    }
    stmt->template_str = output;
    return stmt;
}

static bool isConcreteRegisterConstraint(const std::string &constraint) {
    static const std::vector<std::string> exact_registers = {
        "rax", "rbx", "rcx", "rdx", "rsi", "rdi", "rbp", "rsp",
        "eax", "ebx", "ecx", "edx", "esi", "edi", "ebp", "esp",
        "ax", "bx", "cx", "dx", "si", "di", "bp", "sp",
        "al", "bl", "cl", "dl", "ah", "bh", "ch", "dh",
        "rip", "eip"
    };

    for (const auto &name : exact_registers) {
        if (constraint == name) {
            return true;
        }
    }

    if (constraint.size() >= 2 && constraint[0] == 'r') {
        size_t index = 1;
        while (index < constraint.size() && constraint[index] >= '0' && constraint[index] <= '9') {
            ++index;
        }
        if (index > 1 && index == constraint.size()) {
            int register_number = std::stoi(constraint.substr(1));
            return register_number >= 8 && register_number <= 15;
        }
    }

    auto isDigitSuffix = [](const std::string &name, const std::string &prefix) {
        if (name.rfind(prefix, 0) != 0 || name.size() <= prefix.size())
            return false;
        for (size_t i = prefix.size(); i < name.size(); ++i) {
            if (name[i] < '0' || name[i] > '9')
                return false;
        }
        return true;
    };

    if (isDigitSuffix(constraint, "xmm") || isDigitSuffix(constraint, "ymm") ||
        isDigitSuffix(constraint, "zmm")) {
        return true;
    }

    return false;
}



llvm::Value *LLVMCodegen::generateAsmStatement(
    const std::shared_ptr<AsmStmt> &_stmt) {

    auto stmt = lowerTemplateString(_stmt);

    std::vector<llvm::Type *> outputTypes;
    std::vector<llvm::Value *> args;
    std::vector<llvm::Type *> argTypes;

    // First pass: collect outputs and their types
    for (auto &op : stmt->operands) {
        if (op.type == AsmOperandType::Out || op.type == AsmOperandType::InOut ||
            op.type == AsmOperandType::LateOut || op.type == AsmOperandType::InLateOut) {
            outputTypes.push_back(getLLVMType(op.expr->inferred_type, op.expr));
        }
    }

    // Second pass: generate input values and collect args
    for (auto &op : stmt->operands) {
        llvm::Value *v = generateExpression(op.expr);
        if (op.type == AsmOperandType::In || op.type == AsmOperandType::InLateOut ||
            op.type == AsmOperandType::Const || op.type == AsmOperandType::Sym) {
            args.push_back(v);
            argTypes.push_back(v->getType());
        }
    }

    llvm::Type *retTy;
    if (outputTypes.empty())
        retTy = llvm::Type::getVoidTy(context);
    else if (outputTypes.size() == 1)
        retTy = outputTypes[0];
    else
        retTy = llvm::StructType::get(context, outputTypes);

    auto *fnTy = llvm::FunctionType::get(retTy, argTypes, false);

    auto formatRegisterConstraint = [](const std::optional<std::string> &constraint) {
        if (!constraint)
            return std::string("r");
        if (isConcreteRegisterConstraint(*constraint))
            return std::string("{") + *constraint + "}";
        return *constraint;
    };

    std::vector<std::string> parts;
    for (auto &op : stmt->operands) {
        std::string constraint;
        switch (op.type) {
            case AsmOperandType::In:
                constraint = formatRegisterConstraint(op.constraint);
                break;
            case AsmOperandType::Out:
                constraint = "=" + formatRegisterConstraint(op.constraint);
                break;
            case AsmOperandType::InOut:
                constraint = "+";
                constraint += formatRegisterConstraint(op.constraint);
                break;
            case AsmOperandType::LateOut:
                constraint = "=&" + formatRegisterConstraint(op.constraint);
                break;
            case AsmOperandType::InLateOut:
                constraint = "+&";
                constraint += formatRegisterConstraint(op.constraint);
                break;
            case AsmOperandType::Const:
                constraint = "i";
                break;
            case AsmOperandType::Sym:
                constraint = "s";
                break;
            case AsmOperandType::Clobber:
                constraint = "~{" + op.constraint.value_or("") + "}";
                break;
            case AsmOperandType::ClobberAbi:
                constraint = "~{abi-" + op.constraint.value_or("") + "}";
                break;
        }
        parts.push_back(constraint);
    }

    std::string constraints = "";
    for (size_t i = 0; i < parts.size(); ++i) {
        constraints += parts[i];
        if (i != parts.size() - 1) {
            constraints += ",";
        }
    }

    auto *ia = llvm::InlineAsm::get(fnTy, stmt->template_str, constraints, stmt->is_volatile);

    llvm::Value *result = m_builder.CreateCall(ia, args);

    // Store outputs back to memory
    size_t outputIndex = 0;
    for (auto &op : stmt->operands) {
        if (op.type == AsmOperandType::Out || op.type == AsmOperandType::LateOut) {
            llvm::Value *v = (outputTypes.size() == 1) ? result :
                m_builder.CreateExtractValue(result, outputIndex);
            llvm::Value *outputPtr = generateAddress(op.expr);
            m_builder.CreateStore(v, outputPtr);
            outputIndex++;
        } else if (op.type == AsmOperandType::InOut || op.type == AsmOperandType::InLateOut) {
            llvm::Value *v = (outputTypes.size() == 1) ? result :
                m_builder.CreateExtractValue(result, outputIndex);
            llvm::Value *outputPtr = generateAddress(op.expr);
            m_builder.CreateStore(v, outputPtr);
            outputIndex++;
        }
    }

    return outputTypes.empty() ? nullptr : result;
}

llvm::Value* LLVMCodegen::generateSwitchStatement(const std::shared_ptr<SwitchStmt> &switchStmt) {
    int numCases = 0;

    for (const auto &caseBlock : switchStmt->cases) {
        numCases += caseBlock.first.size();
    }

    auto parentBlock = m_builder.GetInsertBlock();
    auto mergeBlock = llvm::BasicBlock::Create(context, "switch_merge", parentBlock->getParent());
    auto defaultBlock = llvm::BasicBlock::Create(context, "switch_default", parentBlock->getParent());


    auto sStmt =m_builder.CreateSwitch(generateExpression(switchStmt->condition), defaultBlock, numCases);

    for (const auto &caseBlock : switchStmt->cases) {
        auto exprs = caseBlock.first;
        numCases += exprs.size();

        auto caseBB = llvm::BasicBlock::Create(context, "switch_case", parentBlock->getParent());
        m_builder.SetInsertPoint(caseBB);
        generateStatement(caseBlock.second);

        if (!m_builder.GetInsertBlock()->getTerminator()) {
            m_builder.CreateBr(mergeBlock);
        }
        for (const auto &expr : exprs) {
            if (!expr->inferred_type) {
                throw CodeGenError(expr, "Case expression has no inferred type");
            }
            if (!IS_INSTANCE(expr, Literal)) {
                throw CodeGenError(expr, "Only literal expressions are supported in switch cases");
            }
            if (!expr->inferred_type->isInteger()){
                throw CodeGenError(expr, "Only int, bool, and char literals are supported in switch cases");
            }
            llvm::Value *caseVal = generateLiteral(std::dynamic_pointer_cast<Literal>(expr));
            sStmt->addCase(llvm::cast<llvm::ConstantInt>(caseVal), caseBB);
        }
    }

    m_builder.SetInsertPoint(defaultBlock);
    if (switchStmt->default_case) {
        generateStatement(switchStmt->default_case);
    }
    if (!m_builder.GetInsertBlock()->getTerminator()) {
        m_builder.CreateBr(mergeBlock);
    }
    m_builder.SetInsertPoint(mergeBlock);
    return nullptr;
}
