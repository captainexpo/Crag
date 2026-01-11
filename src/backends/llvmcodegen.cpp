#include "llvmcodegen.h"
#include "../module_resolver.h"
#include <array>
#include <filesystem>
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
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
        false, // not constant
        llvm::GlobalValue::AppendingLinkage,
        llvm::ConstantArray::get(arrTy, ctorStruct),
        "llvm.global_ctors");
}

void LLVMCodegen::emitBuiltinDeclarations() {
    llvm::FunctionType *panicType = llvm::FunctionType::get(llvm::Type::getVoidTy(context), {
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
    // https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl08.html Make

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

    pass.run(*m_llvm_module);
    dest.flush();
}

void LLVMCodegen::emitIrToFile(const std::string &filepath) {
    if (m_llvm_module) {
        std::error_code ec;
        llvm::raw_fd_ostream out(filepath, ec, llvm::sys::fs::OF_None);
        if (ec) {
            throw CodeGenError(nullptr, "Could not open file: " + ec.message());
        }
        m_llvm_module->print(out, nullptr);
    }
}
void LLVMCodegen::compileObjectFileToExecutable(const std::string &object_filepath,
                                                const std::filesystem::path &executable_filepath,
                                                const std::filesystem::path &runtime_path,
                                                bool no_runtime, std::string additional_compiler_args) {

    // See if clang is available
    if (system("clang --version > /dev/null 2>&1") != 0) {
        throw CodeGenError(nullptr, "Clang is not installed or not found in PATH.");
    }
    std::cout << "Compiling: " << object_filepath << " to executable...\n";
    std::string opt = (m_options.opt_level == Release) ? " -O3 " : " -O0 ";

    // Run clang to compile IR to executable
    std::filesystem::path finalOutputFilePath = executable_filepath.parent_path() / executable_filepath.stem();
    std::string clangCmd = "clang " + object_filepath + opt + additional_compiler_args;

    if (!no_runtime) {
        clangCmd += " " + runtime_path.string();
    }

    clangCmd += " -o " + finalOutputFilePath.string();

    int ret = system(clangCmd.c_str());

    if (ret != 0) {
        throw CodeGenError(nullptr, "Failed to invoke clang to create executable.");
    }
}

llvm::Type *LLVMCodegen::getLLVMType(const std::shared_ptr<Type> &type) {

    if (IS_INSTANCE(type, I32))
        return llvm::Type::getInt32Ty(context);
    if (IS_INSTANCE(type, I64))
        return llvm::Type::getInt64Ty(context);
    if (IS_INSTANCE(type, U8))
        return llvm::Type::getInt8Ty(context);
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
    if (IS_INSTANCE(type, USize))
        return llvm::Type::getInt64Ty(context); // Assuming 64-bit for USize
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
            paramTypes.push_back(getLLVMType(paramType));
        }
        return llvm::FunctionType::get(getLLVMType(funcType->ret), paramTypes,
                                       funcType->variadic);
    }
    if (IS_INSTANCE(type, ErrorUnionType)) {
        auto eut = std::dynamic_pointer_cast<ErrorUnionType>(type);
        // Represent as a struct { valueType, errorType, isError (i1) }
        std::vector<llvm::Type *> elements = {
            getLLVMType(eut->valueType), getLLVMType(eut->errorType),
            llvm::Type::getInt1Ty(context)};
        auto st = llvm::StructType::get(context, elements);
        return st;
    }
    if (IS_INSTANCE(type, EnumType)) {
        return getLLVMType(std::dynamic_pointer_cast<EnumType>(type)->base_type);
    }
    if (!type) {
        throw CodeGenError(nullptr, "Type is null");
    } else {
        throw CodeGenError(nullptr, "Unsupported type: " + type->str());
    }
    return nullptr;
}

llvm::Value *LLVMCodegen::generateAddress(const std::shared_ptr<Expression> &expr) {
    if (IS_INSTANCE(expr, VarAccess)) {
        auto var = std::dynamic_pointer_cast<VarAccess>(expr);
        return CUR_SCOPE.get(canonicalizeNonexternName(var->name))->value;
    } else if (IS_INSTANCE(expr, OffsetAccess)) {
        return generateOffsetAccess(std::dynamic_pointer_cast<OffsetAccess>(expr),
                                    false);
    } else if (IS_INSTANCE(expr, FieldAccess)) {
        return generateFieldAccess(std::dynamic_pointer_cast<FieldAccess>(expr),
                                   false);
    } else if (IS_INSTANCE(expr, UnaryOperation)) {
        auto unOp = std::dynamic_pointer_cast<UnaryOperation>(expr);
        return generateUnaryOp(unOp->operand, unOp->op, false);
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
    if (IS_INSTANCE(expr, BinaryOperation)) {
        auto binOp = std::dynamic_pointer_cast<BinaryOperation>(expr);
        return generateBinaryOp(binOp->left, binOp->right, binOp->op, loadValue);
    }
    if (IS_INSTANCE(expr, UnaryOperation)) {
        auto unOp = std::dynamic_pointer_cast<UnaryOperation>(expr);
        return generateUnaryOp(unOp->operand, unOp->op, loadValue);
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
            return m_builder.CreateLoad(getLLVMType(deref->inferred_type), ptr,
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
    llvm::Type *destType = getLLVMType(typeCast->target_type);
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
    }

    throw CodeGenError(stmt, "Unknown statement type: " + stmt->str());
}

llvm::Function *LLVMCodegen::generateFunctionDefinition(std::shared_ptr<FunctionDeclaration> func) {
    llvm::FunctionType *fType =
        llvm::cast<llvm::FunctionType>(this->getLLVMType(func->type));

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

    llvm::Function *function = llvm::Function::Create(
        abiType, llvm::Function::ExternalLinkage, fname, m_llvm_module.get());
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

    llvm::BasicBlock *entry =
        llvm::BasicBlock::Create(context, "entry", function);
    m_builder.SetInsertPoint(entry);
    // Allocate space for arguments and store them
    unsigned int idx = 0;
    for (auto &arg : function->args()) {
        llvm::AllocaInst *alloca =
            m_builder.CreateAlloca(arg.getType(), nullptr, arg.getName() + ".addr");
        m_builder.CreateStore(&arg, alloca);
        auto argname = arg.getName().str();
        CUR_SCOPE.set(canonicalizeNonexternName(argname), alloca, arg.getType(),
                      func->type->params[idx]);
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
        if (IS_INSTANCE(func->type->ret, Void)) {
            m_builder.CreateRetVoid();
        } else {
            throw CodeGenError(func, "Non-void function missing return: " + func->name);
        }
    }
    m_error_union_return_type = nullptr;

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

llvm::Value *LLVMCodegen::generateVariableDeclaration(
    const std::shared_ptr<VariableDeclaration> &varDecl) {
    static int varCounter = 0;
    bool is_global = m_scopeStack.size() == 1;

    if (is_global) {
        if (varDecl->is_extern) {
            // Extern global variable declaration
            // Don't canonicalize name due to extern linkage
            llvm::Type *varType = getLLVMType(varDecl->var_type);
            llvm::GlobalVariable *gVar = new llvm::GlobalVariable(
                *m_llvm_module, varType, varDecl->is_const,
                llvm::GlobalValue::ExternalLinkage, nullptr,
                varDecl->name);
            CUR_SCOPE.set(varDecl->name, gVar, varType, varDecl->var_type);
            return gVar;
        }
        llvm::Type *varType = getLLVMType(varDecl->var_type);
        llvm::Constant *initVal = llvm::Constant::getNullValue(varType);
        llvm::GlobalVariable *gVar = new llvm::GlobalVariable(
            *m_llvm_module, varType, false,
            llvm::GlobalValue::ExternalLinkage, initVal, canonicalizeNonexternName(varDecl->name));
        CUR_SCOPE.set(canonicalizeNonexternName(varDecl->name), gVar, varType, varDecl->var_type);
        if (varDecl->initializer) {
            if (auto expr = std::dynamic_pointer_cast<Expression>(varDecl->initializer)) {
                addGlobalVarInitializer(gVar, expr);
            } else {
                throw CodeGenError(varDecl, "Unsupported initializer type for global variable");
            }
        }
        return gVar;
    }

    llvm::Type *varType = getLLVMType(varDecl->var_type);
    llvm::Value *alloca = m_builder.CreateAlloca(varType, nullptr, varDecl->name);
    CUR_SCOPE.set(canonicalizeNonexternName(varDecl->name), alloca, varType, varDecl->var_type);
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
        llvm::Type *ty = getLLVMType(field.second);
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
        llvm::Type *ty = getLLVMType(field.second);
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
LLVMCodegen::generateUnaryOp(const std::shared_ptr<Expression> &operand,
                             std::string op, bool loadValue) {
    auto val = generateExpression(operand);
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
        return generateAddress(operand);
    else if (op == "~") {
        return m_builder.CreateNot(val, "bwnottmp");
    }

    throw CodeGenError(operand, "Unsupported unary operator: " + op);
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
    } else if (IS_INSTANCE(lit->inferred_type, I64)) {
        return llvm::ConstantInt::get(context,
                                      llvm::APInt(64, tryGetConstValue<int64_t>(lit)));
    } else if (IS_INSTANCE(lit->inferred_type, U8)) {
        return llvm::ConstantInt::get(context,
                                      llvm::APInt(8, tryGetConstValue<uint64_t>(lit)));
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
        return llvm::ConstantInt::get(
            context,
            llvm::APInt(64, tryGetConstValue<uint64_t>(lit))); // Assuming 64-bit for USize
    } else if (IS_INSTANCE(lit->inferred_type, PointerType)) {
        if (std::holds_alternative<uint64_t>(lit->value)) {
            int intVal = tryGetConstValue<uint64_t>(lit);
            if (intVal == 0) {
                // Null pointer literal
                return llvm::ConstantPointerNull::get(
                    llvm::cast<llvm::PointerType>(getLLVMType(lit->inferred_type)));
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
            getLLVMType(std::make_shared<PointerType>(std::make_shared<Void>()))));
    } else if (IS_INSTANCE(lit->inferred_type, ArrayType)) {
    }
    if (!lit->inferred_type) {
        throw CodeGenError(lit, "Literal " + lit->str() + " has no inferred type");
    }
    throw CodeGenError(lit, "Unsupported literal type: " + lit->inferred_type->str());
}

llvm::Value *LLVMCodegen::generateArrayLiteral(
    const std::shared_ptr<ArrayLiteral> &arrayLit, bool loadValue) {

    auto arrayType = std::dynamic_pointer_cast<ArrayType>(arrayLit->inferred_type);
    auto elemType = getLLVMType(arrayType->element_type);
    int numElements = arrayLit->len;

    // Allocate the raw array: [N x elemType]
    auto arrayLLVMType = getLLVMType(arrayType); // struct { ptr, i64 }
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
    m_builder.CreateStore(llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), numElements), lengthPtr);

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
    auto *sym = CUR_SCOPE.get(canonicalizeNonexternName(varAccess->name));
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

    if (auto *func = llvm::dyn_cast<llvm::Function>(calleeValue)) {
        // Hacky way to determine if function is extern:
        // If the function name starts with '/', it's internal
        // TODO: Better way to mark internal vs extern functions b/c for some reason the cleaner ways don't work
        funcTy = func->getFunctionType();
        std::string funcName = func->getName().str();
        isExtern = !funcName.empty() && funcName[0] != '/';
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
        funcTy = llvm::cast<llvm::FunctionType>(getLLVMType(funcType));
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
            llvm::Type *structType = getLLVMType(funcCall->inferred_type);
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
    std::cout << "Base ptr for array access: " << basePtr << "\n";
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
        std::dynamic_pointer_cast<ArrayType>(arrayAccess->base->inferred_type)->element_type);
    if (!elementType) {
        throw CodeGenError(arrayAccess, "Unknown element type for array access: " +
                                            arrayAccess->base->inferred_type->str());
    }
    // Condition check for index within bounds
    if (index->getType() != llvm::Type::getInt64Ty(context)) {
        index = m_builder.CreateZExt(index, llvm::Type::getInt64Ty(context), "index_to_i64");
    }
    llvm::Value *arrLoad = m_builder.CreateLoad(
        getLLVMType(arrayAccess->base->inferred_type), basePtr);
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
    llvm::Type *resultType = getLLVMType(offsetAccess->inferred_type);
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
        llvm::StructType* structType = llvm::cast<llvm::StructType>(getLLVMType(fieldAccess->base->inferred_type));
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
        llvm::cast<llvm::StructType>(getLLVMType(arr_type));

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
        llvm::StructType* structType = llvm::cast<llvm::StructType>(getLLVMType(fieldAccess->base->inferred_type));
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
        llvm::cast<llvm::StructType>(getLLVMType(eu_type));

    if (fieldAccess->field == "ok") {
        // Return index 0
        llvm::Type *ok_type = getLLVMType(eu_type->valueType);
        auto gep = m_builder.CreateStructGEP(euStructType, basePtr, 0, "ok_access");
        return loadValue
                   ? m_builder.CreateLoad(ok_type, gep, "load_ok")
                   : gep;

    } else if (fieldAccess->field == "err") {
        // Return index 1
        llvm::Type *err_type = getLLVMType(eu_type->errorType);
        auto gep = m_builder.CreateStructGEP(euStructType, basePtr, 1, "err_access");
        return loadValue
                   ? m_builder.CreateLoad(err_type, gep, "load_err")
                   : gep;

    } else if (fieldAccess->field == "is_err") {
        // Return index 2
        llvm::Type *bool_type = llvm::Type::getInt1Ty(context);

        auto gep = m_builder.CreateStructGEP(euStructType, basePtr, 2, "is_err_access");
        return loadValue
                   ? m_builder.CreateLoad(bool_type, gep, "load_is_err")
                   : gep;
    } else {
        throw CodeGenError(fieldAccess, "Unknown field on error union: " + fieldAccess->field);
    }
}

bool LLVMCodegen::isLValue(const std::shared_ptr<Expression> &expr) {
    if (!expr) return false;

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
        llvm::StructType* structType = llvm::cast<llvm::StructType>(getLLVMType(fieldAccess->base->inferred_type));
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
        llvm::Type *targetType = getLLVMType(fieldType);
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

    // Build GEP safely
    llvm::Value *gep = m_builder.CreateGEP(
        llvmStruct, basePtr,
        {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0),
         llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), fieldIndex)},
        fieldName);

    if (!gep) {
        throw CodeGenError(fieldAccess->base, "Failed to create GEP for field '" + fieldName +
                                                  "' in struct '" + structType->name + "'");
    }

    if (loadValue) {
        if (!fieldAccess->inferred_type) {
            throw CodeGenError(fieldAccess, "Field '" + fieldName +
                                                "' has no inferred type for load");
        }
        return m_builder.CreateLoad(getLLVMType(fieldAccess->inferred_type), gep);
    }

    return gep;
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

    auto *sym = CUR_SCOPE.get(full_name);
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

llvm::Value *LLVMCodegen::generateStructInitializer(
    const std::shared_ptr<StructInitializer> &structInit, bool loadValue) {
    auto _if_type = structInit->inferred_type;
    auto if_type = std::dynamic_pointer_cast<StructType>(_if_type);
    auto union_type = std::dynamic_pointer_cast<UnionType>(_if_type);

    if (union_type) {
        // Handle union initialization
        auto it = m_unionTypes.find(union_type->name);
        if (it == m_unionTypes.end()) {
            throw CodeGenError(structInit, "Unknown union type: " + union_type->name);
        }
        llvm::StructType *llvmUnion = it->second;
        llvm::Value *alloca =
            m_builder.CreateAlloca(llvmUnion, nullptr, "uniontmp");

        // Unions can only initialize one field at a time
        if (structInit->field_values.size() != 1) {
            throw CodeGenError(structInit, "Union initializer must have exactly one field, got " +
                                               std::to_string(structInit->field_values.size()));
        }

        const auto &field = *structInit->field_values.begin();
        auto fieldType = union_type->getFieldType(field.first);
        if (!fieldType) {
            throw CodeGenError(structInit, "Union '" + union_type->name +
                                               "' has no field named '" + field.first + "'");
        }

        // Get pointer to the storage (byte array at field 0)
        llvm::Value *storagePtr = m_builder.CreateGEP(
            llvmUnion, alloca,
            {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0),
             llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0)},
            "union_storage_ptr");

        // Cast to the field type pointer
        llvm::Type *targetType = getLLVMType(fieldType);
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

    if (!if_type) {
        throw CodeGenError(structInit, "Struct initializer has non-struct/union inferred type: " +
                                           (_if_type ? _if_type->str() : "null"));
    }
    auto it = m_structTypes.find(if_type->name);
    if (it == m_structTypes.end()) {
        throw CodeGenError(structInit, "Unknown struct type: " + if_type->name);
    }
    llvm::StructType *llvmStruct = it->second;
    llvm::Value *alloca =
        m_builder.CreateAlloca(llvmStruct, nullptr, "structtmp");
    for (const auto &field : structInit->field_values) {
        // Find field index
        auto fieldIndex = if_type->getFieldIndex(field.first);
        // Generate GEP for field
        llvm::Value *fieldPtr = m_builder.CreateGEP(
            llvmStruct, alloca,
            {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0),
             llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), fieldIndex)},
            field.first + "_ptr");
        llvm::Value *fieldVal = generateExpression(field.second);
        m_builder.CreateStore(fieldVal, fieldPtr);
    }
    if (!loadValue)
        return alloca;
    return m_builder.CreateLoad(llvmStruct, alloca, "loadstruct");
}

llvm::Value *
LLVMCodegen::generateBlock(const std::shared_ptr<Block> &blockNode) {
    auto parent_scope = !m_scopeStack.empty() ? std::make_shared<Scope>(CUR_SCOPE)
                                              : std::make_shared<Scope>(nullptr);
    m_scopeStack.push_back(*parent_scope);
    llvm::Value *lastValue = nullptr;
    for (const auto &stmt : blockNode->statements) {
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
    llvm::Function *theFunction = m_builder.GetInsertBlock()->getParent();
    auto cond_block =
        llvm::BasicBlock::Create(context, "while.cond", theFunction);
    auto body_block =
        llvm::BasicBlock::Create(context, "while.body", theFunction);
    auto after_block =
        llvm::BasicBlock::Create(context, "while.end", theFunction);
    m_loop_stack.push_back(LoopInfo{
        .breakBB = after_block,
        .continueBB = cond_block,
    });
    m_builder.CreateBr(cond_block);
    m_builder.SetInsertPoint(cond_block);
    auto condition = generateExpression(whileStmt->condition);
    condition = m_builder.CreateICmpNE(
        condition, llvm::ConstantInt::get(condition->getType(), 0), "whilecond");
    m_builder.CreateCondBr(condition, body_block, after_block);
    m_builder.SetInsertPoint(body_block);
    generateStatement(whileStmt->body);
    // Only branch back to condition if the block is not already terminated
    if (!m_builder.GetInsertBlock()->getTerminator()) {
        m_builder.CreateBr(cond_block);
    }
    m_builder.SetInsertPoint(after_block);
    m_loop_stack.pop_back();
    return nullptr;
}

llvm::Value *LLVMCodegen::generateForStatement(
    const std::shared_ptr<ForStatement> &forStmt) {

    llvm::Function *theFunction = m_builder.GetInsertBlock()->getParent();
    if (forStmt->init)
        generateStatement(forStmt->init);
    auto cond_block =
        llvm::BasicBlock::Create(context, "for.cond", theFunction);
    auto body_block =
        llvm::BasicBlock::Create(context, "for.body", theFunction);
    auto after_block =
        llvm::BasicBlock::Create(context, "for.end", theFunction);
    m_loop_stack.push_back(LoopInfo{
        .breakBB = after_block,
        .continueBB = cond_block,
    });
    m_builder.CreateBr(cond_block);
    m_builder.SetInsertPoint(cond_block);
    llvm::Value *condition = nullptr;
    if (forStmt->condition) {
        condition = generateExpression(forStmt->condition);
        condition = m_builder.CreateICmpNE(
            condition, llvm::ConstantInt::get(condition->getType(), 0), "forcond");
    } else {
        condition = llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), 1);
    }
    m_builder.CreateCondBr(condition, body_block, after_block);
    m_builder.SetInsertPoint(body_block);
    generateStatement(forStmt->body);
    // Only execute increment and branch if block is not already terminated
    if (!m_builder.GetInsertBlock()->getTerminator()) {
        if (forStmt->increment)
            generateStatement(forStmt->increment);
        m_builder.CreateBr(cond_block);
    }
    m_builder.SetInsertPoint(after_block);
    m_loop_stack.pop_back();
    return nullptr;
}

llvm::Value *LLVMCodegen::generateReturnStatement(
    const std::shared_ptr<ReturnStatement> &retStmt) {
    if (retStmt->value == nullptr) {
        auto retInstr = m_builder.CreateRetVoid();
        return retInstr;
    }
    auto retVal = generateExpression(retStmt->value);
    if (m_error_union_return_type != nullptr) {
        // should return error union struct
        llvm::Value *okVal = nullptr;
        llvm::Value *errVal = nullptr;

        llvm::Value *withTag = nullptr;

        llvm::StructType *errUnionStruct = llvm ::
            cast<llvm::StructType>(getLLVMType(m_error_union_return_type));
        llvm::Value *undefVal = llvm::UndefValue::get(errUnionStruct);

        if (retStmt->is_error) {
            errVal = retVal;
            okVal = llvm::Constant::getNullValue(getLLVMType(m_error_union_return_type->valueType));
            // Create the struct for error return
            // {okType, errType, i1 (is_error, should be true)}
            auto withOk = m_builder.CreateInsertValue(undefVal, okVal, 0, "insert_ok");

            auto withErr = m_builder.CreateInsertValue(withOk, errVal, 1, "insert_err");
            withTag = m_builder.CreateInsertValue(withErr,
                                                  llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), 1),
                                                  2, "insert_is_err");

        } else {
            okVal = retVal;
            errVal = llvm::Constant::getNullValue(getLLVMType(m_error_union_return_type->errorType));

            // Create the struct for ok return
            // {okType, errType, i1 (is_error, should be false)}
            auto withOk = m_builder.CreateInsertValue(undefVal, okVal, 0, "insert_ok");

            auto withErr = m_builder.CreateInsertValue(withOk, errVal, 1, "insert_err");
            withTag = m_builder.CreateInsertValue(withErr,
                                                  llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), 0),
                                                  2, "insert_is_err");
        }
        retVal = withTag;
    }

    auto retInstr = m_builder.CreateRet(retVal);
    return retInstr;
}

llvm::Value *LLVMCodegen::generateExpressionStatement(
    const std::shared_ptr<ExpressionStatement> &exprStmt) {
    return generateExpression(exprStmt->expression);
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

    // 1. Allocate ABI-shaped temporary
    llvm::AllocaInst *abiTmp =
        B.CreateAlloca(abiTy, nullptr, "abi.tmp");

    // ABI alignment (safe default)
    llvm::DataLayout DL = m_llvm_module->getDataLayout();
    unsigned abiAlign = DL.getABITypeAlign(abiTy).value();
    abiTmp->setAlignment(llvm::Align(abiAlign));

    // 2. Store ABI value
    B.CreateStore(abiValue, abiTmp);

    // 3. Allocate language-level struct
    llvm::AllocaInst *langTmp =
        B.CreateAlloca(langType, nullptr, "agg.tmp");

    unsigned langAlign = DL.getABITypeAlign(langType).value();
    langTmp->setAlignment(llvm::Align(langAlign));

    // 4. Compute copy size (language layout!)
    uint64_t copySize = DL.getTypeAllocSize(langType);

    // 5. memcpy ABI → language object
    B.CreateMemCpy(
        langTmp,
        llvm::Align(langAlign),
        abiTmp,
        llvm::Align(abiAlign),
        copySize);

    // 6. Return pointer to materialized object
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

    // Allocate temporary for struct
    llvm::AllocaInst *structAlloca = m_builder.CreateAlloca(structType, nullptr, "struct.tmp");
    m_builder.CreateStore(structValue, structAlloca);

    // Bitcast to ABI type pointer and load
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
