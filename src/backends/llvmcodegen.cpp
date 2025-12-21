#include "llvmcodegen.h"
#include "../module_resolver.h"
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

void LLVMCodegen::emitBuiltinDeclarations() {
    llvm::FunctionType *panicType = llvm::FunctionType::get(llvm::Type::getVoidTy(context), {
                                                                                                llvm::PointerType::getUnqual(context), // Error message
                                                                                                llvm::Type::getInt32Ty(context),       // Line number
                                                                                                llvm::Type::getInt32Ty(context)        // Column number
                                                                                            },
                                                            false);
    llvm::Function::Create(panicType, llvm::Function::ExternalLinkage, "__panic__", m_llvm_module.get());

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
            if (IS_INSTANCE(decl, FunctionDeclaration)) {
                auto fd = std::dynamic_pointer_cast<FunctionDeclaration>(decl);
                llvm::Function *fn = generateFunctionDefinition(fd);
                funcDecls.push_back({fn, fd});
                continue;
            }
            if (IS_INSTANCE(decl, VariableDeclaration)) {
                generateVariableDeclaration(
                    std::dynamic_pointer_cast<VariableDeclaration>(decl));
                continue;
            }
            if (IS_INSTANCE(decl, StructDeclaration)) {
                auto sd = std::dynamic_pointer_cast<StructDeclaration>(decl);
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
    if (llvm::verifyModule(*m_llvm_module, &llvm::errs())) {
        throw CodeGenError(nullptr, "Module verification failed - invalid IR");
    }

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
                                                bool no_runtime) {

    // See if clang is available
    if (system("clang --version > /dev/null 2>&1") != 0) {
        throw CodeGenError(nullptr, "Clang is not installed or not found in PATH.");
    }
    std::cout << "Compiling: " << object_filepath << " to executable...\n";
    std::string opt = (m_options.opt_level == Release) ? " -O3 " : " -O0 ";

    // Run clang to compile IR to executable
    std::filesystem::path finalOutputFilePath = executable_filepath.parent_path() / executable_filepath.stem();
    std::string clangCmd = "clang " + object_filepath + opt;

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

llvm::Value *
LLVMCodegen::generateAddress(const std::shared_ptr<Expression> &expr) {
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
    } else {
        throw CodeGenError(typeCast, "Unsupported cast from " + llvmTypeToString(srcType) +
                                         " to " + llvmTypeToString(destType) + " at " + typeCast->str());
    }
    return nullptr;
}

llvm::Value *
LLVMCodegen::generateStatement(const std::shared_ptr<Statement> &stmt) {
    if (IS_INSTANCE(stmt, VariableDeclaration)) {
        generateVariableDeclaration(
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
    } else {

        throw CodeGenError(stmt, "Unknown statement type: " + stmt->str());
    }
    return nullptr;
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

    llvm::Function *function = llvm::Function::Create(
        fType, llvm::Function::ExternalLinkage, fname, m_llvm_module.get());
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
void LLVMCodegen::generateVariableDeclaration(
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
            return;
        }
        llvm::Type *varType = getLLVMType(varDecl->var_type);
        llvm::Constant *initVal = nullptr;
        if (varDecl->initializer) {
            if (IS_INSTANCE(varDecl->initializer, Expression)) {
                llvm::Value *initValV = generateExpression(
                    std::dynamic_pointer_cast<Expression>(varDecl->initializer));
                if (llvm::isa<llvm::Constant>(initValV)) {
                    initVal = llvm::cast<llvm::Constant>(initValV);
                } else {
                    throw CodeGenError(varDecl, "Global variable initializer must be a constant");
                    initVal = llvm::Constant::getNullValue(varType);
                }
            } else {
                throw CodeGenError(varDecl, "Unsupported initializer type");
                initVal = llvm::Constant::getNullValue(varType);
            }
        } else {
            initVal = llvm::Constant::getNullValue(varType);
        }
        llvm::GlobalVariable *gVar = new llvm::GlobalVariable(
            *m_llvm_module, varType, varDecl->is_const,
            llvm::GlobalValue::ExternalLinkage, initVal, canonicalizeNonexternName(varDecl->name));
        CUR_SCOPE.set(canonicalizeNonexternName(varDecl->name), gVar, varType, varDecl->var_type);
        return;
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
}

std::vector<std::pair<llvm::Function *, std::shared_ptr<FunctionDeclaration>>> LLVMCodegen::generateStructMethods(
    const std::shared_ptr<StructDeclaration> &structDecl) {
    std::vector<std::pair<llvm::Function *, std::shared_ptr<FunctionDeclaration>>> methods;
    for (std::unordered_map<std::string, std::shared_ptr<FunctionDeclaration>>::iterator iter = structDecl->methods.begin();
         iter != structDecl->methods.end(); ++iter) {
        auto method = iter->second;
        std::string mangledName = structDecl->name + "." + method->name;
        method->name = mangledName;

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
    if (op == "=")
        l = generateAddress(left);
    else
        l = generateExpression(left);

    llvm::Value *r = generateExpression(right);

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
              return m_builder.CreateAdd(a, b, "addtmp");
          },
          [this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateFAdd(a, b, "faddtmp");
          }}},
        {"-",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateSub(a, b, "subtmp");
          },
          [this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateFSub(a, b, "fsubtmp");
          }}},
        {"*",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateMul(a, b, "multmp");
          },
          [this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateFMul(a, b, "fmultmp");
          }}},
        {"/",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateSDiv(a, b, "divtmp");
          },
          [this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateFDiv(a, b, "fdivtmp");
          }}},
        {"%",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateSRem(a, b, "modtmp");
          },
          [this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateFRem(a, b, "fmodtmp");
          }}},

        {"==",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateICmpEQ(a, b, "eqtmp");
          },
          [this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateFCmpUEQ(a, b, "feqtmp");
          }}},
        {"!=",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateICmpNE(a, b, "netmp");
          },
          [this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateFCmpUNE(a, b, "fnetmp");
          }}},
        {"<",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateICmpSLT(a, b, "lttmp");
          },
          [this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateFCmpULT(a, b, "flttmp");
          }}},
        {"<=",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateICmpSLE(a, b, "letmp");
          },
          [this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateFCmpULE(a, b, "fletmp");
          }}},
        {">",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateICmpSGT(a, b, "gttmp");
          },
          [this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateFCmpUGT(a, b, "fgttmp");
          }}},
        {">=",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateICmpSGE(a, b, "getmp");
          },
          [this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateFCmpUGE(a, b, "fgetmp");
          }}},
        {"<<",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateShl(a, b, "shltmp");
          },
          nullptr}},
        {">>",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateAShr(a, b, "shrtmp");
          },
          nullptr}},
        {"&",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateAnd(a, b, "andtmp");
          },
          nullptr}},
        {"|",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateOr(a, b, "ortmp");
          },
          nullptr}},
        {"^",
         {[this](llvm::Value *a, llvm::Value *b) {
              return m_builder.CreateXor(a, b, "xortmp");
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
        return nullptr;
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
        return nullptr;
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

llvm::Value *LLVMCodegen::generateLiteral(const std::shared_ptr<Literal> &lit,
                                          bool loadValue) {

    if (IS_INSTANCE(lit->inferred_type, I32)) {
        return llvm::ConstantInt::get(context,
                                      llvm::APInt(32, std::get<int64_t>(lit->value)));
    } else if (IS_INSTANCE(lit->inferred_type, I64)) {
        return llvm::ConstantInt::get(context,
                                      llvm::APInt(64, std::get<int64_t>(lit->value)));
    } else if (IS_INSTANCE(lit->inferred_type, U8)) {
        return llvm::ConstantInt::get(context,
                                      llvm::APInt(8, std::get<uint64_t>(lit->value)));
    } else if (IS_INSTANCE(lit->inferred_type, U32)) {
        return llvm::ConstantInt::get(context,
                                      llvm::APInt(32, std::get<uint64_t>(lit->value)));
    } else if (IS_INSTANCE(lit->inferred_type, U64)) {
        return llvm::ConstantInt::get(context,
                                      llvm::APInt(64, std::get<uint64_t>(lit->value)));
    } else if (IS_INSTANCE(lit->inferred_type, F32)) {
        return llvm::ConstantFP::get(m_builder.getFloatTy(),
                                     std::get<double>(lit->value));
    } else if (IS_INSTANCE(lit->inferred_type, F64)) {
        return llvm::ConstantFP::get(m_builder.getDoubleTy(),
                                     std::get<double>(lit->value));
    } else if (IS_INSTANCE(lit->inferred_type, Boolean)) {
        return llvm::ConstantInt::get(
            context, llvm::APInt(1, std::get<bool>(lit->value) ? 1 : 0));
    } else if (IS_INSTANCE(lit->inferred_type, Void)) {
        return nullptr; // Void literals don't have a value
    } else if (IS_INSTANCE(lit->inferred_type, USize)) {
        return llvm::ConstantInt::get(
            context,
            llvm::APInt(64,
                        std::get<int64_t>(lit->value))); // Assuming 64-bit for USize
    } else if (IS_INSTANCE(lit->inferred_type, PointerType)) {
        if (std::holds_alternative<int64_t>(lit->value)) {
            int intVal = std::get<int64_t>(lit->value);
            if (intVal == 0) {
                // Null pointer literal
                return llvm::ConstantPointerNull::get(
                    llvm::cast<llvm::PointerType>(getLLVMType(lit->inferred_type)));
            } else {
                throw CodeGenError(lit, "Only null (0) is allowed as integer pointer literal");
            }
        } else if (std::holds_alternative<std::string>(lit->value)) {
            // String literal → create global string
            auto strVal = std::get<std::string>(lit->value);
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
    } else {
        if (!lit->inferred_type) {
            throw CodeGenError(lit, "Literal " + lit->str() + " has no inferred type");
        }
        throw CodeGenError(lit, "Unsupported literal type: " + lit->inferred_type->str());
    }
    return nullptr;
}

llvm::Value *LLVMCodegen::generateArrayLiteral(const std::shared_ptr<ArrayLiteral> &arrayLit, bool loadValue) {
    auto arrayType = std::dynamic_pointer_cast<ArrayType>(arrayLit->inferred_type);
    auto elemType = getLLVMType(arrayType->element_type);
    int numElements = arrayType->length;
    size_t actualSize = arrayLit->len;
    auto arrayLLVMType = getLLVMType(arrayType);
    auto rawArrayType = llvm::ArrayType::get(elemType, actualSize);

    auto rawArrAlloc = m_builder.CreateAlloca(rawArrayType, nullptr);
    for (int i = 0; i < numElements; i++) {
        llvm::Value *elemVal = generateExpression(arrayLit->elements[i]);
        llvm::Value *idxs[] = {
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0),
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), i)};
        llvm::Value *elemPtr = m_builder.CreateGEP(rawArrayType, rawArrAlloc, idxs);
        m_builder.CreateStore(elemVal, elemPtr);
    }

    // Actual array as struct { i64, ptr }
    llvm::Value *arrayStructAlloc = m_builder.CreateAlloca(arrayLLVMType, nullptr);
    llvm::Value *lengthPtr = m_builder.CreateGEP(
        arrayLLVMType, arrayStructAlloc,
        {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0),
         llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 1)});
    m_builder.CreateStore(
        llvm::ConstantInt::get(llvm::Type::getInt64Ty(context), actualSize),
        lengthPtr);
    llvm::Value *dataPtr = m_builder.CreateGEP(
        arrayLLVMType, arrayStructAlloc,
        {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0),
         llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0)});
    llvm::Value *decayedPtr = m_builder.CreateInBoundsGEP(
        rawArrayType, rawArrAlloc,
        {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0),
         llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0)});
    m_builder.CreateStore(decayedPtr, dataPtr);

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

llvm::Value *LLVMCodegen::generateMethodCall(const std::shared_ptr<MethodCall> &methodCall, bool loadValue) {

    auto structAccess = methodCall->object;
    llvm::Value *objPtr = generateAddress(std::static_pointer_cast<Expression>(structAccess));
    if (!objPtr) {
        throw CodeGenError(structAccess, "Failed to generate address for method call object");
    }
    auto structType = std::dynamic_pointer_cast<StructType>(structAccess->inferred_type);
    if (!structType) {
        auto ptrType = std::dynamic_pointer_cast<PointerType>(structAccess->inferred_type);
        if (ptrType) {
            structType = std::dynamic_pointer_cast<StructType>(ptrType->base);
            // Update objPtr to load the pointer value
            if (objPtr) {
                objPtr = m_builder.CreateLoad(
                    llvm::PointerType::getUnqual(context), objPtr,
                    "load_ptr_for_method");
            }
        }
        if (!structType) {
            throw CodeGenError(structAccess, "Method call on non-pointer, non-struct type: " + structAccess->inferred_type->str());
        }
    }
    if (!structType) {
        throw CodeGenError(structAccess,
                           "Failed to determine struct type for method call");
        return nullptr;
    }
    // Find the struct type
    auto accessee = m_structTypes.find(structType->name);
    if (accessee == m_structTypes.end()) {
        throw CodeGenError(structAccess, "Unknown struct type in method call: " + structType->name);
    }
    // Mangle method name
    std::string mangledName = canonicalizeNonexternName(structType->name + "." + methodCall->method);
    // Prepare arguments
    std::vector<llvm::Value *> argsV;
    argsV.push_back(objPtr); // 'this' pointer
    for (const auto &arg : methodCall->args) {
        argsV.push_back(generateExpression(arg));
    }
    llvm::Function *calleeValue = m_llvm_module->getFunction(mangledName);
    if (!calleeValue) {
        throw CodeGenError(methodCall, "Unknown method: " + mangledName);
    }
    return m_builder.CreateCall(calleeValue, argsV, calleeValue->getFunctionType()->getReturnType() != llvm::Type::getVoidTy(context) ? "methodcalltmp" : "");
}
llvm::Value *
LLVMCodegen::generateFuncCall(const std::shared_ptr<FuncCall> &funcCall,
                              bool loadValue) {
    std::vector<llvm::Value *> argsV;
    for (const auto &arg : funcCall->args) {
        argsV.push_back(generateExpression(arg));
    }

    llvm::Value *calleeValue =
        generateExpression(std::static_pointer_cast<Expression>(funcCall->func));
    if (!calleeValue) {
        throw CodeGenError(funcCall, "Failed to generate callee for function call");
    }

    llvm::FunctionType *funcTy = nullptr;

    if (auto *func = llvm::dyn_cast<llvm::Function>(calleeValue)) {
        // Direct function
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
        funcTy = llvm::cast<llvm::FunctionType>(getLLVMType(funcType));
    } else {
        throw CodeGenError(funcCall, "Callee is not a function or function pointer: " +
                                         funcCall->func->str());
        return nullptr;
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
                argsV[i] = m_builder.CreateZExt(arg, llvm::Type::getInt32Ty(context), "vararg_promotetmp");
            } else if (ty->isFloatTy()) {
                argsV[i] = m_builder.CreateFPExt(arg, llvm::Type::getDoubleTy(context), "vararg_promotetmp");
            }
        }
    }

    return m_builder.CreateCall(funcTy, calleeValue, argsV, funcTy->getReturnType() != llvm::Type::getVoidTy(context) ? "calltmp" : "");
}
llvm::Value *LLVMCodegen::conditionOrPanic(llvm::Value *condition, RuntimePanicType panicType, int line, int col) {

    llvm::Function *curFunc = m_builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *thenBB = llvm::BasicBlock::Create(context, "good_check", curFunc);
    llvm::BasicBlock *elseBB = llvm::BasicBlock::Create(context, "bad_check", curFunc);
    llvm::BasicBlock *mergeBB = llvm::BasicBlock::Create(context, "post_check", curFunc);

    m_builder.CreateCondBr(condition, thenBB, elseBB);

    m_builder.SetInsertPoint(thenBB);
    // Then block: condition is true, continue execution
    m_builder.CreateBr(mergeBB);
    m_builder.SetInsertPoint(elseBB);
    // Else block: condition is false, emit panic

    // Get panic function (will be extern declared)
    llvm::Function *panicFunc = m_llvm_module->getFunction("__panic__");
    llvm::FunctionType *panicFuncTy = panicFunc->getFunctionType();

    std::vector<llvm::Value *> panicArgs = {
        m_runtime_panic_strings[panicType],
        llvm::ConstantInt::get(context, llvm::APInt(32, line)),
        llvm::ConstantInt::get(context, llvm::APInt(32, col)),
    };
    m_builder.CreateCall(panicFuncTy, panicFunc, panicArgs);

    m_builder.CreateBr(mergeBB);
    m_builder.SetInsertPoint(mergeBB);

    return nullptr; // No meaningful value to return
}
llvm::Value *LLVMCodegen::generateArrayAccess(
    const std::shared_ptr<OffsetAccess> &arrayAccess, bool loadValue) {
    // if (!arrayAccess || !arrayAccess->base || !arrayAccess->index) {
    //     throw CodeGenError(nullptr, "Invalid array access: missing base or index expression");
    // }
    llvm::Value *basePtr = generateAddress(
        std::static_pointer_cast<Expression>(arrayAccess->base));
    std::cout << "Base ptr for array access: " << basePtr << "\n";
    // if (!basePtr) {
    //     throw CodeGenError(arrayAccess, "Failed to generate base address for array access");
    // }
    llvm::Value *index = generateExpression(arrayAccess->index);
    // if (!index) {
    //     throw CodeGenError(arrayAccess->index, "Failed to generate index for array access");
    // }
    // if (!arrayAccess->base->inferred_type ||
    //     arrayAccess->base->inferred_type->kind() != TypeKind::Array) {
    //     throw CodeGenError(arrayAccess->base, "Base expression is not of array type: " +
    //                                               (arrayAccess->base->inferred_type ? arrayAccess->base->inferred_type->str() : "null"));
    // }
    llvm::Type *elementType = getLLVMType(
        std::dynamic_pointer_cast<ArrayType>(arrayAccess->base->inferred_type)->element_type);
    // if (!elementType) {
    //     throw CodeGenError(arrayAccess, "Unknown element type for array access: " +
    //                                         arrayAccess->base->inferred_type->str());
    // }
    // Condition check for index within bounds
    // if (index->getType() != llvm::Type::getInt64Ty(context)) {
    //     index = m_builder.CreateZExt(index, llvm::Type::getInt64Ty(context), "index_to_i64");
    // }
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

    // data ptr
    llvm::Value *dataPtr = m_builder.CreateExtractValue(
        arrLoad,
        {0}, "array_data_ptr");
    // Gep instruction
    llvm::Value *gep = m_builder.CreateGEP(
        elementType, dataPtr, index, "array_elem_ptr");

    // Load the value at the computed address
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
        std::cout << "Generating array access for offset access\n";
        std::cout << "load val: " << (loadValue ? "true" : "false") << "\n";
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
    // Gep instruction
    llvm::Value *gep =
        m_builder.CreateGEP(resultType, basePtr, index, "offset_access");
    // Load the value at the computed address
    if (loadValue)
        return m_builder.CreateLoad(resultType, gep, "load_offset");
    else
        return gep;
}

llvm::Value *LLVMCodegen::generateArrayFieldAccess(const std::shared_ptr<FieldAccess> &fieldAccess, bool loadValue) {
    auto basePtr = generateAddress(
        std::static_pointer_cast<Expression>(fieldAccess->base));
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
    auto basePtr = generateAddress(
        std::static_pointer_cast<Expression>(fieldAccess->base));
    if (!basePtr) {
        throw CodeGenError(fieldAccess->base, "Failed to generate address for error union field base");
    }
    auto eu_type = std::dynamic_pointer_cast<ErrorUnionType>(fieldAccess->base->inferred_type);
    if (!eu_type) {
        throw CodeGenError(fieldAccess->base, "Field access on non-error-union type: " + fieldAccess->base->inferred_type->str());
    }
    llvm::StructType *euStructType =
        llvm::cast<llvm::StructType>(getLLVMType(eu_type));

    if (fieldAccess->field == "ok") {
        // Return index 0
        llvm::Type *ok_type = getLLVMType(eu_type->valueType);
        auto gep = m_builder.CreateStructGEP(euStructType, basePtr, 0, "error_union_ok_access");
        return loadValue
                   ? m_builder.CreateLoad(ok_type, gep, "load_error_union_ok")
                   : gep;

    } else if (fieldAccess->field == "err") {
        // Return index 1
        llvm::Type *err_type = getLLVMType(eu_type->errorType);
        auto gep = m_builder.CreateStructGEP(euStructType, basePtr, 1, "error_union_err_access");
        return loadValue
                   ? m_builder.CreateLoad(err_type, gep, "load_error_union_err")
                   : gep;

    } else if (fieldAccess->field == "is_err") {
        // Return index 2
        llvm::Type *bool_type = llvm::Type::getInt1Ty(context);
        auto gep = m_builder.CreateStructGEP(euStructType, basePtr, 2, "error_union_is_err_access");
        return loadValue
                   ? m_builder.CreateLoad(bool_type, gep, "load_error_union_is_err")
                   : gep;

    } else {
        throw CodeGenError(fieldAccess, "Unknown field on error union: " + fieldAccess->field);
    }

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

    llvm::Value *basePtr =
        generateAddress(std::static_pointer_cast<Expression>(fieldAccess->base));
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
                return nullptr;
            }

            if (structType || unionType) {
                // Adjust basePtr to dereference the pointer
                basePtr = m_builder.CreateLoad(
                    llvm::PointerType::getUnqual(context), basePtr,
                    "load_ptr_for_field");
                if (!basePtr) {
                    throw CodeGenError(fieldAccess->base,
                                       "Failed to load pointer value for field access");
                    return nullptr;
                }
            }
        }

        if (!structType && !unionType) {
            throw CodeGenError(fieldAccess->base,
                               "Field access on non-struct/union type: " +
                                   fieldAccess->base->inferred_type->str());
            return nullptr;
        }
    }

    // Handle union field access
    if (unionType) {
        auto accessee = m_unionTypes.find(unionType->name);
        if (accessee == m_unionTypes.end() || !accessee->second) {
            throw CodeGenError(fieldAccess->base,
                               "Unknown or unregistered union type in field access: " +
                                   unionType->name);
            return nullptr;
        }

        llvm::StructType *llvmUnion = accessee->second;

        // Validate field existence
        const auto &fieldName = fieldAccess->field;
        auto fieldType = unionType->getFieldType(fieldName);
        if (!fieldType) {
            throw CodeGenError(fieldAccess->base,
                               "Union '" + unionType->name +
                                   "' has no field named '" + fieldName + "'");
            return nullptr;
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
        return nullptr;
    }

    llvm::StructType *llvmStruct = accessee->second;

    // Validate field existence
    const auto &fieldName = fieldAccess->field;
    int fieldIndex = structType->getFieldIndex(fieldName);
    if (fieldIndex < 0) {
        throw CodeGenError(fieldAccess->base,
                           "Struct '" + structType->name +
                               "' has no field named '" + fieldName + "'");
        return nullptr;
    }

    // Build GEP safely
    llvm::Value *gep = m_builder.CreateGEP(
        llvmStruct, basePtr,
        {llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), 0),
         llvm::ConstantInt::get(llvm::Type::getInt32Ty(context), fieldIndex)},
        "field_access");

    if (!gep) {
        throw CodeGenError(fieldAccess->base, "Failed to create GEP for field '" + fieldName +
                                                  "' in struct '" + structType->name + "'");
        return nullptr;
    }

    if (loadValue) {
        if (!fieldAccess->inferred_type) {
            throw CodeGenError(fieldAccess, "Field '" + fieldName +
                                                "' has no inferred type for load");
            return nullptr;
        }
        return m_builder.CreateLoad(getLLVMType(fieldAccess->inferred_type), gep,
                                    "load_field");
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
            "fieldptr");
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
