#include "codegen.h"
#include "ast.h"
#include "module_resolver.h"
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

// Public API stubs
void IRGenerator::generate(std::shared_ptr<Module> module) {

  std::vector<std::pair<llvm::Function *, std::shared_ptr<FunctionDeclaration>>> funcDecls;

  m_current_module = module;
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
    if (IS_INSTANCE(decl, EnumDeclaration)) {
      generateEnumDeclaration(
          std::dynamic_pointer_cast<EnumDeclaration>(decl));
      continue;
    }
    if (IS_INSTANCE(decl, ImportDeclaration)) {
      continue; // Handled elsewhere
    }
    throw CodeGenError(decl, "Unknown top-level declaration: " + decl->str());
  }
  for (const auto &fnPair : funcDecls) {
    generateFunctionBody(fnPair.second, fnPair.first);
  }
}

int IRGenerator::outputObjFile(const std::string &filename) {
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
    return 1;
  }
  auto CPU = "generic";
  auto Features = "";

  llvm::TargetOptions opt;
  auto targetMachine = Target->createTargetMachine(targetTriple, CPU, Features,
                                                   opt, llvm::Reloc::PIC_);
  m_llvm_module->setDataLayout(targetMachine->createDataLayout());
  m_llvm_module->setTargetTriple(targetTriple);

  std::error_code ec;
  llvm::raw_fd_ostream dest(filename, ec, llvm::sys::fs::OF_None);

  if (ec) {
    throw CodeGenError(nullptr, "Could not open file: " + ec.message());
    return 1;
  }
  llvm::legacy::PassManager pass;
  auto FileType = llvm::CodeGenFileType::ObjectFile;

  if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
    throw CodeGenError(nullptr, "TargetMachine can't emit a file of this type");
    return 1;
  }

  pass.run(*m_llvm_module);
  dest.flush();

  return 0;
}

void IRGenerator::printIR(const std::string &filename) {
  if (m_llvm_module) {
    std::error_code ec;
    llvm::raw_fd_ostream out(filename, ec, llvm::sys::fs::OF_None);
    if (ec) {
      throw CodeGenError(nullptr, "Could not open file: " + ec.message());
    }
    m_llvm_module->print(out, nullptr);
  }
}

llvm::Type *IRGenerator::getLLVMType(const std::shared_ptr<Type> &type) {

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
  if (IS_INSTANCE(type, PointerType)) {
    auto ptrType = std::dynamic_pointer_cast<PointerType>(type);
    return llvm::PointerType::getUnqual(getLLVMType(ptrType->base));
  }
  if (IS_INSTANCE(type, ArrayType)) {
    auto arrType = std::dynamic_pointer_cast<ArrayType>(type);
    return llvm::ArrayType::get(getLLVMType(arrType->base), arrType->length);
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
IRGenerator::generateAddress(const std::shared_ptr<Expression> &expr) {
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
    }
  } else if (IS_INSTANCE(expr, Dereference)) {
    auto deref = std::dynamic_pointer_cast<Dereference>(expr);
    return generateExpression(deref->pointer, true);
  }
  throw CodeGenError(expr, "Expression is not an lvalue: " + expr->str());
}

llvm::Value *
IRGenerator::generateExpression(const std::shared_ptr<Expression> &expr,
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
    if (loadValue) {
      return m_builder.CreateLoad(getLLVMType(deref->inferred_type), ptr,
                                  "derefloadtmp");
    }
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
  throw CodeGenError(expr, "Unknown expression type: " + expr->str());
}
std::string llvmTypeToString(llvm::Type *ty) {
  std::string str;
  llvm::raw_string_ostream rso(str);
  ty->print(rso);
  return rso.str();
}

llvm::Value *
IRGenerator::generateCast(const std::shared_ptr<TypeCast> &typeCast,
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
                                     " to " + llvmTypeToString(destType));
  }
  return nullptr;
}

llvm::Value *
IRGenerator::generateStatement(const std::shared_ptr<Statement> &stmt) {
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
  } else {

    throw CodeGenError(stmt, "Unknown statement type: " + stmt->str());
  }
  return nullptr;
}

llvm::Function *IRGenerator::generateFunctionDefinition(std::shared_ptr<FunctionDeclaration> func) {
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

llvm::Function *IRGenerator::generateFunctionBody(std::shared_ptr<FunctionDeclaration> func, llvm::Function *function) {
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
void IRGenerator::generateVariableDeclaration(
    const std::shared_ptr<VariableDeclaration> &varDecl) {

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

std::vector<std::pair<llvm::Function *, std::shared_ptr<FunctionDeclaration>>> IRGenerator::generateStructMethods(
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

void IRGenerator::generateStructDeclaration(
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

void IRGenerator::generateEnumDeclaration(
    const std::shared_ptr<EnumDeclaration> &enumDecl) {
  // No actual code-gen here, just putting the type in global scope
  m_enumTypes[enumDecl->name] =
      std::dynamic_pointer_cast<EnumType>(enumDecl->inferred_type);
}

llvm::Value *IRGenerator::generateBinaryOp(
    const std::shared_ptr<Expression> &left,
    const std::shared_ptr<Expression> &right,
    std::string op, bool loadValue) {

  if (!left || !right) {
    throw CodeGenError(nullptr, "Invalid binary operation: null operand expression");
  }

  llvm::Value *l = generateExpression(left);
  llvm::Value *r = generateExpression(right);

  if (!l || !r) {
    throw CodeGenError(nullptr, "Failed to generate LLVM value for binary operands");
  }

  llvm::Type *ty = l->getType();
  if (!ty) {
    throw CodeGenError(left, "Left operand of binary op has no type");
  }

  // Handle assignment separately
  if (op == "=") {
    llvm::Value *addr = generateAddress(left);
    if (!addr) {
      throw CodeGenError(left, "Left operand of assignment is not an lvalue");
    }
    m_builder.CreateStore(r, addr);
    return r;
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
  };

  // Pointer handling → cast to integer
  if (ty->isPointerTy()) {
    ty = llvm::Type::getInt64Ty(context);
    l = m_builder.CreatePtrToInt(l, ty, "ptrtoint_lhs");
    r = m_builder.CreatePtrToInt(r, ty, "ptrtoint_rhs");
    if (!l || !r) {
      throw CodeGenError(nullptr, "Failed to convert pointer operands to integer");
    }
  }

  // Ensure both operands have the same type
  if (l->getType() != r->getType()) {
    throw CodeGenError(nullptr, "Type mismatch in binary op: lhs=" +
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
IRGenerator::generateUnaryOp(const std::shared_ptr<Expression> &operand,
                             std::string op, bool loadValue) {
  auto val = generateExpression(operand);
  if (op == "-") {
    if (val->getType()->isFloatingPointTy()) {
      return m_builder.CreateFNeg(val, "fnegtmp");
    }
    return m_builder.CreateNeg(val, "negtmp");
  } else if (op == "+") {
    return val; // Unary plus is a no-op
  } else if (op == "!") {
    return m_builder.CreateNot(val, "nottmp");
  } else if (op == "&") {
    return generateAddress(operand);
  }
  throw CodeGenError(operand, "Unsupported unary operator: " + op);
}

llvm::Value *IRGenerator::generateLiteral(const std::shared_ptr<Literal> &lit,
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
  } else {
    if (!lit->inferred_type) {
      throw CodeGenError(lit, "Literal " + lit->str() + " has no inferred type");
    }
    throw CodeGenError(lit, "Unsupported literal type: " + lit->inferred_type->str());
  }
  return nullptr;
}

llvm::Value *
IRGenerator::generateVarAccess(const std::shared_ptr<VarAccess> &varAccess,
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
  return m_builder.CreateLoad(sym->type, v, varAccess->name);
}

llvm::Value *IRGenerator::generateEnumAccess(const std::shared_ptr<EnumAccess> &enumAccess, bool loadValue) {
  auto it = m_enumTypes.find(enumAccess->enum_name);
  if (it == m_enumTypes.end()) {
    throw CodeGenError(enumAccess, "Unknown enum type: " + enumAccess->enum_name);
  }
  auto enumType = it->second;
  auto valIt = enumType->variant_map.find(enumAccess->variant);
  if (valIt == enumType->variant_map.end()) {
    throw CodeGenError(enumAccess, "Unknown enum variant: " + enumAccess->variant);
  }
  std::shared_ptr<Literal> literal = valIt->second;
  // Assuming enums are represented as i32
  return generateLiteral(literal, loadValue);
}

llvm::Value *IRGenerator::generateMethodCall(const std::shared_ptr<MethodCall> &methodCall, bool loadValue) {

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
            llvm::PointerType::getUnqual(getLLVMType(ptrType->base)), objPtr,
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
IRGenerator::generateFuncCall(const std::shared_ptr<FuncCall> &funcCall,
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
llvm::Value *IRGenerator::generateOffsetAccess(
    const std::shared_ptr<OffsetAccess> &offsetAccess, bool loadValue) {

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
    return nullptr;
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

llvm::Value *IRGenerator::generateFieldAccess(
    const std::shared_ptr<FieldAccess> &fieldAccess, bool loadValue) {

  if (!fieldAccess || !fieldAccess->base) {
    throw CodeGenError(nullptr, "Invalid field access: missing base expression");
  }

  if (!fieldAccess->base->inferred_type) {
    throw CodeGenError(fieldAccess->base, "Base expression of field access has no inferred type");
  }

  llvm::Value *basePtr =
      generateAddress(std::static_pointer_cast<Expression>(fieldAccess->base));
  if (!basePtr) {
    throw CodeGenError(fieldAccess->base, "Failed to generate address for field base");
  }

  if (auto eu_type = std::dynamic_pointer_cast<ErrorUnionType>(fieldAccess->base->inferred_type)) {
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
  // Check if base type is a struct or pointer-to-struct
  std::shared_ptr<StructType> structType =
      std::dynamic_pointer_cast<StructType>(fieldAccess->base->inferred_type);

  if (!structType) {
    auto ptrType =
        std::dynamic_pointer_cast<PointerType>(fieldAccess->base->inferred_type);
    if (ptrType) {
      structType = std::dynamic_pointer_cast<StructType>(ptrType->base);

      if (!ptrType->base) {
        throw CodeGenError(fieldAccess->base,
                           "Pointer type in field access has no base type");
        return nullptr;
      }

      if (structType) {
        // Adjust basePtr to dereference the pointer
        basePtr = m_builder.CreateLoad(
            llvm::PointerType::getUnqual(getLLVMType(ptrType->base)), basePtr,
            "load_ptr_for_field");
        if (!basePtr) {
          throw CodeGenError(fieldAccess->base,
                             "Failed to load pointer value for field access");
          return nullptr;
        }
      }
    }

    if (!structType) {
      throw CodeGenError(fieldAccess->base,
                         "Field access on non-struct type: " +
                             fieldAccess->base->inferred_type->str());
      return nullptr;
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

llvm::Value *IRGenerator::generateModuleAccess(const std::shared_ptr<ModuleAccess> &moduleAccess,
                                               bool loadValue) {
  // Same as variable access, but with namespaced name

  auto mod_name = moduleAccess->module_name;
  std::shared_ptr<Module> mod;
  if (m_current_module->imports.find(moduleAccess->module_name) !=
      m_current_module->imports.end()) {
    mod =
        m_current_module->imports[moduleAccess->module_name];
  } else {
    throw CodeGenError(moduleAccess,
                       "Module not found: " + moduleAccess->module_name);
    return nullptr;
  }

  auto var_name = moduleAccess->member_name;
  // make sure full name isn't an extern name
  auto full_name = var_name;
  if (mod->externDeclarations.find(var_name) == mod->externDeclarations.end()) {
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

llvm::Value *IRGenerator::generateStructInitializer(
    const std::shared_ptr<StructInitializer> &structInit, bool loadValue) {
  auto it = m_structTypes.find(structInit->struct_type->name);
  if (it == m_structTypes.end()) {
    throw CodeGenError(structInit, "Unknown struct type: " + structInit->struct_type->name);
  }
  llvm::StructType *llvmStruct = it->second;
  llvm::Value *alloca =
      m_builder.CreateAlloca(llvmStruct, nullptr, "structtmp");
  for (const auto &field : structInit->field_values) {
    // Find field index
    auto fieldIndex = structInit->struct_type->getFieldIndex(field.first);
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
IRGenerator::generateBlock(const std::shared_ptr<Block> &blockNode) {
  auto parent_scope = !m_scopeStack.empty() ? std::make_shared<Scope>(CUR_SCOPE)
                                            : std::make_shared<Scope>(nullptr);
  m_scopeStack.push_back(*parent_scope);
  llvm::Value *lastValue = nullptr;
  for (const auto &stmt : blockNode->statements) {
    try {
      lastValue = generateStatement(stmt);
    } catch (const CodeGenError &e) {
      m_errors.push_back(std::make_pair(e.node(), e.what()));
    }
  }
  return lastValue;
}

llvm::Value *
IRGenerator::generateIfStatement(const std::shared_ptr<IfStatement> &ifStmt) {

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
  m_builder.CreateBr(mergeBB);
  thenBB = m_builder.GetInsertBlock();
  m_builder.SetInsertPoint(elseBB);
  if (ifStmt->else_branch)
    llvm::Value *elseV = generateStatement(ifStmt->else_branch);
  m_builder.CreateBr(mergeBB); // <- This is missing
  m_builder.SetInsertPoint(mergeBB);
  return nullptr;
}

llvm::Value *IRGenerator::generateWhileStatement(
    const std::shared_ptr<WhileStatement> &whileStmt) {
  llvm::Function *theFunction = m_builder.GetInsertBlock()->getParent();
  auto cond_block =
      llvm::BasicBlock::Create(context, "while.cond", theFunction);
  auto body_block =
      llvm::BasicBlock::Create(context, "while.body", theFunction);
  auto after_block =
      llvm::BasicBlock::Create(context, "while.end", theFunction);
  m_builder.CreateBr(cond_block);
  m_builder.SetInsertPoint(cond_block);
  auto condition = generateExpression(whileStmt->condition);
  condition = m_builder.CreateICmpNE(
      condition, llvm::ConstantInt::get(condition->getType(), 0), "whilecond");
  m_builder.CreateCondBr(condition, body_block, after_block);
  m_builder.SetInsertPoint(body_block);
  generateStatement(whileStmt->body);
  m_builder.CreateBr(cond_block);
  m_builder.SetInsertPoint(after_block);
  return nullptr;
}

llvm::Value *IRGenerator::generateForStatement(
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
  if (forStmt->increment)
    generateStatement(forStmt->increment);
  m_builder.CreateBr(cond_block);
  m_builder.SetInsertPoint(after_block);
  return nullptr;
}

llvm::Value *IRGenerator::generateReturnStatement(
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

llvm::Value *IRGenerator::generateExpressionStatement(
    const std::shared_ptr<ExpressionStatement> &exprStmt) {
  return generateExpression(exprStmt->expression);
}
