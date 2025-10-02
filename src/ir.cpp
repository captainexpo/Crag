#include "ir.h"
#include "ast.h"
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
// Constructor: create module and initialize builder
IRGenerator::IRGenerator(std::string module_name)
    : m_context(),
      m_module(std::make_unique<llvm::Module>(module_name, m_context)),
      m_builder(m_context) {
  // Initialize the global scope
  m_scopeStack.push_back(Scope(nullptr));
}

// Public API stubs
void IRGenerator::generate(const std::shared_ptr<Program> &node) {
  for (const auto &decl : node->declarations) {
    if (IS_INSTANCE(decl, FunctionDeclaration)) {
      generateFunction(std::dynamic_pointer_cast<FunctionDeclaration>(decl));
      continue;
    }
    if (IS_INSTANCE(decl, VariableDeclaration)) {
      generateVariableDeclaration(
          std::dynamic_pointer_cast<VariableDeclaration>(decl));
      continue;
    }
    if (IS_INSTANCE(decl, StructDeclaration)) {
      generateStructDeclaration(
          std::dynamic_pointer_cast<StructDeclaration>(decl));
      continue;
    }
    if (IS_INSTANCE(decl, EnumDeclaration)) {
      generateEnumDeclaration(
          std::dynamic_pointer_cast<EnumDeclaration>(decl));
      continue;
    }

    error(node, "Unknown top-level declaration: " + decl->str());
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
    error(nullptr, "Failed to lookup target: " + errorStr);
    return 1;
  }
  auto CPU = "generic";
  auto Features = "";

  llvm::TargetOptions opt;
  auto targetMachine = Target->createTargetMachine(targetTriple, CPU, Features,
                                                   opt, llvm::Reloc::PIC_);
  m_module->setDataLayout(targetMachine->createDataLayout());
  m_module->setTargetTriple(targetTriple);

  std::error_code ec;
  llvm::raw_fd_ostream dest(filename, ec, llvm::sys::fs::OF_None);

  if (ec) {
    error(nullptr, "Could not open file: " + ec.message());
    return 1;
  }
  llvm::legacy::PassManager pass;
  auto FileType = llvm::CodeGenFileType::ObjectFile;

  if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, FileType)) {
    error(nullptr, "TargetMachine can't emit a file of this type");
    return 1;
  }

  pass.run(*m_module);
  dest.flush();

  return 0;
}

void IRGenerator::printIR(const std::string &filename) {
  if (m_module) {
    std::error_code ec;
    llvm::raw_fd_ostream out(filename, ec, llvm::sys::fs::OF_None);
    if (ec) {
      error(nullptr, "Could not open file: " + ec.message());
    }
    m_module->print(out, nullptr);
  }
}

void IRGenerator::error(ASTNodePtr node, const std::string &msg) {
  m_errors.push_back(std::make_pair(node, msg));
}

llvm::Type *IRGenerator::getLLVMType(const std::shared_ptr<Type> &type) {

  if (IS_INSTANCE(type, I32))
    return llvm::Type::getInt32Ty(m_context);
  if (IS_INSTANCE(type, I64))
    return llvm::Type::getInt64Ty(m_context);
  if (IS_INSTANCE(type, U8))
    return llvm::Type::getInt8Ty(m_context);
  if (IS_INSTANCE(type, U32))
    return llvm::Type::getInt32Ty(m_context);
  if (IS_INSTANCE(type, U64))
    return llvm::Type::getInt64Ty(m_context);
  if (IS_INSTANCE(type, F32))
    return llvm::Type::getFloatTy(m_context);
  if (IS_INSTANCE(type, F64))
    return llvm::Type::getDoubleTy(m_context);
  if (IS_INSTANCE(type, BOOL))
    return llvm::Type::getInt1Ty(m_context);
  if (IS_INSTANCE(type, Void))
    return llvm::Type::getVoidTy(m_context);
  if (IS_INSTANCE(type, USize))
    return llvm::Type::getInt64Ty(m_context); // Assuming 64-bit for USize
  if (IS_INSTANCE(type, StructType)) {
    auto structType = std::dynamic_pointer_cast<StructType>(type);
    auto it = m_structTypes.find(structType->name);
    if (it != m_structTypes.end()) {
      return it->second;
    } else {
      error(nullptr, "Unknown struct type: " + structType->name);
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
        llvm::Type::getInt1Ty(m_context)};
    auto st = llvm::StructType::get(m_context, elements);
    return st;
  }
  if (IS_INSTANCE(type, EnumType)) {
    return getLLVMType(std::dynamic_pointer_cast<EnumType>(type)->base_type);
  }
  if (!type) {
    error(nullptr, "Type is null");
    return nullptr;
  } else {
    error(nullptr, "Unsupported type: " + type->str());
  }
  return nullptr;
}

llvm::Value *
IRGenerator::generateAddress(const std::shared_ptr<Expression> &expr) {
  if (IS_INSTANCE(expr, VarAccess)) {
    auto var = std::dynamic_pointer_cast<VarAccess>(expr);
    return CUR_SCOPE.get(var->name)->value;
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
  error(expr, "Expression is not an lvalue: " + expr->str());
  return nullptr;
}

llvm::Value *
IRGenerator::generateExpression(const std::shared_ptr<Expression> &expr,
                                bool loadValue) {
  if (IS_INSTANCE(expr, BinaryOperation)) {
    auto binOp = std::dynamic_pointer_cast<BinaryOperation>(expr);
    return generateBinaryOp(binOp->left, binOp->right, binOp->op, loadValue);
  } else if (IS_INSTANCE(expr, UnaryOperation)) {
    auto unOp = std::dynamic_pointer_cast<UnaryOperation>(expr);
    return generateUnaryOp(unOp->operand, unOp->op, loadValue);
  } else if (IS_INSTANCE(expr, Literal)) {
    return generateLiteral(std::dynamic_pointer_cast<Literal>(expr), loadValue);
  } else if (IS_INSTANCE(expr, VarAccess)) {
    return generateVarAccess(std::dynamic_pointer_cast<VarAccess>(expr),
                             loadValue);
  } else if (IS_INSTANCE(expr, EnumAccess)) {
    return generateEnumAccess(std::dynamic_pointer_cast<EnumAccess>(expr),
                              loadValue);
  } else if (IS_INSTANCE(expr, FuncCall)) {
    return generateFuncCall(std::dynamic_pointer_cast<FuncCall>(expr),
                            loadValue);
  } else if (IS_INSTANCE(expr, FieldAccess)) {
    return generateFieldAccess(std::dynamic_pointer_cast<FieldAccess>(expr),
                               loadValue);
  } else if (IS_INSTANCE(expr, StructInitializer)) {
    return generateStructInitializer(
        std::dynamic_pointer_cast<StructInitializer>(expr), loadValue);
  } else if (IS_INSTANCE(expr, OffsetAccess)) {
    return generateOffsetAccess(std::dynamic_pointer_cast<OffsetAccess>(expr),
                                loadValue);
  } else if (IS_INSTANCE(expr, TypeCast)) {
    return generateCast(std::dynamic_pointer_cast<TypeCast>(expr), loadValue);
  } else if (IS_INSTANCE(expr, Dereference)) {
    auto deref = std::dynamic_pointer_cast<Dereference>(expr);
    llvm::Value *ptr = generateExpression(deref->pointer, true);
    if (loadValue) {
      return m_builder.CreateLoad(getLLVMType(deref->inferred_type), ptr,
                                  "derefloadtmp");
    } else {
      return ptr;
    }
  } else if (IS_INSTANCE(expr, MethodCall)) {
    return generateMethodCall(std::dynamic_pointer_cast<MethodCall>(expr),
                              loadValue);
  }
  error(expr, "Unknown expression type");
  return nullptr;
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
    error(typeCast, "Invalid cast operation");
    return nullptr;
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
    return m_builder.CreateSIToFP(val, destType, "sitofptmp");
  } else if (srcType->isFloatingPointTy() && destType->isIntegerTy()) {
    return m_builder.CreateFPToSI(val, destType, "fptositmp");
  } else if (srcType->isPointerTy() && destType->isPointerTy()) {
    return m_builder.CreateBitCast(val, destType, "ptrcasttmp");
  } else if (srcType->isPointerTy() && destType->isIntegerTy()) {
    return m_builder.CreatePtrToInt(val, destType, "ptrtointtmp");
  } else if (srcType->isIntegerTy() && destType->isPointerTy()) {
    return m_builder.CreateIntToPtr(val, destType, "inttoptrtmp");
  } else {
    error(typeCast, "Unsupported cast from " + llvmTypeToString(srcType) +
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

    error(stmt, "Unknown statement type: " + stmt->str());
  }
  return nullptr;
}

llvm::Function *IRGenerator::generateFunction(
    const std::shared_ptr<FunctionDeclaration> &func) {

  llvm::FunctionType *fType =
      llvm::cast<llvm::FunctionType>(this->getLLVMType(func->type));
  llvm::Function *function = llvm::Function::Create(
      fType, llvm::Function::ExternalLinkage, func->name, m_module.get());
  CUR_SCOPE.set(func->name, function, fType, func->type);
  // Set names for all arguments
  unsigned idx = 0;
  for (auto &arg : function->args()) {
    arg.setName(func->param_names[idx++]);
  }

  if (!func->body)
    return function;

  m_scopeStack.push_back(Scope(CUR_SCOPE));

  llvm::BasicBlock *entry =
      llvm::BasicBlock::Create(m_context, "entry", function);
  m_builder.SetInsertPoint(entry);
  // Allocate space for arguments and store them
  idx = 0;
  for (auto &arg : function->args()) {
    llvm::AllocaInst *alloca =
        m_builder.CreateAlloca(arg.getType(), nullptr, arg.getName() + ".addr");
    m_builder.CreateStore(&arg, alloca);
    CUR_SCOPE.set(arg.getName().str(), alloca, arg.getType(),
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
      error(func, "Non-void function missing return: " + func->name);
    }
  }
  m_error_union_return_type = nullptr;
  // Validate the generated code, checking for consistency.
  llvm::verifyFunction(*function);
  return function;
}
void IRGenerator::generateVariableDeclaration(
    const std::shared_ptr<VariableDeclaration> &varDecl) {

  llvm::Type *varType = getLLVMType(varDecl->var_type);
  llvm::Value *alloca = m_builder.CreateAlloca(varType, nullptr, varDecl->name);
  CUR_SCOPE.set(varDecl->name, alloca, varType, varDecl->var_type);
  if (varDecl->initializer) {
    llvm::Value *initVal = nullptr;
    if (IS_INSTANCE(varDecl->initializer, Expression)) {
      initVal = generateExpression(
          std::dynamic_pointer_cast<Expression>(varDecl->initializer));
    } else {
      error(varDecl, "Unsupported initializer type");
    }
    m_builder.CreateStore(initVal, alloca);
  }
}

void IRGenerator::generateStructMethods(
    const std::shared_ptr<StructDeclaration> &structDecl) {
  for (std::unordered_map<std::string, std::shared_ptr<FunctionDeclaration>>::iterator iter = structDecl->methods.begin();
       iter != structDecl->methods.end(); ++iter) {
    auto method = iter->second;
    std::string mangledName = "__" + structDecl->name + "_" + method->name;
    method->name = mangledName;
    generateFunction(method);
  }
}

void IRGenerator::generateStructDeclaration(
    const std::shared_ptr<StructDeclaration> &structDecl) {

  llvm::StructType *llvmStruct =
      llvm::StructType::create(m_context, structDecl->name);

  m_structTypes[structDecl->name] = llvmStruct;

  std::vector<llvm::Type *> fieldTypes;
  for (const auto &field : structDecl->fields) {
    llvm::Type *ty = getLLVMType(field.second);
    fieldTypes.push_back(ty);
  }

  llvmStruct->setBody(fieldTypes, /*packed=*/false);

  generateStructMethods(structDecl);
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
    error(nullptr, "Invalid binary operation: null operand expression");
    return nullptr;
  }

  llvm::Value *l = generateExpression(left);
  llvm::Value *r = generateExpression(right);

  if (!l || !r) {
    error(nullptr, "Failed to generate LLVM value for binary operands");
    return nullptr;
  }

  llvm::Type *ty = l->getType();
  if (!ty) {
    error(left, "Left operand of binary op has no type");
    return nullptr;
  }

  // Handle assignment separately
  if (op == "=") {
    llvm::Value *addr = generateAddress(left);
    if (!addr) {
      error(left, "Left operand of assignment is not an lvalue");
      return nullptr;
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
    ty = llvm::Type::getInt64Ty(m_context);
    l = m_builder.CreatePtrToInt(l, ty, "ptrtoint_lhs");
    r = m_builder.CreatePtrToInt(r, ty, "ptrtoint_rhs");
    if (!l || !r) {
      error(nullptr, "Failed to convert pointer operands to integer");
      return nullptr;
    }
  }

  // Ensure both operands have the same type
  if (l->getType() != r->getType()) {
    error(nullptr, "Type mismatch in binary op: lhs=" +
                       llvmTypeToString(l->getType()) +
                       " rhs=" + llvmTypeToString(r->getType()));
    return nullptr;
  }

  // Check operator support
  auto it = ops.find(op);
  if (it == ops.end()) {
    error(right, "Unsupported binary operator: " + op);
    return nullptr;
  }

  const OpInfo &info = it->second;

  if (ty->isIntegerTy())
    return info.intOp(l, r);
  else if (ty->isFloatingPointTy())
    return info.floatOp(l, r);
  else {
    error(right,
          "Unsupported type for binary operator: " + llvmTypeToString(ty));
    return nullptr;
  }
}

llvm::Value *
IRGenerator::generateUnaryOp(const std::shared_ptr<Expression> &operand,
                             std::string op, bool loadValue) {
  auto val = generateExpression(operand);
  if (op == "-") {
    return m_builder.CreateNeg(val, "negtmp");
  } else if (op == "+") {
    return val; // Unary plus is a no-op
  } else if (op == "!") {
    return m_builder.CreateNot(val, "nottmp");
  } else if (op == "&") {
    return generateAddress(operand);
  }
  error(operand, "Unsupported unary operator: " + op);
  return nullptr;
}

llvm::Value *IRGenerator::generateLiteral(const std::shared_ptr<Literal> &lit,
                                          bool loadValue) {
  if (IS_INSTANCE(lit->lit_type, I32)) {
    return llvm::ConstantInt::get(m_context,
                                  llvm::APInt(32, std::get<int>(lit->value)));
  } else if (IS_INSTANCE(lit->lit_type, I64)) {
    return llvm::ConstantInt::get(m_context,
                                  llvm::APInt(64, std::get<int>(lit->value)));
  } else if (IS_INSTANCE(lit->lit_type, U8)) {
    return llvm::ConstantInt::get(m_context,
                                  llvm::APInt(8, std::get<int>(lit->value)));
  } else if (IS_INSTANCE(lit->lit_type, U32)) {
    return llvm::ConstantInt::get(m_context,
                                  llvm::APInt(32, std::get<int>(lit->value)));
  } else if (IS_INSTANCE(lit->lit_type, U64)) {
    return llvm::ConstantInt::get(m_context,
                                  llvm::APInt(64, std::get<int>(lit->value)));
  } else if (IS_INSTANCE(lit->lit_type, F32)) {
    return llvm::ConstantFP::get(m_builder.getFloatTy(),
                                 std::get<float>(lit->value));
  } else if (IS_INSTANCE(lit->lit_type, F64)) {
    return llvm::ConstantFP::get(m_builder.getDoubleTy(),
                                 std::get<float>(lit->value));
  } else if (IS_INSTANCE(lit->lit_type, BOOL)) {
    return llvm::ConstantInt::get(
        m_context, llvm::APInt(1, std::get<bool>(lit->value) ? 1 : 0));
  } else if (IS_INSTANCE(lit->lit_type, Void)) {
    return nullptr; // Void literals don't have a value
  } else if (IS_INSTANCE(lit->lit_type, USize)) {
    return llvm::ConstantInt::get(
        m_context,
        llvm::APInt(64,
                    std::get<int>(lit->value))); // Assuming 64-bit for USize
  } else if (IS_INSTANCE(lit->lit_type, PointerType)) {
    if (std::holds_alternative<int>(lit->value)) {
      int intVal = std::get<int>(lit->value);
      if (intVal == 0) {
        // Null pointer literal
        return llvm::ConstantPointerNull::get(
            llvm::cast<llvm::PointerType>(getLLVMType(lit->lit_type)));
      } else {
        error(lit, "Only null (0) is allowed as integer pointer literal");
      }
    } else if (std::holds_alternative<std::string>(lit->value)) {
      // String literal → create global string
      auto strVal = std::get<std::string>(lit->value);
      auto strName = "g" + std::to_string(globalVals++);
      auto strType = llvm::ArrayType::get(llvm::Type::getInt8Ty(m_context),
                                          strVal.size() + 1);
      auto strConstant =
          llvm::ConstantDataArray::getString(m_context, strVal, true);
      auto globalStr = new llvm::GlobalVariable(
          *m_module, strType, true, llvm::GlobalValue::PrivateLinkage,
          strConstant, strName);
      llvm::Constant *zero =
          llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_context), 0);
      llvm::Constant *indices[] = {zero, zero};
      return llvm::ConstantExpr::getGetElementPtr(strType, globalStr, indices,
                                                  true);
    } else {
      error(lit, "Unsupported pointer literal value type");
    }
  } else if (IS_INSTANCE(lit->lit_type, NullType)) {
    return llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(
        getLLVMType(std::make_shared<PointerType>(std::make_shared<Void>()))));
  } else {
    error(lit, "Unsupported literal type: " + lit->lit_type->str());
  }
  return nullptr;
}

llvm::Value *
IRGenerator::generateVarAccess(const std::shared_ptr<VarAccess> &varAccess,
                               bool loadValue) {
  auto *sym = CUR_SCOPE.get(varAccess->name);
  if (!sym) {
    error(varAccess, "Unknown variable name: " + varAccess->name);
    return nullptr;
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
    error(enumAccess, "Unknown enum type: " + enumAccess->enum_name);
    return nullptr;
  }
  auto enumType = it->second;
  auto valIt = enumType->variant_map.find(enumAccess->variant);
  if (valIt == enumType->variant_map.end()) {
    error(enumAccess, "Unknown enum variant: " + enumAccess->variant);
    return nullptr;
  }
  std::shared_ptr<Literal> literal = valIt->second;
  // Assuming enums are represented as i32
  return generateLiteral(literal, loadValue);
}

llvm::Value *IRGenerator::generateMethodCall(const std::shared_ptr<MethodCall> &methodCall, bool loadValue) {
  auto structAccess = methodCall->object;
  llvm::Value *objPtr = generateAddress(std::static_pointer_cast<Expression>(structAccess));
  if (!objPtr) {
    error(structAccess, "Failed to generate address for method call object");
    return nullptr;
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
      error(structAccess, "Method call on non-pointer, non-struct type: " + structAccess->inferred_type->str());
      return nullptr;
    }
  }
  if (!structType) {
    error(structAccess,
          "Failed to determine struct type for method call");
    return nullptr;
  }
  // Find the struct type
  auto accessee = m_structTypes.find(structType->name);
  if (accessee == m_structTypes.end()) {
    error(structAccess, "Unknown struct type in method call: " + structType->name);
    return nullptr;
  }
  // Mangle method name
  std::string mangledName = "__" + structType->name + "_" + methodCall->method;
  // Prepare arguments
  std::vector<llvm::Value *> argsV;
  argsV.push_back(objPtr); // 'this' pointer
  for (const auto &arg : methodCall->args) {
    argsV.push_back(generateExpression(arg));
  }
  llvm::Function *calleeValue = m_module->getFunction(mangledName);
  if (!calleeValue) {
    error(methodCall, "Unknown method: " + mangledName);
    return nullptr;
  }
  return m_builder.CreateCall(calleeValue, argsV, "calltmp");
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
    error(funcCall, "Failed to generate callee for function call");
    return nullptr;
  }

  llvm::Type *calleeType = nullptr;

  if (auto *func = llvm::dyn_cast<llvm::Function>(calleeValue)) {
    // Direct call
    return m_builder.CreateCall(func, argsV, "calltmp");
  } else if (calleeValue->getType()->isPointerTy()) {
    // Indirect call via function pointer
    auto it = funcCall->func->inferred_type;
    if (!IS_INSTANCE(it, PointerType)) {
      error(funcCall,
            "Callee is a pointer, but inferred type is not a pointer");
      return nullptr;
    }
    auto ptrType = std::dynamic_pointer_cast<PointerType>(it);
    if (!IS_INSTANCE(ptrType->base, FunctionType)) {
      error(funcCall, "Callee is a pointer, but not to a function type");
      return nullptr;
    }
    auto funcType = std::dynamic_pointer_cast<FunctionType>(ptrType->base);
    calleeType = getLLVMType(funcType);

    llvm::Type *elemTy = calleeType;

    if (!llvm::isa<llvm::FunctionType>(elemTy)) {
      error(funcCall, "Callee is a pointer, but not to a function type");
      return nullptr;
    }

    auto *funcTy = llvm::cast<llvm::FunctionType>(elemTy);
    // Handle varargs: promote i1, i8, i16 to i32 and f32 to f64
    std::cout << "Function call to " << funcCall->func->str() << " with " << argsV.size() << " args\n";
    return m_builder.CreateCall(funcTy, calleeValue, argsV, "calltmp");
  } else {
    error(funcCall, "Callee is not a function or function pointer: " +
                        funcCall->func->str());
    return nullptr;
  }
}

llvm::Value *IRGenerator::generateOffsetAccess(
    const std::shared_ptr<OffsetAccess> &offsetAccess, bool loadValue) {

  llvm::Value *basePtr = generateExpression(
      std::static_pointer_cast<Expression>(offsetAccess->base));
  if (!basePtr) {
    error(offsetAccess, "Failed to generate base for offset access");
    return nullptr;
  }
  llvm::Value *index = generateExpression(offsetAccess->index);
  if (!index) {
    error(offsetAccess->index, "Failed to generate index for offset access");
    return nullptr;
  }
  llvm::Type *resultType = getLLVMType(offsetAccess->inferred_type);
  if (!resultType) {
    error(offsetAccess, "Unknown type for offset access: " +
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
    error(nullptr, "Invalid field access: missing base expression");
    return nullptr;
  }

  if (!fieldAccess->base->inferred_type) {
    error(fieldAccess->base, "Base expression of field access has no inferred type");
    return nullptr;
  }

  llvm::Value *basePtr =
      generateAddress(std::static_pointer_cast<Expression>(fieldAccess->base));
  if (!basePtr) {
    error(fieldAccess->base, "Failed to generate address for field base");
    return nullptr;
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
      llvm::Type *bool_type = llvm::Type::getInt1Ty(m_context);
      auto gep = m_builder.CreateStructGEP(euStructType, basePtr, 2, "error_union_is_err_access");
      return loadValue
                 ? m_builder.CreateLoad(bool_type, gep, "load_error_union_is_err")
                 : gep;

    } else {
      error(fieldAccess, "Unknown field on error union: " + fieldAccess->field);
      return nullptr;
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
        error(fieldAccess->base,
              "Pointer type in field access has no base type");
        return nullptr;
      }

      if (structType) {
        // Adjust basePtr to dereference the pointer
        basePtr = m_builder.CreateLoad(
            llvm::PointerType::getUnqual(getLLVMType(ptrType->base)), basePtr,
            "load_ptr_for_field");
        if (!basePtr) {
          error(fieldAccess->base,
                "Failed to load pointer value for field access");
          return nullptr;
        }
      }
    }

    if (!structType) {
      error(fieldAccess->base,
            "Field access on non-struct type: " +
                fieldAccess->base->inferred_type->str());
      return nullptr;
    }
  }

  // Confirm struct type is registered in LLVM mapping
  auto accessee = m_structTypes.find(structType->name);
  if (accessee == m_structTypes.end() || !accessee->second) {
    error(fieldAccess->base,
          "Unknown or unregistered struct type in field access: " +
              structType->name);
    return nullptr;
  }

  llvm::StructType *llvmStruct = accessee->second;

  // Validate field existence
  const auto &fieldName = fieldAccess->field;
  int fieldIndex = structType->getFieldIndex(fieldName);
  if (fieldIndex < 0) {
    error(fieldAccess->base,
          "Struct '" + structType->name +
              "' has no field named '" + fieldName + "'");
    return nullptr;
  }

  // Build GEP safely
  llvm::Value *gep = m_builder.CreateGEP(
      llvmStruct, basePtr,
      {llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_context), 0),
       llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_context), fieldIndex)},
      "field_access");

  if (!gep) {
    error(fieldAccess->base, "Failed to create GEP for field '" + fieldName +
                                 "' in struct '" + structType->name + "'");
    return nullptr;
  }

  if (loadValue) {
    if (!fieldAccess->inferred_type) {
      error(fieldAccess, "Field '" + fieldName +
                             "' has no inferred type for load");
      return nullptr;
    }
    return m_builder.CreateLoad(getLLVMType(fieldAccess->inferred_type), gep,
                                "load_field");
  }

  return gep;
}

llvm::Value *IRGenerator::generateStructInitializer(
    const std::shared_ptr<StructInitializer> &structInit, bool loadValue) {
  auto it = m_structTypes.find(structInit->struct_type->name);
  if (it == m_structTypes.end()) {
    error(structInit, "Unknown struct type: " + structInit->struct_type->name);
    return nullptr;
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
        {llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_context), 0),
         llvm::ConstantInt::get(llvm::Type::getInt32Ty(m_context), fieldIndex)},
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
    lastValue = generateStatement(stmt);
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
      llvm::BasicBlock::Create(m_context, "then", theFunction);
  llvm::BasicBlock *elseBB =
      llvm::BasicBlock::Create(m_context, "else", theFunction);
  llvm::BasicBlock *mergeBB =
      llvm::BasicBlock::Create(m_context, "ifcont", theFunction);
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
      llvm::BasicBlock::Create(m_context, "while.cond", theFunction);
  auto body_block =
      llvm::BasicBlock::Create(m_context, "while.body", theFunction);
  auto after_block =
      llvm::BasicBlock::Create(m_context, "while.end", theFunction);
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
      llvm::BasicBlock::Create(m_context, "for.cond", theFunction);
  auto body_block =
      llvm::BasicBlock::Create(m_context, "for.body", theFunction);
  auto after_block =
      llvm::BasicBlock::Create(m_context, "for.end", theFunction);
  m_builder.CreateBr(cond_block);
  m_builder.SetInsertPoint(cond_block);
  llvm::Value *condition = nullptr;
  if (forStmt->condition) {
    condition = generateExpression(forStmt->condition);
    condition = m_builder.CreateICmpNE(
        condition, llvm::ConstantInt::get(condition->getType(), 0), "forcond");
  } else {
    condition = llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_context), 1);
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
      auto withOk = m_builder.CreateInsertValue(undefVal, okVal, 0);

      auto withErr = m_builder.CreateInsertValue(withOk, errVal, 1);
      withTag = m_builder.CreateInsertValue(withErr,
                                            llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_context), 1),
                                            2);

    } else {
      okVal = retVal;
      errVal = llvm::Constant::getNullValue(getLLVMType(m_error_union_return_type->errorType));

      // Create the struct for ok return
      // {okType, errType, i1 (is_error, should be false)}
      auto withOk = m_builder.CreateInsertValue(undefVal, okVal, 0);

      auto withErr = m_builder.CreateInsertValue(withOk, errVal, 1);
      withTag = m_builder.CreateInsertValue(withErr,
                                            llvm::ConstantInt::get(llvm::Type::getInt1Ty(m_context), 0),
                                            2);
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
