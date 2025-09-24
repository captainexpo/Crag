#ifndef IR_H
#define IR_H

#include "ast.h"
#include "lexer.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"

class Scope {
public:
  Scope(std::shared_ptr<Scope> parent = nullptr) : m_parent(parent) {}

  struct Symbol {
    llvm::Value *value;
    llvm::Type *type;               // original type, needed for opaque pointers
    std::shared_ptr<Type> ast_type; // original AST type, needed for type info
  };

  void set(const std::string &name, llvm::Value *value, llvm::Type *type,
           std::shared_ptr<Type> ast_type) {
    m_table[name] = {value, type, ast_type};
  }

  Symbol *get(const std::string &name) {
    auto it = m_table.find(name);
    if (it != m_table.end())
      return &it->second;
    if (m_parent)
      return m_parent->get(name);
    return nullptr;
  }

private:
  std::map<std::string, Symbol> m_table;
  std::shared_ptr<Scope> m_parent;
};

class IRGenerator {
public:
  IRGenerator(std::string module_name);
  void generate(const std::shared_ptr<Program> &node);
  void printIR(const std::string &filename);
  int outputObjFile(const std::string &filename);

private:
  llvm::LLVMContext m_context;
  std::unique_ptr<llvm::Module> m_module;
  llvm::IRBuilder<> m_builder;

  std::map<std::string, llvm::Value *> m_namedValues;
  std::map<std::string, llvm::StructType *> m_structTypes;

  std::vector<Scope> m_scopeStack;

  void throwError(const std::string &msg);
  llvm::Type *getLLVMType(const std::shared_ptr<Type> &type);
  llvm::Value *generateExpression(const std::shared_ptr<Expression> &expr,
                                  bool loadValue = true);
  llvm::Value *generateCast(const std::shared_ptr<TypeCast> &typeCast,
                            bool loadValue);
  llvm::Value *generateAddress(const std::shared_ptr<Expression> &expr);
  llvm::Value *generateStatement(const std::shared_ptr<Statement> &stmt);
  llvm::Function *
  generateFunction(const std::shared_ptr<FunctionDeclaration> &func);
  void generateVariableDeclaration(
      const std::shared_ptr<VariableDeclaration> &varDecl);
  void generateStructDeclaration(
      const std::shared_ptr<StructDeclaration> &structDecl);
  llvm::Value *generateBinaryOp(const std::shared_ptr<Expression> &left,
                                const std::shared_ptr<Expression> &right,
                                std::string op, bool loadValue = true);
  llvm::Value *generateUnaryOp(const std::shared_ptr<Expression> &operand,
                               std::string op, bool loadValue = true);
  llvm::Value *generateLiteral(const std::shared_ptr<Literal> &lit,
                               bool loadValue = true);
  llvm::Value *generateVarAccess(const std::shared_ptr<VarAccess> &varAccess,
                                 bool loadValue = true);
  llvm::Value *generateFuncCall(const std::shared_ptr<FuncCall> &funcCall,
                                bool loadValue = true);
  llvm::Value *
  generateFieldAccess(const std::shared_ptr<FieldAccess> &fieldAccess,
                      bool loadValue = true);
  llvm::Value *
  generateOffsetAccess(const std::shared_ptr<OffsetAccess> &offsetAccess,
                       bool loadValue = true);
  llvm::Value *generateStructInitializer(
      const std::shared_ptr<StructInitializer> &structInit,
      bool loadValue = true);
  llvm::Value *generateBlock(const std::shared_ptr<Block> &blockNode);
  llvm::Value *generateIfStatement(const std::shared_ptr<IfStatement> &ifStmt);
  llvm::Value *
  generateWhileStatement(const std::shared_ptr<WhileStatement> &whileStmt);
  llvm::Value *
  generateForStatement(const std::shared_ptr<ForStatement> &forStmt);
  llvm::Value *
  generateReturnStatement(const std::shared_ptr<ReturnStatement> &retStmt);
  llvm::Value *generateExpressionStatement(
      const std::shared_ptr<ExpressionStatement> &exprStmt);
};
#endif
