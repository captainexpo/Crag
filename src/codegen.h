#ifndef IR_H
#define IR_H

#include "ast.h"
#include "lexer.h"
#include "module_resolver.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include <memory>

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

  void dump() {
    for (const auto &pair : m_table) {
      llvm::outs() << "Symbol: " << pair.first << "\n";
    }
    if (m_parent) {
      llvm::outs() << "Parent Scope:\n";
      m_parent->dump();
    }
  }

private:
  std::map<std::string, Symbol> m_table;
  std::shared_ptr<Scope> m_parent;
};

// Map operator string to function
struct OpInfo {
  std::function<llvm::Value *(llvm::Value *, llvm::Value *)> intOp;
  std::function<llvm::Value *(llvm::Value *, llvm::Value *)> floatOp;
};

class IRGenerator {
public:
  IRGenerator(std::string module_name, llvm::LLVMContext &ctx, ModuleResolver moduleResolver)
      : context(ctx), m_builder(ctx), m_module_resolver(moduleResolver) {
    // Initialize the global scope
    m_llvm_module = std::make_unique<llvm::Module>(module_name, ctx);
    m_scopeStack.push_back(Scope(nullptr));
  }
  void generate(std::shared_ptr<Module> module);
  void printIR(const std::string &filename);
  int outputObjFile(const std::string &filename);

  const std::vector<std::pair<ASTNodePtr, std::string>> &errors() const {
    return m_errors;
  }
  bool ok() const { return m_errors.empty(); }
  std::unique_ptr<llvm::Module> takeModule() { return std::move(m_llvm_module); }

  void prepareForNewModule() {
    // Set insert point outside of any function
    m_builder.ClearInsertionPoint();
    auto global_scope = m_scopeStack.front();
    m_scopeStack.clear();
    m_scopeStack.push_back(global_scope);
  }

private:
  std::vector<std::pair<ASTNodePtr, std::string>> m_errors;

  llvm::LLVMContext &context;
  std::unique_ptr<llvm::Module> m_llvm_module;
  llvm::IRBuilder<> m_builder;

  ModuleResolver m_module_resolver;

  std::shared_ptr<Module> m_current_module = nullptr;

  std::map<std::string, llvm::Value *> m_namedValues;
  std::map<std::string, llvm::StructType *> m_structTypes;
  std::map<std::string, std::shared_ptr<EnumType>> m_enumTypes;
  std::vector<Scope> m_scopeStack;

  std::shared_ptr<ErrorUnionType> m_error_union_return_type = nullptr;

  inline std::string canonicalizeNonexternName(const std::string &name) const {
    // HACK: assume extern functions are globally unique and aren't overwritten in lower scopes
    // TODO: Don't do that :)
    if (name == "main")
      return name; // main function should not be namespaced
    if (m_current_module->externFunctions.find(name) !=
        m_current_module->externFunctions.end()) {
      return name;
    }
    return m_current_module->canonicalizeName(name);
  }

  llvm::Type *getLLVMType(const std::shared_ptr<Type> &type);
  llvm::Value *generateExpression(const std::shared_ptr<Expression> &expr,
                                  bool loadValue = true);
  llvm::Value *generateCast(const std::shared_ptr<TypeCast> &typeCast,
                            bool loadValue);
  llvm::Value *generateAddress(const std::shared_ptr<Expression> &expr);
  llvm::Value *generateStatement(const std::shared_ptr<Statement> &stmt);
  llvm::Function *generateFunction(const std::shared_ptr<FunctionDeclaration> &func);
  void generateVariableDeclaration(
      const std::shared_ptr<VariableDeclaration> &varDecl);
  void generateStructDeclaration(
      const std::shared_ptr<StructDeclaration> &structDecl);
  void generateEnumDeclaration(
      const std::shared_ptr<EnumDeclaration> &enumDecl);
  void generateStructMethods(const std::shared_ptr<StructDeclaration> &structDecl);
  llvm::Value *generateBinaryOp(const std::shared_ptr<Expression> &left,
                                const std::shared_ptr<Expression> &right,
                                std::string op, bool loadValue = true);
  llvm::Value *generateUnaryOp(const std::shared_ptr<Expression> &operand,
                               std::string op, bool loadValue = true);
  llvm::Value *generateLiteral(const std::shared_ptr<Literal> &lit,
                               bool loadValue = true);
  llvm::Value *generateVarAccess(const std::shared_ptr<VarAccess> &varAccess,
                                 bool loadValue = true);
  llvm::Value *generateEnumAccess(const std::shared_ptr<EnumAccess> &enumAccess,
                                  bool loadValue = true);
  llvm::Value *generateFuncCall(const std::shared_ptr<FuncCall> &funcCall,
                                bool loadValue = true);
  llvm::Value *generateMethodCall(const std::shared_ptr<MethodCall> &methodCall,
                                  bool loadValue = true);
  llvm::Value *generateFieldAccess(const std::shared_ptr<FieldAccess> &fieldAccess, bool loadValue = true);
  llvm::Value *generateOffsetAccess(const std::shared_ptr<OffsetAccess> &offsetAccess,
                                    bool loadValue = true);
  llvm::Value *generateModuleAccess(const std::shared_ptr<ModuleAccess> &moduleAccess,
                                    bool loadValue = true);
  llvm::Value *generateStructInitializer(
      const std::shared_ptr<StructInitializer> &structInit,
      bool loadValue = true);
  llvm::Value *generateBlock(const std::shared_ptr<Block> &blockNode);
  llvm::Value *generateIfStatement(const std::shared_ptr<IfStatement> &ifStmt);
  llvm::Value *generateWhileStatement(const std::shared_ptr<WhileStatement> &whileStmt);
  llvm::Value *generateForStatement(const std::shared_ptr<ForStatement> &forStmt);
  llvm::Value *generateReturnStatement(const std::shared_ptr<ReturnStatement> &retStmt);
  llvm::Value *generateExpressionStatement(
      const std::shared_ptr<ExpressionStatement> &exprStmt);
};
#endif
