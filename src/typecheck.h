#ifndef TYPECHECK_H
#define TYPECHECK_H

#include "ast.h"
#include <memory>
#include <optional>
#include <stack>
#include <string>
#include <unordered_map>
#include <vector>

class TypeChecker {
public:
  TypeChecker();

  // Entry point
  void check(const std::shared_ptr<Program> &node);

  // Errors collected during checking
  const std::vector<std::string> &errors() const { return m_errors; }
  bool ok() const { return m_errors.empty(); }

private:
  std::vector<std::string> m_errors; // Collected errors

  // Scope / symbol table: stack of name -> Type
  std::vector<std::unordered_map<std::string, std::shared_ptr<Type>>> m_scopes;

  // Struct/type registry for named structs and functions
  std::unordered_map<std::string, std::shared_ptr<StructType>> m_structs;
  std::unordered_map<std::string, std::shared_ptr<FunctionType>> m_functions;

  std::shared_ptr<Type> m_expected_return_type; // Current function return type

  // Error reporting
  void error(const std::string &msg);

  // Scope helpers
  void pushScope();
  void popScope();
  bool insertSymbol(const std::string &name, std::shared_ptr<Type> t);
  std::optional<std::shared_ptr<Type>>
  lookupSymbol(const std::string &name) const;

  // Type helpers
  bool typeEquals(const std::shared_ptr<Type> &a,
                  const std::shared_ptr<Type> &b) const;
  std::string typeName(const std::shared_ptr<Type> &t) const;
  std::shared_ptr<Type> resolveType(const std::shared_ptr<Type> &t) const;
  // Node-specific checking/inference
  void checkNode(const std::shared_ptr<ASTNode> &node);
  void checkStatement(const std::shared_ptr<Statement> &stmt);
  std::shared_ptr<Type>
  inferExpression(const std::shared_ptr<Expression> &expr);

  // Declarations
  void checkFunctionDeclaration(const std::shared_ptr<FunctionDeclaration> &fn);
  void
  checkVariableDeclaration(const std::shared_ptr<VariableDeclaration> &var);
  void checkStructDeclaration(const std::shared_ptr<StructDeclaration> &st);

  // Expression helpers (small set; extend as needed)
  std::shared_ptr<Type> inferTypeCast(const std::shared_ptr<TypeCast> &tc);
  std::shared_ptr<Type> inferVarAccess(const std::shared_ptr<VarAccess> &v);
  std::shared_ptr<Type> inferDereference(const std::shared_ptr<Dereference> &d);
  std::shared_ptr<Type> inferLiteral(const std::shared_ptr<Literal> &lit);
  std::shared_ptr<Type>
  inferBinaryOp(const std::shared_ptr<BinaryOperation> &bin);
  std::shared_ptr<Type> inferUnaryOp(const std::shared_ptr<UnaryOperation> &un);
  std::shared_ptr<Type> inferFuncCall(const std::shared_ptr<FuncCall> &call);
  std::shared_ptr<Type>
  inferFieldAccess(const std::shared_ptr<FieldAccess> &fa);
  std::shared_ptr<Type>
  inferOffsetAccess(const std::shared_ptr<OffsetAccess> &oa);
  std::shared_ptr<Type> inferCast(const std::shared_ptr<Cast> &c);
  std::shared_ptr<Type>
  inferStructInit(const std::shared_ptr<StructInitializer> &init);
};

#endif // TYPECHECK_H
