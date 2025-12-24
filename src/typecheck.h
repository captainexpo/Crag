#pragma once
#ifndef TYPECHECK_H
#define TYPECHECK_H

#include "ast.h"
#include "const_eval.h"
#include "module_resolver.h"
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

class TypeCheckError : public std::runtime_error {
  public:
    TypeCheckError(ASTNodePtr node, const std::string &msg)
        : std::runtime_error(msg), node(std::move(node)) {}

    ASTNodePtr node;
};

struct CastResult {
    std::shared_ptr<Type> type;
    ExprPtr left;
    ExprPtr right;
};

bool canExplicitCast(const std::shared_ptr<Type> &from,
                     const std::shared_ptr<Type> &to);
std::shared_ptr<Type> getCastType(const std::shared_ptr<Type> &from,
                                  const std::shared_ptr<Type> &to);
class TypeChecker {
  public:
    TypeChecker();

    // Entry point
    void check(std::shared_ptr<Module> module);

    // Errors collected during checking
    const std::vector<std::pair<ASTNodePtr, std::string>> &errors() const {
        return m_errors;
    }
    bool ok() const { return m_errors.empty(); }

  private:
    std::shared_ptr<Module> current_module;

    std::vector<std::pair<ASTNodePtr, std::string>> m_errors; // Collected errors

    ConstEvaluator m_const_eval; // For constant expression evaluation

    // Scope / symbol table: stack of name -> Type
    std::vector<std::unordered_map<std::string, std::shared_ptr<Type>>> m_scopes;

    // Struct/type registry for named structs and functions TODO: replace them all with aliases
    std::unordered_map<std::string, std::shared_ptr<StructType>> m_structs;
    std::unordered_map<std::string, std::shared_ptr<UnionType>> m_unions;
    std::unordered_map<std::string, std::shared_ptr<EnumType>> m_enums;

    std::unordered_map<std::string, std::shared_ptr<Type>> m_type_aliases;

    std::unordered_map<std::string, std::shared_ptr<Declaration>> m_templates;
    
    std::unordered_map<
        std::string,
        std::vector<std::pair<std::string, std::shared_ptr<FunctionDeclaration>>>>
        m_struct_methods;
    std::unordered_map<std::string, std::shared_ptr<FunctionType>> m_functions;

    std::shared_ptr<Type> m_expected_return_type; // Current function return type

    std::vector<std::shared_ptr<Type>> m_expected_types; // Stack of expected types
    std::unordered_map<std::string, std::shared_ptr<TypeChecker>> m_imported_module_checkers;

    // Scope helpers
    void pushScope();
    void popScope();
    bool insertSymbol(const std::string &name, std::shared_ptr<Type> t);
    std::optional<std::shared_ptr<Type>> lookupSymbol(const std::string &name) const;

    // Type helpers

    std::string typeName(const std::shared_ptr<Type> &t) const;
    std::shared_ptr<Type> resolveType(const std::shared_ptr<Type> &t);

    bool canImplicitCast(const std::shared_ptr<Type> &from, const std::shared_ptr<Type> &to);

    void checkNode(const std::shared_ptr<ASTNode> &node);
    void checkStatement(const std::shared_ptr<Statement> &stmt);
    std::shared_ptr<Type> inferExpression(const std::shared_ptr<Expression> &expr);

    // Declarations
    void checkFunctionDeclaration(const std::shared_ptr<FunctionDeclaration> &fn);
    void checkVariableDeclaration(const std::shared_ptr<VariableDeclaration> &var);
    void checkStructDeclaration(const std::shared_ptr<StructDeclaration> &st);
    void checkUnionDeclaration(const std::shared_ptr<UnionDeclaration> &ud);
    void checkEnumDeclaration(const std::shared_ptr<EnumDeclaration> &en);

    std::shared_ptr<Type> inferTypeCast(const std::shared_ptr<TypeCast> &tc);
    std::shared_ptr<Type> inferVarAccess(const std::shared_ptr<VarAccess> &v);
    std::shared_ptr<Type> inferDereference(const std::shared_ptr<Dereference> &d);
    std::shared_ptr<Type> inferLiteral(const std::shared_ptr<Literal> &lit);
    std::shared_ptr<Type> inferArrayLiteral(const std::shared_ptr<ArrayLiteral> &al);
    std::shared_ptr<Type> inferBinaryOp(const std::shared_ptr<BinaryOperation> &bin);
    std::shared_ptr<Type> inferUnaryOp(const std::shared_ptr<UnaryOperation> &un);
    std::shared_ptr<Type> inferFuncCall(const std::shared_ptr<FuncCall> &call);
    std::shared_ptr<Type> inferMethodCall(const std::shared_ptr<MethodCall> &mc);
    std::shared_ptr<Type> inferFieldAccess(const std::shared_ptr<FieldAccess> &fa);
    std::shared_ptr<Type> inferModuleAccess(const std::shared_ptr<ModuleAccess> &ma);
    std::shared_ptr<Type> inferErrorUnionFieldAccess(const std::shared_ptr<FieldAccess> &fa);
    std::shared_ptr<Type> inferArrayFieldAccess(const std::shared_ptr<FieldAccess> &fa);
    std::shared_ptr<Type> inferOffsetAccess(const std::shared_ptr<OffsetAccess> &oa);
    std::shared_ptr<Type> inferStructInit(const std::shared_ptr<StructInitializer> &init);
    std::shared_ptr<Type> inferTemplateInstantiation(const std::shared_ptr<TemplateInstantiation> &ti);

};

#endif // TYPECHECK_H
