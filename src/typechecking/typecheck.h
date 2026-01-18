#pragma once
#ifndef TYPECHECK_H
#define TYPECHECK_H

#include "../ast/ast.h"
#include "../const_eval.h"
#include "../module_resolver.h"
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

typedef struct TypeScope {
    std::unordered_map<std::string, std::shared_ptr<Type>> symbols;
    std::unordered_map<std::string, ASTNodePtr> symbol_declarations;

    std::shared_ptr<Type> find(const std::string &name) const {
        auto it = symbols.find(name);
        if (it != symbols.end()) {
            return it->second;
        }
        return nullptr;
    }
} TypeScope;

class TypeChecker {
  public:
    TypeChecker();

    // Entry point
    void check(std::shared_ptr<Module> module);
    ASTNodePtr lookupConstVariableInModulePath(const std::vector<std::string> &module_path,
                                               const std::string &var_name);

    // Errors collected during checking
    const std::vector<std::pair<ASTNodePtr, std::string>> &errors() const {
        return m_errors;
    }
    bool ok() const { return m_errors.empty(); }

    std::unordered_map<std::string, std::shared_ptr<TypeChecker>> imported_module_checkers;
  private:
    std::shared_ptr<Module> current_module;

    std::vector<std::pair<ASTNodePtr, std::string>> m_errors; // Collected errors

    ConstEvaluator m_const_eval;

    // Scope / symbol table: stack of name -> Type
    std::vector<TypeScope> m_scopes;

    // Struct/type registry for named structs and functions TODO: replace them all with aliases
    std::unordered_map<std::string, std::shared_ptr<StructType>> m_structs;
    std::unordered_map<std::string, std::shared_ptr<UnionType>> m_unions;
    std::unordered_map<std::string, std::shared_ptr<EnumType>> m_enums;

    std::unordered_map<std::string, std::shared_ptr<Type>> m_type_aliases;

    std::unordered_map<std::string, std::shared_ptr<Declaration>> m_templates;

    std::unordered_map<std::string, std::shared_ptr<Type>> m_current_generic_types;

    std::unordered_map<
        std::string,
        std::vector<std::pair<std::string, std::shared_ptr<FunctionDeclaration>>>>
        m_struct_methods;
    std::unordered_map<std::string, std::shared_ptr<FunctionType>> m_functions;

    std::shared_ptr<Type> m_expected_return_type; // Current function return type


    // Scope helpers
    void pushScope();
    void popScope();
    bool insertSymbol(const std::string &name, std::shared_ptr<Type> t, ASTNodePtr decl);
    std::optional<std::shared_ptr<Type>> lookupSymbol(const std::string &name) const;

    // Type helpers

    std::string typeName(const std::shared_ptr<Type> &t) const;
    std::shared_ptr<Type> resolveType(const std::shared_ptr<ASTNode> &node, const std::shared_ptr<Type> &t);

    bool canImplicitCast(const std::shared_ptr<Type> &from, const std::shared_ptr<Type> &to);

    void checkNode(const std::shared_ptr<ASTNode> &node);
    void checkStatement(const std::shared_ptr<Statement> &stmt);
    std::shared_ptr<Type> inferExpression(std::shared_ptr<Expression> &expr,
                                          const std::shared_ptr<Type> &expected = nullptr);

    // Declarations
    void checkFunctionDeclaration(const std::shared_ptr<FunctionDeclaration> &fn);
    void checkVariableDeclaration(const std::shared_ptr<VariableDeclaration> &var);
    void checkStructDeclaration(const std::shared_ptr<StructDeclaration> &st);
    void checkUnionDeclaration(const std::shared_ptr<UnionDeclaration> &ud);
    void checkEnumDeclaration(const std::shared_ptr<EnumDeclaration> &en);

    std::shared_ptr<Type> inferTypeCast(const std::shared_ptr<TypeCast> &tc);
    std::shared_ptr<Type> inferVarAccess(const std::shared_ptr<VarAccess> &v);
    std::shared_ptr<Type> inferDereference(const std::shared_ptr<Dereference> &d);
    std::shared_ptr<Type> inferLiteral(const std::shared_ptr<Literal> &lit,
                                       const std::shared_ptr<Type> &expected = nullptr);
    std::shared_ptr<Type> inferArrayLiteral(const std::shared_ptr<ArrayLiteral> &al,
                                            const std::shared_ptr<Type> &expected = nullptr);
    std::shared_ptr<Type> inferBinaryOp(const std::shared_ptr<BinaryOperation> &bin,
                                        const std::shared_ptr<Type> &expected = nullptr);
    std::shared_ptr<Type> inferUnaryOp(const std::shared_ptr<UnaryOperation> &un);
    std::shared_ptr<Type> inferFuncCall(const std::shared_ptr<FuncCall> &call,
                                        const std::shared_ptr<Type> &expected = nullptr);
    std::shared_ptr<Type> inferMethodCall(const std::shared_ptr<MethodCall> &mc);
    std::shared_ptr<Type> inferFieldAccess(const std::shared_ptr<FieldAccess> &fa);
    std::shared_ptr<Type> inferModuleAccess(const std::shared_ptr<ModuleAccess> &ma);
    std::shared_ptr<Type> inferErrorUnionFieldAccess(const std::shared_ptr<FieldAccess> &fa);
    std::shared_ptr<Type> inferArrayFieldAccess(const std::shared_ptr<FieldAccess> &fa);
    std::shared_ptr<Type> inferOffsetAccess(const std::shared_ptr<OffsetAccess> &oa);
    std::shared_ptr<Type> inferStructInit(const std::shared_ptr<StructInitializer> &init,
                                          const std::shared_ptr<Type> &expected = nullptr);
    std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> inferTemplateInstantiation(const std::shared_ptr<TemplateInstantiation> &ti);
    void handleArrayLiteralAssignment(
        const std::string &name,
        const std::shared_ptr<ArrayLiteral> &arr_lit,
        const std::shared_ptr<ArrayType> &arr_type);

    std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> expandSizeOf(const std::shared_ptr<FuncCall> &call);
    std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> expandOffsetOf(const std::shared_ptr<FuncCall> &call);
    std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> expandAlignOf(const std::shared_ptr<FuncCall> &call);
};

void replaceGenericTypes(std::shared_ptr<ASTNode> node, const std::unordered_map<std::string, std::shared_ptr<Type>> &generic_map);

#endif // TYPECHECK_H
