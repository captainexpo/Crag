#pragma once
#include "src/backend.h"
#ifndef TYPECHECK_H
#define TYPECHECK_H

#include "../ast/ast.h"
#include "../const_eval.h"
#include "../module_resolver.h"
#include "tables.h"
#include <memory>
#include <optional>
#include <string>
#include <unordered_map>
#include <vector>

#define DUMP_SYMBOL_TABLE(table)                                                                                    \
    std::cerr << "Symbol table contents:\n";                                                                        \
    for (int i = 0; i < table.entries().size(); ++i) {                                                              \
        const auto &entry = table.entries()[i];                                                                     \
        std::cerr << "  " << entry.name << " (" << i << "): " << (entry.type ? entry.type->str() : "null") << "\n"; \
    }
class TypeCheckError : public std::runtime_error {
  public:
    TypeCheckError(std::shared_ptr<Module> module, ASTNodePtr node, const std::string &msg)
        : std::runtime_error(msg), node(std::move(node)), module(std::move(module)) {}

    ASTNodePtr node;
    std::shared_ptr<Module> module;
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
    std::unordered_map<std::string, SymbolId> symbols;

    SymbolId find(const std::string &name) const {
        auto it = symbols.find(name);
        if (it == symbols.end())
            return INVALID_SYMBOL_ID;
        return it->second;
    }
    void dump() const {
        std::cerr << "TypeScope contents:\n";
        for (const auto &entry : symbols) {
            std::cerr << "  " << entry.first << ": " << entry.second << "\n";
        }
    }
} TypeScope;

typedef struct TemplateInstanceResult {
    std::shared_ptr<Type> type;              // Used for type of declaration or when the template is used as a type
    std::shared_ptr<Expression> replacement; // Used when the template is used as a value (e.g. function template)
} TemplateInstanceResult;

class TypeChecker {
  public:
    TypeChecker(CompilerOptions options);

    // Entry point
    void check(std::shared_ptr<Module> module);
    ASTNodePtr lookupConstVariableInModulePath(const std::vector<std::string> &module_path,
                                               const std::string &var_name);

    // Errors collected during checking
    const std::vector<TypeCheckError> &errors() const {
        return m_errors;
    }

    bool ok() const { return m_errors.empty(); }

    CompilerOptions compilerOptions;

    std::shared_ptr<Module> currentModule() const {
        return current_module;
    }

    GlobalSymbolTable &globalSymbols() {
        return symbol_table;
    }

  private:
    friend class ModuleType;     // For module-level type checking and access
    friend class ConstEvaluator; // For constant evaluation
    friend struct CurrentModuleGuard;

    struct CurrentModuleGuard {
        TypeChecker *tc;
        std::shared_ptr<Module> saved;
        CurrentModuleGuard(TypeChecker *tc_, std::shared_ptr<Module> saved_)
            : tc(tc_), saved(saved_) {}
        ~CurrentModuleGuard() { tc->current_module = saved; }
    };

    GlobalSymbolTable symbol_table;

    std::shared_ptr<Module> current_module;
    std::shared_ptr<Module> main_module;

    std::vector<TypeCheckError> m_errors; // Collected errors

    ConstEvaluator m_const_eval;

    // Scope / symbol table: stack of name -> Type.
    // Map of module id -> vec of scopes
    std::unordered_map<SymbolId, std::vector<TypeScope>> m_mod_scopes;

    std::unordered_map<SymbolId, std::shared_ptr<Declaration>> m_templates;

    std::unordered_map<std::string, std::shared_ptr<Type>> m_current_generic_types;

    std::unordered_map<
        std::string,
        std::vector<std::pair<std::string, std::shared_ptr<FunctionDeclaration>>>>
        m_struct_methods;

    std::shared_ptr<Type> m_expected_return_type; // Current function return type

    SymbolTable &symbolTable();
    const SymbolTable &symbolTable() const;

    void ensureGlobalVariableVisible(const std::string &name);

    std::shared_ptr<Type> lookupNamedType(const std::string &name);
    std::shared_ptr<StructType> lookupStructType(const std::string &name);
    std::shared_ptr<UnionType> lookupUnionType(const std::string &name);
    std::shared_ptr<EnumType> lookupEnumType(const std::string &name);
    std::shared_ptr<FunctionType> lookupFunctionType(const std::string &name);
    std::optional<Symbol> lookupNamedSymbol(const std::string &name);

    // Scope helpers
    void pushScope();
    void popScope();
    bool insertSymbol(const std::string &name, SymbolKind kind, std::shared_ptr<Type> t, ASTNodePtr decl, SymbolId *out_id = nullptr);
    std::optional<SymbolId> lookupSymbolInScope(const std::string &name);

    // Type helpers
    std::string typeName(const std::shared_ptr<Type> &t) const;
    std::shared_ptr<Type> resolveType(const std::shared_ptr<ASTNode> &node, const std::shared_ptr<Type> &t);
    std::shared_ptr<Module> resolveModulePath(std::shared_ptr<ASTNode> node, const std::vector<std::string> &path);

    bool canImplicitCast(const std::shared_ptr<Type> &from, const std::shared_ptr<Type> &to);

    void checkNode(const std::shared_ptr<ASTNode> &node);
    void checkStatement(std::shared_ptr<Statement> &stmt);
    void checkAsmStatement(const std::shared_ptr<AsmStmt> &asm_stmt);
    std::shared_ptr<Type> inferExpression(std::shared_ptr<Expression> &expr,
                                          const std::shared_ptr<Type> &expected = nullptr);

    // Declarations
    void checkFunctionDeclaration(const std::shared_ptr<FunctionDeclaration> &fn);
    void checkVariableDeclaration(const std::shared_ptr<VariableDeclaration> &var);
    void checkStructDeclaration(const std::shared_ptr<StructDeclaration> &st);
    void checkUnionDeclaration(const std::shared_ptr<UnionDeclaration> &ud);
    void checkEnumDeclaration(const std::shared_ptr<EnumDeclaration> &en);

    std::shared_ptr<Type> inferTypeCast(const std::shared_ptr<TypeCast> &tc);
    std::shared_ptr<Type> inferVarAccess(std::shared_ptr<VarAccess> &v);
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
    std::shared_ptr<Type> tryInferGenericFunctionCall(const std::shared_ptr<FuncCall> &fc, const std::shared_ptr<FunctionType> &ft);
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

    std::shared_ptr<Type> getCastType(std::shared_ptr<ASTNode> &node, const std::shared_ptr<Type> &from,
                                      const std::shared_ptr<Type> &to);

    std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> expandSizeOf(const std::shared_ptr<FuncCall> &call);
    std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> expandSlice(const std::shared_ptr<FuncCall> &call);
    std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> expandOffsetOf(const std::shared_ptr<FuncCall> &call);
    std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> expandAlignOf(const std::shared_ptr<FuncCall> &call);

    inline std::vector<TypeScope> &currentScopes() {
        auto mod_id = current_module->id;
        return m_mod_scopes[mod_id];
    }
};

void replaceGenericTypes(std::shared_ptr<ASTNode> node, const std::unordered_map<std::string, std::shared_ptr<Type>> &generic_map);

#endif // TYPECHECK_H
