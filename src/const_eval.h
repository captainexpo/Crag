#pragma once
#ifndef CONST_EVAL_H
#define CONST_EVAL_H

#include "ast/ast.h"
#include <optional>
#include <string>
#include <vector>

using LiteralPtr = std::shared_ptr<Literal>;

struct TypeChecker;

class ConstEvaluator {
  public:
    ConstEvaluator(TypeChecker* tc)
        : m_type_checker(tc) {}


    std::optional<ExprPtr> evaluateExpression(const ExprPtr &expr);
    std::optional<std::shared_ptr<VariableDeclaration>> evaluateVariableDeclaration(const std::shared_ptr<VariableDeclaration> &var);
    std::shared_ptr<Literal> castLiteral(const LiteralPtr &lit, const std::shared_ptr<Type> &targetType);
    const std::vector<std::pair<ASTNodePtr, std::string>> &errors() const { return m_errors; }
    bool ok() const { return m_errors.empty(); }

  private:
    TypeChecker *m_type_checker;
    std::map<std::string, ExprPtr> m_const_vars;
    std::vector<std::pair<ASTNodePtr, std::string>> m_errors;
    std::optional<LiteralPtr>
    evaluateBinaryLiterals(const LiteralPtr &lhs,
                           const LiteralPtr &rhs,
                           const std::string &op);
    void error(const ASTNodePtr &node, const std::string &msg);
};


#endif
