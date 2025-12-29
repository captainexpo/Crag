#pragma once
#ifndef CONST_EVAL_H
#define CONST_EVAL_H

#include "ast/ast.h"
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

using LiteralPtr = std::shared_ptr<Literal>;

class ConstEvaluator {
public:
  ConstEvaluator() = default;

  std::optional<ExprPtr> evaluateExpression(const ExprPtr &expr);
  std::optional<std::shared_ptr<VariableDeclaration>> evaluateVariableDeclaration(const std::shared_ptr<VariableDeclaration> &var);
  const std::vector<std::pair<ASTNodePtr, std::string>> &errors() const { return m_errors; }
  bool ok() const { return m_errors.empty(); }

private:
  std::map<std::string, LiteralPtr> m_const_vars;
  std::vector<std::pair<ASTNodePtr, std::string>> m_errors;
  std::optional<LiteralPtr>
  evaluateBinaryLiterals(const LiteralPtr &lhs,
                         const LiteralPtr &rhs,
                         const std::string &op);
  void error(const ASTNodePtr &node, const std::string &msg);
};

#endif
