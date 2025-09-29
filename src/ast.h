#ifndef AST_H
#define AST_H

#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

// ----------------- Type system -----------------
struct Type {
  bool nullable = false;
  bool is_const = false;

  virtual ~Type() = default;
  virtual std::string str() const = 0;
};

struct Void : Type {
  std::string str() const override { return "Void"; }
};

struct U8 : Type {
  std::string str() const override { return "U8"; }
};
struct U32 : Type {
  std::string str() const override { return "U32"; }
};
struct U64 : Type {
  std::string str() const override { return "U64"; }
};
struct USize : Type {
  std::string str() const override { return "USize"; }
};
struct I32 : Type {
  std::string str() const override { return "I32"; }
};
struct I64 : Type {
  std::string str() const override { return "I64"; }
};
struct F32 : Type {
  std::string str() const override { return "F32"; }
};
struct F64 : Type {
  std::string str() const override { return "F64"; }
};
struct BOOL : Type {
  std::string str() const override { return "BOOL"; }
};
struct NullType : Type {
  std::string str() const override { return "Null"; }
};

struct PointerType : Type {
  std::shared_ptr<Type> base;
  bool pointer_const = false;
  PointerType(std::shared_ptr<Type> b, bool pc = false)
      : base(std::move(b)), pointer_const(pc) {}
  std::string str() const override { return "*" + base->str(); }
};

struct ArrayType : Type {
  std::shared_ptr<Type> base;
  int length;
  ArrayType(std::shared_ptr<Type> b, int len)
      : base(std::move(b)), length(len) {}
  std::string str() const override {
    return "[" + std::to_string(length) + "]" + base->str();
  }
};

struct FunctionDeclaration;

struct StructType : Type {
  std::string name;

  std::vector<std::pair<std::string, std::shared_ptr<Type>>> fields;
  std::unordered_map<std::string, std::shared_ptr<FunctionDeclaration>> methods;
  bool complete;

  std::shared_ptr<Type> getFieldType(const std::string &fname) const {
    for (const auto &field : fields) {
      if (field.first == fname)
        return field.second;
    }
    return nullptr;
  }
  void setFields(
      const std::vector<std::pair<std::string, std::shared_ptr<Type>>> &f) {
    fields = f;
    complete = true;
  }
  int getFieldIndex(const std::string &fname) const {
    for (size_t i = 0; i < fields.size(); ++i) {
      if (fields[i].first == fname)
        return static_cast<int>(i);
    }
    return -1;
  }
  StructType(std::string n,
             std::vector<std::pair<std::string, std::shared_ptr<Type>>> f)
      : name(std::move(n)), fields(std::move(f)), complete(true) {}
  StructType(std::string n) : name(std::move(n)), fields(), complete(false) {}
  std::string str() const override {
    std::string result = "Struct " + name + " { ";
    for (const auto &field : fields) {
      result += field.first + ", ";
    }
    result += "}";
    return result;
  }
};

struct FunctionType : Type {
  std::vector<std::shared_ptr<Type>> params;
  std::shared_ptr<Type> ret;
  bool variadic;
  FunctionType(std::vector<std::shared_ptr<Type>> p, std::shared_ptr<Type> r,
               bool v = false)
      : params(std::move(p)), ret(std::move(r)), variadic(v) {}
  std::string str() const override { return "fn(...) -> " + ret->str(); }
};

// ----------------- AST Nodes -----------------

struct ASTNode {
  int line = 0;
  int col = 0;
  std::shared_ptr<Type> inferred_type = nullptr;

  ASTNode() = default;
  ASTNode(int l, int c) : line(l), col(c) {}

  virtual ~ASTNode() = default;
  virtual std::string str() const { return "ASTNode"; }

  std::string locationStr() const {
    return "(line " + std::to_string(line) + ", col " + std::to_string(col) +
           ")";
  }

  std::string toString() const {
    if (inferred_type) {
      return str() + " : " + inferred_type->str();
    }
    return str();
  }
};

struct Statement : ASTNode {};
struct Expression : ASTNode {};
using ExprPtr = std::shared_ptr<Expression>;
using StmtPtr = std::shared_ptr<Statement>;

using ASTNodePtr = std::shared_ptr<ASTNode>;

struct Program : ASTNode {
  std::vector<ASTNodePtr> declarations;
  Program() = default;
  void append(ASTNodePtr decl) { declarations.push_back(std::move(decl)); }
  std::string str() const override {
    std::string result = "Program:\n";
    for (const auto &decl : declarations) {
      result += "  " + decl->toString() + "\n";
    }
    return result;
  }
};

struct FunctionDeclaration : ASTNode {
  std::string name;
  std::shared_ptr<FunctionType> type;
  std::vector<std::string> param_names;
  std::shared_ptr<Statement> body;

  FunctionDeclaration(std::string n, std::shared_ptr<FunctionType> t,
                      std::vector<std::string> params,
                      std::shared_ptr<Statement> b)
      : name(std::move(n)), type(std::move(t)), param_names(std::move(params)),
        body(std::move(b)) {}
  std::string str() const override {
    return "FunctionDeclaration(" + type->str() + ", " + name + "){" +
           (body ? body->toString() : " ;") + "}";
  }
};

struct VariableDeclaration : Statement {
  std::string name;
  std::shared_ptr<Type> var_type;
  ASTNodePtr initializer;

  VariableDeclaration(std::string n, std::shared_ptr<Type> t, ASTNodePtr i)
      : name(std::move(n)), var_type(std::move(t)), initializer(std::move(i)) {}
  std::string str() const override {
    return "VariableDeclaration(" + name + " : " + var_type->str() + ")";
  }
};

struct StructDeclaration : ASTNode {
  std::string name;
  std::vector<std::pair<std::string, std::shared_ptr<Type>>> fields;
  std::unordered_map<std::string, std::shared_ptr<FunctionDeclaration>> methods;

  std::pair<std::string, std::shared_ptr<Type>>
  getField(const std::string &fname) const {
    for (const auto &field : fields) {
      if (field.first == fname)
        return field;
    }
    return {"", nullptr};
  }

  StructDeclaration(
      std::string n,
      std::vector<std::pair<std::string, std::shared_ptr<Type>>> f)
      : name(std::move(n)), fields(std::move(f)) {}
  std::string str() const override {
    std::string result = "StructDeclaration(" + name + ") { ";
    for (const auto &field : fields) {
      result += field.first + ": " + field.second->str() + "; ";
    }
    for (const auto &method : methods) {
      result += "Method: " + method.first + "\n";
      result += method.second->toString() + " ";
    }
    result += "}";

    return result;
  }
};

// ----------------- Expressions -----------------

enum class CastType { Normal,
                      Reinterperet };

struct TypeCast : Expression {
  ExprPtr expr;
  std::shared_ptr<Type> target_type;
  CastType cast_type;
  TypeCast(ExprPtr e, std::shared_ptr<Type> t, CastType ct)
      : expr(std::move(e)), target_type(std::move(t)), cast_type(ct) {}
  std::string str() const override {
    return "TypeCast(" + expr->toString() +
           (cast_type == CastType::Normal ? " as " : " re ") +
           target_type->str() + ")";
  }
};

struct VarAccess : Expression {
  std::string name;
  VarAccess(std::string n) : name(std::move(n)) {}
  std::string str() const override { return "VarAccess(" + name + ")"; }
};

struct Dereference : Expression {
  ExprPtr pointer;
  Dereference(ExprPtr p) : pointer(std::move(p)) {}
  std::string str() const override {
    return "Dereference(" + pointer->toString() + ")";
  }
};

struct FuncCall : Expression {
  ExprPtr func;
  std::vector<ExprPtr> args;
  FuncCall(ExprPtr f, std::vector<ExprPtr> a)
      : func(std::move(f)), args(std::move(a)) {}
  std::string str() const override {
    std::string result = "FuncCall(" + func->toString() + "(";
    for (size_t i = 0; i < args.size(); ++i) {
      if (i > 0)
        result += ", ";
      result += args[i]->toString();
    }
    result += "))";
    return result;
  }
};

struct MethodCall : Expression {
  ExprPtr object;
  std::string method;
  std::vector<ExprPtr> args;
  MethodCall(ExprPtr o, std::string m, std::vector<ExprPtr> a)
      : object(std::move(o)), method(std::move(m)), args(std::move(a)) {}
  std::string str() const override {
    std::string result = "MethodCall(" + object->toString() + "." + method + "(";
    for (size_t i = 0; i < args.size(); ++i) {
      if (i > 0)
        result += ", ";
      result += args[i]->toString();
    }
    result += "))";
    return result;
  }
};

struct FieldAccess : Expression {
  ASTNodePtr base;
  std::string field;
  FieldAccess(ASTNodePtr b, std::string f)
      : base(std::move(b)), field(std::move(f)) {}
  std::string str() const override {
    return "FieldAccess(" + base->toString() + "." + field + ")";
  }
};

struct OffsetAccess : Expression {
  ASTNodePtr base;
  ExprPtr index;
  OffsetAccess(ASTNodePtr b, ExprPtr i)
      : base(std::move(b)), index(std::move(i)) {}
  std::string str() const override {
    return "OffsetAccess(" + base->toString() + "[" + index->toString() + "])";
  }
};

struct BinaryOperation : Expression {
  ExprPtr left;
  std::string op;
  ExprPtr right;
  BinaryOperation(ExprPtr l, std::string o, ExprPtr r)
      : left(std::move(l)), op(std::move(o)), right(std::move(r)) {}
  std::string str() const override {
    return "BinaryOperation(" + left->toString() + " " + op + " " +
           right->toString() + ")";
  }
};

struct UnaryOperation : Expression {
  std::string op;
  ExprPtr operand;
  UnaryOperation(std::string o, ExprPtr e)
      : op(std::move(o)), operand(std::move(e)) {}
  std::string str() const override {
    return "UnaryOperation(" + op + operand->toString() + ")";
  }
};

struct Literal : Expression {
  std::variant<int, float, bool, std::string> value;
  std::shared_ptr<Type> lit_type;
  Literal(std::variant<int, float, bool, std::string> v,
          std::shared_ptr<Type> t)
      : value(std::move(v)), lit_type(std::move(t)) {}
  std::string str() const override {
    std::string val_str = std::visit(
        [](const auto &v) -> std::string {
          using T = std::decay_t<decltype(v)>;
          if constexpr (std::is_same_v<T, bool>) {
            return v ? "true" : "false";
          } else if constexpr (std::is_same_v<T, std::string>) {
            return "\"" + v + "\"";
          } else if constexpr (std::is_same_v<T, float>) {
            std::ostringstream oss;
            oss << v;
            return oss.str();
          } else {
            return std::to_string(v);
          }
        },
        value);
    return "Literal(" + val_str + " : " + lit_type->str() + ")";
  }
};

struct StructInitializer : Expression {
  std::shared_ptr<StructType> struct_type;
  std::map<std::string, std::shared_ptr<Expression>> field_values;

  StructInitializer(std::shared_ptr<StructType> t,
                    std::map<std::string, std::shared_ptr<Expression>> f)
      : struct_type(std::move(t)), field_values(std::move(f)) {}
  std::string str() const override {
    std::string result = "StructInitializer(" + struct_type->str() + " { ";
    bool first = true;
    for (const auto &[field, value] : field_values) {
      if (!first)
        result += ", ";
      result += field + ": " + value->toString();
      first = false;
    }
    result += " })";
    return result;
  }
};

// ----------------- Statements -----------------

struct Block : Statement {
  std::vector<StmtPtr> statements;
  Block() = default;
  void append(StmtPtr stmt) { statements.push_back(std::move(stmt)); }
  std::string str() const override {
    std::string result = "Block { ";
    for (size_t i = 0; i < statements.size(); ++i) {
      if (i > 0)
        result += ";\n";
      result += statements[i]->toString();
    }
    result += " }";
    return result;
  }
};

struct IfStatement : Statement {
  ExprPtr condition;
  StmtPtr then_branch;
  StmtPtr else_branch;
  IfStatement(ExprPtr c, StmtPtr t, StmtPtr e = nullptr)
      : condition(std::move(c)), then_branch(std::move(t)),
        else_branch(std::move(e)) {}
  std::string str() const override {
    std::string result = "IfStatement(" + condition->toString() + " then " +
                         then_branch->toString();
    if (else_branch)
      result += " else " + else_branch->toString();
    result += ")";
    return result;
  }
};

struct ForStatement : Statement {
  StmtPtr init;
  ExprPtr condition;
  StmtPtr increment;
  StmtPtr body;
  ForStatement(StmtPtr i, ExprPtr c, StmtPtr inc, StmtPtr b)
      : init(std::move(i)), condition(std::move(c)), increment(std::move(inc)),
        body(std::move(b)) {}
  std::string str() const override {
    return "ForStatement(" + (init ? init->toString() : "none") + "; " +
           (condition ? condition->toString() : "none") + "; " +
           (increment ? increment->toString() : "none") + " " +
           body->toString() + ")";
  }
};

struct WhileStatement : Statement {
  ExprPtr condition;
  StmtPtr body;
  WhileStatement(ExprPtr c, StmtPtr b)
      : condition(std::move(c)), body(std::move(b)) {}
  std::string str() const override {
    return "WhileStatement(" + condition->toString() + " " + body->toString() +
           ")";
  }
};

struct ReturnStatement : Statement {
  ExprPtr value;
  ReturnStatement(ExprPtr v) : value(std::move(v)) {}
  std::string str() const override {
    return "ReturnStatement(" + (value ? value->toString() : "void") + ")";
  }
};

struct ExpressionStatement : Statement {
  ExprPtr expression;
  ExpressionStatement(ExprPtr e) : expression(std::move(e)) {}
  std::string str() const override {
    return "ExpressionStatement(" + expression->toString() + ")";
  }
};

struct Assignment : Statement {
  ExprPtr target;
  ExprPtr value;
  Assignment(ExprPtr t, ExprPtr v)
      : target(std::move(t)), value(std::move(v)) {}
  std::string str() const override {
    return "Assignment(" + target->toString() + " = " + value->toString() + ")";
  }
};

#endif // AST_H
