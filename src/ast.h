#ifndef AST_H
#define AST_H

#include <cstdint>
#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

// ----------------- Type system -----------------

enum class TypeKind {
  Bool,
  I32,
  I64,
  U8,
  U32,
  U64,
  USize,
  F32,
  F64,
  Pointer,
  Struct,
  Array,
  Function,
  Null,
  Void,
  Enum,
  Unknown,

  // Categorical types
  Numeric,
  Any, // Any type

};

struct ASTVisitor {
  virtual void visit(struct Program &) {}
  virtual void visit(struct BinaryOperation &) {}
  virtual void visit(struct Literal &) {}
  virtual void visit(struct FuncCall &) {}
  virtual void visit(struct StructDeclaration &) {}
  virtual void visit(struct EnumDeclaration &) {}
  virtual void visit(struct VariableDeclaration &) {}
  virtual void visit(struct ImportDeclaration &) {}
  virtual void visit(struct VarAccess &) {}
  virtual void visit(struct Assignment &) {}
  virtual void visit(struct IfStatement &) {}
  virtual void visit(struct ForStatement &) {}
  virtual void visit(struct WhileStatement &) {}
  virtual void visit(struct ReturnStatement &) {}
  virtual void visit(struct Block &) {}
  virtual void visit(struct TypeCast &) {}
  virtual void visit(struct FieldAccess &) {}
  virtual void visit(struct OffsetAccess &) {}
  virtual void visit(struct ModuleAccess &) {}
  virtual void visit(struct MethodCall &) {}
  virtual void visit(struct Dereference &) {}
  virtual void visit(struct UnaryOperation &) {}
  virtual void visit(struct StructInitializer &) {}
  virtual void visit(struct FunctionDeclaration &) {}
  virtual void visit(struct ExpressionStatement &) {}
  virtual void visit(struct EnumAccess &) {}
};

struct Type {

  bool nullable = false;
  bool is_const = false;

  virtual TypeKind kind() const { return TypeKind::Unknown; }

  virtual ~Type() = default;
  virtual std::string str() const = 0;

  virtual bool equals(const std::shared_ptr<Type> &other) const = 0;

  virtual bool isNumeric() const { return false; }
  virtual bool isInteger() const { return false; }
  virtual bool isUnsigned() const { return false; }
  virtual bool isSigned() const { return false; }
  virtual bool isFloating() const { return false; }

  virtual int numericRank() const { return -1; }

  virtual bool isGeneralNumeric() const { return isNumeric() || kind() == TypeKind::Pointer; }
};

struct Void : Type {
  TypeKind kind() const override { return TypeKind::Void; }
  std::string str() const override { return "Void"; }
  bool equals(const std::shared_ptr<Type> &other) const override {
    return other->kind() == TypeKind::Void;
  }
};

struct U8 : Type {
  TypeKind kind() const override { return TypeKind::U8; }
  std::string str() const override { return "U8"; }
  bool equals(const std::shared_ptr<Type> &other) const override {
    return other->kind() == TypeKind::U8;
  }
  bool isNumeric() const override { return true; }
  bool isInteger() const override { return true; }
  bool isUnsigned() const override { return true; }
  int numericRank() const override { return 0; }
};

struct U32 : Type {
  TypeKind kind() const override { return TypeKind::U32; }
  std::string str() const override { return "U32"; }
  bool equals(const std::shared_ptr<Type> &other) const override {
    return other->kind() == TypeKind::U32;
  }
  bool isNumeric() const override { return true; }
  bool isInteger() const override { return true; }
  bool isUnsigned() const override { return true; }
  int numericRank() const override { return 1; }
};

struct U64 : Type {
  TypeKind kind() const override { return TypeKind::U64; }
  std::string str() const override { return "U64"; }
  bool equals(const std::shared_ptr<Type> &other) const override {
    return other->kind() == TypeKind::U64;
  }
  bool isNumeric() const override { return true; }
  bool isInteger() const override { return true; }
  bool isUnsigned() const override { return true; }
  int numericRank() const override { return 2; }
};

struct USize : Type {
  TypeKind kind() const override { return TypeKind::USize; }
  std::string str() const override { return "USize"; }
  bool equals(const std::shared_ptr<Type> &other) const override {
    return other->kind() == TypeKind::USize;
  }
  bool isNumeric() const override { return true; }
  bool isInteger() const override { return true; }
  bool isUnsigned() const override { return true; }
  int numericRank() const override { return 2; } // usually same as U64
};

struct I32 : Type {
  TypeKind kind() const override { return TypeKind::I32; }
  std::string str() const override { return "I32"; }
  bool equals(const std::shared_ptr<Type> &other) const override {
    return other->kind() == TypeKind::I32;
  }
  bool isNumeric() const override { return true; }
  bool isInteger() const override { return true; }
  bool isSigned() const override { return true; }
  int numericRank() const override { return 1; }
};

struct I64 : Type {
  TypeKind kind() const override { return TypeKind::I64; }
  std::string str() const override { return "I64"; }
  bool equals(const std::shared_ptr<Type> &other) const override {
    return other->kind() == TypeKind::I64;
  }
  bool isNumeric() const override { return true; }
  bool isInteger() const override { return true; }
  bool isSigned() const override { return true; }
  int numericRank() const override { return 2; }
};

struct F32 : Type {
  TypeKind kind() const override { return TypeKind::F32; }
  std::string str() const override { return "F32"; }
  bool equals(const std::shared_ptr<Type> &other) const override {
    return other->kind() == TypeKind::F32;
  }
  bool isNumeric() const override { return true; }
  bool isFloating() const override { return true; }
  int numericRank() const override { return 3; }
};

struct F64 : Type {
  TypeKind kind() const override { return TypeKind::F64; }
  std::string str() const override { return "F64"; }
  bool equals(const std::shared_ptr<Type> &other) const override {
    return other->kind() == TypeKind::F64;
  }
  bool isNumeric() const override { return true; }
  bool isFloating() const override { return true; }
  int numericRank() const override { return 4; }
};

struct Boolean : Type {
  TypeKind kind() const override { return TypeKind::Bool; }
  std::string str() const override { return "BOOL"; }
  bool equals(const std::shared_ptr<Type> &other) const override {
    return other->kind() == TypeKind::Bool;
  }
  int numericRank() const override { return 0; } // optional: treat as lowest
};

struct NullType : Type {
  TypeKind kind() const override { return TypeKind::Null; }
  std::string str() const override { return "Null"; }
  bool equals(const std::shared_ptr<Type> &other) const override {
    return other->kind() == TypeKind::Null;
  }
};

struct PointerType : Type {
  TypeKind kind() const override { return TypeKind::Pointer; }
  std::shared_ptr<Type> base;
  bool pointer_const = false;

  PointerType(std::shared_ptr<Type> b, bool pc = false)
      : base(std::move(b)), pointer_const(pc) {}

  std::string str() const override {
    return (pointer_const ? "const *" : "*") + base->str();
  }

  bool equals(const std::shared_ptr<Type> &other) const override {
    auto o = dynamic_cast<PointerType *>(other.get());
    if (!o)
      return false;
    return pointer_const == o->pointer_const && base->equals(o->base);
  }
};

struct ArrayType : Type {
  TypeKind kind() const override { return TypeKind::Array; }
  std::shared_ptr<Type> base;
  int length;

  ArrayType(std::shared_ptr<Type> b, int len)
      : base(std::move(b)), length(len) {}

  std::string str() const override {
    return "[" + std::to_string(length) + "]" + base->str();
  }

  bool equals(const std::shared_ptr<Type> &other) const override {
    auto o = dynamic_cast<ArrayType *>(other.get());
    if (!o)
      return false;
    return length == o->length && base->equals(o->base);
  }
};

struct ErrorUnionType : Type {
  TypeKind kind() const override { return TypeKind::Unknown; } // or a new TypeKind::ErrorUnion
  std::shared_ptr<Type> valueType;
  std::shared_ptr<Type> errorType;

  ErrorUnionType(std::shared_ptr<Type> v, std::shared_ptr<Type> e)
      : valueType(std::move(v)), errorType(std::move(e)) {}

  std::string str() const override {
    return "Result<" + valueType->str() + ", " + errorType->str() + ">";
  }

  bool equals(const std::shared_ptr<Type> &other) const override {
    auto o = dynamic_cast<ErrorUnionType *>(other.get());
    if (!o)
      return false;
    return valueType->equals(o->valueType) &&
           errorType->equals(o->errorType);
  }
};

struct FunctionDeclaration;

struct StructType : Type {
  TypeKind kind() const override { return TypeKind::Struct; }
  std::string name;
  std::vector<std::pair<std::string, std::shared_ptr<Type>>> fields;
  std::unordered_map<std::string, std::shared_ptr<FunctionDeclaration>> methods;
  bool complete;

  StructType(std::string n,
             std::vector<std::pair<std::string, std::shared_ptr<Type>>> f)
      : name(std::move(n)), fields(std::move(f)), complete(true) {}

  StructType(std::string n) : name(std::move(n)), fields(), complete(false) {}

  std::shared_ptr<Type> getFieldType(const std::string &fname) const {
    for (const auto &field : fields)
      if (field.first == fname)
        return field.second;
    return nullptr;
  }

  int getFieldIndex(const std::string &fname) const {
    for (size_t i = 0; i < fields.size(); ++i)
      if (fields[i].first == fname)
        return static_cast<int>(i);
    return -1;
  }

  void setFields(
      const std::vector<std::pair<std::string, std::shared_ptr<Type>>> &f) {
    fields = f;
    complete = true;
  }

  std::string str() const override {
    std::string result = "Struct " + name + " { ";
    for (const auto &field : fields)
      result += field.first + ", ";
    result += "}";
    return result;
  }

  bool equals(const std::shared_ptr<Type> &other) const override {
    auto o = dynamic_cast<StructType *>(other.get());
    if (!o)
      return false;
    return name == o->name;
    // optional: deep field comparison if you want structural typing
  }
};

struct FunctionType : Type {
  TypeKind kind() const override { return TypeKind::Function; }
  std::vector<std::shared_ptr<Type>> params;
  std::shared_ptr<Type> ret;
  bool variadic;

  FunctionType(std::vector<std::shared_ptr<Type>> p,
               std::shared_ptr<Type> r,
               bool v = false)
      : params(std::move(p)), ret(std::move(r)), variadic(v) {}

  std::string str() const override {
    std::string result = "fn(";
    for (size_t i = 0; i < params.size(); ++i) {
      result += params[i]->str();
      if (i + 1 < params.size())
        result += ", ";
    }
    if (variadic)
      result += "...";
    result += ") -> " + ret->str();
    return result;
  }

  bool equals(const std::shared_ptr<Type> &other) const override {
    auto o = dynamic_cast<FunctionType *>(other.get());
    if (!o)
      return false;
    if (variadic != o->variadic)
      return false;
    if (!ret->equals(o->ret))
      return false;
    if (params.size() != o->params.size())
      return false;
    for (size_t i = 0; i < params.size(); ++i)
      if (!params[i]->equals(o->params[i]))
        return false;
    return true;
  }
};

// ----------------- AST Nodes -----------------

enum class NodeKind {
  Program,
  BinaryOp,
  Literal,
  FuncCall,
  StructDecl,
  EnumDecl,
  VarDecl,
  VarAccess,
  Assignment,
  IfStmt,
  ForStmt,
  WhileStmt,
  ReturnStmt,
  Block,
  TypeCast,
  FieldAccess,
  OffsetAccess,
  ModuleAccess,
  MethodCall,
  Dereference,
  UnaryOp,
  StructInit,
  FunctionDecl,
  ExpressionStatement,
  Unknown
};

struct ASTNode {
  int line = 0;
  int col = 0;
  std::shared_ptr<Type> inferred_type = nullptr;

  ASTNode() = default;
  ASTNode(int l, int c) : line(l), col(c) {}

  virtual ~ASTNode() = default;
  virtual std::string str() const { return "ASTNode"; }
  virtual NodeKind kind() const { return NodeKind::Unknown; }
  virtual void accept(ASTVisitor &v);

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
struct Declaration : ASTNode {
  bool is_pub;
};
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
  NodeKind kind() const override { return NodeKind::Program; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
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
  NodeKind kind() const override { return NodeKind::TypeCast; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
};

struct VarAccess : Expression {
  std::string name;
  bool is_extern = false;
  VarAccess(std::string n) : name(std::move(n)) {}
  std::string str() const override { return "VarAccess(" + name + ")"; }
  NodeKind kind() const override { return NodeKind::VarAccess; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
};

struct EnumAccess : Expression {
  std::string enum_name;
  std::string variant;
  EnumAccess(std::string n, std::string v) : enum_name(n), variant(std::move(v)) {}
  std::string str() const override { return "EnumAccess(" + enum_name + "::" + variant + ")"; }
  NodeKind kind() const override { return NodeKind::VarAccess; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
};

struct Dereference : Expression {
  ExprPtr pointer;
  Dereference(ExprPtr p) : pointer(std::move(p)) {}
  std::string str() const override {
    return "Dereference(" + pointer->toString() + ")";
  }
  NodeKind kind() const override {
    return NodeKind::Dereference;
  };
  void accept(ASTVisitor &v) override { v.visit(*this); }
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
  NodeKind kind() const override { return NodeKind::FuncCall; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
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
  NodeKind kind() const override { return NodeKind::MethodCall; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
};

struct FieldAccess : Expression {
  ExprPtr base;
  std::string field;
  FieldAccess(ExprPtr b, std::string f)
      : base(std::move(b)), field(std::move(f)) {}
  std::string str() const override {
    return "FieldAccess(" + base->toString() + "." + field + ")";
  }
  NodeKind kind() const override { return NodeKind::FieldAccess; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
};

struct OffsetAccess : Expression {
  ExprPtr base;
  ExprPtr index;
  OffsetAccess(ExprPtr b, ExprPtr i)
      : base(std::move(b)), index(std::move(i)) {}
  std::string str() const override {
    return "OffsetAccess(" + base->toString() + "[" + index->toString() + "])";
  }
  NodeKind kind() const override { return NodeKind::OffsetAccess; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
};

struct ModuleAccess : Expression {
  std::string module_name;
  std::string member_name;
  ModuleAccess(std::string m, std::string n)
      : module_name(std::move(m)), member_name(std::move(n)) {}
  std::string str() const override {
    return "ModuleAccess(" + module_name + "::" + member_name + ")";
  }
  NodeKind kind() const override { return NodeKind::ModuleAccess; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
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
  NodeKind kind() const override { return NodeKind::BinaryOp; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
};

struct UnaryOperation : Expression {
  std::string op;
  ExprPtr operand;
  UnaryOperation(std::string o, ExprPtr e)
      : op(std::move(o)), operand(std::move(e)) {}
  std::string str() const override {
    return "UnaryOperation(" + op + operand->toString() + ")";
  }
  NodeKind kind() const override { return NodeKind::UnaryOp; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
};

using ConstValue = std::variant<uint64_t, int64_t, double, bool, std::string>;
struct Literal : Expression {
  ConstValue value;
  std::shared_ptr<Type> lit_type;
  Literal(ConstValue v,
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
    return "Literal(" + val_str + " : " + (lit_type != nullptr ? lit_type->str() : "nullptr") + ")";
  }
  NodeKind kind() const override { return NodeKind::Literal; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
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
  NodeKind kind() const override { return NodeKind::StructInit; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
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
  NodeKind kind() const override { return NodeKind::Block; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
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
  NodeKind kind() const override { return NodeKind::IfStmt; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
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
  NodeKind kind() const override { return NodeKind::ForStmt; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
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
  NodeKind kind() const override { return NodeKind::WhileStmt; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
};

struct ReturnStatement : Statement {
  ExprPtr value;
  bool is_error = false; // For error unions, set outside of constructor
  ReturnStatement(ExprPtr v) : value(std::move(v)) {}
  std::string str() const override {
    return "ReturnStatement(" + (value ? value->toString() : "void") + ")";
  }
  NodeKind kind() const override { return NodeKind::ReturnStmt; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
};

struct ExpressionStatement : Statement {
  ExprPtr expression;
  ExpressionStatement(ExprPtr e) : expression(std::move(e)) {}
  std::string str() const override {
    return "ExpressionStatement(" + expression->toString() + ")";
  }
  NodeKind kind() const override { return NodeKind::ExpressionStatement; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
};

struct Assignment : Statement {
  ExprPtr target;
  ExprPtr value;
  Assignment(ExprPtr t, ExprPtr v)
      : target(std::move(t)), value(std::move(v)) {}
  std::string str() const override {
    return "Assignment(" + target->toString() + " = " + value->toString() + ")";
  }
  NodeKind kind() const override { return NodeKind::Assignment; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
};

struct FunctionDeclaration : Declaration {
  bool is_extern;
  std::string name;
  std::shared_ptr<FunctionType> type;
  std::vector<std::string> param_names;
  std::shared_ptr<Statement> body;

  FunctionDeclaration(std::string n, std::shared_ptr<FunctionType> t,
                      std::vector<std::string> params,
                      std::shared_ptr<Statement> b, bool ext = false)
      : name(std::move(n)), type(std::move(t)), param_names(std::move(params)),
        body(std::move(b)), is_extern(ext) {}
  std::string str() const override {
    return std::string(is_extern ? "Extern" : "") + "FunctionDeclaration(" + type->str() + ", " + name + "){" +
           (body ? body->toString() : " ;") + "}";
  }
  NodeKind kind() const override { return NodeKind::FunctionDecl; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
};

struct VariableDeclaration : Statement {
  std::string name;
  std::shared_ptr<Type> var_type;
  ASTNodePtr initializer;
  bool is_const = false;

  VariableDeclaration(std::string n, std::shared_ptr<Type> t, ASTNodePtr i)
      : name(std::move(n)), var_type(std::move(t)), initializer(std::move(i)) {}
  std::string str() const override {
    return "VariableDeclaration(" + name + " : " + (var_type != nullptr ? var_type->str() : "unknown") + " = " +
           (initializer ? initializer->toString() : "null") + (is_const ? ", const" : "") + ")";
  }
  NodeKind kind() const override { return NodeKind::VarDecl; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
};

struct ImportDeclaration : ASTNode {
  std::string path;
  std::string alias;

  ImportDeclaration(std::string p, std::string a) : path(std::move(p)), alias(std::move(a)) {}
  std::string str() const override {
    return "ImportDeclaration(\"" + path + "\", \"" + alias + "\")";
  }
  NodeKind kind() const override { return NodeKind::Unknown; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
};

struct EnumType : Type {
  TypeKind kind() const override { return TypeKind::Enum; }
  std::string name;
  std::shared_ptr<Type> base_type;
  std::unordered_map<std::string, std::shared_ptr<Literal>> variant_map;

  EnumType(std::string n, std::shared_ptr<Type> b)
      : name(std::move(n)), base_type(std::move(b)) {}

  void addVariant(const std::string &variant, std::shared_ptr<Literal> value) {
    variant_map[variant] = std::move(value);
  }
  std::string str() const override { return "Enum " + name; }
  bool equals(const std::shared_ptr<Type> &other) const override {
    auto o = dynamic_cast<EnumType *>(other.get());
    if (!o)
      return false;
    return name == o->name;
  }
};

struct EnumDeclaration : Declaration {
  std::string name;
  std::shared_ptr<Type> base_type;
  std::shared_ptr<EnumType> enum_type; // Set after declaration
  std::unordered_map<std::string, std::shared_ptr<Literal>> variants;

  EnumDeclaration(std::string n,
                  std::shared_ptr<Type> t,
                  std::unordered_map<std::string, std::shared_ptr<Literal>> v)
      : name(std::move(n)), base_type(std::move(t)), variants(std::move(v)) {}
  std::string str() const override {
    std::string result = "EnumDeclaration(" + name + ") { ";
    for (const auto &variant : variants) {
      result += variant.first + ": " + variant.second->toString() + "; ";
    }
    result += "}";
    return result;
  }
  NodeKind kind() const override { return NodeKind::EnumDecl; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
};

struct StructDeclaration : Declaration {
  bool is_extern = false;
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
  NodeKind kind() const override { return NodeKind::StructDecl; }
  void accept(ASTVisitor &v) override { v.visit(*this); }
};

#endif // AST_H
