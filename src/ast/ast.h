#pragma once

#ifndef AST_H
#define AST_H

#include "../utils.h"
#include <cassert>

#include <cstdint>
#include <map>
#include <memory>
#include <set>
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
    Union,
    Array,
    Function,
    Null,
    Void,
    Enum,
    ErrorUnion,
    Unknown,
    Qualified, // For unknown types from imported modules
    TemplateInstanceType,

    // Categorical types
    Numeric,
    Any,

};

struct ASTVisitor {
    virtual ~ASTVisitor() = default;
    virtual void visit(struct Program &) {}
    virtual void visit(struct BinaryOperation &) {}
    virtual void visit(struct Literal &) {}
    virtual void visit(struct ArrayLiteral &) {}
    virtual void visit(struct FuncCall &) {}
    virtual void visit(struct StructDeclaration &) {}
    virtual void visit(struct UnionDeclaration &) {}
    virtual void visit(struct EnumDeclaration &) {}
    virtual void visit(struct VariableDeclaration &) {}
    virtual void visit(struct TypeAliasDeclaration &) {}
    virtual void visit(struct ImportDeclaration &) {}
    virtual void visit(struct TemplateInstantiation &) {}
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
    virtual void visit(struct BreakStatement &) {}
    virtual void visit(struct ContinueStatement &) {}
    virtual void visit(struct UnknownNode &) {}
    virtual void visit(struct TypeExpression &) {}
};

struct Type {

    bool nullable = false;
    bool is_const = false;

    virtual TypeKind kind() const { return TypeKind::Unknown; }

    virtual ~Type() = default;
    virtual std::string str() const = 0;

    virtual bool equals(const std::shared_ptr<Type> &other) const = 0;

    virtual std::shared_ptr<Type> copy() const = 0;

    virtual bool isNumeric() const { return false; }
    virtual bool isInteger() const { return false; }
    virtual bool isUnsigned() const { return false; }
    virtual bool isSigned() const { return false; }
    virtual bool isFloating() const { return false; }

    virtual int numericRank() const { return -1; }

    virtual bool isGeneralNumeric() const { return isNumeric() || kind() == TypeKind::Pointer; }
};

enum class NodeKind {
    Program,
    BinaryOp,
    Literal,
    FuncCall,
    StructDecl,
    UnionDecl,
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
    ModuleAccess,
    EnumAccess,

    OffsetAccess,
    MethodCall,
    Dereference,
    UnaryOp,
    StructInit,
    FunctionDecl,
    ExpressionStatement,
    BreakStatement,
    ContinueStatement,
    TemplateInstantiation,
    ImportDeclaration,
    TypeAliasDeclaration,
    TypeExpression,
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

    virtual std::shared_ptr<ASTNode> copy() const;

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

struct Statement : virtual ASTNode {};
struct Expression : virtual ASTNode {};

struct Void : Type {
    TypeKind kind() const override { return TypeKind::Void; }
    std::string str() const override { return "Void"; }
    bool equals(const std::shared_ptr<Type> &other) const override {
        return other->kind() == TypeKind::Void;
    }
    std::shared_ptr<Type> copy() const override {
        return std::make_shared<Void>(*this);
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
    std::shared_ptr<Type> copy() const override {
        return std::make_shared<U8>(*this);
    }
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
    std::shared_ptr<Type> copy() const override {
        return std::make_shared<U32>(*this);
    }
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
    std::shared_ptr<Type> copy() const override {
        return std::make_shared<U64>(*this);
    }
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
    std::shared_ptr<Type> copy() const override {
        return std::make_shared<USize>(*this);
    }
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
    std::shared_ptr<Type> copy() const override {
        return std::make_shared<I32>(*this);
    }
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
    std::shared_ptr<Type> copy() const override {
        return std::make_shared<I64>(*this);
    }
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
    std::shared_ptr<Type> copy() const override {
        return std::make_shared<F32>(*this);
    }
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
    std::shared_ptr<Type> copy() const override {
        return std::make_shared<F64>(*this);
    }
};

struct Boolean : Type {
    TypeKind kind() const override { return TypeKind::Bool; }
    std::string str() const override { return "BOOL"; }
    bool equals(const std::shared_ptr<Type> &other) const override {
        return other->kind() == TypeKind::Bool;
    }
    int numericRank() const override { return 0; } // optional: treat as lowest
    std::shared_ptr<Type> copy() const override {
        return std::make_shared<Boolean>(*this);
    }
};

struct NullType : Type {
    TypeKind kind() const override { return TypeKind::Null; }
    std::string str() const override { return "Null"; }
    bool equals(const std::shared_ptr<Type> &other) const override {
        return other->kind() == TypeKind::Null;
    }
    std::shared_ptr<Type> copy() const override {
        return std::make_shared<NullType>(*this);
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
    std::shared_ptr<Type> copy() const override {
        return std::make_shared<PointerType>(base->copy(), pointer_const);
    }
};

struct ArrayType : Type {
    TypeKind kind() const override { return TypeKind::Array; }
    std::shared_ptr<Type> element_type;
    std::shared_ptr<Expression> length_expr;
    size_t actualSize = 0; // Filled in during type checking for sized arrays
    bool unsized;

    ArrayType(std::shared_ptr<Type> b, std::shared_ptr<Expression> len, bool us)
        : element_type(std::move(b)), length_expr(len), unsized(us) {
        if (us)
            ASSERT(len == nullptr, "Unsized array must have length of nullptr: " + str());
        else
            ASSERT(len != nullptr, "Sized array must have valid length expression: " + str());
    }

    std::string str() const override {
        return "[" + (unsized ? "unsized" : length_expr->str()) + "]" + element_type->str();
    }

    bool equals(const std::shared_ptr<Type> &other) const override {
        auto o = dynamic_cast<ArrayType *>(other.get());
        if (!o)
            return false;
        if (unsized != o->unsized)
            return false;
        if (!element_type->equals(o->element_type))
            return false;
        if (unsized)
            return true;
        // TODO: Better length expression comparison
        return length_expr == o->length_expr;
    }
    std::shared_ptr<Type> copy() const override {
        return std::make_shared<ArrayType>(element_type->copy(),
                                           length_expr, unsized);
    }
};

struct ErrorUnionType : Type {
    TypeKind kind() const override { return TypeKind::ErrorUnion; }
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
    std::shared_ptr<Type> copy() const override {
        return std::make_shared<ErrorUnionType>(valueType->copy(),
                                                errorType->copy());
    }
};

struct QualifiedType : Type {
    TypeKind kind() const override { return TypeKind::Qualified; }

    std::vector<std::string> module_path;
    std::string type_name;

    QualifiedType(std::vector<std::string> mp, std::string tn)
        : module_path(std::move(mp)), type_name(std::move(tn)) {}

    std::string str() const override {
        std::string result;
        for (const auto &mod : module_path) {
            result += mod + "::";
        }
        result += type_name;
        return result;
    }

    bool equals(const std::shared_ptr<Type> &other) const override {
        auto o = dynamic_cast<QualifiedType *>(other.get());
        if (!o)
            return false;
        return module_path == o->module_path && type_name == o->type_name;
    }
    std::shared_ptr<Type> copy() const override {
        return std::make_shared<QualifiedType>(module_path, type_name);
    }
};

enum class GenericConstraintKind {
    Any,
    Numeric,
    Integer,
    Unsigned,
    Signed,
    Floating,
    Pointer,
};


struct TemplateInstanceType  : Type {
    std::shared_ptr<Type> base;
    std::vector<std::shared_ptr<Type>> type_args;

    TemplateInstanceType(std::shared_ptr<Type> b,
                         std::vector<std::shared_ptr<Type>> ta)
        : base(std::move(b)), type_args(std::move(ta)) {}

    std::string str() const override {
        std::string result = base->str() + "::<";
        for (size_t i = 0; i < type_args.size(); ++i) {
            if (i > 0)
                result += ", ";
            result += type_args[i]->str();
        }
        result += ">";
        return result;
    }

    bool equals(const std::shared_ptr<Type> &other) const override {
        auto o = dynamic_cast<TemplateInstanceType *>(other.get());
        if (!o)
            return false;
        if (!base->equals(o->base))
            return false;
        if (type_args.size() != o->type_args.size())
            return false;
        for (size_t i = 0; i < type_args.size(); ++i)
            if (!type_args[i]->equals(o->type_args[i]))
                return false;
        return true;
    }

    std::shared_ptr<Type> copy() const override {
        std::vector<std::shared_ptr<Type>> arg_copies;
        for (const auto &arg : type_args) {
            arg_copies.push_back(arg->copy());
        }
        return std::make_shared<TemplateInstanceType>(base->copy(), arg_copies);
    }

    TypeKind kind() const override { return TypeKind::TemplateInstanceType; }
};

struct GenericType : Type {
    TypeKind kind() const override { return TypeKind::Unknown; }

    std::string name;
    GenericConstraintKind constraint = GenericConstraintKind::Any; // Not implemented yet

    GenericType(std::string n) : name(std::move(n)) {}

    std::string str() const override {
        return "Generic<" + name + ">";
    }

    bool equals(const std::shared_ptr<Type> &other) const override {
        auto o = dynamic_cast<GenericType *>(other.get());
        if (!o)
            return false;
        return name == o->name;
    }
    std::shared_ptr<Type> copy() const override {
        return std::make_shared<GenericType>(name);
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
        for (const auto &method : methods)
            result += method.first + "(), ";
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

    std::shared_ptr<Type> copy() const override {
        return std::make_shared<StructType>(name, fields);
    }
};

struct UnionType : Type {
    TypeKind kind() const override { return TypeKind::Union; }
    std::string name;
    std::vector<std::pair<std::string, std::shared_ptr<Type>>> fields;
    bool complete;

    UnionType(std::string n,
              std::vector<std::pair<std::string, std::shared_ptr<Type>>> f)
        : name(std::move(n)), fields(std::move(f)), complete(true) {}

    UnionType(std::string n) : name(std::move(n)), fields(), complete(false) {}

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
        std::string result = "Union " + name + " { ";
        for (const auto &field : fields)
            result += field.first + ", ";
        result += "}";
        return result;
    }

    bool equals(const std::shared_ptr<Type> &other) const override {
        auto o = dynamic_cast<UnionType *>(other.get());
        if (!o)
            return false;
        return name == o->name;
    }

    std::shared_ptr<Type> copy() const override {
        return std::make_shared<UnionType>(name, fields);
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
    std::shared_ptr<Type> copy() const override {
        std::vector<std::shared_ptr<Type>> param_copies;
        for (const auto &param : params) {
            param_copies.push_back(param->copy());
        }
        return std::make_shared<FunctionType>(param_copies, ret->copy(), variadic);
    }
};

// ----------------- AST Nodes -----------------

struct Declaration : virtual ASTNode {
    bool is_pub;
    std::vector<std::string> generic_params;
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
    std::shared_ptr<ASTNode> copy() const override;
};

// ----------------- Expressions -----------------

enum class CastType { Normal,
                      Reinterperet };

struct TypeCast : public Expression {
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
    std::shared_ptr<ASTNode> copy() const override;
};

struct TypeExpression : public Expression {
    std::shared_ptr<Type> type;
    TypeExpression(std::shared_ptr<Type> t) : type(std::move(t)) {}
    std::string str() const override {
        return "TypeExpression(" + type->str() + ")";
    }
    NodeKind kind() const override { return NodeKind::TypeExpression; }
    void accept(ASTVisitor &v) override { v.visit(*this); }
    std::shared_ptr<ASTNode> copy() const override;
};

struct VarAccess : public Expression {
    std::string name;
    bool is_extern = false;
    VarAccess(std::string n) : name(std::move(n)) {}
    std::string str() const override { return "VarAccess(" + name + ")"; }
    NodeKind kind() const override { return NodeKind::VarAccess; }
    void accept(ASTVisitor &v) override { v.visit(*this); }
    std::shared_ptr<ASTNode> copy() const override;
};

struct Dereference : public Expression {
    ExprPtr pointer;
    Dereference(ExprPtr p) : pointer(std::move(p)) {}
    std::string str() const override {
        return "Dereference(" + pointer->toString() + ")";
    }
    NodeKind kind() const override {
        return NodeKind::Dereference;
    };
    void accept(ASTVisitor &v) override { v.visit(*this); }
    std::shared_ptr<ASTNode> copy() const override;
};
struct FuncCall : public Expression {
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
    std::shared_ptr<ASTNode> copy() const override;
};
struct MethodCall : public Expression {
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
    std::shared_ptr<ASTNode> copy() const override;
};

struct Access : Expression {};

struct EnumAccess : public Access {
    std::shared_ptr<Expression> enum_expr;
    std::string variant;
    EnumAccess(std::shared_ptr<Expression> e, std::string v) : enum_expr(e), variant(std::move(v)) {}
    std::string str() const override { return "EnumAccess(" + enum_expr->toString() + "::" + variant + ")"; }
    NodeKind kind() const override { return NodeKind::VarAccess; }
    void accept(ASTVisitor &v) override { v.visit(*this); }
    std::shared_ptr<ASTNode> copy() const override;
};

struct FieldAccess : public Access {
    ExprPtr base;
    std::string field;
    FieldAccess(ExprPtr b, std::string f)
        : base(std::move(b)), field(std::move(f)) {}
    std::string str() const override {
        return "FieldAccess(" + base->toString() + "." + field + ")";
    }
    NodeKind kind() const override { return NodeKind::FieldAccess; }
    void accept(ASTVisitor &v) override { v.visit(*this); }
    std::shared_ptr<ASTNode> copy() const override;
};

struct TemplateInstantiation : public Expression {
    std::string base;
    std::vector<std::shared_ptr<Type>> type_args;
    TemplateInstantiation(std::string b,
                          std::vector<std::shared_ptr<Type>> ta)
        : base(std::move(b)), type_args(std::move(ta)) {}
    std::string str() const override {
        std::string result = "TemplateInstantiation(" + base + "<";
        for (size_t i = 0; i < type_args.size(); ++i) {
            if (i > 0)
                result += ", ";
            result += type_args[i]->str();
        }
        result += ">)";
        return result;
    }
    NodeKind kind() const override { return NodeKind::TemplateInstantiation; }
    void accept(ASTVisitor &v) override { v.visit(*this); }
    std::shared_ptr<ASTNode> copy() const override;
};

struct ModuleAccess : public Access {
    std::vector<std::string> module_path;
    std::string member_name;
    ModuleAccess(std::vector<std::string> mp, std::string mn)
        : module_path(std::move(mp)), member_name(std::move(mn)) {}
    std::string str() const override {
        std::string path;
        for (const auto &part : module_path) {
            path += part + "::";
        }
        path += member_name;
        return "ModuleAccess(" + path + ")";
    }
    NodeKind kind() const override { return NodeKind::ModuleAccess; }
    void accept(ASTVisitor &v) override { v.visit(*this); }
    std::shared_ptr<ASTNode> copy() const override;
};

struct OffsetAccess : public Expression {
    ExprPtr base;
    ExprPtr index;
    OffsetAccess(ExprPtr b, ExprPtr i)
        : base(std::move(b)), index(std::move(i)) {}
    std::string str() const override {
        return "OffsetAccess(" + base->toString() + "[" + index->toString() + "])";
    }
    NodeKind kind() const override { return NodeKind::OffsetAccess; }
    void accept(ASTVisitor &v) override { v.visit(*this); }
    std::shared_ptr<ASTNode> copy() const override;
};

struct BinaryOperation : public Expression {
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
    std::shared_ptr<ASTNode> copy() const override;
};

struct UnaryOperation : public Expression {
    std::string op;
    ExprPtr operand;
    UnaryOperation(std::string o, ExprPtr e)
        : op(std::move(o)), operand(std::move(e)) {}
    std::string str() const override {
        return "UnaryOperation(" + op + operand->toString() + ")";
    }
    NodeKind kind() const override { return NodeKind::UnaryOp; }
    void accept(ASTVisitor &v) override { v.visit(*this); }
    std::shared_ptr<ASTNode> copy() const override;
};

using ConstValue = std::variant<uint64_t, int64_t, double, bool, std::string>;
struct Literal : public Expression {
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
                } else if constexpr (std::is_same_v<T, double>) {
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
    std::shared_ptr<ASTNode> copy() const override;
};

struct ArrayLiteral : public Expression {
    std::vector<ExprPtr> elements;
    size_t len = 0;
    ArrayLiteral(std::vector<ExprPtr> elems)
        : elements(std::move(elems)) {
        len = elements.size();
    }
    std::string str() const override {
        std::string result = "ArrayLiteral([ ";
        for (size_t i = 0; i < elements.size(); ++i) {
            if (i > 0)
                result += ", ";
            result += elements[i]->toString();
        }
        result += " ])";
        return result;
    }
    NodeKind kind() const override { return NodeKind::Literal; }
    void accept(ASTVisitor &v) override { v.visit(*this); }
    std::shared_ptr<ASTNode> copy() const override;
};

struct StructInitializer : public Expression {
    std::shared_ptr<Expression> struct_type_expr;
    std::map<std::string, std::shared_ptr<Expression>> field_values;

    std::shared_ptr<Type> struct_type;

    StructInitializer(std::shared_ptr<Expression> t,
                      std::map<std::string, std::shared_ptr<Expression>> f)
        : struct_type_expr(std::move(t)), field_values(std::move(f)) {}
    std::string str() const override {
        std::string result = "StructInitializer(" + struct_type_expr->str() + " { ";
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
    std::shared_ptr<ASTNode> copy() const override;
};

// ----------------- Statements -----------------

struct Block : public Statement {
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
    std::shared_ptr<ASTNode> copy() const override;
};

struct IfStatement : public Statement {
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
    std::shared_ptr<ASTNode> copy() const override;
};

struct ForStatement : public Statement {
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
    std::shared_ptr<ASTNode> copy() const override;
};

struct WhileStatement : public Statement {
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
    std::shared_ptr<ASTNode> copy() const override;
};

struct ReturnStatement : public Statement {
    ExprPtr value;
    bool is_error = false; // For error unions, set outside of constructor
    ReturnStatement(ExprPtr v) : value(std::move(v)) {}
    std::string str() const override {
        return "ReturnStatement(" + (value ? value->toString() : "void") + ")";
    }
    NodeKind kind() const override { return NodeKind::ReturnStmt; }
    void accept(ASTVisitor &v) override { v.visit(*this); }
    std::shared_ptr<ASTNode> copy() const override;
};

struct BreakStatement : public Statement {
    BreakStatement() {}
    std::string str() const override {
        return "BreakStatement()";
    }
    NodeKind kind() const override { return NodeKind::BreakStatement; }
    void accept(ASTVisitor &v) override { v.visit(*this); }
    std::shared_ptr<ASTNode> copy() const override;
};

struct ContinueStatement : public Statement {
    ContinueStatement() {}
    std::string str() const override {
        return "ContinueStatement()";
    }
    NodeKind kind() const override { return NodeKind::ContinueStatement; }
    void accept(ASTVisitor &v) override { v.visit(*this); }
    std::shared_ptr<ASTNode> copy() const override;
};

struct ExpressionStatement : public Statement {
    ExprPtr expression;
    ExpressionStatement(ExprPtr e) : expression(std::move(e)) {}
    std::string str() const override {
        return "ExpressionStatement(" + expression->toString() + ")";
    }
    NodeKind kind() const override { return NodeKind::ExpressionStatement; }
    void accept(ASTVisitor &v) override { v.visit(*this); }
    std::shared_ptr<ASTNode> copy() const override;
};

struct Assignment : public Statement {
    ExprPtr target;
    ExprPtr value;
    Assignment(ExprPtr t, ExprPtr v)
        : target(std::move(t)), value(std::move(v)) {}
    std::string str() const override {
        return "Assignment(" + target->toString() + " = " + value->toString() + ")";
    }
    NodeKind kind() const override { return NodeKind::Assignment; }
    void accept(ASTVisitor &v) override { v.visit(*this); }
    std::shared_ptr<ASTNode> copy() const override;
};

struct FunctionDeclaration : Declaration {
    bool is_extern;
    std::string name;
    std::shared_ptr<FunctionType> type;
    std::vector<std::string> param_names;
    std::shared_ptr<Statement> body;
    std::set<std::string> attributes;

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
    std::shared_ptr<ASTNode> copy() const override;
};

struct VariableDeclaration : Statement, Declaration {
    std::string name;
    std::shared_ptr<Type> var_type;
    ASTNodePtr initializer;
    bool is_const = false;
    bool is_extern = false;

    VariableDeclaration(std::string n, std::shared_ptr<Type> t, ASTNodePtr i)
        : name(std::move(n)), var_type(std::move(t)), initializer(std::move(i)) {}
    std::string str() const override {
        return "VariableDeclaration(" + name + " : " + (var_type != nullptr ? var_type->str() : "unknown") + " = " +
               (initializer ? initializer->toString() : "null") + (is_const ? ", const" : "") + ")";
    }
    NodeKind kind() const override { return NodeKind::VarDecl; }
    void accept(ASTVisitor &v) override { v.visit(*this); }
    std::shared_ptr<ASTNode> copy() const override;
};

struct TypeAliasDeclaration : Declaration {
    std::string name;
    std::shared_ptr<Type> aliased_type;

    TypeAliasDeclaration(std::string n, std::shared_ptr<Type> t)
        : name(std::move(n)), aliased_type(std::move(t)) {}
    std::string str() const override {
        return "TypeAliasDeclaration(" + name + " = " +
               (aliased_type ? aliased_type->str() : "null") + ")";
    }
    NodeKind kind() const override { return NodeKind::TypeAliasDeclaration; }
    void accept(ASTVisitor &v) override { v.visit(*this); }
    std::shared_ptr<ASTNode> copy() const override;
};

struct ImportDeclaration : Declaration {
    std::string path;
    std::string alias;

    ImportDeclaration(std::string p, std::string a) : path(std::move(p)), alias(std::move(a)) {}
    std::string str() const override {
        return "ImportDeclaration(\"" + path + "\", \"" + alias + "\")";
    }
    NodeKind kind() const override { return NodeKind::ImportDeclaration; }
    void accept(ASTVisitor &v) override { v.visit(*this); }
    std::shared_ptr<ASTNode> copy() const override;
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
    std::shared_ptr<Type> copy() const override {
        auto enum_type_copy = std::make_shared<EnumType>(name, base_type->copy());
        for (const auto &[variant, value] : variant_map) {
            enum_type_copy->addVariant(variant, std::dynamic_pointer_cast<Literal>(value->copy()));
        }
        return enum_type_copy;
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
    std::shared_ptr<ASTNode> copy() const override;
};

struct StructDeclaration : Declaration {
    bool is_extern = false;
    std::string name;
    std::vector<std::pair<std::string, std::shared_ptr<Type>>> fields;
    std::unordered_map<std::string, std::shared_ptr<FunctionDeclaration>> methods;
    std::vector<std::string> generic_params;

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
    std::shared_ptr<ASTNode> copy() const override;
};

struct UnionDeclaration : Declaration {
    bool is_extern = false;
    std::string name;
    std::vector<std::pair<std::string, std::shared_ptr<Type>>> fields;

    std::pair<std::string, std::shared_ptr<Type>>
    getField(const std::string &fname) const {
        for (const auto &field : fields) {
            if (field.first == fname)
                return field;
        }
        return {"", nullptr};
    }

    UnionDeclaration(
        std::string n,
        std::vector<std::pair<std::string, std::shared_ptr<Type>>> f)
        : name(std::move(n)), fields(std::move(f)) {}

    std::string str() const override {
        std::string result = "UnionDeclaration(" + name + ") { ";
        for (const auto &field : fields) {
            result += field.first + ": " + field.second->str() + "; ";
        }
        result += "}";
        return result;
    }

    NodeKind kind() const override { return NodeKind::UnionDecl; }
    void accept(ASTVisitor &v) override { v.visit(*this); }
    std::shared_ptr<ASTNode> copy() const override;
};

uint64_t getLitValue(const std::shared_ptr<Literal> &lit);
void setLitVal(std::shared_ptr<Literal> lit, uint64_t raw_val);
int getTypeSize(const std::shared_ptr<Type> &type);

#endif
