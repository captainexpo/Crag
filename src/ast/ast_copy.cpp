#include "ast.h"

void ASTNode::accept(ASTVisitor &v) {}

std::shared_ptr<ASTNode> ASTNode::copy() const {
    return std::make_shared<ASTNode>(*this);
}

std::shared_ptr<ASTNode> Program::copy() const {
    auto c = std::make_shared<Program>();
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    for (const auto &decl : declarations) {
        c->declarations.push_back(decl->copy());
    }
    return c;
}

std::shared_ptr<ASTNode> TypeCast::copy() const {
    auto c = std::make_shared<TypeCast>(
        std::dynamic_pointer_cast<Expression>(expr->copy()),
        target_type != nullptr ? target_type->copy() : nullptr,
        cast_type);

    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    return c;
}

std::shared_ptr<ASTNode> VarAccess::copy() const {
    auto c = std::make_shared<VarAccess>(name);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    c->is_extern = is_extern;
    return c;
}

std::shared_ptr<ASTNode> Dereference::copy() const {
    auto c = std::make_shared<Dereference>(
        std::dynamic_pointer_cast<Expression>(pointer->copy()));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    return c;
}

std::shared_ptr<ASTNode> FuncCall::copy() const {
    std::vector<ExprPtr> args_copy;
    for (const auto &arg : args) {
        args_copy.push_back(std::dynamic_pointer_cast<Expression>(arg->copy()));
    }
    auto c = std::make_shared<FuncCall>(
        std::dynamic_pointer_cast<Expression>(func->copy()),
        std::move(args_copy));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    return c;
}

std::shared_ptr<ASTNode> MethodCall::copy() const {
    std::vector<ExprPtr> args_copy;
    for (const auto &arg : args) {
        args_copy.push_back(std::dynamic_pointer_cast<Expression>(arg->copy()));
    }
    auto c = std::make_shared<MethodCall>(
        std::dynamic_pointer_cast<Expression>(object->copy()),
        method,
        std::move(args_copy));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    return c;
}

std::shared_ptr<ASTNode> EnumAccess::copy() const {
    auto c = std::make_shared<EnumAccess>(
        std::dynamic_pointer_cast<Expression>(enum_expr->copy()),
        variant);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    return c;
}

std::shared_ptr<ASTNode> FieldAccess::copy() const {
    auto c = std::make_shared<FieldAccess>(
        std::dynamic_pointer_cast<Expression>(base->copy()),
        field);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    return c;
}

std::shared_ptr<ASTNode> TemplateInstantiation::copy() const {
    auto c = std::make_shared<TemplateInstantiation>(base, type_args);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    return c;
}

std::shared_ptr<ASTNode> ModuleAccess::copy() const {
    auto c = std::make_shared<ModuleAccess>(module_path, member_name);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    return c;
}

std::shared_ptr<ASTNode> OffsetAccess::copy() const {
    auto c = std::make_shared<OffsetAccess>(
        std::dynamic_pointer_cast<Expression>(base->copy()),
        std::dynamic_pointer_cast<Expression>(index->copy()));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    return c;
}

std::shared_ptr<ASTNode> BinaryOperation::copy() const {
    auto c = std::make_shared<BinaryOperation>(
        std::dynamic_pointer_cast<Expression>(left->copy()),
        op,
        std::dynamic_pointer_cast<Expression>(right->copy()));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    return c;
}

std::shared_ptr<ASTNode> UnaryOperation::copy() const {
    auto c = std::make_shared<UnaryOperation>(
        op,
        std::dynamic_pointer_cast<Expression>(operand->copy()));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    return c;
}

std::shared_ptr<ASTNode> Literal::copy() const {
    auto c = std::make_shared<Literal>(value, lit_type);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    return c;
}

std::shared_ptr<ASTNode> ArrayLiteral::copy() const {
    std::vector<ExprPtr> elements_copy;
    for (const auto &elem : elements) {
        elements_copy.push_back(std::dynamic_pointer_cast<Expression>(elem->copy()));
    }
    auto c = std::make_shared<ArrayLiteral>(std::move(elements_copy));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    c->len = len;
    return c;
}

std::shared_ptr<ASTNode> StructInitializer::copy() const {
    std::map<std::string, std::shared_ptr<Expression>> field_values_copy;
    for (const auto &[name, expr] : field_values) {
        field_values_copy[name] = std::dynamic_pointer_cast<Expression>(expr->copy());
    }
    auto c = std::make_shared<StructInitializer>(
        std::dynamic_pointer_cast<Expression>(struct_type_expr->copy()),
        std::move(field_values_copy));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    c->struct_type = struct_type != nullptr ? struct_type->copy() : nullptr;
    return c;
}

std::shared_ptr<ASTNode> Block::copy() const {
    auto c = std::make_shared<Block>();
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    for (const auto &stmt : statements) {
        c->statements.push_back(std::dynamic_pointer_cast<Statement>(stmt->copy()));
    }
    return c;
}

std::shared_ptr<ASTNode> IfStatement::copy() const {
    auto c = std::make_shared<IfStatement>(
        std::dynamic_pointer_cast<Expression>(condition->copy()),
        std::dynamic_pointer_cast<Statement>(then_branch->copy()),
        else_branch ? std::dynamic_pointer_cast<Statement>(else_branch->copy()) : nullptr);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    return c;
}

std::shared_ptr<ASTNode> ForStatement::copy() const {
    auto c = std::make_shared<ForStatement>(
        init ? std::dynamic_pointer_cast<Statement>(init->copy()) : nullptr,
        condition ? std::dynamic_pointer_cast<Expression>(condition->copy()) : nullptr,
        increment ? std::dynamic_pointer_cast<Statement>(increment->copy()) : nullptr,
        std::dynamic_pointer_cast<Statement>(body->copy()));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    return c;
}

std::shared_ptr<ASTNode> WhileStatement::copy() const {
    auto c = std::make_shared<WhileStatement>(
        std::dynamic_pointer_cast<Expression>(condition->copy()),
        std::dynamic_pointer_cast<Statement>(body->copy()));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    return c;
}

std::shared_ptr<ASTNode> ReturnStatement::copy() const {
    auto c = std::make_shared<ReturnStatement>(
        value ? std::dynamic_pointer_cast<Expression>(value->copy()) : nullptr);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    c->is_error = is_error;
    return c;
}

std::shared_ptr<ASTNode> BreakStatement::copy() const {
    auto c = std::make_shared<BreakStatement>();
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    return c;
}

std::shared_ptr<ASTNode> ContinueStatement::copy() const {
    auto c = std::make_shared<ContinueStatement>();
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    return c;
}

std::shared_ptr<ASTNode> ExpressionStatement::copy() const {
    auto c = std::make_shared<ExpressionStatement>(
        std::dynamic_pointer_cast<Expression>(expression->copy()));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    return c;
}

std::shared_ptr<ASTNode> Assignment::copy() const {
    auto c = std::make_shared<Assignment>(
        std::dynamic_pointer_cast<Expression>(target->copy()),
        std::dynamic_pointer_cast<Expression>(value->copy()));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    return c;
}

std::shared_ptr<ASTNode> FunctionDeclaration::copy() const {
    auto c = std::make_shared<FunctionDeclaration>(
        name,
        type != nullptr ? std::dynamic_pointer_cast<FunctionType>(type->copy()) : nullptr,
        param_names,
        body ? std::dynamic_pointer_cast<Statement>(body->copy()) : nullptr,
        is_extern);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    c->is_pub = is_pub;
    c->generic_params = generic_params;
    c->attributes = attributes;
    return c;
}

std::shared_ptr<ASTNode> VariableDeclaration::copy() const {
    auto c = std::make_shared<VariableDeclaration>(
        name,
        var_type != nullptr ? var_type->copy() : nullptr,
        initializer ? initializer->copy() : nullptr);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    c->is_pub = is_pub;
    c->generic_params = generic_params;
    c->is_const = is_const;
    c->is_extern = is_extern;
    return c;
}

std::shared_ptr<ASTNode> TypeAliasDeclaration::copy() const {
    auto c = std::make_shared<TypeAliasDeclaration>(name, aliased_type);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    c->is_pub = is_pub;
    c->generic_params = generic_params;
    return c;
}

std::shared_ptr<ASTNode> ImportDeclaration::copy() const {
    auto c = std::make_shared<ImportDeclaration>(path, alias);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    c->is_pub = is_pub;
    c->generic_params = generic_params;
    return c;
}

std::shared_ptr<ASTNode> TypeExpression::copy() const {
    auto c = std::make_shared<TypeExpression>(type != nullptr ? type->copy() : nullptr);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    return c;
}

std::shared_ptr<ASTNode> EnumDeclaration::copy() const {
    std::unordered_map<std::string, std::shared_ptr<Literal>> variants_copy;
    for (const auto &[name, lit] : variants) {
        variants_copy[name] = std::dynamic_pointer_cast<Literal>(lit->copy());
    }
    auto c = std::make_shared<EnumDeclaration>(name, base_type != nullptr ? std::dynamic_pointer_cast<Type>(base_type->copy()) : nullptr, std::move(variants_copy));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    c->is_pub = is_pub;
    c->generic_params = generic_params;
    c->enum_type = enum_type != nullptr ? std::dynamic_pointer_cast<EnumType>(enum_type->copy()) : nullptr;
    return c;
}

std::shared_ptr<ASTNode> StructDeclaration::copy() const {
    auto c = std::make_shared<StructDeclaration>(name, fields);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    c->is_pub = is_pub;
    c->generic_params = generic_params;
    c->is_extern = is_extern;
    for (const auto &[mname, method] : methods) {
        c->methods[mname] = std::dynamic_pointer_cast<FunctionDeclaration>(method->copy());
    }
    return c;
}

std::shared_ptr<ASTNode> UnionDeclaration::copy() const {
    auto c = std::make_shared<UnionDeclaration>(name, fields);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type;
    c->is_pub = is_pub;
    c->generic_params = generic_params;
    c->is_extern = is_extern;
    return c;
}
