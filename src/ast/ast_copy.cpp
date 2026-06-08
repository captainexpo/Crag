#include "ast.h"
#include <memory>

void ASTNode::accept(ASTVisitor &v) {}

std::shared_ptr<ASTNode> ASTNode::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    return std::make_shared<ASTNode>(*this);
}

std::shared_ptr<ASTNode> Program::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<Program>();
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    for (const auto &decl : declarations) {
        c->declarations.push_back(decl->instantiate(gp_replace));
    }
    return c;
}

std::shared_ptr<ASTNode> TypeCast::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<TypeCast>(
        std::dynamic_pointer_cast<Expression>(expr->instantiate(gp_replace)),
        target_type != nullptr ? target_type->instantiate(gp_replace) : nullptr,
        cast_type);

    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    return c;
}

std::shared_ptr<ASTNode> VarAccess::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<VarAccess>(name);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    c->is_extern = is_extern;
    return c;
}

std::shared_ptr<ASTNode> Dereference::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<Dereference>(
        std::dynamic_pointer_cast<Expression>(pointer->instantiate(gp_replace)));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    return c;
}

std::shared_ptr<ASTNode> FuncCall::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    std::vector<ExprPtr> args_copy;
    for (const auto &arg : args) {
        args_copy.push_back(std::dynamic_pointer_cast<Expression>(arg->instantiate(gp_replace)));
    }
    auto c = std::make_shared<FuncCall>(
        std::dynamic_pointer_cast<Expression>(func->instantiate(gp_replace)),
        std::move(args_copy));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    return c;
}

std::shared_ptr<ASTNode> MethodCall::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    std::vector<ExprPtr> args_copy;
    for (const auto &arg : args) {
        args_copy.push_back(std::dynamic_pointer_cast<Expression>(arg->instantiate(gp_replace)));
    }
    auto c = std::make_shared<MethodCall>(
        std::dynamic_pointer_cast<Expression>(object->instantiate(gp_replace)),
        method,
        std::move(args_copy));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    return c;
}

std::shared_ptr<ASTNode> EnumAccess::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<EnumAccess>(
        std::dynamic_pointer_cast<Expression>(enum_expr->instantiate(gp_replace)),
        variant);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    return c;
}

std::shared_ptr<ASTNode> FieldAccess::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<FieldAccess>(
        std::dynamic_pointer_cast<Expression>(base->instantiate(gp_replace)),
        field);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    return c;
}

std::shared_ptr<ASTNode> TemplateInstantiation::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<TemplateInstantiation>(module_path, template_name, type_args);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    c->template_id = template_id;
    return c;
}

std::shared_ptr<ASTNode> ModuleAccess::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<ModuleAccess>(module_path, member_name);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    return c;
}

std::shared_ptr<ASTNode> OffsetAccess::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<OffsetAccess>(
        std::dynamic_pointer_cast<Expression>(base->instantiate(gp_replace)),
        std::dynamic_pointer_cast<Expression>(index->instantiate(gp_replace)));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    return c;
}

std::shared_ptr<ASTNode> BinaryOperation::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<BinaryOperation>(
        std::dynamic_pointer_cast<Expression>(left->instantiate(gp_replace)),
        op,
        std::dynamic_pointer_cast<Expression>(right->instantiate(gp_replace)));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    return c;
}

std::shared_ptr<ASTNode> UnaryOperation::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<UnaryOperation>(
        op,
        std::dynamic_pointer_cast<Expression>(operand->instantiate(gp_replace)));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    return c;
}

std::shared_ptr<ASTNode> Literal::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<Literal>(value, lit_type);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    return c;
}

std::shared_ptr<ASTNode> ArrayLiteral::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    std::vector<ExprPtr> elements_copy;
    for (const auto &elem : elements) {
        elements_copy.push_back(std::dynamic_pointer_cast<Expression>(elem->instantiate(gp_replace)));
    }
    auto c = std::make_shared<ArrayLiteral>(std::move(elements_copy));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    c->len = len;
    return c;
}

std::shared_ptr<ASTNode> StructInitializer::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    std::map<std::string, std::shared_ptr<Expression>> field_values_copy;
    for (const auto &[name, expr] : field_values) {
        field_values_copy[name] = std::dynamic_pointer_cast<Expression>(expr->instantiate(gp_replace));
    }
    auto c = std::make_shared<StructInitializer>(
        std::dynamic_pointer_cast<Expression>(struct_type_expr->instantiate(gp_replace)),
        std::move(field_values_copy));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    c->struct_type = struct_type != nullptr ? struct_type->instantiate(gp_replace) : nullptr;
    return c;
}

std::shared_ptr<ASTNode> Block::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<Block>();
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    for (const auto &stmt : statements) {
        c->statements.push_back(std::dynamic_pointer_cast<Statement>(stmt->instantiate(gp_replace)));
    }
    return c;
}

std::shared_ptr<ASTNode> IfStatement::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<IfStatement>(
        std::dynamic_pointer_cast<Expression>(condition->instantiate(gp_replace)),
        std::dynamic_pointer_cast<Statement>(then_branch->instantiate(gp_replace)),
        else_branch ? std::dynamic_pointer_cast<Statement>(else_branch->instantiate(gp_replace)) : nullptr);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    return c;
}

std::shared_ptr<ASTNode> ForStatement::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<ForStatement>(
        init ? std::dynamic_pointer_cast<Statement>(init->instantiate(gp_replace)) : nullptr,
        condition ? std::dynamic_pointer_cast<Expression>(condition->instantiate(gp_replace)) : nullptr,
        increment ? std::dynamic_pointer_cast<Statement>(increment->instantiate(gp_replace)) : nullptr,
        std::dynamic_pointer_cast<Statement>(body->instantiate(gp_replace)));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    return c;
}

std::shared_ptr<ASTNode> WhileStatement::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<WhileStatement>(
        std::dynamic_pointer_cast<Expression>(condition->instantiate(gp_replace)),
        std::dynamic_pointer_cast<Statement>(body->instantiate(gp_replace)));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    return c;
}

std::shared_ptr<ASTNode> ReturnStatement::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<ReturnStatement>(
        value ? std::dynamic_pointer_cast<Expression>(value->instantiate(gp_replace)) : nullptr);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    c->is_error = is_error;
    return c;
}

std::shared_ptr<ASTNode> BreakStatement::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<BreakStatement>();
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    return c;
}

std::shared_ptr<ASTNode> ContinueStatement::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<ContinueStatement>();
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    return c;
}

std::shared_ptr<ASTNode> ExpressionStatement::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<ExpressionStatement>(
        std::dynamic_pointer_cast<Expression>(expression->instantiate(gp_replace)));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    return c;
}

std::shared_ptr<ASTNode> Assignment::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<Assignment>(
        std::dynamic_pointer_cast<Expression>(target->instantiate(gp_replace)),
        std::dynamic_pointer_cast<Expression>(value->instantiate(gp_replace)));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    return c;
}

std::shared_ptr<ASTNode> FunctionDeclaration::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<FunctionDeclaration>(
        name,
        type != nullptr ? std::dynamic_pointer_cast<FunctionType>(type->instantiate(gp_replace)) : nullptr,
        param_names,
        body ? std::dynamic_pointer_cast<Statement>(body->instantiate(gp_replace)) : nullptr,
        is_extern);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    c->is_pub = is_pub;
    c->generic_params = generic_params;
    c->attributes = attributes;
    return c;
}

std::shared_ptr<ASTNode> VariableDeclaration::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<VariableDeclaration>(
        name,
        var_type != nullptr ? var_type->instantiate(gp_replace) : nullptr,
        initializer ? initializer->instantiate(gp_replace) : nullptr);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    c->is_pub = is_pub;
    c->is_const = is_const;
    c->is_extern = is_extern;
    return c;
}

std::shared_ptr<ASTNode> TypeAliasDeclaration::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<TypeAliasDeclaration>(name, aliased_type);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    c->is_pub = is_pub;
    c->generic_params = generic_params;
    return c;
}

std::shared_ptr<ASTNode> ImportDeclaration::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<ImportDeclaration>(path, alias);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    c->is_pub = is_pub;
    return c;
}

std::shared_ptr<ASTNode> TypeExpression::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<TypeExpression>(type != nullptr ? type->instantiate(gp_replace) : nullptr);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    return c;
}

std::shared_ptr<ASTNode> EnumDeclaration::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    std::unordered_map<std::string, std::shared_ptr<Literal>> variants_copy;
    for (const auto &[name, lit] : variants) {
        variants_copy[name] = std::dynamic_pointer_cast<Literal>(lit->instantiate(gp_replace));
    }
    auto c = std::make_shared<EnumDeclaration>(name, base_type != nullptr ? std::dynamic_pointer_cast<Type>(base_type->instantiate(gp_replace)) : nullptr, std::move(variants_copy));
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    c->is_pub = is_pub;
    c->generic_params = generic_params;
    c->enum_type = enum_type != nullptr ? std::dynamic_pointer_cast<EnumType>(enum_type->instantiate(gp_replace)) : nullptr;
    return c;
}

std::shared_ptr<ASTNode> StructDeclaration::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    std::vector<std::pair<std::string, std::shared_ptr<Type>>> fields_copy;
    for (const auto &field : fields) {
        fields_copy.emplace_back(field.first, field.second->instantiate(gp_replace));
    }
    auto c = std::make_shared<StructDeclaration>(name, fields_copy);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    c->is_pub = is_pub;
    c->generic_params = generic_params;
    c->is_extern = is_extern;
    for (const auto &[mname, method] : methods) {
        c->methods[mname] = std::dynamic_pointer_cast<FunctionDeclaration>(method->instantiate(gp_replace));
    }
    // std::cout << "Copied struct declaration: " << c->str() << "\n";
    return c;
}

std::shared_ptr<ASTNode> UnionDeclaration::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    std::vector<std::pair<std::string, std::shared_ptr<Type>>> fields_copy;
    for (const auto &field : fields) {
        fields_copy.emplace_back(field.first, field.second->instantiate(gp_replace));
    }
    auto c = std::make_shared<UnionDeclaration>(name, fields_copy);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    c->is_pub = is_pub;
    c->generic_params = generic_params;
    c->is_extern = is_extern;
    return c;
}

std::shared_ptr<ASTNode> AsmStmt::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    auto c = std::make_shared<AsmStmt>(is_volatile, template_str, operands, options);
    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    return c;
}


std::shared_ptr<ASTNode> WhenBlock::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    std::vector<ASTNodePtr> containees;
    for (const auto &containee : containees) {
        containees.push_back(containee->instantiate(gp_replace));
    }
    return std::make_shared<WhenBlock>(std::dynamic_pointer_cast<Expression>(condition->instantiate(gp_replace)), std::move(containees));
}

std::shared_ptr<ASTNode> SwitchStmt::instantiate(std::vector<std::shared_ptr<Type>> gp_replace) const {
    std::vector<std::pair<std::vector<std::shared_ptr<Expression>>, std::shared_ptr<Statement>>> cases_copy;

    for (const auto &[case_exprs, case_stmt] : cases) {
        std::vector<std::shared_ptr<Expression>> case_exprs_copy;
        for (const auto &expr : case_exprs) {
            case_exprs_copy.push_back(std::dynamic_pointer_cast<Expression>(expr->instantiate(gp_replace)));
        }
        cases_copy.emplace_back(std::move(case_exprs_copy), std::dynamic_pointer_cast<Statement>(case_stmt->instantiate(gp_replace)));
    }

    auto c = std::make_shared<SwitchStmt>(
        std::dynamic_pointer_cast<Expression>(condition->instantiate(gp_replace)),
        std::move(cases_copy),
        default_case ? std::dynamic_pointer_cast<Statement>(default_case->instantiate(gp_replace)) : nullptr);

    c->line = line;
    c->col = col;
    c->inferred_type = inferred_type != nullptr ? inferred_type->instantiate(gp_replace) : nullptr;
    return c;
}

