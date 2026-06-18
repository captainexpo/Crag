
#include "../ast/ast.h"
#include "src/typechecking/tables.h"
#include "typecheck.h"
#include <cassert>
#include <cstring>
#include <memory>


std::string mangleTemplateName(const std::string &base, const std::vector<std::shared_ptr<Type>> &params) {
    std::string mangled = base + "<";
    for (size_t i = 0; i < params.size(); ++i) {
        mangled += params[i]->str();
        if (i + 1 < params.size())
            mangled += ",";
    }
    mangled += ">";
    return mangled;
}

static void replaceInTypePreserveTemplates(std::shared_ptr<Type> &type,
                                           const std::unordered_map<std::string, std::shared_ptr<Type>> &generic_map) {
    if (auto gt = std::dynamic_pointer_cast<GenericType>(type)) {
        auto it = generic_map.find(gt->name);
        if (it != generic_map.end()) {
            type = it->second;
        }
    } else if (auto eu = std::dynamic_pointer_cast<ErrorUnionType>(type)) {
        replaceInTypePreserveTemplates(eu->errorType, generic_map);
        replaceInTypePreserveTemplates(eu->valueType, generic_map);
    } else if (auto ti = std::dynamic_pointer_cast<TemplateInstanceType>(type)) {
        for (auto &arg : ti->type_args) {
            replaceInTypePreserveTemplates(arg, generic_map);
        }
        replaceInTypePreserveTemplates(ti->base, generic_map);
    } else if (auto ft = std::dynamic_pointer_cast<FunctionType>(type)) {
        for (auto &param : ft->params) {
            replaceInTypePreserveTemplates(param, generic_map);
        }
        replaceInTypePreserveTemplates(ft->ret, generic_map);
    } else if (auto pt = std::dynamic_pointer_cast<PointerType>(type)) {
        replaceInTypePreserveTemplates(pt->base, generic_map);
    } else if (auto at = std::dynamic_pointer_cast<ArrayType>(type)) {
        replaceInTypePreserveTemplates(at->element_type, generic_map);
    } else if (auto st = std::dynamic_pointer_cast<StructType>(type)) {
        for (auto &field : st->fields) {
            replaceInTypePreserveTemplates(field.second, generic_map);
        }
    } else if (auto ut = std::dynamic_pointer_cast<UnionType>(type)) {
        for (auto &variant : ut->fields) {
            replaceInTypePreserveTemplates(variant.second, generic_map);
        }
    }
}

static std::string formatQualifiedTemplateName(const std::vector<std::string> &module_path,
                                               const std::string &template_name) {
    std::string qualified;
    for (const auto &part : module_path) {
        qualified += part + "::";
    }
    qualified += template_name;
    return qualified;
}

static std::string templateDeclName(const std::shared_ptr<Declaration> &decl) {
    if (auto fd = std::dynamic_pointer_cast<FunctionDeclaration>(decl))
        return fd->name;
    if (auto sd = std::dynamic_pointer_cast<StructDeclaration>(decl))
        return sd->name;
    if (auto ta = std::dynamic_pointer_cast<TypeAliasDeclaration>(decl))
        return ta->name;
    return "<unknown>";
}

static bool isTemplateDeclaration(const std::shared_ptr<Declaration> &decl) {
    if (auto fd = std::dynamic_pointer_cast<FunctionDeclaration>(decl))
        return !fd->generic_params.empty();
    if (auto sd = std::dynamic_pointer_cast<StructDeclaration>(decl))
        return !sd->generic_params.empty();
    if (auto ta = std::dynamic_pointer_cast<TypeAliasDeclaration>(decl))
        return !ta->generic_params.empty();
    if (auto ud = std::dynamic_pointer_cast<UnionDeclaration>(decl))
        return !ud->generic_params.empty();
    if (auto ed = std::dynamic_pointer_cast<EnumDeclaration>(decl))
        return !ed->generic_params.empty();
    return false;
}

// Returns type of expression, and replacement for the instantiation
std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> TypeChecker::inferTemplateInstantiation(const std::shared_ptr<TemplateInstantiation> &ti) {
    auto target_mod = resolveModulePath(ti, ti->module_path);

    auto og_current_mod = current_module;
    current_module = target_mod;
    CurrentModuleGuard guard(this, og_current_mod);

    std::vector<std::shared_ptr<Type>> params;
    params.reserve(ti->type_args.size());
    for (const auto &arg : ti->type_args) {
        params.push_back(resolveType(ti, arg));
    }

    SymbolId template_id = currentScopes().front().find(ti->template_name);
    if (template_id == INVALID_SYMBOL_ID) {
        throw TypeCheckError(current_module, ti, "Template '" + ti->template_name + "' not found");
        return {nullptr, nullptr};
    }
    auto entry = symbolTable().get(template_id);

    // symbol_table.dump();

    auto decl = entry ? std::dynamic_pointer_cast<Declaration>(entry->decl) : nullptr;

    if (!decl) {
        throw TypeCheckError(current_module, ti, "Unknown template: " + ti->template_name);
    }

    if (auto alias_decl = std::dynamic_pointer_cast<TypeAliasDeclaration>(decl)) {
        if (alias_decl->generic_params.size() != params.size()) {
            throw TypeCheckError(current_module, ti, "Template instantiation parameter count mismatch for " + templateDeclName(decl) +
                                        ": expected " + std::to_string(alias_decl->generic_params.size()) +
                                        ", got " + std::to_string(params.size()));
        }

        std::unordered_map<std::string, std::shared_ptr<Type>> generic_map;
        for (size_t i = 0; i < alias_decl->generic_params.size(); ++i) {
            generic_map[alias_decl->generic_params[i]] = params[i];
        }
        auto alias_type = alias_decl->aliased_type->instantiate();
        replaceInTypePreserveTemplates(alias_type, generic_map);
        auto resolved = resolveType(alias_decl, alias_type);

        SymbolId inst_id = INVALID_SYMBOL_ID;
        if (auto st = std::dynamic_pointer_cast<StructType>(resolved)) {
            auto complete = lookupStructType(st->name);
            if (complete) {
                resolved = complete;
                insertSymbol(st->name, SymbolKind::Type, complete, nullptr, &inst_id);
            }
        }

        ti->inferred_type = resolved;

        if (inst_id != INVALID_SYMBOL_ID) {
            ti->symbol_id = inst_id;
        }

        return std::make_pair(resolved, std::make_shared<TypeExpression>(resolved));
    }

    auto to_instantiate = decl;

    if (auto fd = std::dynamic_pointer_cast<FunctionDeclaration>(to_instantiate)) {
        auto new_name = mangleTemplateName(fd->name, params);
        // std::cout << "Instantiating function template: " << fd->name << " as " << new_name << "\n";
        // Check if already instantiated
        auto existing_maybe = lookupNamedSymbol(new_name);

        if (existing_maybe) {
            auto existing = *existing_maybe;
            auto ftype = std::dynamic_pointer_cast<FunctionType>(existing.type);
            ti->inferred_type = ftype;
            auto va = std::make_shared<VarAccess>(new_name);
            va->inferred_type = ftype;
            va->symbol_id = existing.id;
            va->line = ti->line;
            va->col = ti->col;

            return std::make_pair(
                ti->inferred_type,
                va);
        }

        std::shared_ptr<FunctionDeclaration> declCopy = std::dynamic_pointer_cast<FunctionDeclaration>(fd->instantiate());
        if (fd->generic_params.size() != params.size()) {
            throw TypeCheckError(current_module, ti, "Template instantiation parameter count mismatch for " + fd->name +
                                         ": expected " + std::to_string(fd->generic_params.size()) +
                                         ", got " + std::to_string(params.size()));
        }
        std::unordered_map<std::string, std::shared_ptr<Type>> generic_map;
        for (size_t i = 0; i < fd->generic_params.size(); ++i) {
            generic_map[fd->generic_params[i]] = params[i];
        }
        // std::cout << "Generic map:\n";
        // for (const auto &gm : generic_map) {
        //     std::cout << "  " << gm.first << " -> " << gm.second->str() << "\n";
        // }
        replaceGenericTypes(declCopy, generic_map);
        // std::cout << declCopy->str() << "\n";
        declCopy->name = new_name;
        // Clear generic_params since this is now an instantiated function, not a template
        declCopy->generic_params.clear();

        auto saved_expected_return_type = m_expected_return_type;

        checkFunctionDeclaration(declCopy);

        m_expected_return_type = saved_expected_return_type;

        ti->inferred_type = declCopy->type;

        SymbolId inst_id = INVALID_SYMBOL_ID;
        insertSymbol(new_name, SymbolKind::Function, ti->inferred_type, declCopy, &inst_id);
        declCopy->symbol_id = inst_id;

        ti->inferred_type = declCopy->type;

        current_module->ast->declarations.push_back(declCopy);

        ti->symbol_id = inst_id;

        auto va = std::make_shared<VarAccess>(new_name);
        va->inferred_type = ti->inferred_type;
        va->symbol_id = inst_id;
        va->line = ti->line;
        va->col = ti->col;

        return std::make_pair(
            ti->inferred_type,
            va);
    } else if (auto sd = std::dynamic_pointer_cast<StructDeclaration>(to_instantiate)) {
        auto new_name = mangleTemplateName(sd->name, params);
        // Check if already instantiated
        auto existing = lookupStructType(new_name);
        if (existing) {
            ti->inferred_type = existing;
            return std::make_pair(
                ti->inferred_type,
                std::make_shared<TypeExpression>(ti->inferred_type));
        }

        for (const auto &field : sd->fields) {
            std::cout << "Field: " << field.first << " : " << field.second->str() << "\n";
        }
        for (const auto &method: sd->methods) {
            std::cout << "Method: " << method.first<< " : " << method.second->type->str() << "\n";
        }
        std::shared_ptr<StructDeclaration> declCopy = std::dynamic_pointer_cast<StructDeclaration>(sd->instantiate());
        if (sd->generic_params.size() != params.size()) {
            throw TypeCheckError(current_module, ti, "Template instantiation parameter count mismatch for " + sd->name +
                                         ": expected " + std::to_string(sd->generic_params.size()) +
                                         ", got " + std::to_string(params.size()));
        }
        std::unordered_map<std::string, std::shared_ptr<Type>> generic_map;
        for (size_t i = 0; i < sd->generic_params.size(); ++i) {
            generic_map[sd->generic_params[i]] = params[i];
        }
        replaceGenericTypes(declCopy, generic_map);
        declCopy->name = new_name;
        // Clear generic_params since this is now an instantiated struct, not a template
        declCopy->generic_params.clear();

        auto st = std::make_shared<StructType>(new_name);
        st->complete = false;
        SymbolId inst_id;
        insertSymbol(new_name, SymbolKind::Type, st, declCopy, &inst_id);

        auto old_expected_return_type = m_expected_return_type;
        m_expected_return_type = nullptr;
        for (const auto &field : declCopy->fields) {
            std::cout << "Field: " << field.first << " : " << field.second->str() << "\n";
        }
        for (const auto &method: declCopy->methods) {
            std::cout << "Method: " << method.first<< " : " << method.second->type->str() << "\n";
        }
        checkStructDeclaration(declCopy);
        // std::cout << "TYP " << st->str() << "\n";
        current_module->ast->declarations.push_back(declCopy);

        declCopy->symbol_id = inst_id;
        ti->inferred_type = st;
        ti->template_id = inst_id;
        ti->symbol_id = inst_id;
        m_expected_return_type = old_expected_return_type;
        return std::make_pair(
            ti->inferred_type,
            std::make_shared<TypeExpression>(ti->inferred_type));
    }
    throw TypeCheckError(current_module, ti, "Unsupported template kind: " + to_instantiate->str());
}

void replaceInType(std::shared_ptr<Type> &type, const std::unordered_map<std::string, std::shared_ptr<Type>> &generic_map) {
    if (auto gt = std::dynamic_pointer_cast<GenericType>(type)) {
        auto it = generic_map.find(gt->name);
        if (it != generic_map.end()) {
            type = it->second;
        }
    } else if (auto eu = std::dynamic_pointer_cast<ErrorUnionType>(type)) {
        replaceInType(eu->errorType, generic_map);
        replaceInType(eu->valueType, generic_map);
    } else if (auto ti = std::dynamic_pointer_cast<TemplateInstanceType>(type)) {
        for (auto &arg : ti->type_args) {
            replaceInType(arg, generic_map);
        }

        if (auto base_struct = std::dynamic_pointer_cast<StructType>(ti->base)) {
            std::string mangled_name = mangleTemplateName(base_struct->name, ti->type_args);

            type = std::make_shared<StructType>(mangled_name);
        } else {
            replaceInType(ti->base, generic_map);
        }
    } else if (auto ft = std::dynamic_pointer_cast<FunctionType>(type)) {
        for (auto &param : ft->params) {
            replaceInType(param, generic_map);
        }
        replaceInType(ft->ret, generic_map);
    } else if (auto pt = std::dynamic_pointer_cast<PointerType>(type)) {
        replaceInType(pt->base, generic_map);
    } else if (auto at = std::dynamic_pointer_cast<ArrayType>(type)) {
        replaceInType(at->element_type, generic_map);
    } else if (auto st = std::dynamic_pointer_cast<StructType>(type)) {
        for (auto &field : st->fields) {
            replaceInType(field.second, generic_map);
        }
    } else if (auto ut = std::dynamic_pointer_cast<UnionType>(type)) {
        for (auto &variant : ut->fields) {
            replaceInType(variant.second, generic_map);
        }
    } else if (auto et = std::dynamic_pointer_cast<EnumType>(type)) {
        return;
    } else
        return;
}

void replaceGenericTypes(std::shared_ptr<ASTNode> node, const std::unordered_map<std::string, std::shared_ptr<Type>> &generic_map) {
    if (!node)
        return;

    if (auto ea = std::dynamic_pointer_cast<EnumAccess>(node)) {
        return;
    }

    if (auto te = std ::dynamic_pointer_cast<TypeExpression>(node)) {
        replaceInType(te->type, generic_map);
        return;
    }

    if (auto ai = std::dynamic_pointer_cast<ArrayLiteral>(node)) {
        for (auto &elem : ai->elements) {
            replaceGenericTypes(elem, generic_map);
        }
        return;
    }

    if (auto fd = std::dynamic_pointer_cast<FunctionDeclaration>(node)) {
        // Replace parameter types
        if (fd->type) {
            for (auto &param : fd->type->params) {
                replaceInType(param, generic_map);
            }
            // Replace return type
            replaceInType(fd->type->ret, generic_map);
        }
        // Process function body
        if (fd->body) {
            replaceGenericTypes(fd->body, generic_map);
        }
        return;
    }

    if (auto block = std::dynamic_pointer_cast<Block>(node)) {
        for (auto &stmt : block->statements) {
            replaceGenericTypes(stmt, generic_map);
        }
        return;
    }

    if (auto vd = std::dynamic_pointer_cast<VariableDeclaration>(node)) {
        // Replace variable type
        if (vd->var_type) {
            replaceInType(vd->var_type, generic_map);
        }
        if (vd->initializer) {
            replaceGenericTypes(vd->initializer, generic_map);
        }
        return;
    }

    if (auto iff = std::dynamic_pointer_cast<IfStatement>(node)) {
        if (iff->condition) {
            replaceGenericTypes(iff->condition, generic_map);
        }
        if (iff->then_branch) {
            replaceGenericTypes(iff->then_branch, generic_map);
        }
        if (iff->else_branch) {
            replaceGenericTypes(iff->else_branch, generic_map);
        }
        return;
    }

    if (auto wh = std::dynamic_pointer_cast<WhileStatement>(node)) {
        if (wh->condition) {
            replaceGenericTypes(wh->condition, generic_map);
        }
        if (wh->body) {
            replaceGenericTypes(wh->body, generic_map);
        }
        return;
    }

    if (auto forst = std::dynamic_pointer_cast<ForStatement>(node)) {
        if (forst->init) {
            replaceGenericTypes(forst->init, generic_map);
        }
        if (forst->condition) {
            replaceGenericTypes(forst->condition, generic_map);
        }
        if (forst->increment) {
            replaceGenericTypes(forst->increment, generic_map);
        }
        if (forst->body) {
            replaceGenericTypes(forst->body, generic_map);
        }
        return;
    }

    if (auto ret = std::dynamic_pointer_cast<ReturnStatement>(node)) {
        if (ret->value) {
            replaceGenericTypes(ret->value, generic_map);
        }
        return;
    }

    if (auto es = std::dynamic_pointer_cast<ExpressionStatement>(node)) {
        if (es->expression) {
            replaceGenericTypes(es->expression, generic_map);
        }
        return;
    }

    if (auto asg = std::dynamic_pointer_cast<Assignment>(node)) {
        if (asg->target) {
            replaceGenericTypes(asg->target, generic_map);
        }
        if (asg->value) {
            replaceGenericTypes(asg->value, generic_map);
        }
        return;
    }

    if (auto binop = std::dynamic_pointer_cast<BinaryOperation>(node)) {
        if (binop->left) {
            replaceGenericTypes(binop->left, generic_map);
        }
        if (binop->right) {
            replaceGenericTypes(binop->right, generic_map);
        }
        return;
    }

    if (auto unop = std::dynamic_pointer_cast<UnaryOperation>(node)) {
        if (unop->operand) {
            replaceGenericTypes(unop->operand, generic_map);
        }
        return;
    }

    if (auto call = std::dynamic_pointer_cast<FuncCall>(node)) {
        if (call->func) {
            replaceGenericTypes(call->func, generic_map);
        }
        for (auto &arg : call->args) {
            replaceGenericTypes(arg, generic_map);
        }
        return;
    }

    if (auto tc = std::dynamic_pointer_cast<TypeCast>(node)) {
        replaceInType(tc->target_type, generic_map);
        if (tc->expr) {
            replaceGenericTypes(tc->expr, generic_map);
        }
        return;
    }

    if (auto fa = std::dynamic_pointer_cast<FieldAccess>(node)) {
        if (fa->base) {
            replaceGenericTypes(fa->base, generic_map);
        }
        return;
    }

    if (auto deref = std::dynamic_pointer_cast<Dereference>(node)) {
        if (deref->pointer) {
            replaceGenericTypes(deref->pointer, generic_map);
        }
        return;
    }

    if (auto oa = std::dynamic_pointer_cast<OffsetAccess>(node)) {
        if (oa->base) {
            replaceGenericTypes(oa->base, generic_map);
        }
        if (oa->index) {
            replaceGenericTypes(oa->index, generic_map);
        }
        return;
    }

    if (auto mc = std::dynamic_pointer_cast<MethodCall>(node)) {
        if (mc->object) {
            replaceGenericTypes(mc->object, generic_map);
        }
        for (auto &arg : mc->args) {
            replaceGenericTypes(arg, generic_map);
        }
        return;
    }

    if (auto si = std::dynamic_pointer_cast<StructInitializer>(node)) {
        if (si->struct_type_expr) {
            replaceGenericTypes(si->struct_type_expr, generic_map);
        }
        if (si->inferred_type) {
            replaceInType(si->inferred_type, generic_map);
        }
        for (auto &field : si->field_values) {
            replaceGenericTypes(field.second, generic_map);
        }
        return;
    }

    if (auto ti = std::dynamic_pointer_cast<TemplateInstantiation>(node)) {
        for (auto &type_arg : ti->type_args) {
            replaceInType(type_arg, generic_map);
        }
        return;
    }

    if (auto sd = std::dynamic_pointer_cast<StructDeclaration>(node)) {
        for (auto &field : sd->fields) {
            replaceInType(field.second, generic_map);
        }
        for (auto &method_pair : sd->methods) {
            replaceGenericTypes(method_pair.second, generic_map);
        }
        return;
    }

    if (auto lit = std::dynamic_pointer_cast<Literal>(node)) {
        return;
    }

    if (auto va = std::dynamic_pointer_cast<VarAccess>(node)) {
        return;
    }

    if (auto ma = std::dynamic_pointer_cast<ModuleAccess>(node)) {
        return;
    }

    throw std::runtime_error("Unknown AST node type in replaceGenericTypes");
}

