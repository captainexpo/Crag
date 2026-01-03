
#include "../ast/ast.h"
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

// Returns type of expression, and replacement for the instantiation
std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> TypeChecker::inferTemplateInstantiation(const std::shared_ptr<TemplateInstantiation> &ti) {
        std::string temp = ti->base;
    std::vector<std::shared_ptr<Type>> params = ti->type_args;

    // Check if this is a module-qualified template (contains "::")
    size_t double_colon_pos = temp.find("::");
    if (double_colon_pos != std::string::npos) {
        // Parse the module path and template name
        std::vector<std::string> parts;
        size_t start = 0;
        size_t pos = 0;
        while ((pos = temp.find("::", start)) != std::string::npos) {
            parts.push_back(temp.substr(start, pos - start));
            start = pos + 2;
        }
        parts.push_back(temp.substr(start)); // Add the final part (template name)

        if (parts.size() < 2) {
            throw TypeCheckError(ti, "Invalid module-qualified template: " + temp);
        }

        // The first parts form the module path, last part is the template name
        std::string template_name = parts.back();
        parts.pop_back();

        // Look up the module
        auto module_it = imported_module_checkers.find(parts[0]);
        if (module_it == imported_module_checkers.end()) {
            throw TypeCheckError(ti, "Unknown module in template: " + parts[0]);
        }

        // If there are more path components, recursively resolve
        std::shared_ptr<TypeChecker> target_checker = module_it->second;
        for (size_t i = 1; i < parts.size(); ++i) {
            auto sub_module_it = target_checker->imported_module_checkers.find(parts[i]);
            if (sub_module_it == target_checker->imported_module_checkers.end()) {
                throw TypeCheckError(ti, "Unknown module in path: " + parts[i]);
            }
            target_checker = sub_module_it->second;
        }

        // Now instantiate the template in the target module's context
        auto template_it = target_checker->m_templates.find(template_name);
        if (template_it == target_checker->m_templates.end()) {
            throw TypeCheckError(ti, "Unknown template in module: " + template_name);
        }

        // Create a new template instantiation for the target module to process
        auto ti_copy = std::make_shared<TemplateInstantiation>(template_name, params);
        ti_copy->line = ti->line;
        ti_copy->col = ti->col;

        // Let the target module instantiate it
        auto result = target_checker->inferTemplateInstantiation(ti_copy);
        std::cout << "Instantiated module-qualified template: " << ti->str()
                  << " to type " << result.first->str() << "\n";

        // If the result type contains a struct, we need to get the complete version
        // from the target module's m_structs and register it in the calling module
        if (auto fn_type = std::dynamic_pointer_cast<FunctionType>(result.first)) {
            if (auto st = std::dynamic_pointer_cast<StructType>(fn_type->ret)) {
                // Look up the complete struct in the target module
                auto it = target_checker->m_structs.find(st->name);
                if (it != target_checker->m_structs.end()) {
                    // Replace the return type with the complete struct
                    fn_type->ret = it->second;
                    // Register it in the calling module's m_structs
                    m_structs[st->name] = it->second;
                }
            }
        } else if (auto st = std::dynamic_pointer_cast<StructType>(result.first)) {
            // Look up the complete struct in the target module
            auto it = target_checker->m_structs.find(st->name);
            if (it != target_checker->m_structs.end()) {
                result.first = it->second;
                m_structs[st->name] = it->second;
            }
        }

        ti->inferred_type = result.first;

        // Return a module-qualified access to the instantiated template
        std::string mangled_name = mangleTemplateName(template_name, params);
        auto ma = std::make_shared<ModuleAccess>(parts, mangled_name);
        ma->inferred_type = result.first;
        return std::make_pair(result.first, ma);
    }

    // Local template instantiation (existing code)
    auto maybe_to_instantiate = m_templates.find(temp);
    if (maybe_to_instantiate == m_templates.end()) {
        throw TypeCheckError(ti, "Unknown template: " + ti->base);
    }
    auto to_instantiate = maybe_to_instantiate->second;

    if (auto fd = std::dynamic_pointer_cast<FunctionDeclaration>(to_instantiate)) {
        auto new_name = mangleTemplateName(fd->name, params);
        // std::cout << "Instantiating function template: " << fd->name << " as " << new_name << "\n";
        // Check if already instantiated
        auto it = m_functions.find(new_name);
        if (it != m_functions.end()) {
            ti->inferred_type = it->second;
            return std::make_pair(
                ti->inferred_type,
                std::make_shared<VarAccess>(new_name));
        }

        std::shared_ptr<FunctionDeclaration> declCopy = std::dynamic_pointer_cast<FunctionDeclaration>(fd->copy());
        if (fd->generic_params.size() != params.size()) {
            throw TypeCheckError(ti, "Template instantiation parameter count mismatch for " + fd->name +
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
        m_functions.insert({new_name, std::dynamic_pointer_cast<FunctionType>(ti->inferred_type)});
        // std::cout << m_functions[new_name]->str() << "\n";
        current_module->ast->declarations.push_back(declCopy);
        return std::make_pair(
            ti->inferred_type,
            std::make_shared<VarAccess>(new_name));
    }
    else if (auto sd = std::dynamic_pointer_cast<StructDeclaration>(to_instantiate)) {
        auto new_name = mangleTemplateName(sd->name, params);
        // Check if already instantiated
        auto it = m_structs.find(new_name);
        if (it != m_structs.end()) {
            ti->inferred_type = it->second;
            std::cout << new_name << " (already instantiated)\n";
            return std::make_pair(
                ti->inferred_type,
                std::make_shared<TypeExpression>(ti->inferred_type));
        }

        std::shared_ptr<StructDeclaration> declCopy = std::dynamic_pointer_cast<StructDeclaration>(sd->copy());
        if (sd->generic_params.size() != params.size()) {
            std::cout << "Parameter count mismatch: expected " << sd->generic_params.size()
                      << ", got " << params.size() << "\n";
            throw TypeCheckError(ti, "Template instantiation parameter count mismatch for " + sd->name +
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
        m_structs[new_name] = st;
        insertSymbol(new_name, st, sd);

        auto old_expected_return_type = m_expected_return_type;
        m_expected_return_type = nullptr;
        checkStructDeclaration(declCopy);
        std::cout << "TYP" << m_structs[new_name]->str() << "\n";
        current_module->ast->declarations.push_back(declCopy);

        ti->inferred_type = m_structs[new_name];
        m_expected_return_type = old_expected_return_type;
        return std::make_pair(
            ti->inferred_type,
            std::make_shared<TypeExpression>(ti->inferred_type));
    }
    throw TypeCheckError(ti, "Unsupported template kind: " + to_instantiate->str());
}

void replaceInType(std::shared_ptr<Type> &type, const std::unordered_map<std::string, std::shared_ptr<Type>> &generic_map) {
    if (auto gt = std::dynamic_pointer_cast<GenericType>(type)) {
        auto it = generic_map.find(gt->name);
        if (it != generic_map.end()) {
            type = it->second;
        }
    }else if (auto eu = std::dynamic_pointer_cast<ErrorUnionType>(type)) {
        replaceInType(eu->errorType, generic_map);
        replaceInType(eu->valueType, generic_map);
    }
    else if (auto ti = std::dynamic_pointer_cast<TemplateInstanceType>(type)) {
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
        for (auto &method : st->methods) {
            std::cout << "Replacing in method: " << method.first << "\n";
            replaceGenericTypes(method.second, generic_map);
        }
    } else if (auto ut = std::dynamic_pointer_cast<UnionType>(type)) {
        for (auto &variant : ut->fields) {
            replaceInType(variant.second, generic_map);
        }
    } else if (auto et = std::dynamic_pointer_cast<EnumType>(type)) {
        return;
    }else
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

    throw TypeCheckError(node, "replaceGenericTypes: unhandled AST node type: " + node->str());
}
