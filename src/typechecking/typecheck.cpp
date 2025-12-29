
#include "typecheck.h"
#include "../ast/ast.h"
#include "src/const_eval.h"
#include <cassert>
#include <cstdint>
#include <cstring>
#include <memory>

TypeChecker::TypeChecker() { pushScope(); }

void TypeChecker::pushScope() { m_scopes.emplace_back(); }

void TypeChecker::popScope() {
    if (!m_scopes.empty())
        m_scopes.pop_back();
}

bool TypeChecker::insertSymbol(const std::string &name,
                               std::shared_ptr<Type> t) {
    if (m_scopes.empty())
        pushScope();
    auto &top = m_scopes.back();
    if (top.find(name) != top.end())
        return false;
    top[name] = std::move(t);
    return true;
}

std::optional<std::shared_ptr<Type>>
TypeChecker::lookupSymbol(const std::string &name) const {
    for (auto it = m_scopes.rbegin(); it != m_scopes.rend(); ++it) {
        auto found = it->find(name);
        if (found != it->end())
            return found->second;
    }
    return std::nullopt;
}

bool TypeChecker::canImplicitCast(const std::shared_ptr<Type> &from,
                                  const std::shared_ptr<Type> &to) {
    if (!from || !to)
        return false;
    if (from->equals(to))
        return true;

    if (from->isNumeric() && to->isNumeric())
        return true;

    TypeKind from_tk = from->kind();
    TypeKind to_tk = to->kind();

    if (from_tk == TypeKind::Null && to_tk == TypeKind::Pointer)
        return true;

    if (from_tk == TypeKind::Pointer && to_tk == TypeKind::Pointer) {
        auto to_ptr = dynamic_cast<PointerType *>(to.get());
        auto from_ptr = dynamic_cast<PointerType *>(from.get());
        if (!to_ptr || !from_ptr)
            return false;

        if (from_ptr->base->equals(to_ptr->base)) {
            if (from_ptr->pointer_const && !to_ptr->pointer_const) {
                // std::cout << "Cannot cast away constness in pointer cast from "
                //           << from->str() << " to " << to->str() << "\n";
                return false;
            }
            return true;
        }

        if (to_ptr->base->kind() == TypeKind::Void)
            return true;

        if (to_ptr->base->kind() == TypeKind::Void && to_ptr->pointer_const)
            return true;

        if (from_ptr->base->kind() == TypeKind::Void)
            return false;
    }
    if (auto from_enum = std::dynamic_pointer_cast<EnumType>(from)) {
        if (from_enum->base_type->equals(to))
            return true;
    }
    if (from_tk == TypeKind::Pointer && to_tk == TypeKind::USize)
        return true;
    if (from_tk == TypeKind::Array) {
        auto from_array = std::dynamic_pointer_cast<ArrayType>(from);
        assert(from_array);
        if (to_tk == TypeKind::Array) {
            auto to_array = std::dynamic_pointer_cast<ArrayType>(to);
            assert(to_array);
            // if (from_array->unsized && !to_array->unsized)
            //     return false;
            if (from_array->element_type->equals(to_array->element_type))
                return true;
        }
    }

    return false;
}
bool canExplicitCast(const std::shared_ptr<Type> &from,
                     const std::shared_ptr<Type> &to) {
    if (!from || !to)
        return false;
    if (from->equals(to))
        return true;
    if (from->isNumeric() && to->isNumeric())
        return true;
    TypeKind from_tk = from->kind();
    TypeKind to_tk = from->kind();
    if (from_tk == to_tk)
        return true; // TODO: Figure out if this is bad
    if (from_tk == TypeKind::Enum && to->isNumeric())
        return true;
    if (from_tk == TypeKind::Null && to_tk == TypeKind::Pointer)
        return true;
    if (from_tk == TypeKind::Pointer && to_tk == TypeKind::Pointer)
        return true;

    // Numeric -> Ptr, Ptr -> Numeric

    if (from_tk == TypeKind::Pointer && to_tk == TypeKind::USize)
        return true;
    if (from_tk == TypeKind::USize && to_tk == TypeKind::Pointer)
        return true;

    return false;
}
std::shared_ptr<Type> getCastType(const std::shared_ptr<Type> &from,
                                  const std::shared_ptr<Type> &to) {
    if (!from || !to)
        return nullptr;
    if (from->equals(to))
        return to;

    if (from->isNumeric() && to->isNumeric()) {
        if (from->numericRank() >= to->numericRank())
            return from;
        return to;
    }

    TypeKind from_tk = from->kind();
    TypeKind to_tk = to->kind();

    if (from_tk == TypeKind::Null && to_tk == TypeKind::Pointer)
        return to;

    if (from_tk == TypeKind::Pointer && to_tk == TypeKind::Pointer) {
        auto to_ptr = dynamic_cast<PointerType *>(to.get());
        auto from_ptr = dynamic_cast<PointerType *>(from.get());
        if (!to_ptr || !from_ptr)
            return nullptr;

        if (from_ptr->base->equals(to_ptr->base)) {
            if (from_ptr->pointer_const && !to_ptr->pointer_const)
                return nullptr;
            return to;
        }

        if (to_ptr->base->kind() == TypeKind::Void)
            return to;

        if (to_ptr->base->kind() == TypeKind::Void && to_ptr->pointer_const)
            return to;

        if (from_ptr->base->kind() == TypeKind::Void)
            return nullptr;
    }

    if ((from_tk == TypeKind::Pointer && to_tk == TypeKind::USize) ||
        (from_tk == TypeKind::USize && to_tk == TypeKind::Pointer))
        return to;

    return nullptr;
}
std::string TypeChecker::typeName(const std::shared_ptr<Type> &t) const {
    return t ? t->str() : "<unknown>";
}

std::shared_ptr<Type>
TypeChecker::resolveType(const std::shared_ptr<Type> &t) {
    if (!t)
        return nullptr;

    // Resolve bound generics if any (used in instantiation paths)
    if (auto gt = std::dynamic_pointer_cast<GenericType>(t)) {
        // In general, GenericType should be substituted via explicit bindings; if none, leave as-is
        return t;
    }

    if (auto st = std::dynamic_pointer_cast<StructType>(t)) {
        auto it = m_structs.find(st->name);
        if (it != m_structs.end())
            return it->second;
        return st; // unknown struct type
    }
    if (auto pt = std::dynamic_pointer_cast<PointerType>(t)) {
        auto bt = resolveType(pt->base);
        if (!bt)
            return nullptr;
        return std::make_shared<PointerType>(bt, pt->pointer_const);
    }
    if (auto at = std::dynamic_pointer_cast<ArrayType>(t)) {
        auto bt = resolveType(at->element_type);
        if (at->length_expr)
            at->length_expr->inferred_type = inferExpression(at->length_expr);

        if (!bt)
            return nullptr;
        return std::make_shared<ArrayType>(bt, at->length_expr, at->unsized);
    }
    if (auto ft = std::dynamic_pointer_cast<FunctionType>(t)) {
        auto rt = resolveType(ft->ret);
        if (!rt)
            return nullptr;
        std::vector<std::shared_ptr<Type>> pts;
        for (const auto &p : ft->params) {
            auto pt = resolveType(p);
            if (!pt)
                return nullptr;
            pts.push_back(pt);
        }
        return std::make_shared<FunctionType>(pts, rt, ft->variadic);
    }
    if (auto qt = std::dynamic_pointer_cast<QualifiedType>(t)) {
        // std::cout << "Resolving qualified type: ";
        // for (const auto &part : qt->module_path) {
        //     std::cout << part << "::";
        // }
        // std::cout << qt->type_name << "\n";
        if (qt->module_path.empty()) {
            auto sym = lookupSymbol(qt->type_name);
            if (sym)
                return *sym;
            return qt; // unknown type
        } else {
            // look in imported modules
            auto mod_name = qt->module_path[0];
            auto it = m_imported_module_checkers.find(mod_name);
            if (it != m_imported_module_checkers.end()) {
                auto imported_checker = it->second;
                // Recursively resolve the rest of the module path
                return imported_checker->resolveType(
                    std::make_shared<QualifiedType>(
                        std::vector<std::string>(qt->module_path.begin() + 1, qt->module_path.end()),
                        qt->type_name));
            }

            return qt; // unknown qualified type
        }
    }
    // primitive types are already resolved
    return t;
}

// Entry point
void TypeChecker::check(std::shared_ptr<Module> module) {

    current_module = module;
    if (!module || !module->ast)
        return;

    std::vector<std::shared_ptr<StructDeclaration>> struct_decls;
    std::vector<std::shared_ptr<UnionDeclaration>> union_decls;

    for (int i = 0; i < module->ast->declarations.size(); ++i) {
        auto decl = module->ast->declarations[i];
        if (auto sd = std::dynamic_pointer_cast<StructDeclaration>(decl)) {

            // Check if this is a generic struct
            if (sd->generic_params.size() > 0) {
                m_templates[sd->name] = sd;
            } else {
                // Build a StructType and register
                auto st = std::make_shared<StructType>(sd->name);
                st->complete = false;
                m_structs[sd->name] = st;
                insertSymbol(sd->name, st);
                struct_decls.push_back(sd);
            }
        } else if (auto ud = std::dynamic_pointer_cast<UnionDeclaration>(decl)) {
            // Build a UnionType and register
            auto ut = std::make_shared<UnionType>(ud->name);
            ut->complete = false;
            m_unions[ud->name] = ut;
            insertSymbol(ud->name, ut);
            union_decls.push_back(ud);
        } else if (auto fd = std::dynamic_pointer_cast<FunctionDeclaration>(decl)) {
            if (fd->generic_params.size() > 0) {
                // Function has generic type parameters, don't immediately add to symbol table
                m_templates[fd->name] = fd;
            } else {
                m_functions[fd->name] = fd->type;
                insertSymbol(fd->name, fd->type);
            }
        } else if (auto en = std::dynamic_pointer_cast<EnumDeclaration>(decl)) {
            if (!en->base_type) {
                throw TypeCheckError(en, "Enum " + en->name + " has no base type");
                return;
            }
            auto et = en->enum_type;
            for (const auto &v : en->variants) {
                et->addVariant(v.first, v.second);

                // infer type of variant value
                std::shared_ptr<Expression> val = v.second;
                auto i = inferExpression(val);
                auto vtype = resolveType(i);
                if (!vtype->equals(en->base_type)) {
                    throw TypeCheckError(v.second,
                                         "Enum variant " + v.first + " has type " + typeName(vtype) +
                                             ", expected " + typeName(en->base_type));
                }
            }

            m_enums[en->name] = et;
            en->inferred_type = et;
            insertSymbol(en->name, et);
        } else if (auto im = std::dynamic_pointer_cast<ImportDeclaration>(decl)) {
            auto import_checker = std::make_shared<TypeChecker>();
            import_checker->check(module->imports[im->alias]);
            if (!import_checker->ok()) {
                for (const auto &e : import_checker->errors()) {
                    throw TypeCheckError(e.first, "In imported module '" + im->alias + "': " + e.second);
                }
            }
            m_imported_module_checkers[im->alias] = import_checker;
        } else if (auto ta = std::dynamic_pointer_cast<TypeAliasDeclaration>(decl)) {
            auto alias_type = resolveType(ta->aliased_type);
            if (!alias_type) {
                throw TypeCheckError(ta, "Type alias " + ta->name + " has unknown aliased type");
                return;
            }
            m_type_aliases[ta->name] = alias_type;
            insertSymbol(ta->name, alias_type);
        }
    }

    // Remove templates from AST
    module->ast->declarations.erase(
        std::remove_if(
            module->ast->declarations.begin(),
            module->ast->declarations.end(),
            [this](const std::shared_ptr<ASTNode> &decl) {
                if (auto sd = std::dynamic_pointer_cast<StructDeclaration>(decl)) {
                    return sd->generic_params.size() > 0;
                }
                if (auto fd = std::dynamic_pointer_cast<FunctionDeclaration>(decl)) {
                    return fd->generic_params.size() > 0;
                }
                return false;
            }),
        module->ast->declarations.end());

    for (const auto &sd : struct_decls) {
        auto fields = std::vector<std::pair<std::string, std::shared_ptr<Type>>>();
        for (const auto &f : sd->fields) {
            fields.emplace_back(f.first, resolveType(f.second));
        }
        auto it = m_structs.find(sd->name);
        if (it != m_structs.end()) {
            it->second->fields = std::move(fields);
            it->second->methods = sd->methods;
            it->second->complete = true;
        } else {
            throw TypeCheckError(sd, "Internal error: struct " + sd->name + " not found in map");
        }

        for (const auto &m : sd->methods) {

            for (const auto &ud : union_decls) {
                auto fields = std::vector<std::pair<std::string, std::shared_ptr<Type>>>();
                for (const auto &f : ud->fields) {
                    fields.emplace_back(f.first, resolveType(f.second));
                }
                auto it = m_unions.find(ud->name);
                if (it != m_unions.end()) {
                    it->second->fields = std::move(fields);
                    it->second->complete = true;
                } else {
                    throw TypeCheckError(ud, "Internal error: union " + ud->name + " not found in map");
                }
            }
            // Type check method bodies
            checkFunctionDeclaration(m.second);
        }
    }

    for (const auto &decl : module->ast->declarations) {
        checkNode(decl);
    }
}

void TypeChecker::checkNode(const std::shared_ptr<ASTNode> &node) {
    if (!node)
        return;
    if (auto fd = std::dynamic_pointer_cast<FunctionDeclaration>(node)) {
        checkFunctionDeclaration(fd);
        return;
    }
    if (auto vd = std::dynamic_pointer_cast<VariableDeclaration>(node)) {
        checkVariableDeclaration(vd);
        return;
    }
    if (auto sd = std::dynamic_pointer_cast<StructDeclaration>(node)) {
        checkStructDeclaration(sd);
        return;
    }
    if (auto ud = std::dynamic_pointer_cast<UnionDeclaration>(node)) {
        checkUnionDeclaration(ud);
        return;
    }
    if (auto ed = std::dynamic_pointer_cast<EnumDeclaration>(node)) {
        checkEnumDeclaration(ed);
        return;
    }
    if (auto im = std::dynamic_pointer_cast<ImportDeclaration>(node)) {
        // Already handled in check()
        return;
    }
    if (auto ta = std::dynamic_pointer_cast<TypeAliasDeclaration>(node)) {
        // Already handled in check()
        return;
    }
    throw TypeCheckError(node, "Unsupported top-level declaration: " + node->toString());
}

void TypeChecker::checkStructDeclaration(
    const std::shared_ptr<StructDeclaration> &st) {
    // Already registered the struct type in check(); nothing else to do for
    // now.
    (void)st;
}

void TypeChecker::checkUnionDeclaration(
    const std::shared_ptr<UnionDeclaration> &ud) {
    // Already registered the union type in check(); nothing else to do for
    // now.
    (void)ud;
}

void TypeChecker::checkEnumDeclaration(
    const std::shared_ptr<EnumDeclaration> &en) {
}

void TypeChecker::checkFunctionDeclaration(
    const std::shared_ptr<FunctionDeclaration> &fn) {
    if (!fn->type) {
        throw TypeCheckError(fn, "Function " + fn->name + " has no type information");
        return;
    }
    pushScope();
    for (size_t i = 0; i < fn->type->params.size(); ++i) {
        std::string pname =
            (i < fn->param_names.size() ? fn->param_names[i]
                                        : ("arg" + std::to_string(i)));
        if (!insertSymbol(pname, fn->type->params[i])) {
            throw TypeCheckError(fn,
                                 "Duplicate parameter name " + pname + " in function " + fn->name);
        }
    }

    m_expected_return_type = fn->type->ret;
    if (fn->body) {
        checkStatement(fn->body);
    }

    popScope();
}

void TypeChecker::checkVariableDeclaration(
    const std::shared_ptr<VariableDeclaration> &var) {

    const std::string &name = var->name;
    auto init = var->initializer;

    bool is_global = m_scopes.size() == 1;

    if (!init)
        goto end;

    if (auto expr = std::dynamic_pointer_cast<Expression>(init)) {
        if (is_global && var->is_const && init) {
            // Global variable initializer must be a constant expression
            std::optional<std::shared_ptr<VariableDeclaration>> const_val = m_const_eval.evaluateVariableDeclaration(var);
            if (!m_const_eval.ok()) {
                for (const auto &e : m_const_eval.errors()) {
                    throw TypeCheckError(e.first, e.second);
                }
                goto end;
            }
            if (const_val && const_val->get()->initializer) {
                var->initializer = const_val->get()->initializer;
                expr = std::dynamic_pointer_cast<Expression>(var->initializer);
            } else {
                throw TypeCheckError(var, "Failed to evaluate constant initializer for variable " + name);
                goto end;
            }
        } else if (is_global && init) {
            throw TypeCheckError(var, "Global variable " + name + " must be declared const if it has an initializer");
            goto end;
        }

        var->var_type = resolveType(var->var_type);

        // Pass expected type to initializer inference
        auto init_type = resolveType(inferExpression(expr, var->var_type));

        if (!var->var_type) {
            var->var_type = init_type; // Type inference
        }
        var->var_type->is_const = var->is_const;

        // If is array literal and our var_type is array with unspecified size, infer size
        if (auto arr_lit = std::dynamic_pointer_cast<ArrayLiteral>(expr)) {
            if (auto arr_type = std::dynamic_pointer_cast<ArrayType>(var->var_type)) {
                if (arr_type->unsized) {
                    auto i64 = std::make_shared<I64>();
                    arr_type->length_expr = std::make_shared<Literal>(static_cast<int64_t>(arr_lit->elements.size()), i64);
                    // std::cout << "Inferred array length for variable " << name << ": "
                    //           << arr_lit->elements.size() << "\n";
                    // HACK: This is kinda dirty
                    arr_type->length_expr->inferred_type = i64;

                    arr_type->unsized = false;
                    var->var_type = arr_type;
                } else {
                    auto constantExpression = m_const_eval.evaluateExpression(arr_type->length_expr);
                    if (constantExpression != std::nullopt && constantExpression.value() != nullptr) {
                        arr_type->length_expr = constantExpression.value();
                    }
                    if (!m_const_eval.ok()) {
                        for (const auto &e : m_const_eval.errors()) {
                            throw TypeCheckError(e.first, e.second);
                        }
                        goto end;
                    }
                    int64_t expected_length = -1;
                    if (constantExpression) {
                        if (auto lit = std::dynamic_pointer_cast<Literal>(arr_type->length_expr)) {
                            if (auto val = std::get_if<int64_t>(&lit->value)) {
                                expected_length = *val;
                            }
                        } else {
                            throw TypeCheckError(arr_type->length_expr, "Array length expression did not evaluate to a literal");
                            goto end;
                        }
                    }
                    auto lit_len = static_cast<int64_t>(arr_lit->elements.size());
                    if (expected_length != lit_len) {
                        throw TypeCheckError(arr_lit, "Array literal length mismatch for variable " + name +
                                                          ": expected " + std::to_string(expected_length) +
                                                          ", got " + std::to_string(arr_lit->elements.size()));
                        goto end;
                    }
                }
            }
        }

        if (!init_type) {
            std::cerr << "DEBUG: init = " << init->toString() << "\n";
            throw TypeCheckError(init, "Failed to infer type of initializer for variable " + name);
            goto end;
        }

        if (!init_type->equals(var->var_type)) {
            if (canImplicitCast(init_type, var->var_type)) {
                var->initializer = std::make_shared<TypeCast>(expr, var->var_type,
                                                              CastType::Normal);
            } else if (canExplicitCast(init_type, var->var_type)) {
                throw TypeCheckError(init, "Explicit cast needed in initializer for variable " + name +
                                               ": cannot implicitly convert " + typeName(init_type) +
                                               " to " + typeName(var->var_type));
                goto end;
            } else {
                throw TypeCheckError(var, "Type mismatch in initializer for variable " + name +
                                              ": expected " + typeName(var->var_type) + " but got " +
                                              typeName(init_type));
                goto end;
            }
        }
    } else if (auto si = std::dynamic_pointer_cast<StructInitializer>(init)) {
        auto t = inferStructInit(si);
        if (!t || !t->equals(var->var_type)) {
            throw TypeCheckError(init, "Type mismatch in initializer for variable " + name);
            goto end;
        }
    } else {
        throw TypeCheckError(init, "Unsupported initializer node for variable " + name);
        goto end;
    }

end:
    if (!insertSymbol(name, var->var_type)) {
        throw TypeCheckError(var, "Duplicate variable declaration: " + name);
    }
}

void TypeChecker::checkStatement(const std::shared_ptr<Statement> &stmt) {
    if (!stmt)
        return;
    if (auto block = std::dynamic_pointer_cast<Block>(stmt)) {
        pushScope();
        for (auto &s : block->statements) {
            try {
                checkStatement(s);
            } catch (const TypeCheckError &e) {
                m_errors.emplace_back(e.node, e.what());
            }
        }
        popScope();
        return;
    }
    if (auto iff = std::dynamic_pointer_cast<IfStatement>(stmt)) {
        auto t = inferExpression(iff->condition);
        if (!t || !dynamic_cast<Boolean *>(t.get())) {
            throw TypeCheckError(iff->condition,
                                 "Condition in if-statement is not boolean: got " + typeName(t));
        }
        checkStatement(iff->then_branch);
        if (iff->else_branch)
            checkStatement(iff->else_branch);
        return;
    }
    if (auto ret = std::dynamic_pointer_cast<ReturnStatement>(stmt)) {
        std::shared_ptr<Type> ret_inferred = nullptr;
        if (ret->value) {
            // Pass expected return type to the return value inference
            ret_inferred = inferExpression(ret->value, m_expected_return_type);
        }
        if (!m_expected_return_type) {
            if (ret->value) {
                throw TypeCheckError(ret->value, "Return with value in void function");
            }
        } else {
            if (!ret->value) {
                if (!dynamic_cast<Void *>(m_expected_return_type.get())) {
                    throw TypeCheckError(ret, "Return type mismatch: expected " +
                                                  typeName(m_expected_return_type) +
                                                  " but got void return");
                }
            } else {

                auto t = resolveType(ret_inferred);
                if (ret->is_error) { // Returning an error from the function (oh no!)
                    if (!dynamic_cast<ErrorUnionType *>(m_expected_return_type.get())) {
                        throw TypeCheckError(ret, "Return type mismatch: expected " +
                                                      typeName(m_expected_return_type) +
                                                      " but got error return");
                    }
                    auto exp_error = dynamic_cast<ErrorUnionType *>(
                        m_expected_return_type.get());
                    if (!t->equals(exp_error->errorType)) {
                        if (canImplicitCast(t, exp_error->errorType)) {
                            ret->value = std::make_shared<TypeCast>(ret->value, exp_error->errorType, CastType::Normal);
                        } else {
                            throw TypeCheckError(ret, "Return type mismatch: expected " +
                                                          typeName(exp_error->errorType) + " but got " +
                                                          typeName(t));
                        }
                    }
                    return;
                }
                if (auto expected_error = std::dynamic_pointer_cast<ErrorUnionType>(m_expected_return_type)) {
                    if (!t->equals(expected_error->valueType)) {
                        if (canImplicitCast(t, expected_error->valueType)) {
                            ret->value = std::make_shared<TypeCast>(ret->value, expected_error->valueType, CastType::Normal);
                        } else {
                            throw TypeCheckError(ret, "Return type mismatch: expected " +
                                                          typeName(expected_error->valueType) + " but got " +
                                                          typeName(t));
                        }
                    }
                    return;
                }
                if (!t || !t->equals(m_expected_return_type)) {
                    if (t && canImplicitCast(t, m_expected_return_type)) {
                        ret->value = std::make_shared<TypeCast>(ret->value, m_expected_return_type, CastType::Normal);
                    } else {
                        throw TypeCheckError(ret, "Return type mismatch: expected " +
                                                      typeName(m_expected_return_type) + " but got " +
                                                      typeName(t));
                    }
                }
            }
        }
        return;
    }
    if (auto exprs = std::dynamic_pointer_cast<ExpressionStatement>(stmt)) {
        inferExpression(exprs->expression);
        return;
    }
    if (auto vd = std::dynamic_pointer_cast<VariableDeclaration>(stmt)) {
        checkVariableDeclaration(vd);
        return;
    }
    if (auto asg = std::dynamic_pointer_cast<Assignment>(stmt)) {
        auto lt = inferExpression(asg->target);
        // Pass expected type (left side) to right side inference
        auto rt = inferExpression(asg->value, lt);
        if (!lt || !rt)
            return;
        if (!lt->equals(rt)) {
            if (canImplicitCast(rt, lt)) {
                asg->value = std::make_shared<TypeCast>(asg->value, lt, CastType::Normal);
                return;
            }

            throw TypeCheckError(asg,
                                 "Assignment type mismatch: " + typeName(lt) + " = " + typeName(rt));
        }
        return;
    }
    if (auto forst = std::dynamic_pointer_cast<ForStatement>(stmt)) {
        pushScope();
        if (forst->init) {
            checkStatement(forst->init);
        }
        if (forst->condition) {
            inferExpression(forst->condition);
        }
        if (forst->increment) {
            checkStatement(forst->increment);
        }
        checkStatement(forst->body);
        popScope();
        return;
    }
    if (auto wh = std::dynamic_pointer_cast<WhileStatement>(stmt)) {
        auto t = inferExpression(wh->condition);
        if (!t || !dynamic_cast<Boolean *>(t.get())) {
            throw TypeCheckError(wh->condition,
                                 "Condition in while-statement is not boolean: got " + typeName(t));
        }
        checkStatement(wh->body);
        return;
    }
    if (stmt->kind() == NodeKind::BreakStatement ||
        stmt->kind() == NodeKind::ContinueStatement) {
        return; // nothing to check
    }

    throw TypeCheckError(stmt, "Unsupported statement type");
}

std::shared_ptr<Type>
TypeChecker::inferExpression(std::shared_ptr<Expression> &expr,
                             const std::shared_ptr<Type> &expected) {
    if (!expr)
        return nullptr;
    if (auto va = std::dynamic_pointer_cast<VarAccess>(expr))
        return inferVarAccess(va);
    if (auto lit = std::dynamic_pointer_cast<Literal>(expr))
        return inferLiteral(lit, expected);
    if (auto bin = std::dynamic_pointer_cast<BinaryOperation>(expr))
        return inferBinaryOp(bin, expected);
    if (auto un = std::dynamic_pointer_cast<UnaryOperation>(expr))
        return inferUnaryOp(un);
    if (auto call = std::dynamic_pointer_cast<FuncCall>(expr)) {
        if (auto va = std::dynamic_pointer_cast<VarAccess>(call->func)) {
            // Check for built-in functions
            if (va->name == "sizeof") {
                auto so = expandSizeOf(call);
                expr = so.second;
                return so.first;
            }
            if (va->name == "alignof") {
                auto ao = expandAlignOf(call);
                expr = ao.second;
                return ao.first;
            }
            if (va->name == "offsetof") {
                auto oo = expandOffsetOf(call);
                expr = oo.second;
                return oo.first;
            }
        }
        return inferFuncCall(call, expected);
    }
    if (auto fa = std::dynamic_pointer_cast<FieldAccess>(expr))
        return inferFieldAccess(fa);
    if (auto oa = std::dynamic_pointer_cast<OffsetAccess>(expr))
        return inferOffsetAccess(oa);
    if (auto si = std::dynamic_pointer_cast<StructInitializer>(expr))
        return inferStructInit(si, expected);
    if (auto tc = std::dynamic_pointer_cast<TypeCast>(expr))
        return inferTypeCast(tc);
    if (auto d = std::dynamic_pointer_cast<Dereference>(expr))
        return inferDereference(d);
    if (auto mc = std::dynamic_pointer_cast<MethodCall>(expr))
        return inferMethodCall(mc);
    if (auto ma = std::dynamic_pointer_cast<ModuleAccess>(expr))
        return inferModuleAccess(ma);
    if (auto ea = std::dynamic_pointer_cast<EnumAccess>(expr)) {
        std::string enum_name;
        auto va = std::dynamic_pointer_cast<VarAccess>(ea->enum_expr);
        std::shared_ptr<EnumType> res;
        // std::cout << "Inferring enum access: " << ea->str() << "\n";
        if (!va) {
            auto mod = std::dynamic_pointer_cast<ModuleAccess>(ea->enum_expr);
            if (!mod) {
                throw TypeCheckError(ea, "Enum access base is not allowed: " + ea->enum_expr->str());
            }
            // Get the enum from the other module
            auto mod_type = inferModuleAccess(mod);
            auto enum_type = std::dynamic_pointer_cast<EnumType>(mod_type);
            if (!enum_type) {
                throw TypeCheckError(ea, "Enum access base is not an enum: " + ea->enum_expr->str());
            }
            enum_name = enum_type->name;
            res = enum_type;
        } else {
            enum_name = va->name;
            res = std::dynamic_pointer_cast<EnumType>(resolveType(m_enums[enum_name]));
        }
        ea->inferred_type = res;
        return res;
    }
    if (auto ta = std::dynamic_pointer_cast<TemplateInstantiation>(expr)) {
        std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> pair = inferTemplateInstantiation(ta);
        expr = pair.second;
        return pair.first;
    }
    if (auto al = std::dynamic_pointer_cast<ArrayLiteral>(expr))
        return inferArrayLiteral(al, expected);
    // Unknown expression kind
    throw TypeCheckError(expr, "Type inference: unhandled expression type: " + expr->str());
}

std::shared_ptr<Type> TypeChecker::inferModuleAccess(const std::shared_ptr<ModuleAccess> &ma) {
    if (ma->module_path.size() == 0) {
        return inferVarAccess(std::make_shared<VarAccess>(ma->member_name));
    }
    auto next_module_name = ma->module_path[0];
    // std::cout << "Inferring module access: " << ma->str() << "\n";
    auto maybe_mod = m_imported_module_checkers.find(next_module_name);
    if (maybe_mod == m_imported_module_checkers.end()) {
        throw TypeCheckError(ma, "Unknown module: " + next_module_name);
    }

    std::shared_ptr<TypeChecker> mod_checker = maybe_mod->second;
    // Recurse into sub-module access

    auto sub_ma = std::make_shared<ModuleAccess>(
        std::vector<std::string>(ma->module_path.begin() + 1, ma->module_path.end()),
        ma->member_name);
    return mod_checker->inferModuleAccess(sub_ma);
}

std::shared_ptr<Type>
TypeChecker::inferTypeCast(const std::shared_ptr<TypeCast> &tc) {
    if (tc->cast_type == CastType::Reinterperet) {
        // allow any reinterpreted cast
        inferExpression(tc->expr);
        tc->inferred_type = resolveType(tc->target_type);
        return tc->target_type;
    }
    auto ot = inferExpression(tc->expr);
    if (!ot)
        return nullptr;
    if (canExplicitCast(ot, tc->target_type)) {
        tc->inferred_type = resolveType(tc->target_type);
        return tc->target_type;
    }
    throw TypeCheckError(tc, "Invalid type cast from " + typeName(ot) + " to " +
                                 tc->target_type->str());
    return nullptr;
}

std::shared_ptr<Type>
TypeChecker::inferVarAccess(const std::shared_ptr<VarAccess> &v) {
    auto maybe = lookupSymbol(v->name);
    if (!maybe) {
        throw TypeCheckError(v, "Unknown variable: " + v->name);
    }
    v->inferred_type = resolveType(*maybe);
    return *maybe;
}

std::shared_ptr<Type>
TypeChecker::inferLiteral(const std::shared_ptr<Literal> &lit,
                          const std::shared_ptr<Type> &expected) {
    auto preType = resolveType(lit->lit_type);
    if (!preType) {
        throw TypeCheckError(lit, "Unknown literal type");
    }

    // Use expected type for numeric literals when possible
    if (expected && expected->isNumeric() && lit->lit_type->isNumeric()) {
        // Check if we can use the expected type directly
        if (canImplicitCast(preType, expected)) {

            uint64_t lit_val = getLitValue(lit);

            lit->lit_type = expected;
            lit->inferred_type = expected;

            // Update the literal value to match the expected type
            setLitVal(lit, lit_val);
            return expected;
        }
    }

    lit->inferred_type = preType;
    return lit->lit_type;
}

std::shared_ptr<Type> TypeChecker::inferArrayLiteral(const std::shared_ptr<ArrayLiteral> &al,
                                                     const std::shared_ptr<Type> &expected) {
    if (al->elements.empty()) {
        throw TypeCheckError(al, "Cannot infer type of empty array literal");
    }

    // Extract expected element type from expected array type
    std::shared_ptr<Type> expected_elem = nullptr;
    if (expected) {
        if (auto arr_type = std::dynamic_pointer_cast<ArrayType>(expected)) {
            expected_elem = arr_type->element_type;
        }
    }

    std::shared_ptr<Type> elem_type = nullptr;
    for (int i = 0; i < al->elements.size(); ++i) {
        auto el = al->elements[i];
        // Pass expected element type to each element
        auto t = inferExpression(el, expected_elem ? expected_elem : elem_type);
        if (!t)
            return nullptr;
        if (!elem_type) {
            elem_type = t;
        } else {
            if (!t->equals(elem_type)) {
                if (canImplicitCast(t, elem_type)) {
                    // insert cast
                    al->elements[i] = std::make_shared<TypeCast>(el, elem_type, CastType::Normal);
                } else if (canImplicitCast(elem_type, t)) {
                    // insert cast
                    for (auto &e2 : al->elements) {
                        e2 = std::make_shared<TypeCast>(e2, t, CastType::Normal);
                    }
                    elem_type = t;
                } else {
                    throw TypeCheckError(al, "Array literal element type mismatch: " +
                                                 typeName(elem_type) + " vs " + typeName(t));
                    return nullptr;
                }
            }
        }
    }
    auto i64 = std::make_shared<I64>();
    auto arrSizeLit = std::make_shared<Literal>(static_cast<int64_t>(al->elements.size()), i64);
    arrSizeLit->inferred_type = i64;
    auto at = std::make_shared<ArrayType>(elem_type, arrSizeLit, false);
    al->inferred_type = resolveType(at);
    return at;
}

std::shared_ptr<Type>
TypeChecker::inferBinaryOp(const std::shared_ptr<BinaryOperation> &bin,
                           const std::shared_ptr<Type> &expected) {
    auto lt = resolveType(inferExpression(bin->left));
    auto rt = resolveType(inferExpression(bin->right));
    if (!lt || !rt)
        return nullptr;

    // simple numeric ops
    if (bin->op == "+" || bin->op == "-" || bin->op == "*" || bin->op == "/" || bin->op == "%" ||
        bin->op == "|" || bin->op == "&" || bin->op == "^" || bin->op == "<<" ||
        bin->op == ">>") {

        if (!lt->isGeneralNumeric() || !rt->isGeneralNumeric()) { // Allow pointers here
            throw TypeCheckError(bin, "Arithmetic operators require numeric operands: got " +
                                          typeName(lt) + " " + bin->op + " " + typeName(rt));
            return nullptr;
        }
        if (!lt->equals(rt)) {
            // try implicit cast of right to left
            if (canImplicitCast(rt, lt)) {
                bin->right = std::make_shared<TypeCast>(bin->right, lt, CastType::Normal);
            } else if (canImplicitCast(lt, rt)) {
                bin->left = std::make_shared<TypeCast>(bin->left, rt, CastType::Normal);
                lt = rt;
            } else {
                throw TypeCheckError(bin, "Arithmetic operator type mismatch: " + typeName(lt) + " " +
                                              bin->op + " " + typeName(rt));
                return nullptr;
            }
        }
        bin->inferred_type = resolveType(lt);
        return lt;
    }

    // BOOL -> BOOL
    if (bin->op == "&&" || bin->op == "||") {
        if (!dynamic_cast<Boolean *>(lt.get()) ||
            !dynamic_cast<Boolean *>(rt.get())) {
            throw TypeCheckError(bin, "Logical operators require boolean operands: got " +
                                          typeName(lt) + " " + bin->op + " " + typeName(rt));
            return nullptr;
        }
        auto b = std::make_shared<Boolean>();
        bin->inferred_type = resolveType(b);
        return b;
    }

    // comparisons -> BOOL
    if (bin->op == "==" || bin->op == "!=" || bin->op == "<" || bin->op == ">" ||
        bin->op == "<=" || bin->op == ">=") {
        if (!lt->equals(rt) && !canImplicitCast(rt, lt) &&
            !canImplicitCast(lt, rt)) {
            throw TypeCheckError(bin, "Comparison operands must have same type: " + typeName(lt) +
                                          " vs " + typeName(rt));
            return nullptr;
        } else if (!lt->equals(rt)) {
            // try implicit cast
            if (canImplicitCast(rt, lt)) {
                bin->right = std::make_shared<TypeCast>(bin->right, lt, CastType::Normal);
            } else if (canImplicitCast(lt, rt)) {
                bin->left = std::make_shared<TypeCast>(bin->left, rt, CastType::Normal);
                lt = rt;
            }
        }
        auto b = std::make_shared<Boolean>();
        bin->inferred_type = resolveType(b);
        return b;
    }

    if (bin->op == "=") {
        // assignment: left must be lvalue (VarAccess, FieldAccess, OffsetAccess)
        if (!(std::dynamic_pointer_cast<VarAccess>(bin->left) ||
              std::dynamic_pointer_cast<FieldAccess>(bin->left) ||
              std::dynamic_pointer_cast<OffsetAccess>(bin->left) ||
              std::dynamic_pointer_cast<Dereference>(bin->left) ||
              std::dynamic_pointer_cast<ModuleAccess>(bin->left))) {
            throw TypeCheckError(bin->left,
                                 "Left operand of assignment is not an lvalue: " + bin->left->str());
            return nullptr;
        }
        if (lt->is_const) {
            throw TypeCheckError(bin->left, "Cannot assign to const value");
        }
        if (auto deref = std::dynamic_pointer_cast<Dereference>(bin->left)) {
            // make sure we're not assigning to a const pointer
            auto pt = inferExpression(deref->pointer);
            if (!pt)
                return nullptr;
            auto ptype = std::dynamic_pointer_cast<PointerType>(pt);
            if (!ptype) {
                throw TypeCheckError(deref->pointer,
                                     "Dereference of non-pointer type: " + typeName(pt));
                return nullptr;
            }
            if (ptype->pointer_const) {
                throw TypeCheckError(deref->pointer, "Cannot modify value through const pointer");
            }
        }
        if (!lt->equals(rt)) {
            if (canImplicitCast(rt, lt)) {
                bin->right = std::make_shared<TypeCast>(bin->right, lt, CastType::Normal);
                bin->inferred_type = resolveType(lt);
                return lt;
            }
            throw TypeCheckError(bin,
                                 "Assignment type mismatch: " + typeName(lt) + " = " + typeName(rt));
            return nullptr;
        }
        bin->inferred_type = resolveType(lt);
        return lt;
    }

    throw TypeCheckError(bin, "Unhandled binary operator: " + bin->op);
}

std::shared_ptr<Type>
TypeChecker::inferUnaryOp(const std::shared_ptr<UnaryOperation> &un) {
    auto ot = inferExpression(un->operand);
    if (!ot)
        return nullptr;

    if (un->op == "-" || un->op == "+" || un->op == "~") {
        // numeric
        un->inferred_type = resolveType(ot);
        return ot;
    }
    if (un->op == "!") {
        if (!dynamic_cast<Boolean *>(ot.get())) {
            throw TypeCheckError(un, "Logical not expects boolean operand, got " + typeName(ot));
        }
        un->inferred_type = resolveType(ot);
        return ot;
    }
    if (un->op == "&") {
        // address-of: result is pointer to operand type
        auto pt = std::make_shared<PointerType>(ot);
        un->inferred_type = resolveType(pt);
        return pt;
    }
    throw TypeCheckError(un, "Unhandled unary operator: " + un->op);
}

std::shared_ptr<Type>
TypeChecker::inferDereference(const std::shared_ptr<Dereference> &d) {
    auto ot = inferExpression(d->pointer);
    if (!ot)
        return nullptr;
    auto pt = std::dynamic_pointer_cast<PointerType>(ot);
    if (!pt) {
        throw TypeCheckError(d, "Dereference of non-pointer type: " + typeName(ot));
    }
    d->inferred_type = resolveType(pt->base);
    return pt->base;
}

std::shared_ptr<Type>
TypeChecker::inferMethodCall(const std::shared_ptr<MethodCall> &mc) {
    auto bt = inferExpression(mc->object);
    if (!bt)
        return nullptr;
    auto st = std::dynamic_pointer_cast<StructType>(bt);
    if (!st) {
        auto pt = std::dynamic_pointer_cast<PointerType>(bt);
        if (pt)
            st = std::dynamic_pointer_cast<StructType>(resolveType(pt->base));
        if (!pt) {
            throw TypeCheckError(mc, "Method call on non-struct type: " + typeName(bt));
        }
    }
    auto it = st->methods.find(mc->method);
    if (it == st->methods.end()) {
        throw TypeCheckError(mc, "Struct " + st->name + " has no method " + mc->method);
    }
    auto ftype = std::dynamic_pointer_cast<FunctionType>(resolveType(it->second->type));
    // check arity (naive, no implicit conversions)
    if (!ftype->variadic &&
        mc->args.size() !=
            (ftype->params.size() - 1)) {
        throw TypeCheckError(mc, "Method call argument count mismatch: expected " +
                                     std::to_string(ftype->params.size() - 1) + " got " +
                                     std::to_string(mc->args.size()));
        return nullptr;
    }
    size_t n = ftype->params.size();
    for (size_t i = 0; i < mc->args.size(); ++i) {
        auto at = inferExpression(mc->args[i]);

        if (!at)
            throw TypeCheckError(mc, "Failed to infer type of method call argument " +
                                         std::to_string(i));
        if (i + 1 < ftype->params.size() && !at->equals(ftype->params[i + 1])) {
            if (canImplicitCast(at, ftype->params[i + 1])) {
                mc->args[i] = std::make_shared<TypeCast>(mc->args[i], ftype->params[i + 1], CastType::Normal);
            } else
                throw TypeCheckError(mc, "Method call argument " + std::to_string(i) +
                                             " type mismatch: expected " + typeName(ftype->params[i + 1]) +
                                             " got " + typeName(at));
        }
    }
    mc->inferred_type = resolveType(ftype->ret);
    return ftype->ret;
}
std::shared_ptr<Type>
TypeChecker::inferFuncCall(const std::shared_ptr<FuncCall> &call,
                           const std::shared_ptr<Type> &expected) {
    // infer function expression type
    std::shared_ptr<FunctionType> ftype = nullptr;

    auto ft = inferExpression(call->func);
    if (!ft)
        return nullptr;
    ftype = std::dynamic_pointer_cast<FunctionType>(ft);
    if (!ftype) {
        if (auto pt = std::dynamic_pointer_cast<PointerType>(ft)) {
            ftype = std::dynamic_pointer_cast<FunctionType>(resolveType(pt->base));

            if (!ftype) {
                throw TypeCheckError(call,
                                     "Attempted to call non-function pointer type: " + typeName(ft));
            }
        } else {
            throw TypeCheckError(call, "Attempted to call non-function type: " + typeName(ft));
        }
    }

    // check arity (naive, no implicit conversions)
    if (!ftype->variadic &&
        call->args.size() !=
            (ftype->params.size())) { // if method, first param is 'self'
        throw TypeCheckError(call, "Function call argument count mismatch: expected " +
                                       std::to_string(ftype->params.size()) + " got " +
                                       std::to_string(call->args.size()));
    }

    size_t n = ftype->params.size();
    for (size_t i = 0; i < call->args.size(); ++i) {
        // Pass expected parameter type to argument inference
        std::shared_ptr<Type> expected_arg_type = (i < ftype->params.size()) ? ftype->params[i] : nullptr;
        auto at = inferExpression(call->args[i], expected_arg_type);
        if (!at)
            throw TypeCheckError(call, "Failed to infer type of function call argument " +
                                           std::to_string(i));

        if (i < ftype->params.size() && !at->equals(ftype->params[i])) {
            if (canImplicitCast(at, ftype->params[i])) {
                call->args[i] = std::make_shared<TypeCast>(call->args[i], ftype->params[i], CastType::Normal);
            } else
                throw TypeCheckError(call, "Function call argument " + std::to_string(i) +
                                               " type mismatch: expected " + typeName(ftype->params[i]) +
                                               " got " + typeName(at));
        }
    }

    call->inferred_type = resolveType(ftype->ret);
    return ftype->ret;
}

std::shared_ptr<Type> TypeChecker::inferErrorUnionFieldAccess(const std::shared_ptr<FieldAccess> &fa) {
    auto expr = std::dynamic_pointer_cast<Expression>(fa->base);
    auto base = inferExpression(expr);
    if (!base) {
        throw TypeCheckError(fa, "Failed to infer type of error union field access base");
    }
    auto eut = std::dynamic_pointer_cast<ErrorUnionType>(base);
    if (!eut) {
        throw TypeCheckError(fa, "Error union field access on non-error-union type: " + typeName(base));
    }
    if (fa->field == "err") {
        fa->inferred_type = resolveType(eut->errorType);
        return eut->errorType;
    }
    if (fa->field == "ok") {
        fa->inferred_type = resolveType(eut->valueType);
        return eut->valueType;
    }
    if (fa->field == "is_err") {
        fa->inferred_type = std::make_shared<Boolean>();
        return fa->inferred_type;
    }
    throw TypeCheckError(fa, "Error union has no field " + fa->field);
}

std::shared_ptr<Type> TypeChecker::inferArrayFieldAccess(const std::shared_ptr<FieldAccess> &fa) {
    auto expr = std::dynamic_pointer_cast<Expression>(fa->base);
    auto base = inferExpression(expr);
    if (!base) {
        throw TypeCheckError(fa, "Failed to infer type of error union field access base");
    }
    auto arrty = std::dynamic_pointer_cast<ArrayType>(base);
    if (!arrty) {
        throw TypeCheckError(fa, "Error union field access on non-error-union type: " + typeName(base));
    }
    if (fa->field == "ptr") {
        fa->inferred_type = std::make_shared<PointerType>(resolveType(arrty->element_type));
        return fa->inferred_type;
    }
    if (fa->field == "len") {
        fa->inferred_type = std::make_shared<I64>();
        return fa->inferred_type;
    }
    throw TypeCheckError(fa, "Error union has no field " + fa->field);
}

std::shared_ptr<Type>
TypeChecker::inferFieldAccess(const std::shared_ptr<FieldAccess> &fa) {
    if (auto baseExpr = std::dynamic_pointer_cast<Expression>(fa->base)) {
        auto bt = inferExpression(baseExpr);
        if (!bt)
            throw TypeCheckError(fa, "Failed to infer type of field access base");
        auto st = std::dynamic_pointer_cast<StructType>(bt);
        if (!st) {
            auto pt = std::dynamic_pointer_cast<PointerType>(bt);
            if (pt)
                st = std::dynamic_pointer_cast<StructType>(resolveType(pt->base));
            if (!pt) {
                auto eut = std::dynamic_pointer_cast<ErrorUnionType>(bt);
                if (eut) {
                    return inferErrorUnionFieldAccess(fa);
                }
                auto arrt = std::dynamic_pointer_cast<ArrayType>(bt);
                if (arrt) {
                    return inferArrayFieldAccess(fa);
                }
                auto ut = std::dynamic_pointer_cast<UnionType>(bt);
                if (ut) {
                    // field access on union type: get the field from the union
                    auto ft = ut->getFieldType(fa->field);
                    if (!ft) {
                        throw TypeCheckError(fa, "Union has no field " + fa->field);
                    }
                    fa->inferred_type = resolveType(ft);
                    return ft;
                }
                throw TypeCheckError(fa, "Field access on disallowed type: " + typeName(bt));
            }
        }
        auto ft = st->getFieldType(fa->field);
        if (!ft) {
            throw TypeCheckError(fa, fa->str() + " has no field " + fa->field);
        }
        fa->inferred_type = resolveType(ft);
        return ft;
    }
    throw TypeCheckError(fa->base, "FieldAccess base is not an expression: " + fa->base->str());
}

std::shared_ptr<Type>
TypeChecker::inferOffsetAccess(const std::shared_ptr<OffsetAccess> &oa) {
    // base could be expression or ASTNode
    if (auto baseExpr = std::dynamic_pointer_cast<Expression>(oa->base)) {
        auto bt = inferExpression(baseExpr);
        inferExpression(oa->index);
        if (!bt)
            throw TypeCheckError(oa, "Failed to infer type of offset access base");
        if (auto at = std::dynamic_pointer_cast<ArrayType>(bt)) {
            auto idxt = inferExpression(oa->index);
            if (!idxt)
                throw TypeCheckError(oa, "Failed to infer type of offset access index");
            oa->inferred_type = resolveType(at->element_type);
            return at->element_type;
        }
        if (auto pt = std::dynamic_pointer_cast<PointerType>(bt)) {
            // pointer indexing returns base
            auto bt = resolveType(pt->base);
            oa->inferred_type = bt;
            return bt;
        }
        throw TypeCheckError(oa, "Offset access on non-array/pointer type: " + typeName(bt));
    } else {
        throw TypeCheckError(oa->base, "OffsetAccess base is not an expression: " + oa->base->str());
    }
}

std::shared_ptr<Type>
TypeChecker::inferStructInit(const std::shared_ptr<StructInitializer> &init,
                             const std::shared_ptr<Type> &expected) {
    std::shared_ptr<Type> _st = inferExpression(init->struct_type_expr);
    auto st = std::dynamic_pointer_cast<StructType>(_st);
    if (!st) {
        throw TypeCheckError(init, "Struct initializer with non-struct type: " + typeName(_st));
    }

    for (auto &p : init->field_values) {
        auto fieldType = resolveType(st->getFieldType(p.first));
        if (fieldType == nullptr) {
            std::cerr << "DEBUG: struct fields:\n";
            for (const auto &f : st->fields) {
                std::cerr << "  " << f.first << ": " << f.second->str() << "\n";
            }
            std::cerr << "Struct " + st->name + " has no field " + p.first << "\n";
            throw TypeCheckError(init, "Struct " + st->name + " has no field " + p.first);
        }
        auto typeActual = inferExpression(p.second);
        if (!typeActual)
            return nullptr;
        if (!typeActual->equals(fieldType)) {
            if (canImplicitCast(typeActual, fieldType)) {
                p.second = std::make_shared<TypeCast>(p.second, fieldType, CastType::Normal);
            } else {
                throw TypeCheckError(init, "Struct " + st->name + " field " + p.first +
                                               " type mismatch: expected " + typeName(fieldType) + " got " +
                                               typeName(typeActual));
            }
        }
    }
    init->inferred_type = resolveType(st);
    return st;
}
