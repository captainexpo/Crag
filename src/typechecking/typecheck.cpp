#include "typecheck.h"
#include "../ast/ast.h"
#include "../const_eval.h"
#include "src/backend.h"
#include "src/typechecking/tables.h"
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <cstring>
#include <memory>
#include <optional>
#include <stdexcept>

// TODO: FFS don't do this, but it's the easiest way to solve the conflict between the when blocks and the module resolver.
void rebuildModuleMetadata(const std::shared_ptr<Module> &module) {
    if (!module || !module->ast) {
        return;
    }

    module->exports.clear();
    module->externLinkage.clear();

    for (const auto &decl : module->ast->declarations) {
        if (auto f = std::dynamic_pointer_cast<FunctionDeclaration>(decl)) {
            if (f->is_pub) {
                module->exports[f->name] = f;
            }
            if (f->is_extern || f->attributes.count("noprefix")) {
                module->externLinkage.insert(f->name);
            }
        } else if (auto s = std::dynamic_pointer_cast<StructDeclaration>(decl)) {
            if (s->is_pub) {
                module->exports[s->name] = s;
            }
        } else if (auto e = std::dynamic_pointer_cast<EnumDeclaration>(decl)) {
            if (e->is_pub) {
                module->exports[e->name] = e;
            }
        } else if (auto v = std::dynamic_pointer_cast<VariableDeclaration>(decl)) {
            if (v->is_pub) {
                module->exports[v->name] = v;
            }
            if (v->is_extern) {
                module->externLinkage.insert(v->name);
            }
        }
    }
}

TypeChecker::TypeChecker(CompilerOptions options) : m_const_eval(*new ConstEvaluator(this)) {
    m_const_eval.addIntrinsic(
        "@target_ptr_width",
        std::make_shared<Literal>(options.target.arch == X86 ? 32 : 64,
                                  std::make_shared<USize>()));
    m_const_eval.addIntrinsic(
        "@target_arch",
        std::make_shared<Literal>(std::string(options.target.archToString()),
                                  std::make_shared<PointerType>(std::make_shared<U8>(), true)));
    m_const_eval.addIntrinsic(
        "@target_os",
        std::make_shared<Literal>(std::string(options.target.osToString()),
                                  std::make_shared<PointerType>(std::make_shared<U8>(), true)));
    m_const_eval.addIntrinsic(
        "@target_env",
        std::make_shared<Literal>(std::string(options.target.environmentToString()),
                                  std::make_shared<PointerType>(std::make_shared<U8>(), true)));

    m_expected_return_type = nullptr;
    m_errors.clear();
    m_mod_scopes.clear();
    m_templates.clear();
    m_current_generic_types.clear();
    m_struct_methods.clear();
    compilerOptions = options;

    // pushScope();
}

SymbolTable &TypeChecker::symbolTable() {
    return symbol_table.lookupModule(current_module->id)->first;
}

const SymbolTable &TypeChecker::symbolTable() const {
    return symbol_table.lookupModule(current_module->id)->first;
}

std::shared_ptr<Type> TypeChecker::lookupNamedType(const std::string &name) {
    if (currentScopes().empty()) {
        throw std::runtime_error("No current scopes in lookupNamedType");
    }
    // Iteratively check scopes from innermost to outermost
    for (auto it = currentScopes().rbegin(); it != currentScopes().rend(); ++it) {
        auto &scope = *it;
        SymbolId id = scope.find(name);
        if (id != INVALID_SYMBOL_ID) {
            auto entry = symbolTable().get(id);
            if (entry) {
                return (*entry).type;
            }
        }
    }
    return nullptr;
}

std::optional<Symbol> TypeChecker::lookupNamedSymbol(const std::string &name) {
    if (currentScopes().empty()) {
        throw std::runtime_error("No current scopes in lookupNamedSymbol");
    }
    // Iteratively check scopes from innermost to outermost
    for (auto it = currentScopes().rbegin(); it != currentScopes().rend(); ++it) {
        auto &scope = *it;
        SymbolId id = scope.find(name);
        if (id != INVALID_SYMBOL_ID) {
            return symbol_table.lookupSymbol(id);
        }
    }
    return std::nullopt;
}

std::shared_ptr<StructType> TypeChecker::lookupStructType(const std::string &name) {
    return std::dynamic_pointer_cast<StructType>(lookupNamedType(name));
}

std::shared_ptr<UnionType> TypeChecker::lookupUnionType(const std::string &name) {
    return std::dynamic_pointer_cast<UnionType>(lookupNamedType(name));
}

std::shared_ptr<EnumType> TypeChecker::lookupEnumType(const std::string &name) {
    return std::dynamic_pointer_cast<EnumType>(lookupNamedType(name));
}

std::shared_ptr<FunctionType> TypeChecker::lookupFunctionType(const std::string &name) {
    return std::dynamic_pointer_cast<FunctionType>(lookupNamedType(name));
}

void TypeChecker::pushScope() { currentScopes().emplace_back(); }

void TypeChecker::popScope() {
    if (!currentScopes().empty()) {
        currentScopes().pop_back();
    }
}

bool TypeChecker::insertSymbol(const std::string &name, SymbolKind kind,
                               std::shared_ptr<Type> t, ASTNodePtr decl, SymbolId *id_out) {
    if (currentScopes().empty())
        pushScope();

    auto &top = currentScopes().back();

    if (top.find(name) != INVALID_SYMBOL_ID)
        return false;

    SymbolId id = symbolTable().insert({kind, name, t, decl});

    top.symbols[name] = id;
    if (id_out)
        *id_out = id;
    return true;
}

void TypeChecker::ensureGlobalVariableVisible(const std::string &name) {
    if (!current_module || !current_module->ast || currentScopes().empty()) {
        return;
    }

    if (currentScopes().front().find(name) != INVALID_SYMBOL_ID) {
        return;
    }

    for (const auto &decl : current_module->ast->declarations) {
        auto vd = std::dynamic_pointer_cast<VariableDeclaration>(decl);
        if (!vd || vd->name != name) {
            continue;
        }

        auto saved_scopes = currentScopes();
        while (currentScopes().size() > 1) {
            currentScopes().pop_back();
        }
        try {
            checkVariableDeclaration(vd);
        } catch (...) {
            currentScopes() = std::move(saved_scopes);
            throw;
        }
        currentScopes() = std::move(saved_scopes);
        return;
    }
}

std::optional<SymbolId> TypeChecker::lookupSymbolInScope(const std::string &name) {
    for (auto it = currentScopes().rbegin(); it != currentScopes().rend(); ++it) {
        SymbolId id = it->find(name);

        if (id != INVALID_SYMBOL_ID)
            return id;
    }
    return std::nullopt;
}

bool TypeChecker::canImplicitCast(const std::shared_ptr<Type> &from, const std::shared_ptr<Type> &to) {
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

    // str coerces to *u8, *const u8, or *void (pointer decay for C interop)
    if (from_tk == TypeKind::Str && to_tk == TypeKind::Pointer) {
        auto to_ptr = dynamic_cast<PointerType *>(to.get());
        if (to_ptr && (to_ptr->base->kind() == TypeKind::U8 || to_ptr->base->kind() == TypeKind::Void))
            return true;
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
    TypeKind to_tk = to->kind();
    if (from_tk == to_tk)
        return true; // TODO: Figure out if this is bad
    if (from_tk == TypeKind::Enum && to->isNumeric())
        return true;
    if (from_tk == TypeKind::Bool && to->isNumeric())
        return true;
    if (from->isNumeric() && to_tk == TypeKind::Bool)
        return true;
    if (from_tk == TypeKind::Null && to_tk == TypeKind::Pointer)
        return true;
    if (from_tk == TypeKind::Pointer && to_tk == TypeKind::Pointer)
        return true;

    // Numeric -> Ptr, Ptr -> Numeric

    if (from_tk == TypeKind::Pointer && to->isInteger())
        return true;
    if (from->isInteger() && to_tk == TypeKind::Pointer)
        return true;

    // str can be explicitly cast to any pointer (decay to data pointer)
    if (from_tk == TypeKind::Str && to_tk == TypeKind::Pointer)
        return true;

    return false;
}
std::shared_ptr<Type> TypeChecker::getCastType(std::shared_ptr<ASTNode> &node, const std::shared_ptr<Type> &from,
                                               const std::shared_ptr<Type> &to) {
    if (!from || !to)
        throw TypeCheckError(current_module, node,
                             "getCastType: from or to type is null");
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
            throw TypeCheckError(current_module, node,
                                 "getCastType: failed to cast to/from pointer types");

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
    return t ? t->str() : "<no_type>";
}

static bool containsGenericType(const std::shared_ptr<Type> &t) {
    if (!t)
        return false;
    if (std::dynamic_pointer_cast<GenericType>(t))
        return true;
    if (auto ti = std::dynamic_pointer_cast<TemplateInstanceType>(t)) {
        if (containsGenericType(ti->base))
            return true;
        for (const auto &arg : ti->type_args) {
            if (containsGenericType(arg))
                return true;
        }
        return false;
    }
    if (auto pt = std::dynamic_pointer_cast<PointerType>(t))
        return containsGenericType(pt->base);
    if (auto at = std::dynamic_pointer_cast<ArrayType>(t))
        return containsGenericType(at->element_type);
    if (auto ft = std::dynamic_pointer_cast<FunctionType>(t)) {
        if (containsGenericType(ft->ret))
            return true;
        for (const auto &param : ft->params) {
            if (containsGenericType(param))
                return true;
        }
        return false;
    }
    if (auto eu = std::dynamic_pointer_cast<ErrorUnionType>(t)) {
        return containsGenericType(eu->valueType) || containsGenericType(eu->errorType);
    }
    return false;
}

static void substituteGenericTypes(std::shared_ptr<Type> &type,
                                   const std::unordered_map<std::string, std::shared_ptr<Type>> &generic_map) {
    if (!type)
        return;
    if (auto gt = std::dynamic_pointer_cast<GenericType>(type)) {
        auto it = generic_map.find(gt->name);
        if (it != generic_map.end())
            type = it->second;
        return;
    }
    if (auto eut = std::dynamic_pointer_cast<ErrorUnionType>(type)) {
        substituteGenericTypes(eut->valueType, generic_map);
        substituteGenericTypes(eut->errorType, generic_map);
        return;
    }
    if (auto ti = std::dynamic_pointer_cast<TemplateInstanceType>(type)) {
        substituteGenericTypes(ti->base, generic_map);
        for (auto &arg : ti->type_args) {
            substituteGenericTypes(arg, generic_map);
        }
        return;
    }
    if (auto pt = std::dynamic_pointer_cast<PointerType>(type)) {
        substituteGenericTypes(pt->base, generic_map);
        return;
    }
    if (auto at = std::dynamic_pointer_cast<ArrayType>(type)) {
        substituteGenericTypes(at->element_type, generic_map);
        return;
    }
    if (auto ft = std::dynamic_pointer_cast<FunctionType>(type)) {
        substituteGenericTypes(ft->ret, generic_map);
        for (auto &param : ft->params) {
            substituteGenericTypes(param, generic_map);
        }
        return;
    }
    if (auto st = std::dynamic_pointer_cast<StructType>(type)) {
        for (auto &field : st->fields) {
            substituteGenericTypes(field.second, generic_map);
        }
        return;
    }
    if (auto ut = std::dynamic_pointer_cast<UnionType>(type)) {
        for (auto &variant : ut->fields) {
            substituteGenericTypes(variant.second, generic_map);
        }
        return;
    }
}

std::shared_ptr<Type> TypeChecker::resolveType(const std::shared_ptr<ASTNode> &node, const std::shared_ptr<Type> &t) {
    if (!t) {
        throw TypeCheckError(current_module, node, "Attempted to resolve a null type");
    }

    // Resolve bound generics if any (used in instantiation paths)
    if (auto gt = std::dynamic_pointer_cast<GenericType>(t)) {
        // In general, GenericType should be substituted via explicit bindings; if none, leave as-is
        return t;
    }
    if (auto eut = std::dynamic_pointer_cast<ErrorUnionType>(t)) {
        auto vt = resolveType(node, eut->valueType);
        auto et = resolveType(node, eut->errorType);
        return std::make_shared<ErrorUnionType>(vt, et);
    }
    if (auto st = std::dynamic_pointer_cast<StructType>(t)) {
        auto resolved = lookupStructType(st->name);
        if (resolved) {
            return resolved;
        }
        return st; // unknown struct type
    }
    if (auto pt = std::dynamic_pointer_cast<PointerType>(t)) {
        auto bt = resolveType(node, pt->base);
        if (!bt)
            throw TypeCheckError(current_module, node, "Could not resolve base type of pointer type '" + pt->str() + "'");
        return std::make_shared<PointerType>(bt, pt->pointer_const);
    }
    if (auto at = std::dynamic_pointer_cast<ArrayType>(t)) {
        auto bt = resolveType(node, at->element_type);
        if (at->length_expr)
            at->length_expr->inferred_type = inferExpression(at->length_expr);

        if (!bt)
            throw TypeCheckError(current_module, node, "Could not resolve array element type for '" + at->str() + "'");
        return std::make_shared<ArrayType>(bt, at->length_expr, at->unsized);
    }
    if (auto ft = std::dynamic_pointer_cast<FunctionType>(t)) {
        auto rt = resolveType(node, ft->ret);
        if (!rt)
            throw TypeCheckError(current_module, node, "Could not resolve function return type in '" + ft->str() + "'");
        std::vector<std::shared_ptr<Type>> pts;
        for (const auto &p : ft->params) {
            auto pt = resolveType(node, p);
            if (!pt)
                throw TypeCheckError(current_module, node, "Could not resolve one of the function parameter types in '" + ft->str() + "'");
            pts.push_back(pt);
        }
        return std::make_shared<FunctionType>(pts, rt, ft->variadic, ft->is_extern);
    }
    if (auto qt = std::dynamic_pointer_cast<QualifiedType>(t)) {
        if (qt->module_path.empty()) {
            auto resolved = lookupNamedType(qt->type_name);
            if (resolved)
                return resolved;
            return qt; // unknown type
        } else {
            auto og_mod = current_module;
            auto cur_mod = og_mod;
            for (const auto &part : qt->module_path) {
                auto maybe = cur_mod->imports.find(part);
                if (maybe == cur_mod->imports.end()) {
                    throw TypeCheckError(current_module, node, "Could not resolve module '" + part + "' in qualified type '" + qt->str() + "'");
                }
                cur_mod = maybe->second;
            }
            auto resolved_id = m_mod_scopes[cur_mod->id].front().find(qt->type_name);
            if (resolved_id == INVALID_SYMBOL_ID) {
                throw TypeCheckError(current_module, node, "Could not resolve type '" + qt->type_name + "' in module '" + cur_mod->name + "'");
            }
            auto resolved = symbol_table.lookupSymbol(resolved_id);
            if (!resolved) {
                throw TypeCheckError(current_module, node, "Symbol '" + qt->type_name + "' in module '" + cur_mod->canon_name + "' is not a type for qualified type '" + qt->str() + "'");
            }
            return resolved->type;
        }
    }
    if (auto tut = std::dynamic_pointer_cast<TemplateInstanceType>(t)) {
        // Resolve type arguments
        std::vector<std::shared_ptr<Type>> resolved_args;
        for (const auto &arg : tut->type_args) {
            auto rat = resolveType(node, arg);
            resolved_args.push_back(rat);
        }

        bool has_generic_arg = false;
        for (const auto &arg : resolved_args) {
            if (containsGenericType(arg)) {
                has_generic_arg = true;
                break;
            }
        }
        if (has_generic_arg) {
            auto base_type = resolveType(node, tut->base);
            return std::make_shared<TemplateInstanceType>(base_type, resolved_args);
        }

        // Check if base is a qualified type (module-qualified template)
        if (auto qt = std::dynamic_pointer_cast<QualifiedType>(tut->base)) {
            auto ti = std::make_shared<TemplateInstantiation>(qt->module_path, qt->type_name, resolved_args);
            auto pair = inferTemplateInstantiation(ti);
            auto instantiated_type = pair.first;
            if (!instantiated_type) {
                throw TypeCheckError(current_module,
                                     nullptr,
                                     "Failed to resolve template instantiation type for: " + tut->str());
            }
            return instantiated_type;
        }

        // Otherwise, resolve the base type normally
        auto base_type = resolveType(node, tut->base);
        if (auto struct_type = std::dynamic_pointer_cast<StructType>(base_type)) {
            auto ti = std::make_shared<TemplateInstantiation>(struct_type->name, resolved_args);
            auto pair = inferTemplateInstantiation(ti); // Will instantiate the template
            auto instantiated_type = pair.first;
            if (!instantiated_type) {
                throw TypeCheckError(current_module,
                                     nullptr,
                                     "Failed to resolve template instantiation type for: " + tut->str());
            }
            return instantiated_type;
        }
        if (auto base_template = std::dynamic_pointer_cast<TemplateInstanceType>(base_type)) {
            bool all_generic_params = base_template->type_args.size() == resolved_args.size();
            std::unordered_map<std::string, std::shared_ptr<Type>> generic_map;
            if (all_generic_params) {
                for (size_t i = 0; i < base_template->type_args.size(); ++i) {
                    auto gt = std::dynamic_pointer_cast<GenericType>(base_template->type_args[i]);
                    if (!gt) {
                        all_generic_params = false;
                        break;
                    }
                    generic_map[gt->name] = resolved_args[i];
                }
            }
            if (all_generic_params) {
                auto base_copy = base_template->instantiate();
                substituteGenericTypes(base_copy, generic_map);
                auto collapsed = resolveType(node, base_copy);
                if (auto collapsed_struct = std::dynamic_pointer_cast<StructType>(collapsed)) {
                    auto ti = std::make_shared<TemplateInstantiation>(collapsed_struct->name, resolved_args);
                    auto pair = inferTemplateInstantiation(ti);
                    if (!pair.first) {
                        throw TypeCheckError(current_module,
                                             nullptr,
                                             "Failed to resolve template instantiation type for: " + tut->str());
                    }
                    return pair.first;
                }
                return collapsed;
            }
            return std::make_shared<TemplateInstanceType>(base_template, resolved_args);
        }
        throw TypeCheckError(current_module,
                                node,
                             "Template instantiation base type is not a struct: " + base_type->str());
    }
    // primitive types are already resolved
    return t;
}

// Entry point
void TypeChecker::check(std::shared_ptr<Module> module) {

    // auto old_scopes = std::move(currentScopes());
    // currentScopes() = std::vector<TypeScope>();

    if (!module || !module->ast)
        return;

    if (module->typechecked) {
        current_module = module;
        return;
    }

    if (module->typechecking) {
        return;
    }

    if (!main_module) {
        main_module = module;
        m_const_eval.addIntrinsic(
            "@main_mod",
            std::make_shared<Literal>((uint64_t)main_module->id,
                                      std::make_shared<U64>()));
    }

    module->typechecking = true;
    current_module = module;

    pushScope();

    symbol_table.insertModule(module);

    std::vector<std::shared_ptr<StructDeclaration>> struct_decls;
    std::vector<std::shared_ptr<UnionDeclaration>> union_decls;

    try {
        for (int i = 0; i < module->ast->declarations.size(); ++i) {
            auto decl = module->ast->declarations[i];
            if (auto sd = std::dynamic_pointer_cast<StructDeclaration>(decl)) {

                // Check if this is a generic struct
                if (sd->generic_params.size() > 0) {
                    SymbolId tid;
                    insertSymbol(sd->name, SymbolKind::Type, nullptr, sd, &tid);
                    m_templates[tid] = sd;
                } else {
                    // Build a StructType and register
                    auto st = std::make_shared<StructType>(sd->name);
                    st->complete = false;
                    insertSymbol(sd->name, SymbolKind::Type, st, sd);
                    struct_decls.push_back(sd);
                }
            } else if (auto ud = std::dynamic_pointer_cast<UnionDeclaration>(decl)) {
                // Build a UnionType and register
                auto ut = std::make_shared<UnionType>(ud->name);
                ut->complete = false;
                insertSymbol(ud->name, SymbolKind::Type, ut, ud);
                union_decls.push_back(ud);
            } else if (auto fd = std::dynamic_pointer_cast<FunctionDeclaration>(decl)) {
                if (fd->generic_params.size() > 0) {
                    SymbolId tid;
                    insertSymbol(fd->name, SymbolKind::Function, fd->type, fd, &tid);
                    decl->symbol_id = tid;
                    m_templates[tid] = fd;
                } else {
                    auto ty = std::dynamic_pointer_cast<FunctionType>(resolveType(fd, fd->type));

                    SymbolId fid;
                    insertSymbol(fd->name, SymbolKind::Function, ty, fd, &fid);
                    decl->symbol_id = fid;
                }
            } else if (auto en = std::dynamic_pointer_cast<EnumDeclaration>(decl)) {
                if (!en->base_type) {
                    throw TypeCheckError(current_module, en, "Enum " + en->name + " has no base type");
                    return;
                }
                auto et = en->enum_type;
                for (const auto &v : en->variants) {
                    et->addVariant(v.first, v.second);

                    // infer type of variant value
                    std::shared_ptr<Expression> val = v.second;
                    auto i = inferExpression(val);
                    auto vtype = resolveType(en, i);
                    if (!vtype->equals(en->base_type)) {
                        throw TypeCheckError(current_module, v.second,
                                             "Enum variant " + v.first + " has type " + typeName(vtype) +
                                                 ", expected " + typeName(en->base_type));
                    }
                }

                SymbolId eid;
                insertSymbol(en->name, SymbolKind::Type, et, en, &eid);

                en->symbol_id = eid;
                en->inferred_type = et;
            } else if (auto im = std::dynamic_pointer_cast<ImportDeclaration>(decl)) {
                auto imported_module = module->imports[im->alias];

                if (!imported_module) {
                    throw TypeCheckError(current_module, im, "Failed to resolve imported module: " + im->alias);
                }

                if (!imported_module->typechecked) {
                    symbol_table.insertModule(imported_module);

                    auto og_module = current_module;
                    // Typecheck module
                    check(imported_module);
                    if (!ok()) {
                        return;
                    }

                    current_module = og_module;
                }
            } else if (auto ta = std::dynamic_pointer_cast<TypeAliasDeclaration>(decl)) {
                std::shared_ptr<Type> alias_type = ta->aliased_type;
                if (ta->generic_params.empty()) {
                    alias_type = resolveType(ta, ta->aliased_type);
                }
                if (!alias_type) {
                    throw TypeCheckError(current_module, ta, "Type alias " + ta->name + " has unknown aliased type");
                    return;
                }
                SymbolId aid;
                insertSymbol(ta->name, SymbolKind::Type, alias_type, ta, &aid);
                ta->symbol_id = aid;
            } else if (auto wb = std::dynamic_pointer_cast<WhenBlock>(decl)) {
                auto cond_lit_opt = m_const_eval.evaluateExpression(wb->condition);
                if (!m_const_eval.ok()) {
                    for (const auto &err : m_const_eval.errors()) {
                        m_errors.emplace_back(TypeCheckError(current_module, wb->condition, "Failed to evaluate when block condition: " + err.second));
                    }
                    throw TypeCheckError(current_module, wb, "Failed to evaluate condition for when block");
                }
                if (!cond_lit_opt) {
                    for (const auto &err : m_const_eval.errors()) {
                        m_errors.emplace_back(TypeCheckError(current_module, wb->condition, "Failed to evaluate when block condition: " + err.second));
                    }
                    throw TypeCheckError(current_module, wb, "Failed to evaluate condition for when block");
                }
                auto cond_lit = *cond_lit_opt;

                if (auto bool_lit = std::dynamic_pointer_cast<Literal>(cond_lit)) {
                    if (bool_lit->lit_type->kind() != TypeKind::Bool) {
                        throw TypeCheckError(current_module, wb, "When block condition is not a boolean");
                    }
                    bool condition_true = std::get<bool>(bool_lit->value);
                    if (condition_true) {
                        module->ast->declarations.insert(
                            module->ast->declarations.begin() + i + 1, wb->body.begin(), wb->body.end());
                        module->ast->declarations.erase(module->ast->declarations.begin() + i);
                        i -= 1;
                    } else {
                        module->ast->declarations.erase(module->ast->declarations.begin() + i);
                        i -= 1;
                    }
                } else {
                    throw TypeCheckError(current_module, wb, "When block condition is not a literal");
                }
            }
        }

        for (const auto &decl : module->ast->declarations) {
            if (auto vd = std::dynamic_pointer_cast<VariableDeclaration>(decl)) {
                auto global_symbol = currentScopes().front().find(vd->name);
                if (global_symbol != INVALID_SYMBOL_ID) {
                    auto entry = symbolTable().get(global_symbol);
                    if (entry && entry->decl == vd) {
                        continue;
                    }
                }
                checkVariableDeclaration(vd);
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
            checkStructDeclaration(sd);
        }
        for (const auto &ud : union_decls) {
            checkUnionDeclaration(ud);
        }
        for (const auto &decl : module->ast->declarations) {
            try {
                checkNode(decl);
            } catch (const TypeCheckError &e) {
                m_errors.emplace_back(e);
            }
        }

        rebuildModuleMetadata(module);
    } catch (const TypeCheckError &e) {
        m_errors.emplace_back(e);
    }

    module->typechecking = false;
    module->typechecked = true;
    // while (currentScopes().size() > 1) {
    //     popScope();
    // }
    // currentScopes() = std::move(old_scopes);
    // Dump whole symbol table for debugging
    // symbol_table.dump();
}

void TypeChecker::checkNode(const std::shared_ptr<ASTNode> &node) {
    if (!node)
        return;
    if (auto fd = std::dynamic_pointer_cast<FunctionDeclaration>(node)) {
        checkFunctionDeclaration(fd);
        return;
    }
    if (auto vd = std::dynamic_pointer_cast<VariableDeclaration>(node)) {
        if (!currentScopes().empty()) {
            auto global_symbol = currentScopes().front().find(vd->name);
            if (global_symbol != INVALID_SYMBOL_ID) {
                auto entry = symbolTable().get(global_symbol);
                if (entry && entry->decl == vd) {
                    return;
                }
            }
        }
        if (currentScopes().size() != 1) {
            return;
        }
        checkVariableDeclaration(vd);
        return;
    }
    if (auto sd = std::dynamic_pointer_cast<StructDeclaration>(node)) {
        // checkStructDeclaration(sd);
        return;
    }
    if (auto ud = std::dynamic_pointer_cast<UnionDeclaration>(node)) {
        // checkUnionDeclaration(ud);
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
    if (auto wb = std::dynamic_pointer_cast<WhenBlock>(node)) {
        // Already handled in check()
        return;
    }
    throw TypeCheckError(current_module, node, "Unsupported top-level declaration: " + node->toString());
}

void TypeChecker::checkStructDeclaration(
    const std::shared_ptr<StructDeclaration> &sd) {
    auto fields = std::vector<std::pair<std::string, std::shared_ptr<Type>>>();
    for (int i = 0; i < sd->fields.size(); ++i) {
        auto &f = sd->fields[i];
        sd->fields[i].second = resolveType(sd, f.second);
        fields.emplace_back(f.first, resolveType(sd, f.second));
    }
    auto st = lookupStructType(sd->name);
    if (st) {
        st->fields = std::move(fields);
        st->methods = sd->methods;
        st->complete = true;
        std::cout << "Completed struct type: " << st->str() << std::endl;
    } else {
        throw TypeCheckError(current_module, sd, "Internal error: struct " + sd->name + " not found in map");
    }

    for (const auto &m : sd->methods) {
        // Type check method bodies
        checkFunctionDeclaration(m.second);
    }
}

void TypeChecker::checkUnionDeclaration(
    const std::shared_ptr<UnionDeclaration> &ud) {
    auto fields = std::vector<std::pair<std::string, std::shared_ptr<Type>>>();
    for (const auto &f : ud->fields) {
        fields.emplace_back(f.first, resolveType(ud, f.second));
    }
    auto ut = lookupUnionType(ud->name);
    if (ut) {
        ut->fields = std::move(fields);
        ut->complete = true;
    } else {
        throw TypeCheckError(current_module, ud, "Internal error: union " + ud->name + " not found in map");
    }
}

void TypeChecker::checkEnumDeclaration(
    const std::shared_ptr<EnumDeclaration> &en) {
}

void TypeChecker::checkFunctionDeclaration(
    const std::shared_ptr<FunctionDeclaration> &fn) {
    if (!fn->type) {
        throw TypeCheckError(current_module, fn, "Function " + fn->name + " has no type information");
        return;
    }
    fn->type = std::dynamic_pointer_cast<FunctionType>(resolveType(fn, fn->type));
    pushScope();
    for (size_t i = 0; i < fn->type->params.size(); ++i) {
        std::string pname =
            (i < fn->param_names.size() ? fn->param_names[i]
                                        : ("arg" + std::to_string(i)));
        SymbolId psid;
        if (!insertSymbol(pname, SymbolKind::Variable, fn->type->params[i], fn, &psid)) {
            throw TypeCheckError(current_module, fn,
                                 "Duplicate parameter name " + pname + " in function " + fn->name);
        }
        fn->param_symbols.push_back(psid);
    }

    m_expected_return_type = resolveType(fn, fn->type->ret);
    // Also update the function's return type in the AST for code generation
    fn->type->ret = m_expected_return_type;
    if (fn->body) {
        checkStatement(fn->body);
    }

    popScope();
}

void TypeChecker::checkVariableDeclaration(
    const std::shared_ptr<VariableDeclaration> &var) {
    const std::string &name = var->name;

    if (var->var_type)
        var->var_type = resolveType(var, var->var_type);

    SymbolId symbol_id = INVALID_SYMBOL_ID;
    if (!var->initializer) {
        if (!insertSymbol(name, SymbolKind::Variable, var->var_type, var, &symbol_id)) {
            throw TypeCheckError(current_module, var, "Duplicate variable declaration: " + name);
        }
        var->symbol_id = symbol_id;
        return;
    }

    auto init = var->initializer;
    try {
        if (auto expr = std::dynamic_pointer_cast<Expression>(init)) {

            auto init_type = resolveType(expr, inferExpression(expr, var->var_type));

            // If the initializer expression was rewritten by inference (e.g. builtins
            // such as sizeof/slice expand into other AST nodes), make sure the
            // variable keeps the rewritten initializer for later checks and codegen.
            var->initializer = expr;
            init = var->initializer;

            if (!init_type) {
                throw TypeCheckError(current_module,
                                     init,
                                     "Failed to infer type of initializer for variable " + name);
            }

            // Type inference for variable
            if (!var->var_type) {
                var->var_type = init_type;
            }

            var->var_type->is_const = var->is_const;

            if (auto arr_lit = std::dynamic_pointer_cast<ArrayLiteral>(expr)) {
                if (auto arr_type =
                        std::dynamic_pointer_cast<ArrayType>(var->var_type)) {

                    handleArrayLiteralAssignment(
                        name, arr_lit, arr_type);
                }
            }

            if (!init_type->equals(var->var_type)) {
                if (canImplicitCast(init_type, var->var_type)) {
                    var->initializer =
                        std::make_shared<TypeCast>(
                            expr, var->var_type, CastType::Normal);
                } else if (canExplicitCast(init_type, var->var_type)) {
                    throw TypeCheckError(current_module,
                                         init,
                                         "Explicit cast needed in initializer for variable " + name +
                                             ": cannot implicitly convert " +
                                             typeName(init_type) + " to " +
                                             typeName(var->var_type));
                } else {
                    throw TypeCheckError(current_module,
                                         var,
                                         "Type mismatch in initializer for variable " + name +
                                             ": expected " + typeName(var->var_type) +
                                             " but got " + typeName(init_type));
                }
            }

            if (var->is_const) {
                m_const_eval.clearErrors();
                auto evaluated = m_const_eval.evaluateVariableDeclaration(var);
                if (!m_const_eval.ok()) {
                    warn("Failed to evaluate initializer for const variable " + name +
                         ": " + m_const_eval.errors().front().second);
                    // for (const auto &e : m_const_eval.errors()) {
                    //     throw TypeCheckError(current_module, e.first, e.second);
                    // }
                } else {
                    var->initializer->constant_evaluated = true;
                }
            }
        } else if (auto si =
                       std::dynamic_pointer_cast<StructInitializer>(init)) {

            auto t = inferStructInit(si, var->var_type);
            if (!t || !t->equals(var->var_type)) {
                throw TypeCheckError(current_module,
                                     init,
                                     "Type mismatch in initializer for variable " + name);
            }
        } else {
            throw TypeCheckError(current_module,
                                 init,
                                 "Unsupported initializer node for variable " + name);
        }

    } catch (const TypeCheckError &e) {

        m_errors.emplace_back(e);
        // Even if initializer has type errors, we still want to insert the variable into the symbol table, so we don't return early here
    }
    if (!insertSymbol(name, SymbolKind::Variable, var->var_type, var, &symbol_id)) {
        throw TypeCheckError(current_module,
                             var,
                             "Duplicate variable declaration: " + name);
    }

    var->symbol_id = symbol_id;
}

void TypeChecker::handleArrayLiteralAssignment(
    const std::string &name,
    const std::shared_ptr<ArrayLiteral> &arr_lit,
    const std::shared_ptr<ArrayType> &arr_type) {
    int64_t final_len = -1;

    if (arr_type->unsized) {
        final_len = static_cast<int64_t>(arr_lit->elements.size());
    } else {
        m_const_eval.clearErrors();
        auto evaluated = m_const_eval.evaluateExpression(arr_type->length_expr);

        if (!m_const_eval.ok()) {
            for (const auto &e : m_const_eval.errors()) {
                throw TypeCheckError(current_module, e.first, e.second);
            }
        }

        if (evaluated == std::nullopt) {
            throw TypeCheckError(current_module,
                                 arr_type->length_expr,
                                 "Failed to evaluate array length expression");
        }

        (*evaluated)->constant_evaluated = true;

        auto lit =
            std::dynamic_pointer_cast<Literal>(*std::move(evaluated));

        if (!lit) {
            throw TypeCheckError(current_module,
                                 arr_type->length_expr,
                                 "Array length was not a constant expression");
        }

        if (auto v = std::get_if<int64_t>(&lit->value))
            final_len = *v;
        else if (auto v = std::get_if<uint64_t>(&lit->value))
            final_len = static_cast<int64_t>(*v);
    }

    if (final_len < 0) {
        throw TypeCheckError(current_module,
                             arr_type->length_expr,
                             "Array length cannot be negative");
    }

    int64_t lit_len = static_cast<int64_t>(arr_lit->elements.size());

    if (lit_len > final_len) {
        throw TypeCheckError(current_module,
                             arr_lit,
                             "Array literal length mismatch for variable " + name +
                                 ": expected " + std::to_string(final_len) +
                                 ", got " + std::to_string(lit_len));
    }

    auto i64 = std::make_shared<I64>();
    auto len_lit =
        std::make_shared<Literal>(final_len, i64);
    len_lit->inferred_type = i64;

    arr_lit->defined_len = final_len;

    arr_type->length_expr = len_lit;
    arr_type->actualSize = final_len;
    arr_type->unsized = false;
}

void TypeChecker::checkStatement(std::shared_ptr<Statement> &stmt) {
    if (!stmt)
        return;
    if (auto block = std::dynamic_pointer_cast<Block>(stmt)) {
        pushScope();
        for (size_t i = 0; i < block->statements.size();) {
            try {
                auto &s = block->statements[i];
                if (auto when_block = std::dynamic_pointer_cast<WhenBlock>(s)) {
                    auto cond_lit_opt = m_const_eval.evaluateExpression(when_block->condition);
                    if (!m_const_eval.ok()) {
                        for (const auto &e : m_const_eval.errors()) {
                            throw TypeCheckError(current_module, e.first, e.second);
                        }
                    }
                    if (!cond_lit_opt) {
                        throw TypeCheckError(current_module, when_block->condition,
                                             "Failed to evaluate condition for when block");
                    }

                    auto cond_lit = *cond_lit_opt;
                    auto bool_lit = std::dynamic_pointer_cast<Literal>(cond_lit);
                    if (!bool_lit || bool_lit->lit_type->kind() != TypeKind::Bool) {
                        throw TypeCheckError(current_module, when_block->condition,
                                             "When block condition is not a boolean");
                    }

                    if (std::get<bool>(bool_lit->value)) {
                        std::vector<std::shared_ptr<Statement>> replacement;
                        replacement.reserve(when_block->body.size());
                        for (auto &node : when_block->body) {
                            auto body_stmt = std::dynamic_pointer_cast<Statement>(node);
                            if (!body_stmt) {
                                throw TypeCheckError(current_module,
                                                     node,
                                                     "When block body contains non-statement node: " + node->str());
                            }
                            replacement.push_back(body_stmt);
                        }

                        block->statements.erase(block->statements.begin() + i);
                        block->statements.insert(
                            block->statements.begin() + i,
                            replacement.begin(),
                            replacement.end());
                    } else {
                        block->statements.erase(block->statements.begin() + i);
                    }

                    continue;
                }

                checkStatement(s);
            } catch (const TypeCheckError &e) {
                m_errors.emplace_back(e);
            }

            ++i;
        }
        popScope();
        return;
    }
    if (auto iff = std::dynamic_pointer_cast<IfStatement>(stmt)) {
        auto t = inferExpression(iff->condition);
        if (!t || !dynamic_cast<Boolean *>(t.get())) {
            throw TypeCheckError(current_module, iff->condition,
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
                throw TypeCheckError(current_module, ret->value, "Return with value in void function");
            }
        } else {
            if (!ret->value) {
                if (ret->is_error) {
                    throw TypeCheckError(current_module, ret, "Return type mismatch: error return requires a value");
                }
                if (auto expected_error = std::dynamic_pointer_cast<ErrorUnionType>(m_expected_return_type)) {
                    if (!dynamic_cast<Void *>(expected_error->valueType.get())) {
                        throw TypeCheckError(current_module, ret, "Return type mismatch: expected " + typeName(m_expected_return_type) + " but got void return");
                    }
                    return;
                }
                if (!dynamic_cast<Void *>(m_expected_return_type.get())) {
                    throw TypeCheckError(current_module, ret, "Return type mismatch: expected " + typeName(m_expected_return_type) + " but got void return");
                }
            } else {

                auto t = resolveType(ret, ret_inferred);
                if (ret->is_error) { // Returning an error from the function (oh no!)
                    if (!dynamic_cast<ErrorUnionType *>(m_expected_return_type.get())) {
                        throw TypeCheckError(current_module, ret, "Return type mismatch: expected " + typeName(m_expected_return_type) + " but got error return");
                    }
                    auto exp_error = dynamic_cast<ErrorUnionType *>(
                        m_expected_return_type.get());
                    if (!t->equals(exp_error->errorType)) {
                        if (canImplicitCast(t, exp_error->errorType)) {
                            ret->value = std::make_shared<TypeCast>(ret->value, exp_error->errorType, CastType::Normal);
                        } else {
                            throw TypeCheckError(current_module, ret, "Return type mismatch: expected " + typeName(exp_error->errorType) + " but got " + typeName(t));
                        }
                    }
                    return;
                }
                if (auto expected_error = std::dynamic_pointer_cast<ErrorUnionType>(m_expected_return_type)) {
                    if (!t->equals(expected_error->valueType)) {
                        if (canImplicitCast(t, expected_error->valueType)) {
                            ret->value = std::make_shared<TypeCast>(ret->value, expected_error->valueType, CastType::Normal);
                        } else {
                            throw TypeCheckError(current_module, ret, "Return type mismatch: expected " + typeName(expected_error->valueType) + " but got " + typeName(t));
                        }
                    }
                    return;
                }
                if (!t || !t->equals(m_expected_return_type)) {
                    if (t && canImplicitCast(t, m_expected_return_type)) {
                        ret->value = std::make_shared<TypeCast>(ret->value, m_expected_return_type, CastType::Normal);
                    } else {
                        throw TypeCheckError(current_module, ret, "Return type mismatch: expected " + typeName(m_expected_return_type) + " but got " + typeName(t));
                    }
                }
            }
        }
        return;
    }
    if (auto switch_stmt = std::dynamic_pointer_cast<SwitchStmt>(stmt)) {
        auto cond_type = inferExpression(switch_stmt->condition);
        if (!cond_type) {
            throw TypeCheckError(current_module, switch_stmt->condition, "Failed to infer type of switch condition");
        }
        size_t idx = 0;
        for (auto &case_block : switch_stmt->cases) {
            size_t jdx = 0;
            for (auto &case_expr : case_block.first) {
                auto case_type = inferExpression(case_expr);
                if (!case_type) {
                    throw TypeCheckError(current_module, case_expr, "Failed to infer type of switch case expression");
                }
                if (!case_type->equals(cond_type)) {
                    if (canImplicitCast(case_type, cond_type)) {
                        case_expr = std::make_shared<TypeCast>(case_expr, cond_type, CastType::Normal);
                    } else {
                        throw TypeCheckError(current_module, case_expr,
                                             "Switch case type mismatch: expected " + typeName(cond_type) +
                                                 " but got " + typeName(case_type));
                    }
                }
                // Const eval
                m_const_eval.clearErrors();
                auto case_lit_opt = m_const_eval.evaluateExpression(case_expr);
                if (!m_const_eval.ok()) {
                    for (const auto &e : m_const_eval.errors()) {
                        throw TypeCheckError(current_module, e.first, e.second);
                    }
                }
                if (!case_lit_opt) {
                    throw TypeCheckError(current_module, case_expr, "Failed to evaluate switch case expression");
                }
                (*case_lit_opt)->constant_evaluated = true;
                case_block.first[jdx] = *case_lit_opt;
                case_block.first[jdx]->inferred_type = std::dynamic_pointer_cast<Literal>(*case_lit_opt)->lit_type;
                jdx++;
            }
            checkStatement(case_block.second);
            idx++;
        }
        if (switch_stmt->default_case) {
            checkStatement(switch_stmt->default_case);
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

            throw TypeCheckError(current_module, asg,
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
            throw TypeCheckError(current_module, wh->condition,
                                 "Condition in while-statement is not boolean: got " + typeName(t));
        }
        checkStatement(wh->body);
        return;
    }
    if (stmt->kind() == NodeKind::BreakStatement ||
        stmt->kind() == NodeKind::ContinueStatement) {
        return; // nothing to check
    }
    if (stmt->kind() == NodeKind::AsmStmt) {
        auto asm_stmt = std::dynamic_pointer_cast<AsmStmt>(stmt);
        for (auto &op : asm_stmt->operands) {
            inferExpression(op.expr);
        }
        return;
    }
    if (auto when_block = std::dynamic_pointer_cast<WhenBlock>(stmt)) {
        auto cond_lit_opt = m_const_eval.evaluateExpression(when_block->condition);
        if (!m_const_eval.ok()) {
            for (const auto &e : m_const_eval.errors()) {
                throw TypeCheckError(current_module, e.first, e.second);
            }
        }
        if (!cond_lit_opt) {
            throw TypeCheckError(current_module, when_block->condition, "Failed to evaluate condition for when block");
        }
        auto cond_lit = *cond_lit_opt;

        if (auto bool_lit = std::dynamic_pointer_cast<Literal>(cond_lit)) {
            if (bool_lit->lit_type->kind() != TypeKind::Bool) {
                throw TypeCheckError(current_module, when_block->condition, "When block condition is not a boolean");
            }
            bool condition_true = std::get<bool>(bool_lit->value);
            if (condition_true) {
                auto replacement_block = std::make_shared<Block>();
                replacement_block->line = when_block->line;
                replacement_block->col = when_block->col;

                for (auto &node : when_block->body) {
                    auto body_stmt = std::dynamic_pointer_cast<Statement>(node);
                    if (!body_stmt) {
                        throw TypeCheckError(current_module,
                                             node,
                                             "When block body contains non-statement node: " + node->str());
                    }
                    replacement_block->statements.push_back(body_stmt);
                }

                stmt = replacement_block;
                checkStatement(stmt);
            } else {
                auto empty_block = std::make_shared<Block>();
                empty_block->line = when_block->line;
                empty_block->col = when_block->col;
                stmt = empty_block;
            }
        } else {
            throw TypeCheckError(current_module, when_block->condition, "When block condition is not a literal");
        }

        return;
    }
}

std::shared_ptr<Type>
TypeChecker::inferExpression(std::shared_ptr<Expression> &expr,
                             const std::shared_ptr<Type> &expected) {
    if (!expr)
        throw TypeCheckError(current_module, expr, "Cannot infer type of null expression");
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
            if (va->name == "slice") {
                auto so = expandSlice(call);
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
    if (auto te = std::dynamic_pointer_cast<TypeExpression>(expr)) {
        te->inferred_type = resolveType(te, te->type);
        return te->inferred_type;
    }
    if (auto ea = std::dynamic_pointer_cast<EnumAccess>(expr)) {
        std::string enum_name;
        auto va = std::dynamic_pointer_cast<VarAccess>(ea->enum_expr);
        std::shared_ptr<EnumType> res;
        if (!va) {
            auto mod = std::dynamic_pointer_cast<ModuleAccess>(ea->enum_expr);
            if (!mod) {
                throw TypeCheckError(current_module, ea, "Enum access base is not allowed: " + ea->enum_expr->str());
            }
            // Get the enum from the other module
            auto mod_type = inferModuleAccess(mod);
            auto enum_type = std::dynamic_pointer_cast<EnumType>(mod_type);
            if (!enum_type) {
                throw TypeCheckError(current_module, ea, "Enum access base is not an enum: " + ea->enum_expr->str());
            }
            enum_name = enum_type->name;
            res = enum_type;
        } else {
            enum_name = va->name;
            auto enum_type = lookupEnumType(enum_name);
            if (!enum_type) {
                throw TypeCheckError(current_module, ea, "Unknown enum: " + enum_name);
            }
            res = std::dynamic_pointer_cast<EnumType>(resolveType(ea, enum_type));
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
    throw TypeCheckError(current_module, expr, "Type inference: unhandled expression type: " + expr->str());
}

std::shared_ptr<Module> TypeChecker::resolveModulePath(std::shared_ptr<ASTNode> node, const std::vector<std::string> &path) {
    std::shared_ptr<Module> current = current_module;
    for (const auto &part : path) {
        auto it = current->imports.find(part);
        if (it == current->imports.end()) {
            throw TypeCheckError(current_module, node, "Module '" + part + "' not found in imports of module '" + current->canon_name + "'");
        }
        current = it->second;
    }
    return current;
}

std::shared_ptr<Type> TypeChecker::inferModuleAccess(const std::shared_ptr<ModuleAccess> &ma) {
    // if (ma->module_path.size() == 0) {
    //     if (ma->member_name.empty()) {
    //         throw TypeCheckError(current_module, ma, "Invalid module access with empty module path and member name");
    //     }
    //     if (ma->member_name == "") {
    //         throw TypeCheckError(current_module, ma, "Invalid module access with empty member name");
    //     }
    //     if (lookupSymbolInScope(ma->member_name) == std::nullopt) {
    //         throw TypeCheckError(current_module, ma, "Unknown symbol in imported module: " + ma->member_name);
    //     }
    //     auto og_mod = current_module;
    //     current_module = mod;
    //     auto t = inferVarAccess(std::make_shared<VarAccess>(ma->member_name));
    //     current_module = og_mod;
    //     return t;
    // }
    //
    auto mod = resolveModulePath(ma, ma->module_path);
    if (ma->member_name.empty()) {
        throw TypeCheckError(current_module, ma, "Invalid module access with empty member name");
    }
    auto it = mod->exports.find(ma->member_name);

    if (it == mod->exports.end()) {
        throw TypeCheckError(current_module, ma, "Unknown symbol in imported module: " + ma->member_name);
    }
    if (mod->typechecked == false) {
        throw TypeCheckError(current_module, ma, "Module '" + mod->canon_name + "' has not been type-checked yet");
    }
    auto exported = it->second;
    // auto og_mod = current_module;
    // current_module = mod;
    // auto va = std::make_shared<VarAccess>(ma->member_name);
    // va->line = ma->line;
    // va->col = ma->col;
    // std::shared_ptr<Type> t;

    auto sym_id = m_mod_scopes[mod->id].front().find(ma->member_name);

    if (sym_id == INVALID_SYMBOL_ID) {
        throw TypeCheckError(current_module, ma, "Symbol '" + ma->member_name + "' not found in module '" + mod->canon_name + "'");
    }

    auto s = symbol_table.lookupSymbol(sym_id);

    if (!s) {
        // symbol_table.dump();
        throw TypeCheckError(current_module, ma, "Symbol " + ma->member_name + " not found in module '" + mod->name + "'");
    }
    Symbol sym = *s;

    ma->symbol_id = sym_id;
    ma->inferred_type = sym.type;

    return sym.type;
}

std::shared_ptr<Type>
TypeChecker::inferTypeCast(const std::shared_ptr<TypeCast> &tc) {
    if (tc->cast_type == CastType::Reinterperet) {
        // allow any reinterpreted cast
        inferExpression(tc->expr);
        tc->inferred_type = resolveType(tc, tc->target_type);
        return tc->target_type;
    }
    auto ot = inferExpression(tc->expr);
    if (!ot)
        throw TypeCheckError(current_module, tc, "Failed to infer type of expression in type cast");

    if (canExplicitCast(ot, tc->target_type)) {
        tc->inferred_type = resolveType(tc, tc->target_type);
        return tc->target_type;
    }
    throw TypeCheckError(current_module, tc, "Invalid type cast from " + typeName(ot) + " to " + tc->target_type->str());
}

std::shared_ptr<Type> TypeChecker::inferVarAccess(std::shared_ptr<VarAccess> &v) {
    auto maybe = lookupSymbolInScope(v->name);
    if (maybe == std::nullopt) {
        ensureGlobalVariableVisible(v->name);
        maybe = lookupSymbolInScope(v->name);
    }
    // if (maybe == std::nullopt) {
    //     maybe = symbol_table.lookupSymbolId(current_module->id, v->name);
    // }
    if (maybe == std::nullopt || maybe == INVALID_SYMBOL_ID) {
        throw TypeCheckError(current_module, v, "Unknown variable: " + v->name);
    }
    auto entry = symbolTable().get(*maybe);
    if (entry && !entry->type) {
        throw TypeCheckError(current_module, v, "Variable has no resolvable type: " + v->name + " (could it be a template?)");
    }
    if (!entry) {
        throw TypeCheckError(current_module, v, "Variable not found in symbol table: " + v->name);
    }
    v->symbol_id = *maybe;
    v->inferred_type = resolveType(v, entry ? entry->type : nullptr);
    return v->inferred_type;
}

std::shared_ptr<Type> TypeChecker::inferLiteral(const std::shared_ptr<Literal> &lit,
                                                const std::shared_ptr<Type> &expected) {
    auto preType = resolveType(lit, lit->lit_type);
    if (!preType) {
        throw TypeCheckError(current_module, lit, "Unknown literal type");
    }

    if (expected && expected->isNumeric() && lit->lit_type->isNumeric()) {
        if (canImplicitCast(preType, expected)) {
            auto casted = m_const_eval.castLiteral(lit, expected);
            if (m_const_eval.ok() && casted) {
                lit->value = casted->value;
                lit->lit_type = expected;
                lit->inferred_type = expected;
                return expected;
            }
        }
    }

    lit->inferred_type = preType;
    return lit->lit_type;
}

std::shared_ptr<Type> TypeChecker::inferArrayLiteral(const std::shared_ptr<ArrayLiteral> &al,
                                                     const std::shared_ptr<Type> &expected) {
    auto expected_arr = std::dynamic_pointer_cast<ArrayType>(expected);
    std::shared_ptr<Type> expected_elem = nullptr;
    int64_t final_len = -1;

    if (expected_arr) {
        expected_elem = expected_arr->element_type;
        if (expected_arr->unsized) {
            final_len = static_cast<int64_t>(al->elements.size());
        } else {
            m_const_eval.clearErrors();
            auto evaluated = m_const_eval.evaluateExpression(expected_arr->length_expr);
            if (!m_const_eval.ok()) {
                for (const auto &e : m_const_eval.errors()) {
                    throw TypeCheckError(current_module, e.first, e.second);
                }
            }
            if (!evaluated) {
                throw TypeCheckError(current_module, al, "Failed to evaluate expected array length");
            }

            auto len_lit = std::dynamic_pointer_cast<Literal>(*evaluated);
            if (!len_lit) {
                throw TypeCheckError(current_module, al, "Expected array length was not a constant literal");
            }

            if (auto v = std::get_if<int64_t>(&len_lit->value)) {
                final_len = *v;
            } else if (auto v = std::get_if<uint64_t>(&len_lit->value)) {
                final_len = static_cast<int64_t>(*v);
            }
            if (final_len < 0) {
                throw TypeCheckError(current_module, al, "Array length cannot be negative");
            }
        }
    }

    if (final_len < 0 && !al->elements.empty()) {
        final_len = static_cast<int64_t>(al->elements.size());
    }

    if (al->elements.empty() && !expected_arr) {
        throw TypeCheckError(current_module, al, "Cannot infer type of empty array literal");
    }

    if (!expected_arr && al->elements.empty()) {
        throw TypeCheckError(current_module, al, "Cannot infer type of empty array literal");
    }

    if (expected_arr && static_cast<int64_t>(al->elements.size()) > final_len) {
        throw TypeCheckError(current_module, al,
                             "Array literal length mismatch: expected at most " +
                                 std::to_string(final_len) + ", got " +
                                 std::to_string(al->elements.size()));
    }

    if (expected_arr && expected_elem) {
        for (size_t i = 0; i < al->elements.size(); ++i) {
            auto t = inferExpression(al->elements[i], expected_elem);
            if (!t) {
                throw TypeCheckError(current_module, al, "Failed to infer type of array literal element");
            }
            if (!t->equals(expected_elem)) {
                if (canImplicitCast(t, expected_elem)) {
                    al->elements[i] = std::make_shared<TypeCast>(al->elements[i], expected_elem, CastType::Normal);
                } else {
                    throw TypeCheckError(current_module, al, "Array literal element type mismatch: " + typeName(expected_elem) + " expected, got " + typeName(t));
                }
            }
        }

        al->defined_len = static_cast<size_t>(final_len);
        auto resolved = resolveType(al, expected_arr);
        al->inferred_type = resolved;
        return resolved;
    }

    if (al->elements.empty()) {
        if (!expected_arr) {
            throw TypeCheckError(current_module, al, "Cannot infer type of empty array literal");
        }

        al->defined_len = static_cast<size_t>(final_len);
        auto resolved = resolveType(al, expected_arr);
        al->inferred_type = resolved;
        return resolved;
    }

    std::shared_ptr<Type> elem_type = nullptr;
    for (int i = 0; i < al->elements.size(); ++i) {
        auto el = al->elements[i];
        // Pass expected element type to each element
        auto t = inferExpression(el, expected_elem ? expected_elem : elem_type);
        if (!t)
            throw TypeCheckError(current_module, al, "Failed to infer type of array literal element");

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
                    throw TypeCheckError(current_module, al, "Array literal element type mismatch: " + typeName(elem_type) + " vs " + typeName(t));
                }
            }
        }
    }
    auto i64 = std::make_shared<I64>();
    auto arrSizeLit = std::make_shared<Literal>(static_cast<int64_t>(al->elements.size()), i64);
    arrSizeLit->inferred_type = i64;
    auto at = std::make_shared<ArrayType>(elem_type, arrSizeLit, false);
    al->inferred_type = resolveType(al, at);
    return at;
}

std::shared_ptr<Type>
TypeChecker::inferBinaryOp(const std::shared_ptr<BinaryOperation> &bin,
                           const std::shared_ptr<Type> &expected) {
    auto lt = resolveType(bin, inferExpression(bin->left));
    auto rt = resolveType(bin, inferExpression(bin->right));
    if (!lt || !rt)
        throw TypeCheckError(current_module, bin, "Failed to infer types of binary operation operands");

    // str concatenation
    if (bin->op == "+" && lt->kind() == TypeKind::Str && rt->kind() == TypeKind::Str) {
        auto s = std::make_shared<StringType>();
        bin->inferred_type = s;
        return s;
    }

    // str comparison
    if ((bin->op == "==" || bin->op == "!=") && lt->kind() == TypeKind::Str && rt->kind() == TypeKind::Str) {
        auto b = std::make_shared<Boolean>();
        bin->inferred_type = b;
        return b;
    }

    // simple numeric ops
    if (bin->op == "+" || bin->op == "-" || bin->op == "*" || bin->op == "/" || bin->op == "%" ||
        bin->op == "|" || bin->op == "&" || bin->op == "^" || bin->op == "<<" ||
        bin->op == ">>") {

        if (!lt->isGeneralNumeric() || !rt->isGeneralNumeric()) { // Allow pointers here
            throw TypeCheckError(current_module, bin, "Arithmetic operators require numeric operands: got " + typeName(lt) + " " + bin->op + " " + typeName(rt));
        }
        if (!lt->equals(rt)) {
            // If one is pointer and the other is integer, allow it (pointer arithmetic)
            if ((lt->kind() == TypeKind::Pointer && rt->isInteger()) || (lt->isInteger() && rt->kind() == TypeKind::Pointer)) {
                // result type is pointer if one of them is pointer, otherwise integer
                auto result_type = (lt->kind() == TypeKind::Pointer) ? lt : rt;
                bin->inferred_type = resolveType(bin, result_type);
                return result_type;
            } else if (canImplicitCast(rt, lt)) {
                bin->right = std::make_shared<TypeCast>(bin->right, lt, CastType::Normal);
            } else if (canImplicitCast(lt, rt)) {
                bin->left = std::make_shared<TypeCast>(bin->left, rt, CastType::Normal);
                lt = rt;
            } else {
                throw TypeCheckError(current_module, bin, "Arithmetic operator type mismatch: " + typeName(lt) + " " + bin->op + " " + typeName(rt));
            }
        }
        bin->inferred_type = resolveType(bin->left, lt);
        return lt;
    }

    // BOOL -> BOOL
    if (bin->op == "&&" || bin->op == "||") {
        if (!dynamic_cast<Boolean *>(lt.get()) ||
            !dynamic_cast<Boolean *>(rt.get())) {
            throw TypeCheckError(current_module, bin, "Logical operators require boolean operands: got " + typeName(lt) + " " + bin->op + " " + typeName(rt));
        }
        auto b = std::make_shared<Boolean>();
        bin->inferred_type = resolveType(bin, b);
        return b;
    }

    // comparisons -> BOOL
    if (bin->op == "==" || bin->op == "!=" || bin->op == "<" || bin->op == ">" ||
        bin->op == "<=" || bin->op == ">=") {
        if (!lt->equals(rt) && !canImplicitCast(rt, lt) &&
            !canImplicitCast(lt, rt)) {
            throw TypeCheckError(current_module, bin, "Comparison operands must have same type: " + typeName(lt) + " vs " + typeName(rt));
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
        bin->inferred_type = resolveType(bin, b);
        return b;
    }

    if (bin->op == "=") {
        // assignment: left must be lvalue (VarAccess, FieldAccess, OffsetAccess)
        if (!(std::dynamic_pointer_cast<VarAccess>(bin->left) ||
              std::dynamic_pointer_cast<FieldAccess>(bin->left) ||
              std::dynamic_pointer_cast<OffsetAccess>(bin->left) ||
              std::dynamic_pointer_cast<Dereference>(bin->left) ||
              std::dynamic_pointer_cast<ModuleAccess>(bin->left))) {
            throw TypeCheckError(current_module, bin->left,
                                 "Left operand of assignment is not an lvalue: " + bin->left->str());
        }
        if (lt->is_const) {
            throw TypeCheckError(current_module, bin->left, "Cannot assign to const value");
        }
        if (auto deref = std::dynamic_pointer_cast<Dereference>(bin->left)) {
            // make sure we're not assigning to a const pointer
            auto pt = inferExpression(deref->pointer);
            if (!pt)
                throw TypeCheckError(current_module, deref->pointer,
                                     "Failed to infer type of pointer in dereference");
            auto ptype = std::dynamic_pointer_cast<PointerType>(pt);
            if (!ptype) {
                throw TypeCheckError(current_module, deref->pointer,
                                     "Dereference of non-pointer type: " + typeName(pt));
            }
            if (ptype->pointer_const) {
                throw TypeCheckError(current_module, deref->pointer, "Cannot modify value through const pointer");
            }
        }
        if (!lt->equals(rt)) {
            if (canImplicitCast(rt, lt)) {
                bin->right = std::make_shared<TypeCast>(bin->right, lt, CastType::Normal);
                bin->inferred_type = resolveType(bin, lt);
                return lt;
            }
            throw TypeCheckError(current_module, bin,
                                 "Assignment type mismatch: " + typeName(lt) + " = " + typeName(rt));
        }
        bin->inferred_type = resolveType(bin->left, lt);
        return lt;
    }

    throw TypeCheckError(current_module, bin, "Unhandled binary operator: " + bin->op);
}

std::shared_ptr<Type>
TypeChecker::inferUnaryOp(const std::shared_ptr<UnaryOperation> &un) {
    auto ot = inferExpression(un->operand);
    if (!ot)
        throw TypeCheckError(current_module, un, "Failed to infer type of unary operation operand");

    if (un->op == "-" || un->op == "+" || un->op == "~") {
        // numeric
        un->inferred_type = resolveType(un, ot);
        return ot;
    }
    if (un->op == "!") {
        if (!dynamic_cast<Boolean *>(ot.get())) {
            throw TypeCheckError(current_module, un, "Logical not expects boolean operand, got " + typeName(ot));
        }
        un->inferred_type = resolveType(un, ot);
        return ot;
    }
    if (un->op == "&") {
        // address-of: result is pointer to operand type
        auto pt = std::make_shared<PointerType>(ot);
        un->inferred_type = resolveType(un, pt);
        return pt;
    }
    if (un->op == "++") {
        if (!ot->isGeneralNumeric()) {
            throw TypeCheckError(current_module, un, "Increment operator expects numeric operand, got " + typeName(ot));
        }
        un->inferred_type = resolveType(un, ot);
        return ot;
    }
    if (un->op == "--") {
        if (!ot->isGeneralNumeric()) {
            throw TypeCheckError(current_module, un, "Decrement operator expects numeric operand, got " + typeName(ot));
        }
        un->inferred_type = resolveType(un, ot);
        return ot;
    }
    throw TypeCheckError(current_module, un, "Unhandled unary operator: " + un->op);
}

std::shared_ptr<Type>
TypeChecker::inferDereference(const std::shared_ptr<Dereference> &d) {
    auto ot = inferExpression(d->pointer);
    if (!ot)
        throw TypeCheckError(current_module, d, "Failed to infer type of pointer in dereference");

    auto pt = std::dynamic_pointer_cast<PointerType>(ot);
    if (!pt) {
        throw TypeCheckError(current_module, d, "Dereference of non-pointer type: " + typeName(ot));
    }
    d->inferred_type = resolveType(d, pt->base);
    return pt->base;
}

std::shared_ptr<Type>
TypeChecker::inferMethodCall(const std::shared_ptr<MethodCall> &mc) {
    auto bt = inferExpression(mc->object);
    if (!bt)
        throw TypeCheckError(current_module, mc, "Failed to infer type of method call object");
    auto st = std::dynamic_pointer_cast<StructType>(bt);
    if (!st) {
        auto pt = std::dynamic_pointer_cast<PointerType>(bt);
        if (pt)
            st = std::dynamic_pointer_cast<StructType>(resolveType(mc, pt->base));

        if (!pt) {
            throw TypeCheckError(current_module, mc, "Method call on non-struct type: " + typeName(bt));
        }
    }
    st = std::dynamic_pointer_cast<StructType>(resolveType(mc, st));
    auto it = st->methods.find(mc->method);
    if (it == st->methods.end()) {
        std::cout << "Available methods on " << typeName(st) << ": ";
        for (const auto &m : st->methods) {
            std::cout << m.first << " ";
        }
        std::cout << std::endl;
        throw TypeCheckError(current_module, mc, "Struct " + st->name + " has no method " + mc->method);
    }
    auto ftype = std::dynamic_pointer_cast<FunctionType>(resolveType(mc, it->second->type));
    // check arity (naive, no implicit conversions)
    if (!ftype->variadic &&
        mc->args.size() !=
            (ftype->params.size() - 1)) {
        throw TypeCheckError(current_module, mc, "Method call argument count mismatch: expected " + std::to_string(ftype->params.size() - 1) + " got " + std::to_string(mc->args.size()));
    }
    size_t n = ftype->params.size();
    for (size_t i = 0; i < mc->args.size(); ++i) {
        auto at = inferExpression(mc->args[i]);

        if (!at)
            throw TypeCheckError(current_module, mc, "Failed to infer type of method call argument " + std::to_string(i));
        if (i + 1 < ftype->params.size() && !at->equals(ftype->params[i + 1])) {
            if (canImplicitCast(at, ftype->params[i + 1])) {
                mc->args[i] = std::make_shared<TypeCast>(mc->args[i], ftype->params[i + 1], CastType::Normal);
            } else
                throw TypeCheckError(current_module, mc, "Method call argument " + std::to_string(i) + " type mismatch: expected " + typeName(ftype->params[i + 1]) + " got " + typeName(at));
        }
    }
    mc->inferred_type = resolveType(mc, ftype->ret);
    return ftype->ret;
}

std::shared_ptr<Type> TypeChecker::tryInferGenericFunctionCall(const std::shared_ptr<FuncCall> &fc, const std::shared_ptr<FunctionType> &ft) {
    // assumes ft is a generic function type
    // 1. infer argument types
    // 2. match against generic parameters to build associations between Generic -> concrete
    // 3. convert to template instantiation and infer that instead

    std::vector<std::shared_ptr<Type>> arg_types;
    for (auto &arg : fc->args) {
        auto at = inferExpression(arg);
        if (!at)
            throw TypeCheckError(current_module, arg, "Failed to infer type of function call argument");
        arg_types.push_back(at);
    }
    // build generic associations
    std::unordered_map<std::string, std::shared_ptr<Type>> generic_associations;
    for (size_t i = 0; i < ft->params.size(); ++i) {
        auto pt = ft->params[i];
        if (auto gt = std::dynamic_pointer_cast<GenericType>(pt)) {
            // if we already have an association for this generic, check it matches
            auto it = generic_associations.find(gt->name);
            if (it != generic_associations.end()) {
                if (!arg_types[i]->equals(it->second))
                    throw TypeCheckError(current_module, fc, "Generic type association mismatch for " + gt->name + ": expected " + typeName(it->second) + " got " + typeName(arg_types[i]));
            } else {
                generic_associations[gt->name] = arg_types[i];
            }
        } else if (!pt->equals(arg_types[i])) {
            if (canImplicitCast(arg_types[i], pt)) {
                fc->args[i] = std::make_shared<TypeCast>(fc->args[i], pt, CastType::Normal);
            } else
                throw TypeCheckError(current_module, fc, "Function call argument " + std::to_string(i) + " type mismatch: expected " + typeName(pt) + " got " + typeName(arg_types[i]));
        }
    }
    // HACk for a seconds
    std::vector<std::shared_ptr<Type>> template_args;
    for (auto &ga : generic_associations) {
        template_args.push_back(ga.second);
    }
    auto prevFunc = fc->func;
    // *fc->func = std::make_shared<TemplateInstantiation>(prevFunc->name, template_args);
    return nullptr; // TODO: implement template instantiation and inference
}

std::shared_ptr<Type> TypeChecker::inferFuncCall(const std::shared_ptr<FuncCall> &call,
                                                 const std::shared_ptr<Type> &expected) {
    // infer function expression type
    std::shared_ptr<FunctionType> ftype = nullptr;

    auto ft = inferExpression(call->func);
    if (!ft)
        throw TypeCheckError(current_module, call, "Failed to infer type of function in function call");
    ftype = std::dynamic_pointer_cast<FunctionType>(ft);
    if (!ftype) {
        if (auto pt = std::dynamic_pointer_cast<PointerType>(ft)) {
            ftype = std::dynamic_pointer_cast<FunctionType>(resolveType(call, pt->base));

            if (!ftype) {
                throw TypeCheckError(current_module, call,
                                     "Attempted to call non-function pointer type: " + typeName(ft));
            }
        } else {
            throw TypeCheckError(current_module, call, "Attempted to call non-function type: " + typeName(ft));
        }
    }

    if (containsGenericType(ftype)) {
        tryInferGenericFunctionCall(call, ftype); // Destructive, will modify call in-place if successful
    }

    // check arity (naive, no implicit conversions)
    if (!ftype->variadic &&
        call->args.size() !=
            (ftype->params.size())) { // if method, first param is 'self'
        throw TypeCheckError(current_module, call, "Function call argument count mismatch: expected " + std::to_string(ftype->params.size()) + " got " + std::to_string(call->args.size()));
    }

    size_t n = ftype->params.size();
    for (size_t i = 0; i < call->args.size(); ++i) {
        // Pass expected parameter type to argument inference
        std::shared_ptr<Type> expected_arg_type = (i < ftype->params.size()) ? ftype->params[i] : nullptr;
        auto at = inferExpression(call->args[i], expected_arg_type);
        if (!at)
            throw TypeCheckError(current_module, call, "Failed to infer type of function call argument " + std::to_string(i));

        if (i < ftype->params.size() && !at->equals(ftype->params[i])) {
            if (canImplicitCast(at, ftype->params[i])) {
                call->args[i] = std::make_shared<TypeCast>(call->args[i], ftype->params[i], CastType::Normal);
            } else
                throw TypeCheckError(current_module, call, "Function call argument " + std::to_string(i) + " type mismatch: expected " + typeName(ftype->params[i]) + " got " + typeName(at));
        }
    }
    call->func->inferred_type = resolveType(call->func, ft);
    call->inferred_type = resolveType(call, ftype->ret);
    return ftype->ret;
}

std::shared_ptr<Type> TypeChecker::inferErrorUnionFieldAccess(const std::shared_ptr<FieldAccess> &fa) {
    auto expr = std::dynamic_pointer_cast<Expression>(fa->base);
    auto base = resolveType(expr, inferExpression(expr));
    if (!base) {
        throw TypeCheckError(current_module, fa, "Failed to infer type of error union field access base");
    }
    auto eut = std::dynamic_pointer_cast<ErrorUnionType>(base);
    if (!eut) {
        throw TypeCheckError(current_module, fa, "Error union field access on non-error-union type: " + typeName(base));
    }
    if (fa->field == "err") {
        fa->inferred_type = resolveType(fa, eut->errorType);
        return eut->errorType;
    }
    if (fa->field == "ok") {
        fa->inferred_type = resolveType(fa, eut->valueType);
        return eut->valueType;
    }
    if (fa->field == "is_err") {
        fa->inferred_type = std::make_shared<Boolean>();
        return fa->inferred_type;
    }
    throw TypeCheckError(current_module, fa, "Error union has no field " + fa->field);
}

std::shared_ptr<Type> TypeChecker::inferArrayFieldAccess(const std::shared_ptr<FieldAccess> &fa) {
    auto expr = std::dynamic_pointer_cast<Expression>(fa->base);
    auto base = inferExpression(expr);
    if (!base) {
        throw TypeCheckError(current_module, fa, "Failed to infer type of error union field access base");
    }
    auto arrty = std::dynamic_pointer_cast<ArrayType>(base);
    if (!arrty) {
        throw TypeCheckError(current_module, fa, "Error union field access on non-error-union type: " + typeName(base));
    }
    if (fa->field == "ptr") {
        fa->inferred_type = std::make_shared<PointerType>(resolveType(fa, arrty->element_type));
        return fa->inferred_type;
    }
    if (fa->field == "len") {
        fa->inferred_type = std::make_shared<I64>();
        return fa->inferred_type;
    }
    throw TypeCheckError(current_module, fa, "Array has no field " + fa->field);
}

std::shared_ptr<Type>
TypeChecker::inferFieldAccess(const std::shared_ptr<FieldAccess> &fa) {
    if (auto baseExpr = std::dynamic_pointer_cast<Expression>(fa->base)) {
        auto bt = resolveType(fa, inferExpression(baseExpr));
        if (!bt)
            throw TypeCheckError(current_module, fa, "Failed to infer type of field access base");
        auto st = std::dynamic_pointer_cast<StructType>(bt);
        if (st) {
            auto ft = st->getFieldType(fa->field);
            if (!ft) {
                throw TypeCheckError(current_module, fa, fa->str() + " has no field " + fa->field);
            }
            fa->inferred_type = resolveType(fa, ft);
            return ft;
        }
        auto pt = std::dynamic_pointer_cast<PointerType>(bt);
        if (pt) {
            st = std::dynamic_pointer_cast<StructType>(resolveType(fa, pt->base));
            if (!st) {
                throw TypeCheckError(current_module, fa, "Field access on pointer to non-struct type: " + typeName(pt->base));
            }
            auto ft = st->getFieldType(fa->field);
            if (!ft) {
                throw TypeCheckError(current_module, fa, fa->str() + " has no field " + fa->field);
            }
            fa->inferred_type = resolveType(fa, ft);
            return ft;
        }
        auto eut = std::dynamic_pointer_cast<ErrorUnionType>(bt);
        if (eut) {
            return inferErrorUnionFieldAccess(fa);
        }
        auto arrt = std::dynamic_pointer_cast<ArrayType>(bt);
        if (arrt) {
            return inferArrayFieldAccess(fa);
        }
        if (bt->kind() == TypeKind::Str) {
            if (fa->field == "len") {
                auto t = std::make_shared<USize>();
                fa->inferred_type = t;
                return t;
            }
            if (fa->field == "ptr") {
                auto t = std::make_shared<PointerType>(std::make_shared<U8>(), true);
                fa->inferred_type = t;
                return t;
            }
            throw TypeCheckError(current_module, fa, "str has no field '" + fa->field + "' (available: len, ptr)");
        }
        auto ut = std::dynamic_pointer_cast<UnionType>(bt);
        if (ut) {
            // field access on union type: get the field from the union
            auto ft = ut->getFieldType(fa->field);
            if (!ft) {
                throw TypeCheckError(current_module, fa, "Union has no field " + fa->field);
            }
            fa->inferred_type = resolveType(fa, ft);
            return ft;
        }
        auto mt = std::dynamic_pointer_cast<ModuleType>(bt);
        if (mt) {
            auto maybe_id = m_mod_scopes[mt->module_id].front().find(fa->field);
            // auto maybe_id = symbol_table.lookupModule(mt->module_id)->second->symbols.find(fa->field);
            if (maybe_id == INVALID_SYMBOL_ID) {
                throw TypeCheckError(current_module, fa, "Unknown symbol in module " + mt->name + ": " + fa->field);
            }
            auto id = maybe_id;
            auto s = symbol_table.lookupModule(mt->module_id)->second->symbols.get(id);
            if (!s) {
                throw TypeCheckError(current_module, fa, "Failed to lookup symbol in module " + mt->name + ": " + fa->field);
            }
            Symbol sym = *s;
            fa->inferred_type = resolveType(fa, sym.type);
            return sym.type;
        }
        throw TypeCheckError(current_module, fa, "Field access on disallowed type: " + typeName(bt));
    }
    throw TypeCheckError(current_module, fa->base, "FieldAccess base is not an expression: " + fa->base->str());
}

std::shared_ptr<Type>
TypeChecker::inferOffsetAccess(const std::shared_ptr<OffsetAccess> &oa) {
    // base could be expression or ASTNode
    if (auto baseExpr = std::dynamic_pointer_cast<Expression>(oa->base)) {
        auto bt = inferExpression(baseExpr);
        inferExpression(oa->index);
        if (!bt)
            throw TypeCheckError(current_module, oa, "Failed to infer type of offset access base");
        if (auto at = std::dynamic_pointer_cast<ArrayType>(bt)) {
            auto idxt = inferExpression(oa->index);
            if (!idxt)
                throw TypeCheckError(current_module, oa, "Failed to infer type of offset access index");
            oa->inferred_type = resolveType(oa, at->element_type);

            return at->element_type;
        }
        if (auto pt = std::dynamic_pointer_cast<PointerType>(bt)) {
            // pointer indexing returns base
            auto bt = resolveType(oa, pt->base);
            oa->inferred_type = bt;
            return bt;
        }
        if (bt->kind() == TypeKind::Str) {
            auto u8t = std::make_shared<U8>();
            oa->inferred_type = u8t;
            return u8t;
        }
        throw TypeCheckError(current_module, oa, "Offset access on non-array/pointer type: " + typeName(bt));
    } else {
        throw TypeCheckError(current_module, oa->base, "OffsetAccess base is not an expression: " + oa->base->str());
    }
}

std::shared_ptr<Type>
TypeChecker::inferStructInit(const std::shared_ptr<StructInitializer> &init,
                             const std::shared_ptr<Type> &expected) {
    std::shared_ptr<StructType> st;
    std::shared_ptr<ArrayType> at;
    std::shared_ptr<Type> _st;
    if (auto mod_acc = std::dynamic_pointer_cast<ModuleAccess>(init->struct_type_expr)) {
        auto stype = inferModuleAccess(mod_acc);
        st = std::dynamic_pointer_cast<StructType>(stype);
        at = std::dynamic_pointer_cast<ArrayType>(stype);
    } else {
        _st = inferExpression(init->struct_type_expr);
        st = std::dynamic_pointer_cast<StructType>(_st);
        at = std::dynamic_pointer_cast<ArrayType>(_st);
    }
    if (!st && !at) {
        throw TypeCheckError(current_module, init, "Struct initializer with non-struct type: " + typeName(_st));
    }

    if (at) {
        auto ptr_field_type = std::make_shared<PointerType>(resolveType(init, at->element_type));
        auto len_field_type = std::make_shared<USize>();

        auto ptr_it = init->field_values.find("ptr");
        auto len_it = init->field_values.find("len");
        if (ptr_it == init->field_values.end() || len_it == init->field_values.end() ||
            init->field_values.size() != 2) {
            throw TypeCheckError(current_module, init, "Array initializer expects fields ptr and len");
        }

        bool all_are_const = true;
        for (auto &p : init->field_values) {
            auto res = m_const_eval.evaluateExpression(p.second);
            if (!m_const_eval.ok() || !res) {
                all_are_const = false;
                m_const_eval.clearErrors();
            }

            auto fieldTypeRaw = p.first == "ptr" ? std::dynamic_pointer_cast<Type>(ptr_field_type) : std::dynamic_pointer_cast<Type>(len_field_type);
            auto fieldType = resolveType(p.second, fieldTypeRaw);
            auto typeActual = inferExpression(p.second, fieldType);
            if (!typeActual) {
                throw TypeCheckError(current_module, init, "Failed to infer type of array field " + p.first + " initializer");
            }
            if (!typeActual->equals(fieldType)) {
                if (canImplicitCast(typeActual, fieldType)) {
                    p.second = std::make_shared<TypeCast>(p.second, fieldType, CastType::Normal);
                } else {
                    throw TypeCheckError(current_module, init, "Array field " + p.first + " type mismatch: expected " + typeName(fieldType) + " got " + typeName(typeActual));
                }
            }
        }
        if (all_are_const) {
            init->constant_evaluated = true;
        }
        init->inferred_type = at;
        return at;
    }

    // Global variable initializers can be checked before struct declarations are finalized,
    // which leaves `st` as a placeholder with no fields. Hydrate it from the declaration so
    // field lookup and casts use the actual struct layout.
    if ((!st->complete || st->fields.empty()) && current_module && current_module->ast) {
        for (const auto &decl : current_module->ast->declarations) {
            auto sd = std::dynamic_pointer_cast<StructDeclaration>(decl);
            if (!sd || sd->name != st->name) {
                continue;
            }

            std::vector<std::pair<std::string, std::shared_ptr<Type>>> fields;
            fields.reserve(sd->fields.size());
            for (const auto &field : sd->fields) {
                auto resolved_field_type = resolveType(sd, field.second);
                fields.emplace_back(field.first, resolved_field_type);
            }

            st->fields = std::move(fields);
            st->methods = sd->methods;
            st->complete = true;
            break;
        }
    }

    bool all_are_const = true;
    for (auto &p : init->field_values) {
        auto res = m_const_eval.evaluateExpression(p.second);
        if (!m_const_eval.ok() || !res) {
            all_are_const = false;
            m_const_eval.clearErrors();
        }

        auto fieldTypeRaw = st->getFieldType(p.first);
        if (!fieldTypeRaw) {
            throw TypeCheckError(current_module, init,
                                 "Unknown field '" + p.first + "' in struct initializer for " +
                                     st->name);
        }

        auto fieldType = resolveType(p.second, fieldTypeRaw);
        auto typeActual = inferExpression(p.second, fieldType);
        if (!typeActual)
            throw TypeCheckError(current_module, init, "Failed to infer type of struct " + st->name + " field " + p.first + " initializer");
        if (!typeActual->equals(fieldType)) {
            if (canImplicitCast(typeActual, fieldType)) {
                p.second = std::make_shared<TypeCast>(p.second, fieldType, CastType::Normal);
            } else {
                throw TypeCheckError(current_module, init, "Struct " + st->name + " field " + p.first + " type mismatch: expected " + typeName(fieldType) + " got " + typeName(typeActual));
            }
        }
    }
    if (all_are_const) {
        init->constant_evaluated = true;
    }
    init->inferred_type = st; // HACK: Fix error when struct type is module access
    return st;
}

ASTNodePtr TypeChecker::lookupConstVariableInModulePath(const std::vector<std::string> &module_path, const std::string &var_name) {
    if (module_path.size() == 0) {
        // lookup in current module's global scope
        auto maybe = lookupSymbolInScope(var_name);
        if (current_module->exports.find(var_name) == current_module->exports.end()) {
            throw TypeCheckError(current_module, nullptr, "Variable " + var_name + " is not exported from module " + current_module->path);
        }
        if (!maybe) {
            throw TypeCheckError(current_module, nullptr, "Unknown variable in current module: " + var_name);
        }
        auto entry = symbolTable().get(*maybe);
        return entry ? entry->decl : nullptr;
    }

    auto mod = resolveModulePath(nullptr, module_path);
    if (!mod) {
        throw TypeCheckError(current_module, nullptr, "Unknown module: " + module_path.back());
    }
    auto maybe = mod->exports.find(var_name);
    if (maybe == mod->exports.end()) {
        throw TypeCheckError(current_module, nullptr, "Variable " + var_name + " is not exported from module " + mod->path);
    }
    auto decl = maybe->second;
    if (!decl) {
        throw TypeCheckError(current_module, nullptr, "Exported symbol " + var_name + " in module " + mod->path + " not found");
    }
    return decl;
}
