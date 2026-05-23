#include "const_eval.h"
#include "ast/ast.h"
#include "typechecking/typecheck.h"
#include <cstdint>
#include <memory>
#include <variant>

void ConstEvaluator::error(const ASTNodePtr &node, const std::string &msg) {
    m_errors.push_back(std::make_pair(node, msg));
}

static unsigned getIntegerBitWidth(const std::shared_ptr<Type> &t) {
    if (!t)
        return 0;
    switch (t->kind()) {
        case TypeKind::U8:
            return 8;
        case TypeKind::U32:
            return 32;
        case TypeKind::U64:
            return 64;
        case TypeKind::USize:
            return sizeof(size_t) * 8;
        case TypeKind::I32:
            return 32;
        case TypeKind::I64:
            return 64;
        default:
            return 0;
    }
}

static unsigned getFloatBitWidth(const std::shared_ptr<Type> &t) {
    if (!t)
        return 0;
    switch (t->kind()) {
        case TypeKind::F32:
            return 32;
        case TypeKind::F64:
            return 64;
        default:
            return 0;
    }
}


std::optional<ExprPtr> ConstEvaluator::evaluateExpression(const ExprPtr &expr) {
    if (!expr)
        return std::nullopt;

    if (auto lit = std::dynamic_pointer_cast<Literal>(expr)) {
        return lit;
    }

    if (auto uop = std::dynamic_pointer_cast<UnaryOperation>(expr)) {
        if (uop->op == "&") {
            if (std::dynamic_pointer_cast<VarAccess>(uop->operand) ||
                std::dynamic_pointer_cast<ModuleAccess>(uop->operand)) {
                return uop;
            }
            return std::nullopt;
        }

        auto val = evaluateExpression(uop->operand);
        if (!val)
            return std::nullopt;

        if (auto lit = std::dynamic_pointer_cast<Literal>(*val)) {
            ConstValue res;
            if (auto b = std::get_if<bool>(&lit->value)) {
                if (uop->op == "!")
                    res = !(*b);
                else
                    return std::nullopt;
            } else if (auto i = std::get_if<int64_t>(&lit->value)) {
                if (uop->op == "-")
                    res = -(*i);
                else
                    return std::nullopt;
            } else if (auto d = std::get_if<double>(&lit->value)) {
                if (uop->op == "-")
                    res = -(*d);
                else
                    return std::nullopt;
            } else
                return std::nullopt;
            return std::make_shared<Literal>(res, lit->lit_type);
        }
    }

    if (auto bop = std::dynamic_pointer_cast<BinaryOperation>(expr)) {
        auto left = evaluateExpression(bop->left);
        auto right = evaluateExpression(bop->right);
        if (!left || !right)
            return std::nullopt;

        auto lLit = std::dynamic_pointer_cast<Literal>(*left);
        auto rLit = std::dynamic_pointer_cast<Literal>(*right);
        if (lLit && rLit) {
            return evaluateBinaryLiterals(lLit, rLit, bop->op);
        }
    }

    // Replace the existing TypeCast branch with this implementation
    if (auto cast = std::dynamic_pointer_cast<TypeCast>(expr)) {
        auto valOpt = evaluateExpression(cast->expr);
        if (!valOpt)
            return std::nullopt;
        auto lit = std::dynamic_pointer_cast<Literal>(*valOpt);
        if (!lit)
            return std::nullopt;

        // Reinterpret cast: keep raw bits/value, just change type
        if (cast->cast_type == CastType::Reinterperet) {
            return std::make_shared<Literal>(lit->value, cast->target_type);
        }

        // Respect typechecker rules
        auto from_type = lit->lit_type;
        auto to_type = cast->target_type;
        if (!canExplicitCast(from_type, to_type)) {
            error(cast, "Invalid constant cast from " + from_type->str() + " to " +
                            to_type->str());
            return std::nullopt;
        }

        // Helpers
        auto srcIntBits = getIntegerBitWidth(from_type);
        auto dstIntBits = getIntegerBitWidth(to_type);
        auto srcFloatBits = getFloatBitWidth(from_type);
        auto dstFloatBits = getFloatBitWidth(to_type);

        // integer -> integer (trunc/extend)
        if (srcIntBits && dstIntBits) {
            // get source value as unsigned 64 for masking, and signed for sign handling
            uint64_t src_u = 0;
            int64_t src_s = 0;
            bool src_is_unsigned = from_type->isUnsigned();

            if (auto pi = std::get_if<int64_t>(&lit->value)) {
                src_s = *pi;
                src_u = static_cast<uint64_t>(static_cast<int64_t>(*pi));
            } else if (auto pu = std::get_if<uint64_t>(&lit->value)) {
                src_u = *pu;
                src_s = static_cast<int64_t>(*pu);
                // } else if (auto pu = std::get_if<uintptr_t>(&lit->value)) {
                //   src_u = static_cast<uint64_t>(*pu);
                //   src_s = static_cast<int64_t>(src_u);
            } else {
                return std::nullopt;
            }

            // extension
            if (dstIntBits > srcIntBits) {
                if (from_type->isUnsigned()) {
                    // zero-extend: result in uint64_t
                    uint64_t res_u = src_u;
                    // keep as unsigned if target is unsigned, else sign-convert
                    if (to_type->isUnsigned())
                        return std::make_shared<Literal>(static_cast<uint64_t>(res_u), to_type);
                    else
                        return std::make_shared<Literal>(static_cast<int64_t>(res_u), to_type);
                } else {
                    // sign-extend: keep signed value
                    int64_t res_s = src_s; // in C++ this is sign-extended naturally when bits increase
                    if (to_type->isUnsigned()) {
                        return std::make_shared<Literal>(static_cast<uint64_t>(res_s), to_type);
                    } else {
                        return std::make_shared<Literal>(static_cast<int64_t>(res_s), to_type);
                    }
                }
            }

            // truncation
            if (dstIntBits < srcIntBits) {
                // mask lower dstIntBits
                uint64_t mask = (dstIntBits >= 64) ? ~0ULL : ((1ULL << dstIntBits) - 1ULL);
                uint64_t truncated = src_u & mask;
                if (to_type->isUnsigned()) {
                    return std::make_shared<Literal>(static_cast<uint64_t>(truncated), to_type);
                } else {
                    // interpret truncated as signed of dstIntBits
                    uint64_t sign_bit = 1ULL << (dstIntBits - 1);
                    if (dstIntBits == 64) {
                        // fits full signed 64
                        return std::make_shared<Literal>(static_cast<int64_t>(truncated), to_type);
                    } else if (truncated & sign_bit) {
                        // negative after sign extension
                        uint64_t sign_ext = (~mask) | truncated;
                        return std::make_shared<Literal>(static_cast<int64_t>(sign_ext), to_type);
                    } else {
                        return std::make_shared<Literal>(static_cast<int64_t>(truncated), to_type);
                    }
                }
            }

            // same width, maybe change signedness
            if (dstIntBits == srcIntBits) {
                if (to_type->isUnsigned()) {
                    // convert to uint64_t
                    return std::make_shared<Literal>(static_cast<uint64_t>(src_u), to_type);
                } else {
                    return std::make_shared<Literal>(static_cast<int64_t>(src_s), to_type);
                }
            }
        }

        // --- float -> float (extend/trunc) ---
        if (srcFloatBits && dstFloatBits) {
            double src_d = 0.0;
            if (auto pd = std::get_if<double>(&lit->value))
                src_d = *pd;
            else if (auto pi = std::get_if<int64_t>(&lit->value))
                src_d = static_cast<double>(*pi);
            else if (auto pu = std::get_if<uint64_t>(&lit->value))
                src_d = static_cast<double>(*pu);
            else
                return std::nullopt;

            // dst precision: if dstFloatBits > srcFloatBits - promote (both stored as double in ConstValue)
            // storing as double is simplest -- precision beyond double not handled here
            return std::make_shared<Literal>(static_cast<double>(src_d), to_type);
        }

        // --- integer -> float ---
        if (srcIntBits && dstFloatBits) {
            double d = 0.0;
            if (auto pi = std::get_if<int64_t>(&lit->value)) {
                if (from_type->isUnsigned())
                    d = static_cast<double>(static_cast<uint64_t>(*pi));
                else
                    d = static_cast<double>(*pi);
            } else if (auto pu = std::get_if<uint64_t>(&lit->value)) {
                d = static_cast<double>(*pu);
                // } else if (auto pu = std::get_if<uintptr_t>(&lit->value)) {
                //   d = static_cast<double>(static_cast<uint64_t>(*pu));
            } else
                return std::nullopt;

            return std::make_shared<Literal>(d, to_type);
        }

        // --- float -> integer ---
        if (srcFloatBits && dstIntBits) {
            double src_d = 0.0;
            if (auto pd = std::get_if<double>(&lit->value))
                src_d = *pd;
            else
                return std::nullopt;

            // FP->int conversions truncate toward zero (match LLVM FPToSI / FPToUI semantics)
            if (to_type->isUnsigned()) {
                uint64_t u = static_cast<uint64_t>(src_d);
                return std::make_shared<Literal>(u, to_type);
            } else {
                int64_t s = static_cast<int64_t>(src_d);
                return std::make_shared<Literal>(s, to_type);
            }
        }

        // --- pointer <-> pointer or pointer <-> integer ---
        // We require that literal stores uintptr_t for pointer constants.
        if (from_type->kind() == TypeKind::Pointer && to_type->kind() == TypeKind::Pointer) {
            // bitcast: keep address bits as uintptr_t
            if (auto pval = std::get_if<uint64_t>(&lit->value)) {
                return std::make_shared<Literal>(static_cast<uint64_t>(*pval), to_type);
            } else {
                // if literal was integer, accept int->ptr reinterpret
                if (auto pu = std::get_if<uint64_t>(&lit->value)) {
                    return std::make_shared<Literal>(static_cast<uintptr_t>(*pu), to_type);
                }
                return std::nullopt;
            }
        }

        if (from_type->kind() == TypeKind::Pointer && dstIntBits) {
            if (auto pval = std::get_if<uint64_t>(&lit->value)) {
                // pointer -> int (ptrtoint)
                uint64_t u = static_cast<uint64_t>(*pval);
                if (to_type->isUnsigned())
                    return std::make_shared<Literal>(static_cast<uint64_t>(u), to_type);
                else
                    return std::make_shared<Literal>(static_cast<int64_t>(static_cast<int64_t>(u)), to_type);
            }
        }

        if (srcIntBits && to_type->kind() == TypeKind::Pointer) {
            // int -> pointer (inttoptr)
            uint64_t u = 0;
            if (auto pu = std::get_if<uint64_t>(&lit->value))
                u = *pu;
            else if (auto pi = std::get_if<int64_t>(&lit->value))
                u = static_cast<uint64_t>(*pi);
            else
                return std::nullopt;
            return std::make_shared<Literal>(static_cast<uintptr_t>(u), to_type);
        }

        // fallback: unsupported constant cast
        return std::nullopt;
    }

    if (auto va = std::dynamic_pointer_cast<VarAccess>(expr)) {
        auto it = m_const_vars.find(va->name);
        if (it != m_const_vars.end()) {
            return it->second;
        } else {
            if (m_intrinsics.find(va->name) != m_intrinsics.end()) {
                return m_intrinsics[va->name];
            }
            error(va, "Variable is not compile-time known: " + va->name);
            return std::nullopt;
        }
    }

    if (auto funcCall = std::dynamic_pointer_cast<FuncCall>(expr)) {
        error(funcCall, "Function calls are not allowed in constant expressions");
        return std::nullopt;
    }

    if (auto arrayLit = std::dynamic_pointer_cast<ArrayLiteral>(expr)) {
        std::cout << "Evaluating array literal with " << arrayLit->elements.size() << " elements\n";
        std::vector<ExprPtr> constElements;
        for (const auto &elem : arrayLit->elements) {
            std::optional<ExprPtr> valOpt = evaluateExpression(elem);
            if (!valOpt) {
                std::cout << "Failed to evaluate array element: " << elem->toString() << "\n";
                return std::nullopt;
            }
            auto constExpr = std::dynamic_pointer_cast<Expression>(*valOpt);
            if (!constExpr) {
                std::cout << "Array element is not a constant expression: " << elem->toString() << "\n";
                error(elem, "Array element is not a constant expression");
                return std::nullopt;
            }
            constExpr->constant_evaluated = true;
            constElements.push_back(constExpr);
        }
        std::cout << "Successfully evaluated array literal with " << constElements.size() << " constant elements\n";
        auto evaluated = std::make_shared<ArrayLiteral>(constElements);
        evaluated->constant_evaluated = true;
        return evaluated;
    }

    if (auto moduleAccess = std::dynamic_pointer_cast<ModuleAccess>(expr)) {
        // Search through const vars with module prefix
        auto path = moduleAccess->module_path;
        return std::dynamic_pointer_cast<Expression>(m_type_checker->lookupConstVariableInModulePath(path, moduleAccess->member_name));
    }

    return std::nullopt;
}

std::optional<std::shared_ptr<VariableDeclaration>> ConstEvaluator::evaluateVariableDeclaration(const std::shared_ptr<VariableDeclaration> &var) {
    if (!var->is_const || !var->initializer)
        return var; // Not a const variable or no initializer

    auto expr = std::dynamic_pointer_cast<Expression>(var->initializer);
    if (!expr) {
        error(var, "Const variable initializer is not an expression");
        return std::nullopt;
    }

    auto val = evaluateExpression(expr);
    if (!val)
        return std::nullopt;

    m_const_vars[var->name] = val.value();
    var->initializer = val.value();
    var->initializer->inferred_type = var->var_type;
    return var;
}

std::optional<LiteralPtr>
ConstEvaluator::evaluateBinaryLiterals(LiteralPtr lhs,
                                       LiteralPtr rhs,
                                       const std::string &op) {
    // Helpers to map ConstValue -> numeric primitives
    auto toDouble = [](const ConstValue &v) -> double {
        if (auto pi = std::get_if<int64_t>(&v))
            return static_cast<double>(*pi);
        if (auto pu = std::get_if<uint64_t>(&v))
            return static_cast<double>(*pu);
        if (auto pd = std::get_if<double>(&v))
            return *pd;
        throw std::runtime_error("Not a numeric value");
    };
    auto toInt64 = [](const ConstValue &v) -> int64_t {
        if (auto pi = std::get_if<int64_t>(&v))
            return *pi;
        if (auto pu = std::get_if<uint64_t>(&v))
            return static_cast<int64_t>(*pu);
        throw std::runtime_error("Not an integer value");
    };
    auto toUInt64 = [](const ConstValue &v) -> uint64_t {
        if (auto pu = std::get_if<uint64_t>(&v))
            return *pu;
        if (auto pi = std::get_if<int64_t>(&v))
            return static_cast<uint64_t>(*pi);
        throw std::runtime_error("Not an integer value");
    };

    // Are both numeric?
    auto isNumericConst = [](const ConstValue &v) {
        return std::holds_alternative<int64_t>(v) ||
               std::holds_alternative<uint64_t>(v) ||
               std::holds_alternative<double>(v);
    };

    if (isNumericConst(lhs->value) && isNumericConst(rhs->value)) {
        if (!lhs->lit_type || !rhs->lit_type || !lhs->lit_type->equals(rhs->lit_type)) {
            // Try to cast to a common type for mixed-type operations (e.g. int + float)
            if (m_type_checker->canImplicitCast(lhs->lit_type, rhs->lit_type)){
                auto casted = castLiteral(lhs, rhs->lit_type);
                if (casted) {
                    lhs = casted;
                } else {
                    error(nullptr, "Failed to cast left operand to common type for binary operation");
                    return std::nullopt;
                }
            } else if (m_type_checker->canImplicitCast(rhs->lit_type, lhs->lit_type)) {
                auto casted = castLiteral(rhs, lhs->lit_type);
                if (casted) {
                    rhs = casted;
                } else {
                    error(nullptr, "Failed to cast right operand to common type for binary operation");
                    return std::nullopt;
                }
            } else {
                error(nullptr, "Type mismatch between operands in binary operation and no implicit cast available");
                return std::nullopt;
            }

        }

        auto resultType = lhs->lit_type;

        if (resultType->isFloating()) {
            // float arithmetic: convert both to double, compute, store as double
            double l = 0.0;
            double r = 0.0;
            try {
                l = toDouble(lhs->value);
                r = toDouble(rhs->value);
            } catch (const std::runtime_error &e) {
                error(nullptr, "Floating point operation on non-numeric value");
                return std::nullopt;
            }
            double res{};

            if (op == "+")
                res = l + r;
            else if (op == "-")
                res = l - r;
            else if (op == "*")
                res = l * r;
            else if (op == "/") {
                if (r == 0.0) {
                    error(nullptr, "Division by zero");
                    return std::nullopt;
                }
                res = l / r;
            } else if (op == "==")
                return std::make_shared<Literal>(l == r, std::make_shared<Boolean>());
            else if (op == "!=")
                return std::make_shared<Literal>(l != r, std::make_shared<Boolean>());
            else if (op == "<")
                return std::make_shared<Literal>(l < r, std::make_shared<Boolean>());
            else if (op == "<=")
                return std::make_shared<Literal>(l <= r, std::make_shared<Boolean>());
            else if (op == ">")
                return std::make_shared<Literal>(l > r, std::make_shared<Boolean>());
            else if (op == ">=")
                return std::make_shared<Literal>(l >= r, std::make_shared<Boolean>());
            else
                return std::nullopt;

            // create literal with promoted float type (we store float values as double)
            return std::make_shared<Literal>(res, resultType);
        } else {
            // integer arithmetic: decide signedness from resultType
            bool resultUnsigned = resultType->isUnsigned();
            if (resultUnsigned) {
                uint64_t l = 0;
                uint64_t r = 0;
                try {
                    l = toUInt64(lhs->value);
                    r = toUInt64(rhs->value);
                } catch (const std::runtime_error &e) {
                    error(nullptr, "Integer operation on non-integer value");
                    return std::nullopt;
                }
                uint64_t res_u{};
                if (op == "+")
                    res_u = l + r;
                else if (op == "-")
                    res_u = l - r;
                else if (op == "*")
                    res_u = l * r;
                else if (op == "/") {
                    if (r == 0) {
                        error(nullptr, "Division by zero");
                        return std::nullopt;
                    }
                    res_u = l / r;
                } else if (op == "%") {
                    if (r == 0) {
                        error(nullptr, "Modulo by zero");
                        return std::nullopt;
                    }
                    res_u = l % r;
                } else if (op == "|")
                    res_u = l | r;
                else if (op == "&")
                    res_u = l & r;
                else if (op == "^")
                    res_u = l ^ r;
                else if (op == "<<")
                    res_u = l << static_cast<uint64_t>(r);
                else if (op == ">>")
                    res_u = l >> static_cast<uint64_t>(r);
                else if (op == "==")
                    return std::make_shared<Literal>(l == r, std::make_shared<Boolean>());
                else if (op == "!=")
                    return std::make_shared<Literal>(l != r, std::make_shared<Boolean>());
                else if (op == "<")
                    return std::make_shared<Literal>(l < r, std::make_shared<Boolean>());
                else if (op == "<=")
                    return std::make_shared<Literal>(l <= r, std::make_shared<Boolean>());
                else if (op == ">")
                    return std::make_shared<Literal>(l > r, std::make_shared<Boolean>());
                else if (op == ">=")
                    return std::make_shared<Literal>(l >= r, std::make_shared<Boolean>());
                else
                    return std::nullopt;

                return std::make_shared<Literal>(res_u, resultType);
            } else {
                int64_t l = 0;
                int64_t r = 0;
                try {
                    l = toInt64(lhs->value);
                    r = toInt64(rhs->value);
                } catch (const std::runtime_error &e) {
                    error(nullptr, "Integer operation on non-integer value");
                    return std::nullopt;
                }
                int64_t res_s{};
                if (op == "+")
                    res_s = l + r;
                else if (op == "-")
                    res_s = l - r;
                else if (op == "*")
                    res_s = l * r;
                else if (op == "/") {
                    if (r == 0) {
                        error(nullptr, "Division by zero");
                        return std::nullopt;
                    }
                    res_s = l / r;
                } else if (op == "%") {
                    if (r == 0) {
                        error(nullptr, "Modulo by zero");
                        return std::nullopt;
                    }
                    res_s = l % r;
                } else if (op == "|")
                    res_s = l | r;
                else if (op == "&")
                    res_s = l & r;
                else if (op == "^")
                    res_s = l ^ r;
                else if (op == "<<")
                    res_s = l << static_cast<int64_t>(r);
                else if (op == ">>")
                    res_s = l >> static_cast<int64_t>(r);
                else if (op == "==")
                    return std::make_shared<Literal>(l == r, std::make_shared<Boolean>());
                else if (op == "!=")
                    return std::make_shared<Literal>(l != r, std::make_shared<Boolean>());
                else if (op == "<")
                    return std::make_shared<Literal>(l < r, std::make_shared<Boolean>());
                else if (op == "<=")
                    return std::make_shared<Literal>(l <= r, std::make_shared<Boolean>());
                else if (op == ">")
                    return std::make_shared<Literal>(l > r, std::make_shared<Boolean>());
                else if (op == ">=")
                    return std::make_shared<Literal>(l >= r, std::make_shared<Boolean>());
                else
                    return std::nullopt;

                return std::make_shared<Literal>(res_s, resultType);
            }
        }
    }

    // Boolean operations (non-numeric)
    if (auto l = std::get_if<bool>(&lhs->value)) {
        if (auto r = std::get_if<bool>(&rhs->value)) {
            bool res{};
            if (op == "&&")
                res = *l && *r;
            else if (op == "||")
                res = *l || *r;
            else if (op == "==")
                res = *l == *r;
            else if (op == "!=")
                res = *l != *r;
            else
                return std::nullopt;
            return std::make_shared<Literal>(res, std::make_shared<Boolean>());
        }
    }

    if (auto l = std::get_if<std::string>(&lhs->value)) {
        if (auto r = std::get_if<std::string>(&rhs->value)) {
            if (op == "==")
                return std::make_shared<Literal>(*l == *r, std::make_shared<Boolean>());
            if (op == "!=")
                return std::make_shared<Literal>(*l != *r, std::make_shared<Boolean>());
            return std::nullopt;
        }
    }

    // TODO: string concatenation, pointer arithmetic, etc.

    return std::nullopt;
}

std::shared_ptr<Literal> ConstEvaluator::castLiteral(const LiteralPtr &lit, const std::shared_ptr<Type> &targetType) {
    auto lit_type = lit->lit_type;
    if (!canExplicitCast(lit_type, targetType)) {
        error(lit, "Invalid constant cast from " + lit_type->str() + " to " +
                       targetType->str());
        return nullptr;
    }
    // Create a TypeCast node to reuse existing logic
    auto typeCastNode = std::make_shared<TypeCast>(lit, targetType, CastType::Normal);
    auto castedLiteralOpt = evaluateExpression(typeCastNode);
    if (!castedLiteralOpt) {
        error(lit, "Failed to cast literal from " + lit_type->str() + " to " +
                       targetType->str());
        return nullptr;
    }
    auto castedLiteral = std::dynamic_pointer_cast<Literal>(*castedLiteralOpt);
    if (!castedLiteral) {
        error(lit, "Casted expression is not a literal");
        return nullptr;
    }
    return castedLiteral;
}


