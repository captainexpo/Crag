#include "typecheck.h"
#include <memory>
#include <stdexcept>

std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> TypeChecker::expandSizeOf(const std::shared_ptr<FuncCall> &call) {
    assert(call->args.size() == 1);
    if (auto type_arg = std::dynamic_pointer_cast<TypeExpression>(call->args[0])) {
        type_arg->type = resolveType(call, type_arg->type);
        assert(type_arg != nullptr);
        auto lit = std::make_shared<Literal>(static_cast<uint64_t>(getTypeSize(type_arg->type)), std::make_shared<USize>());
        lit->inferred_type = std::make_shared<USize>();
        return std::make_pair(std::make_shared<USize>(), lit);
    }
    else if (auto expr_arg = std::dynamic_pointer_cast<Expression>(call->args[0])) {
        assert(expr_arg != nullptr);
        auto inferred_type = resolveType(call ,inferExpression(expr_arg));
        if (!inferred_type) {
            throw TypeCheckError(call, "Failed to infer type of sizeof argument");
        }
        auto lit = std::make_shared<Literal>(static_cast<uint64_t>(getTypeSize(inferred_type)), std::make_shared<USize>());
        lit->inferred_type = std::make_shared<USize>();
        return std::make_pair(std::make_shared<USize>(), lit);
    } else {
        throw TypeCheckError(call, "Invalid argument to sizeof");
    }
}

std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> TypeChecker::expandSlice(
    const std::shared_ptr<FuncCall> &call) {
    if (call->args.size() != 2) {
        throw TypeCheckError(call, "slice expects exactly 2 arguments: slice(ptr, len)");
    }

    auto ptr_expr = std::dynamic_pointer_cast<Expression>(call->args[0]);
    auto len_expr = std::dynamic_pointer_cast<Expression>(call->args[1]);
    if (!ptr_expr || !len_expr) {
        throw TypeCheckError(call, "slice arguments must be expressions");
    }

    auto ptr_type = resolveType(call, inferExpression(ptr_expr));
    if (!ptr_type) {
        throw TypeCheckError(call, "Failed to infer type of slice pointer argument");
    }

    auto ptr = std::dynamic_pointer_cast<PointerType>(ptr_type);
    if (!ptr) {
        throw TypeCheckError(call, "slice expects a pointer as its first argument");
    }

    auto elem_type = resolveType(call, ptr->base);
    auto slice_type = std::make_shared<ArrayType>(elem_type, nullptr, true);

    // Ensure the length expression is inferred before creating a TypeCast for it
    auto inferred_len_type = resolveType(call, inferExpression(len_expr));
    (void)inferred_len_type; // not used directly; we still cast to USize
    auto len_type = std::make_shared<USize>();
    auto ptr_field = std::make_shared<TypeCast>(ptr_expr, ptr_type, CastType::Normal);
    auto len_field = std::make_shared<TypeCast>(len_expr, len_type, CastType::Normal);

    std::map<std::string, std::shared_ptr<Expression>> fields;
    fields["ptr"] = ptr_field;
    fields["len"] = len_field;

    auto init = std::make_shared<StructInitializer>(std::make_shared<TypeExpression>(slice_type), fields);
    init->inferred_type = slice_type;
    return std::make_pair(slice_type, init);
}

std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> TypeChecker::expandOffsetOf(const std::shared_ptr<FuncCall> &call) {
    throw std::runtime_error("Unimplemented");
}
std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> TypeChecker::expandAlignOf(const std::shared_ptr<FuncCall> &call) {
    throw std::runtime_error("Unimplemented");
}
