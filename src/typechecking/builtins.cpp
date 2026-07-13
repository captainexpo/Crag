#include "src/ast/ast.h"
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
            throw TypeCheckError(current_module, call, "Failed to infer type of sizeof argument");
        }
        auto lit = std::make_shared<Literal>(static_cast<uint64_t>(getTypeSize(inferred_type)), std::make_shared<USize>());
        lit->inferred_type = std::make_shared<USize>();
        return std::make_pair(std::make_shared<USize>(), lit);
    } else {
        throw TypeCheckError(current_module, call, "Invalid argument to sizeof");
    }
}

std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> TypeChecker::expandSlice(
    const std::shared_ptr<FuncCall> &call) {
    if (call->args.size() != 2) {
        throw TypeCheckError(current_module, call, "slice expects exactly 2 arguments: slice(ptr, len)");
    }

    auto ptr_expr = std::dynamic_pointer_cast<Expression>(call->args[0]);
    auto len_expr = std::dynamic_pointer_cast<Expression>(call->args[1]);
    if (!ptr_expr || !len_expr) {
        throw TypeCheckError(current_module, call, "slice arguments must be expressions");
    }

    auto ptr_type = resolveType(call, inferExpression(ptr_expr));
    if (!ptr_type) {
        throw TypeCheckError(current_module, call, "Failed to infer type of slice pointer argument");
    }

    auto ptr = std::dynamic_pointer_cast<PointerType>(ptr_type);
    if (!ptr) {
        throw TypeCheckError(current_module, call, "slice expects a pointer as its first argument");
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

std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> TypeChecker::expandNewStr(
    const std::shared_ptr<FuncCall> &call) {
    if (call->args.size() != 2) {
        throw TypeCheckError(current_module, call, "slice expects exactly 2 arguments: slice(ptr, len)");
    }

    auto ptr_expr = std::dynamic_pointer_cast<Expression>(call->args[0]);
    auto len_expr = std::dynamic_pointer_cast<Expression>(call->args[1]);
    if (!ptr_expr || !len_expr) {
        throw TypeCheckError(current_module, call, "slice arguments must be expressions");
    }

    auto ptr_type = resolveType(call, inferExpression(ptr_expr));
    if (!ptr_type) {
        throw TypeCheckError(current_module, call, "Failed to infer type of slice pointer argument");
    }

    auto ptr = std::dynamic_pointer_cast<PointerType>(ptr_type);
    if (!ptr) {
        throw TypeCheckError(current_module, call, "slice expects a pointer as its first argument");
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

    auto stringType = std::make_shared<StringType>();
    auto init = std::make_shared<StructInitializer>(std::make_shared<TypeExpression>(stringType), fields);
    init->inferred_type = stringType;
    return std::make_pair(stringType, init);
}

static int getTypeId(std::shared_ptr<Type> type){
    if(std::dynamic_pointer_cast<I8>(type)) return 1;
    if(std::dynamic_pointer_cast<I16>(type)) return 2;
    if(std::dynamic_pointer_cast<I32>(type)) return 3;
    if(std::dynamic_pointer_cast<I64>(type)) return 4;

    if(std::dynamic_pointer_cast<U8>(type)) return 5;
    if(std::dynamic_pointer_cast<U16>(type)) return 6;
    if(std::dynamic_pointer_cast<U32>(type)) return 7;
    if(std::dynamic_pointer_cast<U64>(type)) return 8;

    if(std::dynamic_pointer_cast<USize>(type)) return 9;

    if(std::dynamic_pointer_cast<F32>(type)) return 10;
    if(std::dynamic_pointer_cast<F64>(type)) return 11;

    if(std::dynamic_pointer_cast<Boolean>(type)) return 12;
    if(std::dynamic_pointer_cast<StringType>(type)) return 13;
    if(std::dynamic_pointer_cast<StructType>(type)) return 14;
    if (std::dynamic_pointer_cast<PointerType>(type)) return 15 + getTypeId(std::dynamic_pointer_cast<PointerType>(type)->base);

    throw std::runtime_error("Unknown type for typeid");
}

std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> TypeChecker::expandTypeId(const std::shared_ptr<FuncCall> &call) {
    auto argType = inferExpression(call->args[0]);
    auto resolvedType = resolveType(call, argType);
    if (!resolvedType) {
        throw TypeCheckError(current_module, call, "Failed to resolve type for typeid");

    }
    auto lit = std::make_shared<Literal>(static_cast<int32_t>(getTypeId(resolvedType)), std::make_shared<I32>());
    lit->inferred_type = std::make_shared<I32>();

    return std::make_pair(lit->inferred_type, lit);

}

std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> TypeChecker::expandOffsetOf(const std::shared_ptr<FuncCall> &call) {
    throw std::runtime_error("Unimplemented");
}
std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> TypeChecker::expandAlignOf(const std::shared_ptr<FuncCall> &call) {
    throw std::runtime_error("Unimplemented");
}
