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
std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> TypeChecker::expandOffsetOf(const std::shared_ptr<FuncCall> &call) {
    throw std::runtime_error("Unimplemented");
}
std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> TypeChecker::expandAlignOf(const std::shared_ptr<FuncCall> &call) {
    throw std::runtime_error("Unimplemented");
}
