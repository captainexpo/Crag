#include "typecheck.h"
#include <memory>
#include <stdexcept>

std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> TypeChecker::expandSizeOf(const std::shared_ptr<FuncCall> &call) {
    assert(call->args.size() == 1);
    auto ty = std::dynamic_pointer_cast<TypeExpression>(call->args[0]);
    assert(ty != nullptr);
    auto lit = std::make_shared<Literal>(static_cast<uint64_t>(getTypeSize(ty->type)), std::make_shared<USize>());
    lit->inferred_type = std::make_shared<USize>();
    return std::make_pair(std::make_shared<USize>(), lit);
}
std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> TypeChecker::expandOffsetOf(const std::shared_ptr<FuncCall> &call) {
    throw std::runtime_error("Unimplemented");
}
std::pair<std::shared_ptr<Type>, std::shared_ptr<Expression>> TypeChecker::expandAlignOf(const std::shared_ptr<FuncCall> &call) {
    throw std::runtime_error("Unimplemented");
}
