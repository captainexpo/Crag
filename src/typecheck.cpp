
#include "typecheck.h"
#include <memory>
#include <typeinfo>

TypeChecker::TypeChecker() { pushScope(); }

void TypeChecker::error(const std::string &msg) { m_errors.push_back(msg); }

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

bool TypeChecker::typeEquals(const std::shared_ptr<Type> &a,
                             const std::shared_ptr<Type> &b) const {
  if (!a || !b)
    return false;

  if (typeid(*a) != typeid(*b))
    return false;

  if (auto pa = dynamic_cast<PointerType *>(a.get())) {
    auto pb = dynamic_cast<PointerType *>(b.get());
    return typeEquals(pa->base, pb->base);
  }
  if (auto aa = dynamic_cast<ArrayType *>(a.get())) {
    auto ab = dynamic_cast<ArrayType *>(b.get());
    return aa->length == ab->length && typeEquals(aa->base, ab->base);
  }
  if (auto sa = dynamic_cast<StructType *>(a.get())) {
    auto sb = dynamic_cast<StructType *>(b.get());
    return sa->name == sb->name;
  }
  if (auto fa = dynamic_cast<FunctionType *>(a.get())) {
    auto fb = dynamic_cast<FunctionType *>(b.get());
    if (!typeEquals(fa->ret, fb->ret))
      return false;
    if (fa->params.size() != fb->params.size())
      return false;
    for (size_t i = 0; i < fa->params.size(); ++i)
      if (!typeEquals(fa->params[i], fb->params[i]))
        return false;
    return fa->variadic == fb->variadic;
  }
  // primitive types: if dynamic type matches, consider equal
  return true;
}

std::string TypeChecker::typeName(const std::shared_ptr<Type> &t) const {
  return t ? t->str() : "<unknown>";
}

std::shared_ptr<Type>
TypeChecker::resolveType(const std::shared_ptr<Type> &t) const {
  if (!t)
    return nullptr;
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
    return std::make_shared<PointerType>(bt);
  }
  if (auto at = std::dynamic_pointer_cast<ArrayType>(t)) {
    auto bt = resolveType(at->base);
    if (!bt)
      return nullptr;
    return std::make_shared<ArrayType>(bt, at->length);
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
  // primitive types are already resolved
  return t;
}

// Entry point
void TypeChecker::check(const std::shared_ptr<Program> &node) {
  if (!node)
    return;
  std::vector<std::shared_ptr<StructDeclaration>> struct_decls;
  for (const auto &decl : node->declarations) {
    if (auto sd = std::dynamic_pointer_cast<StructDeclaration>(decl)) {
      // Build a StructType and register
      auto st = std::make_shared<StructType>(sd->name);
      st->complete = false;
      m_structs[sd->name] = st;
      insertSymbol(sd->name, st);
      struct_decls.push_back(sd);
    } else if (auto fd = std::dynamic_pointer_cast<FunctionDeclaration>(decl)) {
      m_functions[fd->name] = fd->type;
      insertSymbol(fd->name, fd->type);
    }
  }

  for (const auto &sd : struct_decls) {
    auto fields = std::vector<std::pair<std::string, std::shared_ptr<Type>>>();
    for (const auto &f : sd->fields) {
      fields.emplace_back(f.first, resolveType(f.second));
    }
    auto it = m_structs.find(sd->name);
    if (it != m_structs.end()) {
      it->second->fields = std::move(fields);
      it->second->complete = true;
    } else {
      error("Internal error: struct " + sd->name + " not found in map");
    }
  }

  for (const auto &decl : node->declarations) {
    checkNode(decl);
  }
}

void TypeChecker::checkNode(const std::shared_ptr<ASTNode> &node) {
  if (!node)
    return;
  if (auto fd = std::dynamic_pointer_cast<FunctionDeclaration>(node)) {
    checkFunctionDeclaration(fd);
  } else if (auto vd = std::dynamic_pointer_cast<VariableDeclaration>(node)) {
    checkVariableDeclaration(vd);
  } else if (auto sd = std::dynamic_pointer_cast<StructDeclaration>(node)) {
    checkStructDeclaration(sd);
  } else {
    // Unknown top-level node type -- ignore or emit a warning
  }
}

void TypeChecker::checkStructDeclaration(
    const std::shared_ptr<StructDeclaration> &st) {
  // Already registered the struct type in check(); nothing else to do for now.
  (void)st;
}

void TypeChecker::checkFunctionDeclaration(
    const std::shared_ptr<FunctionDeclaration> &fn) {
  if (!fn->type) {
    error("Function " + fn->name + " has no type information");
    return;
  }
  pushScope();
  for (size_t i = 0; i < fn->type->params.size(); ++i) {
    std::string pname =
        (i < fn->param_names.size() ? fn->param_names[i]
                                    : ("arg" + std::to_string(i)));
    if (!insertSymbol(pname, fn->type->params[i])) {
      error("Duplicate parameter name " + pname + " in function " + fn->name);
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
  if (!var->var_type) {
    error("Variable " + var->name + " has no declared type");
    return;
  }
  if (var->initializer) {
    // initializer may be expression or something that is ASTNode (we handle by
    // attempting cast)
    if (auto expr = std::dynamic_pointer_cast<Expression>(var->initializer)) {
      auto t = inferExpression(expr);
      if (!t) {
        error("Failed to infer type of initializer for variable " + var->name);
      } else if (!typeEquals(t, var->var_type)) {
        error("Type mismatch in initializer for variable " + var->name +
              ": expected " + typeName(var->var_type) + " but got " +
              typeName(t));
      }
    } else {
      // initializer is not an expression (could be StructInitializer stored as
      // ASTNode) - try dynamic cast to StructInitializer
      if (auto si =
              std::dynamic_pointer_cast<StructInitializer>(var->initializer)) {
        auto t = inferStructInit(si);
        if (!t || !typeEquals(t, var->var_type)) {
          error("Type mismatch in initializer for variable " + var->name);
        }
      } else {
        error("Unsupported initializer node for variable " + var->name);
      }
    }
  }

  // insert variable into current scope
  if (!insertSymbol(var->name, var->var_type)) {
    error("Duplicate variable declaration: " + var->name);
  }
}

void TypeChecker::checkStatement(const std::shared_ptr<Statement> &stmt) {
  if (!stmt)
    return;
  if (auto block = std::dynamic_pointer_cast<Block>(stmt)) {
    pushScope();
    for (auto &s : block->statements)
      checkStatement(s);
    popScope();
    return;
  }
  if (auto iff = std::dynamic_pointer_cast<IfStatement>(stmt)) {
    auto t = inferExpression(iff->condition);
    if (!t || !dynamic_cast<BOOL *>(t.get())) {
      error("Condition in if-statement is not boolean: got " + typeName(t));
    }
    checkStatement(iff->then_branch);
    if (iff->else_branch)
      checkStatement(iff->else_branch);
    return;
  }
  if (auto ret = std::dynamic_pointer_cast<ReturnStatement>(stmt)) {
    if (ret->value) {
      inferExpression(ret->value);
    }
    if (!m_expected_return_type) {
      if (ret->value) {
        error("Return with value in void function");
      }
    } else {
      if (!ret->value) {
        error("Return without value in non-void function");
      } else {
        auto t = inferExpression(ret->value);
        if (!t || !typeEquals(t, m_expected_return_type)) {
          error("Return type mismatch: expected " +
                typeName(m_expected_return_type) + " but got " + typeName(t));
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
    auto rt = inferExpression(asg->value);
    if (!lt || !rt)
      return;
    if (!typeEquals(lt, rt)) {
      error("Assignment type mismatch: " + typeName(lt) + " = " + typeName(rt));
    }
    return;
  }
  if (auto forst = std::dynamic_pointer_cast<ForStatement>(stmt)) {
    if (forst->init)
      checkStatement(forst->init);
    if (forst->condition)
      inferExpression(forst->condition);
    if (forst->increment)
      checkStatement(forst->increment);
    checkStatement(forst->body);
    return;
  }
  if (auto wh = std::dynamic_pointer_cast<WhileStatement>(stmt)) {
    auto t = inferExpression(wh->condition);
    if (!t || !dynamic_cast<BOOL *>(t.get())) {
      error("Condition in while-statement is not boolean: got " + typeName(t));
    }
    checkStatement(wh->body);
    return;
  }

  // fallback -- unhandled statement type
  (void)stmt;
}

// Expression inference — return inferred type or nullptr if error/unhandled
std::shared_ptr<Type>
TypeChecker::inferExpression(const std::shared_ptr<Expression> &expr) {
  if (!expr)
    return nullptr;
  if (auto va = std::dynamic_pointer_cast<VarAccess>(expr))
    return inferVarAccess(va);
  if (auto lit = std::dynamic_pointer_cast<Literal>(expr))
    return inferLiteral(lit);
  if (auto bin = std::dynamic_pointer_cast<BinaryOperation>(expr))
    return inferBinaryOp(bin);
  if (auto un = std::dynamic_pointer_cast<UnaryOperation>(expr))
    return inferUnaryOp(un);
  if (auto call = std::dynamic_pointer_cast<FuncCall>(expr))
    return inferFuncCall(call);
  if (auto fa = std::dynamic_pointer_cast<FieldAccess>(expr))
    return inferFieldAccess(fa);
  if (auto oa = std::dynamic_pointer_cast<OffsetAccess>(expr))
    return inferOffsetAccess(oa);
  if (auto c = std::dynamic_pointer_cast<Cast>(expr))
    return inferCast(c);
  if (auto si = std::dynamic_pointer_cast<StructInitializer>(expr))
    return inferStructInit(si);
  if (auto tc = std::dynamic_pointer_cast<TypeCast>(expr))
    return inferTypeCast(tc);
  if (auto d = std::dynamic_pointer_cast<Dereference>(expr))
    return inferDereference(d);
  // Unknown expression kind
  error("Type inference: unhandled expression type: " + expr->str());
  return nullptr;
}

std::shared_ptr<Type>
TypeChecker::inferTypeCast(const std::shared_ptr<TypeCast> &tc) {
  if (tc->cast_type == CastType::Reinterperet) {
    // allow any reinterpreted cast
    tc->inferred_type = resolveType(tc->target_type);
    return tc->target_type;
  }
  auto ot = inferExpression(tc->expr);
  if (!ot)
    return nullptr;
  // naive rule: allow cast if types are the same or both numeric types
  if (typeEquals(ot, tc->target_type)) {
    tc->inferred_type = resolveType(tc->target_type);
    return tc->target_type;
  }
  auto isNumeric = [](const std::shared_ptr<Type> &t) {
    return dynamic_cast<I32 *>(t.get()) || dynamic_cast<I64 *>(t.get()) ||
           dynamic_cast<F32 *>(t.get()) || dynamic_cast<F64 *>(t.get()) ||
           dynamic_cast<U32 *>(t.get()) || dynamic_cast<U64 *>(t.get()) ||
           dynamic_cast<USize *>(t.get());
  };
  if (isNumeric(ot) && isNumeric(tc->target_type)) {
    tc->inferred_type = resolveType(tc->target_type);
    return tc->target_type;
  }
  error("Invalid type cast from " + typeName(ot) + " to " +
        tc->target_type->str());
  return nullptr;
}

std::shared_ptr<Type>
TypeChecker::inferVarAccess(const std::shared_ptr<VarAccess> &v) {
  auto maybe = lookupSymbol(v->name);
  if (!maybe) {
    error("Unknown variable: " + v->name);
    return nullptr;
  }
  v->inferred_type = resolveType(*maybe);
  return *maybe;
}

std::shared_ptr<Type>
TypeChecker::inferLiteral(const std::shared_ptr<Literal> &lit) {
  lit->inferred_type = resolveType(lit->lit_type);
  return lit->lit_type;
}

std::shared_ptr<Type>
TypeChecker::inferBinaryOp(const std::shared_ptr<BinaryOperation> &bin) {
  auto lt = inferExpression(bin->left);
  auto rt = inferExpression(bin->right);
  if (!lt || !rt)
    return nullptr;

  // simple numeric ops
  if (bin->op == "+" || bin->op == "-" || bin->op == "*" || bin->op == "/") {
    // naive rule: both sides must be same numeric type
    // (I32/I64/F32/F64/U32/U64/USize)
    if (!typeEquals(lt, rt)) {
      error("Binary op operands have different types: " + typeName(lt) +
            " vs " + typeName(rt));
      return nullptr;
    }
    bin->inferred_type = resolveType(lt);
    return lt;
  }

  // comparisons -> BOOL
  if (bin->op == "==" || bin->op == "!=" || bin->op == "<" || bin->op == ">" ||
      bin->op == "<=" || bin->op == ">=") {
    if (!typeEquals(lt, rt)) {
      error("Comparison operands must have same type: " + typeName(lt) +
            " vs " + typeName(rt));
      return nullptr;
    }
    auto b = std::make_shared<BOOL>();
    bin->inferred_type = resolveType(b);
    return b;
  }

  if (bin->op == "=") {
    // assignment: left must be lvalue (VarAccess, FieldAccess, OffsetAccess)
    if (!(std::dynamic_pointer_cast<VarAccess>(bin->left) ||
          std::dynamic_pointer_cast<FieldAccess>(bin->left) ||
          std::dynamic_pointer_cast<OffsetAccess>(bin->left) ||
          std::dynamic_pointer_cast<Dereference>(bin->left))) {
      error("Left operand of assignment is not an lvalue: " + bin->left->str());
      return nullptr;
    }
    if (!typeEquals(lt, rt)) {
      error("Assignment type mismatch: " + typeName(lt) + " = " + typeName(rt));
      return nullptr;
    }
    bin->inferred_type = resolveType(lt);
    return lt;
  }

  error("Unhandled binary operator: " + bin->op);
  return nullptr;
}

std::shared_ptr<Type>
TypeChecker::inferUnaryOp(const std::shared_ptr<UnaryOperation> &un) {
  auto ot = inferExpression(un->operand);
  if (!ot)
    return nullptr;

  if (un->op == "-" || un->op == "+") {
    // numeric
    un->inferred_type = resolveType(ot);
    return ot;
  }
  if (un->op == "!") {
    if (!dynamic_cast<BOOL *>(ot.get())) {
      error("Logical not expects boolean operand, got " + typeName(ot));
      return nullptr;
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
  error("Unhandled unary operator: " + un->op);
  return nullptr;
}

std::shared_ptr<Type>
TypeChecker::inferDereference(const std::shared_ptr<Dereference> &d) {
  auto ot = inferExpression(d->pointer);
  if (!ot)
    return nullptr;
  auto pt = std::dynamic_pointer_cast<PointerType>(ot);
  if (!pt) {
    error("Dereference of non-pointer type: " + typeName(ot));
    return nullptr;
  }
  d->inferred_type = resolveType(pt->base);
  return pt->base;
}

std::shared_ptr<Type>
TypeChecker::inferFuncCall(const std::shared_ptr<FuncCall> &call) {
  // infer function expression type
  auto ft = inferExpression(call->func);
  if (!ft)
    return nullptr;
  auto ftype = std::dynamic_pointer_cast<FunctionType>(ft);
  if (!ftype) {
    error("Attempted to call non-function type: " + typeName(ft));
    return nullptr;
  }
  // check arity (naive, no implicit conversions)
  if (!ftype->variadic && call->args.size() != ftype->params.size()) {
    error("Function call argument count mismatch: expected " +
          std::to_string(ftype->params.size()) + " got " +
          std::to_string(call->args.size()));
    return nullptr;
  }

  size_t n = ftype->params.size();
  for (size_t i = 0; i < call->args.size(); ++i) {
    auto at = inferExpression(call->args[i]);
    if (!at)
      error("Failed to infer type of function call argument " +
            std::to_string(i));

    if (i < ftype->params.size() && !typeEquals(at, ftype->params[i])) {
      error("Function call argument " + std::to_string(i) +
            " type mismatch: expected " + typeName(ftype->params[i]) + " got " +
            typeName(at));
    }
  }

  call->inferred_type = resolveType(ftype->ret);
  return ftype->ret;
}

std::shared_ptr<Type>
TypeChecker::inferFieldAccess(const std::shared_ptr<FieldAccess> &fa) {
  // base could be expression or ASTNode
  if (auto baseExpr = std::dynamic_pointer_cast<Expression>(fa->base)) {
    auto bt = inferExpression(baseExpr);
    if (!bt)
      return nullptr;
    auto st = std::dynamic_pointer_cast<StructType>(bt);
    if (!st) {
      auto pt = std::dynamic_pointer_cast<PointerType>(bt);
      if (pt)
        st = std::dynamic_pointer_cast<StructType>(resolveType(pt->base));
      if (!pt) {
        error("Field access on non-struct type: " + typeName(bt));
        return nullptr;
      }
    }
    auto ft = st->getFieldType(fa->field);
    if (!ft) {
      error(fa->str() + " has no field " + fa->field);
      return nullptr;
    }
    fa->inferred_type = resolveType(ft);
    return ft;
  }
  error("FieldAccess base is not an expression: " + fa->base->str());
  return nullptr;
}

std::shared_ptr<Type>
TypeChecker::inferOffsetAccess(const std::shared_ptr<OffsetAccess> &oa) {
  // base could be expression or ASTNode
  if (auto baseExpr = std::dynamic_pointer_cast<Expression>(oa->base)) {
    auto bt = inferExpression(baseExpr);
    if (!bt)
      return nullptr;
    if (auto at = std::dynamic_pointer_cast<ArrayType>(bt)) {
      auto idxt = inferExpression(oa->index);
      if (!idxt)
        return nullptr;
      // naive: index must be integer type (I32/I64/USize/U32/U64 etc.) - here
      // we only check it's numeric/integer by checking type name contains "I"
      // or "U" or "USize" For now, accept any integer-like literal types —
      // better logic can be added.
      oa->inferred_type = resolveType(at->base);
      return at->base;
    }
    if (auto pt = std::dynamic_pointer_cast<PointerType>(bt)) {
      // pointer indexing returns base
      auto bt = resolveType(pt->base);
      oa->inferred_type = bt;
      return bt;
    }
    error("Offset access on non-array/pointer type: " + typeName(bt));
    return nullptr;
  } else {
    error("OffsetAccess base is not an expression: " + oa->base->str());
    return nullptr;
  }
}

std::shared_ptr<Type> TypeChecker::inferCast(const std::shared_ptr<Cast> &c) {
  // For now allow casts between primitive numeric types and pointers as-is.
  auto ot = inferExpression(c->operand);
  if (!ot)
    return nullptr;
  // You can add rules to restrict allowed casts.
  auto tt = resolveType(c->target_type);
  c->inferred_type = tt;
  return tt;
}

std::shared_ptr<Type>
TypeChecker::inferStructInit(const std::shared_ptr<StructInitializer> &init) {
  // ensure struct type exists (it should, as it's shared_ptr<StructType>)
  auto st = init->struct_type;
  if (!st) {
    error("Struct initializer has no struct type");
    return nullptr;
  }
  // check fields are present and types match

  for (auto &p : init->field_values) {
    auto ft = st->getFieldType(p.first);
    if (ft == nullptr) {
      error("Struct " + st->name + " has no field " + p.first);
      return nullptr;
    }
    auto vt = inferExpression(p.second);
    if (!vt)
      return nullptr;
    if (!typeEquals(vt, ft)) {
      error("Struct " + st->name + " field " + p.first +
            " type mismatch: expected " + typeName(ft) + " got " +
            typeName(vt));
      return nullptr;
    }
  }
  init->inferred_type = resolveType(st);
  return st;
}
