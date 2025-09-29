
#include "typecheck.h"
#include "ast.h"
#include <memory>
#include <typeinfo>

int typeRank(TypeKind k) {
  switch (k) {
  case TypeKind::U8:
    return 1;
  case TypeKind::I32:
    return 2;
  case TypeKind::U32:
    return 3;
  case TypeKind::I64:
    return 4;
  case TypeKind::U64:
    return 5;
  case TypeKind::USize:
    return 6;
  case TypeKind::F32:
    return 7;
  case TypeKind::F64:
    return 8;
  default:
    return -1;
  }
}

TypeKind kindOf(const std::shared_ptr<Type> &t) {
  if (!t)
    return TypeKind::Unknown;
  if (dynamic_cast<BOOL *>(t.get()))
    return TypeKind::Bool;
  if (dynamic_cast<I32 *>(t.get()))
    return TypeKind::I32;
  if (dynamic_cast<I64 *>(t.get()))
    return TypeKind::I64;
  if (dynamic_cast<U8 *>(t.get()))
    return TypeKind::U8;
  if (dynamic_cast<U32 *>(t.get()))
    return TypeKind::U32;
  if (dynamic_cast<U64 *>(t.get()))
    return TypeKind::U64;
  if (dynamic_cast<USize *>(t.get()))
    return TypeKind::USize;
  if (dynamic_cast<F32 *>(t.get()))
    return TypeKind::F32;
  if (dynamic_cast<F64 *>(t.get()))
    return TypeKind::F64;
  if (dynamic_cast<PointerType *>(t.get()))
    return TypeKind::Pointer;
  if (dynamic_cast<ArrayType *>(t.get()))
    return TypeKind::Array;
  if (dynamic_cast<StructType *>(t.get()))
    return TypeKind::Struct;
  if (dynamic_cast<FunctionType *>(t.get()))
    return TypeKind::Function;
  if (dynamic_cast<NullType *>(t.get()))
    return TypeKind::Null;
  return TypeKind::Unknown;
}

int numericRank(TypeKind k) {
  switch (k) {
  case TypeKind::U8:
    return 1;
  case TypeKind::I32:
    return 2;
  case TypeKind::U32:
    return 3;
  case TypeKind::I64:
    return 4;
  case TypeKind::U64:
    return 5;
  case TypeKind::USize:
    return 6;
  case TypeKind::F32:
    return 7;
  case TypeKind::F64:
    return 8;
  default:
    return -1;
  }
}
bool isNumeric(TypeKind k) { return numericRank(k) >= 0; }

TypeChecker::TypeChecker() { pushScope(); }

void TypeChecker::error(ASTNodePtr node, const std::string &msg) {
  m_errors.push_back(std::make_pair(node, msg));
}

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
  if (typeEquals(from, to))
    return true;

  TypeKind fk = kindOf(from), tk = kindOf(to);

  // null → pointer
  if (fk == TypeKind::Null && tk == TypeKind::Pointer)
    return true;

  // numeric promotion (only widening allowed)
  if (isNumeric(fk) && isNumeric(tk)) {
    return numericRank(fk) <= numericRank(tk);
  }

  // pointer → void* (modeled as U8* in your code)
  if (fk == TypeKind::Pointer && tk == TypeKind::Pointer) {
    auto fromPtr = std::dynamic_pointer_cast<PointerType>(from);
    auto toPtr = std::dynamic_pointer_cast<PointerType>(to);
    if (!fromPtr || !toPtr)
      return false;
    if (dynamic_cast<U8 *>(toPtr->base.get()))
      return true;
  }

  // array → pointer decay
  if (fk == TypeKind::Array && tk == TypeKind::Pointer) {
    auto arr = std::dynamic_pointer_cast<ArrayType>(from);
    auto toPtr = std::dynamic_pointer_cast<PointerType>(to);
    if (arr && toPtr && typeEquals(arr->base, toPtr->base))
      return true;
  }

  return false;
}
bool TypeChecker::canExplicitCast(const std::shared_ptr<Type> &from,
                                  const std::shared_ptr<Type> &to) {
  if (!from || !to)
    return false;
  if (typeEquals(from, to))
    return true;

  TypeKind fk = kindOf(from), tk = kindOf(to);

  // Any numeric ↔ any numeric
  if (isNumeric(fk) && isNumeric(tk))
    return true;

  // null ↔ pointer
  if (fk == TypeKind::Null && tk == TypeKind::Pointer)
    return true;
  if (fk == TypeKind::Pointer && tk == TypeKind::Null)
    return true;

  // pointer ↔ pointer (reinterpret)
  if (fk == TypeKind::Pointer && tk == TypeKind::Pointer)
    return true;

  // array ↔ pointer
  if ((fk == TypeKind::Array && tk == TypeKind::Pointer) ||
      (fk == TypeKind::Pointer && tk == TypeKind::Array)) {
    return true;
  }

  // function pointer casts
  if (fk == TypeKind::Pointer && tk == TypeKind::Function)
    return true;
  if (fk == TypeKind::Function && tk == TypeKind::Pointer)
    return true;

  return false;
}

bool TypeChecker::typeEquals(const std::shared_ptr<Type> &a,
                             const std::shared_ptr<Type> &b) const {
  if (!a || !b)
    return false;
  if (dynamic_cast<NullType *>(a.get())) {
    return dynamic_cast<PointerType *>(b.get()) != nullptr;
  }
  if (dynamic_cast<NullType *>(b.get())) {
    return dynamic_cast<PointerType *>(a.get()) != nullptr;
  }
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
      it->second->methods = sd->methods;
      it->second->complete = true;
    } else {
      error(sd, "Internal error: struct " + sd->name + " not found in map");
    }

    for (const auto &m : sd->methods) {
      // Type check method bodies
      checkFunctionDeclaration(m.second);
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
  // Already registered the struct type in check(); nothing else to do for
  // now.
  (void)st;
}

void TypeChecker::checkFunctionDeclaration(
    const std::shared_ptr<FunctionDeclaration> &fn) {
  if (!fn->type) {
    error(fn, "Function " + fn->name + " has no type information");
    return;
  }
  pushScope();
  for (size_t i = 0; i < fn->type->params.size(); ++i) {
    std::string pname =
        (i < fn->param_names.size() ? fn->param_names[i]
                                    : ("arg" + std::to_string(i)));
    if (!insertSymbol(pname, fn->type->params[i])) {
      error(fn,
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

  if (!init)
    goto end;

  if (auto expr = std::dynamic_pointer_cast<Expression>(init)) {
    auto init_type = resolveType(inferExpression(expr));

    if (!var->var_type) {
      var->var_type = init_type; // TODO: decide if this is allowed
    }

    if (!init_type) {
      error(init, "Failed to infer type of initializer for variable " + name);
      goto end;
    }

    if (!typeEquals(init_type, var->var_type)) {
      if (canImplicitCast(init_type, var->var_type)) {
        init =
            std::make_shared<TypeCast>(expr, var->var_type, CastType::Normal);
      } else if (canExplicitCast(init_type, var->var_type)) {
        error(init, "Explicit cast needed in initializer for variable " + name +
                        ": cannot implicitly convert " + typeName(init_type) +
                        " to " + typeName(var->var_type));
        goto end;
      } else {
        error(var, "Type mismatch in initializer for variable " + name +
                       ": expected " + typeName(var->var_type) + " but got " +
                       typeName(init_type));
        goto end;
      }
    }
  } else if (auto si = std::dynamic_pointer_cast<StructInitializer>(init)) {
    auto t = inferStructInit(si);
    if (!t || !typeEquals(t, var->var_type)) {
      error(init, "Type mismatch in initializer for variable " + name);
      goto end;
    }
  } else {
    error(init, "Unsupported initializer node for variable " + name);
    goto end;
  }

end:
  if (!insertSymbol(name, var->var_type)) {
    error(var, "Duplicate variable declaration: " + name);
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
      error(iff->condition,
            "Condition in if-statement is not boolean: got " + typeName(t));
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
        error(ret->value, "Return with value in void function");
      }
    } else {
      if (!ret->value) {
        error(ret, "Return without value in non-void function");
      } else {
        auto t = inferExpression(ret->value);
        if (!t || !typeEquals(t, m_expected_return_type)) {
          error(ret, "Return type mismatch: expected " +
                         typeName(m_expected_return_type) + " but got " +
                         typeName(t));
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
      error(asg,
            "Assignment type mismatch: " + typeName(lt) + " = " + typeName(rt));
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
      error(wh->condition,
            "Condition in while-statement is not boolean: got " + typeName(t));
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
  std::cout << "Inferring type for expr: " << expr->toString() << " at "
            << expr->line << ":" << expr->col << std::endl;
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
  if (auto si = std::dynamic_pointer_cast<StructInitializer>(expr))
    return inferStructInit(si);
  if (auto tc = std::dynamic_pointer_cast<TypeCast>(expr))
    return inferTypeCast(tc);
  if (auto d = std::dynamic_pointer_cast<Dereference>(expr))
    return inferDereference(d);
  if (auto mc = std::dynamic_pointer_cast<MethodCall>(expr))
    return inferMethodCall(mc);
  // Unknown expression kind
  error(expr, "Type inference: unhandled expression type: " + expr->str());
  return nullptr;
}

CastResult TypeChecker::unifyBinaryOperands(const ExprPtr &lhs,
                                            const std::shared_ptr<Type> &lt,
                                            const ExprPtr &rhs,
                                            const std::shared_ptr<Type> &rt,
                                            const ASTNodePtr &ctx) {
  CastResult result;
  result.left = lhs;
  result.right = rhs;

  // Exact match, no casts needed
  if (typeEquals(lt, rt)) {
    result.type = lt;
    return result;
  }

  // If left can be promoted to right
  if (canImplicitCast(lt, rt)) {
    result.left = std::make_shared<TypeCast>(lhs, rt, CastType::Normal);
    result.type = rt;
    return result;
  }

  // If right can be promoted to left
  if (canImplicitCast(rt, lt)) {
    result.right = std::make_shared<TypeCast>(rhs, lt, CastType::Normal);
    result.type = lt;
    return result;
  }

  // Error: no valid promotion
  error(ctx, "Cannot unify binary operands: " + typeName(lt) + " vs " +
                 typeName(rt));
  return result;
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
  error(tc, "Invalid type cast from " + typeName(ot) + " to " +
                tc->target_type->str());
  return nullptr;
}

std::shared_ptr<Type>
TypeChecker::inferVarAccess(const std::shared_ptr<VarAccess> &v) {
  auto maybe = lookupSymbol(v->name);
  if (!maybe) {
    error(v, "Unknown variable: " + v->name);
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

    if (isNumeric(kindOf(lt)) && isNumeric(kindOf(rt))) {
      auto unified = unifyBinaryOperands(bin->left, lt, bin->right, rt, bin);
      bin->left = unified.left;
      bin->right = unified.right;
      return unified.type;
    }
    bin->inferred_type = resolveType(lt);
    return lt;
  }

  // comparisons -> BOOL
  if (bin->op == "==" || bin->op == "!=" || bin->op == "<" || bin->op == ">" ||
      bin->op == "<=" || bin->op == ">=") {
    if (!typeEquals(lt, rt)) {
      error(bin, "Comparison operands must have same type: " + typeName(lt) +
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
      error(bin->left,
            "Left operand of assignment is not an lvalue: " + bin->left->str());
      return nullptr;
    }
    if (lt->is_const) {
      error(bin->left, "Cannot assign to const value");
      return nullptr;
    }
    if (auto deref = std::dynamic_pointer_cast<Dereference>(bin->left)) {
      // make sure we're not assigning to a const pointer
      auto pt = inferExpression(deref->pointer);
      if (!pt)
        return nullptr;
      auto ptype = std::dynamic_pointer_cast<PointerType>(pt);
      if (!ptype) {
        error(deref->pointer,
              "Dereference of non-pointer type: " + typeName(pt));
        return nullptr;
      }
      if (ptype->pointer_const) {
        error(deref->pointer, "Cannot modify value through const pointer");
        return nullptr;
      }
    }
    if (!typeEquals(lt, rt)) {
      error(bin,
            "Assignment type mismatch: " + typeName(lt) + " = " + typeName(rt));
      return nullptr;
    }
    bin->inferred_type = resolveType(lt);
    return lt;
  }

  error(bin, "Unhandled binary operator: " + bin->op);
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
      error(un, "Logical not expects boolean operand, got " + typeName(ot));
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
  error(un, "Unhandled unary operator: " + un->op);
  return nullptr;
}

std::shared_ptr<Type>
TypeChecker::inferDereference(const std::shared_ptr<Dereference> &d) {
  auto ot = inferExpression(d->pointer);
  if (!ot)
    return nullptr;
  auto pt = std::dynamic_pointer_cast<PointerType>(ot);
  if (!pt) {
    error(d, "Dereference of non-pointer type: " + typeName(ot));
    return nullptr;
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
      error(mc, "Method call on non-struct type: " + typeName(bt));
      return nullptr;
    }
  }
  auto it = st->methods.find(mc->method);
  if (it == st->methods.end()) {
    error(mc, "Struct " + st->name + " has no method " + mc->method);
    return nullptr;
  }
  auto ftype = std::dynamic_pointer_cast<FunctionType>(resolveType(it->second->type));
  // check arity (naive, no implicit conversions)
  if (!ftype->variadic &&
      mc->args.size() !=
          (ftype->params.size() - 1)) {
    error(mc, "Method call argument count mismatch: expected " +
                  std::to_string(ftype->params.size() - 1) + " got " +
                  std::to_string(mc->args.size()));
    return nullptr;
  }
  size_t n = ftype->params.size();
  for (size_t i = 0; i < mc->args.size(); ++i) {
    auto at = inferExpression(mc->args[i]);
    if (!at)
      error(mc, "Failed to infer type of method call argument " +
                    std::to_string(i));
    if (i + 1 < ftype->params.size() && !typeEquals(at, ftype->params[i + 1])) {
      error(mc, "Method call argument " + std::to_string(i) +
                    " type mismatch: expected " + typeName(ftype->params[i + 1]) +
                    " got " + typeName(at));
    }
  }
  mc->inferred_type = resolveType(ftype->ret);
  return ftype->ret;
}

std::shared_ptr<Type>
TypeChecker::inferFuncCall(const std::shared_ptr<FuncCall> &call) {
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
        error(call,
              "Attempted to call non-function pointer type: " + typeName(ft));
        return nullptr;
      }
    } else {
      error(call, "Attempted to call non-function type: " + typeName(ft));
      return nullptr;
    }
  }

  // check arity (naive, no implicit conversions)
  if (!ftype->variadic &&
      call->args.size() !=
          (ftype->params.size())) { // if method, first param is 'self'
    error(call, "Function call argument count mismatch: expected " +
                    std::to_string(ftype->params.size()) + " got " +
                    std::to_string(call->args.size()));
    return nullptr;
  }

  size_t n = ftype->params.size();
  for (size_t i = 0; i < call->args.size(); ++i) {
    auto at = inferExpression(call->args[i]);
    if (!at)
      error(call, "Failed to infer type of function call argument " +
                      std::to_string(i));

    if (i < ftype->params.size() && !typeEquals(at, ftype->params[i])) {
      error(call, "Function call argument " + std::to_string(i) +
                      " type mismatch: expected " + typeName(ftype->params[i]) +
                      " got " + typeName(at));
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
        error(fa, "Field access on non-struct type: " + typeName(bt));
        return nullptr;
      }
    }
    auto ft = st->getFieldType(fa->field);
    if (!ft) {
      error(fa, fa->str() + " has no field " + fa->field);
      return nullptr;
    }
    std::cout << "Resolved field access " << fa->str() << " to type "
              << ft->str() << "\n";
    std::cout << "Line: " << fa->line << " Col: " << fa->col << "\n";
    fa->inferred_type = resolveType(ft);
    return ft;
  }
  error(fa->base, "FieldAccess base is not an expression: " + fa->base->str());
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
    error(oa, "Offset access on non-array/pointer type: " + typeName(bt));
    return nullptr;
  } else {
    error(oa->base,
          "OffsetAccess base is not an expression: " + oa->base->str());
    return nullptr;
  }
}

std::shared_ptr<Type>
TypeChecker::inferStructInit(const std::shared_ptr<StructInitializer> &init) {
  // ensure struct type exists (it should, as it's shared_ptr<StructType>)
  auto st = init->struct_type;
  if (!st) {
    error(init, "Struct initializer has no struct type");
    return nullptr;
  }
  // check fields are present and types match

  for (auto &p : init->field_values) {
    auto ft = resolveType(st->getFieldType(p.first));
    if (ft == nullptr) {
      error(init, "Struct " + st->name + " has no field " + p.first);
      return nullptr;
    }
    auto vt = inferExpression(p.second);
    if (!vt)
      return nullptr;
    if (!typeEquals(vt, ft)) {
      error(init, "Struct " + st->name + " field " + p.first +
                      " type mismatch: expected " + typeName(ft) + " got " +
                      typeName(vt));
      return nullptr;
    }
  }
  init->inferred_type = resolveType(st);
  return st;
}
