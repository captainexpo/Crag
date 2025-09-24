
#include "parser.h"
#include "ast.h"
#include "lexer.h"
#include <algorithm>
#include <cstddef>
#include <iostream>
#include <memory>

Parser::Parser(const std::vector<Token> &tokens)
    : tokens(tokens), position(0) {}

ParseError Parser::raiseError(const std::string &msg, const Token &token) {
  std::vector<Token> context_tokens;
  int start = std::max(0, (int)position - 3);
  int end = std::min((int)tokens.size(), (int)(position + 3));
  for (int i = start; i < end; ++i)
    context_tokens.push_back(tokens[i]);
  return ParseError(msg, token, context_tokens);
}

// ---- Token helpers ----
Token Parser::peek() const {
  if (position < tokens.size())
    return tokens[position];
  return Token{TokenType::EOF_T, "", -1, -1};
}

Token Parser::advance() {
  Token t = peek();
  if (t.type != TokenType::EOF_T)
    position++;
  return t;
}

bool Parser::match(const std::set<TokenType> &types) {
  Token t = peek();
  if (types.count(t.type)) {
    position++;
    return true;
  }
  return false;
}

Token Parser::consume(TokenType expected_type) {
  Token t = peek();
  if (t.type == expected_type) {
    position++;
    return t;
  }
  throw raiseError("Unexpected token", t);
}

void Parser::synchronize() {
  while (peek().type != TokenType::EOF_T) {
    if (advance().type == TokenType::SEMICOLON)
      return;
    TokenType t = peek().type;
    if (t == TokenType::FN || t == TokenType::LET || t == TokenType::CONST ||
        t == TokenType::STRUCT)
      return;
  }
}

// ---- Type parsing ----
std::shared_ptr<Type> Parser::parse_type() {
  Token current = peek();
  switch (current.type) {
  case TokenType::ID:
    if (declared_structs.count(current.value)) {
      std::string name = consume(TokenType::ID).value;
      return declared_structs[name];
    }
    return parse_primitive_type();
  case TokenType::STAR:
    return parse_pointer_type();
  case TokenType::LBRACKET:
    return parse_array_type();
  case TokenType::LPAREN:
    return parse_function_type();
  default:
    throw raiseError("Unexpected token in type", current);
  }
}

std::shared_ptr<Type> Parser::parse_primitive_type() {
  std::string val = consume(TokenType::ID).value;
  if (val == "u8")
    return std::make_shared<U8>();
  if (val == "u32")
    return std::make_shared<U32>();
  if (val == "u64")
    return std::make_shared<U64>();
  if (val == "usize")
    return std::make_shared<USize>();
  if (val == "i32")
    return std::make_shared<I32>();
  if (val == "i64")
    return std::make_shared<I64>();
  if (val == "f32")
    return std::make_shared<F32>();
  if (val == "f64")
    return std::make_shared<F64>();
  if (val == "bool")
    return std::make_shared<BOOL>();
  if (val == "void")
    return std::make_shared<Void>();
  throw raiseError("Unknown primitive type " + val, peek());
}

std::shared_ptr<Type> Parser::parse_pointer_type() {
  consume(TokenType::STAR);
  return std::make_shared<PointerType>(parse_type());
}

std::shared_ptr<Type> Parser::parse_array_type() {
  consume(TokenType::LBRACKET);
  auto elem_type = parse_type();
  consume(TokenType::SEMICOLON);
  int size = std::stoi(consume(TokenType::NUMBER).value);
  consume(TokenType::RBRACKET);
  return std::make_shared<ArrayType>(elem_type, size);
}

std::shared_ptr<FunctionType> Parser::parse_function_type() {
  consume(TokenType::LPAREN);
  std::vector<std::shared_ptr<Type>> param_types;
  bool variadic = false;
  if (peek().type != TokenType::RPAREN) {
    while (true) {
      if (match({TokenType::TRIPLE_DOT})) {
        variadic = true;
        break;
      }
      param_types.push_back(parse_type());
      if (!match({TokenType::COMMA}))
        break;
    }
  }
  consume(TokenType::RPAREN);
  consume(TokenType::ARROW);
  auto ret_type = parse_type();
  return std::make_shared<FunctionType>(param_types, ret_type, variadic);
}

// ---- Declarations ----
std::shared_ptr<ASTNode> Parser::parse_declaration() {
  Token t = peek();
  switch (t.type) {
  case TokenType::FN:
    return parse_function_declaration();
  case TokenType::LET:
  case TokenType::CONST:
    return parse_variable_declaration();
  case TokenType::STRUCT:
    return parse_struct_declaration();
  default:
    return parse_statement();
  }
}

std::shared_ptr<VariableDeclaration> Parser::parse_variable_declaration() {
  bool is_const = match({TokenType::CONST});
  if (!is_const)
    consume(TokenType::LET);
  std::string name = consume(TokenType::ID).value;
  consume(TokenType::COLON);
  auto var_type = parse_type();
  std::shared_ptr<ASTNode> initializer = nullptr;
  if (match({TokenType::ASSIGN}))
    initializer = parse_expression();
  return std::make_shared<VariableDeclaration>(name, var_type, initializer);
}

std::shared_ptr<StructDeclaration> Parser::parse_struct_declaration() {
  consume(TokenType::STRUCT);
  std::string name = consume(TokenType::ID).value;
  declared_structs[name] = std::make_shared<StructType>(
      name, std::vector<std::pair<std::string, std::shared_ptr<Type>>>{});
  consume(TokenType::LBRACE);
  std::vector<std::pair<std::string, std::shared_ptr<Type>>> fields;
  while (peek().type != TokenType::RBRACE) {
    std::string fname = consume(TokenType::ID).value;
    consume(TokenType::COLON);
    auto ftype = parse_type();
    fields.push_back({fname, ftype});
    if (peek().type == TokenType::COMMA)
      consume(TokenType::COMMA);
  }
  consume(TokenType::RBRACE);
  std::vector<std::pair<std::string, std::shared_ptr<Type>>> fmap;
  for (size_t i = 0; i < fields.size(); ++i)
    fmap.push_back({fields[i].first, fields[i].second});
  declared_structs[name] = std::make_shared<StructType>(name, fmap);
  return std::make_shared<StructDeclaration>(name, fields);
}

// ---- Statements ----
std::shared_ptr<Statement> Parser::parse_statement(bool req_semi) {
  Token t = peek();
  std::shared_ptr<Statement> node;
  switch (t.type) {
  case TokenType::IF:
    node = parse_if_statement();
    req_semi = false;
    break;
  case TokenType::WHILE:
    node = parse_while_statement();
    req_semi = false;
    break;
  case TokenType::FOR:
    node = parse_for_statement();
    req_semi = false;
    break;
  case TokenType::RETURN:
    node = parse_return_statement();
    break;
  case TokenType::LBRACE:
    node = parse_block();
    req_semi = false;
    break;
  case TokenType::LET:
  case TokenType::CONST:
    node = parse_variable_declaration();
    break;
  default:
    node = parse_expression_statement();
  }
  if (req_semi)
    consume(TokenType::SEMICOLON);
  return node;
}

std::shared_ptr<Block> Parser::parse_block() {
  consume(TokenType::LBRACE);
  auto block = std::make_shared<Block>();
  try {
    while (peek().type != TokenType::RBRACE)
      block->statements.push_back(parse_statement());
    consume(TokenType::RBRACE);
  } catch (const ParseError &e) {
    synchronize();
    throw e;
  }
  return block;
}

std::shared_ptr<Expression> Parser::parse_expression(int min_prec) {
  auto left = parse_nud();
  while (true) {
    Token current = peek();
    int prec = get_precedence(current);
    if (prec < min_prec)
      break;
    if (!OP_TOKENS.count(current.type) && !POSTFIX_OPS.count(current.type))
      break;
    left = parse_led(left);
  }
  return left;
}

std::shared_ptr<Expression> Parser::parse_nud() {
  Token t = advance();
  switch (t.type) {
  case TokenType::NUMBER: {
    if (t.value.find('.') != std::string::npos)
      return std::make_shared<Literal>(std::stof(t.value),
                                       std::make_shared<F64>());
    else
      return std::make_shared<Literal>(std::stoi(t.value),
                                       std::make_shared<I32>());
  }
  case TokenType::TRUE:
    return std::make_shared<Literal>(true, std::make_shared<BOOL>());
  case TokenType::FALSE:
    return std::make_shared<Literal>(false, std::make_shared<BOOL>());
  case TokenType::LPAREN: {
    auto expr = parse_expression();
    consume(TokenType::RPAREN);
    return expr;
  }
  case TokenType::NULL_T:
    return std::make_shared<Literal>(
        0, std::make_shared<PointerType>(std::make_shared<U8>()));
  case TokenType::STRING:
    return std::make_shared<Literal>(
        t.value, std::make_shared<PointerType>(std::make_shared<U8>()));
  case TokenType::ID: {
    std::string name = t.value;
    if (declared_structs.count(name)) {
      consume(TokenType::LBRACE);
      std::map<std::string, std::shared_ptr<Expression>> field_values;
      while (peek().type != TokenType::RBRACE) {
        std::string fname = consume(TokenType::ID).value;
        consume(TokenType::COLON);
        field_values[fname] = parse_expression();
        if (peek().type == TokenType::COMMA)
          consume(TokenType::COMMA);
      }
      consume(TokenType::RBRACE);
      return std::make_shared<StructInitializer>(declared_structs[name],
                                                 field_values);
    } else if (match({TokenType::LPAREN})) {
      std::vector<std::shared_ptr<Expression>> args;
      if (peek().type != TokenType::RPAREN) {
        while (true) {
          args.push_back(parse_expression());
          if (!match({TokenType::COMMA}))
            break;
        }
      }
      consume(TokenType::RPAREN);
      return std::make_shared<FuncCall>(std::make_shared<VarAccess>(name),
                                        args);
    } else {
      return std::make_shared<VarAccess>(name);
    }
  }

  default:
    if (PREFIX_OPS.count(t.type)) {
      auto right = parse_expression(get_precedence(t) + 1);
      return std::make_shared<UnaryOperation>(t.value, right);
    }
    throw raiseError("Unexpected token in nud", t);
  }
}

std::shared_ptr<Expression>
Parser::parse_led(std::shared_ptr<Expression> left) {
  Token t = advance();
  if (OP_TOKENS.count(t.type)) {
    switch (t.type) {
    case TokenType::AS:
    case TokenType::RE: {
      auto target_type = parse_type();
      return std::make_shared<TypeCast>(
          left, target_type,
          t.type == TokenType::RE ? CastType::Reinterperet : CastType::Normal);
    }
    case TokenType::DOT: {
      if (match({TokenType::STAR})) {
        return std::make_shared<Dereference>(left);
      }
      std::string field_name = consume(TokenType::ID).value;
      return std::make_shared<FieldAccess>(left, field_name);
    }
    default:
      break;
    }
    auto right = parse_expression(get_precedence(t) + 1);
    return std::make_shared<BinaryOperation>(left, t.value, right);
  } else if (POSTFIX_OPS.count(t.type)) {
    switch (t.type) {
    case TokenType::LBRACKET: {
      auto index = parse_expression();
      consume(TokenType::RBRACKET);
      auto field_access = std::make_shared<OffsetAccess>(left, index);
      return field_access;
    }
    default:
      throw raiseError("Unexpected postfix operator", t);
    }
    throw raiseError("Unexpected token in led", t);
  }
  throw raiseError("Unexpected token in led", t);
}
int Parser::get_precedence(const Token &token) const {
  switch (token.type) {
  case TokenType::ASSIGN:
    return 1;
  case TokenType::AS:
    return 3;
  case TokenType::RE:
    return 3;
  case TokenType::EQ:
  case TokenType::NEQ:
  case TokenType::LT:
  case TokenType::LE:
  case TokenType::GT:
  case TokenType::GE:
    return 5;
  case TokenType::PLUS:
  case TokenType::MINUS:
    return 10;
  case TokenType::STAR:
  case TokenType::SLASH:
    return 20;
  case TokenType::CARET:
    return 30;
  case TokenType::DOT:
    return 50;
  case TokenType::LBRACKET:
    return 50;
  default:
    return -1;
  }
}

std::shared_ptr<IfStatement> Parser::parse_if_statement() {
  consume(TokenType::IF);
  consume(TokenType::LPAREN);
  auto condition = parse_expression();
  consume(TokenType::RPAREN);
  auto then_branch = parse_statement(false);
  std::shared_ptr<Statement> else_branch = nullptr;
  if (match({TokenType::ELSE}))
    else_branch = parse_statement(false);
  return std::make_shared<IfStatement>(condition, then_branch, else_branch);
}

std::shared_ptr<WhileStatement> Parser::parse_while_statement() {
  consume(TokenType::WHILE);
  consume(TokenType::LPAREN);
  auto condition = parse_expression();
  consume(TokenType::RPAREN);
  auto body = parse_statement(false);
  return std::make_shared<WhileStatement>(condition, body);
}

std::shared_ptr<ForStatement> Parser::parse_for_statement() {
  consume(TokenType::FOR);
  consume(TokenType::LPAREN);

  std::shared_ptr<Statement> init = nullptr;
  if (peek().type != TokenType::SEMICOLON) {
    // Parse variable declaration or expression statement
    if (peek().type == TokenType::LET) {
      init = parse_variable_declaration();
    } else {
      init = parse_expression_statement();
    }
  }
  consume(TokenType::SEMICOLON);

  std::shared_ptr<Expression> condition = nullptr;
  if (peek().type != TokenType::SEMICOLON) {
    condition = parse_expression();
  }
  consume(TokenType::SEMICOLON);

  std::shared_ptr<Statement> increment = nullptr;
  if (peek().type != TokenType::RPAREN) {
    increment = parse_expression_statement();
  }
  consume(TokenType::RPAREN);

  auto body = parse_statement();
  return std::make_shared<ForStatement>(init, condition, increment, body);
}

std::shared_ptr<ReturnStatement> Parser::parse_return_statement() {
  consume(TokenType::RETURN);
  auto value = parse_expression();
  return std::make_shared<ReturnStatement>(value);
}

std::shared_ptr<ExpressionStatement> Parser::parse_expression_statement() {
  auto expr = parse_expression();
  return std::make_shared<ExpressionStatement>(expr);
}

std::shared_ptr<FunctionDeclaration> Parser::parse_function_declaration() {
  consume(TokenType::FN);
  std::string name = consume(TokenType::ID).value;
  consume(TokenType::LPAREN);
  std::vector<std::pair<std::string, std::shared_ptr<Type>>> params;
  bool variadic = false;
  if (peek().type != TokenType::RPAREN) {
    while (true) {
      if (match({TokenType::TRIPLE_DOT})) {
        variadic = true;
        break;
      }
      std::string pname = consume(TokenType::ID).value;
      consume(TokenType::COLON);
      auto ptype = parse_type();
      params.push_back({pname, ptype});
      if (!match({TokenType::COMMA}))
        break;
    }
  }
  std::vector<std::string> param_names;
  std::vector<std::shared_ptr<Type>> param_types;
  for (auto &p : params) {
    param_names.push_back(p.first);
    param_types.push_back(p.second);
  }
  consume(TokenType::RPAREN);
  consume(TokenType::ARROW);
  auto ret_type = parse_type();

  auto func_type =
      std::make_shared<FunctionType>(param_types, ret_type, variadic);
  std::shared_ptr<Statement> body = nullptr;
  if (match({TokenType::SEMICOLON})) {
    body = nullptr;
  } else {
    body = parse_statement();
  }
  return std::make_shared<FunctionDeclaration>(name, func_type, param_names,
                                               body);
}

std::vector<std::pair<std::string, std::shared_ptr<Type>>>
Parser::parse_parameter_def() {
  std::vector<std::pair<std::string, std::shared_ptr<Type>>> params;
  if (peek().type != TokenType::RPAREN) {
    while (true) {
      std::string pname = consume(TokenType::ID).value;
      consume(TokenType::COLON);
      auto ptype = parse_type();
      params.push_back({pname, ptype});
      if (!match({TokenType::COMMA}))
        break;
    }
  }
  return params;
}

std::shared_ptr<Program> Parser::parse() {
  auto program = std::make_shared<Program>();
  while (peek().type != TokenType::EOF_T) {
    try {
      program->append(parse_declaration());
    } catch (const ParseError &e) {
      std::cerr << "Parse error: " << e.what() << "\n";
      synchronize();
    }
  }
  return program;
}

// ---- Top-level parse function ----
std::shared_ptr<Program> parse(const std::vector<Token> &tokens) {
  Parser p(tokens);
  return p.parse();
}
