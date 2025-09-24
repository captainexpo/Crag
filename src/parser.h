
#ifndef PARSER_H
#define PARSER_H

#include "ast.h"
#include "lexer.h"

#include <map>
#include <memory>
#include <set>
#include <stdexcept>
#include <string>
#include <vector>

class ParseError : public std::exception {
  std::string message;

public:
  ParseError(const std::string &msg, const Token &token,
             const std::vector<Token> &context_tokens) {
    message = "[line " + std::to_string(token.line) + ", col " +
              std::to_string(token.column) + "] " + msg + "\n";
    message += "Token: '" + token.value + "' (type " +
               tokenTypeName(token.type) + ")\n";
    message += "Context:\n";
    for (size_t i = 0; i < context_tokens.size(); ++i) {
      message += "  " + tokenTypeName(context_tokens[i].type) + " '" +
                 context_tokens[i].value + "'";
      if (context_tokens[i].value == token.value)
        message += " <- here";
      message += "\n";
    }
  }

  const char *what() const noexcept override { return message.c_str(); }
};

class Parser {
public:
  Parser(const std::vector<Token> &tokens);

  std::shared_ptr<Program> parse();

private:
  std::vector<Token> tokens;
  size_t position;

  std::map<std::string, std::shared_ptr<StructType>> declared_structs;

  // Helper methods
  Token peek() const;
  Token advance();
  bool match(const std::set<TokenType> &types);
  Token consume(TokenType expected_type);
  void synchronize();

  ParseError raiseError(const std::string &msg, const Token &token);

  // Parsing methods
  std::shared_ptr<Type> parse_type();
  std::shared_ptr<Type> parse_primitive_type();
  std::shared_ptr<Type> parse_pointer_type();
  std::shared_ptr<Type> parse_array_type();
  std::shared_ptr<FunctionType> parse_function_type();
  std::vector<std::pair<std::string, std::shared_ptr<Type>>>
  parse_parameter_def();

  std::shared_ptr<ASTNode> parse_declaration();
  std::shared_ptr<FunctionDeclaration> parse_function_declaration();
  std::shared_ptr<StructDeclaration> parse_struct_declaration();
  std::shared_ptr<VariableDeclaration> parse_variable_declaration();

  std::shared_ptr<Statement> parse_statement(bool req_semi = true);
  std::shared_ptr<Block> parse_block();
  std::shared_ptr<IfStatement> parse_if_statement();
  std::shared_ptr<WhileStatement> parse_while_statement();
  std::shared_ptr<ForStatement> parse_for_statement();
  std::shared_ptr<ReturnStatement> parse_return_statement();
  std::shared_ptr<ExpressionStatement> parse_expression_statement();

  std::shared_ptr<Expression> parse_expression(int min_prec = 0);
  std::shared_ptr<Expression> parse_nud();
  std::shared_ptr<Expression> parse_led(std::shared_ptr<Expression> left);
  int get_precedence(const Token &token) const;

  // Operator categories
  const std::set<TokenType> OP_TOKENS = {
      TokenType::ASSIGN, TokenType::PLUS,  TokenType::MINUS, TokenType::STAR,
      TokenType::SLASH,  TokenType::CARET, TokenType::EQ,    TokenType::NEQ,
      TokenType::LT,     TokenType::LE,    TokenType::GT,    TokenType::GE,
      TokenType::DOT,    TokenType::AS,    TokenType::RE};

  const std::set<TokenType> PREFIX_OPS = {TokenType::BAND, TokenType::STAR,
                                          TokenType::MINUS, TokenType::NOT};

  const std::set<TokenType> POSTFIX_OPS = {TokenType::LBRACKET};
};

std::shared_ptr<Program> parse(const std::vector<Token> &tokens);

#endif // PARSER_H
