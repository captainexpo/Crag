
#ifndef PARSER_H
#define PARSER_H

#include "ast.h"
#include "lexer.h"

#include <map>
#include <memory>
#include <set>
#include <stdexcept>
#include <string>
#include <unordered_set>
#include <vector>

class ParseError : public std::exception {
public:
  std::string message;
  int line;
  int col;

  ParseError(std::string m, int l, int c) : message(m), line(l), col(c) {}
  ParseError(std::string m, const Token &t)
      : message(m), line(t.line), col(t.column) {}
  const char *what() const noexcept override { return message.c_str(); }
};

class Parser {
public:
  Parser(const std::vector<Token> &tokens);

  std::shared_ptr<Program> parse();
  bool ok() const { return m_errors.empty(); }
  const std::vector<ParseError> &errors() const {
    return m_errors;
  }

private:
  std::vector<ParseError> m_errors; // Collected errors

  std::vector<Token> tokens;
  size_t position;

  std::map<std::string, std::shared_ptr<StructType>> declared_structs;
  std::map<std::string, std::shared_ptr<EnumDeclaration>> declared_enums;

  std::unordered_set<std::string> imported_modules;

  // Helper methods
  Token peek() const;
  Token advance();
  bool match(const std::set<TokenType> &types);
  Token consume(TokenType expected_type);
  void synchronize();

  void error(const std::string &msg, const Token &token, bool throw_now_and_exit = false);

  // Parsing methods
  std::shared_ptr<Type> parse_type(bool top_level = true);
  std::shared_ptr<Type> parse_primitive_type();
  std::shared_ptr<Type> parse_pointer_type();
  std::shared_ptr<Type> parse_array_type();
  std::shared_ptr<PointerType> parse_function_ptr_type();
  std::vector<std::pair<std::string, std::shared_ptr<Type>>>
  parse_parameter_def();

  std::shared_ptr<ASTNode> parse_declaration();
  std::shared_ptr<FunctionDeclaration> parse_function_declaration();
  std::shared_ptr<StructDeclaration> parse_struct_declaration();
  std::shared_ptr<EnumDeclaration> parse_enum_declaration();
  std::shared_ptr<VariableDeclaration> parse_variable_declaration();
  std::shared_ptr<ImportDeclaration> parse_import_declaration();

  std::shared_ptr<Declaration> parse_extern_declaration();

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
      TokenType::ASSIGN,
      TokenType::PLUS_ASSIGN,
      TokenType::MINUS_ASSIGN,
      TokenType::STAR_ASSIGN,
      TokenType::SLASH_ASSIGN,
      TokenType::PERCENT_ASSIGN,
      TokenType::PLUS,
      TokenType::MINUS,
      TokenType::STAR,
      TokenType::SLASH,
      TokenType::CARET,
      TokenType::PERCENT,
      TokenType::EQ,
      TokenType::NEQ,
      TokenType::LT,
      TokenType::LE,
      TokenType::GT,
      TokenType::GE,
      TokenType::DOT,
      TokenType::AS,
      TokenType::RE,
      TokenType::DOUBLE_COLON,
  };

  const std::set<TokenType> PREFIX_OPS = {TokenType::BAND, TokenType::STAR,
                                          TokenType::MINUS, TokenType::BANG};

  const std::set<TokenType> POSTFIX_OPS = {TokenType::LBRACKET,
                                           TokenType::LPAREN};

  const std::set<TokenType> ASSIGN_OPS = {
      TokenType::ASSIGN,
      TokenType::PLUS_ASSIGN,
      TokenType::MINUS_ASSIGN,
      TokenType::STAR_ASSIGN,
      TokenType::SLASH_ASSIGN
  };
};

#endif // PARSER_H
