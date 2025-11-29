#pragma once
#include <string>
#include <unordered_map>
#include <vector>

enum class TokenType {
  // Literals & identifiers
  NUMBER,
  STRING,
  CHAR,
  ID,
  ATTRIBUTE,

  // Keywords
  FN,
  RETURN,
  IF,
  ELSE,
  WHILE,
  FOR,
  LET,
  CONST,
  STRUCT,
  ENUM,
  TRUE,
  FALSE,
  NULL_T,
  IMPORT,
  EXTERN,
  PUB,
  BREAK,
  CONTINUE,

  // Operators
  EQ,
  NEQ,
  LE,
  GE,
  LT,
  GT,
  PLUS,
  MINUS,
  STAR,
  CARET,
  SLASH,
  PERCENT,
  QUESTION,
  AND,
  OR,
  BANG,
  BAND,
  BOR,
  BXOR,
  SHL,
  SHR,
  ARROW,
  AS,
  RE,
  INC,
  DEC,
  BNOT,

  // Assignment operators
  ASSIGN,
  PLUS_ASSIGN,
  MINUS_ASSIGN,
  STAR_ASSIGN,
  SLASH_ASSIGN,
  PERCENT_ASSIGN,
  SHL_ASSIGN,
  SHR_ASSIGN,
  BAND_ASSIGN,
  BOR_ASSIGN,
  BXOR_ASSIGN,

  // Delimiters
  TRIPLE_DOT,
  DOUBLE_COLON,
  LPAREN,
  RPAREN,
  LBRACE,
  RBRACE,
  LBRACKET,
  RBRACKET,
  COMMA,
  COLON,
  DOT,
  SEMICOLON,

  // Misc
  SKIP,
  EOF_T
};

std::string tokenTypeName(TokenType type);

struct Token {
  TokenType type;
  std::string value;
  int line;
  int column;
};

class Lexer {
public:
  explicit Lexer(const std::string &src);
  std::vector<Token> tokenize();

private:
  std::string code;
  size_t pos = 0;
  int line = 1;
  int col = 1;

  std::unordered_map<std::string, TokenType> keywords;

  char peek(int lookahead = 0) const;
  char get();
  bool match(const std::string &s);
};
