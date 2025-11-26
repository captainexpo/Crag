#include "lexer.h"
#include <cctype>
#include <iostream>
#include <unordered_map>

std::string tokenTypeName(TokenType type) {
  switch (type) {
  case TokenType::NUMBER:
    return "NUMBER";
  case TokenType::STRING:
    return "STRING";
  case TokenType::CHAR:
    return "CHAR";
  case TokenType::ID:
    return "ID";
  case TokenType::ATTRIBUTE:
    return "ATTRIBUTE";
  case TokenType::FN:
    return "FN";
  case TokenType::RETURN:
    return "RETURN";
  case TokenType::IF:
    return "IF";
  case TokenType::ELSE:
    return "ELSE";
  case TokenType::WHILE:
    return "WHILE";
  case TokenType::FOR:
    return "FOR";
  case TokenType::LET:
    return "LET";
  case TokenType::CONST:
    return "CONST";
  case TokenType::STRUCT:
    return "STRUCT";
  case TokenType::ENUM:
    return "ENUM";
  case TokenType::TRUE:
    return "TRUE";
  case TokenType::FALSE:
    return "FALSE";
  case TokenType::NULL_T:
    return "NULL_T";
  case TokenType::IMPORT:
    return "IMPORT";
  case TokenType::EXTERN:
    return "EXTERN";
  case TokenType::PUB:
    return "PUB";
  case TokenType::BREAK:
    return "BREAK";
  case TokenType::CONTINUE:
    return "CONTINUE";
  case TokenType::QUESTION:
    return "QUESTION";
  case TokenType::EQ:
    return "EQ";
  case TokenType::NEQ:
    return "NEQ";
  case TokenType::LE:
    return "LE";
  case TokenType::GE:
    return "GE";
  case TokenType::LT:
    return "LT";
  case TokenType::GT:
    return "GT";
  case TokenType::PLUS:
    return "PLUS";
  case TokenType::MINUS:
    return "MINUS";
  case TokenType::STAR:
    return "STAR";
  case TokenType::CARET:
    return "CARET";
  case TokenType::SLASH:
    return "SLASH";
  case TokenType::PERCENT:
    return "PERCENT";
  case TokenType::AND:
    return "AND";
  case TokenType::OR:
    return "OR";
  case TokenType::BANG:
    return "NOT";
  case TokenType::BAND:
    return "BAND";
  case TokenType::BOR:
    return "BOR";
  case TokenType::BXOR:
    return "BXOR";
  case TokenType::SHL:
    return "SHL";
  case TokenType::SHR:
    return "SHR";
  case TokenType::ASSIGN:
    return "ASSIGN";
  case TokenType::PLUS_ASSIGN:
    return "PLUS_ASSIGN";
  case TokenType::MINUS_ASSIGN:
    return "MINUS_ASSIGN";
  case TokenType::STAR_ASSIGN:
    return "STAR_ASSIGN";
  case TokenType::SLASH_ASSIGN:
    return "SLASH_ASSIGN";
  case TokenType::PERCENT_ASSIGN:
    return "PERCENT_ASSIGN";
  case TokenType::ARROW:
    return "ARROW";
  case TokenType::TRIPLE_DOT:
    return "TRIPLE_DOT";
  case TokenType::LPAREN:
    return "LPAREN";
  case TokenType::RPAREN:
    return "RPAREN";
  case TokenType::LBRACE:
    return "LBRACE";
  case TokenType::RBRACE:
    return "RBRACE";
  case TokenType::LBRACKET:
    return "LBRACKET";
  case TokenType::RBRACKET:
    return "RBRACKET";
  case TokenType::COMMA:
    return "COMMA";
  case TokenType::COLON:
    return "COLON";
  case TokenType::DOT:
    return "DOT";
  case TokenType::SEMICOLON:
    return "SEMICOLON";
  case TokenType::SKIP:
    return "SKIP";
  case TokenType::EOF_T:
    return "EOF_T";
  case TokenType::AS:
    return "AS";
  case TokenType::RE:
    return "RE";
  default:
    return "UNKNOWN";
  }
}
Lexer::Lexer(const std::string &src) : code(src) {
  keywords = {
      {"fn", TokenType::FN},
      {"return", TokenType::RETURN},
      {"if", TokenType::IF},
      {"else", TokenType::ELSE},
      {"while", TokenType::WHILE},
      {"for", TokenType::FOR},
      {"let", TokenType::LET},
      {"const", TokenType::CONST},
      {"struct", TokenType::STRUCT},
      {"enum", TokenType::ENUM},
      {"true", TokenType::TRUE},
      {"false", TokenType::FALSE},
      {"null", TokenType::NULL_T},
      {"as", TokenType::AS},
      {"re", TokenType::RE},
      {"import", TokenType::IMPORT},
      {"extern", TokenType::EXTERN},
      {"pub", TokenType::PUB},
      {"break", TokenType::BREAK},
      {"continue", TokenType::CONTINUE},
  };
}

char Lexer::peek() const { return pos < code.size() ? code[pos] : '\0'; }

char Lexer::get() {
  char c = peek();
  pos++;
  if (c == '\n') {
    line++;
    col = 1;
  } else
    col++;
  return c;
}

bool Lexer::match(const std::string &s) {
  if (code.substr(pos, s.size()) == s) {
    for (size_t i = 0; i < s.size(); i++)
      get();
    return true;
  }
  return false;
}

std::string convertStringEscapeCodes(const std::string &s) {
  std::string result;
  for (size_t i = 0; i < s.size(); i++) {
    if (s[i] == '\\' && i + 1 < s.size()) {
      i++;
      switch (s[i]) {
      case 'n':
        result += '\n';
        break;
      case 't':
        result += '\t';
        break;
      case 'r':
        result += '\r';
        break;
      case '\\':
        result += '\\';
        break;
      case '"':
        result += '"';
        break;
      case '\'':
        result += '\'';
        break;
      default:
        result += s[i];
        break;
      }
    } else {
      result += s[i];
    }
  }
  return result;
}

std::vector<Token> Lexer::tokenize() {
  std::vector<Token> tokens;
  while (pos < code.size()) {
    char c = peek();
    int token_line = line;
    int token_col = col;

    // Skip whitespace
    if (isspace(c)) {
      get();
      continue;
    }

    // Skip comments
    if (match("//")) {
      while (peek() != '\n' && peek() != '\0')
        get();
      continue;
    }
    if (match("/*")) {
      while (!match("*/") && peek() != '\0')
        get();
      continue;
    }

    // Numbers
    if (isdigit(c)) {
      std::string val;
      while (isdigit(peek()) || peek() == '.')
        val += get();
      tokens.push_back({TokenType::NUMBER, val, token_line, token_col});
      continue;
    }

    // Identifiers / keywords
    if (isalpha(c) || c == '_') {
      std::string val;
      while (isalnum(peek()) || peek() == '_')
        val += get();
      TokenType type = keywords.count(val) ? keywords[val] : TokenType::ID;

      tokens.push_back({type, val, token_line, token_col});
      continue;
    }

    if (c == '@') {
      get(); // consume '@'
      std::string val;
      while (isalnum(peek()) || peek() == '_')
        val += get();
      tokens.push_back({TokenType::ATTRIBUTE, val, token_line, token_col});
      continue;
    }

    // Strings
    if (c == '"') {
      get();
      std::string val;
      while (peek() != '"' && peek() != '\0') {
        if (peek() == '\\') {
          val += get();
          val += get();
        } else
          val += get();
      }
      get(); // consume closing "
      val = convertStringEscapeCodes(val);
      tokens.push_back({TokenType::STRING, val, token_line, token_col});
      continue;
    }

    // Chars
    if (c == '\'') {
      get();
      std::string val;
      if (peek() == '\\') {
        val += get();
        val += get();
      } else
        val += get();
      get(); // consume closing '
      tokens.push_back({TokenType::CHAR, val, token_line, token_col});
      continue;
    }

    // Operators and delimiters
    if (match("=="))
      tokens.push_back({TokenType::EQ, "==", token_line, token_col});
    else if (match("!="))
      tokens.push_back({TokenType::NEQ, "!=", token_line, token_col});
    else if (match("<="))
      tokens.push_back({TokenType::LE, "<=", token_line, token_col});
    else if (match(">="))
      tokens.push_back({TokenType::GE, ">=", token_line, token_col});
    else if (match("<<"))
      tokens.push_back({TokenType::SHL, "<<", token_line, token_col});
    else if (match(">>"))
      tokens.push_back({TokenType::SHR, ">>", token_line, token_col});
    else if (match("&&"))
      tokens.push_back({TokenType::AND, "&&", token_line, token_col});
    else if (match("||"))
      tokens.push_back({TokenType::OR, "||", token_line, token_col});
    else if (match("+="))
      tokens.push_back({TokenType::PLUS_ASSIGN, "+=", token_line, token_col});
    else if (match("-="))
      tokens.push_back({TokenType::MINUS_ASSIGN, "-=", token_line, token_col});
    else if (match("*="))
      tokens.push_back({TokenType::STAR_ASSIGN, "*=", token_line, token_col});
    else if (match("/="))
      tokens.push_back({TokenType::SLASH_ASSIGN, "/=", token_line, token_col});
    else if (match("%="))
      tokens.push_back({TokenType::PERCENT_ASSIGN, "%=", token_line, token_col});
    else if (match("->"))
      tokens.push_back({TokenType::ARROW, "->", token_line, token_col});
    else if (match("..."))
      tokens.push_back({TokenType::TRIPLE_DOT, "...", token_line, token_col});
    else if (match("::"))
      tokens.push_back({TokenType::DOUBLE_COLON, "::", token_line, token_col});
    else {
      char op = get();
      switch (op) {
      case '?':
        tokens.push_back({TokenType::QUESTION, "?", token_line, token_col});
        break;
      case '=':
        tokens.push_back({TokenType::ASSIGN, "=", token_line, token_col});
        break;
      case '+':
        tokens.push_back({TokenType::PLUS, "+", token_line, token_col});
        break;
      case '-':
        tokens.push_back({TokenType::MINUS, "-", token_line, token_col});
        break;
      case '*':
        tokens.push_back({TokenType::STAR, "*", token_line, token_col});
        break;
      case '/':
        tokens.push_back({TokenType::SLASH, "/", token_line, token_col});
        break;
      case '%':
        tokens.push_back({TokenType::PERCENT, "%", token_line, token_col});
        break;
      case '<':
        tokens.push_back({TokenType::LT, "<", token_line, token_col});
        break;
      case '>':
        tokens.push_back({TokenType::GT, ">", token_line, token_col});
        break;
      case '!':
        tokens.push_back({TokenType::BANG, "!", token_line, token_col});
        break;
      case '&':
        tokens.push_back({TokenType::BAND, "&", token_line, token_col});
        break;
      case '|':
        tokens.push_back({TokenType::BOR, "|", token_line, token_col});
        break;
      case '^':
        tokens.push_back({TokenType::BXOR, "^", token_line, token_col});
        break;
      case '(':
        tokens.push_back({TokenType::LPAREN, "(", token_line, token_col});
        break;
      case ')':
        tokens.push_back({TokenType::RPAREN, ")", token_line, token_col});
        break;
      case '{':
        tokens.push_back({TokenType::LBRACE, "{", token_line, token_col});
        break;
      case '}':
        tokens.push_back({TokenType::RBRACE, "}", token_line, token_col});
        break;
      case '[':
        tokens.push_back({TokenType::LBRACKET, "[", token_line, token_col});
        break;
      case ']':
        tokens.push_back({TokenType::RBRACKET, "]", token_line, token_col});
        break;
      case ',':
        tokens.push_back({TokenType::COMMA, ",", token_line, token_col});
        break;
      case ':':
        tokens.push_back({TokenType::COLON, ":", token_line, token_col});
        break;
      case '.':
        tokens.push_back({TokenType::DOT, ".", token_line, token_col});
        break;
      case ';':
        tokens.push_back({TokenType::SEMICOLON, ";", token_line, token_col});
        break;
      default:
        std::cerr << "Unexpected character: " << op << " at line " << token_line
                  << "\n";
        break;
      }
    }
  }
  tokens.push_back({TokenType::EOF_T, "", line, col});
  return tokens;
}
