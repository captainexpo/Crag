#include "parser.h"
#include "ast.h"
#include "lexer.h"
#include <algorithm>
#include <cstddef>
#include <iostream>
#include <llvm/IR/Intrinsics.h>
#include <memory>
#include <unordered_map>
#include <vector>

Parser::Parser(const std::vector<Token> &tokens)
    : tokens(tokens), position(0) {}

void Parser::error(const std::string &msg, const Token &token, bool throw_now_and_exit) {
    std::vector<Token> context_tokens;
    int start = std::max(0, (int)position - 3);
    int end = std::min((int)tokens.size(), (int)(position + 3));
    for (int i = start; i < end; ++i)
        context_tokens.push_back(tokens[i]);
    m_errors.push_back(ParseError(msg, token.line, token.column));

    if (throw_now_and_exit)
        throw ParseError(msg, token.line, token.column);
}

// ---- Token helpers ----
Token Parser::peek(int lookahead) const {
    if (position + lookahead < tokens.size() + lookahead)
        return tokens[position + lookahead];
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
    throw ParseError("Expected " + tokenTypeName(expected_type) +
                         ", got " + tokenTypeName(t.type),
                     t.line, t.column);
    return Token{TokenType::EOF_T, "", -1, -1};
}

void Parser::synchronize() {
    advance();
    while (peek().type != TokenType::EOF_T) {
        switch (peek().type) {
            case TokenType::FN:
            //case TokenType::LET:
            //case TokenType::CONST:
            case TokenType::USING:
            case TokenType::STRUCT:
            case TokenType::ENUM:
            case TokenType::IMPORT:
            case TokenType::EXTERN:
            //case TokenType::RETURN:
            //case TokenType::IF:
            //case TokenType::FOR:
            //case TokenType::WHILE:
            //case TokenType::BREAK:
            //case TokenType::CONTINUE:
            case TokenType::PUB:
                return;
        }
        advance();
    }
}

bool str_in_vector(const std::string &s, const std::vector<std::string> &vec, size_t* index) {
    auto it = std::find(vec.begin(), vec.end(), s);
    if (it != vec.end()) {
        if (index != nullptr) {
            *index = std::distance(vec.begin(), it);
        }
        return true;
    }
    return false;
}

// ---- Type parsing ----
std::shared_ptr<Type> Parser::parse_type(bool top_level) {
    Token current = peek();
    std::shared_ptr<Type> t = nullptr;
    switch (current.type) {
        case TokenType::ID:
            if (declared_structs.count(current.value)) {
                std::string name = consume(TokenType::ID).value;
                t = declared_structs[name];
                break;
            }
            if (declared_unions.count(current.value)) {
                std::string name = consume(TokenType::ID).value;
                t = declared_unions[name];
                break;
            }
            if (declared_enums.count(current.value)) {
                std::string name = consume(TokenType::ID).value;
                t = declared_enums[name]->enum_type;
                break;
            }
            if (declared_type_aliases.count(current.value)) {
                std::string name = consume(TokenType::ID).value;
                t = declared_type_aliases[name];
                break;
            }
            if (str_in_vector(current.value, current_generic_params, nullptr)) {
                std::string name = consume(TokenType::ID).value;
                t = std::make_shared<GenericType>(name);
                break;
            }
            if (peek(1).type == TokenType::DOUBLE_COLON) {
                std::vector<std::string> path;
                path.push_back(consume(TokenType::ID).value);
                while (match({TokenType::DOUBLE_COLON})) {
                    path.push_back(consume(TokenType::ID).value);
                }
                auto b = path.back();
                path.pop_back();
                t = std::make_shared<QualifiedType>(path, b);
                break;
            }
            t = parse_primitive_type();
            break;
        case TokenType::STAR:
            t = parse_pointer_type();
            break;
        case TokenType::LBRACKET:
            t = parse_array_type();
            break;
        case TokenType::FN:
            t = parse_function_ptr_type();
            break;
        default:
            throw ParseError("Unexpected token in type", current.line, current.column);
    }
    if (top_level && peek().type == TokenType::QUESTION) {
        consume(TokenType::QUESTION);
        throw ParseError("Nullable types not supported yet", current.line, current.column);
        t->nullable = true;
    }
    if (peek().type == TokenType::BANG) {
        consume(TokenType::BANG);
        auto error_type = parse_type();
        return std::make_shared<ErrorUnionType>(t, error_type);
    }
    return t;
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
        return std::make_shared<Boolean>();
    if (val == "void")
        return std::make_shared<Void>();
    throw ParseError("Unknown primitive type " + val, peek().line,
                     peek().column);
}

std::shared_ptr<PointerType> Parser::parse_pointer_type() {
    consume(TokenType::STAR);
    if (peek().type == TokenType::CONST) {
        consume(TokenType::CONST);
        return std::make_shared<PointerType>(parse_type(false), true);
    }
    return std::make_shared<PointerType>(parse_type(false));
}

std::shared_ptr<ArrayType> Parser::parse_array_type() {
    consume(TokenType::LBRACKET);

    std::shared_ptr<Expression> size = nullptr;
    bool unsized = peek().type == TokenType::RBRACKET;
    if (!unsized) {
        size = parse_expression();

        // HACK: should probably put this in the typechecker
        size->inferred_type = std::make_shared<I64>();    
    }
    consume(TokenType::RBRACKET);
    auto elem_type = parse_type(false);
    auto arrTy = std::make_shared<ArrayType>(elem_type, size, unsized);
    return arrTy;
}

std::shared_ptr<PointerType> Parser::parse_function_ptr_type() {
    consume(TokenType::FN);
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
    return std::make_shared<PointerType>(
        std::make_shared<FunctionType>(param_types, ret_type, variadic));
}

// ---- Declarations ----
std::shared_ptr<ASTNode> Parser::parse_declaration() {
    Token t = peek();
    bool is_pub = match({TokenType::PUB});
    t = peek();
    switch (t.type) {
        case TokenType::FN: {
            auto fn = parse_function_declaration();
            fn->is_pub = is_pub;
            return fn;
        }
        case TokenType::LET:
        case TokenType::CONST: {
            auto vd = parse_variable_declaration();
            consume(TokenType::SEMICOLON);
            return vd;
        }
        case TokenType::USING: {
            // Type alias 
            consume(TokenType::USING);
            std::string alias_name = consume(TokenType::ID).value;
            consume(TokenType::ASSIGN);
            auto alias_type = parse_type();
            consume(TokenType::SEMICOLON);
            auto type_alias = std::make_shared<TypeAliasDeclaration>(alias_name, alias_type);
            type_alias->is_pub = is_pub;
            declared_type_aliases[alias_name] = alias_type;
            return type_alias;
        }
        case TokenType::STRUCT: {
            auto sd = parse_struct_declaration();
            sd->is_pub = is_pub;
            return sd;
        }
        case TokenType::UNION: {
            auto ud = parse_union_declaration();
            ud->is_pub = is_pub;
            return ud;
        }
        case TokenType::ENUM: {
            auto ed = parse_enum_declaration();
            ed->is_pub = is_pub;
            return ed;
        }
        case TokenType::IMPORT:
            return parse_import_declaration();
        case TokenType::EXTERN: {
            auto ed = parse_extern_declaration();
            ed->is_pub = is_pub;
            return ed;
        }
        default:
            throw ParseError("Unexpected token in declaration", t.line,
                             t.column);
    }
}

std::shared_ptr<Declaration> Parser::parse_extern_declaration() {
    consume(TokenType::EXTERN);
    switch (peek().type) {
        case TokenType::LET:
        case TokenType::CONST: {
            auto var_decl = parse_variable_declaration();
            var_decl->is_extern = true;
            consume(TokenType::SEMICOLON);
            return var_decl;
        }

        case TokenType::STRUCT:
        case TokenType::ENUM:
            break;
        case TokenType::FN: {
            auto fn = parse_function_declaration();
            fn->is_extern = true;
            return fn;
        }
        default:
            break;
    }
    throw ParseError("Only function extern declarations are supported", peek().line,
                     peek().column);
}

std::shared_ptr<ImportDeclaration> Parser::parse_import_declaration() {
    consume(TokenType::IMPORT);
    std::string path = consume(TokenType::STRING).value;
    consume(TokenType::AS);
    std::string alias = consume(TokenType::ID).value;

    imported_modules.insert(alias);

    return std::make_shared<ImportDeclaration>(path, alias);
}

std::shared_ptr<VariableDeclaration> Parser::parse_variable_declaration() {
    Token start_token = peek(); // Store the start token for line and col
    bool is_const = match({TokenType::CONST});
    if (!is_const)
        consume(TokenType::LET);
    std::string name = consume(TokenType::ID).value;
    std::shared_ptr<Type> var_type = nullptr;
    if (!(peek().type == TokenType::SEMICOLON || peek().type == TokenType::ASSIGN)) {
        consume(TokenType::COLON);
        var_type = parse_type();
    }
    std::shared_ptr<ASTNode> initializer = nullptr;
    if (match({TokenType::ASSIGN}))
        initializer = parse_expression();
    auto var_decl =
        std::make_shared<VariableDeclaration>(name, var_type, initializer);
    var_decl->is_const = is_const;
    var_decl->line = start_token.line;
    var_decl->col = start_token.column;
    return var_decl;
}

std::shared_ptr<EnumDeclaration> Parser::parse_enum_declaration() {
    std::shared_ptr<Type> enum_type = std::make_shared<I32>();
    Token start_token = peek(); // Store the start token for line and col
    consume(TokenType::ENUM);
    std::string name = consume(TokenType::ID).value;
    if (match({TokenType::LPAREN})) {
        enum_type = parse_type();
        consume(TokenType::RPAREN);
    }
    consume(TokenType::LBRACE);
    std::unordered_map<std::string, std::shared_ptr<Literal>> variant_map;
    std::shared_ptr<Literal> last_literal = nullptr;
    while (peek().type != TokenType::RBRACE) {
        if (peek().type == TokenType::EOF_T) {
            throw ParseError("Unterminated enum declaration, expected '}'", peek().line, peek().column);
            break;
        }
        std::string vname = consume(TokenType::ID).value;
        if (match({TokenType::ASSIGN})) {
            auto val = parse_expression();
            if (auto lit = std::dynamic_pointer_cast<Literal>(val)) {
                lit->lit_type = enum_type;
                variant_map.insert({vname, lit});
            } else {
                throw ParseError("Enum variant value must be a literal", peek().line, peek().column);
            }
        } else {
            // Auto-assign value
            if (!last_literal) {
                throw ParseError("First enum variant must have an explicit value", peek());
                continue;
            }
            if (auto ilit = std::dynamic_pointer_cast<Literal>(last_literal)) {
                if (auto ival = std::get_if<int64_t>(&ilit->value)) {
                    auto new_lit = std::make_shared<Literal>((*ival) + 1, enum_type);
                    variant_map.insert({vname, new_lit});
                    last_literal = new_lit;
                } else {
                    throw ParseError("Enum auto-assigned values must be integers", peek());
                }
            } else {
                throw ParseError("Enum auto-assigned values must be integers", peek());
            }
        }
        last_literal = variant_map[vname];
        if (peek().type == TokenType::COMMA)
            consume(TokenType::COMMA);
    }
    consume(TokenType::RBRACE);
    auto enum_decl = std::make_shared<EnumDeclaration>(name, enum_type, variant_map);
    enum_decl->enum_type = std::make_shared<EnumType>(name, enum_type);
    enum_decl->line = start_token.line;
    enum_decl->col = start_token.column;
    declared_enums[name] = enum_decl;
    return enum_decl;
}
std::shared_ptr<StructDeclaration> Parser::parse_struct_declaration() {
    Token start_token = peek(); // Store the start token for line and col
    consume(TokenType::STRUCT);
    std::string name = consume(TokenType::ID).value;
    declared_structs[name] = std::make_shared<StructType>(
        name, std::vector<std::pair<std::string, std::shared_ptr<Type>>>{});
    consume(TokenType::LBRACE);
    std::vector<std::pair<std::string, std::shared_ptr<Type>>> fields;
    std::unordered_map<std::string, std::shared_ptr<FunctionDeclaration>> methods;
    while (peek().type != TokenType::RBRACE) {
        if (peek().type == TokenType::EOF_T) {
            throw ParseError("Unterminated struct declaration, expected '}'", peek());
            break;
        }
        if (peek().type == TokenType::FN) {
            std::shared_ptr<FunctionDeclaration> method =
                parse_function_declaration();
            methods.insert({method->name, method});
            continue;
        }
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
    auto st = std::make_shared<StructType>(name, fmap);
    st->methods = methods;
    declared_structs[name] = st;
    auto struct_decl = std::make_shared<StructDeclaration>(name, fields);
    struct_decl->methods = methods;
    struct_decl->line = start_token.line;
    struct_decl->col = start_token.column;
    return struct_decl;
}

std::shared_ptr<UnionDeclaration> Parser::parse_union_declaration() {
    Token start_token = peek(); // Store the start token for line and col
    consume(TokenType::UNION);
    std::string name = consume(TokenType::ID).value;
    declared_unions[name] = std::make_shared<UnionType>(
        name, std::vector<std::pair<std::string, std::shared_ptr<Type>>>{});
    consume(TokenType::LBRACE);
    std::vector<std::pair<std::string, std::shared_ptr<Type>>> fields;
    
    while (peek().type != TokenType::RBRACE) {
        if (peek().type == TokenType::EOF_T) {
            throw ParseError("Unterminated union declaration, expected '}'", peek());
            break;
        }
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
    auto ut = std::make_shared<UnionType>(name, fmap);
    declared_unions[name] = ut;
    
    auto union_decl = std::make_shared<UnionDeclaration>(name, fields);
    union_decl->line = start_token.line;
    union_decl->col = start_token.column;
    return union_decl;
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
        case TokenType::BREAK:
            node = std::make_shared<BreakStatement>();
            consume(TokenType::BREAK);
            break;
        case TokenType::CONTINUE:
            node = std::make_shared<ContinueStatement>();
            consume(TokenType::CONTINUE);
            break;
        default:
            node = parse_expression_statement();
    }
    if (req_semi)
        consume(TokenType::SEMICOLON);
    return node;
}

std::shared_ptr<Block> Parser::parse_block() {
    Token start_token = peek(); // Store the start token for line and col
    consume(TokenType::LBRACE);
    auto block = std::make_shared<Block>();
    block->line = start_token.line;
    block->col = start_token.column;
    while (peek().type != TokenType::RBRACE && peek().type != TokenType::EOF_T) {
        try {
            block->statements.push_back(parse_statement());
            if (peek().type == TokenType::EOF_T) {
                throw ParseError("Unterminated block, expected '}'", peek());
                break;
            }
        } catch (const ParseError &e) {
            m_errors.push_back(e);
            synchronize();
        }
    }
    consume(TokenType::RBRACE);
    return block;
}

std::shared_ptr<Expression> Parser::parse_expression(int min_prec) {
    auto left = parse_nud();
    while (true) {
        Token current = peek();
        int prec = get_precedence(current, false);
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
    std::shared_ptr<Expression> expr;

    switch (t.type) {
        case TokenType::NUMBER: {
            if (t.value[1] == 'x') {
                // Hexadecimal
                expr = std::make_shared<Literal>(std::stoull(t.value, nullptr, 16),
                                                 std::make_shared<U64>());
            } else if (t.value[1] == 'b') {
                // Binary
                expr = std::make_shared<Literal>(std::stoull(t.value.substr(2), nullptr, 2),
                                                 std::make_shared<U64>());
            } else if (t.value.find('.') != std::string::npos) {
                expr = std::make_shared<Literal>(std::stof(t.value),
                                                 std::make_shared<F64>());
            } else {
                expr = std::make_shared<Literal>(std::stol(t.value),
                                                 std::make_shared<I32>());
            }
            expr->line = t.line;
            expr->col = t.column;
            return expr;
        }
        case TokenType::TRUE: {
            expr = std::make_shared<Literal>(true, std::make_shared<Boolean>());
            expr->line = t.line;
            expr->col = t.column;
            return expr;
        }
        case TokenType::FALSE: {
            expr = std::make_shared<Literal>(false, std::make_shared<Boolean>());
            expr->line = t.line;
            expr->col = t.column;
            return expr;
        }
        case TokenType::LPAREN: {
            auto expr = parse_expression();
            consume(TokenType::RPAREN);
            return expr;
        }
        case TokenType::NULL_T: {
            expr = std::make_shared<Literal>(0, std::make_shared<NullType>());
            expr->line = t.line;
            expr->col = t.column;
            return expr;
        }
        case TokenType::STRING: {
            auto ty = std::make_shared<PointerType>(
                std::make_shared<U8>(), false);
            auto lit = std::make_shared<Literal>(
                t.value, ty);
            expr = lit;
            expr->line = t.line;
            expr->col = t.column;
            return expr;
        }
        case TokenType::CHAR: {
            if (t.value.length() != 1)
                throw ParseError("Invalid char literal", t);
            expr = std::make_shared<Literal>(
                (uint64_t)(t.value[0]), std::make_shared<U8>());
            expr->line = t.line;
            expr->col = t.column;
            return expr;
        }
        case TokenType::ID: {

            std::string name = t.value;
            if (peek().type == TokenType::DOUBLE_COLON) {
                if (peek(1).type == TokenType::LT) {
                    // Template instantiation
                    std::vector<std::shared_ptr<Type>> type_args;
                    consume(TokenType::DOUBLE_COLON);
                    consume(TokenType::LT);
                    while (true) {
                        type_args.push_back(parse_type());
                        if (peek().type == TokenType::COMMA)
                            consume(TokenType::COMMA);
                        else
                            break;
                    }
                    consume(TokenType::GT);
                    expr = std::make_shared<TemplateInstantiation>(name, type_args);
                    expr->line = t.line;
                    expr->col = t.column;
                    return expr;
                }
                // Module access
                std::vector<std::string> module_path;
                module_path.push_back(name);
                consume(TokenType::DOUBLE_COLON);
                while (true) {
                    std::string part = consume(TokenType::ID).value;
                    module_path.push_back(part);
                    if (!match({TokenType::DOUBLE_COLON}))
                        break;
                }
                std::string member_name = module_path.back();
                module_path.pop_back();
                expr = std::make_shared<ModuleAccess>(module_path, member_name);
                expr->line = t.line;
                expr->col = t.column;
            }
            else {
                expr = std::make_shared<VarAccess>(name);
                expr->line = t.line;
                expr->col = t.column;
            }
            // Check for struct initializer
            if (peek().type == TokenType::LBRACE) {
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
                expr = std::make_shared<StructInitializer>(expr,
                                                           field_values);
                expr->line = t.line;
                expr->col = t.column;
            }
            return expr;
        }
        case TokenType::LBRACE: {
            // Array literal
            std::vector<std::shared_ptr<Expression>> elements;
            if (peek().type != TokenType::RBRACE) {
                while (true) {
                    elements.push_back(parse_expression());
                    if (!match({TokenType::COMMA}))
                        break;
                }
            }
            consume(TokenType::RBRACE);
            expr = std::make_shared<ArrayLiteral>(elements);
            expr->line = t.line;
            expr->col = t.column;
            return expr;
        }
        default:
            if (PREFIX_OPS.count(t.type)) {
                auto right = parse_expression(get_precedence(t, false) + 1);
                expr = std::make_shared<UnaryOperation>(t.value, right);
                expr->line = t.line;
                expr->col = t.column;
                return expr;
            }
            throw ParseError("Unexpected token in expression: '" + t.value + "'", t.line, t.column);
    }
}

std::shared_ptr<Expression>
Parser::parse_led(std::shared_ptr<Expression> left) {
    Token t = advance();
    std::shared_ptr<Expression> expr;

    if (OP_TOKENS.count(t.type)) {
        switch (t.type) {
            case TokenType::AS:
            case TokenType::RE: {
                auto target_type = parse_type();
                expr = std::make_shared<TypeCast>(
                    left, target_type,
                    t.type == TokenType::RE ? CastType::Reinterperet : CastType::Normal);
                expr->line = t.line;
                expr->col = t.column;
                return expr;
            }
            case TokenType::DOT: {
                if (match({TokenType::STAR})) {
                    expr = std::make_shared<Dereference>(left);
                    expr->line = t.line;
                    expr->col = t.column;
                    return expr;
                }
                std::string field_name = consume(TokenType::ID).value;
                expr = std::make_shared<FieldAccess>(left, field_name);
                expr->line = t.line;
                expr->col = t.column;
                return expr;
            }
            // case TokenType::DOUBLE_COLON: {
            // }
            case TokenType::COLON: {
                std::string variant_name = consume(TokenType::ID).value;
                expr = std::make_shared<EnumAccess>(left, variant_name);
                expr->line = t.line;
                expr->col = t.column;
                return expr;
            }
            default:
                break;
        }
        auto right = parse_expression(get_precedence(t, false) + 1);
        if (t.value == "+=" || t.value == "-=" || t.value == "*=" ||
            t.value == "/=" || t.value == "%=" || t.value == "&=" ||
            t.value == "|=" || t.value == "^=" || t.value == "<<=" ||
            t.value == ">>=") {
            // HACK: convert to regular op and assign (probably fine for now, but I can't be bothered to deal with the "right" way. KISS)
            auto reg_op = t.value.substr(0, t.value.length() - 1);
            auto bin_op = std::make_shared<BinaryOperation>(
                left, reg_op, right);
            bin_op->line = t.line;
            bin_op->col = t.column;
            expr = std::make_shared<BinaryOperation>(
                left, "=", bin_op);
            expr->line = t.line;
            expr->col = t.column;
            return expr;
        }
        expr = std::make_shared<BinaryOperation>(left, t.value, right);
        expr->line = t.line;
        expr->col = t.column;
        return expr;
    } else if (POSTFIX_OPS.count(t.type)) {
        switch (t.type) {
            case TokenType::LBRACKET: {
                auto index = parse_expression();
                consume(TokenType::RBRACKET);
                expr = std::make_shared<OffsetAccess>(left, index);
                expr->line = t.line;
                expr->col = t.column;
                return expr;
            }
            case TokenType::LPAREN: {
                std::vector<std::shared_ptr<Expression>> args;
                if (peek().type != TokenType::RPAREN) {
                    while (true) {
                        args.push_back(parse_expression());
                        if (!match({TokenType::COMMA}))
                            break;
                    }
                }
                consume(TokenType::RPAREN);
                if (auto fa = std::dynamic_pointer_cast<FieldAccess>(left)) {
                    expr = std::make_shared<MethodCall>(std::dynamic_pointer_cast<Expression>(fa->base), fa->field, args);
                } else {
                    expr = std::make_shared<FuncCall>(left, args);
                }
                expr->line = t.line;
                expr->col = t.column;
                return expr;
            }
            default:
                throw ParseError("Unexpected postfix operator", t);
        }
    }
    throw ParseError("Unexpected token in led", t.line, t.column);
}
int Parser::get_precedence(const Token &token, bool postfix) const {
    if (ASSIGN_OPS.count(token.type))
        return 1;
    switch (token.type) {
        case TokenType::AS:
        case TokenType::RE:
            return 2;
        case TokenType::OR:
            return 3;
        case TokenType::AND:
            return 4;
        case TokenType::BOR:
            return 5;
        case TokenType::BXOR:
            return 6;
        case TokenType::SHL:
        case TokenType::SHR:
            return 7;
        case TokenType::BAND:
            return 8;
        case TokenType::EQ:
        case TokenType::NEQ:
        case TokenType::LT:
        case TokenType::LE:
        case TokenType::GT:
        case TokenType::GE:
            return 9;
        case TokenType::PLUS:
        case TokenType::MINUS:
            return 10;
        case TokenType::STAR:
        case TokenType::SLASH:
        case TokenType::PERCENT:
            return 20;
        case TokenType::CARET:
            return 30;
        case TokenType::BANG:
        case TokenType::BNOT:
            return 40;
        case TokenType::INC:
        case TokenType::DEC:
            return postfix ? 50 : 40;
        case TokenType::DOT:
            return 50;
        case TokenType::COLON:
            return 50;
        case TokenType::DOUBLE_COLON:
            return 50;
        case TokenType::LBRACKET:
            return 50;
        case TokenType::LPAREN:
            return 50;
        default:
            return -1;
    }
}

std::shared_ptr<IfStatement> Parser::parse_if_statement() {
    Token start_token = peek(); // Store the start token for line and col
    consume(TokenType::IF);
    consume(TokenType::LPAREN);
    auto condition = parse_expression();
    consume(TokenType::RPAREN);
    auto then_branch = parse_statement(peek().type != TokenType::LBRACE);
    std::shared_ptr<Statement> else_branch = nullptr;
    if (match({TokenType::ELSE}))
        else_branch = parse_statement(peek().type != TokenType::LBRACE);

    auto if_stmt =
        std::make_shared<IfStatement>(condition, then_branch, else_branch);
    if_stmt->line = start_token.line;
    if_stmt->col = start_token.column;
    return if_stmt;
}

std::shared_ptr<WhileStatement> Parser::parse_while_statement() {
    Token start_token = peek(); // Store the start token for line and col
    consume(TokenType::WHILE);
    consume(TokenType::LPAREN);
    auto condition = parse_expression();
    consume(TokenType::RPAREN);
    auto body = parse_statement(peek().type != TokenType::LBRACE);

    auto while_stmt = std::make_shared<WhileStatement>(condition, body);
    while_stmt->line = start_token.line;
    while_stmt->col = start_token.column;
    return while_stmt;
}

std::shared_ptr<ForStatement> Parser::parse_for_statement() {
    Token start_token = peek(); // Store the start token for line and col
    consume(TokenType::FOR);
    consume(TokenType::LPAREN);

    std::shared_ptr<Statement> init = nullptr;
    if (peek().type != TokenType::SEMICOLON) {
        init = parse_statement(false);
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

    auto body = parse_statement(peek().type != TokenType::LBRACE);
    auto for_stmt =
        std::make_shared<ForStatement>(init, condition, increment, body);
    for_stmt->line = start_token.line;
    for_stmt->col = start_token.column;
    return for_stmt;
}

std::shared_ptr<ReturnStatement> Parser::parse_return_statement() {
    Token start_token = peek(); // Store the start token for line and col
    consume(TokenType::RETURN);
    bool is_error_return = false;
    if (peek().type == TokenType::BANG) {
        consume(TokenType::BANG);
        is_error_return = true;
    }
    if (peek().type == TokenType::SEMICOLON) {
        auto return_stmt = std::make_shared<ReturnStatement>(nullptr);
        return_stmt->line = start_token.line;
        return_stmt->col = start_token.column;
        return_stmt->is_error = is_error_return;
        return return_stmt;
    }
    auto value = parse_expression();
    auto return_stmt = std::make_shared<ReturnStatement>(value);
    return_stmt->line = start_token.line;
    return_stmt->col = start_token.column;
    return_stmt->is_error = is_error_return;
    return return_stmt;
}

std::shared_ptr<ExpressionStatement> Parser::parse_expression_statement() {
    Token start_token = peek(); // Store the start token for position
    auto expr = parse_expression();
    auto expr_stmt = std::make_shared<ExpressionStatement>(expr);
    expr_stmt->line = start_token.line;
    expr_stmt->col = start_token.column;
    return expr_stmt;
}

std::vector<std::string> Parser::try_parse_generic_parameters() {
    std::vector<std::string> generic_params;
    if (match({TokenType::LT})) {
        while (true) {
            std::string gpname = consume(TokenType::ID).value;
            generic_params.push_back(gpname);
            if (match({TokenType::COMMA}))
                continue;
            consume(TokenType::GT);
            break;
        }
    }
    return generic_params;
}

std::shared_ptr<FunctionDeclaration> Parser::parse_function_declaration() {
    Token start_token = peek(); // Store the start token for line and col
    consume(TokenType::FN);
    std::string name = consume(TokenType::ID).value;
    std::vector<std::string> generic_params = try_parse_generic_parameters();
    this->current_generic_params = generic_params;

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
    std::set<std::string> attributes;
    while (peek().type == TokenType::ATTRIBUTE) {
        std::string attr_name = consume(TokenType::ATTRIBUTE).value;
        attributes.insert(attr_name);
    }
    std::shared_ptr<Statement> body = nullptr;
    if (match({TokenType::SEMICOLON})) {
        body = nullptr;
    } else {
        body = parse_statement();
    }
    auto func_decl =
        std::make_shared<FunctionDeclaration>(name, func_type, param_names, body);
    func_decl->line = start_token.line;
    func_decl->col = start_token.column;
    func_decl->attributes = attributes;
    func_decl->generic_params = generic_params;

    this->current_generic_params.clear();
    return func_decl;
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
    // Set line and col to 1,1 as it's the root of the AST
    program->line = 1;
    program->col = 1;

    while (peek().type != TokenType::EOF_T) {
        try {
            program->append(parse_declaration());
        } catch (const ParseError &e) {
            m_errors.push_back(e);
            synchronize();
        }
    }
    return program;
}
