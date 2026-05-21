#include <string>
#include <memory>
#include <unordered_map>
#include <vector>
#include "../ast/ast.h"

#ifndef TABLES_H
#define TABLES_H

#define INVALID_SYMBOL_ID UINT32_MAX

using SymbolId = uint32_t;

struct Symbol {
    std::string name;
    std::shared_ptr<Type> type;
    std::shared_ptr<ASTNode> decl;
};

class SymbolTable {
public:
    SymbolId insert(Symbol symbol) {
        SymbolId id = symbols.size();

        symbols.push_back(std::move(symbol));

        name_lookup[symbols[id].name] = id;

        return id;
    }

    SymbolId lookup(const std::string& name) const {
        auto it = name_lookup.find(name);

        if (it == name_lookup.end())
            return INVALID_SYMBOL_ID;

        return it->second;
    }

    Symbol* get(SymbolId id) {
        if (id >= symbols.size())
            return nullptr;

        return &symbols[id];
    }

    const Symbol* get(SymbolId id) const {
        if (id >= symbols.size())
            return nullptr;

        return &symbols[id];
    }

    const std::vector<Symbol> &entries() const {
        return symbols;
    }

private:
    std::vector<Symbol> symbols;

    std::unordered_map<std::string, SymbolId> name_lookup;
};
#endif
