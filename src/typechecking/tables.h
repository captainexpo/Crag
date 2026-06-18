#ifndef TABLES_H
#define TABLES_H

#include "../ast/ast.h"
#include <deque>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#define INVALID_SYMBOL_ID UINT32_MAX

// Symbol IDs are 64-bit integers where the upper 32 bits represent the module ID and the lower 32 bits represent the symbol index within that module's symbol table.
using SymbolId = uint64_t;

inline SymbolId extractModuleId(SymbolId id) {
    return id >> 32;
}

inline SymbolId extractSymbolIndex(SymbolId id) {
    return id & 0xFFFFFFFF;
}

inline SymbolId makeSymbolId(SymbolId module_id, SymbolId symbol_index) {
    if (module_id > 0xFFFFFFFF || symbol_index > 0xFFFFFFFF) {
        throw std::runtime_error("Module ID or Symbol Index exceeds 32 bits");
    }
    return (static_cast<SymbolId>(module_id) << 32) | symbol_index;
}

class Module;

enum class SymbolKind {
    Variable,
    Function,
    Type,
    Module,
};

struct Symbol {
    SymbolKind kind;
    std::string name;
    std::shared_ptr<Type> type;
    std::shared_ptr<ASTNode> decl;
    SymbolId id = INVALID_SYMBOL_ID;
};

class SymbolTable {
  public:
    SymbolId insert(Symbol symbol);

    std::optional<const Symbol> get(SymbolId id);

    std::optional<const Symbol> get(SymbolId id) const;

    bool remove(SymbolId id);

    const std::deque<Symbol> &entries() const;

    // Id for the module this table belongs to, set by the global symbol table when the module is inserted
    SymbolId table_id;

  private:
    std::deque<Symbol> symbols;
};

class GlobalSymbolTable {
  public:
    SymbolId insertModule(std::shared_ptr<Module> module);

    std::pair<SymbolTable, std::shared_ptr<Module>> *lookupModule(uint32_t module_id);

    std::optional<const Symbol> lookupSymbol(SymbolId id) const;

    const std::pair<SymbolTable, std::shared_ptr<Module>> *lookupModule(uint32_t module_id) const;

    std::unordered_map<uint32_t, std::pair<SymbolTable, std::shared_ptr<Module>>> modules;

    void dump() const;

  private:
};

#endif
