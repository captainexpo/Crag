#include "tables.h"
#include <iostream>
#include "../module.h"

SymbolId SymbolTable::insert(Symbol symbol) {
    SymbolId id = makeSymbolId(table_id, symbols.size());
    symbol.id = id;

    symbols.push_back(std::move(symbol));

    return id;
}

std::optional<const Symbol> SymbolTable::get(SymbolId id) {
    id = extractSymbolIndex(id); // Ensure we only use the symbol index part of the ID
    if (id >= symbols.size())
        return std::nullopt;

    return symbols[id];
}

std::optional<const Symbol> SymbolTable::get(SymbolId id) const {
    id = extractSymbolIndex(id); // Ensure we only use the symbol index part of the ID
    if (id >= symbols.size())
        return std::nullopt;

    return symbols[id];
}

const std::deque<Symbol> &SymbolTable::entries() const {
    return symbols;
}

bool SymbolTable::remove(SymbolId id) {
    id = extractSymbolIndex(id); // Ensure we only use the symbol index part of the ID
    if (id >= symbols.size())
        return false;

    // TODO: Actually remove from vector and update indices
    // Mark as removed
    symbols[id].name.clear();
    symbols[id].type = nullptr;
    symbols[id].decl = nullptr;

    return true;
}

SymbolId GlobalSymbolTable::insertModule(std::shared_ptr<Module> module) {
    SymbolId id = module->id;

    modules[id] = {SymbolTable(), module};
    modules[id].first.table_id = id;

    return id;
}

void GlobalSymbolTable::dump() const {
    std::cerr << "Global symbol table contents:\n";
    for (const auto &[module_id, pair] : modules) {
        const auto &[table, module] = pair;
        std::cerr << "Module " << module->canon_name << " (ID: " << module_id << "):\n";
        for (int i = 0; i < table.entries().size(); ++i) {
            const auto &entry = table.entries()[i];
            std::cerr << "  " << entry.name << " (" << module_id << ", " << i << ", " << entry.id << ", " << &entry << "): " << (entry.type ? entry.type->str() : "null") << "\n";
        }
    }
}

std::pair<SymbolTable, std::shared_ptr<Module>> *GlobalSymbolTable::lookupModule(uint32_t module_id) {
    auto it = modules.find(module_id);

    if (it == modules.end())
        return nullptr;

    return &it->second;
}

const std::pair<SymbolTable, std::shared_ptr<Module>> *GlobalSymbolTable::lookupModule(uint32_t module_id) const {
    auto it = modules.find(module_id);

    if (it == modules.end())
        return nullptr;

    return &it->second;
}

std::optional<const Symbol> GlobalSymbolTable::lookupSymbol(SymbolId id) const {
    uint32_t module_id = extractModuleId(id);
    auto mod = lookupModule(module_id);
    if (!mod) {
        return std::nullopt;
    }
    const auto &[table, module] = *mod;
    std::optional<Symbol> s = table.get(id);
    if (!s) {
        return std::nullopt;
    }
    return *s;
}

