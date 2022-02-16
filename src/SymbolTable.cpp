#include "SymbolTable.h"
#include <iostream>
#include <sstream>
#include "Type.h"

SymbolEntry::SymbolEntry(Type* type, int kind) {
    this->type = type;
    this->kind = kind;
}

ConstantSymbolEntry::ConstantSymbolEntry(Type* type, int value)
    : SymbolEntry(type, SymbolEntry::CONSTANT) {
    assert(type->isInt());
    this->value = value;
}

ConstantSymbolEntry::ConstantSymbolEntry(Type* type, std::string value)
    : SymbolEntry(type, SymbolEntry::CONSTANT) {
    assert(type->isString());
    this->strValue = value;
}

ConstantSymbolEntry::ConstantSymbolEntry(Type* type)
    : SymbolEntry(type, SymbolEntry::CONSTANT) {
}

int ConstantSymbolEntry::getValue() const {
    assert(type->isInt());
    return value;
}

std::string ConstantSymbolEntry::getStrValue() const {
    assert(type->isString());
    return strValue;
}

std::string ConstantSymbolEntry::toStr() {
    std::ostringstream buffer;
    if (type->isInt())
        buffer << value;
    else if (type->isString())
        buffer << strValue;
    return buffer.str();
}

IdentifierSymbolEntry::IdentifierSymbolEntry(Type* type,
    std::string name,
    int scope,
    int param_num,
    bool sysy)
    : SymbolEntry(type, SymbolEntry::VARIABLE),
    name(name),
    sysy(sysy),
    param_num(param_num) {
    this->scope = scope;
    this->initial = false;
    this->label = -1;
    this->allZero = false;
    this->constant = false;
}

void IdentifierSymbolEntry::setValue(int value) {
    this->value = value;
}

void IdentifierSymbolEntry::setArrayValue(int* arrayValue) {
    this->arrayValue = arrayValue;
}

std::string IdentifierSymbolEntry::toStr() {
    std::ostringstream buffer;
    if (label < 0) {
        if (type->isFunc())
            buffer << '@';
        buffer << name;
    }
    else
        buffer << "%t" << label;
    return buffer.str();
}

TemporarySymbolEntry::TemporarySymbolEntry(Type* type, int label)
    : SymbolEntry(type, SymbolEntry::TEMPORARY) {
    this->label = label;
}

std::string TemporarySymbolEntry::toStr() {
    std::ostringstream buffer;
    buffer << "%t" << label;
    return buffer.str();
}

SymbolTable::SymbolTable() {
    prev = nullptr;
    level = 0;
}

SymbolTable::SymbolTable(SymbolTable* prev) {
    this->prev = prev;
    this->level = prev->level + 1;
}

/*
    Description: lookup the symbol entry of an identifier in the symbol table
    Parameters:
        name: identifier name
    Return: pointer to the symbol entry of the identifier
    hint:
    1. The symbol table is a stack. The top of the stack contains symbol entries
   in the current scope.
    2. Search the entry in the current symbol table at first.
    3. If it's not in the current table, search it in previous ones(along the
   'prev' link).
    4. If you find the entry, return it.
    5. If you can't find it in all symbol tables, return nullptr.
*/
SymbolEntry* SymbolTable::lookup(std::string name) {
    SymbolTable* tab = this;
    if (!tab)
        return nullptr;
    if (tab->symbolTable.find(name) != tab->symbolTable.end()) {
        return tab->symbolTable[name];
    }
    else {
        return tab->getPrev()->lookup(name);
    }
    return nullptr;
}

// install the entry into current symbol table.
void SymbolTable::install(std::string name, SymbolEntry* entry) {
    symbolTable[name] = entry;
    return;
}
int SymbolTable::counter = 0;
static SymbolTable t;
SymbolTable* identifiers = &t;
SymbolTable* globals = &t;