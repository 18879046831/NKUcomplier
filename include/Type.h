#ifndef __TYPE_H__
#define __TYPE_H__
#include <string>
#include <vector>
#include "SymbolTable.h"

class Type {
   private:
    int kind;

   protected:
    enum { INT, VOID, FUNC, PTR, ARRAY, STRING };
    int size;//数组中总元素所占空间

   public:
    Type(int kind, int size = 0) : kind(kind), size(size){};
    virtual ~Type(){};
    virtual std::string toStr() = 0;
    bool isInt() const { return kind == INT; };
    bool isVoid() const { return kind == VOID; };
    bool isFunc() const { return kind == FUNC; };
    bool isPtr() const { return kind == PTR; };
    bool isArray() const { return kind == ARRAY; };
    bool isString() const { return kind == STRING; };
    int getKind() const { return kind; };
    int getSize() const { return size; };
};

class IntType : public Type {
   private:
    bool constant;

   public:
    IntType(int size, bool constant = false)
        : Type(Type::INT, size), constant(constant){};
    std::string toStr();
    bool isConst() const { return constant; };
};

class VoidType : public Type {
   public:
    VoidType() : Type(Type::VOID){};
    std::string toStr();
};

class StringType : public Type {
   private:
    int length;

   public:
    StringType(int length) : Type(Type::STRING), length(length){};
    int getLength() const { return length; };
    std::string toStr();
};

class FunctionType : public Type {//函数类型
   private:
    Type* returnType;//返回值类型
    std::vector<Type*> paramsType;//参数类型
    std::vector<SymbolEntry*> paramsSe;//参数的SymbolEntry

   public:
    FunctionType(Type* returnType,
                 std::vector<Type*> paramsType,
                 std::vector<SymbolEntry*> paramsSe)
        : Type(Type::FUNC),
          returnType(returnType),
          paramsType(paramsType),
          paramsSe(paramsSe){};
    void setParamsType(std::vector<Type*> paramsType) {
        this->paramsType = paramsType;
    };
    std::vector<Type*> getParamsType() { return paramsType; };
    std::vector<SymbolEntry*> getParamsSe() { return paramsSe; };
    Type* getRetType() { return returnType; };
    std::string toStr();
};

class ArrayType : public Type {
   private:
    Type* elementType;//高维的下一维的元素类型
    Type* arrayType = nullptr;
    //int a(array_type[2](array_type[3](array_type[4])))最上层的array_type类型
    int length;//数组的本维长度
    bool constant;//是否是常量数组

   public:
    ArrayType(Type* elementType, int length, bool constant = false)
        : Type(Type::ARRAY),
          elementType(elementType),
          length(length),//这一维数组的元素个数
          constant(constant) {
        size = elementType->getSize() * length;//数组的总大小
    };
    std::string toStr();
    int getLength() const { return length; };
    Type* getElementType() const { return elementType; };
    void setArrayType(Type* arrayType) { this->arrayType = arrayType; };
    bool isConst() const { return constant; };
    Type* getArrayType() const { return arrayType; };
};

class PointerType : public Type {
   private:
    Type* valueType;

   public:
    PointerType(Type* valueType) : Type(Type::PTR) {
        this->valueType = valueType;
    };
    std::string toStr();
    Type* getType() const { return valueType; };
};

class TypeSystem {
   private:
    static IntType commonInt;
    static IntType commonBool;
    static VoidType commonVoid;
    static IntType commonConstInt;

   public:
    static Type* intType;
    static Type* voidType;
    static Type* boolType;
    static Type* constIntType;
};

#endif
