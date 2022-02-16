%code top{
    #include <iostream>
    #include <assert.h>
    #include "parser.h"
    #include <cstring>
    #include <stack>
    extern Ast ast;

    int yylex();
    int yyerror(char const*);
    ArrayType* arrayType;//存储数组类型
    int value_index;//数组中元素的索引
    int* arrayValue;//存储数组中的值
    std::stack<InitValueListExpr*> Expvalue_stack;//存储用于数值初始化的表达式
    std::stack<StmtNode*> whileStk;//while的嵌套情况应用于break
    InitValueListExpr* top;
    int Wait_distrib = 0;//多维数组的大括号嵌套深度，多层嵌套，每嵌套一层表示我们要分配的initValueListExpr个数
    int paramNo = 0;//记录参数的个数
    #include <iostream>
}

%code requires {
    #include "Ast.h"
    #include "SymbolTable.h"
    #include "Type.h"
}

%union {
    int itype;
    char* strtype;
    StmtNode* stmttype;
    ExprNode* exprtype;
    Type* type;
    SymbolEntry* se;
}

%start Program
%token <strtype> ID STRING
%token <itype> INTEGER
%token IF ELSE WHILE
%token INT VOID
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON LBRACKET RBRACKET COMMA  
%token ADD SUB MUL DIV MOD OR AND LESS LESSEQUAL GREATER GREATEREQUAL ASSIGN EQUAL NOTEQUAL NOT
%token CONST
%token RETURN CONTINUE BREAK

%type<stmttype> Stmts Stmt AssignStmt ExprStmt BlockStmt IfStmt WhileStmt BreakStmt
%type<stmttype> ContinueStmt ReturnStmt DeclStmt FuncDef ConstDeclStmt VarDeclStmt
%type<stmttype> ConstDefList VarDef ConstDef VarDefList FuncFParam FuncFParams OptFuncFParams BlankStmt
%type<exprtype> Exp AddExp Cond LOrExp PrimaryExp LVal RelExp LAndExp MulExp FuncCall
%type<exprtype> EqExp UnaryExp InitVal InitValList FuncArray Func_RealPars Func_RealPar Array
%type<type> Type

%precedence THEN
%precedence ELSE
%%
Program
    : Stmts {
        ast.setRoot($1);
    }
    ;
Stmts
    : Stmt {$$=$1;}
    | Stmts Stmt{
        $$ = new SeqNode($1, $2);
    }
    ;
Stmt
    : AssignStmt {$$=$1;}
    | BlockStmt {$$=$1;}
    | IfStmt {$$=$1;}
    | ReturnStmt {$$=$1;}
    | DeclStmt {$$=$1;}
    | FuncDef {$$=$1;}
    | ExprStmt {$$=$1;}
    | BlankStmt {$$=$1;}
    | WhileStmt {$$=$1;}
    | BreakStmt {$$=$1;}
    | ContinueStmt {$$=$1;}
    ;
LVal
    : ID {
        SymbolEntry* se;
        se = identifiers->lookup($1);
        $$ = new Id(se);
        delete []$1;
    }
    | ID Array{
        SymbolEntry* se;
        se = identifiers->lookup($1);
        $$ = new Id(se, $2);
        delete []$1;
    }
    ; 
AssignStmt
    : LVal ASSIGN Exp SEMICOLON {
        $$ = new AssignStmt($1, $3);
    }
    ;
BlockStmt
    : LBRACE {
        identifiers = new SymbolTable(identifiers);
    } 
      Stmts RBRACE {
        $$ = new CompoundStmt($3);
        SymbolTable* top = identifiers;
        identifiers = identifiers->getPrev();
        delete top;
    }
    | LBRACE RBRACE {
        $$ = new CompoundStmt();
    }
    ;
IfStmt
    : IF LPAREN Cond RPAREN Stmt %prec THEN {
        $$ = new IfStmt($3, $5);
    }
    | IF LPAREN Cond RPAREN Stmt ELSE Stmt {
        $$ = new IfElseStmt($3, $5, $7);
    }
    ;
ReturnStmt
    : RETURN SEMICOLON {
        $$ = new ReturnStmt();
    }
    | RETURN Exp SEMICOLON {
        $$ = new ReturnStmt($2);
    }
    ;
DeclStmt
    : VarDeclStmt {$$ = $1;}
    | ConstDeclStmt {$$ = $1;}
    ;
ExprStmt
    : Exp SEMICOLON {
        $$ = new ExprStmt($1);
    }
    ;
BlankStmt
    : SEMICOLON {
        $$ = new BlankStmt();
    }
    ;
WhileStmt
    : WHILE LPAREN Cond RPAREN {
        WhileStmt *whileNode = new WhileStmt($3);
        $<stmttype>$ = whileNode;//嵌入语义，将stmttype细化到whileNode类型
        whileStk.push(whileNode);//将当前whileNode节点压入保留栈
    }
    Stmt {
        StmtNode *whileNode = $<stmttype>5; 
        ((WhileStmt*)whileNode)->setStmt($6);
        $$=whileNode;
        whileStk.pop();//将当前whileNode节点弹出保留栈
    }
    ;
BreakStmt
    : BREAK SEMICOLON {
        $$ = new BreakStmt(whileStk.top());//取当前所在的whileNode节点并将其保留在BREAK语句中
    }
    ;
ContinueStmt
    : CONTINUE SEMICOLON {
        $$ = new ContinueStmt(whileStk.top());//取当前所在的whileNode节点并将其保留在CONTINUE语句中
    }
    ;
Exp
    :
    AddExp {$$ = $1;}
    ;
Cond
    :
    LOrExp {$$ = $1;}
    ;
PrimaryExp
    : LPAREN Exp RPAREN {
        $$ = $2;
    }
    | LVal {
        $$ = $1;
    }
    | STRING {
        SymbolEntry* se;
        se = globals->lookup(std::string($1));
        if(se == nullptr){
            Type* type = new StringType(strlen($1));
            se = new ConstantSymbolEntry(type, std::string($1));
            globals->install(std::string($1), se);
        }
        ExprNode* expr = new ExprNode(se);

        $$ = expr;
    }
    | INTEGER {
        SymbolEntry* se = new ConstantSymbolEntry(TypeSystem::intType, $1);
        $$ = new Constant(se);
    }
    ;
FuncCall
    :
    ID LPAREN Func_RealPars RPAREN {
        SymbolEntry* se;
        se = identifiers->lookup($1);
        $$ = new CallExpr(se, $3);
    }
    | ID LPAREN RPAREN {
        SymbolEntry* se;
        se = identifiers->lookup($1);
        $$ = new CallExpr(se);
    }
    ;
UnaryExp 
    : PrimaryExp {$$ = $1;}
    | FuncCall  {$$ = $1;}
    | ADD UnaryExp {$$ = $2;}
    | SUB UnaryExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new UnaryExpr(se, UnaryExpr::SUB, $2);
    }
    | NOT UnaryExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new UnaryExpr(se, UnaryExpr::NOT, $2);
    }
    ;
MulExp
    : UnaryExp {$$ = $1;}
    | MulExp MUL UnaryExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::MUL, $1, $3);
    }
    | MulExp DIV UnaryExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::DIV, $1, $3);
    }
    | MulExp MOD UnaryExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::MOD, $1, $3);
    }
    ;
AddExp
    : MulExp {$$ = $1;}
    | AddExp ADD MulExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::ADD, $1, $3);
    }
    | AddExp SUB MulExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::intType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::SUB, $1, $3);
    }
    ;
RelExp
    : AddExp {
        $$ = $1;
    }
    | RelExp LESS AddExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::LESS, $1, $3);
    }
    | RelExp LESSEQUAL AddExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::LESSEQUAL, $1, $3);
    }
    | RelExp GREATER AddExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::GREATER, $1, $3);
    }
    | RelExp GREATEREQUAL AddExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::GREATEREQUAL, $1, $3);
    }
    ;
EqExp
    : RelExp {$$ = $1;}
    | EqExp EQUAL RelExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::EQUAL, $1, $3);
    }
    | EqExp NOTEQUAL RelExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::NOTEQUAL, $1, $3);
    }
    ;
LAndExp
    : EqExp {$$ = $1;}
    | LAndExp AND EqExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::AND, $1, $3);
    }
    ;
LOrExp
    : LAndExp {$$ = $1;}
    | LOrExp OR LAndExp {
        SymbolEntry* se = new TemporarySymbolEntry(TypeSystem::boolType, SymbolTable::getLabel());
        $$ = new BinaryExpr(se, BinaryExpr::OR, $1, $3);
    }
    ;
Func_RealPars 
    : Func_RealPar {$$ = $1;}
    | Func_RealPars COMMA Func_RealPar {
        $$ = $1;
        $$->setNext($3);
    }
    ;
Func_RealPar
    :Exp {$$=$1;}
    ;
Type
    : INT {
        $$ = TypeSystem::intType;
    }
    | VOID {
        $$ = TypeSystem::voidType;
    }
    ;
VarDeclStmt
    : Type VarDefList SEMICOLON {$$ = $2;}
    ;
ConstDeclStmt
    : CONST Type ConstDefList SEMICOLON {
        $$ = $3;
    }
    ;
VarDefList
    : VarDefList COMMA VarDef {
        $$ = $1;
        $1->setNext($3);
    } 
    | VarDef {$$ = $1;}
    ;
ConstDefList
    : ConstDefList COMMA ConstDef {
        $$ = $1;
        $1->setNext($3);
    }
    | ConstDef {$$ = $1;}
    ;
VarDef
    : ID {
        SymbolEntry* se;
        se = new IdentifierSymbolEntry(TypeSystem::intType, $1, identifiers->getLevel());
        identifiers->install($1, se);
        $$ = new DeclStmt(new Id(se));
        delete []$1;
    }
    | ID Array {//数组定义
        SymbolEntry* se;
        std::vector<int> vec_dimension;//存储数组中的那些维度值信息
        ExprNode* temp_dimension = $2;//使用temp节点
        while(temp_dimension){//循环把数组维度值压入
            vec_dimension.push_back(temp_dimension->getValue());
            temp_dimension = (ExprNode*)(temp_dimension->getNext());
        }
        //迭代来构造数组类型
        Type *type = TypeSystem::intType;//多维数组中的最后一维开始，其类型为int
        Type* temp_array;
        while(!vec_dimension.empty()){//维度不为空的时候
            temp_array = new ArrayType(type, vec_dimension.back());//取维度值和数组迭代类型构造ArrayType
            if(type->isArray())
                ((ArrayType*)type)->setArrayType(temp_array);
            type = temp_array;//低维构造完后，修改type用于构造上维的arraytype
            vec_dimension.pop_back();
        }
        se = new IdentifierSymbolEntry(type, $1, identifiers->getLevel());
        ((IdentifierSymbolEntry*)se)->setAllZero();//未初始化的全部设置为0
        int *p = new int[type->getSize()];//数组的总大小
        ((IdentifierSymbolEntry*)se)->setArrayValue(p);//用于存储数组的值
        identifiers->install($1, se);
        $$ = new DeclStmt(new Id(se));
        delete []$1;
    }
    | ID ASSIGN InitVal {
        SymbolEntry* se;
        se = new IdentifierSymbolEntry(TypeSystem::intType, $1, identifiers->getLevel());
        identifiers->install($1, se);
        ((IdentifierSymbolEntry*)se)->setValue($3->getValue());
        $$ = new DeclStmt(new Id(se), $3);
        delete []$1;
    }
    | ID Array ASSIGN {
        SymbolEntry* se;
        std::vector<int> vec_dimension;//存储数组中的那些维度值信息
        ExprNode* temp_dimension = $2;//使用temp节点
        while(temp_dimension){//循环把数组维度值压入
            vec_dimension.push_back(std::move(temp_dimension->getValue()));
            temp_dimension = (ExprNode*)(temp_dimension->getNext());
        }
        //迭代来构造数组类型
        Type *type = TypeSystem::intType;//多维数组中的最后一维开始，其类型为int
        Type* temp_array;
        while(!vec_dimension.empty()){//维度不为空的时候
            temp_array = new ArrayType(type, vec_dimension.back());//取维度值和数组迭代类型构造ArrayType
            if(type->isArray())
                ((ArrayType*)type)->setArrayType(temp_array);
            type = temp_array;//低维构造完后，修改type用于构造上维的arraytype
            vec_dimension.pop_back();
        }
        arrayType = (ArrayType*)type;
        value_index = 0;//索引置0
        std::stack<InitValueListExpr*>().swap(Expvalue_stack);//栈清空
        se = new IdentifierSymbolEntry(type, $1, identifiers->getLevel());
        $<se>$ = se;//设置symbolEntry
        arrayValue = new int[arrayType->getSize()];//数组大小
    }//$<se>4设为我们临时的symbolentry
      InitVal {//数组的值进行初始化赋值
        ((IdentifierSymbolEntry*)$<se>4)->setArrayValue(arrayValue);//设置arrayvalue
        if(((InitValueListExpr*)$5)->isEmpty())//判断初始化数组值的expr栈是不是空
            ((IdentifierSymbolEntry*)$<se>4)->setAllZero();//空全部设置为0
        identifiers->install($1, $<se>4);//插入符号表
        $$ = new DeclStmt(new Id($<se>4), $5);//构造ID
        delete []$1;
    }
    ;
ConstDef//常量定义和变量定义一致,就是把类型从inttype改为了constIntType
    : ID ASSIGN InitVal {
        SymbolEntry* se;
        se = new IdentifierSymbolEntry(TypeSystem::constIntType, $1, identifiers->getLevel());
        ((IdentifierSymbolEntry*)se)->setConst();
        identifiers->install($1, se);
        ((IdentifierSymbolEntry*)se)->setValue($3->getValue());
        $$ = new DeclStmt(new Id(se), $3);
        delete []$1;
    }
    | ID Array ASSIGN  {
        SymbolEntry* se;
        std::vector<int> vec;
        ExprNode* temp = $2;
        while(temp){
            vec.push_back(std::move(temp->getValue()));
            temp = (ExprNode*)(temp->getNext());
        }
        Type* type = TypeSystem::constIntType;
        Type* temp1;
        for(auto it = vec.rbegin(); it != vec.rend(); it++) {
            temp1 = new ArrayType(type, *it, true);
            if(type->isArray())
                ((ArrayType*)type)->setArrayType(temp1);
            type = temp1;
        }
        arrayType = (ArrayType*)type;
        value_index = 0;
        std::stack<InitValueListExpr*>().swap(Expvalue_stack);
        se = new IdentifierSymbolEntry(type, $1, identifiers->getLevel());
        ((IdentifierSymbolEntry*)se)->setConst();
        $<se>$ = se;
        arrayValue = new int[arrayType->getSize()];
    }
      InitVal {
        ((IdentifierSymbolEntry*)$<se>4)->setArrayValue(arrayValue);
        identifiers->install($1, $<se>4);
        $$ = new DeclStmt(new Id($<se>4), $5);
        delete []$1;
    } 
    ;
Array
    : LBRACKET Exp RBRACKET {
        $$ = $2;
    }
    | Array LBRACKET Exp RBRACKET {
        $$ = $1;
        $1->setNext($3);
    }
    ;
InitVal 
    : Exp {
        $$ = $1;
        if(!Expvalue_stack.empty()){//栈不为空的时候
            arrayValue[value_index++] = $1->getValue();//值放入arrayvalue
            Type* array_Type = Expvalue_stack.top()->getSymbolEntry()->getType();
            if(array_Type == TypeSystem::intType||array_Type == TypeSystem::constIntType)//如果栈顶的不是数组类型
                Expvalue_stack.top()->addExpr($1);//我们直接将值setnext()上就好
            else
                while(array_Type){//如果是数组类型
                    if(((ArrayType*)array_Type)->getElementType() != TypeSystem::intType&&((ArrayType*)array_Type)->getElementType() != TypeSystem::constIntType){
                        //判断组成元素是不是数组类型,如果仍然是数组类型,表明我们的值不是在这一维赋值的
                        array_Type = ((ArrayType*)array_Type)->getElementType();//取下一维的元素类型
                        SymbolEntry* se = new ConstantSymbolEntry(array_Type);//构建symbolentry
                        InitValueListExpr* list = new InitValueListExpr(se);//构建新的初始化列表
                        Expvalue_stack.top()->addExpr(list);//将新的初始化列表链setnext()在我们高维的数组后面
                        Expvalue_stack.push(std::move(list));//将新的初始化列表压入栈。
                    }//反复迭代，直到不是数组类型，此时栈顶的InitValueListExpr也就是我们最低维的数组的要分配的Exprvaluelist
                    else{
                        Expvalue_stack.top()->addExpr($1);//将此时的Exp设置为我们最低维的InitValueListExp的setnext()
                        //分配完值后要循环判断，是不是这一维的数组的值已经分配完毕了
                        while(Expvalue_stack.top()->isFull() && Expvalue_stack.size() !=Wait_distrib){
                            Expvalue_stack.pop();//分配完毕将这个Exprvaluelist从栈里弹出
                        }
                        break;
                    }
                }
        }         
    }
    | LBRACE RBRACE {//空数组
        SymbolEntry* se;
        ExprNode* list;
        if(Expvalue_stack.empty()){//如果栈空，已经分配到了数组最低维度
            memset(arrayValue, 0, arrayType->getSize());//将对应的arrayvalue的中对应个数的值设置为0
            value_index += arrayType->getSize() / TypeSystem::intType->getSize();//修改索引
            se = new ConstantSymbolEntry(arrayType);//设置对应的symbolentry
            list = new InitValueListExpr(se);//构造InitvaluelistExpr链表
        }else{//栈不是空说明还在分配高维度
            Type* type = ((ArrayType*)(Expvalue_stack.top()->getSymbolEntry()->getType()))->getElementType();
            //获取这一维度的元素的arraytype
            int len = type->getSize() / TypeSystem::intType->getSize();
            memset(arrayValue + value_index, 0, type->getSize());//给元素的的值全都设置为0
            value_index += len;//修改索引
            se = new ConstantSymbolEntry(type);//构建symbolentry
            list = new InitValueListExpr(se);//构建对应初始化列表
            Expvalue_stack.top()->addExpr(list);//将新的初始化列表链setnext()在我们高维的数组后面
            //分配完值后要循环判断，是不是这一维的数组的值已经分配完毕了
            while(Expvalue_stack.top()->isFull() && Expvalue_stack.size() !=Wait_distrib){
                Expvalue_stack.pop();//分配完毕将这个Exprvaluelist从栈里弹出
            }
        }
        $$ = list;
    }
    | LBRACE {//每进入一个做大括号
        SymbolEntry* se;
        if(!Expvalue_stack.empty())
            //如果栈非空，取数组元素的arraytype去构建symbolentry，然后构建initvaluelistExpr压入，如果是空的，直接自己构建压入
            arrayType = (ArrayType*)(((ArrayType*)(Expvalue_stack.top()->getSymbolEntry()->getType()))->getElementType());
        se = new ConstantSymbolEntry(arrayType);//构建symbolentry
        InitValueListExpr* expr = new InitValueListExpr(se);//构建对应初始化列表
        if(!Expvalue_stack.empty())
            Expvalue_stack.top()->addExpr(expr);//如果栈非空，将新构建的初始化列表setnext()顶部的InitvalueListExpr
        Expvalue_stack.push(expr);//压栈我们的expr
        $<exprtype>$ = expr;
        Wait_distrib++;//待分配的+1
    } 
      InitValList RBRACE {
        Wait_distrib--;//分配完后leftcnt减一
        while(Expvalue_stack.top() != $<exprtype>2 && Expvalue_stack.size() > (Wait_distrib + 1))//栈元素个数大于剩下要分配的
            Expvalue_stack.pop();//分配完值后要循环判断，是不是这一维的数组的值已经分配完毕了
        if(Expvalue_stack.top() == $<exprtype>2)
            Expvalue_stack.pop();
        $$ = $<exprtype>2;
        if(!Expvalue_stack.empty())
            while(Expvalue_stack.top()->isFull() && Expvalue_stack.size() != Wait_distrib){
                Expvalue_stack.pop();
            }
        int size = ((ArrayType*)($$->getSymbolEntry()->getType()))->getSize()/ TypeSystem::intType->getSize();
        while(value_index % size != 0)
            arrayValue[value_index++] = 0;//后面未分配的补零
        if(!Expvalue_stack.empty())
            arrayType = (ArrayType*)(((ArrayType*)(Expvalue_stack.top()->getSymbolEntry()->getType()))->getElementType());
    }
    ;
InitValList
    : InitVal {
        $$ = $1;
    }
    | InitValList COMMA InitVal {
        $$ = $1;
    }
    ;
FuncDef
    :
    Type ID {
        identifiers = new SymbolTable(identifiers);
        paramNo = 0;
    }
    LPAREN OptFuncFParams RPAREN {
        Type* funcType;
        std::vector<Type*> Type_vec;//存储参数类型
        std::vector<SymbolEntry*> SymEntry_vec;//存储参数的SymbolEntry
        DeclStmt* Paramlists = (DeclStmt*)$5;//函数实参用stmt表示，temp指向OptFuncFparams
        while(Paramlists){//循环压入vector中
            Type_vec.push_back(Paramlists->getId()->getSymbolEntry()->getType());
            SymEntry_vec.push_back(Paramlists->getId()->getSymbolEntry());
            Paramlists = (DeclStmt*)(Paramlists->getNext());
        }
        funcType = new FunctionType($1, Type_vec, SymEntry_vec);
        SymbolEntry* se = new IdentifierSymbolEntry(funcType, $2, identifiers->getPrev()->getLevel());
        identifiers->getPrev()->install($2, se);//插入符号表
        $<se>$ = se; 
    } 
    BlockStmt {
        $$ = new FunctionDef($<se>7, (DeclStmt*)$5, $8);
        SymbolTable* top = identifiers;
        identifiers = identifiers->getPrev();
        delete top;
        delete []$2;
    }
    ;
OptFuncFParams
    : FuncFParams {$$ = $1;}
    | %empty {$$ = nullptr;}
FuncFParams
    : FuncFParams COMMA FuncFParam {
        $$ = $1;
        $$->setNext($3);//前面的函数参数链接上将后面的函数参数
    }
    | FuncFParam {
        $$ = $1;
    }
    ;
FuncFParam
    : Type ID {
        SymbolEntry* se;
        se = new IdentifierSymbolEntry($1, $2, identifiers->getLevel(), paramNo++);
        identifiers->install($2, se);
        ((IdentifierSymbolEntry*)se)->setLabel();//设置ID对应的标签
        ((IdentifierSymbolEntry*)se)->setAddr(new Operand(se));//设置对应的地址Oprand，为后续的函数调用中的中间代码生成做准备
        $$ = new DeclStmt(new Id(se));
        delete []$2;
    }
    | Type ID FuncArray {
        SymbolEntry* se;
        ExprNode* temp = $3;
        Type* arr_now = TypeSystem::intType;
        Type* arr_new;
        std::stack<ExprNode*> Expvalue_stack;
        while(temp){
            //将FuncArray中的InitExprVal逐个压栈
            Expvalue_stack.push(temp);
            temp = (ExprNode*)(temp->getNext());
        }
        //从低维度逐个取出来值，迭代更新我们的Arraytype(多维数组做参数)
        while(!Expvalue_stack.empty()){
            arr_new = new ArrayType(arr_now, Expvalue_stack.top()->getValue());
            if(arr_now->isArray())
                ((ArrayType*)arr_now)->setArrayType(arr_new);
            arr_now = arr_new;
            Expvalue_stack.pop();//每取一个删除我们栈中的一个元素
        }
        se = new IdentifierSymbolEntry(arr_now, $2, identifiers->getLevel(), paramNo++);//构建符号表项
        identifiers->install($2, se);//插入符号表
        ((IdentifierSymbolEntry*)se)->setLabel();//设置lable
        ((IdentifierSymbolEntry*)se)->setAddr(new Operand(se));//设置对应的地址Oprand
        $$ = new DeclStmt(new Id(se));
        delete []$2;
    }
    ;
FuncArray //数组做参数，其中的Exp也就是a[3][4][7]，其中从a到3，4，7逐个链接
    : LBRACKET RBRACKET {
        $$ = new ExprNode(nullptr);
    }
    | FuncArray LBRACKET Exp RBRACKET {
        $$ = $1;
        $$->setNext($3);
    }
%%

int yyerror(char const* message)
{
    std::cerr<<message<<std::endl;
    return -1;
}
