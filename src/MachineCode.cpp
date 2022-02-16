#include "MachineCode.h"
#include <iostream>
#include "Type.h"
#include <memory>
//#include <unistd.h>
#define likely(x)       __builtin_expect(!!(x), 1)
#define unlikely(x)     __builtin_expect(!!(x), 0)
extern FILE* yyout;

int MachineBlock::label = 0;

MachineOperand::MachineOperand(int tp, int val) {//立即数或者寄存器
    this->type = tp;
    if (tp == MachineOperand::IMM)
        this->val = val;
    else
        this->reg_no = val;
}

MachineOperand::MachineOperand(std::string label) {//操作数未Label地址标签
    this->type = MachineOperand::LABEL;
    this->label = label;
}

bool MachineOperand::operator==(const MachineOperand& a) const {
    if (this->type != a.type)
        return false;
    if (this->type == IMM)
        return this->val == a.val;
    return this->reg_no == a.reg_no;
}

bool MachineOperand::operator<(const MachineOperand& a) const {
    if (this->type == a.type) {
        if (this->type == IMM)
            return this->val < a.val;
        return this->reg_no < a.reg_no;
    }
    return this->type < a.type;

    if (this->type != a.type)
        return false;
    if (this->type == IMM)
        return this->val == a.val;
    return this->reg_no == a.reg_no;
}

void MachineOperand::PrintReg() {
    switch (reg_no) {
    case 11:
        fprintf(yyout, "fp");
        break;
    case 13:
        fprintf(yyout, "sp");
        break;
    case 14:
        fprintf(yyout, "lr");
        break;
    case 15:
        fprintf(yyout, "pc");
        break;
    default:
        fprintf(yyout, "r%d", reg_no);
        break;
    }
}

void MachineOperand::output() {
    /* HINT：print operand
     * Example:
     * immediate num 1 -> print #1;
     * register 1 -> print r1;
     * lable _addra -> print _addra; */
    switch (this->type) {
    case IMM:
        fprintf(yyout, "#%d", this->val);
        break;
    case VREG:
        fprintf(yyout, "v%d", this->reg_no);
        break;
    case REG:
        PrintReg();
        break;
    case LABEL:
        if (this->label.substr(0, 2) == ".L")
            fprintf(yyout, "%s", this->label.c_str());
        else if (this->label.substr(0, 1) == "@")
            fprintf(yyout, "%s", this->label.c_str() + 1);
        else
            fprintf(yyout, "_addr%s%d", this->label.c_str(), parent->getParent()->getParent()->getParent()->getN());
    default:
        break;
    }
}

void MachineInstruction::PrintCond()
{
    // TODO 全部条件码EQ, NE, LT, LE , GT, GE
    switch (cond)
    {
    case LT:
        fprintf(yyout, "lt");
        break;
    case EQ:
        fprintf(yyout, "eq");
        break;
    case NE:
        fprintf(yyout, "ne");
        break;
    case LE:
        fprintf(yyout, "le");
        break;
    case GT:
        fprintf(yyout, "gt");
        break;
    case GE:
        fprintf(yyout, "ge");
        break;
    default:
        break;
    }
}

void MachineInstruction::insertBefore(MachineInstruction* inst) {
    auto& instructions = parent->getInsts();
    auto it = std::find(instructions.begin(), instructions.end(), this);
    instructions.insert(it, inst);
}

void MachineInstruction::insertAfter(MachineInstruction* inst) {
    auto& instructions = parent->getInsts();
    auto it = std::find(instructions.begin(), instructions.end(), this);
    instructions.insert(++it, inst);
}

BinaryMInstruction::BinaryMInstruction(MachineBlock* p,
    int op,
    MachineOperand* dst,
    MachineOperand* src1,
    MachineOperand* src2,
    int cond) {
    this->parent = p;
    this->type = MachineInstruction::BINARY;
    this->op = op;
    this->cond = cond;
    this->def_list.push_back(std::move(dst));
    this->use_list.push_back(std::move(src1));
    this->use_list.push_back(std::move(src2));
    dst->setParent(this);
    src1->setParent(this);
    src2->setParent(this);
}

void BinaryMInstruction::output()
{
    // TODO: 
    // Complete other instructions,
    switch (this->op)
    {
    case BinaryMInstruction::ADD:
        fprintf(yyout, "\tadd ");
        this->PrintCond();
        this->def_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[1]->output();
        fprintf(yyout, "\n");
        break;
    case BinaryMInstruction::SUB:
        fprintf(yyout, "\tsub ");
        this->PrintCond();
        this->def_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[1]->output();
        fprintf(yyout, "\n");
        break;
    case BinaryMInstruction::MUL:
        fprintf(yyout, "\tmul ");
        this->PrintCond();
        this->def_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[1]->output();
        fprintf(yyout, "\n");
        break;
    case BinaryMInstruction::DIV:
        fprintf(yyout, "\tsdiv ");
        this->PrintCond();
        this->def_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[1]->output();
        fprintf(yyout, "\n");
        break;
    case BinaryMInstruction::OR:
        fprintf(yyout, "\torr ");
        this->PrintCond();
        this->def_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[1]->output();
        fprintf(yyout, "\n");
        break;
    case BinaryMInstruction::AND:
        fprintf(yyout, "\tand ");
        this->PrintCond();
        this->def_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[0]->output();
        fprintf(yyout, ", ");
        this->use_list[1]->output();
        fprintf(yyout, "\n");
        break;
        // case BinaryMInstruction::EOR://异或
        //     fprintf(yyout, "\teor ");
        //     this->PrintCond();
        //     this->def_list[0]->output();
        //     fprintf(yyout, ", ");
        //     this->use_list[0]->output();
        //     fprintf(yyout, ", ");
        //     this->use_list[1]->output();
        //     fprintf(yyout, "\n");
        //     break;
    default:
        break;
    }
}

LoadMInstruction::LoadMInstruction(MachineBlock* p,
    MachineOperand* dst,
    MachineOperand* src1,
    MachineOperand* src2,
    int cond) {
    this->parent = p;
    this->type = MachineInstruction::LOAD;
    this->op = -1;
    this->cond = cond;
    this->def_list.push_back(std::move(dst));
    this->use_list.push_back(std::move(src1));//基地址
    if (src2)
        this->use_list.push_back(std::move(src2));//偏移部分
    dst->setParent(this);
    src1->setParent(this);
    if (src2)
        src2->setParent(this);
}

void LoadMInstruction::output() {
    fprintf(yyout, "\tldr ");
    this->def_list[0]->output();
    fprintf(yyout, ", ");

    // Load immediate num, eg: ldr r1, =8
    if (this->use_list[0]->isImm()) {
        fprintf(yyout, "=%d\n", this->use_list[0]->getVal());
        return;
    }

    // Load address
    if (this->use_list[0]->isReg() || this->use_list[0]->isVReg())
        fprintf(yyout, "[");

    this->use_list[0]->output();//基地址
    if (this->use_list.size() > 1) {
        fprintf(yyout, ", ");
        this->use_list[1]->output();//偏移部分
    }

    if (this->use_list[0]->isReg() || this->use_list[0]->isVReg())
        fprintf(yyout, "]");
    fprintf(yyout, "\n");
}

StoreMInstruction::StoreMInstruction(MachineBlock* p,
    MachineOperand* src1,//目的地址
    MachineOperand* src2,//基地址
    MachineOperand* src3,//偏移部分
    int cond) {
    this->parent = p;
    this->type = MachineInstruction::STORE;
    this->op = -1;
    this->cond = cond;
    this->use_list.push_back(std::move(src1));
    this->use_list.push_back(std::move(src2));
    if (src3)
        this->use_list.push_back(std::move(src3));
    src1->setParent(this);
    src2->setParent(this);
    if (src3)
        src3->setParent(this);
}

void StoreMInstruction::output()
{
    // TODO
    fprintf(yyout, "\tstr ");
    this->use_list[0]->output();
    fprintf(yyout, ", ");

    // 存寄存器中数据到内存,合法的程序，对str指令不用判断imm
    if (this->use_list[1]->isReg() || this->use_list[1]->isVReg())
        fprintf(yyout, "[");

    this->use_list[1]->output();
    if (this->use_list.size() > 2)//偏移部分
    {
        fprintf(yyout, ", ");
        this->use_list[2]->output();
    }

    if (this->use_list[1]->isReg() || this->use_list[1]->isVReg())
        fprintf(yyout, "]");
    fprintf(yyout, "\n");
}

MovMInstruction::MovMInstruction(MachineBlock* p,
    int op,
    MachineOperand* dst,
    MachineOperand* src,
    int cond) {
    this->parent = p;
    this->type = MachineInstruction::MOV;
    this->op = op;
    this->cond = cond;
    this->def_list.push_back(std::move(dst));
    this->use_list.push_back(std::move(src));
    dst->setParent(this);
    src->setParent(this);
}

void MovMInstruction::output()
{
    // TODO
    if (this->op == MOV)
        fprintf(yyout, "\tmov");
    else
        fprintf(yyout, "\tmvn");
    this->PrintCond();
    fprintf(yyout, " ");
    this->def_list[0]->output();
    fprintf(yyout, ", ");
    this->use_list[0]->output();
    fprintf(yyout, "\n");
}

BranchMInstruction::BranchMInstruction(MachineBlock* p,
    int op,
    MachineOperand* dst,
    int cond) {
    this->parent = p;
    this->type = MachineInstruction::BRANCH;
    this->op = op;
    this->cond = cond;
    this->use_list.push_back(dst);
    dst->setParent(this);
}

void BranchMInstruction::output()
{
    // TODO
    switch (this->op)
    {
    case B:
        fprintf(yyout, "\tb");
        break;
    case BL:
        fprintf(yyout, "\tbl");
        break;
    case BX:
        fprintf(yyout, "\tbx");
        break;
    }
    this->PrintCond();
    fprintf(yyout, "\t");
    this->use_list[0]->output();
    fprintf(yyout, "\n");
}

CmpMInstruction::CmpMInstruction(MachineBlock* p,
    MachineOperand* src1,
    MachineOperand* src2,
    int cond) {
    this->parent = p;
    this->type = MachineInstruction::CMP;
    this->op = -1;
    this->cond = cond;
    p->setCmpCond(cond);
    this->use_list.push_back(std::move(src1));
    this->use_list.push_back(std::move(src2));
    src1->setParent(this);
    src2->setParent(this);
}

void CmpMInstruction::output() {
    // TODO
    fprintf(yyout, "\tcmp ");
    this->use_list[0]->output();
    fprintf(yyout, ", ");
    this->use_list[1]->output();
    fprintf(yyout, "\n");
}

StackMInstrcuton::StackMInstrcuton(MachineBlock* p,//寄存器压栈和弹栈指令
    int op,
    std::vector<MachineOperand*> srcs,
    MachineOperand* src,
    MachineOperand* src1,
    int cond) {
    this->parent = p;
    this->type = MachineInstruction::STACK;
    this->op = op;
    this->cond = cond;
    if (srcs.size())
        for (auto it = srcs.begin(); it != srcs.end(); it++)
            this->use_list.push_back(std::move(*it));
    this->use_list.push_back(std::move(src));
    src->setParent(this);
    if (src1) {
        this->use_list.push_back(std::move(src1));
        src1->setParent(this);
    }
}

void StackMInstrcuton::output() {
    //TODO
    switch (op) {
    case PUSH:
        fprintf(yyout, "\tpush ");
        break;
    case POP:
        fprintf(yyout, "\tpop ");
        break;
    }
    fprintf(yyout, "{");
    this->use_list[0]->output();
    for (long unsigned int i = 1; i < use_list.size(); i++) {
        fprintf(yyout, ", ");
        this->use_list[i]->output();
    }
    fprintf(yyout, "}\n");
}

MachineFunction::MachineFunction(MachineUnit* p, SymbolEntry* sym_ptr) {
    this->parent = p;
    this->sym_ptr = sym_ptr;
    this->stack_size = 0;
    this->paramsNum =
        ((FunctionType*)(sym_ptr->getType()))->getParamsSe().size();
};
void MachineBlock::output() {
    bool first = true;
    int offset = (parent->getSavedRegs().size() + 2) * 4;//fp,lr+saved寄存器
    int num = parent->getParamsNum();//参数个数
    int count = 0;
    if (!inst_list.empty())
    {
        fprintf(yyout, ".L%d:\n", this->no);//块号标签
        for (auto it = inst_list.begin(); it != inst_list.end(); it++)
        {
            if ((*it)->isBX())
            {
                //bx之前输出pop信息
                auto fp = new MachineOperand(MachineOperand::REG, 11);
                auto lr = new MachineOperand(MachineOperand::REG, 14);
                std::unique_ptr<StackMInstrcuton> cur1(new StackMInstrcuton(
                    this, StackMInstrcuton::POP, parent->getSavedRegs(), fp, lr));
                cur1->output();
            }
            if (num > 4 && (*it)->isStore())
            {
                MachineOperand* operand = (*it)->getUse()[0];
                if (operand->isReg() && operand->getReg() == 1)
                {
                    if (first)
                    {//第一次遇到r1，正常output
                        first = false;
                    }
                    else
                    {
                        //后几次遇到r1，说明是4个之后的参数，output其之前先输出load信息
                        //此时在文本中r1就是实际的值
                        auto fp = new MachineOperand(MachineOperand::REG, 11);
                        auto r1 = new MachineOperand(MachineOperand::REG, 1);
                        auto off =
                            new MachineOperand(MachineOperand::IMM, offset);//栈中偏移
                        offset += 4;
                        std::unique_ptr<LoadMInstruction> cur2(new LoadMInstruction(
                            this, r1, fp, off));
                        cur2->output();
                    }
                }
            }
            if ((*it)->isAdd())
            {
                //add之前先判断栈空间是否已经超限
                auto dst = (*it)->getDef()[0];
                auto src1 = (*it)->getUse()[0];
                if (dst->isReg() && dst->getReg() == 13 && src1->isReg() &&
                    src1->getReg() == 13 && (*(it + 1))->isBX())
                {
                    int size = parent->AllocSpace(0);
                    //超限先load
                    if (likely(size < -255 || size > 255))
                    {
                        auto r1 = new MachineOperand(MachineOperand::REG, 1);
                        auto off = new MachineOperand(MachineOperand::IMM, size);
                        std::unique_ptr<LoadMInstruction> cur3(new LoadMInstruction(
                            nullptr, r1, off));
                        cur3->output();
                        (*it)->getUse()[1]->setReg(1);
                    }
                    else
                        (*it)->getUse()[1]->setVal(size);
                }
            }
            (*it)->output();//每条指令打印
            count++;
            if (likely(count % 400 == 0)) {
                //强制生成文字池，防止load地址超限
                fprintf(yyout, "\tb .B%d\n", label);
                fprintf(yyout, ".LTORG\n");
                parent->getParent()->printGlobal();
                fprintf(yyout, ".B%d:\n", label++);
            }
        }
    }
}

void MachineFunction::output() {
    fprintf(yyout, "\t.global %s\n", this->sym_ptr->toStr().c_str() + 1);
    fprintf(yyout, "\t.type %s , %%function\n",
        this->sym_ptr->toStr().c_str() + 1);
    fprintf(yyout, "%s:\n", this->sym_ptr->toStr().c_str() + 1);
    // TODO
    /* Hint:
     *  1. Save fp
     *  2. fp = sp
     *  3. Save callee saved register
     *  4. Allocate stack space for local variable */

     // Traverse all the block in block_list to print assembly code.
    auto fp = new MachineOperand(MachineOperand::REG, 11);
    auto sp = new MachineOperand(MachineOperand::REG, 13);
    auto lr = new MachineOperand(MachineOperand::REG, 14);
    //push {{SavedRegs}，fp,lr}
    std::unique_ptr<StackMInstrcuton> p1(new StackMInstrcuton(
        nullptr, StackMInstrcuton::PUSH, getSavedRegs(), fp, lr));
    p1->output();
    p1.reset();
    //mov fp,sp
    std::unique_ptr<MovMInstruction> p2(new MovMInstruction(
        nullptr, MovMInstruction::MOV, fp, sp));
    p2->output();
    p2.reset();
    int off = AllocSpace(0);
    auto size = new MachineOperand(MachineOperand::IMM, off);
    if (likely(off < -255 || off > 255))
    {
        //栈空间超限，先load
        auto r4 = new MachineOperand(MachineOperand::REG, 4);
        std::unique_ptr<LoadMInstruction> p3(new LoadMInstruction(
            nullptr, r4, size));
        p3->output();
        p3.reset();
        std::unique_ptr<BinaryMInstruction> p4(new BinaryMInstruction(
            nullptr, BinaryMInstruction::SUB, sp, sp, r4));
        p4->output();
        p4.reset();
    }
    else
    {//否则直接sub sp,sp,stacksize
        std::unique_ptr<BinaryMInstruction> p5(new BinaryMInstruction(
            nullptr, BinaryMInstruction::SUB, sp, sp, size));
        p5->output();
        p5.reset();
    }
    int count = 0;
    for (auto iter : block_list) {
        iter->output();//函数中每一个基本块output
        count += iter->getSize();
        if (likely(count > 100))
        {
            //强制生成文字池，防止load地址超限
            fprintf(yyout, "\tb .F%d\n", parent->getN());
            fprintf(yyout, ".LTORG\n");
            parent->printGlobal();
            fprintf(yyout, ".F%d:\n", parent->getN() - 1);
            count = 0;
        }
    }
    fprintf(yyout, "\n");
}

std::vector<MachineOperand*> MachineFunction::getSavedRegs() {
    std::vector<MachineOperand*> regs;
    for (auto it = saved_regs.begin(); it != saved_regs.end(); it++)
    {
        auto reg = new MachineOperand(MachineOperand::REG, *it);
        regs.push_back(std::move(reg));
    }
    return regs;
}

void MachineUnit::PrintGlobalDecl() {
    // TODO:
    // You need to print global variable/const declarition code;
    if (!global_list.empty())
        fprintf(yyout, "\t.data\n");
    std::vector<int> const_indicator;
    std::vector<int> zero_indicator;
    for (long unsigned int i = 0; i < global_list.size(); i++) {//全局数据逐个取值
        IdentifierSymbolEntry* se = (IdentifierSymbolEntry*)global_list[i];
        if (se->getConst())
        {
            const_indicator.push_back(i);//如果是常量，将其压入常量栈，暂时不处理
        }
        else
        {
            if (se->Non_intial())
            {
                zero_indicator.push_back(i);//如果未初始化，默认全都是0，暂时不处理
            }
            else {//否则全局变量，打印相关信息
                fprintf(yyout,
                    "\t.global %s\n\t.align 4\n\t.size %s, %d\n%s:\n",
                    se->toStr().c_str(),
                    se->toStr().c_str(),
                    se->getType()->getSize() / 8,
                    se->toStr().c_str()
                );
                if (!se->getType()->isArray())
                {//如果不是数组，直接输出
                    fprintf(yyout, "\t.word %d\n", se->getValue());
                }
                else
                {//如果是数组，循环输出
                    int n = se->getType()->getSize() / 32;
                    int* p = se->getArrayValue();
                    for (int i = 0; i < n; i++) {
                        fprintf(yyout, "\t.word %d\n", p[i]);
                    }
                }
            }
        }
    }

    if (!zero_indicator.empty()) {//处理未初始化的段
        for (auto i : zero_indicator) {
            IdentifierSymbolEntry* se = (IdentifierSymbolEntry*)global_list[i];
            if (se->getType()->isArray()) {
                fprintf(yyout, "\t.comm %s, %d, 4\n", se->toStr().c_str(),
                    se->getType()->getSize() / 8);
            }
        }
    }

    if (!const_indicator.empty()) {//处理常量段
        fprintf(yyout, "\t.section .rodata\n");
        for (auto i : const_indicator) {//逐个取数据
            IdentifierSymbolEntry* se = (IdentifierSymbolEntry*)global_list[i];
            fprintf(yyout, "\t.global %s\n\t.align 4\n\t.size %s, %d\n%s:\n",//输出相关信息
                se->toStr().c_str(),
                se->toStr().c_str(),
                se->getType()->getSize() / 8,
                se->toStr().c_str()
            );
            if (!se->getType()->isArray())
            {//如果不是数组，输出相关信息
                fprintf(yyout, "\t.word %d\n", se->getValue());
            }
            else {//如果是数组，循环打印
                int n = se->getType()->getSize() / 32;
                int* p = se->getArrayValue();
                for (int i = 0; i < n; i++) {
                    fprintf(yyout, "\t.word %d\n", p[i]);
                }
            }
        }
    }
}

void MachineUnit::output() {
    // TODO
    /* Hint:
     * 1. You need to print global variable/const declarition code;
     * 2. Traverse all the function in func_list to print assembly code;
     * 3. Don't forget print bridge label at the end of assembly code!! */
    fprintf(yyout, "\t.arch armv8-a\n");
    fprintf(yyout, "\t.arch_extension crc\n");
    fprintf(yyout, "\t.arm\n");
    PrintGlobalDecl();
    fprintf(yyout, "\t.text\n");
    for (auto iter : func_list)
        iter->output();
    printGlobal();//最后要输出全局变量标签信息
}

void MachineUnit::insertGlobal(SymbolEntry* se) {
    global_list.push_back(std::move(se));
}

void MachineUnit::printGlobal() {
    for (auto& se : this->global_list)
    {
        if (se->getType()->isString())//采用哈希，字符长度作为对应label
        {
            fprintf(yyout, "_addrSTR%d%d:\n", static_cast<StringType*>(se->getType())->getLength(), SymbolTable::getLabel());
            fprintf(yyout, "\t.word .STR%d\n", static_cast<StringType*>(se->getType())->getLength());
        }
        else
        {
            fprintf(yyout, "_addr%s%d:\n", se->toStr().c_str(), n);
            fprintf(yyout, "\t.word %s\n", se->toStr().c_str());
        }
    }
    n++;
}