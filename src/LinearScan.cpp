#include "LinearScan.h"
#include <algorithm>
#include <iostream>
#include "LiveVariableAnalysis.h"
#include "MachineCode.h"

LinearScan::LinearScan(MachineUnit* unit) {
    this->unit = unit;
    for (int i = 10; i >=4; i--)//当前可用寄存器
        regs.push_back(i);
}

void LinearScan::allocateRegisters() {
    for (auto& f : unit->getFuncs()) {
        func = f;
        bool success;
        success = false;
        while (!success)  // repeat until all vregs can be mapped
        {
            computeLiveIntervals();//旧区间重构，重分配原来的寄存器
            success = linearScanRegisterAllocation();
            if (!success)  // spill vregs that can't be mapped to real regs
                genSpillCode();//生成溢出代码
            else  // all vregs can be mapped to real regs
                modifyCode();//映射寄存器调整
        }
    }
}
//细节不用管
void LinearScan::makeDuChains() {
    LiveVariableAnalysis lva;
    lva.pass(func);
    du_chains.clear();
    int i = 0;
    std::map<MachineOperand, std::set<MachineOperand*>> liveVar;
    for (auto& bb : func->getBlocks()) {
        liveVar.clear();
        for (auto& t : bb->getLiveOut())
            liveVar[*t].insert(t);
        int no;
        no = i = bb->getInsts().size() + i;
        for (auto inst = bb->getInsts().rbegin(); inst != bb->getInsts().rend();
             inst++) {
            (*inst)->setNo(no--);
            for (auto& def : (*inst)->getDef()) {
                if (def->isVReg()) {
                    auto& uses = liveVar[*def];
                    du_chains[def].insert(uses.begin(), uses.end());
                    auto& kill = lva.getAllUses()[*def];
                    std::set<MachineOperand*> res;
                    set_difference(uses.begin(), uses.end(), kill.begin(),
                                   kill.end(), inserter(res, res.end()));
                    liveVar[*def] = res;
                }
            }
            for (auto& use : (*inst)->getUse()) {
                if (use->isVReg())
                    liveVar[*use].insert(use);
            }
        }
    }
}
//活跃区间计算，，原始代码已经给出，细节不用管
void LinearScan::computeLiveIntervals() {
    makeDuChains();
    intervals.clear();
    for (auto& du_chain : du_chains) {
        int t = -1;
        for (auto& use : du_chain.second)
            t = std::max(t, use->getParent()->getNo());
        Interval* interval = new Interval({du_chain.first->getParent()->getNo(),
                                           t,
                                           false,
                                           0,
                                           0,
                                           {du_chain.first},
                                           du_chain.second});
        intervals.push_back(std::move(interval));
    }
    bool change;
    change = true;
    while (change) {
        change = false;
        std::vector<Interval*> t(intervals.begin(), intervals.end());
        for (size_t i = 0; i < t.size(); i++)
            for (size_t j = i + 1; j < t.size(); j++) {
                Interval* w1 = t[i];
                Interval* w2 = t[j];
                if (**w1->defs.begin() == **w2->defs.begin()) {
                    std::set<MachineOperand*> temp;
                    set_intersection(w1->uses.begin(), w1->uses.end(),
                                     w2->uses.begin(), w2->uses.end(),
                                     inserter(temp, temp.end()));
                    if (!temp.empty()) {
                        change = true;
                        w1->defs.insert(w2->defs.begin(), w2->defs.end());
                        w1->uses.insert(w2->uses.begin(), w2->uses.end());
                        w1->start = std::min(w1->start, w2->start);
                        w1->end = std::max(w1->end, w2->end);
                        auto it =
                            std::find(intervals.begin(), intervals.end(), w2);
                        if (it != intervals.end())
                            intervals.erase(it);
                    }
                }
            }
    }
    sort(intervals.begin(), intervals.end(),
        [](Interval* a, Interval* b) {return a->start < b->start;});//按开始时间升序
}
//扫描分配寄存器
bool LinearScan::linearScanRegisterAllocation() {
    bool success = true;
    active.clear();
    regs.clear();//初始化
    for (int i = 10; i >=4; i--)
        regs.push_back(i);
    for (auto& i : intervals) {
        expireOldIntervals(i);//对被占用的寄存器重新分配
        if (regs.empty()) {
            spillAtInterval(i);//无可用寄存器，溢出到栈
            success = false;
        } else {
            //分配寄存器
            i->rreg = regs.front();
            regs.erase(regs.begin());
            active.push_back(std::move(i));
            sort(active.begin(), active.end(),
                [](Interval* a, Interval* b) {return a->end < b->end;});//按结束时间升序
        }
    }
    return success;
}

void LinearScan::modifyCode() {
    //映射到实际寄存器，便于output
    for (auto& interval : intervals) {
        func->addSavedRegs(interval->rreg);
        for (auto def : interval->defs)
            def->setReg(interval->rreg);
        for (auto use : interval->uses)
            use->setReg(interval->rreg);
    }
}

void LinearScan::genSpillCode() {
    for (auto& interval : intervals) {
        if (!interval->spill)//只对spill置位的操作
            continue;
        // TODO
        /* HINT:
         * The vreg should be spilled to memory.
         * 1. insert ldr inst before the use of vreg
         * 2. insert str inst after the def of vreg
         */
        interval->disp = -func->AllocSpace(4);//栈空间分配
        auto off = new MachineOperand(MachineOperand::IMM, interval->disp);
        auto fp = new MachineOperand(MachineOperand::REG, 11);
        for (auto use : interval->uses) {
            //在use之前插入load指令
            auto temp = new MachineOperand(*use);
            MachineOperand* operand = nullptr;
            //范围超过255就load，最后load范围256*4kb
            if (interval->disp > 255 || interval->disp < -255) {
                operand = new MachineOperand(MachineOperand::VREG,
                                             SymbolTable::getLabel());
                auto inst1 = new LoadMInstruction(use->getParent()->getParent(),
                                                  operand, off);
                use->getParent()->insertBefore(inst1);
            }
            if (operand) {
                //经过一次超限判断
                auto inst =
                    new LoadMInstruction(use->getParent()->getParent(), temp,
                                         fp, new MachineOperand(*operand));
                use->getParent()->insertBefore(inst);
            } else {
                //正常load
                auto inst = new LoadMInstruction(use->getParent()->getParent(),
                                                 temp, fp, off);
                use->getParent()->insertBefore(inst);
            }
        }
        for (auto def : interval->defs) {
            //在def之后插入store指令
            auto temp = new MachineOperand(*def);
            MachineOperand* operand = nullptr;
            MachineInstruction *inst1 = nullptr, *inst = nullptr;
            //范围超限判断
            if (interval->disp > 255 || interval->disp < -255) {
                operand = new MachineOperand(MachineOperand::VREG,
                                             SymbolTable::getLabel());
                inst1 = new LoadMInstruction(def->getParent()->getParent(),
                                             operand, off);
                def->getParent()->insertAfter(inst1);
            }
            if (operand)//经过一次超限判断
                inst =
                    new StoreMInstruction(def->getParent()->getParent(), temp,
                                          fp, new MachineOperand(*operand));
            else//正常store
                inst = new StoreMInstruction(def->getParent()->getParent(),
                                             temp, fp, off);
            if (inst1)
                inst1->insertAfter(inst);
            else
                def->getParent()->insertAfter(inst);
        }
    }
}

void LinearScan::expireOldIntervals(Interval* interval) {
    //active按照end时间升序排列
    auto it = active.begin();
    if (it != active.end()) {
        if ((*it)->end >= interval->start)//只用比较第一个
            return;
        //第一个end早于欲分配的sta  [](Interval* a, Interval* b) {return a->start < b->start;});//按开始时间rt，可以回收其寄存器重利用
        regs.push_back(std::move((*it)->rreg));
        it = active.erase(find(active.begin(), active.end(), *it));
        sort(regs.begin(), regs.end());
    }
}

void LinearScan::spillAtInterval(Interval* interval) {
    //选择active列表末尾与当前unhandled的一个溢出到栈中
    auto spill = active.back();
    if (spill->end > interval->end) {
        //选择active列表末尾溢出，并回收其寄存器
        spill->spill = true;
        interval->rreg = spill->rreg;
        active.push_back(std::move(interval));
        sort(active.begin(), active.end(),
            [](Interval* a, Interval* b) {return a->end < b->end;});
    } else {
        //只需置位spill标志位
        interval->spill = true;
    }
}
