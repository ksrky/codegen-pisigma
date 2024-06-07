#include <stdio.h>

#include "interpreter.hpp"
#include "context.hpp"

class Instruction {
        int op_;
public:
        Instruction(int op) : op_(op) {}

        const int op = op_;
};

class IFormat : public Instruction {
        int rs_;
        int rt_;
        int imm_;
public:
        IFormat(TalContext &ctx, int op, int rs, int rt, int imm)
        : Instruction(op), rs_(rs), rt_(rt), imm_(imm) {
                *ctx.pc = imm & (rt << RT_OFFSET) & (rs << RS_OFFSET) & (op << OP_OFFSET);
        }
        IFormat(TalContext &ctx, int op, int rs, int rt)
        : Instruction(op), rs_(rs), rt_(rt) {
                *ctx.pc = (rt << RT_OFFSET) & (rs << RS_OFFSET) & (op << OP_OFFSET);
        }
        IFormat(TalContext &ctx, int op) : Instruction(op) {}
};

class BeqInst : public IFormat {
public:
        BeqInst(TalContext &ctx, int rs, int rt, int imm) : IFormat(ctx, OP_BEQ, rs, rt, imm) {}
};

class BneInst : public IFormat {
public:
        BneInst(TalContext &ctx, int rs, int rt, int imm) : IFormat(ctx, OP_BNE, rs, rt, imm) {}
};

class BgtInst : public IFormat {
public:
        BgtInst(TalContext &ctx, int rs, int rt, int imm) : IFormat(ctx, OP_BGT, rs, rt, imm) {}
};

class BltInst : public IFormat {
public:
        BltInst(TalContext &ctx, int rs, int rt, int imm) : IFormat(ctx, OP_BLT, rs, rt, imm) {}
};

class BgeInst : public IFormat {
public:
        BgeInst(TalContext &ctx, int rs, int rt, int imm) : IFormat(ctx, OP_BGE, rs, rt, imm) {}
};

class BleInst : public IFormat {
public:
        BleInst(TalContext &ctx, int rs, int rt, int imm) : IFormat(ctx, OP_BLE, rs, rt, imm) {}
};

class LoadInst : public IFormat {
public:
        LoadInst(TalContext &ctx, int rt, int rs, int i) : IFormat(ctx, OP_LOAD, rs, rt, i) {}
};

class MoveInst : public IFormat {
public:
        MoveInst(TalContext &ctx, int rd, int rs) : IFormat(ctx, OP_MOVE, rs, rd) {}
};

class StoreInst : public IFormat {
public:
        StoreInst(TalContext &ctx, int rt, int i, int rs) : IFormat(ctx, OP_STORE, rs, rt, i) {}
};

class UnpackInst : public IFormat {
public:
        UnpackInst(TalContext &ctx, int rt, int rs) : IFormat(ctx, op) {};
};

class JFormat : public Instruction {
        int addr_;
public:
        JFormat(TalContext &ctx, int op, int addr) : Instruction(op), addr_(addr) {
                *ctx.pc = addr_ & (op << OP_OFFSET);
        }

        const int addr = addr_;
};

class CallInst : public JFormat {
public:
        CallInst(TalContext &ctx, int addr) : JFormat(ctx, OP_CALL, addr) {}
};

class JumpInst : public JFormat {
public:
        JumpInst(TalContext &ctx, int addr) : JFormat(ctx, OP_JUMP, addr) {}
};

class SallocInst : public JFormat {
public:
        SallocInst(TalContext &ctx, int n) : JFormat(ctx, OP_SALLOC, n) {}
};

class SfreeInst : public JFormat {
public:
        SfreeInst(TalContext &ctx, int n) : JFormat(ctx, OP_SFREE, n) {}
};

class RFormat : public Instruction {
        int rs_;
        int rt_;
        int rd_;
        int shamt_;
        int funct_;
public:
        RFormat(TalContext &ctx, int op, int rs, int rt, int rd, int shamt, int funct)
        : Instruction(op), rs_(rs), rt_(rt), rd_(rd), shamt_(shamt), funct_(funct) {
               *ctx.pc = funct_ & (shamt_ << SHAMT_OFFSET) & (rd_ << RD_OFFSET) & (rt_ << RD_OFFSET) & (op << OP_OFFSET); 
        }
        RFormat(TalContext &ctx, int rs, int rt, int rd) : Instruction(op), rs_(rs), rt_(rt), rd_(rd) {
               *ctx.pc = (rd_ << RD_OFFSET) & (rt_ << RD_OFFSET) & (op << OP_OFFSET); 
        }

        const int rs = rs_;
        const int rt = rt_;
        const int rd = rd_;
};

class AddInst : public RFormat {
public:
        AddInst(TalContext &ctx, int rs, int rt, int rd) : RFormat(ctx, rs, rt, rd) {}
};

class SubInst : public RFormat {
public:
        SubInst(TalContext &ctx, int rs, int rt, int rd) : RFormat(ctx, rs, rt, rd) {}
};

class MulInst : public RFormat {
public:
        MulInst(TalContext &ctx, int rs, int rt, int rd) : RFormat(ctx, rs, rt, rd) {}
};

class DivInst : public RFormat {
public:
        DivInst(TalContext &ctx, int rs, int rt, int rd) : RFormat(ctx, rs, rt, rd) {}
};
