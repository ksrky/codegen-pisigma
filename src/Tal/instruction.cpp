#include "context.hpp"

class IFormat {
        int op_;
        int rs_;
        int rt_;
        int imm_;
public:
        IFormat(TalContext &ctx, int op, int rs, int rt, int imm)
        : op_(op), rs_(rs), rt_(rt), imm_(imm) {
                ctx.addInstruction(I_FORMAT(op, rs, rt, imm));
        }
        IFormat(TalContext &ctx, int op, int rs, int rt)
        : op_(op), rs_(rs), rt_(rt) {
                ctx.addInstruction(I_FORMAT(op, rs, rt, 0));
        }
        IFormat(TalContext &ctx, int op) : op_(op) {
                ctx.addInstruction(I_FORMAT(op, 0, 0, 0));
        }
};

class JFormat {
        int op_;
        int addr_;
public:
        JFormat(TalContext &ctx, int op, int addr) : op_(op), addr_(addr) {
                ctx.addInstruction(J_FORMAT(op, addr));
        }
        JFormat(TalContext &ctx, int op) : op_(op) {
                ctx.addInstruction(J_FORMAT(op, 0));
        }
};

class RFormat {
        int op_;
        int rs_;
        int rt_;
        int rd_;
        int shamt_;
        int funct_;
public:
        RFormat(TalContext &ctx, int op, int rs, int rt, int rd, int shamt, int funct)
        : op_(op), rs_(rs), rt_(rt), rd_(rd), shamt_(shamt), funct_(funct) {
                ctx.addInstruction(R_FORMAT(op, rs, rt, rd, shamt, funct));
        }
        RFormat(TalContext &ctx, int op, int rs, int rt, int rd, int shamt)
        : op_(op), rs_(rs), rt_(rt), rd_(rd), shamt_(shamt) {
                ctx.addInstruction(R_FORMAT(op, rs, rt, rd, shamt, 0));
        }
        RFormat(TalContext &ctx, int op, int rs, int rt, int rd)
        : op_(op), rs_(rs), rt_(rt), rd_(rd) {
                ctx.addInstruction(R_FORMAT(op, rs, rt, rd, 0, 0));
        }

};

class MFormat {
        int op_;
        int rs_;
        int imm_;
public:
        MFormat(TalContext &ctx, int op, int rs, int imm) : op_(op), rs_(rs), imm_(imm) {
                ctx.addInstruction(I_FORMAT(op, rs, 0, imm));
        }
};

class HaltInst : public IFormat {
public:
        HaltInst(TalContext &ctx) : IFormat(ctx, OP_HALT) {}
};

class AddInst : public RFormat {
public:
        AddInst(TalContext &ctx, int rd, int rs, int rt)
        : RFormat(ctx, OP_ADD, rs, rt, rd) {}
};

class SubInst : public RFormat {
public:
        SubInst(TalContext &ctx, int rd, int rs, int rt)
        : RFormat(ctx, OP_SUB, rs, rt, rd) {}
};

class MulInst : public RFormat {
public:
        MulInst(TalContext &ctx, int rd, int rs, int rt)
        : RFormat(ctx, OP_MUL, rs, rt, rd) {}
};

class DivInst : public RFormat {
public:
        DivInst(TalContext &ctx, int rd, int rs, int rt)
        : RFormat(ctx, OP_DIV, rs, rt, rd) {}
};

class BneInst : public IFormat {
public:
        BneInst(TalContext &ctx, int rs, int rt, int16_t imm)
        : IFormat(ctx, OP_BNE, rs, rt, imm) {}
};

class BgtInst : public IFormat {
public:
        BgtInst(TalContext &ctx, int rs, int rt, int16_t imm)
        : IFormat(ctx, OP_BGT, rs, rt, imm) {}
};

class BltInst : public IFormat {
public:
        BltInst(TalContext &ctx, int rs, int rt, int16_t imm)
        : IFormat(ctx, OP_BLT, rs, rt, imm) {}
};

class BgeInst : public IFormat {
public:
        BgeInst(TalContext &ctx, int rs, int rt, int16_t imm)
        : IFormat(ctx, OP_BGE, rs, rt, imm) {}
};

class BleInst : public IFormat {
public:
        BleInst(TalContext &ctx, int rs, int rt, int16_t imm)
        : IFormat(ctx, OP_BLE, rs, rt, imm) {}
};

class CallInst : public JFormat {
public:
        CallInst(TalContext &ctx, int addr)
        : JFormat(ctx, OP_CALL, addr) {}
};

class JumpInst : public JFormat {
public:
        JumpInst(TalContext &ctx, int addr)
        : JFormat(ctx, OP_JUMP, addr) {}
};

class LoadInst : public IFormat {
public:
        LoadInst(TalContext &ctx, int rd, int rs, int16_t imm)
        : IFormat(ctx, OP_LOAD, rd, rs, imm) {}
};

class LoadiInst : public MFormat {
public:
        LoadiInst(TalContext &ctx, int rd, int imm)
        : MFormat(ctx, OP_LOADI, rd, imm) {}
};

class MallocInst : public MFormat {
public:
        MallocInst(TalContext &ctx, int rd, int imm)
        : MFormat(ctx, OP_MALLOC, rd, imm) {}
};

class StoreInst : public IFormat {
public:
        StoreInst(TalContext &ctx, int rs, int rd, int16_t imm)
        : IFormat(ctx, OP_STORE, rs, rd, imm) {}
};

class UnpackInst : public IFormat {
public:
        UnpackInst(TalContext &ctx, int rd, int rs)
        : IFormat(ctx, OP_UNPACK, rd, rs) {}
};

class SallocInst : public MFormat {
public:
        SallocInst(TalContext &ctx, int rd, int imm)
        : MFormat(ctx, OP_SALLOC, rd, imm) {}
};

class SfreeInst : public MFormat {
public:
        SfreeInst(TalContext &ctx, int rd, int imm)
        : MFormat(ctx, OP_SFREE, rd, imm) {}
};

class SloadInst : public IFormat {
public:
        SloadInst(TalContext &ctx, int rd, int rs, int16_t imm)
        : IFormat(ctx, OP_SLOAD, rd, rs, imm) {}
};

class SstoreInst : public IFormat {
public:
        SstoreInst(TalContext &ctx, int rs, int rd, int16_t imm)
        : IFormat(ctx, OP_SSTORE, rs, rd, imm) {}
};

class IntegerData {
        int data_;
public:
        IntegerData(TalContext &ctx, int data) : data_(data) {
                ctx.addData(data);
        }
};