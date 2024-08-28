#include <stdint.h>
#include <stdio.h>

#include "context.h"
#include "spec.h"

typedef struct IFormat {
        int8_t op_;
        int8_t rs_;
        int8_t rt_;
        int16_t imm_;
} IFormat;

void CreateIFormat(TalContext *ctx, int8_t op, int8_t rs, int8_t rt, int16_t imm) {
        AddInstruction(ctx, I_FORMAT(op, rs, rt, imm));
}

typedef struct JFormat {
        int8_t op_;
        int addr_;
} JFormat;

void CreateJFormat(TalContext *ctx, int8_t op, int addr) {
        AddInstruction(ctx, J_FORMAT(op, addr));
}

typedef struct RFormat {
        int8_t op_;
        int8_t rs_;
        int8_t rt_;
        int8_t rd_;
        int shamt_;
        int funct_;
} RFormat;

void CreateRFormat(TalContext *ctx, int8_t op, int8_t rs, int8_t rt, int8_t rd, int shamt, int funct) {
        AddInstruction(ctx, R_FORMAT(op, rs, rt, rd, shamt, funct));
}

void CreateRFormat_(TalContext *ctx, int8_t op, int8_t rs, int8_t rt, int8_t rd) {
        AddInstruction(ctx, R_FORMAT(op, rs, rt, rd, 0, 0));
}

/*
 * Instructions
 */

void CreateHaltInst(TalContext *ctx) {
        CreateJFormat(ctx, OP_HALT, 0);
}

void CreateAddInst(TalContext *ctx, int8_t rd, int8_t rs, int8_t rt) {
        CreateRFormat_(ctx, OP_ADD, rs, rt, rd);
}

void CreateSubInst(TalContext *ctx, int8_t rd, int8_t rs, int8_t rt) {
        CreateRFormat_(ctx, OP_SUB, rs, rt, rd);
}

void CreateMulInst(TalContext *ctx, int8_t rd, int8_t rs, int8_t rt) {
        CreateRFormat_(ctx, OP_MUL, rs, rt, rd);
}

void CreateDivInst(TalContext *ctx, int8_t rd, int8_t rs, int8_t rt) {
        CreateRFormat_(ctx, OP_DIV, rs, rt, rd);
}

void CreateBeqInst(TalContext *ctx, int8_t rs, int8_t rt, int16_t imm) {
        CreateIFormat(ctx, OP_BEQ, rs, rt, imm);
}

void CreateBneInst(TalContext *ctx, int8_t rs, int8_t rt, int16_t imm) {
        CreateIFormat(ctx, OP_BNE, rs, rt, imm);
}

void CreateBgtInst(TalContext *ctx, int8_t rs, int8_t rt, int16_t imm) {
        CreateIFormat(ctx, OP_BGT, rs, rt, imm);
}

void CreateBltInst(TalContext *ctx, int8_t rs, int8_t rt, int16_t imm) {
        CreateIFormat(ctx, OP_BLT, rs, rt, imm);
}

void CreateBgeInst(TalContext *ctx, int8_t rs, int8_t rt, int16_t imm) {
        CreateIFormat(ctx, OP_BGE, rs, rt, imm);
}

void CreateBleInst(TalContext *ctx, int8_t rs, int8_t rt, int16_t imm) {
        CreateIFormat(ctx, OP_BLE, rs, rt, imm);
}

void CreateCallInst(TalContext *ctx, int addr) {
        CreateJFormat(ctx, OP_CALL, addr);
}

void CreateJumpInst(TalContext *ctx, int addr) {
        CreateJFormat(ctx, OP_JUMP, addr);
}

void CreateJumpRInst(TalContext *ctx, int8_t rs) {
        CreateIFormat(ctx, OP_JUMPR, rs, 0, 0);
}

void CreateLoadInst(TalContext *ctx, int8_t rd, int8_t rs, int16_t imm) {
        CreateIFormat(ctx, OP_LOAD, rd, rs, imm);
}

void CreateLoadiInst(TalContext *ctx, int8_t rd, int16_t imm) {
        CreateIFormat(ctx, OP_LOADI, rd, 0, imm);
}

void CreateMallocInst(TalContext *ctx, int8_t rd, int16_t imm) {
        CreateIFormat(ctx, OP_MALLOC, rd, 0, imm);
}

void CreateStoreInst(TalContext *ctx, int8_t rs, int8_t rd, int16_t imm) {
        CreateIFormat(ctx, OP_STORE, rs, rd, imm);
}

void CreateUnpackInst(TalContext *ctx, int8_t rd, int8_t rs) {
        CreateIFormat(ctx, OP_UNPACK, rd, rs, 0);
}

void CreateSallocInst(TalContext *ctx, int8_t rd, int16_t addr) {
        CreateJFormat(ctx, OP_SALLOC, addr);
}

void CreateSfreeInst(TalContext *ctx, int8_t rd, int16_t addr) {
        CreateJFormat(ctx, OP_SFREE, addr);
}

void CreateSloadInst(TalContext *ctx, int8_t rd, int8_t rs, int16_t imm) {
        CreateIFormat(ctx, OP_SLOAD, rd, rs, imm);
}

void CreateSstoreInst(TalContext *ctx, int8_t rs, int8_t rd, int16_t imm) {
        CreateIFormat(ctx, OP_SSTORE, rs, rd, imm);
}

void CreateIntegerData(TalContext *ctx, int data) {
        AddData(ctx, data);
}
