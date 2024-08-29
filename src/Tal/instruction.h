#include "context.h"

void CreateHaltInst(TalContext *ctx);
void CreateAddInst(TalContext *ctx, int8_t rd, int8_t rs, int8_t rt);
void CreateSubInst(TalContext *ctx, int8_t rd, int8_t rs, int8_t rt);
void CreateMulInst(TalContext *ctx, int8_t rd, int8_t rs, int8_t rt);
void CreateDivInst(TalContext *ctx, int8_t rd, int8_t rs, int8_t rt);
void CreateBeqInst(TalContext *ctx, int8_t rs, int8_t rt, int16_t imm);
void CreateBneInst(TalContext *ctx, int8_t rs, int8_t rt, int16_t imm);
void CreateBgtInst(TalContext *ctx, int8_t rs, int8_t rt, int16_t imm);
void CreateBltInst(TalContext *ctx, int8_t rs, int8_t rt, int16_t imm);
void CreateBgeInst(TalContext *ctx, int8_t rs, int8_t rt, int16_t imm);
void CreateBleInst(TalContext *ctx, int8_t rs, int8_t rt, int16_t imm);
void CreateCallInst(TalContext *ctx, int addr);
void CreateJumpInst(TalContext *ctx, int addr);
void CreateJumpRInst(TalContext *ctx, int8_t rs);
void CreateLoadInst(TalContext *ctx, int8_t rd, int8_t rs, int16_t imm);
void CreateLoadiInst(TalContext *ctx, int8_t rd, int16_t imm);
void CreateMallocInst(TalContext *ctx, int8_t rd, int16_t imm);
void CreateStoreInst(TalContext *ctx, int8_t rs, int8_t rd, int16_t imm);
void CreateUnpackInst(TalContext *ctx, int8_t rd, int8_t rs);
void CreateSallocInst(TalContext *ctx, int8_t rd, int16_t addr);
void CreateSfreeInst(TalContext *ctx, int8_t rd, int16_t addr);
void CreateSloadInst(TalContext *ctx, int8_t rd, int8_t rs, int16_t imm);
void CreateSstoreInst(TalContext *ctx, int8_t rs, int8_t rd, int16_t imm);
void CreateIntegerData(TalContext *ctx, int data);
