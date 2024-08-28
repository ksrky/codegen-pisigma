#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "context.h"

typedef struct {
        word *memory;
        word *bp;
        word *pc;
        word *hp;
        word *sp;
        word *prog_end;
        word register_file[REG_SIZE];
        bool halt;
} RuntimeContext;

#define BP (ctx->bp)
#define PC (ctx->pc)
#define HP (ctx->hp)
#define SP (ctx->sp)
#define REG(r) (ctx->register_file[r])

RuntimeContext *CreateRuntimeContext(TalContext *tc) {
        printf("CreateRuntimeContext");
        RuntimeContext *ctx = (RuntimeContext *) malloc(sizeof(RuntimeContext));
        ctx->memory = tc->memory;
        BP = tc->seg_ptrs.bp;
        PC = tc->seg_ptrs.prog_base;
        HP = tc->seg_ptrs.heap_base;
        SP = tc->seg_ptrs.stack_base;
        ctx->sp = tc->seg_ptrs.stack_base;
        ctx->prog_end = tc->seg_ptrs.prog_base + GetProgramSize(tc);
        ctx->register_file[ZR] = 0;
        ctx->halt = false;
        return ctx;
}

int LookupRegister(RuntimeContext *ctx, int reg) {
        return REG(reg);
}

void Step(RuntimeContext *ctx) {
        word c = *(ctx->pc)++;
        switch (c >> OP_OFFSET) {
        case OP_HALT:
                ctx->halt = true;
        case OP_ADD:
                REG(c >> RD_OFFSET) = REG(c >> RS_OFFSET) + REG(c >> RT_OFFSET);
        case OP_SUB:
                REG(c >> RD_OFFSET) = REG(c >> RS_OFFSET) - REG(c >> RT_OFFSET);
        case OP_MUL:
                REG(c >> RD_OFFSET) = REG(c >> RS_OFFSET) * REG(c >> RT_OFFSET);
        case OP_DIV:
                REG(c >> RD_OFFSET) = REG(c >> RS_OFFSET) / REG(c >> RT_OFFSET);
        case OP_BEQ:
                if (REG(c >> RS_OFFSET) == REG(c >> RT_OFFSET)) PC += c & IMM_BITS;
        case OP_BNE:
                if (REG(c >> RS_OFFSET) != REG(c >> RT_OFFSET)) PC += c & IMM_BITS;
        case OP_BGT:
                if (REG(c >> RS_OFFSET) > REG(c >> RT_OFFSET)) PC += c & IMM_BITS;
        case OP_BLT:
                if (REG(c >> RS_OFFSET) < REG(c >> RT_OFFSET)) PC += c & IMM_BITS;
        case OP_BGE:
                if (REG(c >> RS_OFFSET) >= REG(c >> RT_OFFSET)) PC += c & IMM_BITS;
        case OP_BLE:
                if (REG(c >> RS_OFFSET) <= REG(c >> RT_OFFSET)) PC += c & IMM_BITS;
        case OP_CALL:
                REG(RA) = (PC++) - BP;
                PC = BP + (c & ADDR_BITS);
        case OP_JUMP:
                PC = BP + (c & ADDR_BITS);
        case OP_JUMPR:
                PC = BP + REG(c >> RS_OFFSET);
        case OP_LOAD:
                REG(c >> RS_OFFSET) = ctx->memory[REG(c >> RT_OFFSET) + (c & IMM_BITS)];
        case OP_LOADI:
                REG(c >> RS_OFFSET) = REG(c & IMM_BITS);
        case OP_MALLOC:
                REG(c >> RT_OFFSET) = HP - BP;
                HP += REG(c >> RS_OFFSET);
        case OP_STORE:
                ctx->memory[REG(c >> RT_OFFSET) + (c & IMM_BITS)] = REG(c >> RS_OFFSET);
        case OP_UNPACK:
                break;
        case OP_SALLOC:
                SP -= c & ADDR_BITS;
        case OP_SFREE:
                SP += c & ADDR_BITS;
        case OP_SLOAD:
                REG((c >> RS_OFFSET)) = *(SP + REG(c >> RT_OFFSET));
        case OP_SSTORE:
                *(SP + REG(c >> RT_OFFSET)) = REG(c >> RS_OFFSET);
        default:
                break;
        }
}

void Steps(RuntimeContext *ctx) {
        while (PC < ctx->prog_end && !ctx->halt) Step(ctx);
}
