#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "context.h"

#define REG(r) (ctx->register_file[r]) 

typedef struct {
        word *memory;
        word *bp;
        word *pc;
        word *hp;
        word *sp;
        word *prog_end;
        word register_file[REG_SIZE];
} RuntimeContext;

void CreateRuntimeContext(TalContext *tc) {
        RuntimeContext *ctx = (RuntimeContext*)malloc(sizeof(RuntimeContext));
        ctx->memory = tc->memory;
        ctx->bp = tc->seg_ptrs.bp;
        ctx->pc = tc->seg_ptrs.prog_base;
        ctx->hp = tc->seg_ptrs.heap_base;
        ctx->sp = tc->seg_ptrs.stack_base;
        ctx->prog_end = tc->seg_ptrs.prog_base + GetProgramSize(tc);
}

void step(RuntimeContext *ctx) {
        word c = *(ctx->pc);
        ctx->pc++;
        switch (c >> OP_OFFSET) {
        case OP_HALT:
                exit(REG(0));
        case OP_ADD: 
                REG(c >> RD_OFFSET) = REG(c >> RS_OFFSET) + REG(c >> RT_OFFSET);
        case OP_SUB:
                REG(c >> RD_OFFSET) = REG(c >> RS_OFFSET) - REG(c >> RT_OFFSET);
        case OP_MUL:
                REG(c >> RD_OFFSET) = REG(c >> RS_OFFSET) * REG(c >> RT_OFFSET);
        case OP_DIV:
                REG(c >> RD_OFFSET) = REG(c >> RS_OFFSET) / REG(c >> RT_OFFSET);
        case OP_BEQ:
                if (REG(c >> RS_OFFSET) == REG(c >> RT_OFFSET)) ctx->pc += c & IMM_BITS;
        case OP_BNE:
                if (REG(c >> RS_OFFSET) != REG(c >> RT_OFFSET)) ctx->pc += c & IMM_BITS;
        case OP_BGT:
                if (REG(c >> RS_OFFSET) > REG(c >> RT_OFFSET)) ctx->pc += c & IMM_BITS;
        case OP_BLT:
                if (REG(c >> RS_OFFSET) < REG(c >> RT_OFFSET)) ctx->pc += c & IMM_BITS;
        case OP_BGE:
                if (REG(c >> RS_OFFSET) >= REG(c >> RT_OFFSET)) ctx->pc += c & IMM_BITS;
        case OP_BLE:
                if (REG(c >> RS_OFFSET) <= REG(c >> RT_OFFSET)) ctx->pc += c & IMM_BITS;
        case OP_CALL:
                REG(RA) = (ctx->pc++) - ctx->bp;
                ctx->pc = ctx->bp + (c & ADDR_BITS);
        case OP_JUMP:
                ctx->pc = ctx->bp + (c & ADDR_BITS);
        case OP_LOAD:
                REG(c >> RT_OFFSET) = ctx->memory[REG(c >> RS_OFFSET) + (c & IMM_BITS)];
        case OP_LOADI:
                REG(c >> RT_OFFSET) = REG(c >> RS_OFFSET);
        case OP_MALLOC:
                REG(c >> RT_OFFSET) = ctx->hp - ctx->bp;
                ctx->hp += REG(c >> RS_OFFSET);
        case OP_STORE:
                ctx->memory[REG(c >> RS_OFFSET) + (c & IMM_BITS)] = REG(c >> RT_OFFSET);
        case OP_UNPACK:
                break;
        case OP_SALLOC:
                ctx->sp -= c & ADDR_BITS;
        case OP_SFREE:
                ctx->sp += c & ADDR_BITS;
        case OP_SLOAD:
                REG((c >> RT_OFFSET)) = *(ctx->sp + REG(c >> RS_OFFSET));
        case OP_SSTORE:
                *(ctx->sp + REG(c >> RS_OFFSET)) = REG(c >> RT_OFFSET);
        default:
                break;
        }
}

void steps (RuntimeContext *ctx) {
        while (ctx->pc < ctx->prog_end) step(ctx);
}
