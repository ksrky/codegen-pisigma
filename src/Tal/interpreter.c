#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "context.h"
#include "interpreter.h"

RuntimeContext *CreateRuntimeContext(TalContext *tc) {
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
        switch (GET_OP(c)) {
        case OP_HALT:
                ctx->halt = true;
                break;
        case OP_ADD:
                REG(GET_RD(c)) = REG(GET_RS(c)) + REG(GET_RT(c));
                break;
        case OP_SUB:
                REG(GET_RD(c)) = REG(GET_RS(c)) - REG(GET_RT(c));
                break;
        case OP_MUL:
                REG(GET_RD(c)) = REG(GET_RS(c)) * REG(GET_RT(c));
                break;
        case OP_DIV:
                REG(GET_RD(c)) = REG(GET_RS(c)) / REG(GET_RT(c));
                break;
        case OP_BEQ:
                if (REG(GET_RS(c)) == REG(GET_RT(c))) PC += GET_IMM(c);
                break;
        case OP_BNE:
                if (REG(GET_RS(c)) != REG(GET_RT(c))) PC += GET_IMM(c);
                break;
        case OP_BGT:
                if (REG(GET_RS(c)) > REG(GET_RT(c))) PC += GET_IMM(c);
                break;
        case OP_BLT:
                if (REG(GET_RS(c)) < REG(GET_RT(c))) PC += GET_IMM(c);
                break;
        case OP_BGE:
                if (REG(GET_RS(c)) >= REG(GET_RT(c))) PC += GET_IMM(c);
                break;
        case OP_BLE:
                if (REG(GET_RS(c)) <= REG(GET_RT(c))) PC += GET_IMM(c);
                break;
        case OP_CALL:
                REG(RA) = (PC++) - BP;
                PC = BP + (c & ADDR_BITS);
                break;
        case OP_JUMP:
                PC = BP + (c & ADDR_BITS);
                break;
        case OP_JUMPR:
                PC = BP + REG(GET_RS(c));
                break;
        case OP_LOAD:
                REG(GET_RS(c)) = ctx->memory[REG(GET_RT(c)) + GET_IMM(c)];
                break;
        case OP_LOADI:
                REG(GET_RS(c)) = GET_IMM(c);
                break;
        case OP_MALLOC:
                REG(GET_RT(c)) = HP - BP;
                HP += REG(GET_RS(c));
                break;
        case OP_STORE:
                ctx->memory[REG(GET_RT(c)) + (GET_IMM(c))] = REG(GET_RS(c));
                break;
        case OP_UNPACK:
                break;
        case OP_SALLOC:
                SP -= c & ADDR_BITS;
                break;
        case OP_SFREE:
                SP += c & ADDR_BITS;
                break;
        case OP_SLOAD:
                REG((GET_RS(c))) = *(SP + REG(GET_RT(c)));
                break;
        case OP_SSTORE:
                *(SP + REG(GET_RT(c))) = REG(GET_RS(c));
                break;
        default:
                break;
        }
}

void Steps(RuntimeContext *ctx) {
        while (PC < ctx->prog_end && !ctx->halt) Step(ctx);
}
