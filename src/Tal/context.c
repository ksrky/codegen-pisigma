#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "context.h"
#include "spec.h"

TalContext *CreateTalContext() {
        TalContext *ctx = (TalContext *) malloc(sizeof(TalContext));
        SegmentPointers seg_ptrs = {
                .bp = ctx->memory,
                .prog_base = ctx->memory,
                .data_base = ctx->memory + TEXT_SEG_SIZE,
                .heap_base = ctx->memory + TEXT_SEG_SIZE + DATA_SEG_SIZE,
                .stack_base = ctx->memory + MEM_SIZE};
        ctx->pr = seg_ptrs.prog_base;
        ctx->dr = seg_ptrs.data_base;
        ctx->seg_ptrs = seg_ptrs;
        return ctx;
}

void AddInstruction(TalContext *ctx, word instr) {
        *(ctx->pr) = instr;
        ctx->pr++;
}

void AddData(TalContext *ctx, word data) {
        *ctx->dr = data;
        ctx->dr++;
}

int GetProgramSize(TalContext *ctx) {
        return ctx->pr - ctx->seg_ptrs.prog_base;
}

int GetProgramAddress(TalContext *ctx) {
        return ctx->pr - ctx->seg_ptrs.bp;
}

int GetDataAddress(TalContext *ctx) {
        return ctx->dr - ctx->seg_ptrs.bp;
}
