#include <stdio.h>
#include <stdint.h>

#include "context.hpp"

TalContext::TalContext() {
        pc = bp;
        heap_base = bp;
        hp = heap_base;
        stack_base = bp + MEM_SIZE;
        sp = stack_base;
}

void TalContext::addInstruction(word instr) {
        *heap_base = instr;
        heap_base++;
}

extern "C" {
        TalContext* TalContext_new() {
                return new TalContext();
        }

        void TalContext_addInstruction(TalContext* ctx, word instr) {
                ctx->addInstruction(instr);
        }

        void TalContext_delete(TalContext* ctx) {
                delete ctx;
        }
}