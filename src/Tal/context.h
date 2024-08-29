#include <stdint.h>

#include "spec.h"

typedef int32_t word;

typedef struct {
        word *bp;         /* the base pointer */
        word *prog_base;  /* the base of the program segment */
        word *data_base;  /* the base of the data segment */
        word *heap_base;  /* the base of the heap */
        word *stack_base; /* the base of the stack */
} SegmentPointers;

typedef struct {
        word memory[MEM_SIZE];
        word *pr; /* the program segment register */
        word *dr; /* the data segment register */
        SegmentPointers seg_ptrs;
} TalContext;

TalContext *CreateTalContext();

void AddInstruction(TalContext *ctx, word instr);

void AddData(TalContext *ctx, word data);

int GetProgramSize(TalContext *ctx);