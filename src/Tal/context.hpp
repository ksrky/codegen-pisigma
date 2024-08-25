#include <stdio.h>
#include <stdint.h>

#include "spec.hpp"

typedef int32_t word;

class TalContext {
public:
        word memory[MEM_SIZE];

        word *bp = memory;

        word *pc; /* the program counter */

        word *hp; /* the heap pointer. */

        word *sp; /* the stack pointer */

        word *heap_base; /* the base of the heap */

        word *heap_end; /* the beginning of the heap */

        word *stack_base; /* the base of the stack */

        word register_file[REG_SIZE];

        TalContext();

        void addInstruction(word instr);
};
