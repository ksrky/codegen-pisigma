#include <stdio.h>

#include "interpreter.hpp"

class TalContext {
        word memory[MEM_SIZE];
        word stack[MEM_SIZE];
        word register_file[REG_SIZE];
public:
        const word *bp = memory;

        word *sp = &stack[MEM_SIZE - 1]; /* the stack pointer */

        word *pc; /* the program counter */

        word *hp; /* the heap pointer. */

        word *prog_end; /* the end of the program */

        word *heap_base; /* the base of the heap */

        word *heap_end; /* the beginning of the heap */

        word heap_size; /* the size of the heap */

        TalContext() {}

        void loadProgram() {}

        inline int calcOffset(word *ptr) { return ptr - memory; }

        inline int calcPtr(int offset) { return memory + offset; }

        inline word getReg(int r) { return register_file[r]; }
};
