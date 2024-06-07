#include <stdio.h>

#include "interpreter.hpp"

class TalContext {
        word memory[MEM_SIZE];
        word register_file[REG_SIZE];
public:
        word *sp; /* the stack pointer */

        word *pc; /* the program counter */

        word *hp; /* the heap pointer. */

        word *prog_end; /* the end of the program */

        word *heap_base; /* the base of the heap */

        word *heap_end; /* the beginning of the heap */

        word heap_size; /* the size of the heap */

        inline word calcOffset(word *ptr);

        inline word calcPtr(int offset);

        inline word getReg(int r);
};
