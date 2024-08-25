#include <stdio.h>
#include <stdint.h>

#include "spec.hpp"

typedef int32_t word;

class TalContext {
        int *pr; /* the program segment register */
        int *dr; /* the data segment register */
public:
        word memory[MEM_SIZE];
        word *bp = memory; /* the base pointer */
        word *prog_base = memory; /* the base of the program segment */
        word *data_base = memory + TEXT_SEG_SIZE; /* the base of the data segment */
        word *heap_base = memory + TEXT_SEG_SIZE + DATA_SEG_SIZE; /* the base of the heap */
        word *stack_base = memory + MEM_SIZE; /* the base of the stack */

        TalContext();

        void addInstruction(word instr);
        void addData(word data);

        int getProgramEnd();
        int getDataEnd();
};
