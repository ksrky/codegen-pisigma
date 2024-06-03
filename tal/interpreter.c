#include <stdio.h>
#include <stdint.h>

#include "interpreter.h"

static word memory[MEM_SIZE];

static word *sp = &memory[MEM_SIZE - 1]; /* the stack pointer */

static word *pc = memory; /* the program counter */

static word *hp; /* the heap pointer. */

static word register_file[REG_SIZE];

#define NEXT goto *instructions[IDX_FROM_INS(*pc++)]

#define REG(r) (register_file + r)

int main()
{
        int fd = -1;
        FILE *src = NULL;
        int nread;
        word c = 0;

        switch (c >> OP_OFFSET)
        {
        case I_ADD:
                *REG((c >> RD_OFFSET)) = *REG((c >> RS_OFFSET)) + *REG((c >> RT_OFFSET));
        case I_SUB:
                *REG((c >> RD_OFFSET)) = *REG((c >> RS_OFFSET)) - *REG((c >> RT_OFFSET));
        case I_MUL:
                *REG((c >> RD_OFFSET)) = *REG((c >> RS_OFFSET)) * *REG((c >> RT_OFFSET));
        case I_DIV:
                *REG((c >> RD_OFFSET)) = *REG((c >> RS_OFFSET)) / *REG((c >> RT_OFFSET));
        case I_BEQ:
                if (*REG((c >> RS_OFFSET)) == *REG((c >> RT_OFFSET)))
                        pc += c & IMM_BITS;
        case I_BNE:
                if (*REG((c >> RS_OFFSET)) != *REG((c >> RT_OFFSET)))
                        pc += c & IMM_BITS;
        case I_BGT:
                if (*REG((c >> RS_OFFSET)) > *REG((c >> RT_OFFSET)))
                        pc += c & IMM_BITS;
        case I_BLT:
                if (*REG((c >> RS_OFFSET)) < *REG((c >> RT_OFFSET)))
                        pc += c & IMM_BITS;
        case I_BGE:
                if (*REG((c >> RS_OFFSET)) >= *REG((c >> RT_OFFSET)))
                        pc += c & IMM_BITS;
        case I_BLE:
                if (*REG((c >> RS_OFFSET)) <= *REG((c >> RT_OFFSET)))
                        pc += c & IMM_BITS;
        case I_CALL:
                *REG(RA) = pc++;
                pc = memory + (c & ADDR_BITS);
        case I_LOAD:
                *REG((c >> RT_OFFSET)) = memory[*REG((c >> RS_OFFSET)) + (c & IMM_BITS)];
        case I_MALLOC:
                *REG((c >> RT_OFFSET)) = hp;
                hp += *REG((c >> RS_OFFSET));
        case I_MOVE:
                *REG((c >> RT_OFFSET)) = *REG((c >> RS_OFFSET));
        case I_STORE:
                memory[*REG((c >> RS_OFFSET)) + (c & IMM_BITS)] = *REG((c >> RT_OFFSET));
        case I_UNPACK:
                break;
        case I_SALLOC:
                sp -= c & IMM_BITS;
        case I_SFREE:
                sp += c & IMM_BITS;
        case I_SLOAD:
                *REG((c >> RT_OFFSET)) = *(sp + *REG((c >> RS_OFFSET)));
        case I_SSTORE:
                *(sp + *REG((c >> RS_OFFSET))) = *REG((c >> RT_OFFSET));
        default:
                break;
        }
}
