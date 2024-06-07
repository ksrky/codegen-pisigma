#include <stdint.h>
#include <stdio.h>

#include "interpreter.hpp"
#include "context.hpp"

static TalContext *ctx;

static word memory[MEM_SIZE];

static word *sp = &memory[MEM_SIZE - 1]; /* the stack pointer */

static word *pc = memory; /* the program counter */

static word *hp; /* the heap pointer. */

static word *prog_end; /* the end of the program */

static word *heap_base; /* the base of the heap */

static word *heap_end; /* the beginning of the heap */

static word heap_size; /* the size of the heap */

static word register_file[REG_SIZE];

#define REG(r) (register_file[r])

int load_instructions() {
        return 0;
}

void interpreter(word c) {
        switch (c >> OP_OFFSET) {
        case OP_ADD:
                REG(c >> RD_OFFSET) = REG(c >> RS_OFFSET) + REG(c >> RT_OFFSET);
        case OP_SUB:
                REG(c >> RD_OFFSET) = REG(c >> RS_OFFSET) - REG(c >> RT_OFFSET);
        case OP_MUL:
                REG(c >> RD_OFFSET) = REG(c >> RS_OFFSET) * REG(c >> RT_OFFSET);
        case OP_DIV:
                REG(c >> RD_OFFSET) = REG(c >> RS_OFFSET) / REG(c >> RT_OFFSET);
        case OP_BEQ:
                if (REG(c >> RS_OFFSET) == REG(c >> RT_OFFSET)) pc += c & IMM_BITS;
        case OP_BNE:
                if (REG(c >> RS_OFFSET) != REG(c >> RT_OFFSET)) pc += c & IMM_BITS;
        case OP_BGT:
                if (REG(c >> RS_OFFSET) > REG(c >> RT_OFFSET)) pc += c & IMM_BITS;
        case OP_BLT:
                if (REG(c >> RS_OFFSET) < REG(c >> RT_OFFSET)) pc += c & IMM_BITS;
        case OP_BGE:
                if (REG(c >> RS_OFFSET) >= REG(c >> RT_OFFSET)) pc += c & IMM_BITS;
        case OP_BLE:
                if (REG(c >> RS_OFFSET) <= REG(c >> RT_OFFSET)) pc += c & IMM_BITS;
        case OP_CALL:
                REG(RA) = (pc++) - memory;
                pc = memory + (c & ADDR_BITS);
        case OP_JUMP:
                pc = memory + (c & ADDR_BITS);
        case OP_LOAD:
                REG(c >> RT_OFFSET) = memory[REG(c >> RS_OFFSET) + (c & IMM_BITS)];
        case OP_MALLOC:
                REG(c >> RT_OFFSET) = hp - memory;
                hp += REG(c >> RS_OFFSET);
        case OP_MOVE:
                REG(c >> RT_OFFSET) = REG(c >> RS_OFFSET);
        case OP_STORE:
                memory[REG(c >> RS_OFFSET) + (c & IMM_BITS)] = REG(c >> RT_OFFSET);
        case OP_UNPACK:
                break;
        case OP_SALLOC:
                sp -= c & ADDR_BITS;
        case OP_SFREE:
                sp += c & ADDR_BITS;
        case OP_SLOAD:
                REG((c >> RT_OFFSET)) = *(sp + REG(c >> RS_OFFSET));
        case OP_SSTORE:
                *(sp + REG(c >> RS_OFFSET)) = REG(c >> RT_OFFSET);
        default:
                break;
        }
}

int main() {
        ctx = new TalContext();
        int prog_size = load_instructions();
        prog_end = heap_base = hp = memory + prog_size;
        heap_size = (MEM_SIZE - STACK_SIZE - prog_size) / 2;
        heap_end = hp + heap_size;

        while (pc < prog_end) {
                interpreter(*(pc++));
        }
        return 0;
}
