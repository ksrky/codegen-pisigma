#define PTR_TARGET_BITS 18
#define PTR_SIZE_MASK ((1 << (DATA_BITS - PTR_TARGET_BITS)) - 1)

/*
 * VM specifications
 */
typedef int32_t word;
typedef uint32_t uword;

#define FLAG_BITS 2
#define DATA_BITS ((sizeof(word) << 3) - FLAG_BITS)
#define FLAG_MASK (((1 << FLAG_BITS) - 1) << DATA_BITS)
#define DATA_MASK (~FLAG_MASK)

#define NUM (0x3 << DATA_BITS)
#define LCONST (0x1 << DATA_BITS)
#define VCONST (0x0)
#define PTR (0x2 << DATA_BITS)

#define CHAR_FLAG (0x00800000)

#define MAKE_VCONST(x) (x)
#define MAKE_LCONST(x) (((x) & ~NUM) | LCONST)
#define MAKE_CHAR(x) (CHAR_FLAG | ((x) & 0x007fffff))
#define MAKE_NUM(x) (((x) & ~NUM) | NUM)

#define MEM_SIZE (1 << PTR_TARGET_BITS)
#define STACK_SIZE 0x400

#define REG_SIZE 32

/*
 * Instruction format
 */
#define OP_OFFSET 26 /* OP_BITS = 6 */
#define RS_OFFSET 21 /* RS_BITS = 5 */
#define RT_OFFSET 16 /* RT_BITS = 5 */
#define IMM_BITS 16
#define I_FORMAT(op, rs, rt, imm) \
        ((op) << OP_OFFSET |      \
         (rs) << RS_OFFSET |      \
         (rt) << RT_OFFSET |      \
         imm)

#define ADDR_BITS 26
#define J_FORMAT (op, addr)((op) << OP_OFFSET | addr)

#define RD_OFFSET 11 /* RD_BITS = 5 */
#define SHAMT_OFFSET 6
#define FUNCT_BITS 6
#define R_FORMAT(op, rs, rt, rd, shamt, funct) \
        ((op) << OP_OFFSET |                   \
         (rs) << RS_OFFSET |                   \
         (rt) << RT_OFFSET |                   \
         (rd) << RD_OFFSET |                   \
         (shamt) << SHAMT_OFFSET |             \
         funct)

/*
 * Registers
 */
#define ZR 0
#define RA 1
#define SP 2
#define RV 3
#define A0 4
#define A1 5
#define A2 6
#define A3 7
#define R0 8
#define R1 9
#define R2 10
#define R3 11
#define R4 12
#define R5 13
#define R6 14
#define R7 15

/*
 * Instructions
 */
#define OPCODE(i) MAKE_VCONST(i)

/* R FORMAT */
#define I_ADD OPCODE(0)
#define I_SUB OPCODE(1)
#define I_MUL OPCODE(2)
#define I_DIV OPCODE(3)
#define I_BEQ OPCODE(4)
#define I_BNE OPCODE(5)
#define I_BGT OPCODE(6)
#define I_BLT OPCODE(7)
#define I_BGE OPCODE(8)
#define I_BLE OPCODE(9)

/* J FORMAT */
#define I_CALL OPCODE(10)

/* I_FORMAT */
#define I_LOAD OPCODE(11)
#define I_MALLOC OPCODE(12)
#define I_MOVE OPCODE(13)
#define I_STORE OPCODE(14)
#define I_UNPACK OPCODE(15)
#define I_SALLOC OPCODE(16)
#define I_SFREE OPCODE(17)
#define I_SLOAD OPCODE(18)
#define I_SSTORE OPCODE(19)
