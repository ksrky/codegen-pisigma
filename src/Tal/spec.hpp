/*
 * Machine specification
 */
#define MEM_SIZE (1 << 18)
#define REG_SIZE 16

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
#define J_FORMAT(op, addr) ((op) << OP_OFFSET | addr)

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

#define OPCODE(i) (i)

#define OP_HALT OPCODE(0)
#define OP_ADD OPCODE(1)
#define OP_SUB OPCODE(2)
#define OP_MUL OPCODE(3)
#define OP_DIV OPCODE(4)
#define OP_BEQ OPCODE(5)
#define OP_BNE OPCODE(6)
#define OP_BGT OPCODE(7)
#define OP_BLT OPCODE(8)
#define OP_BGE OPCODE(9)
#define OP_BLE OPCODE(10)
#define OP_CALL OPCODE(11)
#define OP_JUMP OPCODE(12)
#define OP_LOAD OPCODE(13)
#define OP_LOADI OPCODE(14)
#define OP_MALLOC OPCODE(15)
#define OP_STORE OPCODE(16)
#define OP_UNPACK OPCODE(17)
#define OP_SALLOC OPCODE(18)
#define OP_SFREE OPCODE(19)
#define OP_SLOAD OPCODE(20)
#define OP_SSTORE OPCODE(21)

