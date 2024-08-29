#include <stdbool.h>

typedef struct {
        word *memory;
        word *bp;
        word *pc;
        word *hp;
        word *sp;
        word *prog_end;
        word register_file[REG_SIZE];
        bool halt;
} RuntimeContext;

#define BP (ctx->bp)
#define PC (ctx->pc)
#define HP (ctx->hp)
#define SP (ctx->sp)
#define REG(r) (ctx->register_file[r])

RuntimeContext *CreateRuntimeContext(TalContext *tc);