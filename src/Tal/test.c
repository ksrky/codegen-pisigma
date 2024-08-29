#include <stdio.h>
#include <stdlib.h>

#include "instruction.h"
#include "interpreter.h"

int main() {
        TalContext *ctx = CreateTalContext();
        CreateLoadiInst(ctx, RV, 1);
        CreateHaltInst(ctx);
        RuntimeContext *rctx = CreateRuntimeContext(ctx);
        Steps(rctx);
        int ret = LookupRegister(rctx, RV);
        printf(ret);
        return 0;
}