#include <stdio.h>
#include <stdint.h>

#include "context.hpp"

TalContext::TalContext() {
        pr = prog_base;
        dr = data_base;
}

void TalContext::addInstruction(word instr) {
        *pr = instr;
        pr++;
}

void TalContext::addData(word data) {
        *dr = data;
        dr++;
}

int TalContext::getProgramEnd() {
        return pr - bp;
}

int TalContext::getDataEnd() {
        return dr - bp;
}
