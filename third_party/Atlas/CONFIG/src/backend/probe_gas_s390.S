#define ATL_GAS_PPC
#include "atlas_asm.h"
/*
 * Linux S390 assembler for:
 * int asm_probe(int i)
 * RETURNS: i*3
 */
.globl ATL_asmdecor(asm_probe)
ATL_asmdecor(asm_probe):
        lr r3,r2
        ar r2,r3
        ar r2,r3
        br r14
