#define ATL_GAS_PPC
#include "atlas_asm.h"
/*
 * Linux PPC assembler for:
 * int asm_probe(int i)
 * RETURNS: i*3
 */
.globl ATL_asmdecor(asm_probe)
ATL_asmdecor(asm_probe):
        add     r4, r3, r3
        add     r3, r3, r4
        blr
