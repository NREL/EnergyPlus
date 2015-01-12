#define ATL_GAS_ARM
#include "atlas_asm.h"
#
# Linux ARM assembler for:
# int asm_probe(int i)
# RETURNS: i*3
#
.code   32
.text
.align  2
.globl  ATL_asmdecor(asm_probe)
.type   ATL_asmdecor(asm_probe), %function
ATL_asmdecor(asm_probe):
        add     r0, r0, r0, LSL #1
        bx      lr
.size ATL_asmdecor(asm_probe),.-ATL_asmdecor(asm_probe)
