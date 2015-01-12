#include "atlas_asm.h"
#
# x86-32 assembler for:
# int asm_probe(int i)
# RETURNS: i*3
#
.text
.globl ATL_asmdecor(asm_probe)
ATL_asmdecor(asm_probe):
        movl    4(%esp), %eax
        movl    %eax, %ecx
        shl     $1, %eax
        addl    %ecx, %eax
        ret
