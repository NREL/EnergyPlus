#include "atlas_asm.h"
# x86-64 assembler for:
# int asm_probe(int i)
# RETURNS: i*3
#
.text
.global ATL_asmdecor(asm_probe)
ATL_asmdecor(asm_probe):
        xor     %rax, %rax
        movl    %edi, %eax
        shl     $1, %eax
        addl    %edi, %eax
        ret
