#include "atlas_asm.h"

; MIPS assembler for:
; int asm_probe(int i)
; RETURNS: i*3
.globl ATL_asmdecor(asm_probe)
ATL_asmdecor(asm_probe):
        daddu   $2, $4, $4
        j       $31
        daddu   $2, $2, $4    /* delay slot of return */
