#include "atlas_asm.h"
#
# x86 assembler for:
# void do_vsum(z, x, y)
# RETURNS: z = x + y;
#
.text
.globl ATL_asmdecor(do_vsum)
ATL_asmdecor(do_vsum):
#ifdef ATL_GAS_x8664
        movq    (%rsi), %mm0
        movq    8(%rsi), %mm1
        movq    (%rdx), %mm2
        movq    8(%rdx), %mm3
        pfadd   %mm2, %mm0
        pfadd   %mm3, %mm1
        movq    %mm0, (%rdi)
        movq    %mm1, 8(%rdi)
        emms
        ret
#else
        movl    12(%esp), %eax
        movq    (%eax), %mm0
        movq    8(%eax), %mm1
        movl    8(%esp), %eax
        movq    (%eax), %mm2
        movq    8(%eax), %mm3
        pfadd   %mm2, %mm0
        pfadd   %mm3, %mm1
        movl    4(%esp), %eax
        movq    %mm0, (%eax)
        movq    %mm1, 8(%eax)
        emms
        ret
#endif
