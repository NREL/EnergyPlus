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
        movupd  0(%rsi), %xmm0
        movupd  0(%rdx), %xmm1
        addpd   %xmm1, %xmm0
        movupd  %xmm0, 0(%rdi)
        ret
#else
        movl    12(%esp), %eax
        movupd  0(%eax), %xmm0
        movl    8(%esp), %eax
        movupd  (%eax), %xmm1
        addpd   %xmm1, %xmm0
        movl    4(%esp), %eax
        movupd  %xmm0, 0(%eax)
        ret
#endif
