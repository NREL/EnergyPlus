#include "atlas_asm.h"
#
# x86 assembler for:
# void do_vsum(z, x, y)
# RETURNS: z[0] = x[0]+x[1]; z[1] = y[0]+y[1];
#
.text
.globl ATL_asmdecor(do_vsum)
ATL_asmdecor(do_vsum):
#ifdef ATL_GAS_x8664
        movupd  0(%rsi), %xmm0
        movupd  0(%rdx), %xmm1
        haddpd  %xmm1, %xmm0
        movupd  %xmm0, 0(%rdi)
        ret
#else
        movl    12(%esp), %eax
        movupd  0(%eax), %xmm0
        movl    8(%esp), %eax
        movupd  (%eax), %xmm1
        haddpd  %xmm0, %xmm1
        movl    4(%esp), %eax
        movupd  %xmm1, 0(%eax)
        ret
#endif
