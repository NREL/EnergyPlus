#include "atlas_asm.h"
#
# x86 assembler for:
# void do_vmacc(z, x, y)
# RETURNS: z[0:3] += x[0:3] *  y[0:3];
#
.text
.globl ATL_asmdecor(do_vmacc)
ATL_asmdecor(do_vmacc):
#ifdef ATL_GAS_x8664
        vmovupd 0(%rdi), %ymm0                  /* load z */
        vmovupd 0(%rsi), %ymm1                  /* load x */
        vmovupd 0(%rdx), %ymm2                  /* load y */
        vmulpd %ymm1, %ymm2, %ymm3
        vaddpd %ymm3, %ymm0, %ymm0
        vmovupd %ymm0, 0(%rdi)                  /* store z */
        ret
#else
        movl    12(%esp), %eax
        vmovupd 0(%eax), %ymm2                  /* load y */
        movl    8(%esp), %eax
        vmovupd  (%eax), %xmm1                  /* load x */
        movl    4(%esp), %eax
        vmovupd  (%eax), %xmm0                  /* load z */
        vmulpd %ymm1, %ymm2, %ymm3
        vaddpd %ymm3, %ymm0, %ymm0
        vmovupd %xmm0, 0(%eax)                  /* store z */
        ret
#endif
