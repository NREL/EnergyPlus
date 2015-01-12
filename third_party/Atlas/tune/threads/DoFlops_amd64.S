#include "atlas_asm.h"
/*
 * function is: void DoFlops(size_t nflops);
 */
#define nflops  %rdi
#define N       %rax
#define ZR  %xmm0
#define A0  %xmm1
#define B0  %xmm2
#define B1  %xmm3
#define B2  %xmm4
#define B3  %xmm5
#define B4  %xmm6
#define C0  %xmm7
#define C1  %xmm8
#define C2  %xmm9
#define C3  %xmm10
#define C4  %xmm11
#define C5  %xmm12
#define C6  %xmm13
#define C7  %xmm14

.text
.globl ATL_asmdecor(DoFlops)
ATL_asmdecor(DoFlops):
/*
 * Zero all xmm regs
 */
   xorps %xmm0, %xmm0
   xorps %xmm1, %xmm1
   xorps %xmm2, %xmm2
   xorps %xmm3, %xmm3
   xorps %xmm4, %xmm4
   xorps %xmm5, %xmm5
   xorps %xmm6, %xmm6
   xorps %xmm7, %xmm7
   xorps %xmm8, %xmm8
   xorps %xmm9, %xmm9
   xorps %xmm10, %xmm10
   xorps %xmm11, %xmm11
   xorps %xmm12, %xmm12
   xorps %xmm13, %xmm13
   xorps %xmm14, %xmm14
   xorps %xmm15, %xmm15
/*
 * This loop adds into 8 different accumulators, after doing a chained
 * multiplication.  The number of flops is therefore:
 *   (veclen)*(vecflops)*(naccum) = 4 * 2 * 8 = 64 flops/iteration
 */
   movq nflops, N
   shr  $6, N      /* N = nflops / 64 */
   LOOPN:
/*
 *    On Intel chips, you need to write all read registers once every loop
 *    iteration, or you cannot achieve peak.
 */
      mulps A0, B0
      addps B0, C0
      mulps A0, B1
      addps B1, C1
      mulps A0, B2
      addps B2, C2
      mulps A0, B3
      addps B3, C3
      mulps A0, B0
      addps B0, C4
      mulps A0, B1
      addps B1, C5
      mulps A0, B2
      addps B2, C6
      mulps A0, B3
      addps B3, C7
      #ifdef Intel
         xorps A0, A0
      #endif
   sub $1, N
   jnz LOOPN

ret
