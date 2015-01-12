
#include "atlas_asm.h"
#ifndef ATL_GAS_x8664
   #error "This kernel requires x86-64 assembly!"
#endif
/*
 * Register usage
 */
#define M       %rdi  /* already in */
#define N       %rsi  /* already in */
#define itmp    %rax  /* tmp used for byte load/use pipeline */
#define itb     %al
#define pX      %rdx  /* already in */
#define pA0     %rcx  /* 8(%rsp) */
#define Ab      %cl
#define II      %rbx  /* loaded in loop */
#define pY      %rbp  /* moved from r9 */
#define incAn   %r8   /* 16(%rsp) */
#define mask    %r9
#define Mr      %r10
#define mask7   %r11
#define incX    %r12

#define y0      %xmm0
#define x0      %xmm1

/*
void ATL_UGERK
          %rdi        %rsi              %xmm0           %rdx
   (ATL_CINT M, ATL_CINT N, const TYPE alpha0, const TYPE *X,
             %rcx            %r8            %r9  8(%rsp)  16(%rsp)
    ATL_CINT incX, const TYPE *Y, ATL_CINT incY, TYPE *A, ATL_CINT lda)

*/
.text
.global ATL_asmdecor(ATL_UGERK)
ALIGN64
ATL_asmdecor(ATL_UGERK):

/*
 * Save callee-saved iregs
 */
   movq %rbp, -8(%rsp)
   movq %rbx, -16(%rsp)
   movq %r12, -24(%rsp)
#if 0
   movq %r13, -32(%rsp)
   movq %r14, -40(%rsp)
   movq %r15, -48(%rsp)
#endif
/*
 * Load & compute all integer variables
 */
   movq 8(%rsp), pA0
   movq %r8, pY
   movslq 16(%rsp), incAn       /* incAn = lda */
   sub  M, incAn                /* incAn = lda - M */
   shl  $3, incAn               /* incAn = (lda-M)*sizeof */
   movq $0x3F, mask
   movq $0x7, mask7
   not  mask7
   mov M, incX                  /* incX = M */
   shl $3, incX                 /* incX = M*sizeof */

   lea (pY, N, 8), pY           /* pY += N */
   neg N                        /* N = -N */
/*
 * Start N-loop
 */
   NLOOP:
      movb (pA0), itb           /* forced fetch of pA0 */
      movb itb, (pA0)           /* force cache write coherence message */

      movddup (pY,N,8), y0      /* y0 = {y0, y0} */
      mov M, II

/*
 *    Align data on CL boundary
 */
      test mask, pA0            /* if (pA0 & 0x3F) --> is 64-byte aligned */
      jz CLALIGNED              /* start aligned loop */
      LOOPALIGNCL:              /* loop until aligned or out of ops */
         movsd (pX), x0         /* x0 = {xx, x0} */
         mulsd y0, x0           /* x0 = {xx, x0*y0} */
         addsd (pA0), x0        /* x0 = {xx, a00+x0*y0} */
         movsd x0, (pA0)
         add $8, pA0
         add $8, pX
         sub       $1, II
         jz MLOOPDONE           /* finish MLOOP if out of M */
      test mask, pA0            /* if (pA0 & 0x3F) --> is 64-byte aligned */
      jnz LOOPALIGNCL           /* continue until aligned */

CLALIGNED:
      mov II, Mr                /* Mr = remaining iterations */
      and mask7, II             /* II = ((remaining iter)/8)*8 */
      jz  ROLLED_DO_MR          /* if nothing left, goto cleanup loop */
      sub II, Mr                /* Mr = # of iter left after II (mul8) done */

      sub $8, II                /* stop 1 iter early; if not enough its left */
      jbe ROLLED_ADD8           /* handle everything in rolled loop */

/*
 *    This loop starts at a 64-byte cache line boundary
 */
      movapd (pX), x0           /* x0 = {x1, x0}, pipelined out of loop */
      MLOOP:
         movb 64(pA0), itb      /* forced fetch of pA0 */
         movb itb, 64(pA0)      /* force cache write coherence message */
         mulpd y0, x0           /* x0 = {x1*y0, x0*y0} */
         addpd (pA0), x0        /* x0 = {a10+x1*y0, a00+x0*y0} */
         movapd x0, (pA0)

         movapd 16(pX),x0       /* x0 = {x1, x0} */
         mulpd y0, x0           /* x0 = {x1*y0, x0*y0} */
         addpd 16(pA0), x0      /* x0 = {a10+x1*y0, a00+x0*y0} */
         movapd x0, 16(pA0)

         movapd 32(pX),x0       /* x0 = {x1, x0} */
         mulpd y0, x0           /* x0 = {x1*y0, x0*y0} */
         addpd 32(pA0), x0        /* x0 = {a10+x1*y0, a00+x0*y0} */
         movapd x0, 32(pA0)

         movapd 48(pX),x0       /* x0 = {x1, x0} */
         add    $64, pX
         mulpd y0, x0           /* x0 = {x1*y0, x0*y0} */
         addpd 48(pA0), x0      /* x0 = {a10+x1*y0, a00+x0*y0} */
         movapd x0, 48(pA0)

         movapd (pX),x0 /* x0 = {x1, x0}, SW pipelined */
         add    $64, pA0
      sub       $8, II
      jnz MLOOP
/*
 *    Drain preloading pipe
 */
      mulpd y0, x0           /* x0 = {x1*y0, x0*y0} */
      addpd (pA0), x0        /* x0 = {a10+x1*y0, a00+x0*y0} */
      movapd x0, (pA0)

      movapd 16(pX),x0          /* x0 = {x1, x0} */
      mulpd y0, x0              /* x0 = {x1*y0, x0*y0} */
      addpd 16(pA0), x0         /* x0 = {a10+x1*y0, a00+x0*y0} */
      movapd x0, 16(pA0)

      movapd 32(pX),x0         /* x0 = {x1, x0} */
      mulpd y0, x0              /* x0 = {x1*y0, x0*y0} */
      addpd 32(pA0), x0         /* x0 = {a10+x1*y0, a00+x0*y0} */
      movapd x0, 32(pA0)

      movapd 48(pX),x0          /* x0 = {x1, x0} */
      mulpd y0, x0              /* x0 = {x1*y0, x0*y0} */
      addpd 48(pA0), x0         /* x0 = {a10+x1*y0, a00+x0*y0} */
      movapd x0, 48(pA0)
         add $64, pX
         add $64, pA0
      test Mr, Mr
      jnz  ROLLED_DO_MR

MLOOPDONE:
      sub incX, pX
      add incAn, pA0
   add $1, N
   jnz NLOOP

/*
 * EPILOGUE: restore registers and return
 */
   movq -8(%rsp), %rbp
   movq -16(%rsp), %rbx
   movq -24(%rsp), %r12
#if 0
   movq %r13, -32(%rsp), %r13
   movq %r14, -40(%rsp), %r14
   movq %r15, -48(%rsp), %r15
#endif
   ret

ROLLED_ADD8:
   lea 8(II, Mr), II
   jmp MLOOP_ROLLED
ROLLED_DO_MR:
   mov Mr, II
MLOOP_ROLLED:
   movsd (pX), x0       /* x0 = {xx, x0} */
   mulsd y0, x0         /* x0 = {xx, x0*y0} */
   addsd (pA0), x0      /* x0 = {xx, a00+x0*y0} */
   movsd x0, (pA0)
   add $8, pX
   add $8, pA0          /* pA0++ */
sub  $1, II
jnz MLOOP_ROLLED
jmp MLOOPDONE
