/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions, and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *   3. The name of the ATLAS group or the names of its contributers may
 *      not be used to endorse or promote products derived from this
 *      software without specific written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE ATLAS GROUP OR ITS CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 */
#include "atlas_asm.h"
#ifndef ATL_GAS_x8664
   #error "This kernel requires x86-64 assembly!"
#endif
#ifndef ATL_SSE3
   #error "This routine requires SSE3!"
#endif
/*
 * Register usage
 */
#define M       %rdi  /* already in */
#define N       %rsi  /* already in */
#define II      %rax  /* loaded in loop */
#define pX      %rdx  /* already in */
#define pA0     %r11  /* 56(%rsp) */
#define pA1     %rbx  /* computed */
#define pY      %rcx  /* already in */
#define pZ      %r9   /* already in */
#define lda     %r10  /* 16(%rsp) */
#define pW      %r8   /* already in */

#define a0      %xmm0
#define x0      %xmm1
#define revx0   %xmm2
#define a1      %xmm3
#define A0      %xmm4
#define y0r     %xmm5
#define y0i     %xmm6
#define y1r     %xmm7
#define y1i     %xmm8
#define z0r     %xmm9
#define z0i     %xmm10
#define z1r     %xmm11
#define z1i     %xmm12
#define vposneg %xmm15

#define movapd movaps
/*
void ATL_UGER2K
          %rdi        %rsi           %rdx           %rcx
   (ATL_CINT M, ATL_CINT N, const TYPE *X, const TYPE *Y,
              %r8            %r9  8(%rsp)       16(%rsp)
    const TYPE *W, const TYPE *Z, TYPE *A, ATL_CINT lda);
*/
.text
.global ATL_asmdecor(ATL_UGER2K)
ALIGN64
ATL_asmdecor(ATL_UGER2K):

/*
 * construct vector that has -1.0 in low word, and 1.0 in high word
 */
   fld1               /* ST = 1.0 */
   fstl -16(%rsp)
   fchs               /* ST = -1.0 */
   fstpl  -24(%rsp)
   movapd -24(%rsp), vposneg      /* vposneg = {1.0, -1.0} */
/*
 * Save callee-saved regs
 */
   movq %rbx, -8(%rsp)
/*
 * Load & compute all integer variables
 */
   movslq 16(%rsp), lda
   shl  $4, lda         /* lda *= sizeof */
   movq 8(%rsp), pA0
   lea (pA0, lda), pA1   /* pA1 = pA0 + lda */

   lea -2(M,M), M              /* M = 2(M-1) */
//   add M, M                     /* M = 2*M */
   lea (pX, M, 8), pX           /* pX += 2*M */
   lea (pW, M, 8), pW           /* pW += 2*M */
   lea (pA0, M, 8), pA0         /* pA0 += 2*M */
   lea (pA0, lda), pA1          /* pA1 next column over */
   neg M                        /* M = -M */
/*
 * We assume N is a multiple of 2 for this loop
 */
   LOOPN:
      movddup (pY), y0r         /* y0r = {y0r, y0r} */
      movddup 8(pY), y0i        /* y0i = {y0i, y0i} */
      mulpd   vposneg, y0i      /* y0i = {y0i,-y0i} */
      movddup 16(pY), y1r       /* y1r = {y1r, y1r} */
      movddup 24(pY), y1i       /* y1i = {y1i, y1i} */
      mulpd   vposneg, y1i      /* y0i = {y1i,-y1i} */
         movapd (pX,M,8), x0           /* x0 = {x0i, x0r} */
      add     $32, pY
#
      movddup (pZ), z0r         /* z0r = {z0r, z0r} */
      movddup 8(pZ), z0i        /* z0i = {z0i, z0i} */
      mulpd   vposneg, z0i      /* z0i = {z0i,-z0i} */
      movddup 16(pZ), z1r       /* z1r = {z1r, z1r} */
      movddup 24(pZ), z1i       /* z1i = {z1i, z1i} */
      mulpd   vposneg, z1i      /* z0i = {z1i,-z1i} */
      add     $32, pZ
         pshufd $0x4E, x0, revx0        /* revx0 = {x0r, x0i} */
      mov M, II

ALIGN32
      LOOPM:
/*
 *       Reuse X to compute rank-1 update x*y for 2 columns of A
 */
         movapd x0, a0          /* a0 = {x0i, x0r} */
         mulpd  y0r, a0         /* a0 = {x0i*y0r, x0r*y0r} */
         addpd  (pA0,II,8), a0  /* a0 = {a0i+x0i*y0r, a0r+x0r*y0r} */

         movapd revx0, A0      /* A0 = {x0r, x0i} */
         mulpd  y0i, A0        /* A0 = {x0r*y0i, -x0i*y0i} */
         addpd  A0, a0;     /* a0 = {a0i+x0i*y0r+x0r*y0i,a0r+x0r*y0r-x0i*x0i} */

         movapd x0, a1          /* a1 = {x0i, x0r} */
                movapd (pW,II,8), x0    /* x0 = {w0i, w0r} */
         mulpd  y1r, a1        /* a1 = {x0i*y1r, x0r*y1r} */
         addpd  (pA1,II,8), a1 /* a1 = {A1i+x0i*y1r, A1r+x0r*y1r} */
         mulpd  y1i, revx0     /* rx0= {x0r*y1i, -x0i*y1i} */
         addpd  revx0, a1   /* a1={A1i+x0i*y1r+x0r*y1i, A1r+x0r*y1r-x0i*y1i} */
                pshufd $0x4E, x0, revx0        /* revx0 = {w0r, w0i} */
/*
 *       Reuse W to compute rank-1 update w*z for 2 columns of A to complete
 *       the rank-2 update of these two column elements
 */

         movapd x0, A0         /* A0 = {w0i, w0r} */
         mulpd  z0r, A0        /* A0 = {w0i*z0r, w0r*z0r} */
         addpd  A0, a0         /* a0 = {a0i+w0i*z0r, a0r+w0r*z0r} */

         movapd revx0, A0      /* A0 = {w0r, w0i} */
         mulpd  z0i, A0        /* A0 = {w0r*z0i, -w0i*z0i} */
         addpd  A0, a0         /* a0 = completed rank-2 update */
         movapd a0, (pA0,II,8) /* store completed rank-2 update */

         mulpd z1r, x0         /* x0 = {w0i*z1r, w0r*z1r} */
         addpd x0, a1          /* a1 = {a1i+w0i*z1r, a1r+w0r*z1r} */
                movapd 16(pX,II,8), x0           /* x0 = {x0i, x0r} */

         mulpd z1i, revx0      /* revx0={w0r*z1i, -w0i*z1i} */
         addpd revx0, a1       /* completed rank-2 update for a1 */
                pshufd $0x4E, x0, revx0        /* revx0 = {x0r, x0i} */
         movapd a1, (pA1,II,8)

      add       $2, II
      jnz LOOPM
/*
 *    ==================
 *    Drain X fetch pipe
 *    ==================
 */
/*
 *    Reuse X to compute rank-1 update x*y for 2 columns of A
 */
      movapd x0, a0          /* a0 = {x0i, x0r} */
      mulpd  y0r, a0         /* a0 = {x0i*y0r, x0r*y0r} */
      addpd  (pA0,II,8), a0  /* a0 = {a0i+x0i*y0r, a0r+x0r*y0r} */

      movapd revx0, A0      /* A0 = {x0r, x0i} */
      mulpd  y0i, A0        /* A0 = {x0r*y0i, -x0i*y0i} */
      addpd  A0, a0;     /* a0 = {a0i+x0i*y0r+x0r*y0i,a0r+x0r*y0r-x0i*x0i} */

      movapd x0, a1          /* a1 = {x0i, x0r} */
             movapd (pW,II,8), x0    /* x0 = {w0i, w0r} */
      mulpd  y1r, a1        /* a1 = {x0i*y1r, x0r*y1r} */
      addpd  (pA1,II,8), a1 /* a1 = {A1i+x0i*y1r, A1r+x0r*y1r} */
      mulpd  y1i, revx0     /* rx0= {x0r*y1i, -x0i*y1i} */
      addpd  revx0, a1   /* a1={A1i+x0i*y1r+x0r*y1i, A1r+x0r*y1r-x0i*y1i} */
             pshufd $0x4E, x0, revx0        /* revx0 = {w0r, w0i} */
/*
 *    Reuse W to compute rank-1 update w*z for 2 columns of A to complete
 *    the rank-2 update of these two column elements
 */

      movapd x0, A0         /* A0 = {w0i, w0r} */
      mulpd  z0r, A0        /* A0 = {w0i*z0r, w0r*z0r} */
      addpd  A0, a0         /* a0 = {a0i+w0i*z0r, a0r+w0r*z0r} */

      movapd revx0, A0      /* A0 = {w0r, w0i} */
      mulpd  z0i, A0        /* A0 = {w0r*z0i, -w0i*z0i} */
      addpd  A0, a0         /* a0 = completed rank-2 update */
      movapd a0, (pA0,II,8) /* store completed rank-2 update */

      mulpd z1r, x0         /* x0 = {w0i*z1r, w0r*z1r} */
      addpd x0, a1          /* a1 = {a1i+w0i*z1r, a1r+w0r*z1r} */

      mulpd z1i, revx0      /* revx0={w0r*z1i, -w0i*z1i} */
      addpd revx0, a1       /* completed rank-2 update for a1 */
      movapd a1, (pA1,II,8)
      lea (pA0, lda, 2), pA0
      lea (pA1, lda, 2), pA1

   sub $2, N
   jnz LOOPN

/*
 * EPILOGUE: restore registers and return
 */
   movq -8(%rsp), %rbx
#if 0
   movq %r13, -32(%rsp), %r13
   movq %r14, -40(%rsp), %r14
   movq %r15, -48(%rsp), %r15
#endif
   ret
