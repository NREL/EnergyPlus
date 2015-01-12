/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2012, 2010 R. Clint Whaley
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
#ifndef ATL_SSE3
   #error "This routine requires SSE3!"
#endif
#include "atlas_asm.h"
/*
 * This file does a 1x4 unrolled mvn_sse with these params:
 *    CL=8, ORDER=clmajor
 */
#ifndef ATL_GAS_x8664
   #error "This kernel requires x86-64 assembly!"
#endif
/*
 * Integer register assignment
 */
#define M       %rdi
#define N       %rsi
#define pA0     %rdx
#define lda     %rax
#define pX      %r8
#define pY      %r9
#define II      %rbx
#define pY0     %r11
#define Mr      %rcx
#define incAYm  %r10
#define incII   %r15
#define incAn   %r14
#define lda3    %r12
#define Ma      %r13
/*
 * SSE register assignment
 */
#define rA      %xmm0
#define ra      %xmm1
#define rY      %xmm2
#define ry      %xmm3
#define rX0     %xmm4
#define iX0     %xmm5
#define rX1     %xmm6
#define iX1     %xmm7
#define rX2     %xmm8
#define iX2     %xmm9
#define rX3     %xmm10
#define iX3     %xmm11
#define NONEPONEOFF -72
#define NONEPONE %xmm15
/*
 * macros
 */
#ifndef MOVA
   #define MOVA movaps
#endif
#define movapd movaps
#define movupd movups
#define xorpd xorps
#define addpd addps
#define mulps mulps
#define addsd addss
#define mulsd mulss
#define movsd movss
#define haddpd haddps
/*
 * Define macros controlling prefetch
 */
#ifndef PFDIST
   #define PFDIST 256
#endif
#ifndef PFADIST
   #define PFADIST PFDIST
#endif
#ifndef PFYDIST
   #define PFYDIST 64
#endif
#ifndef PFXDIST
   #define PFXDIST 64
#endif
#ifndef PFIY
   #ifdef ATL_3DNow
      #define PFIY prefetchw
   #else
      #define PFIY prefetcht0
   #endif
#endif
#ifndef PFIX
   #define PFIX prefetchnta
#endif
#ifndef PFIA
   #define PFIA prefetchnta
#endif
#if PFADIST == 0                /* flag for no prefetch */
   #define prefA(mem)
#else
   #define prefA(mem) PFIA mem
#endif
#if PFYDIST == 0                /* flag for no prefetch */
   #define prefY(mem)
#else
   #define prefY(mem) PFIY mem
#endif
#if PFXDIST == 0                /* flag for no prefetch */
   #define prefX(mem)
#else
   #define prefX(mem) PFIX mem
#endif
/*
 *                      %rdi        %rsi           %rdx          %rcx
 * void ATL_UGEMV(ATL_CINT M, ATL_CINT N, const TYPE *A, ATL_CINT lda,
 *                          %r8      %r9
 *                const TYPE *X, TYPE *Y)
 */
.text
.text
.global ATL_asmdecor(ATL_UGEMV)
ALIGN64
ATL_asmdecor(ATL_UGEMV):

/*
 * Save callee-saved iregs
 */
   movq %rbp, -8(%rsp)
   movq %rbx, -16(%rsp)
   movq %r12, -24(%rsp)
   movq %r13, -32(%rsp)
   movq %r14, -40(%rsp)
   movq %r15, -48(%rsp)
/*
 * Compute M = (M/MU)*MU, Mr = M - (M/MU)*MU
 * NOTE: Mr is %rcx reg, so we can use jcx to go to cleanup loop
 */
   mov  %rcx, lda       /* move lda to assigned register, rax */
   mov  $1, Mr          /* setup assignment to peel */
   xor  Ma, Ma          /* default to no peel */
   test $0xF, pA0       /* 0 if 16-byte aligned */
   cmovnz Mr, Ma        /* if nonzero, say need 1 iteration peel */
   sub  Ma, M
   mov  M, Mr           /* Mr = M */
   shr $3, M            /* M = M / MU */
   shl $3, M            /* M = (M/MU)*MU */
   sub M, Mr            /* Mr = M - (M/MU)*MU */
/*
 * Construct nonepone = {1.0,-1.0,1.0,-1.0}
 */
   finit
   fld1                                 /* ST =  1.0 */
   fldz                                 /* ST =  0.0 1.0 */
   fsub %st(1), %st                     /* ST = -1.0 1.0 */
   fsts NONEPONEOFF(%rsp)               /* ST= -1.0 1.0 */
   fstps NONEPONEOFF+8(%rsp)            /* ST=1.0 */
   fsts NONEPONEOFF+4(%rsp)             /* ST=1.0 */
   fstps NONEPONEOFF+12(%rsp)          /* ST=NULL, mem={1.0, -1.0, 1.0, -1.0}*/
   movapd NONEPONEOFF(%rsp), NONEPONE
/*
 * Setup constants
 */
   mov lda, incAn       /* incAn = lda */
   sub M, incAn         /* incAn = lda - (M/MU)*MU */
   sub Ma, incAn
   sub Mr, incAn        /* incAn = lda - M */
   shl $3, incAn        /* incAn = (lda-M)*sizeof */
   shl $3, lda          /* lda *= sizeof */
   sub $-128, pA0       /* code compaction by using signed 1-byte offsets */
   sub $-128, pY        /* code compaction by using signed 1-byte offsets */
   mov pY, pY0          /* save for restore after M loops */
   mov $-64, incAYm     /* code comp: use reg rather than constant */
   lea (lda, lda,2), lda3       /* lda3 = 3*lda */
   lea (incAn, lda3), incAn     /* incAn = (4*lda-M)*sizeof */
   mov $8*1, incII      /* code comp: use reg rather than constant */
   mov M, II
/*
 * Zero Y if beta = 0; should peel to use MOVAPD, but too lazy
 */
   #ifdef BETA0
      add Mr, II
      add Ma, II
      shr $1, II
      xorpd rY, rY
      LOOPZERO:
         movupd rY, -128(pY)
         add $16, pY
      dec II
      jnz LOOPZERO
      lea (M, Mr), II
      add Ma, II
      bt $0, II
      jnc DONE_ZERO_CLEAN
      movlps rY, -128(pY)
DONE_ZERO_CLEAN:
      mov pY0, pY
      mov M, II
   #endif

   ALIGN32
   LOOPN:
      movaps (pX), iX1       /* iX1 = {iX1, rX1, iX0, rX0} */
      pshufd $0x00, iX1, rX0 /* rX0 = {rX0, rX0, rX0, rX0} */
      pshufd $0x55, iX1, iX0 /* iX0 = {iX0, iX0, iX0, iX0} */
      mulps NONEPONE, iX0          /* iX0 = {iX0,-iX0, iX0,-iX0} */
      pshufd $0xAA, iX1, rX1 /* rX1 = {rX1, rX1, rX1, rX1} */
      pshufd $0xFF, iX1, iX1 /* iX1 = {iX1, iX1, iX1, iX1} */
      mulps NONEPONE, iX1          /* iX1 = {iX1,-iX1, iX1,-iX1} */
      movaps 16(pX), iX3     /* iX3 = {iX3, rX3, iX2, rX2} */
      pshufd $0x00, iX3, rX2 /* rX2 = {rX2, rX2, rX2, rX2} */
      pshufd $0x55, iX3, iX2 /* iX2 = {iX2, iX2, iX2, iX2} */
      mulps NONEPONE, iX2          /* iX2 = {iX2,-iX2, iX2,-iX2} */
      pshufd $0xAA, iX3, rX3 /* rX3 = {rX3, rX3, rX3, rX3} */
      pshufd $0xFF, iX3, iX3 /* iX3 = {iX3, iX3, iX3, iX3} */
      mulps NONEPONE, iX3          /* iX3 = {iX3,-iX3, iX3,-iX3} */
/*
 *    If no peeled iteration, start M-loop, else do peeled iteration
 */
   bt $0, Ma
   jnc LOOPM
         movlps   -128(pA0), rY         /* rY = {iA0, rA0} */
         pshufd $0x11, rY, ry           /* ry = {rA0, iA0} */
         mulps rX0, rY                  /* rY = {rX0*iA0, rX0*rA0} */
         movddup -128(pY), rA
         addps rA, rY
         mulps iX0, ry                  /* ry = {iX0*rA0, -iX0*iA0} */
         movlps -128(pA0,lda), rA
         pshufd $0x11, rA, ra
         mulps rX1, rA
         addpd rA, rY
         mulps iX1, ra
         addpd ra, ry
         movlps -128(pA0,lda,2), rA
         pshufd $0x11, rA, ra
         mulps rX2, rA
         addpd rA, rY
         mulps iX2, ra
         addpd ra, ry
         movlps -128(pA0,lda3), rA
         pshufd $0x11, rA, ra
         mulps rX3, rA
         addpd rA, rY
         mulps iX3, ra
         addpd ra, ry
         addpd ry, rY
         movlps rY, -128(pY)
         add $8, pY
         add $8, pA0

      LOOPM:
         MOVA   0-128(pA0), rY         /* rY = {iA0, rA0} */
         pshufd $0xB1, rY, ry           /* ry = {rA0, iA0} */
         mulps rX0, rY                  /* rY = {rX0*iA0, rX0*rA0} */
         addpd 0-128(pY), rY
         prefA(PFADIST+0(pA0))
         mulps iX0, ry                  /* ry = {iX0*rA0, -iX0*iA0} */

         MOVA   0-128(pA0,lda), rA    /* rA = {iA, rA} */
         pshufd $0xB1, rA, ra           /* ra = {rA, iA} */
         mulps rX1, rA               /* rA = {rX*iA, rX*rA} */
         addpd rA, rY
         prefA(PFADIST+0(pA0,lda))
         mulps iX1, ra               /* ra = {iX*rA, -iX*iA} */
         addpd ra, ry
         MOVA   0-128(pA0,lda,2), rA    /* rA = {iA, rA} */
         pshufd $0xB1, rA, ra           /* ra = {rA, iA} */
         mulps rX2, rA               /* rA = {rX*iA, rX*rA} */
         addpd rA, rY
         prefA(PFADIST+0(pA0,lda,2))
         mulps iX2, ra               /* ra = {iX*rA, -iX*iA} */
         addpd ra, ry
         MOVA   0-128(pA0,lda3), rA    /* rA = {iA, rA} */
         pshufd $0xB1, rA, ra           /* ra = {rA, iA} */
         mulps rX3, rA               /* rA = {rX*iA, rX*rA} */
         addpd rA, rY
         prefA(PFADIST+0(pA0,lda3))
         mulps iX3, ra               /* ra = {iX*rA, -iX*iA} */
         addpd ra, ry
         addpd ry, rY
         movapd rY, 0-128(pY)

         MOVA   16-128(pA0), rY         /* rY = {iA0, rA0} */
         pshufd $0xB1, rY, ry           /* ry = {rA0, iA0} */
         mulps rX0, rY                  /* rY = {rX0*iA0, rX0*rA0} */
         addpd 16-128(pY), rY
         mulps iX0, ry                  /* ry = {iX0*rA0, -iX0*iA0} */

         MOVA   16-128(pA0,lda), rA    /* rA = {iA, rA} */
         pshufd $0xB1, rA, ra           /* ra = {rA, iA} */
         mulps rX1, rA               /* rA = {rX*iA, rX*rA} */
         addpd rA, rY
         mulps iX1, ra               /* ra = {iX*rA, -iX*iA} */
         addpd ra, ry
         MOVA   16-128(pA0,lda,2), rA    /* rA = {iA, rA} */
         pshufd $0xB1, rA, ra           /* ra = {rA, iA} */
         mulps rX2, rA               /* rA = {rX*iA, rX*rA} */
         addpd rA, rY
         mulps iX2, ra               /* ra = {iX*rA, -iX*iA} */
         addpd ra, ry
         MOVA   16-128(pA0,lda3), rA    /* rA = {iA, rA} */
         pshufd $0xB1, rA, ra           /* ra = {rA, iA} */
         mulps rX3, rA               /* rA = {rX*iA, rX*rA} */
         addpd rA, rY
         mulps iX3, ra               /* ra = {iX*rA, -iX*iA} */
         addpd ra, ry
         addpd ry, rY
         movapd rY, 16-128(pY)

         MOVA   32-128(pA0), rY         /* rY = {iA0, rA0} */
         pshufd $0xB1, rY, ry           /* ry = {rA0, iA0} */
         mulps rX0, rY                  /* rY = {rX0*iA0, rX0*rA0} */
         addpd 32-128(pY), rY
         mulps iX0, ry                  /* ry = {iX0*rA0, -iX0*iA0} */

         MOVA   32-128(pA0,lda), rA    /* rA = {iA, rA} */
         pshufd $0xB1, rA, ra           /* ra = {rA, iA} */
         mulps rX1, rA               /* rA = {rX*iA, rX*rA} */
         addpd rA, rY
         mulps iX1, ra               /* ra = {iX*rA, -iX*iA} */
         addpd ra, ry
         MOVA   32-128(pA0,lda,2), rA    /* rA = {iA, rA} */
         pshufd $0xB1, rA, ra           /* ra = {rA, iA} */
         mulps rX2, rA               /* rA = {rX*iA, rX*rA} */
         addpd rA, rY
         mulps iX2, ra               /* ra = {iX*rA, -iX*iA} */
         addpd ra, ry
         MOVA   32-128(pA0,lda3), rA    /* rA = {iA, rA} */
         pshufd $0xB1, rA, ra           /* ra = {rA, iA} */
         mulps rX3, rA               /* rA = {rX*iA, rX*rA} */
         addpd rA, rY
         mulps iX3, ra               /* ra = {iX*rA, -iX*iA} */
         addpd ra, ry
         addpd ry, rY
         movapd rY, 32-128(pY)

         MOVA   48-128(pA0), rY         /* rY = {iA0, rA0} */
         pshufd $0xB1, rY, ry           /* ry = {rA0, iA0} */
         mulps rX0, rY                  /* rY = {rX0*iA0, rX0*rA0} */
         addpd 48-128(pY), rY
         mulps iX0, ry                  /* ry = {iX0*rA0, -iX0*iA0} */

         MOVA   48-128(pA0,lda), rA    /* rA = {iA, rA} */
         pshufd $0xB1, rA, ra           /* ra = {rA, iA} */
         mulps rX1, rA               /* rA = {rX*iA, rX*rA} */
         addpd rA, rY
         mulps iX1, ra               /* ra = {iX*rA, -iX*iA} */
         addpd ra, ry
         MOVA   48-128(pA0,lda,2), rA    /* rA = {iA, rA} */
         pshufd $0xB1, rA, ra           /* ra = {rA, iA} */
         mulps rX2, rA               /* rA = {rX*iA, rX*rA} */
         addpd rA, rY
         mulps iX2, ra               /* ra = {iX*rA, -iX*iA} */
         addpd ra, ry
         MOVA   48-128(pA0,lda3), rA    /* rA = {iA, rA} */
         pshufd $0xB1, rA, ra           /* ra = {rA, iA} */
         mulps rX3, rA               /* rA = {rX*iA, rX*rA} */
         addpd rA, rY
         mulps iX3, ra               /* ra = {iX*rA, -iX*iA} */
         addpd ra, ry
         addpd ry, rY
         movapd rY, 48-128(pY)

         sub incAYm, pY
         sub incAYm, pA0
      sub incII, II
      jnz LOOPM

      #if 1 /* def ATL_OS_OSX */
         cmp $0, Mr
         jz  MCLEANED
      #else
         jecxz MCLEANED        /* skip cleanup loop if Mr == 0 */
      #endif

      mov Mr, II
      LOOPMCU:
         movlps   -128(pA0), rY         /* rY = {iA0, rA0} */
         pshufd $0x11, rY, ry           /* ry = {rA0, iA0} */
         mulps rX0, rY                  /* rY = {rX0*iA0, rX0*rA0} */
         movddup -128(pY), rA
         addps rA, rY
         mulps iX0, ry                  /* ry = {iX0*rA0, -iX0*iA0} */
         movlps -128(pA0,lda), rA
         pshufd $0x11, rA, ra
         mulps rX1, rA
         addpd rA, rY
         mulps iX1, ra
         addpd ra, ry
         movlps -128(pA0,lda,2), rA
         pshufd $0x11, rA, ra
         mulps rX2, rA
         addpd rA, rY
         mulps iX2, ra
         addpd ra, ry
         movlps -128(pA0,lda3), rA
         pshufd $0x11, rA, ra
         mulps rX3, rA
         addpd rA, rY
         mulps iX3, ra
         addpd ra, ry
         addpd ry, rY
         movlps rY, -128(pY)
         add $8, pY
         add $8, pA0
      dec II
      jnz LOOPMCU

MCLEANED:
      prefX(4*8+PFXDIST(pX))
      add $4*8, pX
      add incAn, pA0
      mov pY0, pY
      mov M, II
   sub $4, N
   jnz LOOPN
/*
 * EPILOGUE: restore registers and return
 */
   movq -8(%rsp), %rbp
   movq -16(%rsp), %rbx
   movq -24(%rsp), %r12
   movq -32(%rsp), %r13
   movq -40(%rsp), %r14
   movq -48(%rsp), %r15
   ret
