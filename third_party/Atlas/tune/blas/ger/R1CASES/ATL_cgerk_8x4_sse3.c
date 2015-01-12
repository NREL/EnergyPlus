/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2010 R. Clint Whaley
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
 * This file does a 1x4 unrolled r1_sse with these params:
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
#define pA0     %r8
#define lda     %rax
#define pX      %rdx
#define pY      %r9
#define II      %rbx
#define pX0     %r11
#define Mr      %rcx
#define incAXm  %r10
#define incII   %r15
#define incAn   %r14
#define lda3    %r12
#define Ma      %r13
/*
 * SSE register assignment
 */
#define rXr     %xmm0
#define rXi     %xmm1
#define ra0     %xmm2
#define rA0     %xmm3
#define rY0      %xmm4
#define rYh0     %xmm5
#define rY1      %xmm6
#define rYh1     %xmm7
#define rY2      %xmm8
#define rYh2     %xmm9
#define rY3      %xmm10
#define rYh3     %xmm11
#define NONEPONEOFF -72
#define rneg %xmm15

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
#define mulpd mulps
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
   #define PFADIST 0
#endif
#ifndef PFYDIST
   #define PFYDIST 64
#endif
#ifndef PFXDIST
   #define PFXDIST 64
#endif
#ifndef PFIY
   #define PFIY prefetchnta
#endif
#ifndef PFIA
   #ifdef ATL_3DNow
      #define PFIA prefetchw
   #else
      #define PFIA prefetcht0
   #endif
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
.text
/*
 *                      %rdi        %rsi           %rdx          %rcx
 * void ATL_UGERK(ATL_CINT M, ATL_CINT N, const TYPE *X, const TYPE *Y,
 *                    %r8      %r9
 *                TYPE *A, ATL_CINT lda)
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
   movq %r13, -32(%rsp)
   movq %r14, -40(%rsp)
   movq %r15, -48(%rsp)
/*
 * Compute if we peel or not.  This kernel asserts that lda is a multiple
 * of 16 bytes, and that A is at least 8-byte aligned, which means we must
 * either peel A on iteration or none, depending on alignment
 *
 * Compute M = (M/MU)*MU, Mr = M - (M/MU)*MU
 * NOTE: Mr is %rcx reg, so we can use jcx to go to cleanup loop
 */
   mov  %r9, lda        /* move lda to assigned register, rax */
   mov  %rcx, pY        /* move pY to assigned register, r9 */
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
   movaps NONEPONEOFF(%rsp), rneg
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
   sub $-128, pX        /* code compaction by using signed 1-byte offsets */
   mov pX, pX0          /* save for restore after M loops */
   mov $-64, incAXm     /* code comp: use reg rather than constant */
   lea (lda, lda,2), lda3       /* lda3 = 3*lda */
   lea (incAn, lda3), incAn     /* incAn = (4*lda-M)*sizeof */
   mov $8*1, incII      /* code comp: use reg rather than constant */
   mov M, II

   ALIGN32
   LOOPN:
      movlps 0*8(pY), rY0   /* rY0 = {xx,xx, Y0i, Y0r} */
      movlhps rY0, rY0      /* rY0 = {Y0i, Y0r, Y0i, Y0r} */
      pshufd $0x11, rY0, rYh0 /* rYh0 = {Y0r, Y0i, Y0r, Y0i} */
      mulps rneg, rYh0  /* rYh0 = {Y0r,-Y0i, Y0r,-Y0i} */
      movlps 1*8(pY), rY1   /* rY1 = {xx,xx, Y1i, Y1r} */
      movlhps rY1, rY1      /* rY1 = {Y1i, Y1r, Y1i, Y1r} */
      pshufd $0x11, rY1, rYh1 /* rYh1 = {Y1r, Y1i, Y1r, Y1i} */
      mulps rneg, rYh1  /* rYh1 = {Y1r,-Y1i, Y1r,-Y1i} */
      movlps 2*8(pY), rY2   /* rY2 = {xx,xx, Y2i, Y2r} */
      movlhps rY2, rY2      /* rY2 = {Y2i, Y2r, Y2i, Y2r} */
      pshufd $0x11, rY2, rYh2 /* rYh2 = {Y2r, Y2i, Y2r, Y2i} */
      mulps rneg, rYh2  /* rYh2 = {Y2r,-Y2i, Y2r,-Y2i} */
      movlps 3*8(pY), rY3   /* rY3 = {xx,xx, Y3i, Y3r} */
      movlhps rY3, rY3      /* rY3 = {Y3i, Y3r, Y3i, Y3r} */
      pshufd $0x11, rY3, rYh3 /* rYh3 = {Y3r, Y3i, Y3r, Y3i} */
      mulps rneg, rYh3  /* rYh3 = {Y3r,-Y3i, Y3r,-Y3i} */
/*
 *    If no peeled iteration, start M-loop, else do peeled iteration
 */
      bt $0, Ma
      jnc LOOPM
         movlps -128(pX), rXi           /* rXr = {0, 0, X0i, X0r} */
         pshufd $0xE0, rXi, rXr         /* rXr = {0, 0, X0r, X0r} */
         movaps rY0, rA0                /* rA0 = {Y0i, Y0r, Y0i, Y0r} */
         mulps  rXr, rA0                /* rA0 = {0, 0, X0r*Y0i, X0r*Y0r} */
         movlps -128(pA0), ra0
         addps ra0, rA0
         shufps $0xE5, rXi, rXi         /* rXi = {0, 0, X0i, X0i} */
         movaps rYh0, ra0               /* ra0 = {Y0r, -Y0i, Y0r,-Y0i} */
         mulps  rXi, ra0                /* ra0 = {0, 0, X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         movlps rA0, -128(pA0)
         movlps -128(pX), rXi           /* rXr = {0, 0, X0i, X0r} */
         pshufd $0xE0, rXi, rXr         /* rXr = {0, 0, X0r, X0r} */
         movaps rY1, rA0                /* rA0 = {Y1i, Y1r, Y1i, Y1r} */
         mulps  rXr, rA0                /* rA0 = {0, 0, X0r*Y1i, X0r*Y1r} */
         movlps -128(pA0,lda), ra0
         addps ra0, rA0
         shufps $0xE5, rXi, rXi         /* rXi = {0, 0, X0i, X0i} */
         movaps rYh1, ra0               /* ra0 = {Y1r, -Y1i, Y1r,-Y1i} */
         mulps  rXi, ra0                /* ra0 = {0, 0, X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         movlps rA0, -128(pA0,lda)
         movlps -128(pX), rXi           /* rXr = {0, 0, X0i, X0r} */
         pshufd $0xE0, rXi, rXr         /* rXr = {0, 0, X0r, X0r} */
         movaps rY2, rA0                /* rA0 = {Y2i, Y2r, Y2i, Y2r} */
         mulps  rXr, rA0                /* rA0 = {0, 0, X0r*Y2i, X0r*Y2r} */
         movlps -128(pA0,lda,2), ra0
         addps ra0, rA0
         shufps $0xE5, rXi, rXi         /* rXi = {0, 0, X0i, X0i} */
         movaps rYh2, ra0               /* ra0 = {Y2r, -Y2i, Y2r,-Y2i} */
         mulps  rXi, ra0                /* ra0 = {0, 0, X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         movlps rA0, -128(pA0,lda,2)
         movlps -128(pX), rXi           /* rXr = {0, 0, X0i, X0r} */
         pshufd $0xE0, rXi, rXr         /* rXr = {0, 0, X0r, X0r} */
         movaps rY3, rA0                /* rA0 = {Y3i, Y3r, Y3i, Y3r} */
         mulps  rXr, rA0                /* rA0 = {0, 0, X0r*Y3i, X0r*Y3r} */
         movlps -128(pA0,lda3), ra0
         addps ra0, rA0
         shufps $0xE5, rXi, rXi         /* rXi = {0, 0, X0i, X0i} */
         movaps rYh3, ra0               /* ra0 = {Y3r, -Y3i, Y3r,-Y3i} */
         mulps  rXi, ra0                /* ra0 = {0, 0, X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         movlps rA0, -128(pA0,lda3)
         add $8, pX
         add $8, pA0
      LOOPM:
         movsldup 0-128(pX), rXr /* rXr = { X1r, X1r, X0r, X0r} */
         movaps rY0, rA0   /* rA0 = {Y0i, Y0r,Y0i, Y0r} */
         mulps  rXr, rA0   /* rA0 = {X1r*Y0i,X1r*Y0r,X0r*Y0i,X0r*Y0r} */
         addps  0-128(pA0), rA0
         movshdup 0-128(pX), rXi /* rXi = {X1i, X1i, X0i, X0i} */
         movaps rYh0, ra0  /* ra0 = {Y0r,-Y0i,Y0r,-Y0i} */
         mulps  rXi, ra0   /* ra0 = {X1i*Y0r,-X1i*Y0i,X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         MOVA   rA0, 0-128(pA0)
         prefA(PFADIST+0(pA0))
         prefA(PFADIST+0(pA0,lda))
         movaps rY1, rA0   /* rA0 = {Y1i, Y1r,Y1i, Y1r} */
         mulps  rXr, rA0   /* rA0 = {X1r*Y1i,X1r*Y1r,X0r*Y1i,X0r*Y1r} */
         addps  0-128(pA0,lda), rA0
         movaps rYh1, ra0  /* ra0 = {Y1r,-Y1i,Y1r,-Y1i} */
         mulps  rXi, ra0   /* ra0 = {X1i*Y0r,-X1i*Y0i,X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         MOVA   rA0, 0-128(pA0,lda)
         prefA(PFADIST+0(pA0,lda,2))
         movaps rY2, rA0   /* rA0 = {Y2i, Y2r,Y2i, Y2r} */
         mulps  rXr, rA0   /* rA0 = {X1r*Y2i,X1r*Y2r,X0r*Y2i,X0r*Y2r} */
         addps  0-128(pA0,lda,2), rA0
         movaps rYh2, ra0  /* ra0 = {Y2r,-Y2i,Y2r,-Y2i} */
         mulps  rXi, ra0   /* ra0 = {X1i*Y0r,-X1i*Y0i,X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         MOVA   rA0, 0-128(pA0,lda,2)
         prefA(PFADIST+0(pA0,lda3))
         movaps rY3, rA0   /* rA0 = {Y3i, Y3r,Y3i, Y3r} */
         mulps  rXr, rA0   /* rA0 = {X1r*Y3i,X1r*Y3r,X0r*Y3i,X0r*Y3r} */
         addps  0-128(pA0,lda3), rA0
         movaps rYh3, ra0  /* ra0 = {Y3r,-Y3i,Y3r,-Y3i} */
         mulps  rXi, ra0   /* ra0 = {X1i*Y0r,-X1i*Y0i,X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         MOVA   rA0, 0-128(pA0,lda3)

         movsldup 16-128(pX), rXr /* rXr = { X1r, X1r, X0r, X0r} */
         movaps rY0, rA0   /* rA0 = {Y0i, Y0r,Y0i, Y0r} */
         mulps  rXr, rA0   /* rA0 = {X1r*Y0i,X1r*Y0r,X0r*Y0i,X0r*Y0r} */
         addps  16-128(pA0), rA0
         movshdup 16-128(pX), rXi /* rXi = {X1i, X1i, X0i, X0i} */
         movaps rYh0, ra0  /* ra0 = {Y0r,-Y0i,Y0r,-Y0i} */
         mulps  rXi, ra0   /* ra0 = {X1i*Y0r,-X1i*Y0i,X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         MOVA   rA0, 16-128(pA0)
         movaps rY1, rA0   /* rA0 = {Y1i, Y1r,Y1i, Y1r} */
         mulps  rXr, rA0   /* rA0 = {X1r*Y1i,X1r*Y1r,X0r*Y1i,X0r*Y1r} */
         addps  16-128(pA0,lda), rA0
         movaps rYh1, ra0  /* ra0 = {Y1r,-Y1i,Y1r,-Y1i} */
         mulps  rXi, ra0   /* ra0 = {X1i*Y0r,-X1i*Y0i,X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         MOVA   rA0, 16-128(pA0,lda)
         movaps rY2, rA0   /* rA0 = {Y2i, Y2r,Y2i, Y2r} */
         mulps  rXr, rA0   /* rA0 = {X1r*Y2i,X1r*Y2r,X0r*Y2i,X0r*Y2r} */
         addps  16-128(pA0,lda,2), rA0
         movaps rYh2, ra0  /* ra0 = {Y2r,-Y2i,Y2r,-Y2i} */
         mulps  rXi, ra0   /* ra0 = {X1i*Y0r,-X1i*Y0i,X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         MOVA   rA0, 16-128(pA0,lda,2)
         movaps rY3, rA0   /* rA0 = {Y3i, Y3r,Y3i, Y3r} */
         mulps  rXr, rA0   /* rA0 = {X1r*Y3i,X1r*Y3r,X0r*Y3i,X0r*Y3r} */
         addps  16-128(pA0,lda3), rA0
         movaps rYh3, ra0  /* ra0 = {Y3r,-Y3i,Y3r,-Y3i} */
         mulps  rXi, ra0   /* ra0 = {X1i*Y0r,-X1i*Y0i,X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         MOVA   rA0, 16-128(pA0,lda3)

         movsldup 32-128(pX), rXr /* rXr = { X1r, X1r, X0r, X0r} */
         movaps rY0, rA0   /* rA0 = {Y0i, Y0r,Y0i, Y0r} */
         mulps  rXr, rA0   /* rA0 = {X1r*Y0i,X1r*Y0r,X0r*Y0i,X0r*Y0r} */
         addps  32-128(pA0), rA0
         movshdup 32-128(pX), rXi /* rXi = {X1i, X1i, X0i, X0i} */
         movaps rYh0, ra0  /* ra0 = {Y0r,-Y0i,Y0r,-Y0i} */
         mulps  rXi, ra0   /* ra0 = {X1i*Y0r,-X1i*Y0i,X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         MOVA   rA0, 32-128(pA0)
         movaps rY1, rA0   /* rA0 = {Y1i, Y1r,Y1i, Y1r} */
         mulps  rXr, rA0   /* rA0 = {X1r*Y1i,X1r*Y1r,X0r*Y1i,X0r*Y1r} */
         addps  32-128(pA0,lda), rA0
         movaps rYh1, ra0  /* ra0 = {Y1r,-Y1i,Y1r,-Y1i} */
         mulps  rXi, ra0   /* ra0 = {X1i*Y0r,-X1i*Y0i,X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         MOVA   rA0, 32-128(pA0,lda)
         movaps rY2, rA0   /* rA0 = {Y2i, Y2r,Y2i, Y2r} */
         mulps  rXr, rA0   /* rA0 = {X1r*Y2i,X1r*Y2r,X0r*Y2i,X0r*Y2r} */
         addps  32-128(pA0,lda,2), rA0
         movaps rYh2, ra0  /* ra0 = {Y2r,-Y2i,Y2r,-Y2i} */
         mulps  rXi, ra0   /* ra0 = {X1i*Y0r,-X1i*Y0i,X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         MOVA   rA0, 32-128(pA0,lda,2)
         movaps rY3, rA0   /* rA0 = {Y3i, Y3r,Y3i, Y3r} */
         mulps  rXr, rA0   /* rA0 = {X1r*Y3i,X1r*Y3r,X0r*Y3i,X0r*Y3r} */
         addps  32-128(pA0,lda3), rA0
         movaps rYh3, ra0  /* ra0 = {Y3r,-Y3i,Y3r,-Y3i} */
         mulps  rXi, ra0   /* ra0 = {X1i*Y0r,-X1i*Y0i,X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         MOVA   rA0, 32-128(pA0,lda3)

         movsldup 48-128(pX), rXr /* rXr = { X1r, X1r, X0r, X0r} */
         movaps rY0, rA0   /* rA0 = {Y0i, Y0r,Y0i, Y0r} */
         mulps  rXr, rA0   /* rA0 = {X1r*Y0i,X1r*Y0r,X0r*Y0i,X0r*Y0r} */
         addps  48-128(pA0), rA0
         movshdup 48-128(pX), rXi /* rXi = {X1i, X1i, X0i, X0i} */
         movaps rYh0, ra0  /* ra0 = {Y0r,-Y0i,Y0r,-Y0i} */
         mulps  rXi, ra0   /* ra0 = {X1i*Y0r,-X1i*Y0i,X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         MOVA   rA0, 48-128(pA0)
         movaps rY1, rA0   /* rA0 = {Y1i, Y1r,Y1i, Y1r} */
         mulps  rXr, rA0   /* rA0 = {X1r*Y1i,X1r*Y1r,X0r*Y1i,X0r*Y1r} */
         addps  48-128(pA0,lda), rA0
         movaps rYh1, ra0  /* ra0 = {Y1r,-Y1i,Y1r,-Y1i} */
         mulps  rXi, ra0   /* ra0 = {X1i*Y0r,-X1i*Y0i,X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         MOVA   rA0, 48-128(pA0,lda)
         movaps rY2, rA0   /* rA0 = {Y2i, Y2r,Y2i, Y2r} */
         mulps  rXr, rA0   /* rA0 = {X1r*Y2i,X1r*Y2r,X0r*Y2i,X0r*Y2r} */
         addps  48-128(pA0,lda,2), rA0
         movaps rYh2, ra0  /* ra0 = {Y2r,-Y2i,Y2r,-Y2i} */
         mulps  rXi, ra0   /* ra0 = {X1i*Y0r,-X1i*Y0i,X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         MOVA   rA0, 48-128(pA0,lda,2)
         movaps rY3, rA0   /* rA0 = {Y3i, Y3r,Y3i, Y3r} */
         mulps  rXr, rA0   /* rA0 = {X1r*Y3i,X1r*Y3r,X0r*Y3i,X0r*Y3r} */
         addps  48-128(pA0,lda3), rA0
         movaps rYh3, ra0  /* ra0 = {Y3r,-Y3i,Y3r,-Y3i} */
         mulps  rXi, ra0   /* ra0 = {X1i*Y0r,-X1i*Y0i,X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         MOVA   rA0, 48-128(pA0,lda3)

         sub incAXm, pX
         sub incAXm, pA0
      sub incII, II
      jnz LOOPM

      cmp $0, Mr
      jz  MCLEANED

      mov Mr, II
      xorps rXr, rXr
      movaps rXr, rXi
      xorps ra0, ra0
      LOOPMCU:
         movlps -128(pX), rXi           /* rXr = {0, 0, X0i, X0r} */
         pshufd $0xE0, rXi, rXr         /* rXr = {0, 0, X0r, X0r} */
         movaps rY0, rA0                /* rA0 = {Y0i, Y0r, Y0i, Y0r} */
         mulps  rXr, rA0                /* rA0 = {0, 0, X0r*Y0i, X0r*Y0r} */
         movlps -128(pA0), ra0
         addps ra0, rA0
         shufps $0xE5, rXi, rXi         /* rXi = {0, 0, X0i, X0i} */
         movaps rYh0, ra0               /* ra0 = {Y0r, -Y0i, Y0r,-Y0i} */
         mulps  rXi, ra0                /* ra0 = {0, 0, X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         movlps rA0, -128(pA0)
         movlps -128(pX), rXi           /* rXr = {0, 0, X0i, X0r} */
         pshufd $0xE0, rXi, rXr         /* rXr = {0, 0, X0r, X0r} */
         movaps rY1, rA0                /* rA0 = {Y1i, Y1r, Y1i, Y1r} */
         mulps  rXr, rA0                /* rA0 = {0, 0, X0r*Y1i, X0r*Y1r} */
         movlps -128(pA0,lda), ra0
         addps ra0, rA0
         shufps $0xE5, rXi, rXi         /* rXi = {0, 0, X0i, X0i} */
         movaps rYh1, ra0               /* ra0 = {Y1r, -Y1i, Y1r,-Y1i} */
         mulps  rXi, ra0                /* ra0 = {0, 0, X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         movlps rA0, -128(pA0,lda)
         movlps -128(pX), rXi           /* rXr = {0, 0, X0i, X0r} */
         pshufd $0xE0, rXi, rXr         /* rXr = {0, 0, X0r, X0r} */
         movaps rY2, rA0                /* rA0 = {Y2i, Y2r, Y2i, Y2r} */
         mulps  rXr, rA0                /* rA0 = {0, 0, X0r*Y2i, X0r*Y2r} */
         movlps -128(pA0,lda,2), ra0
         addps ra0, rA0
         shufps $0xE5, rXi, rXi         /* rXi = {0, 0, X0i, X0i} */
         movaps rYh2, ra0               /* ra0 = {Y2r, -Y2i, Y2r,-Y2i} */
         mulps  rXi, ra0                /* ra0 = {0, 0, X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         movlps rA0, -128(pA0,lda,2)
         movlps -128(pX), rXi           /* rXr = {0, 0, X0i, X0r} */
         pshufd $0xE0, rXi, rXr         /* rXr = {0, 0, X0r, X0r} */
         movaps rY3, rA0                /* rA0 = {Y3i, Y3r, Y3i, Y3r} */
         mulps  rXr, rA0                /* rA0 = {0, 0, X0r*Y3i, X0r*Y3r} */
         movlps -128(pA0,lda3), ra0
         addps ra0, rA0
         shufps $0xE5, rXi, rXi         /* rXi = {0, 0, X0i, X0i} */
         movaps rYh3, ra0               /* ra0 = {Y3r, -Y3i, Y3r,-Y3i} */
         mulps  rXi, ra0                /* ra0 = {0, 0, X0i*Y0r,-X0i*Y0i} */
         addps  ra0, rA0
         movlps rA0, -128(pA0,lda3)
         add $8, pX
         add $8, pA0
      dec II
      jnz LOOPMCU

MCLEANED:
      prefY(4*8+PFYDIST(pY))
      add $4*8, pY
      add incAn, pA0
      mov pX0, pX
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
