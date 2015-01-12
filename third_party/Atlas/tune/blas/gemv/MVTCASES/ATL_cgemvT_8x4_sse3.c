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
 * This file does a 1x4 unrolled mvt_sse with these params:
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
#define rA0     %xmm0
#define rX0     %xmm1
#define rx0     %xmm2
#define rt0     %xmm3
#define rY0r    %xmm4
#define rY0i    %xmm5
#define rY1r    %xmm6
#define rY1i    %xmm7
#define rY2r    %xmm8
#define rY2i    %xmm9
#define rY3r    %xmm10
#define rY3i    %xmm11
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
      #define PFIY prefetchnta
   #endif
#endif
#ifndef PFIX
   #define PFIX prefetcht0
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
 * Construct ponenone = {-1.0,1.0,-1.0,1.0}
 */
   finit
   fld1                                 /* ST =  1.0 */
   fldz                                 /* ST =  0.0 1.0 */
   fsub %st(1), %st                     /* ST = -1.0 1.0 */
   fsts NONEPONEOFF+4(%rsp)
   fstps NONEPONEOFF+12(%rsp)           /* ST = 1.0 */
   fsts NONEPONEOFF(%rsp)
   fstps NONEPONEOFF+8(%rsp)            /* ST=NULL, mem=-1,1,-1,1*/
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
   sub $-128, pX        /* code compaction by using signed 1-byte offsets */
   mov pX, pX0          /* save for restore after M loops */
   mov $-64, incAXm     /* code comp: use reg rather than constant */
   lea (lda, lda,2), lda3       /* lda3 = 3*lda */
   lea (incAn, lda3), incAn     /* incAn = (4*lda-M)*sizeof */
   mov $8*1, incII      /* code comp: use reg rather than constant */
   mov M, II
   ALIGN32
   LOOPN:
      xorpd rY0r, rY0r
      xorpd rY0i, rY0i
      xorpd rY1r, rY1r
      xorpd rY1i, rY1i
      xorpd rY2r, rY2r
      xorpd rY2i, rY2i
      xorpd rY3r, rY3r
      xorpd rY3i, rY3i
/*
 *    If no peeled iteration, start M-loop, else do peeled iteration
 */
      bt $0, Ma
      jnc LOOPM
         xorps rA0, rA0
         xorps rX0, rX0
         xorps rx0, rx0
         movlps -128(pX), rX0           /* rX0 = {0, 0, Xi, Xr} */
         pshufd $0xB1, rX0, rx0     /* rx0 = {0, 0, Xr, Xi} */
         movlps -128(pA0), rA0          /* rA0 = {0, 0, Ai, Ar} */
         movaps rA0, rt0                /* rt0 = {0, 0, Ai, Ar} */
         mulps rx0, rA0                 /* rA0 = {0, 0, Xr*Ai, Xi*Ar} */
         addps rA0, rY0i
         mulps rX0, rt0                 /* rt0 = {0, 0, Xi*Ai, Xr*Ar} */
         addps rt0, rY0r
         movlps -128(pA0,lda), rA0        /* rA0 = {0, 0, Ai, Ar} */
         movaps rA0, rt0                /* rt0 = {0, 0, Ai, Ar} */
         mulps rx0, rA0                 /* rA0 = {0, 0, Xr*Ai, Xi*Ar} */
         addps rA0, rY1i
         mulps rX0, rt0                 /* rt0 = {0, 0, Xi*Ai, Xr*Ar} */
         addps rt0, rY1r
         movlps -128(pA0,lda,2), rA0        /* rA0 = {0, 0, Ai, Ar} */
         movaps rA0, rt0                /* rt0 = {0, 0, Ai, Ar} */
         mulps rx0, rA0                 /* rA0 = {0, 0, Xr*Ai, Xi*Ar} */
         addps rA0, rY2i
         mulps rX0, rt0                 /* rt0 = {0, 0, Xi*Ai, Xr*Ar} */
         addps rt0, rY2r
         movlps -128(pA0,lda3), rA0        /* rA0 = {0, 0, Ai, Ar} */
         movaps rA0, rt0                /* rt0 = {0, 0, Ai, Ar} */
         mulps rx0, rA0                 /* rA0 = {0, 0, Xr*Ai, Xi*Ar} */
         addps rA0, rY3i
         mulps rX0, rt0                 /* rt0 = {0, 0, Xi*Ai, Xr*Ar} */
         addps rt0, rY3r
         add $8, pX
         add $8, pA0

      LOOPM:
         movapd 0-128(pX), rX0              /* rX0 = Xi,    Xr */
         pshufd $0xB1, rX0, rx0                 /* rx0 = Xr,    Xi */
         MOVA   0-128(pA0), rA0             /* rA0 = Ai,    Ar */
         movapd rA0, rt0                        /* rt0 = Ai,    Ar */
         mulpd rx0, rA0                         /* rA0 = Ai*Xr, Ar*Xi */
         addpd rA0, rY0i
         prefA(PFADIST+0(pA0))
         mulpd rX0, rt0                         /* rt0 = Ai*Xi, Ar*Xr */
         addpd rt0, rY0r

         MOVA   0-128(pA0,lda), rA0           /* rA0 = Ai,    Ar */
         movapd rA0, rt0                        /* rt0 = Ai,    Ar */
         mulpd rx0, rA0                         /* rA0 = Ai*Xr, Ar*Xi */
         addpd rA0, rY1i
         prefA(PFADIST+0(pA0,lda))
         mulpd rX0, rt0                         /* rt0 = Ai*Xi, Ar*Xr */
         addpd rt0, rY1r
         MOVA   0-128(pA0,lda,2), rA0           /* rA0 = Ai,    Ar */
         movapd rA0, rt0                        /* rt0 = Ai,    Ar */
         mulpd rx0, rA0                         /* rA0 = Ai*Xr, Ar*Xi */
         addpd rA0, rY2i
         prefA(PFADIST+0(pA0,lda,2))
         mulpd rX0, rt0                         /* rt0 = Ai*Xi, Ar*Xr */
         addpd rt0, rY2r
         MOVA   0-128(pA0,lda3), rA0           /* rA0 = Ai,    Ar */
         movapd rA0, rt0                        /* rt0 = Ai,    Ar */
         mulpd rx0, rA0                         /* rA0 = Ai*Xr, Ar*Xi */
         addpd rA0, rY3i
         prefA(PFADIST+0(pA0,lda3))
         mulpd rX0, rt0                         /* rt0 = Ai*Xi, Ar*Xr */
         addpd rt0, rY3r

         movapd 16-128(pX), rX0              /* rX0 = Xi,    Xr */
         pshufd $0xB1, rX0, rx0                 /* rx0 = Xr,    Xi */
         MOVA   16-128(pA0), rA0             /* rA0 = Ai,    Ar */
         movapd rA0, rt0                        /* rt0 = Ai,    Ar */
         mulpd rx0, rA0                         /* rA0 = Ai*Xr, Ar*Xi */
         addpd rA0, rY0i
         mulpd rX0, rt0                         /* rt0 = Ai*Xi, Ar*Xr */
         addpd rt0, rY0r

         MOVA   16-128(pA0,lda), rA0           /* rA0 = Ai,    Ar */
         movapd rA0, rt0                        /* rt0 = Ai,    Ar */
         mulpd rx0, rA0                         /* rA0 = Ai*Xr, Ar*Xi */
         addpd rA0, rY1i
         mulpd rX0, rt0                         /* rt0 = Ai*Xi, Ar*Xr */
         addpd rt0, rY1r
         MOVA   16-128(pA0,lda,2), rA0           /* rA0 = Ai,    Ar */
         movapd rA0, rt0                        /* rt0 = Ai,    Ar */
         mulpd rx0, rA0                         /* rA0 = Ai*Xr, Ar*Xi */
         addpd rA0, rY2i
         mulpd rX0, rt0                         /* rt0 = Ai*Xi, Ar*Xr */
         addpd rt0, rY2r
         MOVA   16-128(pA0,lda3), rA0           /* rA0 = Ai,    Ar */
         movapd rA0, rt0                        /* rt0 = Ai,    Ar */
         mulpd rx0, rA0                         /* rA0 = Ai*Xr, Ar*Xi */
         addpd rA0, rY3i
         mulpd rX0, rt0                         /* rt0 = Ai*Xi, Ar*Xr */
         addpd rt0, rY3r

         movapd 32-128(pX), rX0              /* rX0 = Xi,    Xr */
         pshufd $0xB1, rX0, rx0                 /* rx0 = Xr,    Xi */
         MOVA   32-128(pA0), rA0             /* rA0 = Ai,    Ar */
         movapd rA0, rt0                        /* rt0 = Ai,    Ar */
         mulpd rx0, rA0                         /* rA0 = Ai*Xr, Ar*Xi */
         addpd rA0, rY0i
         mulpd rX0, rt0                         /* rt0 = Ai*Xi, Ar*Xr */
         addpd rt0, rY0r

         MOVA   32-128(pA0,lda), rA0           /* rA0 = Ai,    Ar */
         movapd rA0, rt0                        /* rt0 = Ai,    Ar */
         mulpd rx0, rA0                         /* rA0 = Ai*Xr, Ar*Xi */
         addpd rA0, rY1i
         mulpd rX0, rt0                         /* rt0 = Ai*Xi, Ar*Xr */
         addpd rt0, rY1r
         MOVA   32-128(pA0,lda,2), rA0           /* rA0 = Ai,    Ar */
         movapd rA0, rt0                        /* rt0 = Ai,    Ar */
         mulpd rx0, rA0                         /* rA0 = Ai*Xr, Ar*Xi */
         addpd rA0, rY2i
         mulpd rX0, rt0                         /* rt0 = Ai*Xi, Ar*Xr */
         addpd rt0, rY2r
         MOVA   32-128(pA0,lda3), rA0           /* rA0 = Ai,    Ar */
         movapd rA0, rt0                        /* rt0 = Ai,    Ar */
         mulpd rx0, rA0                         /* rA0 = Ai*Xr, Ar*Xi */
         addpd rA0, rY3i
         mulpd rX0, rt0                         /* rt0 = Ai*Xi, Ar*Xr */
         addpd rt0, rY3r

         movapd 48-128(pX), rX0              /* rX0 = Xi,    Xr */
         pshufd $0xB1, rX0, rx0                 /* rx0 = Xr,    Xi */
         MOVA   48-128(pA0), rA0             /* rA0 = Ai,    Ar */
         movapd rA0, rt0                        /* rt0 = Ai,    Ar */
         mulpd rx0, rA0                         /* rA0 = Ai*Xr, Ar*Xi */
         addpd rA0, rY0i
         mulpd rX0, rt0                         /* rt0 = Ai*Xi, Ar*Xr */
         addpd rt0, rY0r

         MOVA   48-128(pA0,lda), rA0           /* rA0 = Ai,    Ar */
         movapd rA0, rt0                        /* rt0 = Ai,    Ar */
         mulpd rx0, rA0                         /* rA0 = Ai*Xr, Ar*Xi */
         addpd rA0, rY1i
         mulpd rX0, rt0                         /* rt0 = Ai*Xi, Ar*Xr */
         addpd rt0, rY1r
         MOVA   48-128(pA0,lda,2), rA0           /* rA0 = Ai,    Ar */
         movapd rA0, rt0                        /* rt0 = Ai,    Ar */
         mulpd rx0, rA0                         /* rA0 = Ai*Xr, Ar*Xi */
         addpd rA0, rY2i
         mulpd rX0, rt0                         /* rt0 = Ai*Xi, Ar*Xr */
         addpd rt0, rY2r
         MOVA   48-128(pA0,lda3), rA0           /* rA0 = Ai,    Ar */
         movapd rA0, rt0                        /* rt0 = Ai,    Ar */
         mulpd rx0, rA0                         /* rA0 = Ai*Xr, Ar*Xi */
         addpd rA0, rY3i
         mulpd rX0, rt0                         /* rt0 = Ai*Xi, Ar*Xr */
         addpd rt0, rY3r

         sub incAXm, pX
         sub incAXm, pA0
      sub incII, II
      jnz LOOPM

      cmp $0, Mr
      jz  MCLEANED

      mov Mr, II
      xorps rA0, rA0
      xorps rX0, rX0
      xorps rx0, rx0
      LOOPMCU:
         movlps -128(pX), rX0           /* rX0 = {0, 0, Xi, Xr} */
         pshufd $0xB1, rX0, rx0     /* rx0 = {0, 0, Xr, Xi} */
         movlps -128(pA0), rA0          /* rA0 = {0, 0, Ai, Ar} */
         movaps rA0, rt0                /* rt0 = {0, 0, Ai, Ar} */
         mulps rx0, rA0                 /* rA0 = {0, 0, Xr*Ai, Xi*Ar} */
         addps rA0, rY0i
         mulps rX0, rt0                 /* rt0 = {0, 0, Xi*Ai, Xr*Ar} */
         addps rt0, rY0r
         movlps -128(pA0,lda), rA0        /* rA0 = {0, 0, Ai, Ar} */
         movaps rA0, rt0                /* rt0 = {0, 0, Ai, Ar} */
         mulps rx0, rA0                 /* rA0 = {0, 0, Xr*Ai, Xi*Ar} */
         addps rA0, rY1i
         mulps rX0, rt0                 /* rt0 = {0, 0, Xi*Ai, Xr*Ar} */
         addps rt0, rY1r
         movlps -128(pA0,lda,2), rA0        /* rA0 = {0, 0, Ai, Ar} */
         movaps rA0, rt0                /* rt0 = {0, 0, Ai, Ar} */
         mulps rx0, rA0                 /* rA0 = {0, 0, Xr*Ai, Xi*Ar} */
         addps rA0, rY2i
         mulps rX0, rt0                 /* rt0 = {0, 0, Xi*Ai, Xr*Ar} */
         addps rt0, rY2r
         movlps -128(pA0,lda3), rA0        /* rA0 = {0, 0, Ai, Ar} */
         movaps rA0, rt0                /* rt0 = {0, 0, Ai, Ar} */
         mulps rx0, rA0                 /* rA0 = {0, 0, Xr*Ai, Xi*Ar} */
         addps rA0, rY3i
         mulps rX0, rt0                 /* rt0 = {0, 0, Xi*Ai, Xr*Ar} */
         addps rt0, rY3r
         add $8, pX
         add $8, pA0
      dec II
      jnz LOOPMCU

MCLEANED:
                                /* rYr0 = {-rY0d, rY0c, -rY0b, rY0a} */
                                /* rYi0 = { iY0d, iY0c,  iY0b, iY0a} */
      mulps NONEPONE, rY0r   /* rYr = {rY0d,  rY0c,    rY0b,   rY0a} */
      mulps NONEPONE, rY1r   /* rYr = {rY1d,  rY1c,    rY1b,   rY1a} */
      haddps rY0i, rY0r   /* rYr = {iY0cd  ,iY0ab,  rY0cd,  rY0ab} */
      haddps rY1i, rY1r   /* rYr = {iY1cd  ,iY1ab,  rY1cd,  rY1ab} */
      haddps rY1r, rY0r   /* rYr = {iY1abcd,rY1abcd,iY0abcd,rY0abcd} */
      #ifndef BETA0
         addpd 0(pY), rY0r
      #endif
      movaps rY0r, 0(pY)
      mulps NONEPONE, rY2r   /* rYr = {rY0d,  rY0c,    rY0b,   rY0a} */
      mulps NONEPONE, rY3r   /* rYr = {rY1d,  rY1c,    rY1b,   rY1a} */
      haddps rY2i, rY2r   /* rYr = {iY0cd  ,iY0ab,  rY0cd,  rY0ab} */
      haddps rY3i, rY3r   /* rYr = {iY1cd  ,iY1ab,  rY1cd,  rY1ab} */
      haddps rY3r, rY2r   /* rYr = {iY1abcd,rY1abcd,iY0abcd,rY0abcd} */
      #ifndef BETA0
         addpd 16(pY), rY2r
      #endif
      movaps rY2r, 16(pY)
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
