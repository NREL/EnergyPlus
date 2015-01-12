/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2006 R. Clint Whaley
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


#if !defined(ATL_GAS_x8664) && !defined(ATL_GAS_x8632)
   #error "This kernel requires x86 assembly!"
#endif

#if !defined(KB) || (KB == 0)
   #error "KB must be a compile-time constant!"
#endif
#if KB > 90
   #error "KB can at most be 90!"
#endif

#ifdef DCPLX
   #define CMUL(arg_) 2*arg_
#else
   #define CMUL(arg_) arg_
#endif
/*
 * Prefetch defines
 */
#if defined(ATL_SSE1) || defined(ATL_SSE2)
   #define pref2(mem) prefetcht1   mem
   #define prefB(mem) prefetcht0   mem
   #ifdef ATL_3DNow
      #define prefC(mem) prefetchw  mem
   #else
      #define prefC(mem) prefetchnta  mem
   #endif
#elif defined(ATL_3DNow)
   #define pref2(mem) prefetch   mem
   #define prefB(mem) prefetch   mem
   #define prefC(mem) prefetchw  mem
#else
   #define pref2(mem)
   #define prefB(mem)
   #define prefC(mem)
#endif
#ifdef ATL_GAS_x8632
   #define movq movl
   #define addq addl
   #define subq subl
   #define shrq shrl
   #define rsp  esp
   #define STKSIZE 36
   #define IOFF    STKSIZE-4
   #define MOFF    IOFF-4
   #define JOFF    MOFF-4
   #define iAOFF    JOFF-4
   #define iCOFF    iAOFF-4
   #define BETAOFF  STKSIZE+40
#endif
/*
 *Integer register usage shown by these defines
 */
#ifdef ATL_GAS_x8632
   #define pA0     %ecx
   #define lda     %ebx
   #define lda3    %ebp
   #define pAE     pA0
   #define pB0     %eax
   #define pC0     %esi
   #define pBE     pB0
   #define ldb     %edi
   #define pfA     %edx

   #define incAn   iAOFF(%esp)
   #define incCn   iCOFF(%esp)
   #define MM      IOFF(%esp)
   #define NN      JOFF(%esp)
   #define MM0     MOFF(%esp)
#else
   #define pA0     %rcx
   #define lda     %rbx
   #define lda3    %rbp
   #define pAE     %rdi
   #define pB0     %rax
   #define pC0     %rsi
   #define pBE     %rdx
   #define incAn   %r8
   #define incCn   %r9
   #define ldb     %r10
   #define MM      %r11
   #define NN      %r12
   #define pfA     %r13
   #define MM0     %r14
#endif
/*
                      %rdi/4       %rsi/8       %rdx/12          %xmm0/16
 void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha,
                       %rcx/24         %r8/28         %r9/32           8/36
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                        %xmm1/40    16/48          24/52
                 const TYPE beta, TYPE *C, const int ldc)
*/
        .text
.global ATL_asmdecor(ATL_USERMM)
ALIGN16
ATL_asmdecor(ATL_USERMM):
/*
 *      Save callee-saved iregs
 */
#ifdef ATL_GAS_x8632
        sub     $STKSIZE, %esp
        movl    %ebp, (%esp)
        movl    %ebx, 4(%esp)
        movl    %esi, 8(%esp)
        movl    %edi, 12(%esp)
#else
        movq    %rbp, -8(%rsp)
        movq    %rbx, -16(%rsp)
        movq    %r12, -24(%rsp)
        movq    %r13, -32(%rsp)
        movq    %r14, -40(%rsp)
                                        pref2((pA0))
/*        movq    %r15, -48(%rsp) */
#endif

/*
 *      Setup input parameters
 */
#ifdef ATL_GAS_x8632
        movl    STKSIZE+4(%esp), lda3
        movl    lda3, MM0
        movl    STKSIZE+8(%esp), lda3
        movl    lda3, NN
        movl    STKSIZE+24(%esp), pA0
                                pref2((pA0))
        movl    STKSIZE+28(%esp), lda
                                pref2((pA0,lda))
        movl    STKSIZE+32(%esp), pB0
                                pref2((pB0))
        movl    STKSIZE+36(%esp), ldb
                                pref2((pA0,lda,2))
        movl    STKSIZE+48(%esp), pC0
                                pref2(KB*8(pA0,lda,2))
/*
 *      incCn = (ldc - M)*sizeof
 */
        movl    STKSIZE+52(%esp), lda3
        subl    MM0, lda3
   #ifdef DCPLX
        shl     $4, lda3
   #else
        shl     $3, lda3
   #endif
        movl    lda3, incCn
/*
 *      pA0 += 128; pB0 += 128
 */
        sub     $-128, pA0
        sub     $-128, pB0
                                prefB(-64(pB0))
/*
 *      lda *= sizeof; ldb *= sizeof; lda3 = lda*3
 */
        shl     $3, lda
                                prefB((pB0))
        shl     $3, ldb
                                prefB(64(pB0))
        lea     (lda,lda,2), lda3
/*
 *      pfA = A + lda*M; incAn = lda*M
 */
        movl    MM0, pfA
                                prefB(128(pB0))
        imull   lda, pfA
                                prefB(192(pB0))
                                prefB(256(pB0))
        movl    pfA, incAn
        lea     -128(pA0, pfA), pfA
                                prefB(320(pB0))
        shrl    $2, MM0            /* MM0 = MM0 / mu */
#else
   #ifdef BETAX
      #define BETAOFF -48
        movlpd  %xmm1, BETAOFF(%rsp)
   #endif
        movq    %rdi, MM0
        movq    %rsi, NN
        movq    %r8, lda
                                        pref2((pA0,lda))
        movq    %r9, pB0
                                        prefB((pB0))
        movslq  8(%rsp), ldb
                                        pref2((pA0,lda,2))
        movq    16(%rsp), pC0
        movslq  24(%rsp), incCn
                                        pref2(KB*8(pA0,lda,2))
/*
 *      incCn = (ldc-M)*sizeof
 */
        sub     MM0, incCn
#ifdef DCPLX
        shl     $4, incCn
#else
        shl     $3, incCn
#endif
/*
 *      pA0 += 128; pB0 += 128
 */
        sub     $-128, pA0
        sub     $-128, pB0
                                        prefB(-64(pB0))
/*
 *      lda = lda*sizeof;  lda3 = lda*3
 */
        shl     $3, lda
                                                prefB((pB0))
        lea     (lda,lda,2), lda3
/*
 *      ldb = ldb*sizeof
 */
        shl     $3, ldb
                                                prefB(64(pB0))
/*
 *      pfA = A + lda*M ; incAn = lda*M, pfB = B + ldb*N
 */
        movq    lda, pfA
                                                prefB(128(pB0))
        imulq   MM0, pfA
/*                                                prefB(192(pB0)) */
/*                                                prefB(256(pB0)) */
        movq    pfA, incAn
/*        movq    ldb, pfB */
/*        imulq   NN, pfB */
        lea     -128(pA0, pfA), pfA
/*                                                prefB(320(pB0)) */
/*       lea     -128-(MB-8)*KB*8(pA0, pfA), pfA */
/*
 *      pAE (pointer to end of column of A) = pA + lda
 */
   #if KB > 32
/*        lea     -128(pA0,lda), pAE */
/*        lea     -128(pB0,ldb), pBE */
        lea     KB*8-128(pA0), pAE
        lea     KB*8-128(pB0), pBE
   #endif
/*
 *      MM0 = MM0/mu
 */
        shr     $2, MM0
#endif
ALIGN16
NLOOP:
#ifdef ATL_GAS_x8632
        movl    MM0, lda3
        movl    lda3, MM
        lea     (lda,lda,2), lda3
#else
        movq    MM0, MM
#endif
        prefB(-128(pB0,ldb,2))
        prefB(-64(pB0,ldb,2))
        prefB((pB0,ldb,2))
MLOOP:
        prefC((pC0))
        fldl    0-128(pB0)       /* st = rB0 */
        fldl    0-128(pA0)       /* st = rA0, rB0 */
        fmul    %st(1), %st      /* st = rA0*rB0, rB0 */
        fldl    0-128(pA0,lda)   /* st = rA1, rC0, rB0 */
        fmul    %st(2), %st      /* st = rA1*rB0, rC0, rB0 */
        fldl    0-128(pA0,lda,2) /*       st = rA2, rC1, rC0, rB0 */
        fmul    %st(3), %st      /* st = rA2*rB0, rC1, rC0, rB0 */
        fxch    %st(3)           /* st = rB0, rC1, rC0, rC2 */
        fmull   0-128(pA0,lda3)  /* st = rA3*rB0, rC1, rC0, rC2 */
/*KLOOP: */                      /* st = rC3, rC1, rC0, rC2 */
#if KB > 1
	fldl	8-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 2
	fldl	16-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	16-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	16-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	16-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	16-128(pA0,lda3) /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 3
	fldl	24-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	24-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	24-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	24-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	24-128(pA0,lda3) /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 4
	fldl	32-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	32-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	32-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	32-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	32-128(pA0,lda3) /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 5
	fldl	40-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	40-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	40-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	40-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	40-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 6
	fldl	48-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	48-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	48-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	48-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	48-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 7
	fldl	56-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	56-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	56-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	56-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	56-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 8
	fldl	64-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	64-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	64-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	64-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	64-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 9
	fldl	72-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	72-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	72-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	72-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	72-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 10
	fldl	80-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	80-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	80-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	80-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	80-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 11
	fldl	88-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	88-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	88-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	88-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	88-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 12
	fldl	96-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	96-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	96-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	96-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	96-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 13
	fldl	104-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	104-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	104-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	104-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	104-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 14
	fldl	112-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	112-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	112-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	112-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	112-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 15
	fldl	120-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	120-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	120-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	120-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	120-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 16
	fldl	128-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	128-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	128-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	128-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	128-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 17
	fldl	136-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	136-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	136-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	136-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	136-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 18
	fldl	144-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	144-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	144-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	144-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	144-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 19
	fldl	152-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	152-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	152-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	152-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	152-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 20
	fldl	160-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	160-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	160-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	160-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	160-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 21
	fldl	168-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	168-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	168-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	168-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	168-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 22
	fldl	176-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	176-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	176-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	176-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	176-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 23
	fldl	184-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	184-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	184-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	184-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	184-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 24
	fldl	192-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	192-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	192-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	192-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	192-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 25
	fldl	200-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	200-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	200-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	200-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	200-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 26
	fldl	208-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	208-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	208-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	208-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	208-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 27
	fldl	216-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	216-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	216-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	216-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	216-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 28
	fldl	224-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	224-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	224-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	224-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	224-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 29
	fldl	232-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	232-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	232-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	232-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	232-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 30
	fldl	240-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	240-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	240-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	240-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	240-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 31
	fldl	248-128(pB0)	 /* st = rB0, rC3, rC1, rC0, rC2 */
#if KB > 32 && defined(ATL_GAS_x8632)
        addl    $KB*8-128, pB0
#endif
	fldl	248-128(pA0)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	248-128(pA0,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	248-128(pA0,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	248-128(pA0,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
#if KB > 32 && defined(ATL_GAS_x8632)
        addl    $KB*8-128, pA0
#endif
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if (KB > 32)
   #ifdef ATL_GAS_x8632
ALIGN8
   #endif
	fldl	8*(32-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(32-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(32-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(32-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(32-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 33
	fldl	8*(33-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(33-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(33-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(33-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(33-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 34
	fldl	8*(34-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(34-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(34-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(34-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(34-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 35
	fldl	8*(35-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(35-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(35-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(35-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(35-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 36
	fldl	8*(36-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(36-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(36-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(36-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(36-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 37
	fldl	8*(37-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(37-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(37-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(37-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(37-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 38
	fldl	8*(38-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(38-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(38-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(38-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(38-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 39
	fldl	8*(39-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(39-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(39-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(39-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(39-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 40
	fldl	8*(40-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(40-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(40-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(40-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(40-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 41
	fldl	8*(41-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(41-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(41-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(41-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(41-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 42
	fldl	8*(42-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(42-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(42-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(42-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(42-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 43
	fldl	8*(43-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(43-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(43-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(43-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(43-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 44
	fldl	8*(44-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(44-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(44-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(44-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(44-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 45
	fldl	8*(45-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(45-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(45-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(45-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(45-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 46
	fldl	8*(46-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(46-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(46-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(46-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(46-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 47
	fldl	8*(47-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(47-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(47-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(47-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(47-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 48
	fldl	8*(48-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(48-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(48-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(48-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(48-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 49
	fldl	8*(49-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(49-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(49-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(49-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(49-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 50
	fldl	8*(50-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(50-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(50-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(50-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(50-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 51
	fldl	8*(51-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(51-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(51-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(51-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(51-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 52
	fldl	8*(52-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(52-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(52-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(52-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(52-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 53
	fldl	8*(53-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(53-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(53-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(53-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(53-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 54
	fldl	8*(54-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(54-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(54-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(54-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(54-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 55
	fldl	8*(55-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(55-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(55-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(55-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(55-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 56
	fldl	8*(56-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(56-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(56-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(56-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(56-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 57
	fldl	8*(57-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(57-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(57-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(57-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(57-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 58
	fldl	8*(58-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(58-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(58-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(58-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(58-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 59
	fldl	8*(59-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(59-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(59-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(59-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(59-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 60
	fldl	8*(60-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(60-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(60-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(60-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(60-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 61
	fldl	8*(61-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(61-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(61-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(61-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(61-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 62
	fldl	8*(62-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(62-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(62-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(62-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(62-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 63
	fldl	8*(63-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(63-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(63-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(63-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(63-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 64
	fldl	8*(64-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(64-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(64-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(64-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(64-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 65
	fldl	8*(65-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(65-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(65-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(65-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(65-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 66
	fldl	8*(66-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(66-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(66-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(66-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(66-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 67
	fldl	8*(67-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(67-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(67-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(67-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(67-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 68
	fldl	8*(68-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(68-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(68-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(68-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(68-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 69
	fldl	8*(69-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(69-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(69-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(69-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(69-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 70
	fldl	8*(70-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(70-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(70-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(70-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(70-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 71
	fldl	8*(71-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(71-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(71-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(71-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(71-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 72
	fldl	8*(72-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(72-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(72-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(72-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(72-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 73
	fldl	8*(73-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(73-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(73-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(73-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(73-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 74
	fldl	8*(74-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(74-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(74-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(74-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(74-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 75
	fldl	8*(75-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(75-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(75-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(75-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(75-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 76
	fldl	8*(76-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(76-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(76-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(76-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(76-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 77
	fldl	8*(77-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(77-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(77-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(77-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(77-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 78
	fldl	8*(78-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(78-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(78-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(78-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(78-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 79
	fldl	8*(79-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(79-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(79-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(79-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(79-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 80
	fldl	8*(80-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(80-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(80-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(80-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(80-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 81
	fldl	8*(81-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(81-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(81-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(81-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(81-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 82
	fldl	8*(82-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(82-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(82-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(82-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(82-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 83
	fldl	8*(83-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(83-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(83-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(83-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(83-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 84
	fldl	8*(84-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(84-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(84-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(84-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(84-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 85
	fldl	8*(85-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(85-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(85-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(85-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(85-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 86
	fldl	8*(86-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(86-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(86-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(86-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(86-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 87
	fldl	8*(87-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(87-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(87-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(87-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(87-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 88
	fldl	8*(88-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(88-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(88-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(88-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(88-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
#if KB > 89
	fldl	8*(89-KB)(pBE)	 /* st = rB0, rC3, rC1, rC0, rC2 */
	fldl	8*(89-KB)(pAE)	 /* st = rA0, rB0, rC3, rC1, rC0, rC2 */
	fmul	%st(1), %st	 /* st = rA0*rB0, rB0, rC3, rC1, rC0, rC2 */
	faddp	%st, %st(4)	 /* st = rB0, rC3, rC1, rC0+, rC2 */
	fldl	8*(89-KB)(pAE,lda)	 /* st = rA1, rB0, rC3, rC1, rC0+, rC2 */
	fmul	%st(1),%st	 /* st = rA1*rB0, rB0, rC3, rC1, rC0+, rC2 */
	faddp	%st, %st(3)	 /* st = rA1*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	fldl	8*(89-KB)(pAE,lda,2) /* st = rA2, rB0, rC3, rC1+, rC0+,rC2 */
	fmul	%st(1), %st	 /* st = rA2*rB0, rB0, rC3, rC1+, rC0+, rC2 */
	faddp	%st, %st(5)	 /* st = rB0, rC3, rC1+, rC0+, rC2+ */
	fmull	8*(89-KB)(pAE,lda3)	 /* st = rA3*rB0, rC3, rC1+, rC0+, rC2+ */
	faddp	%st, %st(1)	 /* st = rC3+, rC1+, rC0+, rC2+ */
#endif
/*        jnz     KLOOP */
#DONEK:
#ifdef BETA0
        fstpl   CMUL(24)(pC0)
        fstpl   CMUL(8)(pC0)
        fstpl   (pC0)
        fstpl   CMUL(16)(pC0)
#elif defined(BETA1) || defined(BETAN1)
   #ifdef BETAN1
      #define faddl fsubl
   #endif
                                 /* st = rC3, rC1, rC0, rC2  */
        fxch    %st(2)           /* st = rC0, rC1, rC3, rC2 */
        faddl   (pC0)            /* st = rC0+,rC1, rC3, rC2 */
        fstpl   (pC0)            /* st = rC1, rC3, rC2 */
        faddl   CMUL(8)(pC0)     /* st = rC1+,rC3, rC2 */
        fstpl   CMUL(8)(pC0)     /* st = rC3, rC2 */
        fxch    %st(1)           /* st = rC2, rC3 */
        faddl   CMUL(16)(pC0)    /* st = rC2+,rC3 */
        fstpl   CMUL(16)(pC0)    /* st = rC3 */
        faddl   CMUL(24)(pC0)    /* st = rC3+ */
        fstpl   CMUL(24)(pC0)    /* st = null */
   #ifdef BETAN1
      #undef  faddl
   #endif
#else
                                 /* st = rC3, rC1, rC0, rC2  */
        fldl    BETAOFF(%rsp)    /* st = bet, rC3, rC1, rC0, rC2 */
        fldl    (pC0)            /* st = c0, bet, rC3, rC1, rC0, rC2 */
        fmul    %st(1),%st       /* st = c0*bet, bet, rC3, rC1, rC0, rC2 */
        faddp   %st, %st(4)      /* st = bet, rC3, rC1, rC0+, rC2 */
        fldl    CMUL(8)(pC0)     /* st = c1, bet, rC3, rC1, rC0+, rC2 */
        fmul    %st(1),%st       /* st = c1*bet, bet, rC3, rC1, rC0+, rC2 */
        faddp   %st,%st(3)       /* st = bet, rC3, rC1+, rC0+, rC2 */
        fldl    CMUL(16)(pC0)    /* st = c2, bet, rC3, rC1+, rC0+, rC2 */
        fmul    %st(1),%st       /* st = bet*c2, bet, rC3, rC1+, rC0+, rC2 */
        faddp   %st,%st(5)       /* st = bet, rC3, rC1+, rC0+, rC2+ */
        fmull   CMUL(24)(pC0)    /* st = bet*c3, rC3, rC1+, rC0+, rC2+ */
        faddp   %st,%st(1)       /* st = rC3+, rC1+, rC0+, rC2+ */
        fstpl   CMUL(24)(pC0)
        fstpl   CMUL(8)(pC0)
        fstpl   (pC0)
        fstpl   CMUL(16)(pC0)
#endif
        add     $4*CMUL(8), pC0
          pref2((pfA))
          add     $32, pfA

#if KB > 32 && defined(ATL_GAS_x8632)
        lea     128-KB*8(pA0,lda,4), pA0
        subl    $KB*8-128, pB0
#else
        lea     0(pA0,lda,4), pA0
   #if KB > 32
        lea     0(pAE,lda,4), pAE
   #endif
#endif
        sub     $1, MM
        jnz     MLOOP

        sub     incAn, pA0
        prefB(64(pB0,ldb,2))
   #if KB > 32 && !defined(ATL_GAS_x8632)
        sub     incAn, pAE
        add     ldb, pBE
   #endif
        prefB(128(pB0,ldb,2))
        add     incCn, pC0
        prefB(192(pB0,ldb,2))
        add     ldb, pB0
        prefB(256(pB0,ldb,2))
        prefB(320(pB0,ldb,2))
        sub     $1, NN
        jnz     NLOOP

DONE:
#ifdef ATL_GAS_x8632
        movl    (%esp), %ebp
        movl    4(%esp), %ebx
        movl    8(%esp), %esi
        movl    12(%esp), %edi
        add     $STKSIZE, %esp
#else
        movq    -8(%rsp), %rbp
        movq    -16(%rsp), %rbx
        movq    -24(%rsp), %r12
        movq    -32(%rsp), %r13
        movq    -40(%rsp), %r14
/*        movq    -48(%rsp), %r15 */
#endif
        ret
