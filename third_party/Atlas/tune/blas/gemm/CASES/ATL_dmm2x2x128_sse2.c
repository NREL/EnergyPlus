/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2008 R. Clint Whaley
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
#ifndef ATL_SSE2
   #error "This routine requires SSE2!"
#endif
#include "atlas_asm.h"
#if !defined(ATL_GAS_x8664) && !defined(ATL_GAS_x8632)
   #error "This kernel requires x86 assembly!"
#endif

#if !defined(KB) || (KB == 0)
   #error "KB must be a compile-time constant!"
#endif
#if KB > 128
   #error "KB can at most be 128!"
#endif
#if (KB/2)*2 != KB
   #error "KB must be a multiple of 2
#endif

#ifdef DCPLX
   #define CMUL(arg_) 2*arg_
   #define CSH 4
#else
   #define CMUL(arg_) arg_
   #define CSH 3
#endif
#ifdef ATL_GAS_x8632
   #define movq movl
   #define addq addl
   #define subq subl
   #define shrq shrl
   #define testq testl
   #define rsp  esp
   #ifdef BETAX
      #define BETAOFF 0
      #define BETASZ 16
   #else
      #define BETASZ 0
   #endif
   #define FSIZE 12*4+BETASZ
   #define PFAOFF  FSIZE-4
   #define PFBOFF  PFAOFF-4
   #define IOFF    PFBOFF-4
   #define MOFF    IOFF-4
   #define iAnOFF  MOFF-4
   #define iCnOFF  iAnOFF-4
   #define JOFF    iCnOFF-4
#endif
/*
 *Integer register usage shown by these defines
 */
#ifdef ATL_GAS_x8632
   #define pA0     %ecx
   #define lda     %eax
   #define pB0     %ebx
   #define ldb     %edi
   #define pC0     %esi
   #define itmp    %edx
   #define ldc     %ebp

   #define incAn   iAnOFF(%esp)
   #define incCn   iCnOFF(%esp)
   #define MM      IOFF(%esp)
   #define NN      JOFF(%esp)
   #define MM0     MOFF(%esp)
   #define pfA     PFAOFF(%esp)
   #define pfB     PFBOFF(%esp)
   #define BETA    BETAOFF(%esp)
#else
   #define pA0     %rcx
   #define lda     %rbx
   #define pB0     %rbp
   #define ldb     %rax
   #define pC0     %rdi
   #define pfA     %rdx
   #define pfB     %rsi
   #define incCn   %r8
   #define ldc     %r9
   #define MM0     %r10
   #define incAn   %r11
   #define MM      %r12
   #define NN      %r13
   #define itmp    ldc
#endif
#define rA0     %xmm0
#define rA1     %xmm1
#define ra0     %xmm2
#define rB0     %xmm3
#define rC00    %xmm4
#define rC10    %xmm5
#define rC01    %xmm6
#define rC11    %xmm7
#ifdef ATL_GAS_x8664
   #define BETA %xmm8
#endif
/*
 * Define some macros for instruction selection
 *    VZERO: xorpd, xorps, pxor
 *    MOVAB: movapd,movaps or movupd/movups
 */
#define VZERO(reg_) xorps reg_, reg_
#define MOVAB  movaps
#define MOVAPD movaps
#define MOVUPD movups
#define PFAINC -64
#define PFBINC 32
#define movlpd movlps
#define movhpd movhps
#if 1
   #define pref2(mem) prefetcht1        mem
   #define prefB(mem) prefetcht0        mem
   #define prefC(mem) prefetcht0        mem
#else
   #define pref2(mem)
   #define prefB(mem)
   #define prefC(mem)
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
        movl    %esp, %eax              /* save original stack ptr */
        sub     $FSIZE, %esp            /* allocate stack space */
        andw    $0xFFF0, %sp            /* SP now 16-byte aligned */
        movl    %ebp, BETASZ(%esp)
        movl    %ebx, BETASZ+4(%esp)
        movl    %esi, BETASZ+8(%esp)
        movl    %edi, BETASZ+12(%esp)
        movl    %eax, BETASZ+16(%esp)   /* original SP saved to new stack */
#else
        movq    %rbp, -8(%rsp)
        movq    %rbx, -16(%rsp)
        movq    %r12, -24(%rsp)
        movq    %r13, -32(%rsp)
#endif

/*
 *      Setup input parameters
 *      For x8632 %eax has old stack ptr; eax is lda, so set this up late
 */
#ifdef ATL_GAS_x8632
        movl    4(%eax), itmp
        movl    itmp, MM0
        movl    itmp, MM
        movl    8(%eax), itmp
        movl    itmp, NN
        movl    24(%eax), pA0
        movl    32(%eax), pB0
        movl    36(%eax), ldb           /* ldb = ldb */
        shl     $3, ldb                 /* ldb = ldb*sizeof */
        lea     (pB0, ldb, 2), itmp     /* itmp = pB0 + 2*ldb*sizeof */
        movl    itmp, pfB               /* pfB = pB0 + 2*ldb*sizeof */
#ifdef BETAX
        movsd   40(%eax), rB0           /* load beta */
        unpcklpd rB0, rB0               /* rB0 = {beta, beta} */
        MOVAPD  rB0, BETAOFF(%esp)      /* store BETA to BETAOFF */
#endif
        movl    48(%eax), pC0
        movl    52(%eax), ldc           /* ldc = ldc */
        lea     (ldc,ldc), itmp         /* itmp = 2*ldc */
        shl     $CSH, ldc               /* ldc = ldc*sizeof */
        sub     MM0, itmp               /* itmp = 2*ldc - M */
        shl     $CSH, itmp              /* itmp = (2*ldc-M)*sizeof */
        movl    itmp, incCn             /* incCn = (2*ldc-M)*sizeof */
        movl    28(%eax), lda           /* lda = lda; overwrote old SP in EAX */
        shl     $3, lda                 /* lda = lda*sizeof */
/*
 *      pfA = A + 2*lda*M; incAn = lda*M
 */
        lea     (lda,lda), itmp         /* itmp = 2*lda*sizeof */
        imull   MM0, itmp               /* itmp = 2*lda*M*sizeof */
        lea     PFAINC(pA0, itmp), itmp /* pfA = pA0 + 2*lda*M - PFAINC */
        movl    itmp, pfA               /* pfA = 2*lda*M + pA0 - PFAINC */
        sub     pA0, itmp               /* itmp = 2*lda*M - PFAINC*/
        sub     $PFAINC, itmp           /* itmp = 2*lda*M */
        shr     $1, itmp                /* itmp = lda*M */
        movl    itmp, incAn             /* incAn = lda*M */
        movl    MM0, itmp
        movl    itmp, MM
#else
/*
 *      Get parameters moves to correct registers
 */
        movq    %rdi, MM
        movq    %rsi, NN
        movq    %r8, lda                /* lda = lda */
        movq    %r9, pB0                /* pB0 = B */
        movslq  8(%rsp), ldb            /* ldb = ldb */
        unpcklpd        %xmm1, %xmm1    /* xmm1 = beta, beta */
        MOVAPD  %xmm1, BETA             /* BETA = beta, beta */
        movq    16(%rsp), pC0           /* pC0 = C */
        movslq  24(%rsp), ldc           /* ldc = ldc */
/*
 *      ===================================================
 *      Compute rest of needed variables using these inputs
 *      ===================================================
 */
        shl     $3, ldb                 /* ldb = ldb*sizeof */
        lea     (ldc,ldc), incCn        /* incCn = 2*ldc */
        sub     MM, incCn               /* incCn = 2*ldc - M */
        shl     $CSH, incCn             /* incCn = (2*ldc-M)*sizeof */
        shl     $CSH, ldc               /* ldc *= sizeof */
        shl     $3, lda                 /* lda = lda * sizeof */
        mov     MM, incAn               /* incAn = M */
        imulq   lda, incAn              /* incAn = M * lda*sizeof */
        lea     PFAINC(pA0,incAn,2),pfA /* pfA = pA0+2*M*lda*sizeof - PFAINC */
        lea     (pB0, ldb, 2),pfB       /* pfB = pB0 + 2*ldb*sizeof */
        mov     MM, MM0                 /* MM0 = MM */
#endif
/*
 *      Advance A & B by 128 so we can use byte indexing as long as possible
 */
        sub     $-128, pA0              /* pA0 += 128 */
        sub     $-128, pB0              /* pB0 += 128 */
/*
 * ======================================================================
 * This set of loops assumes aligned C & ldc; CPLX code takes every other
 * elt of C, and thus can't use aligned C access in any case
 * ======================================================================
 */
#ifndef DCPLX
        test    $15, pC0
        jnz     UNALIGNED_C
        testq   $15, ldc
        jnz     UNALIGNED_C

ALIGN16
MNLOOP:
/*
 *      Peel 1st iteration of K to avoid need to zero rCxx
 */
        MOVAB   -128(pB0), rA0
        MOVAB   -128(pA0), rC00
        MOVAPD  rC00, rC01
        mulpd   rA0, rC00
        #ifdef ATL_GAS_x8632
           movq    pfB, itmp
        #else
           prefB((pfB))
        #endif
        MOVAB   -128(pA0,lda), rC10
        MOVAPD  rC10, rC11
        mulpd   rA0, rC10
        #ifdef ATL_GAS_x8632
           prefB((itmp))
        #else
           add      $PFBINC, pfB
        #endif
        MOVAB   -128(pB0,ldb), rA0
        mulpd   rA0, rC01
        #if KB > 2
           MOVAB   -112(pB0), rB0
        #endif
        mulpd   rA0, rC11
        #if KB == 2
           #ifdef DCPLX
              movsd  (pC0), rA0
              movhpd 16(pC0), rA0
              movsd  (pC0,ldc), ra0
              movhpd 16(pC0,ldc), ra0
           #else
              MOVAPD (pC0), rA0
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        #ifdef ATL_GAS_x8632
           add  $PFBINC, pfB
        #endif

   #if KB > 4
        MOVAB   -112(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -112(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -112(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   -112+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 4
        MOVAB   -112(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -112(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -112(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 6
        MOVAB   -96(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -96(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -96(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   -96+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 6
        MOVAB   -96(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -96(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -96(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 8
        MOVAB   -80(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -80(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -80(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   -80+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 8
        MOVAB   -80(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -80(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -80(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 10
        MOVAB   -64(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -64(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -64(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   -64+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 10
        MOVAB   -64(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -64(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -64(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 12
        MOVAB   -48(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -48(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -48(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   -48+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 12
        MOVAB   -48(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -48(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -48(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 14
        MOVAB   -32(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -32(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -32(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   -32+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 14
        MOVAB   -32(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -32(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -32(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 16
        MOVAB   -16(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -16(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -16(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   -16+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 16
        MOVAB   -16(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -16(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -16(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 18
        MOVAB   0(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   0(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   0(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   0+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 18
        MOVAB   0(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   0(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   0(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 20
        MOVAB   16(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   16(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   16(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   16+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 20
        MOVAB   16(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   16(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   16(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 22
        MOVAB   32(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   32(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   32(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   32+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 22
        MOVAB   32(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   32(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   32(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 24
        MOVAB   48(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   48(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   48(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   48+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 24
        MOVAB   48(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   48(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   48(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 26
        MOVAB   64(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   64(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   64(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   64+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 26
        MOVAB   64(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   64(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   64(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 28
        MOVAB   80(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   80(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   80(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   80+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 28
        MOVAB   80(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   80(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   80(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 30
        MOVAB   96(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   96(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   96(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   96+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 30
        MOVAB   96(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   96(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   96(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 32
        MOVAB   112(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   112(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   112(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   112+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 32
        MOVAB   112(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   112(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   112(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 34
        MOVAB   128(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   128(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   128(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   128+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 34
        MOVAB   128(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   128(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   128(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 36
        MOVAB   144(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   144(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   144(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   144+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 36
        MOVAB   144(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   144(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   144(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 38
        MOVAB   160(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   160(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   160(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   160+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 38
        MOVAB   160(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   160(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   160(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 40
        MOVAB   176(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   176(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   176(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   176+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 40
        MOVAB   176(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   176(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   176(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 42
        MOVAB   192(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   192(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   192(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   192+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 42
        MOVAB   192(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   192(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   192(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 44
        MOVAB   208(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   208(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   208(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   208+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 44
        MOVAB   208(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   208(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   208(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 46
        MOVAB   224(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   224(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   224(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   224+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 46
        MOVAB   224(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   224(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   224(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 48
        MOVAB   240(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   240(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   240(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   240+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 48
        MOVAB   240(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   240(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   240(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 50
        MOVAB   256(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   256(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   256(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   256+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 50
        MOVAB   256(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   256(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   256(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 52
        MOVAB   272(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   272(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   272(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   272+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 52
        MOVAB   272(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   272(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   272(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 54
        MOVAB   288(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   288(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   288(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   288+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 54
        MOVAB   288(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   288(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   288(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 56
        MOVAB   304(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   304(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   304(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   304+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 56
        MOVAB   304(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   304(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   304(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 58
        MOVAB   320(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   320(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   320(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   320+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 58
        MOVAB   320(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   320(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   320(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 60
        MOVAB   336(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   336(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   336(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   336+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 60
        MOVAB   336(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   336(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   336(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 62
        MOVAB   352(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   352(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   352(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   352+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 62
        MOVAB   352(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   352(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   352(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 64
        MOVAB   368(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   368(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   368(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   368+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 64
        MOVAB   368(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   368(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   368(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 66
        MOVAB   384(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   384(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   384(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   384+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 66
        MOVAB   384(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   384(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   384(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 68
        MOVAB   400(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   400(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   400(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   400+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 68
        MOVAB   400(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   400(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   400(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 70
        MOVAB   416(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   416(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   416(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   416+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 70
        MOVAB   416(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   416(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   416(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 72
        MOVAB   432(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   432(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   432(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   432+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 72
        MOVAB   432(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   432(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   432(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 74
        MOVAB   448(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   448(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   448(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   448+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 74
        MOVAB   448(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   448(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   448(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 76
        MOVAB   464(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   464(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   464(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   464+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 76
        MOVAB   464(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   464(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   464(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 78
        MOVAB   480(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   480(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   480(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   480+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 78
        MOVAB   480(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   480(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   480(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 80
        MOVAB   496(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   496(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   496(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   496+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 80
        MOVAB   496(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   496(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   496(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 82
        MOVAB   512(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   512(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   512(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   512+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 82
        MOVAB   512(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   512(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   512(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 84
        MOVAB   528(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   528(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   528(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   528+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 84
        MOVAB   528(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   528(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   528(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 86
        MOVAB   544(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   544(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   544(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   544+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 86
        MOVAB   544(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   544(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   544(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 88
        MOVAB   560(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   560(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   560(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   560+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 88
        MOVAB   560(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   560(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   560(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 90
        MOVAB   576(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   576(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   576(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   576+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 90
        MOVAB   576(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   576(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   576(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 92
        MOVAB   592(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   592(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   592(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   592+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 92
        MOVAB   592(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   592(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   592(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 94
        MOVAB   608(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   608(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   608(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   608+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 94
        MOVAB   608(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   608(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   608(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 96
        MOVAB   624(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   624(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   624(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   624+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 96
        MOVAB   624(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   624(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   624(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 98
        MOVAB   640(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   640(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   640(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   640+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 98
        MOVAB   640(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   640(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   640(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 100
        MOVAB   656(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   656(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   656(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   656+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 100
        MOVAB   656(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   656(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   656(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 102
        MOVAB   672(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   672(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   672(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   672+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 102
        MOVAB   672(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   672(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   672(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 104
        MOVAB   688(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   688(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   688(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   688+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 104
        MOVAB   688(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   688(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   688(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 106
        MOVAB   704(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   704(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   704(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   704+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 106
        MOVAB   704(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   704(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   704(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 108
        MOVAB   720(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   720(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   720(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   720+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 108
        MOVAB   720(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   720(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   720(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 110
        MOVAB   736(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   736(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   736(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   736+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 110
        MOVAB   736(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   736(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   736(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 112
        MOVAB   752(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   752(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   752(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   752+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 112
        MOVAB   752(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   752(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   752(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 114
        MOVAB   768(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   768(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   768(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   768+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 114
        MOVAB   768(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   768(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   768(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 116
        MOVAB   784(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   784(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   784(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   784+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 116
        MOVAB   784(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   784(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   784(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 118
        MOVAB   800(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   800(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   800(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   800+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 118
        MOVAB   800(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   800(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   800(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 120
        MOVAB   816(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   816(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   816(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   816+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 120
        MOVAB   816(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   816(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   816(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 122
        MOVAB   832(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   832(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   832(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   832+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 122
        MOVAB   832(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   832(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   832(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 124
        MOVAB   848(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   848(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   848(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   848+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 124
        MOVAB   848(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   848(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   848(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 126
        MOVAB   864(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   864(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   864(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   864+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 126
        MOVAB   864(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   864(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   864(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVAPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVAPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif

        #ifdef ATL_GAS_x8632
           movl    pfA, itmp
        #else
           pref2((pfA))
        #endif
/*
 * After loop pC00 is in rA0, and pC01 is in ra0
 */
#ifndef ATL_SSE3
/*
 * After loop pC00 is in rA0, and pC01 is in ra0, so use pB0 & rA1 here
 */
        MOVAPD          rC00, rA1       /* rA1  = c00a c00b */
        MOVAPD          rC01, rB0       /* rB0  = c01a c01b */
        unpcklpd        rC10, rC00      /* rC00 = c00a c10a */
        unpcklpd        rC11, rC01      /* rC01 = c01a c11a */
        unpckhpd        rC10, rA1       /* rA1  = c00b c10b */
        unpckhpd        rC11, rB0       /* rB0  = c01b c11b */
        addpd           rA1, rC00       /* rC00 = c00ab c10ab */
        addpd           rB0, rC01       /* rC01 = c01ab c11ab */
#endif
        #ifdef ATL_SSE3
           haddpd  rC10, rC00
        #endif
        #ifdef ATL_GAS_x8632
           pref2((itmp))
        #else
           add  $PFAINC, pfA
        #endif
        #ifdef BETAX
           #ifdef ATL_GAS_x8632
              movapd       BETAOFF(%esp), rB0
              mulpd        rB0, rA0
           #else
              mulpd    BETA, rA0
           #endif
        #endif
        #ifndef BETA0
           addpd  rA0, rC00
        #endif
        #ifdef ATL_SSE3
           haddpd  rC11, rC01
        #endif
        #ifdef ATL_GAS_x8632
           add     $PFAINC, itmp
        #endif
        #ifdef BETAX
           #ifdef ATL_GAS_x8632
              mulpd        rB0, ra0
           #else
              mulpd    BETA, ra0
           #endif
        #endif
        #ifndef BETA0
           addpd  ra0, rC01
        #endif
        #ifdef ATL_GAS_x8632
           movl    itmp, pfA
        #endif

        add     $2*CMUL(8), pC0
        lea     (pA0, lda, 2), pA0
        subq    $2, MM
        #ifdef ATL_GAS_x8632
           movl    ldc, itmp
        #endif
   #ifdef DCPLX
        movlpd  rC00, -32(pC0)
        movhpd  rC00, -16(pC0)
        movlpd  rC01, -32(pC0,itmp)
        movhpd  rC01, -16(pC0,itmp)
   #else
        MOVAPD  rC00, -2*CMUL(8)(pC0)
        MOVAPD  rC01, -2*CMUL(8)(pC0,itmp)
   #endif
        jnz     MNLOOP

        subq    incAn, pA0
        addq    incCn, pC0
        lea     (pB0, ldb, 2), pB0
        subq    $2, NN
#ifdef ATL_GAS_x8632
        movl    MM0, itmp
        movl    itmp, MM
#else
        movq    MM0, MM
#endif
        jnz     MNLOOP
#ifdef ATL_GAS_x8632
        movl    BETASZ(%esp), %ebp
        movl    BETASZ+4(%esp), %ebx
        movl    BETASZ+8(%esp), %esi
        movl    BETASZ+12(%esp), %edi
        movl    BETASZ+16(%esp), %esp    /* restore saved original SP */
#else
        movq    -8(%rsp), %rbp
        movq    -16(%rsp), %rbx
        movq    -24(%rsp), %r12
        movq    -32(%rsp), %r13
#endif
        ret
#endif  /* end of ifndef DCPLX -- CPLX must use unaligned loads to C */
ALIGN16
/*
 * Code specialized for when C or ldc is not aligned to 16-byte boundary, so
 * we must use unaligned loads.  This is a big cost on Core2 systems
 */
UNALIGNED_C:
/*
 *      Peel 1st iteration of K to avoid need to zero rCxx
 */
        MOVAB   -128(pB0), rA0
        MOVAB   -128(pA0), rC00
        MOVAPD  rC00, rC01
        mulpd   rA0, rC00
        #ifdef ATL_GAS_x8632
           movq    pfB, itmp
        #else
           prefB((pfB))
        #endif
        MOVAB   -128(pA0,lda), rC10
        MOVAPD  rC10, rC11
        mulpd   rA0, rC10
        #ifdef ATL_GAS_x8632
           prefB((itmp))
        #else
           add      $PFBINC, pfB
        #endif
        MOVAB   -128(pB0,ldb), rA0
        mulpd   rA0, rC01
        #if KB > 2
           MOVAB   -112(pB0), rB0
        #endif
        mulpd   rA0, rC11
        #if KB == 2
           #ifdef DCPLX
              movsd  (pC0), rA0
              movhpd 16(pC0), rA0
              movsd  (pC0,ldc), ra0
              movhpd 16(pC0,ldc), ra0
           #else
              MOVUPD (pC0), rA0
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        #ifdef ATL_GAS_x8632
           add  $PFBINC, pfB
        #endif

   #if KB > 4
        MOVAB   -112(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -112(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -112(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   -112+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 4
        MOVAB   -112(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -112(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -112(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 6
        MOVAB   -96(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -96(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -96(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   -96+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 6
        MOVAB   -96(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -96(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -96(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 8
        MOVAB   -80(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -80(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -80(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   -80+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 8
        MOVAB   -80(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -80(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -80(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 10
        MOVAB   -64(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -64(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -64(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   -64+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 10
        MOVAB   -64(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -64(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -64(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 12
        MOVAB   -48(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -48(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -48(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   -48+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 12
        MOVAB   -48(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -48(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -48(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 14
        MOVAB   -32(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -32(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -32(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   -32+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 14
        MOVAB   -32(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -32(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -32(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 16
        MOVAB   -16(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -16(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -16(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   -16+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 16
        MOVAB   -16(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -16(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -16(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 18
        MOVAB   0(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   0(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   0(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   0+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 18
        MOVAB   0(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   0(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   0(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 20
        MOVAB   16(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   16(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   16(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   16+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 20
        MOVAB   16(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   16(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   16(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 22
        MOVAB   32(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   32(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   32(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   32+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 22
        MOVAB   32(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   32(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   32(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 24
        MOVAB   48(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   48(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   48(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   48+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 24
        MOVAB   48(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   48(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   48(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 26
        MOVAB   64(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   64(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   64(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   64+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 26
        MOVAB   64(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   64(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   64(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 28
        MOVAB   80(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   80(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   80(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   80+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 28
        MOVAB   80(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   80(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   80(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 30
        MOVAB   96(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   96(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   96(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   96+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 30
        MOVAB   96(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   96(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   96(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 32
        MOVAB   112(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   112(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   112(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   112+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 32
        MOVAB   112(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   112(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   112(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 34
        MOVAB   128(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   128(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   128(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   128+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 34
        MOVAB   128(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   128(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   128(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 36
        MOVAB   144(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   144(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   144(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   144+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 36
        MOVAB   144(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   144(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   144(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 38
        MOVAB   160(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   160(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   160(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   160+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 38
        MOVAB   160(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   160(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   160(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 40
        MOVAB   176(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   176(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   176(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   176+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 40
        MOVAB   176(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   176(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   176(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 42
        MOVAB   192(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   192(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   192(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   192+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 42
        MOVAB   192(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   192(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   192(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 44
        MOVAB   208(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   208(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   208(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   208+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 44
        MOVAB   208(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   208(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   208(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 46
        MOVAB   224(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   224(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   224(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   224+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 46
        MOVAB   224(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   224(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   224(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 48
        MOVAB   240(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   240(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   240(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   240+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 48
        MOVAB   240(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   240(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   240(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 50
        MOVAB   256(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   256(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   256(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   256+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 50
        MOVAB   256(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   256(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   256(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 52
        MOVAB   272(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   272(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   272(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   272+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 52
        MOVAB   272(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   272(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   272(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 54
        MOVAB   288(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   288(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   288(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   288+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 54
        MOVAB   288(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   288(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   288(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 56
        MOVAB   304(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   304(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   304(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   304+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 56
        MOVAB   304(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   304(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   304(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 58
        MOVAB   320(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   320(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   320(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   320+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 58
        MOVAB   320(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   320(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   320(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 60
        MOVAB   336(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   336(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   336(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   336+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 60
        MOVAB   336(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   336(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   336(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 62
        MOVAB   352(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   352(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   352(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   352+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 62
        MOVAB   352(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   352(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   352(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 64
        MOVAB   368(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   368(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   368(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   368+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 64
        MOVAB   368(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   368(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   368(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 66
        MOVAB   384(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   384(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   384(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   384+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 66
        MOVAB   384(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   384(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   384(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 68
        MOVAB   400(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   400(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   400(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   400+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 68
        MOVAB   400(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   400(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   400(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 70
        MOVAB   416(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   416(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   416(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   416+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 70
        MOVAB   416(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   416(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   416(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 72
        MOVAB   432(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   432(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   432(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   432+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 72
        MOVAB   432(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   432(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   432(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 74
        MOVAB   448(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   448(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   448(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   448+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 74
        MOVAB   448(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   448(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   448(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 76
        MOVAB   464(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   464(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   464(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   464+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 76
        MOVAB   464(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   464(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   464(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 78
        MOVAB   480(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   480(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   480(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   480+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 78
        MOVAB   480(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   480(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   480(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 80
        MOVAB   496(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   496(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   496(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   496+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 80
        MOVAB   496(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   496(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   496(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 82
        MOVAB   512(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   512(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   512(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   512+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 82
        MOVAB   512(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   512(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   512(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 84
        MOVAB   528(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   528(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   528(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   528+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 84
        MOVAB   528(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   528(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   528(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 86
        MOVAB   544(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   544(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   544(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   544+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 86
        MOVAB   544(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   544(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   544(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 88
        MOVAB   560(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   560(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   560(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   560+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 88
        MOVAB   560(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   560(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   560(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 90
        MOVAB   576(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   576(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   576(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   576+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 90
        MOVAB   576(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   576(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   576(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 92
        MOVAB   592(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   592(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   592(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   592+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 92
        MOVAB   592(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   592(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   592(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 94
        MOVAB   608(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   608(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   608(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   608+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 94
        MOVAB   608(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   608(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   608(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 96
        MOVAB   624(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   624(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   624(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   624+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 96
        MOVAB   624(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   624(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   624(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 98
        MOVAB   640(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   640(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   640(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   640+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 98
        MOVAB   640(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   640(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   640(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 100
        MOVAB   656(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   656(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   656(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   656+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 100
        MOVAB   656(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   656(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   656(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 102
        MOVAB   672(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   672(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   672(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   672+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 102
        MOVAB   672(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   672(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   672(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 104
        MOVAB   688(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   688(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   688(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   688+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 104
        MOVAB   688(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   688(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   688(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 106
        MOVAB   704(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   704(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   704(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   704+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 106
        MOVAB   704(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   704(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   704(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 108
        MOVAB   720(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   720(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   720(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   720+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 108
        MOVAB   720(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   720(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   720(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 110
        MOVAB   736(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   736(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   736(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   736+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 110
        MOVAB   736(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   736(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   736(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 112
        MOVAB   752(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   752(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   752(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   752+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 112
        MOVAB   752(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   752(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   752(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 114
        MOVAB   768(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   768(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   768(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   768+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 114
        MOVAB   768(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   768(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   768(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 116
        MOVAB   784(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   784(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   784(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   784+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 116
        MOVAB   784(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   784(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   784(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 118
        MOVAB   800(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   800(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   800(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   800+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 118
        MOVAB   800(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   800(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   800(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 120
        MOVAB   816(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   816(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   816(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   816+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 120
        MOVAB   816(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   816(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   816(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 122
        MOVAB   832(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   832(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   832(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   832+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 122
        MOVAB   832(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   832(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   832(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 124
        MOVAB   848(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   848(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   848(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   848+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 124
        MOVAB   848(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   848(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   848(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif
   #if KB > 126
        MOVAB   864(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   864(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   864(pB0,ldb), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   864+16(pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #elif KB == 126
        MOVAB   864(pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   864(pA0,lda), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   864(pB0,ldb), rB0
        mulpd   rB0, ra0
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
           #else
              MOVUPD (pC0), rA0
           #endif
        #endif
        mulpd   rB0, rA1
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0), rA0
        #endif
        addpd   ra0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd (pC0,ldc), ra0
           #else
              MOVUPD (pC0,ldc), ra0
           #endif
        #endif
        addpd   rA1, rC11
        #if !defined(BETA0) && defined(DCPLX)
           movhpd 16(pC0,ldc), ra0
        #endif
   #endif

        #ifdef ATL_GAS_x8632
           movl    pfA, itmp
        #else
           pref2((pfA))
        #endif
/*
 * After loop pC00 is in rA0, and pC01 is in ra0
 */
#ifndef ATL_SSE3
/*
 * After loop pC00 is in rA0, and pC01 is in ra0, so use pB0 & rA1 here
 */
        MOVAPD          rC00, rA1       /* rA1  = c00a c00b */
        MOVAPD          rC01, rB0       /* rB0  = c01a c01b */
        unpcklpd        rC10, rC00      /* rC00 = c00a c10a */
        unpcklpd        rC11, rC01      /* rC01 = c01a c11a */
        unpckhpd        rC10, rA1       /* rA1  = c00b c10b */
        unpckhpd        rC11, rB0       /* rB0  = c01b c11b */
        addpd           rA1, rC00       /* rC00 = c00ab c10ab */
        addpd           rB0, rC01       /* rC01 = c01ab c11ab */
#endif
        #ifdef ATL_SSE3
           haddpd  rC10, rC00
        #endif
        #ifdef ATL_GAS_x8632
           pref2((itmp))
        #else
           add  $PFAINC, pfA
        #endif
        #ifdef BETAX
           #ifdef ATL_GAS_x8632
              movapd       BETAOFF(%esp), rB0
              mulpd        rB0, rA0
           #else
              mulpd    BETA, rA0
           #endif
        #endif
        #ifndef BETA0
           addpd  rA0, rC00
        #endif
        #ifdef ATL_SSE3
           haddpd  rC11, rC01
        #endif
        #ifdef ATL_GAS_x8632
           add     $PFAINC, itmp
        #endif
        #ifdef BETAX
           #ifdef ATL_GAS_x8632
              mulpd        rB0, ra0
           #else
              mulpd    BETA, ra0
           #endif
        #endif
        #ifndef BETA0
           addpd  ra0, rC01
        #endif
        #ifdef ATL_GAS_x8632
           movl    itmp, pfA
        #endif

        add     $2*CMUL(8), pC0
        lea     (pA0, lda, 2), pA0
        subq    $2, MM
        #ifdef ATL_GAS_x8632
           movl    ldc, itmp
        #endif
   #ifdef DCPLX
        movlpd  rC00, -32(pC0)
        movhpd  rC00, -16(pC0)
        movlpd  rC01, -32(pC0,itmp)
        movhpd  rC01, -16(pC0,itmp)
   #else
        MOVUPD  rC00, -2*CMUL(8)(pC0)
        MOVUPD  rC01, -2*CMUL(8)(pC0,itmp)
   #endif
        jnz     UNALIGNED_C

        subq    incAn, pA0
        addq    incCn, pC0
        lea     (pB0, ldb, 2), pB0
        subq    $2, NN
#ifdef ATL_GAS_x8632
        movl    MM0, itmp
        movl    itmp, MM
#else
        movq    MM0, MM
#endif
        jnz     UNALIGNED_C
#ifdef ATL_GAS_x8632
        movl    BETASZ(%esp), %ebp
        movl    BETASZ+4(%esp), %ebx
        movl    BETASZ+8(%esp), %esi
        movl    BETASZ+12(%esp), %edi
        movl    BETASZ+16(%esp), %esp    /* restore saved original SP */
#else
        movq    -8(%rsp), %rbp
        movq    -16(%rsp), %rbx
        movq    -24(%rsp), %r12
        movq    -32(%rsp), %r13
#endif
        ret
