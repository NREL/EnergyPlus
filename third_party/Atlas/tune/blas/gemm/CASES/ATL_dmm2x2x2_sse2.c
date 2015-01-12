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
   #define FSIZE 16*4+BETASZ
   #define KOFF    FSIZE-4
   #define ldcOFF  KOFF-4
   #define iAmOFF  ldcOFF-4
   #define PFAOFF  iAmOFF-4
   #define PFBOFF  PFAOFF-4
   #define IOFF    PFBOFF-4
   #define MOFF    IOFF-4
   #define iAnOFF  MOFF-4
   #define iBnOFF  iAnOFF-4
   #define iCnOFF  iBnOFF-4
   #define JOFF    iCnOFF-4
#endif
/*
 *Integer register usage shown by these defines
 */
#ifdef ATL_GAS_x8632
   #define pA0     %ecx
   #define pA1     %eax
   #define pB0     %ebx
   #define pB1     %edi
   #define pC0     %esi
   #define itmp    %edx
   #define KK      %ebp

   #define incAm   iAmOFF(%esp)
   #define incAn   iAnOFF(%esp)
   #define incBn   iBnOFF(%esp)
   #define incCn   iCnOFF(%esp)
   #define MM      IOFF(%esp)
   #define NN      JOFF(%esp)
   #define MM0     MOFF(%esp)
   #define PFA     PFAOFF(%esp)
   #define PFB     PFBOFF(%esp)
   #define ldc     ldcOFF(%esp)
   #define KK0     KOFF(%esp)
#else
   #define pA0     %rcx
   #define pA1     %rbx
   #define pB0     %rbp
   #define pB1     %rax
   #define pC0     %rdi
   #define KK      %rdx
   #define PFB     %rsi
   #define PFA     %r8
   #define ldc     %r9
   #define KK0     %r10
   #define incAm   %r11
   #define incAn   %r12
   #define incBn   %r13
   #define MM      %r14
   #define NN      %r15
   #define incCn   iCOFF(%rsp)
   #define MM0     iIOFF(%rsp)
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
   #define rC0  %xmm9
   #define rC1 %xmm10
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
        movq    %r14, -40(%rsp)
        movq    %r15, -48(%rsp)
   #define iCOFF -56
   #define iIOFF -64
#endif

/*
 *      Setup input parameters
 *      For x8632 %eax has old stack ptr; eax is pA1, so set this up late
 */
#ifdef ATL_GAS_x8632
        movl    4(%eax), itmp
        movl    itmp, MM0
        movl    8(%eax), itmp
        movl    itmp, NN
        movl    12(%eax), KK            /* load K */
        movl    24(%eax), pA0
        movl    32(%eax), pB0
        movl    36(%eax), itmp          /* itmp = ldb */
        lea     (pB0, itmp, 8), pB1      /* pB1 = pB0 + ldb*sizeof */
        shl     $4, itmp                /* itmp = 2*sizeof*ldb */
        movl    itmp, incBn             /* incBn = 2*sizeof*ldb */
        add     pB0, itmp
        movl    itmp, PFB
#ifdef BETAX
        movsd   40(%eax), rB0           /* load beta */
        unpcklpd rB0, rB0               /* rB0 = {beta, beta} */
        MOVAPD  rB0, BETAOFF(%esp)      /* store BETA to BETAOFF */
#endif
        movl    48(%eax), pC0
        movl    52(%eax), itmp          /* itmp = ldc */
        shl     $CSH, itmp              /* itmp = ldc*sizeof */
        movl    itmp, ldcOFF(%esp)      /* ldc = ldc*sizeof */
        shr     $CSH-1, itmp            /* itmp = 2*ldc */
        sub     MM0, itmp               /* itmp = 2*ldc - M */
        shl     $CSH, itmp              /* itmp = (2*ldc-M)*sizeof */
        movl    itmp, incCn             /* incCn = (2*ldc-M)*sizeof */
        movl    28(%eax), itmp          /* itmp = lda */
        lea     (pA0, itmp,8), pA1      /* just overwrote old SP in EAX */
        shl     $4, itmp                /* itmp = 2*sizeof*lda */
        movl    itmp, incAm             /* incAm = 2*sizeof*lda */

/*
 *      pfA = A + 2*lda*M; incAn = lda*M
 */
        movl    MM0, itmp       /* itmp = M */
        imull   incAm, itmp     /* itmp = 2*lda*M */
        lea     PFAINC(pA0, itmp), itmp /* pfA = pA0 + 2*lda*M - PFAINC */
        movl    itmp, PFA       /* pfA = 2*lda*M + pA0 - PFAINC */
        sub     pA0, itmp       /* itmp = 2*lda*M - PFAINC*/
        sub     $PFAINC, itmp   /* itmp = 2*lda*M */
        shr     $1, itmp        /* itmp = lda*M */
        movl    itmp, incAn     /* incAn = lda*M */
#else
/*
 *      Get parameters moves to correct registers
 */
        movq    %rdi, MM
        movq    %rsi, NN
        movq    %r8, pA1                /* pA1 = lda */
        movq    %r9, pB0                /* pB0 = B */
        movslq  8(%rsp), pB1            /* pB1 = ldb */
        unpcklpd        %xmm1, %xmm1    /* xmm1 = {beta, beta} */
        MOVAPD  %xmm1, BETA
        movq    16(%rsp), pC0           /* pC0 = C */
        movslq  24(%rsp), ldc           /* ldc = ldc */
/*
 *      ===================================================
 *      Compute rest of needed variables using these inputs
 *      ===================================================
 */
        shl     $3, pB1                 /* pB1 = ldb*sizeof */
        lea     (pB1, pB1), incBn       /* incBn = 2*ldb*sizeof */
        add     pB0, pB1                /* pB1 = pB0 + ldb*sizeof */
        lea     (ldc,ldc), PFA          /* PFA = 2*ldc */
        sub     MM, PFA                 /* PFA = 2*ldc - M */
        shl     $CSH, PFA               /* PFA = (2*ldc-M)*sizeof */
        movq    PFA, incCn              /* incCn = (2*ldc-M)*sizeof */
        shl     $CSH, ldc                 /* ldc *= sizeof */
        shl     $3, pA1                 /* pA1 = lda * sizeof */
        lea     (pA1, pA1), incAm       /* incAm = 2*lda*sizeof */
        mov     MM, PFA                 /* PFA = M */
        imulq   pA1, PFA                /* PFA = M * lda*sizeof */
        movq    PFA, incAn              /* incAn = M*lda*sizeof */
        lea     PFAINC(pA0,PFA,2),PFA   /* PFA = pA0+2*M*lda*sizeof - PFAINC */
        add     pA0, pA1                /* pA1 = pA0 + lda*sizeof */
        mov     pB0, PFB                /* PBF = pB0 */
        add     incBn, PFB              /* PFB = pB0 + 2*ldb*sizeof */
        movq    MM, MM0                 /* MM0 = MM */
#endif
        sub     $2, KK          /* must stop K it early to drain advance load */
        jz      K_IS_2
/*
 *      Have pA/B point to end of column, so we can run loop backwards
 */
        lea     (pA0, KK, 8), pA0       /* pA0 += K */
        lea     (pB0, KK, 8), pB0       /* pB0 += K */
        lea     (pA1, KK, 8), pA1       /* pA1 += K */
        lea     (pB1, KK, 8), pB1       /* pB1 += K */
        neg     KK                      /* KK = -K */
        add     $2, KK
        jz      K_IS_4
        movq    KK, KK0
#ifdef ATL_GAS_x8632
        movl    MM0, itmp
        movl    itmp, MM
#else
        movq    MM, MM0
#endif
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
        MOVAB   -16(pB0,KK,8), rA0
        MOVAB   -16(pA0,KK,8), rC00
        MOVAPD  rC00, rC01
        mulpd   rA0, rC00
        #ifdef ATL_GAS_x8632
           movq    PFB, itmp
        #else
           #ifndef BETA0
              #ifdef DCPLX
                 movsd (pC0), rC0
                 movhpd 16(pC0), rC0
              #else
                 MOVAPD  (pC0), rC0
              #endif
           #else
               prefB((PFB))
           #endif
        #endif
        MOVAB   -16(pA1,KK,8), rC10
        MOVAPD  rC10, rC11
        mulpd   rA0, rC10
        #ifdef ATL_GAS_x8632
           prefB((itmp))
        #else
           #ifndef BETA0
              #ifdef DCPLX
                 movsd (pC0,ldc), rC1
                 movhpd 16(pC0,ldc), rC1
              #else
                 MOVAPD  (pC0,ldc), rC1
              #endif
           #else
               add      $PFBINC, PFB
           #endif
        #endif
        MOVAB   -16(pB1,KK,8), rA0
        mulpd   rA0, rC01
           MOVAB   (pB0,KK,8), rB0
        mulpd   rA0, rC11
        #if !defined(ATL_GAS_x8632) && !defined(BETA0)
               prefB((PFB))
        #endif
ALIGN16
KLOOP:
        MOVAB   (pA0,KK,8), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   (pA1,KK,8), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   (pB1,KK,8), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   16(pB0,KK,8), rB0
           add  $2, KK
        addpd   ra0, rC01
        addpd   rA1, rC11
        jnz     KLOOP
/*
 *      Peel last iteration to stop forward fetch of B
 */
        MOVAB   (pA0), rA0
        #ifdef ATL_GAS_x8632
           add     $PFBINC, itmp
        #elif defined(BETAX)
           mulpd BETA, rC0
        #endif
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        #ifdef ATL_GAS_x8632
           movl    itmp, PFB
        #else
           pref2((PFA))
        #endif
        addpd   rA0, rC00
        MOVAB   (pA1), rA1
        mulpd   rA1, rB0
        #ifdef ATL_GAS_x8632
           movq    PFA, itmp
        #elif defined(BETAX)
           mulpd  BETA, rC1
        #endif
        addpd   rB0, rC10
        MOVAB   (pB1), rB0
        mulpd   rB0, ra0
        #ifdef ATL_GAS_x8632
           prefetcht1      (itmp)
        #elif !defined(BETA0)
           add  $PFBINC, PFB
        #endif
        mulpd   rB0, rA1
        #ifdef ATL_GAS_x8632
           add     $PFAINC, itmp
        #else
           add     $PFAINC, PFA
        #endif
        addpd   ra0, rC01
        #ifdef ATL_GAS_x8632
           movl    itmp, PFA
        #endif
        addpd   rA1, rC11

#ifndef ATL_SSE3
        MOVAPD          rC00, rA0       /* rA0  = c00a c00b */
        MOVAPD          rC01, rB0       /* rB0  = c01a c01b */
        unpcklpd        rC10, rC00      /* rC00 = c00a c10a */
        unpcklpd        rC11, rC01      /* rC01 = c01a c11a */
        unpckhpd        rC10, rA0       /* rA0  = c00b c10b */
        unpckhpd        rC11, rB0       /* rB0  = c01b c11b */
        addpd           rA0, rC00       /* rC00 = c00ab c10ab */
        addpd           rB0, rC01       /* rC01 = c01ab c11ab */
#endif
#ifdef ATL_GAS_x8632
        movl    ldcOFF(%esp), itmp
        #ifdef ATL_SSE3
           haddpd  rC10, rC00
        #endif
        #ifdef BETAX
           MOVAPD BETAOFF(%esp), rB0
           MOVAPD (pC0), rA0
           mulpd  rB0, rA0
           addpd  rA0, rC00
        #endif
        #ifdef BETA1
           addpd  (pC0), rC00
        #endif
        #ifdef ATL_SSE3
           haddpd  rC11, rC01
        #endif
        #ifdef BETAX
           MOVAPD (pC0,itmp), rA1
           mulpd  rB0, rA1
           addpd  rA1, rC01
        #endif
        #ifdef BETA1
           addpd  (pC0,itmp), rC01
        #endif
#else
        #ifdef ATL_SSE3
           haddpd  rC10, rC00
        #endif
        #ifndef BETA0
           addpd  rC0, rC00
        #endif
        #ifdef ATL_SSE3
           haddpd  rC11, rC01
        #endif
        #ifndef BETA0
           addpd  rC1, rC01
        #endif
#endif
        add     $2*CMUL(8), pC0
        addq    incAm, pA0    /* pA0 += lda*sizeof*2 */
        addq    incAm, pA1    /* pA1 += lda*sizeof*2 */
        subq    $2, MM
        movq    KK0, KK
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

#ifdef ATL_GAS_x8632
        movl    MM0, itmp
        movl    itmp, MM
#else
        movq    MM0, MM
#endif
        movq    KK0, KK
        subq    incAn, pA0
        subq    incAn, pA1
        addq    incCn, pC0
        addq    incBn, pB0
        addq    incBn, pB1
        subq    $2, NN
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
        movq    -40(%rsp), %r14
        movq    -48(%rsp), %r15
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
        MOVAB   -16(pB0,KK,8), rA0
        MOVAB   -16(pA0,KK,8), rC00
        MOVAPD  rC00, rC01
        mulpd   rA0, rC00
        #ifdef ATL_GAS_x8632
           movq    PFB, itmp
        #else
           #ifndef BETA0
              #ifdef DCPLX
                 movsd (pC0), rC0
                 movhpd 16(pC0), rC0
              #else
                 MOVUPD  (pC0), rC0
              #endif
           #else
               prefB((PFB))
           #endif
        #endif
        MOVAB   -16(pA1,KK,8), rC10
        MOVAPD  rC10, rC11
        mulpd   rA0, rC10
        #ifdef ATL_GAS_x8632
           prefB((itmp))
        #else
           #ifndef BETA0
              #ifdef DCPLX
                 movsd (pC0,ldc), rC1
                 movhpd 16(pC0,ldc), rC1
              #else
                 MOVUPD  (pC0,ldc), rC1
              #endif
           #else
               add      $PFBINC, PFB
           #endif
        #endif
        MOVAB   -16(pB1,KK,8), rA0
        mulpd   rA0, rC01
           MOVAB   (pB0,KK,8), rB0
        mulpd   rA0, rC11
        #if !defined(ATL_GAS_x8632) && !defined(BETA0)
               prefB((PFB))
        #endif
ALIGN16
UKLOOP:
        MOVAB   (pA0,KK,8), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   (pA1,KK,8), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   (pB1,KK,8), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
           MOVAB   16(pB0,KK,8), rB0
           add  $2, KK
        addpd   ra0, rC01
        addpd   rA1, rC11
        jnz     UKLOOP
/*
 *      Peel last iteration to stop forward fetch of B
 */
        MOVAB   (pA0), rA0
        #ifdef ATL_GAS_x8632
           add     $PFBINC, itmp
        #elif defined(BETAX)
           mulpd BETA, rC0
        #endif
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        #ifdef ATL_GAS_x8632
           movl    itmp, PFB
        #else
           pref2((PFA))
        #endif
        addpd   rA0, rC00
        MOVAB   (pA1), rA1
        mulpd   rA1, rB0
        #ifdef ATL_GAS_x8632
           movq    PFA, itmp
        #elif defined(BETAX)
           mulpd  BETA, rC1
        #endif
        addpd   rB0, rC10
        MOVAB   (pB1), rB0
        mulpd   rB0, ra0
        #ifdef ATL_GAS_x8632
           prefetcht1      (itmp)
        #elif !defined(BETA0)
           add  $PFBINC, PFB
        #endif
        mulpd   rB0, rA1
        #ifdef ATL_GAS_x8632
           add     $PFAINC, itmp
        #else
           add     $PFAINC, PFA
        #endif
        addpd   ra0, rC01
        #ifdef ATL_GAS_x8632
           movl    itmp, PFA
        #endif
        addpd   rA1, rC11

#ifndef ATL_SSE3
        MOVAPD          rC00, rA0       /* rA0  = c00a c00b */
        MOVAPD          rC01, rB0       /* rB0  = c01a c01b */
        unpcklpd        rC10, rC00      /* rC00 = c00a c10a */
        unpcklpd        rC11, rC01      /* rC01 = c01a c11a */
        unpckhpd        rC10, rA0       /* rA0  = c00b c10b */
        unpckhpd        rC11, rB0       /* rB0  = c01b c11b */
        addpd           rA0, rC00       /* rC00 = c00ab c10ab */
        addpd           rB0, rC01       /* rC01 = c01ab c11ab */
#endif
#ifdef ATL_GAS_x8632
        movl    ldcOFF(%esp), itmp
   #ifdef DCPLX
        #ifdef ATL_SSE3
           haddpd  rC10, rC00
        #endif
        #ifndef BETA0
           #ifdef BETAX
              movapd BETAOFF(%esp), rB0
           #endif
           movsd (pC0), rA0
           movhpd 16(pC0), rA0
           #ifdef BETAX
              mulpd  rB0, rA0
           #endif
           addpd  rA0, rC00
        #endif
        #ifdef ATL_SSE3
           haddpd  rC11, rC01
        #endif
        #ifndef BETA0
           movsd (pC0,itmp), rA1
           movhpd 16(pC0,itmp), rA1
           #ifdef BETAX
              mulpd  rB0, rA1
           #endif
           addpd  rA1, rC01
        #endif
   #else
        #ifdef ATL_SSE3
           haddpd  rC10, rC00
        #endif
        #ifdef BETAX
           MOVAPD BETAOFF(%esp), rB0
           MOVUPD (pC0), rA0
           mulpd  rB0, rA0
           addpd  rA0, rC00
        #endif
        #ifdef BETA1
           MOVUPD (pC0), rA0
           addpd   rA0, rC00
        #endif
        #ifdef ATL_SSE3
           haddpd  rC11, rC01
        #endif
        #ifdef BETAX
           MOVUPD (pC0,itmp), rA1
           mulpd  rB0, rA1
           addpd  rA1, rC01
        #endif
        #ifdef BETA1
           MOVUPD (pC0, itmp), rA1
           addpd  rA1, rC01
        #endif
   #endif
#else
        #ifdef ATL_SSE3
           haddpd  rC10, rC00
        #endif
        #ifndef BETA0
           addpd  rC0, rC00
        #endif
        #ifdef ATL_SSE3
           haddpd  rC11, rC01
        #endif
        #ifndef BETA0
           addpd  rC1, rC01
        #endif
#endif
        add     $2*CMUL(8), pC0
        addq    incAm, pA0    /* pA0 += lda*sizeof*2 */
        addq    incAm, pA1    /* pA1 += lda*sizeof*2 */
        subq    $2, MM
        movq    KK0, KK
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

#ifdef ATL_GAS_x8632
        movl    MM0, itmp
        movl    itmp, MM
#else
        movq    MM0, MM
#endif
        movq    KK0, KK
        subq    incAn, pA0
        subq    incAn, pA1
        addq    incCn, pC0
        addq    incBn, pB0
        addq    incBn, pB1
        subq    $2, NN
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
        movq    -40(%rsp), %r14
        movq    -48(%rsp), %r15
#endif
        ret
/*
 * Code specialized for K == 2; pA0 & pAB pt to start of arrays
 * Assume C unaligned so we don't have to write this cleanup case twice.
 * This assumption costs you major perf. if you care about this case
 * (since load/store of C dominant cost when K=2).  This code is more for
 * correctness than perf. as presently written.
 */
K_IS_2:
#ifdef ATL_GAS_x8632
        movq    ldc, itmp
#endif
        movq    MM0, KK          /* KK is now M-loop counter */
ALIGN16
MNLOOP_K2:
   #ifdef BETA0
        MOVAB   (pB0), rA0
        MOVAB   (pA0), rC00
        MOVAPD  rC00, rC01
        mulpd   rA0, rC00
        MOVAB   (pA1), rC10
        MOVAPD  rC10, rC11
        mulpd   rA0, rC10
        MOVAB   (pB1), rA0
        mulpd   rA0, rC01
        mulpd   rA0, rC11
   #else
        movsd   (pC0), rC00
        movsd   CMUL(8)(pC0), rC10
        movsd   (pC0,itmp), rC01
        movsd   CMUL(8)(pC0,itmp), rC11
        #ifdef BETAX
           #ifdef ATL_GAS_x8632
              MOVAPD BETAOFF(%esp), rA1
              mulpd  rA1, rC00
              mulpd  rA1, rC10
              mulpd  rA1, rC01
              mulpd  rA1, rC11
           #else
              mulpd  BETA, rC00
              mulpd  BETA, rC10
              mulpd  BETA, rC01
              mulpd  BETA, rC11
           #endif
        #endif
        MOVAB   (pA0), rA0
        MOVAB   (pB0), rB0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   (pA1), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   (pB1), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
        addpd   ra0, rC01
        addpd   rA1, rC11
   #endif
           add     $2*CMUL(8), pC0
#ifndef ATL_SSE3
        MOVAPD          rC00, rA0       /* rA0  = c00a c00b */
        MOVAPD          rC01, rB0       /* rB0  = c01a c01b */
        unpcklpd        rC10, rC00      /* rC00 = c00a c10a */
        unpcklpd        rC11, rC01      /* rC01 = c01a c11a */
        unpckhpd        rC10, rA0       /* rA0  = c00b c10b */
        unpckhpd        rC11, rB0       /* rB0  = c01b c11b */
        addpd           rA0, rC00       /* rC00 = c00ab c10ab */
        addpd           rB0, rC01       /* rC01 = c01ab c11ab */
#endif
        #ifdef ATL_SSE3
           haddpd  rC10, rC00
        #endif
           addq  incAm, pA0
        #ifdef ATL_SSE3
           haddpd  rC11, rC01
        #endif
           addq  incAm, pA1
           sub   $2, KK
   #ifdef DCPLX
        movlpd  rC00, -32(pC0)
        movhpd  rC00, -16(pC0)
        movlpd  rC01, -32(pC0,itmp)
        movhpd  rC01, -16(pC0,itmp)
   #else
        MOVUPD  rC00, -2*8(pC0)
        MOVUPD  rC01, -2*8(pC0,itmp)
   #endif
        jnz MNLOOP_K2                   /* end of M-loop */

        subq    incAn, pA0
        subq    incAn, pA1
        addq    incCn, pC0
        addq    incBn, pB0
        addq    incBn, pB1
        subq    $2, NN
        mov     MM0, KK
        jnz     MNLOOP_K2
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
        movq    -40(%rsp), %r14
        movq    -48(%rsp), %r15
#endif
        ret
/*
 * Code specialized for K == 4; pA0 & pAB pt to start of arrays + 16
 * Assume C unaligned so we don't have to write this cleanup case twice.
 * This assumption costs you major perf. if you care about this case
 * (since load/store of C dominant cost when K=4).  This code is more for
 * correctness than perf. as presently written.
 */
K_IS_4:
#ifdef ATL_GAS_x8632
        movq    ldc, itmp
#endif
        movq    MM0, KK          /* KK is now M-loop counter */
ALIGN16
MNLOOP_K4:
   #ifdef BETA0
        MOVAB   -16(pB0), rA0
        MOVAB   -16(pA0), rC00
        MOVAPD  rC00, rC01
        mulpd   rA0, rC00
        MOVAB   -16(pA1), rC10
        MOVAPD  rC10, rC11
        mulpd   rA0, rC10
        MOVAB   -16(pB1), rA0
        mulpd   rA0, rC01
        MOVAB   (pB0), rB0
        mulpd   rA0, rC11
   #else
        movsd   (pC0), rC00
        movsd   CMUL(8)(pC0), rC10
        movsd   (pC0,itmp), rC01
        movsd   CMUL(8)(pC0,itmp), rC11
        #ifdef BETAX
           #ifdef ATL_GAS_x8632
              MOVAPD BETAOFF(%esp), rA1
              mulpd  rA1, rC00
              mulpd  rA1, rC10
              mulpd  rA1, rC01
              mulpd  rA1, rC11
           #else
              mulpd  BETA, rC00
              mulpd  BETA, rC10
              mulpd  BETA, rC01
              mulpd  BETA, rC11
           #endif
        #endif
        MOVAB   -16(pA0), rA0
        MOVAB   -16(pB0), rB0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   -16(pA1), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   -16(pB1), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
        MOVAB   (pB0), rB0
        addpd   ra0, rC01
        addpd   rA1, rC11
   #endif
        MOVAB   (pA0), rA0
        MOVAPD  rA0, ra0
        mulpd   rB0, rA0
        addpd   rA0, rC00
        MOVAB   (pA1), rA1
        mulpd   rA1, rB0
        addpd   rB0, rC10
        MOVAB   (pB1), rB0
        mulpd   rB0, ra0
        mulpd   rB0, rA1
        addpd   ra0, rC01
        addpd   rA1, rC11
           add     $2*CMUL(8), pC0
#ifndef ATL_SSE3
        MOVAPD          rC00, rA0       /* rA0  = c00a c00b */
        MOVAPD          rC01, rB0       /* rB0  = c01a c01b */
        unpcklpd        rC10, rC00      /* rC00 = c00a c10a */
        unpcklpd        rC11, rC01      /* rC01 = c01a c11a */
        unpckhpd        rC10, rA0       /* rA0  = c00b c10b */
        unpckhpd        rC11, rB0       /* rB0  = c01b c11b */
        addpd           rA0, rC00       /* rC00 = c00ab c10ab */
        addpd           rB0, rC01       /* rC01 = c01ab c11ab */
#endif
        #ifdef ATL_SSE3
           haddpd  rC10, rC00
        #endif
           addq  incAm, pA0
        #ifdef ATL_SSE3
           haddpd  rC11, rC01
        #endif
           addq  incAm, pA1
           sub   $2, KK
   #ifdef DCPLX
        movlpd  rC00, -32(pC0)
        movhpd  rC00, -16(pC0)
        movlpd  rC01, -32(pC0,itmp)
        movhpd  rC01, -16(pC0,itmp)
   #else
        MOVUPD  rC00, -2*CMUL(8)(pC0)
        MOVUPD  rC01, -2*CMUL(8)(pC0,itmp)
   #endif
        jnz MNLOOP_K4                   /* end of M-loop */

        subq    incAn, pA0
        subq    incAn, pA1
        addq    incCn, pC0
        addq    incBn, pB0
        addq    incBn, pB1
        subq    $2, NN
        movq    MM0, KK
        jnz     MNLOOP_K4
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
        movq    -40(%rsp), %r14
        movq    -48(%rsp), %r15
#endif
        ret
