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
#if KB > 256
   #error "KB can at most be 256!"
#endif
#if (KB/4)*4 != KB
   #error "KB must be a multiple of 4
#endif

#ifdef SCPLX
   #define CMUL(arg_) 2*arg_
   #define CSH 3
#else
   #define CMUL(arg_) arg_
   #define CSH 2
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
 void ATL_AUSERMM(const int M, const int N, const int K, const TYPE alpha,
                       %rcx/20         %r8/24         %r9/28           8/32
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                        %xmm1/36    16/40          24/44
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
        movl    20(%eax), pA0
        movl    28(%eax), pB0
        movl    32(%eax), ldb           /* ldb = ldb */
        shl     $2, ldb                 /* ldb = ldb*sizeof */
        lea     (pB0, ldb, 2), itmp     /* itmp = pB0 + 2*ldb*sizeof */
        movl    itmp, pfB               /* pfB = pB0 + 2*ldb*sizeof */
#ifdef BETAX
        movss   36(%eax), rB0           /* load beta */
        shufps  $0, rB0, rB0            /* rB0 = beta,beta,beta,beta */
        movaps  rB0, BETAOFF(%esp)      /* store BETA to stack */
#endif
        movl    40(%eax), pC0
        movl    44(%eax), ldc           /* ldc = ldc */
        lea     (ldc,ldc), itmp         /* itmp = 2*ldc */
        shl     $CSH, ldc               /* ldc = ldc*sizeof */
        sub     MM0, itmp               /* itmp = 2*ldc - M */
        shl     $CSH, itmp              /* itmp = (2*ldc-M)*sizeof */
        movl    itmp, incCn             /* incCn = (2*ldc-M)*sizeof */
        movl    24(%eax), lda           /* lda = lda; overwrote old SP in EAX */
        shl     $2, lda                 /* lda = lda*sizeof */
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
        shufps  $0, %xmm1, %xmm1            /* rB0 = beta,beta,beta,beta */
        MOVAPD  %xmm1, BETA             /* BETA = beta, beta */
        movq    16(%rsp), pC0           /* pC0 = C */
        movslq  24(%rsp), ldc           /* ldc = ldc */
/*
 *      ===================================================
 *      Compute rest of needed variables using these inputs
 *      ===================================================
 */
        shl     $2, ldb                 /* ldb = ldb*sizeof */
        lea     (ldc,ldc), incCn        /* incCn = 2*ldc */
        sub     MM, incCn               /* incCn = 2*ldc - M */
        shl     $CSH, incCn             /* incCn = (2*ldc-M)*sizeof */
        shl     $CSH, ldc               /* ldc *= sizeof */
        shl     $2, lda                 /* lda = lda * sizeof */
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
MNLOOP:
/*
 * Peel first iteration of K to avoid need to zero rCxx
 */
   movaps       -128(pB0), rA0
   movaps       -128(pA0), rC00
   movaps       rC00, rC01
   mulps        rA0, rC00
   #ifdef ATL_GAS_x8632
      movl      pfB, itmp
   #else
      prefB((pfB))
   #endif
   movaps       -128(pA0,lda), rC10
   movaps       rC10, rC11
   mulps        rA0, rC10
   #ifdef ATL_GAS_x8632
      prefB((itmp))
   #else
      add       $PFBINC, pfB
   #endif
   movaps       -128(pB0,ldb), rA0
   mulps        rA0, rC01
   #if KB > 4
      movaps    -112(pB0), rB0
   #endif
   mulps        rA0, rC11
   #ifdef ATL_GAS_x8632
      addl      $PFBINC, pfB
   #endif
   #if KB == 4
      #ifdef SCPLX
         movss  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
         movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
         shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         movss  8(pC0,ldc), rA1         /* rA1 = c11 XXX XXX XXX */
         movhps (pC0,ldc), rA1          /* rA1 = c11 XXX c01 XXX */
         shufps $0x24, rA1, rA0         /* rA0 = c00 c10 c01 c11 */
      #else
         movlps (pC0), rA0
         movhps (pC0,ldc), rA0
      #endif
   #endif
   #if KB > 8
      movaps    -112(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    -112(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    -112(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps -112+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 8
      movaps    -112(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    -112(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    -112(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 12
      movaps    -96(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    -96(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    -96(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps -96+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 12
      movaps    -96(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    -96(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    -96(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 16
      movaps    -80(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    -80(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    -80(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps -80+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 16
      movaps    -80(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    -80(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    -80(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 20
      movaps    -64(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    -64(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    -64(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps -64+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 20
      movaps    -64(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    -64(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    -64(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 24
      movaps    -48(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    -48(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    -48(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps -48+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 24
      movaps    -48(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    -48(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    -48(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 28
      movaps    -32(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    -32(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    -32(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps -32+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 28
      movaps    -32(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    -32(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    -32(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 32
      movaps    -16(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    -16(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    -16(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps -16+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 32
      movaps    -16(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    -16(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    -16(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 36
      movaps    0(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    0(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    0(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 0+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 36
      movaps    0(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    0(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    0(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 40
      movaps    16(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    16(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    16(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 16+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 40
      movaps    16(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    16(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    16(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 44
      movaps    32(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    32(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    32(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 32+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 44
      movaps    32(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    32(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    32(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 48
      movaps    48(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    48(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    48(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 48+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 48
      movaps    48(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    48(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    48(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 52
      movaps    64(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    64(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    64(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 64+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 52
      movaps    64(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    64(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    64(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 56
      movaps    80(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    80(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    80(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 80+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 56
      movaps    80(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    80(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    80(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 60
      movaps    96(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    96(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    96(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 96+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 60
      movaps    96(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    96(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    96(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 64
      movaps    112(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    112(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    112(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 112+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 64
      movaps    112(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    112(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    112(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 68
      movaps    128(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    128(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    128(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 128+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 68
      movaps    128(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    128(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    128(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 72
      movaps    144(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    144(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    144(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 144+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 72
      movaps    144(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    144(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    144(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 76
      movaps    160(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    160(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    160(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 160+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 76
      movaps    160(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    160(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    160(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 80
      movaps    176(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    176(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    176(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 176+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 80
      movaps    176(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    176(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    176(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 84
      movaps    192(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    192(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    192(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 192+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 84
      movaps    192(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    192(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    192(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 88
      movaps    208(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    208(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    208(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 208+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 88
      movaps    208(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    208(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    208(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 92
      movaps    224(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    224(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    224(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 224+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 92
      movaps    224(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    224(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    224(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 96
      movaps    240(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    240(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    240(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 240+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 96
      movaps    240(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    240(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    240(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 100
      movaps    256(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    256(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    256(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 256+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 100
      movaps    256(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    256(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    256(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 104
      movaps    272(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    272(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    272(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 272+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 104
      movaps    272(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    272(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    272(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 108
      movaps    288(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    288(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    288(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 288+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 108
      movaps    288(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    288(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    288(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 112
      movaps    304(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    304(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    304(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 304+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 112
      movaps    304(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    304(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    304(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 116
      movaps    320(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    320(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    320(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 320+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 116
      movaps    320(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    320(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    320(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 120
      movaps    336(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    336(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    336(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 336+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 120
      movaps    336(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    336(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    336(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 124
      movaps    352(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    352(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    352(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 352+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 124
      movaps    352(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    352(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    352(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 128
      movaps    368(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    368(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    368(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 368+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 128
      movaps    368(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    368(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    368(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 132
      movaps    384(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    384(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    384(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 384+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 132
      movaps    384(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    384(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    384(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 136
      movaps    400(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    400(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    400(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 400+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 136
      movaps    400(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    400(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    400(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 140
      movaps    416(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    416(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    416(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 416+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 140
      movaps    416(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    416(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    416(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 144
      movaps    432(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    432(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    432(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 432+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 144
      movaps    432(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    432(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    432(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 148
      movaps    448(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    448(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    448(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 448+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 148
      movaps    448(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    448(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    448(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 152
      movaps    464(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    464(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    464(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 464+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 152
      movaps    464(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    464(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    464(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 156
      movaps    480(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    480(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    480(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 480+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 156
      movaps    480(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    480(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    480(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 160
      movaps    496(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    496(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    496(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 496+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 160
      movaps    496(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    496(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    496(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 164
      movaps    512(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    512(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    512(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 512+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 164
      movaps    512(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    512(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    512(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 168
      movaps    528(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    528(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    528(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 528+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 168
      movaps    528(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    528(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    528(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 172
      movaps    544(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    544(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    544(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 544+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 172
      movaps    544(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    544(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    544(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 176
      movaps    560(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    560(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    560(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 560+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 176
      movaps    560(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    560(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    560(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 180
      movaps    576(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    576(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    576(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 576+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 180
      movaps    576(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    576(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    576(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 184
      movaps    592(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    592(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    592(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 592+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 184
      movaps    592(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    592(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    592(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 188
      movaps    608(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    608(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    608(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 608+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 188
      movaps    608(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    608(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    608(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 192
      movaps    624(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    624(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    624(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 624+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 192
      movaps    624(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    624(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    624(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 196
      movaps    640(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    640(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    640(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 640+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 196
      movaps    640(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    640(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    640(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 200
      movaps    656(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    656(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    656(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 656+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 200
      movaps    656(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    656(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    656(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 204
      movaps    672(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    672(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    672(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 672+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 204
      movaps    672(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    672(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    672(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 208
      movaps    688(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    688(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    688(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 688+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 208
      movaps    688(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    688(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    688(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 212
      movaps    704(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    704(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    704(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 704+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 212
      movaps    704(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    704(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    704(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 216
      movaps    720(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    720(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    720(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 720+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 216
      movaps    720(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    720(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    720(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 220
      movaps    736(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    736(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    736(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 736+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 220
      movaps    736(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    736(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    736(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 224
      movaps    752(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    752(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    752(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 752+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 224
      movaps    752(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    752(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    752(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 228
      movaps    768(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    768(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    768(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 768+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 228
      movaps    768(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    768(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    768(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 232
      movaps    784(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    784(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    784(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 784+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 232
      movaps    784(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    784(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    784(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 236
      movaps    800(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    800(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    800(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 800+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 236
      movaps    800(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    800(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    800(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 240
      movaps    816(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    816(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    816(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 816+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 240
      movaps    816(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    816(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    816(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 244
      movaps    832(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    832(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    832(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 832+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 244
      movaps    832(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    832(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    832(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 248
      movaps    848(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    848(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    848(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 848+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 248
      movaps    848(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    848(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    848(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
   #if KB > 252
      movaps    864(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    864(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    864(pB0,ldb), rB0
      mulps     rB0, ra0
      mulps     rB0, rA1
         movaps 864+16(pB0), rB0
      addps      ra0, rC01
      addps      rA1, rC11
   #elif KB == 252
      movaps    864(pA0), rA0
      movaps    rA0, ra0
      mulps     rB0, rA0
      addps     rA0, rC00
      movaps    864(pA0,lda), rA1
      mulps     rA1, rB0
      addps     rB0, rC10
      movaps    864(pB0,ldb), rB0
      mulps     rB0, ra0
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0), rA0             /* rA0 = c10 XXX XXX XXX */
            movhps (pC0), rA0              /* rA0 = c10 XXX c00 XXX */
            shufps $0xE2, rA0, rA0         /* rA0 = c00 c10 XXX XXX */
         #else
            movlps (pC0), rA0
         #endif
      #endif
      mulps     rB0, rA1
      addps      ra0, rC01
      #ifndef BETA0
         #ifdef SCPLX
            movsd  8(pC0,ldc), rB0         /* rB0 = c11 XXX XXX XXX */
            movhps (pC0,ldc), rB0          /* rB0 = c11 XXX c01 XXX */
            shufps $0x24, rB0, rA0         /* rA0 = c00 c11 c01 c11 */
         #else
            movhps (pC0,ldc), rA0
         #endif
      #endif
      addps      rA1, rC11
   #endif
#ifndef ATL_SSE3
   #error "This kernel presently requires SSE3!"
                                /* rC00 = c00a    c00b    c00c    c00d    */
                                /* rC10 = c10a    c10b    c10c    c10d    */
                                /* rC01 = c01a    c01b    c01c    c01d    */
                                /* rC11 = c11a    c11b    c11c    c11d    */
   movaps       rC00, rB0       /* rB0  = c00a    c00b    c00c    c00d    */
   movlhps      rC10, rC00      /* rC00 = c00a    c00b    c10a    c10b    */
   shufps $0xBE, rC10, rB0      /* rB0  = c00c    c00d    c10c    c10d    */
   addps        rB0, rC00       /* rC00 = c00ac   c00bd   c10ac   c10bd   */

   movaps       rC01, rA1       /* rA1  = c01a    c01b    c01c    c01d    */
   movlhps      rC11, rC01      /* rC01 = c01a    c01b    c11a    c11b    */
   shufps $0xBE, rC11, rA1      /* rA1  = c01c    c01d    c11c    c11d    */
   addps        rA1, rC01       /* rC01 = c01ac   c01bd   c11ac   c11bd   */

   movaps       rC00, rB0       /* rB0  = c00ac   c00bd   c10ac   c10bd   */
   unpcklps     rC01, rC00      /* rC00 = c00ac   c01ac   c00bd   c01bd   */
   movaps       rC00, rC10      /* rC10 = c00ac   c01ac   c00bd   c01bd   */
   unpckhps     rB0, rC01       /* rC01 = c11ac   c10ac   c11bd   c10bd   */
   movaps       rC01, rC11      /* rC11 = c11ac   c10ac   c11bd   c10bd   */
#else
                                /* rC00 = c00a    c00b    c00c    c00d    */
                                /* rC10 = c10a    c10b    c10c    c10d    */
                                /* rC01 = c01a    c01b    c01c    c01d    */
                                /* rC11 = c11a    c11b    c11c    c11d    */
   #ifdef ATL_GAS_x8632
      movl pfA, itmp
   #endif
   haddps       rC10, rC00      /* rC00 = c00ab   c00cd   c01ab   c01cd   */
   #ifdef ATL_GAS_x8632
      pref2((itmp))
   #else
      pref2((pfA))
   #endif
   haddps       rC11, rC01      /* rC01 = c10ab   c10cd   c11ab   c11cd   */
      addq      $PFAINC, pfA
   haddps       rC01, rC00      /* rC00 = c00abcd c01abcd  c10abcd c11abcd*/
#endif
   #ifndef BETA0
      #ifdef BETAX
         mulps  BETA, rA0
      #endif
      addps     rA0, rC00
   #endif
   add  $2*CMUL(4), pC0
   lea  (pA0, lda, 2), pA0
   subq $2, MM
   #ifdef SCPLX
      movhlps rC00, rC01
      movss  rC00, -16(pC0)
      psrldq    $4, rC00
      movss  rC01, -16(pC0,ldc)
      psrldq    $4, rC01
      movss  rC00, -8(pC0)
      movss  rC01, -8(pC0,ldc)
   #else
      movlpd rC00, -8(pC0)
      movhpd rC00, -8(pC0,ldc)
   #endif
   jnz  MNLOOP

   subq incAn, pA0
   addq incCn, pC0
   lea  (pB0, ldb, 2), pB0
   subq $2, NN
   #ifdef ATL_GAS_x8632
      movl    MM0, itmp
      movl    itmp, MM
   #else
      movq   MM0, MM
   #endif
   jnz MNLOOP
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
