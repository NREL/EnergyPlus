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
#ifndef ATL_SSE3
   #error "This routine requires SSE3!"
#endif
/*
 * This computational kernel was created using a code fragment demonstrating a
 * Core2Duo-friendly 2-D x86 register block sent to me by Yevgen Voronenko
 * of the CMU/SPIRAL group as a template.  Here is original the code fragment
 * that Yevgen sent me:
 *      movapd    (%rdi), %xmm9
 *      movapd    %xmm9, %xmm6
 *      movapd    48(%rdi), %xmm8
 *      mulpd     %xmm8, %xmm6
 *      addpd     %xmm6, %xmm5
 *      movapd    16(%rdi), %xmm10
 *      movapd    %xmm10, %xmm7
 *      mulpd     %xmm8, %xmm7
 *      addpd     %xmm7, %xmm4
 *      movapd    32(%rdi), %xmm12
 *      mulpd     %xmm12, %xmm8
 *      addpd     %xmm8, %xmm3
 *      movapd    64(%rdi), %xmm11
 *      mulpd     %xmm11, %xmm9
 *      mulpd     %xmm11, %xmm10
 *      mulpd     %xmm11, %xmm12
 *      addpd     %xmm9, %xmm2
 *      addpd     %xmm10, %xmm1
 *      addpd     %xmm12, %xmm0
 */


#if !defined(ATL_GAS_x8664)
   #error "This kernel requires x86-64 assembly!"
#endif

#if !defined(KB) || (KB == 0)
   #error "KB must be a compile-time constant!"
#endif
#if KB > 128
   #error "KB can at most be 128!"
#endif

#ifdef DCPLX
   #define CMUL(arg_) 2*arg_
#else
   #define CMUL(arg_) arg_
#endif
/*
 *Integer register usage shown by these defines
 */
#define pA0     %rcx
#define lda     %rbx
#define lda3    %rbp
#define pfA     %rdi
#define pB0     %rax
#define ldb     %rsi
#define pfB     %rdx
#define incAn   %r8
#define incCn   %r9
#define pC0     %r10
#define MM      %r11
#define NN      %r12
#define MM0     %r13
#define ldc     %r14

#define rA0 	%xmm0
#define rA1 	%xmm1
#define rA2 	%xmm2
#define rA3 	%xmm3
#define rB0 	%xmm4
#define ra0 	%xmm5
#define ra1 	%xmm6
#define ra2 	%xmm7
#define rC00 	%xmm8
#define rC10 	%xmm9
#define rC20 	%xmm10
#define rC30 	%xmm11
#define rC01 	%xmm12
#define rC11 	%xmm13
#define rC21 	%xmm14
#define rC31 	%xmm15

/*
 * Save some inst space by using short version of instructions
 */
#define movapd movaps
#define movupd movups
#define movlpd movlps
#define movhpd movhps

/*
                      %rdi/4       %rsi/8       %rdx/12          %xmm0/16
 void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha,
                       %rcx/24         %r8/28         %r9/32           8/36
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                        %xmm1/40    16/48          24/52
                 const TYPE beta, TYPE *C, const int ldc)
*/
#define MOVCPD movapd
        .text
.global ATL_asmdecor(ATL_USERMM)
ALIGN16
ATL_asmdecor(ATL_USERMM):
/*
 *      Save callee-saved iregs
 */
        movq    %rbp, -8(%rsp)
        movq    %rbx, -16(%rsp)
        movq    %r12, -24(%rsp)
        movq    %r13, -32(%rsp)
        movq    %r14, -40(%rsp)
                                        prefetcht0 (pA0)
/*        movq    %r15, -48(%rsp) */

/*
 *      Setup input parameters
 */
   #ifdef BETAX
      #define BETAOFF -56
        unpcklpd        %xmm1, %xmm1
        movapd  %xmm1, BETAOFF(%rsp)
   #endif
        movq    %rdi, MM0
        movq    %rsi, NN
        movq    %r8, lda
                                        prefetcht0      (pA0,lda)
        movq    %r9, pB0
                                        prefetcht0      (pB0)
        movslq  8(%rsp), ldb
                                        prefetcht0      (pA0,lda,2)
        movq    16(%rsp), pC0
        movslq  24(%rsp), incCn
	movq	incCn, ldc
                                        prefetcht0      KB*8(pA0,lda,2)
/*
 *      incCn = (2*ldc-M)*sizeof
 */
	shl	$1, incCn
        sub     MM0, incCn
#ifdef DCPLX
        shl     $4, incCn
        shl	$4, ldc
#else
        shl     $3, incCn
        shl	$3, ldc
#endif
/*
 *      pA0 += 128; pB0 += 128
 */
        sub     $-128, pA0
        sub     $-128, pB0
                                        prefetcht0      -64(pB0)
/*
 *      lda = lda*sizeof;  lda3 = lda*3
 */
        shl     $3, lda
                                                prefetcht0      (pB0)
        lea     (lda,lda,2), lda3
/*
 *      ldb = ldb*sizeof
 */
        shl     $3, ldb
                                                prefetcht0      64(pB0)
/*
 *      pfA = A + lda*M ; incAn = lda*M
 */
        movq    lda, pfA
                                                prefetcht0      128(pB0)
        imulq   MM0, pfA
        movq    pfA, incAn
        lea     -128(pA0, pfA), pfA
        movq    MM0, MM
        lea     -128(pB0,ldb,2), pfB
#ifndef DCPLX
        test    $15, ldc
        jnz UMNLOOP
        test    $15, pC0
        jnz     UMNLOOP
#endif
ALIGN16
MNLOOP:
/* MLOOP: */
/* KLOOP begin */
#if KB == 2
        movapd -128(pA0), rC00
        movapd -128(pB0), rB0
        movapd rC00, rC01
        mulpd  rB0, rC00
        movapd -128(pA0,lda), rC10
        movapd rC10, rC11
        mulpd  rB0, rC10
        movapd -128(pA0,lda,2), rC20
        movapd rC20, rC21
        mulpd  rB0, rC20
        movapd -128(pA0,lda3), rC30
        movapd rC30, rC31
        mulpd  rB0, rC30
        movapd -128(pB0,ldb), rB0
        mulpd  rB0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
              movhpd 16(pC0), rA0
           #else
              MOVCPD (pC0), rA0
           #endif
        #endif
        mulpd  rB0, rC11
        #ifndef BETA0
           #ifdef DCPLX
              movsd  32(pC0), rA1
              movhpd 48(pC0), rA1
           #else
              MOVCPD 16(pC0), rA1
           #endif
        #endif
        mulpd  rB0, rC21
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0,ldc), rA2
              movhpd 16(pC0,ldc), rA2
           #else
              MOVCPD (pC0,ldc), rA2
           #endif
        #endif
        mulpd  rB0, rC31
        #ifndef BETA0
           #ifdef DCPLX
              movsd  32(pC0,ldc), rA3
              movhpd 48(pC0,ldc), rA3
           #else
              MOVCPD 16(pC0,ldc), rA3
           #endif
        #endif
#else
        movapd -128(pA0), rC00 ; \
        movapd -128(pB0), rB0 ; \
        movapd rC00, rC01 ; \
        mulpd  rB0, rC00 ; \
        movapd -128(pA0,lda), rC10 ; \
        movapd rC10, rC11 ; \
        mulpd  rB0, rC10 ; \
        movapd -128(pA0,lda,2), rC20 ; \
        movapd rC20, rC21 ; \
        mulpd  rB0, rC20 ; \
        movapd -128(pA0,lda3), rC30 ; \
        movapd rC30, rC31 ; \
        mulpd  rB0, rC30 ; \
        movapd -128(pB0,ldb), ra0 ; \
        mulpd  ra0, rC01 ; \
        mulpd  ra0, rC11 ; \
           movapd 16-128(pB0), rB0 ; \
        mulpd  ra0, rC21 ; \
        mulpd  ra0, rC31 ;
#endif
        prefetcht0      (pfB)
        add     $64, pfB
   #if KB > 4
      movapd -112(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -112(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -112(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -112(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -112(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+-112(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 4
      movapd -112(pA0), rA0
      movapd -112(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -112(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -112(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -112(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -112(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 6
      movapd -96(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -96(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -96(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -96(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -96(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+-96(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 6
      movapd -96(pA0), rA0
      movapd -96(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -96(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -96(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -96(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -96(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 8
      movapd -80(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -80(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -80(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -80(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -80(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+-80(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 8
      movapd -80(pA0), rA0
      movapd -80(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -80(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -80(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -80(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -80(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 10
      movapd -64(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -64(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -64(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -64(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -64(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+-64(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 10
      movapd -64(pA0), rA0
      movapd -64(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -64(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -64(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -64(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -64(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 12
      movapd -48(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -48(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -48(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -48(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -48(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+-48(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 12
      movapd -48(pA0), rA0
      movapd -48(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -48(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -48(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -48(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -48(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 14
      movapd -32(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -32(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -32(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -32(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -32(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+-32(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 14
      movapd -32(pA0), rA0
      movapd -32(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -32(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -32(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -32(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -32(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 16
      movapd -16(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -16(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -16(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -16(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -16(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+-16(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 16
      movapd -16(pA0), rA0
      movapd -16(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -16(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -16(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -16(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -16(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 18
      movapd 0(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 0(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 0(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 0(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 0(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+0(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 18
      movapd 0(pA0), rA0
      movapd 0(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 0(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 0(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 0(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 0(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 20
      movapd 16(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 16(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 16(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 16(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 16(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+16(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 20
      movapd 16(pA0), rA0
      movapd 16(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 16(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 16(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 16(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 16(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 22
      movapd 32(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 32(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 32(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 32(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 32(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+32(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 22
      movapd 32(pA0), rA0
      movapd 32(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 32(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 32(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 32(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 32(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 24
      movapd 48(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 48(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 48(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 48(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 48(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+48(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 24
      movapd 48(pA0), rA0
      movapd 48(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 48(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 48(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 48(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 48(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 26
      movapd 64(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 64(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 64(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 64(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 64(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+64(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 26
      movapd 64(pA0), rA0
      movapd 64(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 64(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 64(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 64(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 64(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 28
      movapd 80(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 80(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 80(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 80(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 80(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+80(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 28
      movapd 80(pA0), rA0
      movapd 80(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 80(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 80(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 80(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 80(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 30
      movapd 96(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 96(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 96(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 96(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 96(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+96(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 30
      movapd 96(pA0), rA0
      movapd 96(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 96(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 96(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 96(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 96(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 32
      movapd 112(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 112(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 112(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 112(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 112(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+112(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 32
      movapd 112(pA0), rA0
      movapd 112(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 112(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 112(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 112(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 112(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 34
      movapd 128(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 128(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 128(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 128(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 128(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+128(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 34
      movapd 128(pA0), rA0
      movapd 128(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 128(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 128(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 128(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 128(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 36
      movapd 144(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 144(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 144(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 144(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 144(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+144(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 36
      movapd 144(pA0), rA0
      movapd 144(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 144(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 144(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 144(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 144(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 38
      movapd 160(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 160(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 160(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 160(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 160(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+160(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 38
      movapd 160(pA0), rA0
      movapd 160(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 160(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 160(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 160(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 160(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 40
      movapd 176(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 176(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 176(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 176(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 176(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+176(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 40
      movapd 176(pA0), rA0
      movapd 176(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 176(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 176(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 176(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 176(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 42
      movapd 192(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 192(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 192(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 192(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 192(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+192(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 42
      movapd 192(pA0), rA0
      movapd 192(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 192(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 192(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 192(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 192(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 44
      movapd 208(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 208(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 208(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 208(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 208(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+208(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 44
      movapd 208(pA0), rA0
      movapd 208(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 208(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 208(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 208(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 208(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 46
      movapd 224(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 224(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 224(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 224(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 224(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+224(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 46
      movapd 224(pA0), rA0
      movapd 224(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 224(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 224(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 224(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 224(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 48
      movapd 240(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 240(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 240(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 240(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 240(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+240(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 48
      movapd 240(pA0), rA0
      movapd 240(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 240(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 240(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 240(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 240(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 50
      movapd 256(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 256(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 256(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 256(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 256(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+256(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 50
      movapd 256(pA0), rA0
      movapd 256(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 256(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 256(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 256(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 256(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 52
      movapd 272(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 272(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 272(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 272(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 272(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+272(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 52
      movapd 272(pA0), rA0
      movapd 272(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 272(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 272(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 272(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 272(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 54
      movapd 288(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 288(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 288(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 288(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 288(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+288(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 54
      movapd 288(pA0), rA0
      movapd 288(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 288(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 288(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 288(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 288(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 56
      movapd 304(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 304(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 304(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 304(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 304(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+304(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 56
      movapd 304(pA0), rA0
      movapd 304(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 304(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 304(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 304(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 304(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 58
      movapd 320(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 320(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 320(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 320(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 320(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+320(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 58
      movapd 320(pA0), rA0
      movapd 320(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 320(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 320(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 320(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 320(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 60
      movapd 336(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 336(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 336(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 336(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 336(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+336(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 60
      movapd 336(pA0), rA0
      movapd 336(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 336(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 336(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 336(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 336(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 62
      movapd 352(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 352(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 352(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 352(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 352(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+352(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 62
      movapd 352(pA0), rA0
      movapd 352(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 352(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 352(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 352(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 352(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 64
      movapd 368(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 368(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 368(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 368(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 368(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+368(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 64
      movapd 368(pA0), rA0
      movapd 368(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 368(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 368(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 368(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 368(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 66
      movapd 384(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 384(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 384(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 384(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 384(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+384(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 66
      movapd 384(pA0), rA0
      movapd 384(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 384(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 384(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 384(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 384(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 68
      movapd 400(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 400(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 400(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 400(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 400(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+400(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 68
      movapd 400(pA0), rA0
      movapd 400(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 400(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 400(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 400(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 400(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 70
      movapd 416(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 416(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 416(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 416(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 416(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+416(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 70
      movapd 416(pA0), rA0
      movapd 416(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 416(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 416(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 416(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 416(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 72
      movapd 432(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 432(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 432(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 432(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 432(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+432(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 72
      movapd 432(pA0), rA0
      movapd 432(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 432(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 432(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 432(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 432(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 74
      movapd 448(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 448(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 448(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 448(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 448(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+448(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 74
      movapd 448(pA0), rA0
      movapd 448(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 448(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 448(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 448(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 448(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 76
      movapd 464(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 464(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 464(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 464(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 464(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+464(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 76
      movapd 464(pA0), rA0
      movapd 464(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 464(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 464(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 464(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 464(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 78
      movapd 480(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 480(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 480(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 480(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 480(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+480(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 78
      movapd 480(pA0), rA0
      movapd 480(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 480(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 480(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 480(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 480(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 80
      movapd 496(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 496(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 496(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 496(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 496(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+496(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 80
      movapd 496(pA0), rA0
      movapd 496(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 496(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 496(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 496(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 496(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 82
      movapd 512(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 512(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 512(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 512(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 512(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+512(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 82
      movapd 512(pA0), rA0
      movapd 512(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 512(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 512(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 512(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 512(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 84
      movapd 528(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 528(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 528(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 528(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 528(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+528(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 84
      movapd 528(pA0), rA0
      movapd 528(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 528(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 528(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 528(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 528(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 86
      movapd 544(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 544(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 544(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 544(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 544(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+544(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 86
      movapd 544(pA0), rA0
      movapd 544(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 544(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 544(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 544(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 544(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 88
      movapd 560(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 560(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 560(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 560(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 560(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+560(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 88
      movapd 560(pA0), rA0
      movapd 560(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 560(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 560(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 560(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 560(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 90
      movapd 576(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 576(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 576(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 576(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 576(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+576(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 90
      movapd 576(pA0), rA0
      movapd 576(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 576(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 576(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 576(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 576(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 92
      movapd 592(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 592(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 592(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 592(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 592(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+592(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 92
      movapd 592(pA0), rA0
      movapd 592(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 592(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 592(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 592(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 592(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 94
      movapd 608(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 608(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 608(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 608(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 608(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+608(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 94
      movapd 608(pA0), rA0
      movapd 608(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 608(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 608(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 608(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 608(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 96
      movapd 624(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 624(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 624(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 624(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 624(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+624(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 96
      movapd 624(pA0), rA0
      movapd 624(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 624(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 624(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 624(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 624(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 98
      movapd 640(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 640(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 640(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 640(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 640(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+640(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 98
      movapd 640(pA0), rA0
      movapd 640(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 640(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 640(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 640(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 640(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 100
      movapd 656(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 656(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 656(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 656(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 656(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+656(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 100
      movapd 656(pA0), rA0
      movapd 656(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 656(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 656(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 656(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 656(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 102
      movapd 672(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 672(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 672(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 672(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 672(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+672(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 102
      movapd 672(pA0), rA0
      movapd 672(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 672(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 672(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 672(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 672(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 104
      movapd 688(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 688(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 688(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 688(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 688(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+688(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 104
      movapd 688(pA0), rA0
      movapd 688(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 688(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 688(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 688(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 688(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 106
      movapd 704(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 704(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 704(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 704(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 704(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+704(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 106
      movapd 704(pA0), rA0
      movapd 704(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 704(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 704(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 704(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 704(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 108
      movapd 720(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 720(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 720(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 720(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 720(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+720(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 108
      movapd 720(pA0), rA0
      movapd 720(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 720(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 720(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 720(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 720(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 110
      movapd 736(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 736(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 736(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 736(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 736(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+736(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 110
      movapd 736(pA0), rA0
      movapd 736(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 736(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 736(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 736(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 736(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 112
      movapd 752(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 752(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 752(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 752(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 752(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+752(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 112
      movapd 752(pA0), rA0
      movapd 752(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 752(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 752(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 752(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 752(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 114
      movapd 768(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 768(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 768(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 768(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 768(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+768(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 114
      movapd 768(pA0), rA0
      movapd 768(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 768(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 768(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 768(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 768(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 116
      movapd 784(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 784(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 784(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 784(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 784(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+784(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 116
      movapd 784(pA0), rA0
      movapd 784(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 784(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 784(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 784(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 784(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 118
      movapd 800(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 800(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 800(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 800(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 800(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+800(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 118
      movapd 800(pA0), rA0
      movapd 800(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 800(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 800(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 800(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 800(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 120
      movapd 816(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 816(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 816(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 816(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 816(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+816(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 120
      movapd 816(pA0), rA0
      movapd 816(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 816(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 816(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 816(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 816(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 122
      movapd 832(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 832(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 832(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 832(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 832(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+832(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 122
      movapd 832(pA0), rA0
      movapd 832(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 832(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 832(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 832(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 832(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 124
      movapd 848(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 848(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 848(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 848(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 848(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+848(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 124
      movapd 848(pA0), rA0
      movapd 848(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 848(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 848(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 848(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 848(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 126
      movapd 864(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 864(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 864(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 864(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 864(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+864(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 126
      movapd 864(pA0), rA0
      movapd 864(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 864(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 864(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 864(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 864(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
/* KLOOP end */
        haddpd	rC10, rC00
#ifdef BETAX
        movapd  BETAOFF(%rsp), rB0
        mulpd   rB0, rA0
#endif
#ifndef BETA0
        addpd   rA0, rC00
#endif
#ifdef DCPLX
	movlpd	rC00, (pC0)
	movhpd	rC00, 16(pC0)
#else
	MOVCPD	rC00, (pC0)
#endif
        haddpd	rC30, rC20
#ifdef BETAX
        mulpd   rB0, rA1
#endif
#ifndef BETA0
        addpd   rA1, rC20
#endif
#ifdef DCPLX
	movlpd	rC20, 32(pC0)
	movhpd	rC20, 48(pC0)
#else
	MOVCPD	rC20, 16(pC0)
#endif
        haddpd	rC11, rC01
#ifdef BETAX
        mulpd   rB0, rA2
#endif
#ifndef BETA0
        addpd   rA2, rC01
#endif
#ifdef DCPLX
	movlpd	rC01, (pC0,ldc)
	movhpd	rC01, 16(pC0,ldc)
#else
	MOVCPD	rC01, (pC0,ldc)
#endif
        haddpd	rC31, rC21
#ifdef BETAX
        mulpd   rB0, rA3
#endif
#ifndef BETA0
        addpd   rA3, rC21
#endif
#ifdef DCPLX
	movlpd	rC21, 32(pC0,ldc)
	movhpd	rC21, 48(pC0,ldc)
#else
	MOVCPD	rC21, 16(pC0,ldc)
#endif

        add     $4*CMUL(8), pC0
          prefetcht1      (pfA)
          add     $64, pfA

        lea     0(pA0,lda,4), pA0
        sub     $4, MM
        jnz     MNLOOP

        movq    MM0, MM
        sub     incAn, pA0
        add     incCn, pC0
	lea	(pB0, ldb, 2), pB0
        sub     $2, NN
        jnz     MNLOOP

/* DONE: */
        movq    -8(%rsp), %rbp
        movq    -16(%rsp), %rbx
        movq    -24(%rsp), %r12
        movq    -32(%rsp), %r13
        movq    -40(%rsp), %r14
/*        movq    -48(%rsp), %r15  */
        ret
/*
 * Same code as above, but assuming C is not aligned
 */
#ifndef DCPLX
   #undef  MOVCPD
   #define MOVCPD movupd
ALIGN16
UMNLOOP:
/* MLOOP: */
/* KLOOP begin */
#if KB == 2
        movapd -128(pA0), rC00
        movapd -128(pB0), rB0
        movapd rC00, rC01
        mulpd  rB0, rC00
        movapd -128(pA0,lda), rC10
        movapd rC10, rC11
        mulpd  rB0, rC10
        movapd -128(pA0,lda,2), rC20
        movapd rC20, rC21
        mulpd  rB0, rC20
        movapd -128(pA0,lda3), rC30
        movapd rC30, rC31
        mulpd  rB0, rC30
        movapd -128(pB0,ldb), rB0
        mulpd  rB0, rC01
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0), rA0
              movhpd 16(pC0), rA0
           #else
              MOVCPD (pC0), rA0
           #endif
        #endif
        mulpd  rB0, rC11
        #ifndef BETA0
           #ifdef DCPLX
              movsd  32(pC0), rA1
              movhpd 48(pC0), rA1
           #else
              MOVCPD 16(pC0), rA1
           #endif
        #endif
        mulpd  rB0, rC21
        #ifndef BETA0
           #ifdef DCPLX
              movsd  (pC0,ldc), rA2
              movhpd 16(pC0,ldc), rA2
           #else
              MOVCPD (pC0,ldc), rA2
           #endif
        #endif
        mulpd  rB0, rC31
        #ifndef BETA0
           #ifdef DCPLX
              movsd  32(pC0,ldc), rA3
              movhpd 48(pC0,ldc), rA3
           #else
              MOVCPD 16(pC0,ldc), rA3
           #endif
        #endif
#else
        movapd -128(pA0), rC00 ; \
        movapd -128(pB0), rB0 ; \
        movapd rC00, rC01 ; \
        mulpd  rB0, rC00 ; \
        movapd -128(pA0,lda), rC10 ; \
        movapd rC10, rC11 ; \
        mulpd  rB0, rC10 ; \
        movapd -128(pA0,lda,2), rC20 ; \
        movapd rC20, rC21 ; \
        mulpd  rB0, rC20 ; \
        movapd -128(pA0,lda3), rC30 ; \
        movapd rC30, rC31 ; \
        mulpd  rB0, rC30 ; \
        movapd -128(pB0,ldb), ra0 ; \
        mulpd  ra0, rC01 ; \
        mulpd  ra0, rC11 ; \
           movapd 16-128(pB0), rB0 ; \
        mulpd  ra0, rC21 ; \
        mulpd  ra0, rC31 ;
#endif
        prefetcht0      (pfB)
        add     $64, pfB
   #if KB > 4
      movapd -112(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -112(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -112(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -112(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -112(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+-112(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 4
      movapd -112(pA0), rA0
      movapd -112(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -112(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -112(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -112(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -112(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 6
      movapd -96(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -96(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -96(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -96(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -96(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+-96(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 6
      movapd -96(pA0), rA0
      movapd -96(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -96(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -96(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -96(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -96(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 8
      movapd -80(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -80(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -80(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -80(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -80(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+-80(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 8
      movapd -80(pA0), rA0
      movapd -80(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -80(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -80(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -80(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -80(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 10
      movapd -64(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -64(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -64(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -64(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -64(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+-64(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 10
      movapd -64(pA0), rA0
      movapd -64(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -64(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -64(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -64(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -64(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 12
      movapd -48(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -48(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -48(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -48(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -48(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+-48(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 12
      movapd -48(pA0), rA0
      movapd -48(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -48(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -48(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -48(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -48(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 14
      movapd -32(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -32(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -32(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -32(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -32(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+-32(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 14
      movapd -32(pA0), rA0
      movapd -32(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -32(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -32(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -32(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -32(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 16
      movapd -16(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -16(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -16(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -16(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -16(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+-16(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 16
      movapd -16(pA0), rA0
      movapd -16(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd -16(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd -16(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd -16(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd -16(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 18
      movapd 0(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 0(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 0(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 0(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 0(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+0(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 18
      movapd 0(pA0), rA0
      movapd 0(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 0(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 0(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 0(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 0(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 20
      movapd 16(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 16(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 16(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 16(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 16(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+16(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 20
      movapd 16(pA0), rA0
      movapd 16(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 16(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 16(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 16(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 16(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 22
      movapd 32(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 32(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 32(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 32(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 32(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+32(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 22
      movapd 32(pA0), rA0
      movapd 32(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 32(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 32(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 32(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 32(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 24
      movapd 48(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 48(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 48(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 48(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 48(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+48(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 24
      movapd 48(pA0), rA0
      movapd 48(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 48(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 48(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 48(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 48(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 26
      movapd 64(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 64(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 64(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 64(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 64(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+64(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 26
      movapd 64(pA0), rA0
      movapd 64(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 64(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 64(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 64(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 64(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 28
      movapd 80(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 80(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 80(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 80(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 80(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+80(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 28
      movapd 80(pA0), rA0
      movapd 80(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 80(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 80(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 80(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 80(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 30
      movapd 96(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 96(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 96(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 96(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 96(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+96(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 30
      movapd 96(pA0), rA0
      movapd 96(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 96(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 96(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 96(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 96(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 32
      movapd 112(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 112(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 112(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 112(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 112(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+112(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 32
      movapd 112(pA0), rA0
      movapd 112(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 112(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 112(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 112(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 112(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 34
      movapd 128(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 128(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 128(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 128(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 128(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+128(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 34
      movapd 128(pA0), rA0
      movapd 128(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 128(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 128(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 128(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 128(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 36
      movapd 144(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 144(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 144(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 144(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 144(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+144(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 36
      movapd 144(pA0), rA0
      movapd 144(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 144(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 144(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 144(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 144(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 38
      movapd 160(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 160(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 160(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 160(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 160(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+160(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 38
      movapd 160(pA0), rA0
      movapd 160(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 160(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 160(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 160(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 160(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 40
      movapd 176(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 176(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 176(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 176(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 176(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+176(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 40
      movapd 176(pA0), rA0
      movapd 176(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 176(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 176(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 176(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 176(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 42
      movapd 192(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 192(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 192(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 192(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 192(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+192(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 42
      movapd 192(pA0), rA0
      movapd 192(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 192(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 192(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 192(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 192(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 44
      movapd 208(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 208(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 208(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 208(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 208(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+208(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 44
      movapd 208(pA0), rA0
      movapd 208(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 208(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 208(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 208(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 208(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 46
      movapd 224(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 224(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 224(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 224(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 224(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+224(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 46
      movapd 224(pA0), rA0
      movapd 224(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 224(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 224(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 224(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 224(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 48
      movapd 240(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 240(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 240(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 240(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 240(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+240(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 48
      movapd 240(pA0), rA0
      movapd 240(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 240(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 240(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 240(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 240(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 50
      movapd 256(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 256(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 256(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 256(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 256(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+256(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 50
      movapd 256(pA0), rA0
      movapd 256(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 256(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 256(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 256(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 256(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 52
      movapd 272(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 272(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 272(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 272(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 272(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+272(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 52
      movapd 272(pA0), rA0
      movapd 272(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 272(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 272(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 272(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 272(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 54
      movapd 288(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 288(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 288(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 288(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 288(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+288(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 54
      movapd 288(pA0), rA0
      movapd 288(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 288(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 288(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 288(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 288(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 56
      movapd 304(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 304(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 304(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 304(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 304(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+304(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 56
      movapd 304(pA0), rA0
      movapd 304(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 304(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 304(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 304(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 304(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 58
      movapd 320(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 320(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 320(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 320(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 320(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+320(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 58
      movapd 320(pA0), rA0
      movapd 320(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 320(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 320(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 320(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 320(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 60
      movapd 336(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 336(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 336(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 336(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 336(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+336(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 60
      movapd 336(pA0), rA0
      movapd 336(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 336(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 336(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 336(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 336(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 62
      movapd 352(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 352(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 352(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 352(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 352(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+352(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 62
      movapd 352(pA0), rA0
      movapd 352(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 352(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 352(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 352(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 352(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 64
      movapd 368(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 368(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 368(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 368(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 368(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+368(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 64
      movapd 368(pA0), rA0
      movapd 368(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 368(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 368(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 368(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 368(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 66
      movapd 384(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 384(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 384(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 384(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 384(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+384(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 66
      movapd 384(pA0), rA0
      movapd 384(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 384(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 384(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 384(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 384(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 68
      movapd 400(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 400(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 400(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 400(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 400(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+400(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 68
      movapd 400(pA0), rA0
      movapd 400(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 400(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 400(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 400(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 400(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 70
      movapd 416(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 416(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 416(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 416(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 416(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+416(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 70
      movapd 416(pA0), rA0
      movapd 416(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 416(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 416(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 416(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 416(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 72
      movapd 432(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 432(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 432(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 432(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 432(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+432(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 72
      movapd 432(pA0), rA0
      movapd 432(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 432(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 432(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 432(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 432(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 74
      movapd 448(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 448(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 448(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 448(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 448(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+448(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 74
      movapd 448(pA0), rA0
      movapd 448(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 448(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 448(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 448(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 448(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 76
      movapd 464(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 464(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 464(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 464(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 464(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+464(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 76
      movapd 464(pA0), rA0
      movapd 464(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 464(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 464(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 464(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 464(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 78
      movapd 480(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 480(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 480(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 480(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 480(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+480(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 78
      movapd 480(pA0), rA0
      movapd 480(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 480(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 480(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 480(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 480(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 80
      movapd 496(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 496(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 496(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 496(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 496(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+496(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 80
      movapd 496(pA0), rA0
      movapd 496(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 496(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 496(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 496(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 496(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 82
      movapd 512(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 512(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 512(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 512(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 512(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+512(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 82
      movapd 512(pA0), rA0
      movapd 512(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 512(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 512(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 512(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 512(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 84
      movapd 528(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 528(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 528(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 528(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 528(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+528(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 84
      movapd 528(pA0), rA0
      movapd 528(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 528(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 528(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 528(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 528(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 86
      movapd 544(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 544(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 544(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 544(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 544(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+544(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 86
      movapd 544(pA0), rA0
      movapd 544(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 544(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 544(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 544(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 544(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 88
      movapd 560(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 560(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 560(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 560(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 560(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+560(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 88
      movapd 560(pA0), rA0
      movapd 560(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 560(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 560(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 560(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 560(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 90
      movapd 576(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 576(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 576(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 576(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 576(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+576(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 90
      movapd 576(pA0), rA0
      movapd 576(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 576(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 576(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 576(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 576(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 92
      movapd 592(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 592(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 592(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 592(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 592(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+592(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 92
      movapd 592(pA0), rA0
      movapd 592(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 592(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 592(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 592(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 592(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 94
      movapd 608(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 608(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 608(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 608(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 608(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+608(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 94
      movapd 608(pA0), rA0
      movapd 608(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 608(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 608(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 608(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 608(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 96
      movapd 624(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 624(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 624(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 624(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 624(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+624(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 96
      movapd 624(pA0), rA0
      movapd 624(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 624(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 624(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 624(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 624(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 98
      movapd 640(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 640(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 640(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 640(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 640(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+640(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 98
      movapd 640(pA0), rA0
      movapd 640(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 640(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 640(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 640(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 640(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 100
      movapd 656(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 656(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 656(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 656(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 656(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+656(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 100
      movapd 656(pA0), rA0
      movapd 656(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 656(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 656(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 656(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 656(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 102
      movapd 672(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 672(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 672(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 672(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 672(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+672(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 102
      movapd 672(pA0), rA0
      movapd 672(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 672(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 672(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 672(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 672(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 104
      movapd 688(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 688(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 688(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 688(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 688(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+688(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 104
      movapd 688(pA0), rA0
      movapd 688(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 688(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 688(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 688(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 688(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 106
      movapd 704(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 704(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 704(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 704(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 704(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+704(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 106
      movapd 704(pA0), rA0
      movapd 704(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 704(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 704(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 704(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 704(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 108
      movapd 720(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 720(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 720(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 720(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 720(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+720(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 108
      movapd 720(pA0), rA0
      movapd 720(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 720(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 720(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 720(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 720(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 110
      movapd 736(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 736(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 736(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 736(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 736(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+736(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 110
      movapd 736(pA0), rA0
      movapd 736(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 736(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 736(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 736(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 736(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 112
      movapd 752(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 752(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 752(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 752(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 752(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+752(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 112
      movapd 752(pA0), rA0
      movapd 752(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 752(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 752(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 752(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 752(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 114
      movapd 768(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 768(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 768(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 768(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 768(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+768(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 114
      movapd 768(pA0), rA0
      movapd 768(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 768(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 768(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 768(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 768(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 116
      movapd 784(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 784(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 784(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 784(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 784(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+784(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 116
      movapd 784(pA0), rA0
      movapd 784(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 784(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 784(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 784(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 784(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 118
      movapd 800(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 800(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 800(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 800(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 800(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+800(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 118
      movapd 800(pA0), rA0
      movapd 800(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 800(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 800(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 800(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 800(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 120
      movapd 816(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 816(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 816(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 816(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 816(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+816(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 120
      movapd 816(pA0), rA0
      movapd 816(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 816(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 816(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 816(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 816(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 122
      movapd 832(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 832(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 832(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 832(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 832(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+832(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 122
      movapd 832(pA0), rA0
      movapd 832(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 832(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 832(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 832(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 832(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 124
      movapd 848(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 848(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 848(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 848(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 848(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+848(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 124
      movapd 848(pA0), rA0
      movapd 848(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 848(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 848(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 848(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 848(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
   #if KB > 126
      movapd 864(pA0), rA0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 864(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 864(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 864(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 864(pB0,ldb), rB0
      mulpd  rB0, ra0
      mulpd  rB0, ra1
      mulpd  rB0, ra2
      mulpd  rB0, rA3
         movapd 16+864(pB0), rB0
      addpd  ra0, rC01
      addpd  ra1, rC11
      addpd  ra2, rC21
      addpd  rA3, rC31
   #elif KB == 126
      movapd 864(pA0), rA0
      movapd 864(pB0), rB0
      movapd rA0, ra0
      mulpd  rB0, rA0
      addpd  rA0, rC00
      movapd 864(pA0,lda), rA1
      movapd rA1, ra1
      mulpd  rB0, rA1
      addpd  rA1, rC10
      movapd 864(pA0,lda,2), rA2
      movapd rA2, ra2
      mulpd  rB0, rA2
      addpd  rA2, rC20
      movapd 864(pA0,lda3), rA3
      mulpd  rA3, rB0
      addpd  rB0, rC30
      movapd 864(pB0,ldb), rB0
      mulpd  rB0, ra0
      addpd  ra0, rC01
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0), rA0
            movhpd 16(pC0), rA0
         #else
            MOVCPD (pC0), rA0
         #endif
      #endif
      mulpd  rB0, ra1
      addpd  ra1, rC11
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0), rA1
            movhpd 48(pC0), rA1
         #else
            MOVCPD 16(pC0), rA1
         #endif
      #endif
      mulpd  rB0, ra2
      addpd  ra2, rC21
      #ifndef BETA0
         #ifdef DCPLX
            movsd  (pC0,ldc), rA2
            movhpd 16(pC0,ldc), rA2
         #else
            MOVCPD (pC0,ldc), rA2
         #endif
      #endif
      mulpd  rB0, rA3
      addpd  rA3, rC31
      #ifndef BETA0
         #ifdef DCPLX
            movsd  32(pC0,ldc), rA3
            movhpd 48(pC0,ldc), rA3
         #else
            MOVCPD 16(pC0,ldc), rA3
         #endif
      #endif
   #endif
/* KLOOP end */
        haddpd	rC10, rC00
#ifdef BETAX
        movapd  BETAOFF(%rsp), rB0
        mulpd   rB0, rA0
#endif
#ifndef BETA0
        addpd   rA0, rC00
#endif
#ifdef DCPLX
	movlpd	rC00, (pC0)
	movhpd	rC00, 16(pC0)
#else
	MOVCPD	rC00, (pC0)
#endif
        haddpd	rC30, rC20
#ifdef BETAX
        mulpd   rB0, rA1
#endif
#ifndef BETA0
        addpd   rA1, rC20
#endif
#ifdef DCPLX
	movlpd	rC20, 32(pC0)
	movhpd	rC20, 48(pC0)
#else
	MOVCPD	rC20, 16(pC0)
#endif
        haddpd	rC11, rC01
#ifdef BETAX
        mulpd   rB0, rA2
#endif
#ifndef BETA0
        addpd   rA2, rC01
#endif
#ifdef DCPLX
	movlpd	rC01, (pC0,ldc)
	movhpd	rC01, 16(pC0,ldc)
#else
	MOVCPD	rC01, (pC0,ldc)
#endif
        haddpd	rC31, rC21
#ifdef BETAX
        mulpd   rB0, rA3
#endif
#ifndef BETA0
        addpd   rA3, rC21
#endif
#ifdef DCPLX
	movlpd	rC21, 32(pC0,ldc)
	movhpd	rC21, 48(pC0,ldc)
#else
	MOVCPD	rC21, 16(pC0,ldc)
#endif

        add     $4*CMUL(8), pC0
          prefetcht1      (pfA)
          add     $64, pfA

        lea     0(pA0,lda,4), pA0
        sub     $4, MM
        jnz     UMNLOOP

        movq    MM0, MM
        sub     incAn, pA0
        add     incCn, pC0
	lea	(pB0, ldb, 2), pB0
        sub     $2, NN
        jnz     UMNLOOP

/* DONE: */
        movq    -8(%rsp), %rbp
        movq    -16(%rsp), %rbx
        movq    -24(%rsp), %r12
        movq    -32(%rsp), %r13
        movq    -40(%rsp), %r14
/*        movq    -48(%rsp), %r15  */
        ret
#endif   /* end of repeated loops for unaligned C for real precision */
