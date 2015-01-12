/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2011 R. Clint Whaley
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
#if !defined(ATL_GAS_x8664)
   #error "This kernel requires x86-64 assembly!"
#endif
#ifndef ATL_AVX
   #error "This routine requires AVX!"
#endif

#if !defined(KB) || (KB == 0)
   #error "KB must be a compile-time constant!"
#endif
#if ((KB/4)*4 != KB)
   #error "KB must be a multiple of 4!"
#endif
#if KB > 256
   #error "KB can at most be 256!"
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

#define rA0 	%ymm0
   #define xA0  %xmm0
#define rA1 	%ymm1
   #define xA1  %xmm1
#define rA2 	%ymm2
   #define xA2  %xmm2
#define rA3 	%ymm3
   #define xA3  %xmm3
#define rB0 	%ymm4
#define rB1	%ymm5
#define m0 	%ymm6
#define rC00 	%ymm7
#define rC10 	%ymm8
#define rC20 	%ymm9
#define rC30 	%ymm10
#define rC01 	%ymm11
#define rC11 	%ymm12
#define rC21 	%ymm13
#define rC31 	%ymm14
#define rbeta   %ymm15
   #define xbeta   %xmm15

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
        unpcklpd %xmm1, %xmm1
        vinsertf128 $0x01, %xmm1, %ymm1, rbeta
           /* dest, src1, src2, imm8 --> imm8, src2, src1, dest */
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
#ifdef DCPLX00
        test    $15, ldc
        jnz UMNLOOP
        test    $15, pC0
        jnz     UMNLOOP
#endif
ALIGN16
MNLOOP:
/* MLOOP: */
/* KLOOP begin */
      vmovapd -128(pB0), rB0
      vmovapd -128(pA0), rA0
      vmulpd  rA0, rB0, rC00
      vmovapd -128(pA0,lda), rA1
      vmulpd  rA1, rB0, rC10
      vmovapd -128(pA0,lda,2), rA2
      vmulpd  rA2, rB0, rC20
      vmovapd -128(pA0,lda3), rA3
      vmulpd  rA3, rB0, rC30
      vmovapd -128(pB0,ldb), rB1

      #if KB == 4
         vmulpd  rA0, rB1, rC01
            #if defined(DCPLX) && !defined(BETA0)
               movlpd  32(pC0), xA0
            #elif !defined(BETA0)
               vmovupd (pC0), rA0
            #endif
         vmulpd  rA1, rB1, rC11
            #if defined(DCPLX) && !defined(BETA0)
               movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
            #endif
         vmulpd  rA2, rB1, rC21
            #if defined(DCPLX) && !defined(BETA0)
               movlpd  32(pC0,ldc), xA1
            #elif !defined(BETA0)
               vmovupd (pC0,ldc), rA1
            #endif
         vmulpd  rA3, rB1, rC31
            #if defined(DCPLX) && !defined(BETA0)
               movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
            #endif

      #else
         vmulpd  rA0, rB1, rC01
            vmovapd -96(pB0), rB0
         vmulpd  rA1, rB1, rC11
            vmovapd -96(pA0), rA0
         vmulpd  rA2, rB1, rC21
            vmovapd -96(pA0,lda), rA1
         vmulpd  rA3, rB1, rC31
            vmovapd -96(pA0,lda,2), rA2
      #endif

   #if KB == 8
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd -96(pA0,lda3), rA3
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmovapd -96(pB0,ldb), rB1
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
        prefetcht0      (pfB)
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 8
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd -96(pA0,lda3), rA3
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmovapd -96(pB0,ldb), rB1
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
        prefetcht0      (pfB)
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd -64(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd -64(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd -64(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd -64(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd -64(pA0,lda3), rA3
   #endif

   #if KB == 12
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
        add     $64, pfB
      vmovapd -64(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
          prefetcht0      (pfA)
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
          add     $64, pfA
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0, ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 12
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
        add     $64, pfB
      vmovapd -64(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
          prefetcht0      (pfA)
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
          add     $64, pfA
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd -32(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd -32(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd -32(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd -32(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd -32(pA0,lda3), rA3
   #endif

   #if KB == 16
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd -32(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 16
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd -32(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 0(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 0(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 0(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 0(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 0(pA0,lda3), rA3
   #endif
   #if KB == 20
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 0(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 20
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 0(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 32(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 32(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 32(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 32(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 32(pA0,lda3), rA3
   #endif
   #if KB == 24
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 32(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 24
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 32(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 64(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 64(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 64(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 64(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 64(pA0,lda3), rA3
   #endif
   #if KB == 28
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 64(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 28
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 64(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 96(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 96(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 96(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 96(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 96(pA0,lda3), rA3
   #endif
   #if KB == 32
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 96(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 32
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 96(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 128(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 128(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 128(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 128(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 128(pA0,lda3), rA3
   #endif
   #if KB == 36
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 128(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 36
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 128(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 160(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 160(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 160(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 160(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 160(pA0,lda3), rA3
   #endif
   #if KB == 40
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 160(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 40
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 160(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 192(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 192(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 192(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 192(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 192(pA0,lda3), rA3
   #endif
   #if KB == 44
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 192(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 44
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 192(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 224(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 224(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 224(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 224(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 224(pA0,lda3), rA3
   #endif
   #if KB == 48
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 224(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 48
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 224(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 256(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 256(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 256(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 256(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 256(pA0,lda3), rA3
   #endif
   #if KB == 52
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 256(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 52
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 256(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 288(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 288(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 288(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 288(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 288(pA0,lda3), rA3
   #endif
   #if KB == 56
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 288(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 56
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 288(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 320(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 320(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 320(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 320(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 320(pA0,lda3), rA3
   #endif
   #if KB == 60
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 320(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 60
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 320(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 352(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 352(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 352(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 352(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 352(pA0,lda3), rA3
   #endif
   #if KB == 64
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 352(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 64
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 352(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 384(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 384(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 384(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 384(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 384(pA0,lda3), rA3
   #endif
   #if KB == 68
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 384(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 68
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 384(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 416(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 416(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 416(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 416(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 416(pA0,lda3), rA3
   #endif
   #if KB == 72
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 416(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 72
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 416(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 448(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 448(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 448(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 448(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 448(pA0,lda3), rA3
   #endif
   #if KB == 76
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 448(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 76
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 448(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 480(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 480(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 480(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 480(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 480(pA0,lda3), rA3
   #endif
   #if KB == 80
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 480(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 80
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 480(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 512(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 512(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 512(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 512(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 512(pA0,lda3), rA3
   #endif
   #if KB == 84
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 512(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 84
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 512(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 544(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 544(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 544(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 544(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 544(pA0,lda3), rA3
   #endif
   #if KB == 88
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 544(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 88
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 544(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 576(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 576(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 576(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 576(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 576(pA0,lda3), rA3
   #endif
   #if KB == 92
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 576(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 92
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 576(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 608(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 608(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 608(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 608(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 608(pA0,lda3), rA3
   #endif
   #if KB == 96
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 608(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 96
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 608(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 640(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 640(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 640(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 640(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 640(pA0,lda3), rA3
   #endif
   #if KB == 100
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 640(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 100
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 640(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 672(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 672(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 672(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 672(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 672(pA0,lda3), rA3
   #endif
   #if KB == 104
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 672(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 104
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 672(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 704(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 704(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 704(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 704(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 704(pA0,lda3), rA3
   #endif
   #if KB == 108
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 704(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 108
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 704(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 736(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 736(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 736(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 736(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 736(pA0,lda3), rA3
   #endif
   #if KB == 112
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 736(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 112
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 736(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 768(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 768(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 768(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 768(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 768(pA0,lda3), rA3
   #endif
   #if KB == 116
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 768(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 116
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 768(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 800(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 800(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 800(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 800(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 800(pA0,lda3), rA3
   #endif
   #if KB == 120
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 800(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 120
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 800(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 832(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 832(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 832(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 832(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 832(pA0,lda3), rA3
   #endif
   #if KB == 124
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 832(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 124
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 832(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 864(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 864(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 864(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 864(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 864(pA0,lda3), rA3
   #endif
   #if KB == 128
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 864(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 128
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 864(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 896(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 896(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 896(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 896(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 896(pA0,lda3), rA3
   #endif
   #if KB == 132
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 896(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 132
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 896(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 928(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 928(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 928(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 928(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 928(pA0,lda3), rA3
   #endif
   #if KB == 136
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 928(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 136
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 928(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 960(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 960(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 960(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 960(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 960(pA0,lda3), rA3
   #endif
   #if KB == 140
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 960(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 140
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 960(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 992(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 992(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 992(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 992(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 992(pA0,lda3), rA3
   #endif
   #if KB == 144
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 992(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 144
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 992(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1024(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1024(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1024(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1024(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1024(pA0,lda3), rA3
   #endif
   #if KB == 148
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1024(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 148
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1024(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1056(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1056(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1056(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1056(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1056(pA0,lda3), rA3
   #endif
   #if KB == 152
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1056(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 152
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1056(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1088(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1088(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1088(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1088(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1088(pA0,lda3), rA3
   #endif
   #if KB == 156
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1088(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 156
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1088(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1120(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1120(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1120(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1120(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1120(pA0,lda3), rA3
   #endif
   #if KB == 160
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1120(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 160
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1120(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1152(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1152(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1152(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1152(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1152(pA0,lda3), rA3
   #endif
   #if KB == 164
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1152(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 164
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1152(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1184(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1184(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1184(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1184(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1184(pA0,lda3), rA3
   #endif
   #if KB == 168
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1184(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 168
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1184(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1216(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1216(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1216(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1216(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1216(pA0,lda3), rA3
   #endif
   #if KB == 172
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1216(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 172
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1216(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1248(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1248(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1248(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1248(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1248(pA0,lda3), rA3
   #endif
   #if KB == 176
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1248(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 176
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1248(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1280(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1280(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1280(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1280(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1280(pA0,lda3), rA3
   #endif
   #if KB == 180
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1280(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 180
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1280(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1312(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1312(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1312(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1312(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1312(pA0,lda3), rA3
   #endif
   #if KB == 184
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1312(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 184
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1312(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1344(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1344(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1344(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1344(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1344(pA0,lda3), rA3
   #endif
   #if KB == 188
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1344(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 188
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1344(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1376(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1376(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1376(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1376(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1376(pA0,lda3), rA3
   #endif
   #if KB == 192
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1376(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 192
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1376(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1408(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1408(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1408(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1408(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1408(pA0,lda3), rA3
   #endif
   #if KB == 196
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1408(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 196
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1408(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1440(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1440(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1440(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1440(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1440(pA0,lda3), rA3
   #endif
   #if KB == 200
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1440(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 200
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1440(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1472(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1472(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1472(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1472(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1472(pA0,lda3), rA3
   #endif
   #if KB == 204
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1472(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 204
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1472(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1504(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1504(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1504(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1504(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1504(pA0,lda3), rA3
   #endif
   #if KB == 208
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1504(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 208
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1504(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1536(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1536(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1536(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1536(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1536(pA0,lda3), rA3
   #endif
   #if KB == 212
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1536(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 212
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1536(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1568(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1568(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1568(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1568(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1568(pA0,lda3), rA3
   #endif
   #if KB == 216
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1568(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 216
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1568(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1600(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1600(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1600(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1600(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1600(pA0,lda3), rA3
   #endif
   #if KB == 220
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1600(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 220
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1600(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1632(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1632(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1632(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1632(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1632(pA0,lda3), rA3
   #endif
   #if KB == 224
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1632(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 224
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1632(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1664(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1664(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1664(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1664(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1664(pA0,lda3), rA3
   #endif
   #if KB == 228
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1664(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 228
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1664(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1696(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1696(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1696(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1696(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1696(pA0,lda3), rA3
   #endif
   #if KB == 232
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1696(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 232
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1696(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1728(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1728(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1728(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1728(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1728(pA0,lda3), rA3
   #endif
   #if KB == 236
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1728(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 236
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1728(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1760(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1760(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1760(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1760(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1760(pA0,lda3), rA3
   #endif
   #if KB == 240
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1760(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 240
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1760(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1792(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1792(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1792(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1792(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1792(pA0,lda3), rA3
   #endif
   #if KB == 244
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1792(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 244
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1792(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1824(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1824(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1824(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1824(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1824(pA0,lda3), rA3
   #endif
   #if KB == 248
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1824(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 248
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1824(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1856(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1856(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1856(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1856(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1856(pA0,lda3), rA3
   #endif
   #if KB == 252
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1856(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0), xA0
      #elif !defined(BETA0)
         vmovupd (pC0), rA0
      #endif
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0), xA0                   /* XXX XXX c30 c20 */
      #elif !defined(BETA0)
         vmovupd (pC0,ldc), rA1
      #endif
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
      #if defined(DCPLX) && !defined(BETA0)
         movlpd  32(pC0,ldc), xA1
      #endif
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
      #if defined(DCPLX) && !defined(BETA0)
         movhpd  48(pC0,ldc), xA1               /* XXX XXX c31, c21 */
      #endif
   #elif KB > 252
      vmulpd  rA0, rB0, m0
      vaddpd  rC00, m0, rC00
      vmovapd 1856(pB0,ldb), rB1
      vmulpd  rA1, rB0, m0
      vaddpd  rC10, m0, rC10
      vmulpd  rA2, rB0, m0
      vaddpd  rC20, m0, rC20
      vmulpd  rA3, rB0, m0
      vaddpd  rC30, m0, rC30
         vmovapd 1888(pB0), rB0

      vmulpd  rA0, rB1, m0
      vaddpd  rC01, m0, rC01
         vmovapd 1888(pA0), rA0
      vmulpd  rA1, rB1, m0
      vaddpd  rC11, m0, rC11
         vmovapd 1888(pA0,lda), rA1
      vmulpd  rA2, rB1, m0
      vaddpd  rC21, m0, rC21
         vmovapd 1888(pA0,lda,2), rA2
      vmulpd  rA3, rB1, m0
      vaddpd  rC31, m0, rC31
         vmovapd 1888(pA0,lda3), rA3
   #endif
       /* dest, src1, src2, imm8 --> imm8, src2, src1, dest */
/*
 *  Reduce Cvecs and, if BETA != 0 add to actual C, then store
 */
    vhaddpd rC10, rC00, rC00                    /* c10cd  c00cd c10ab c00ab */
    #if defined(DCPLX) && (defined(BETA1) || defined(BETAX))
       movlpd (pC0), xA2                        /* XXX XXX XXX c00 */
    #endif
    vhaddpd rC30, rC20, rC20                    /* c30cd  c20cd c30ab c20ab */
    #if defined(DCPLX) && (defined(BETA1) || defined(BETAX))
       movhpd 16(pC0), xA2                      /* XXX XXX c10 c00 */
    #endif
    #if defined(BETAX) && !defined(DCPLX)
       vmulpd rA0, rbeta, rA0
    #endif
    vperm2f128 $0x20, rC20, rC00, rC10          /* c30ab  c20ab  c10ab c00ab */
    #if defined(DCPLX) && (defined(BETA1) || defined(BETAX))
       vperm2f128 $0x20, rA0, rA2, rA0
    #endif
    vhaddpd rC11, rC01, rC01
    #if defined(DCPLX) && (defined(BETA1) || defined(BETAX))
       movlpd (pC0,ldc), xA3                    /* XXX XXX XXX c01 */
    #endif
    #if defined(BETAX) && !defined(DCPLX)
       vmulpd rA1, rbeta, rA1
    #endif
    vperm2f128 $0x31, rC20, rC00, rC00          /* c30cd  c20cd  c10cd c00cd */
    vhaddpd rC31, rC21, rC21
    #if defined(DCPLX) && (defined(BETA1) || defined(BETAX))
       movhpd 16(pC0,ldc), xA3                  /* XXX XXX c11 c01 */
    #endif
    vperm2f128 $0x20, rC21, rC01, rC11          /* c31ab  c21ab  c11ab c01ab */
    vaddpd rC00, rC10, rC00                     /* c30    c20    c10   c00 */
    #if defined(DCPLX) && (defined(BETA1) || defined(BETAX))
       vperm2f128 $0x20, rA1, rA3, rA1
    #endif
    vperm2f128 $0x31, rC21, rC01, rC01          /* c31cd  c21cd  c11cd c01cd */
    vaddpd rC11, rC01, rC01                     /* c31    c21    c11   c01 */
    #if defined(BETAX) && defined(DCPLX)
       vmulpd rA0, rbeta, rA0
    #endif

    #ifndef BETA0
       vaddpd rC00, rA0, rC00
       #if defined(BETAX) && defined(DCPLX)
          vmulpd rA1, rbeta, rA1
       #endif
       vaddpd rC01, rA1, rC01
    #endif
    #ifdef DCPLX
       movsd %xmm7, (pC0)                       /* xmm7 is rC00 */
       movhpd %xmm7, 16(pC0)
       vextractf128 $1, rC00, %xmm7
       movsd %xmm11, (pC0,ldc)                  /* xmm11 is rC01 */
       movhpd %xmm11, 16(pC0,ldc)
       vextractf128 $1, rC01, %xmm11
       movsd %xmm7, 32(pC0)
       movhpd %xmm7, 48(pC0)
       movsd %xmm11, 32(pC0,ldc)
       movhpd %xmm11, 48(pC0,ldc)
    #else
       vmovupd rC00, (pC0)
       vmovupd rC01, (pC0,ldc)
    #endif
/* KLOOP end */
        add     $4*CMUL(8), pC0

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
