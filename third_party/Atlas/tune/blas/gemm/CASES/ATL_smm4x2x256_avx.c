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
#if ((KB/8)*8 != KB)
   #error "KB must be a multiple of 8!"
#endif
#if KB > 256
   #error "KB can at most be 256!"
#endif

#ifdef SCPLX
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
   #define xB0  %xmm4
#define rB1	%ymm5
   #define xB1  %xmm5
#define m0 	%ymm6
   #define xm0  %xmm6
#define rC00 	%ymm7
   #define xC00	%xmm7
#define rC10 	%ymm8
   #define xC10	%xmm8
#define rC20 	%ymm9
   #define xC20	%xmm9
#define rC30 	%ymm10
   #define xC30	%xmm10
#define rC01 	%ymm11
   #define xC01	%xmm11
#define rC11 	%ymm12
   #define xC11	%xmm12
#define rC21 	%ymm13
   #define xC21	%xmm13
#define rC31 	%ymm14
   #define xC31	%xmm14
#define rbeta   %ymm15
   #define xbeta   %xmm15

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
        pshufd $0, %xmm1, %xmm1
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
#ifdef SCPLX
        shl     $3, incCn
        shl	$3, ldc
#else
        shl     $2, incCn
        shl	$2, ldc
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
        shl     $2, lda
                                                prefetcht0      (pB0)
        lea     (lda,lda,2), lda3
/*
 *      ldb = ldb*sizeof
 */
        shl     $2, ldb
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
ALIGN16
MNLOOP:
/* MLOOP: */
/* KLOOP begin */
      vmovaps -128(pB0), rB0
      vmovaps -128(pA0), rA0
      vmulps  rA0, rB0, rC00
      vmovaps -128(pA0,lda), rA1
      vmulps  rA1, rB0, rC10
      vmovaps -128(pA0,lda,2), rA2
      vmulps  rA2, rB0, rC20
      vmovaps -128(pA0,lda3), rA3
      vmulps  rA3, rB0, rC30
      vmovaps -128(pB0,ldb), rB1

      #if KB == 8
         vmulps  rA0, rB1, rC01
         vmulps  rA1, rB1, rC11
         vmulps  rA2, rB1, rC21
         vmulps  rA3, rB1, rC31
      #else
         vmulps  rA0, rB1, rC01
            vmovaps -96(pB0), rB0
         vmulps  rA1, rB1, rC11
            vmovaps -96(pA0), rA0
         vmulps  rA2, rB1, rC21
            vmovaps -96(pA0,lda), rA1
         vmulps  rA3, rB1, rC31
            vmovaps -96(pA0,lda,2), rA2
      #endif

   #if KB == 16
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps -96(pA0,lda3), rA3
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmovaps -96(pB0,ldb), rB1
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
        prefetcht0      (pfB)
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 16
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps -96(pA0,lda3), rA3
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmovaps -96(pB0,ldb), rB1
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
        prefetcht0      (pfB)
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps -64(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps -64(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps -64(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps -64(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps -64(pA0,lda3), rA3
   #endif

   #if KB == 24
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
        add     $64, pfB
      vmovaps -64(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
          prefetcht0      (pfA)
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
          add     $64, pfA
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 24
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
        add     $64, pfB
      vmovaps -64(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
          prefetcht0      (pfA)
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
          add     $64, pfA
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps -32(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps -32(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps -32(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps -32(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps -32(pA0,lda3), rA3
   #endif

   #if KB == 32
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps -32(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 32
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps -32(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 0(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 0(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 0(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 0(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 0(pA0,lda3), rA3
   #endif
   #if KB == 40
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 0(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 40
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 0(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 32(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 32(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 32(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 32(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 32(pA0,lda3), rA3
   #endif
   #if KB == 48
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 32(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 48
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 32(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 64(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 64(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 64(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 64(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 64(pA0,lda3), rA3
   #endif
   #if KB == 56
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 64(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 56
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 64(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 96(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 96(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 96(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 96(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 96(pA0,lda3), rA3
   #endif
   #if KB == 64
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 96(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 64
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 96(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 128(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 128(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 128(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 128(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 128(pA0,lda3), rA3
   #endif
   #if KB == 72
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 128(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 72
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 128(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 160(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 160(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 160(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 160(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 160(pA0,lda3), rA3
   #endif
   #if KB == 80
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 160(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 80
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 160(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 192(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 192(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 192(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 192(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 192(pA0,lda3), rA3
   #endif
   #if KB == 88
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 192(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 88
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 192(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 224(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 224(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 224(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 224(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 224(pA0,lda3), rA3
   #endif
   #if KB == 96
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 224(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 96
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 224(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 256(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 256(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 256(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 256(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 256(pA0,lda3), rA3
   #endif
   #if KB == 104
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 256(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 104
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 256(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 288(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 288(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 288(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 288(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 288(pA0,lda3), rA3
   #endif
   #if KB == 112
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 288(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 112
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 288(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 320(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 320(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 320(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 320(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 320(pA0,lda3), rA3
   #endif
   #if KB == 120
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 320(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 120
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 320(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 352(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 352(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 352(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 352(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 352(pA0,lda3), rA3
   #endif
   #if KB == 128
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 352(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 128
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 352(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 384(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 384(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 384(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 384(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 384(pA0,lda3), rA3
   #endif
   #if KB == 136
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 384(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 136
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 384(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 416(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 416(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 416(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 416(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 416(pA0,lda3), rA3
   #endif
   #if KB == 144
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 416(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 144
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 416(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 448(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 448(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 448(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 448(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 448(pA0,lda3), rA3
   #endif
   #if KB == 152
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 448(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 152
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 448(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 480(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 480(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 480(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 480(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 480(pA0,lda3), rA3
   #endif
   #if KB == 160
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 480(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 160
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 480(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 512(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 512(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 512(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 512(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 512(pA0,lda3), rA3
   #endif
   #if KB == 168
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 512(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 168
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 512(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 544(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 544(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 544(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 544(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 544(pA0,lda3), rA3
   #endif
   #if KB == 176
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 544(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 176
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 544(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 576(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 576(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 576(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 576(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 576(pA0,lda3), rA3
   #endif
   #if KB == 184
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 576(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 184
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 576(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 608(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 608(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 608(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 608(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 608(pA0,lda3), rA3
   #endif
   #if KB == 192
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 608(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 192
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 608(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 640(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 640(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 640(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 640(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 640(pA0,lda3), rA3
   #endif
   #if KB == 200
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 640(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 200
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 640(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 672(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 672(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 672(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 672(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 672(pA0,lda3), rA3
   #endif
   #if KB == 208
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 672(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 208
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 672(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 704(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 704(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 704(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 704(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 704(pA0,lda3), rA3
   #endif
   #if KB == 216
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 704(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 216
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 704(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 736(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 736(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 736(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 736(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 736(pA0,lda3), rA3
   #endif
   #if KB == 224
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 736(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 224
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 736(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 768(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 768(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 768(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 768(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 768(pA0,lda3), rA3
   #endif
   #if KB == 232
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 768(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 232
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 768(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 800(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 800(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 800(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 800(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 800(pA0,lda3), rA3
   #endif
   #if KB == 240
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 800(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 240
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 800(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 832(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 832(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 832(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 832(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 832(pA0,lda3), rA3
   #endif
   #if KB == 248
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 832(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
   #elif KB > 248
      vmulps  rA0, rB0, m0
      vaddps  rC00, m0, rC00
      vmovaps 832(pB0,ldb), rB1
      vmulps  rA1, rB0, m0
      vaddps  rC10, m0, rC10
      vmulps  rA2, rB0, m0
      vaddps  rC20, m0, rC20
      vmulps  rA3, rB0, m0
      vaddps  rC30, m0, rC30
         vmovaps 864(pB0), rB0

      vmulps  rA0, rB1, m0
      vaddps  rC01, m0, rC01
         vmovaps 864(pA0), rA0
      vmulps  rA1, rB1, m0
      vaddps  rC11, m0, rC11
         vmovaps 864(pA0,lda), rA1
      vmulps  rA2, rB1, m0
      vaddps  rC21, m0, rC21
         vmovaps 864(pA0,lda,2), rA2
      vmulps  rA3, rB1, m0
      vaddps  rC31, m0, rC31
         vmovaps 864(pA0,lda3), rA3
   #endif
/* KLOOP end */
       /* dest, src1, src2, imm8 --> imm8, src2, src1, dest */
/*
 *  Reduce Cvecs and, if BETA != 0 add to actual C, then store
 */
        #if defined(SCPLX) && !defined(BETA0)
                          /* MEM = SEG c30 XXX c20 XXX c10 XXX c00 */
           movss  24(pC0), xA2                  /*   0   0   0 c30 */
           movhps 16(pC0), xA2                  /* XXX c20   0 c30 */
           movups (pC0), xA0                    /* XXX c10 XXX c00 */
           shufps $0x28, xA2, xA0               /* c30 c20 c10 c00 */

           movss  24(pC0,ldc), xA3              /*   0   0   0 c31 */
           movhps 16(pC0,ldc), xA3              /* XXX c21   0 c31 */
           movups (pC0,ldc), xA1                /* XXX c11 XXX c01 */
           shufps $0x28, xA3, xA1               /* c31 c21 c11 c01 */
        #elif !defined(BETA0)
            movups (pC0), xA0                   /* c30 c20 c10 c00 */
            movups (pC0,ldc), xA1               /* c31 c21 c11 c01 */
        #endif
                    /* rC00 */  /* c00h c00g c00f c00e c00d c00c c00b c00a */
                    /* rC10 */  /* c10h c10g c10f c10e c10d c10c c10b c10a */
                    /* rC20 */  /* c20h c20g c20f c20e c20d c20c c20b c20a */
                    /* rC30 */  /* c30h c30g c30f c30e c30d c30c c30b c30a */
        vhaddps rC10, rC00, rC00
        #ifdef BETAX
           mulps xbeta, xA0
        #endif
        vhaddps rC11, rC01, rC01
        #ifdef BETAX
           mulps xbeta, xA1
        #endif
           /* c10gh  c10ef  c00gh  c00ef  c10cd  c10ab  c00cd  c00ab */
        vhaddps rC30, rC20, rC20
        vhaddps rC31, rC21, rC21
           /* c30gh  c30ef  c20gh  c20ef  c30cd  c30ab  c20cd  c20ab */
        vhaddps rC20, rC00, rC00
        vhaddps rC21, rC01, rC01
           /* c30e-h c20e-h c10e-h c00e-h c30a-d c20a-d c10a-d c00a-b */
        vextractf128 $1, rC00, xB0   /* c30e-h c20e-h c10e-h c00e-h */
        addps xB0, xC00              /* c30a-h c20a-h c10a-h c00a-h */
        vextractf128 $1, rC01, xB1   /* c31e-h c21e-h c11e-h c01e-h */
        addps xB1, xC01              /* c31a-h c21a-h c11a-h c01a-h */
/*
 *      Add in original C if BETA != 0
 */
        #ifndef BETA0
           addps xA0, xC00
           addps xA1, xC01
        #endif
/*
 *      Store answer to mem
 */
        #ifdef SCPLX
                          /* MEM = SEG c30 XXX c20 XXX c10 XXX c00 */
           movss xC00, (pC0)
           psrldq $4, xC00
           movss xC00, 8(pC0)
           psrldq $4, xC00
           movss xC00, 16(pC0)
           psrldq $4, xC00
           movss xC00, 24(pC0)

           movss xC01, (pC0,ldc)
           psrldq $4, xC01
           movss xC01, 8(pC0,ldc)
           psrldq $4, xC01
           movss xC01, 16(pC0,ldc)
           psrldq $4, xC01
           movss xC01, 24(pC0,ldc)
        #else
            movups xC00, (pC0)
            movups xC01, (pC0,ldc)
        #endif

        add     $4*CMUL(4), pC0

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
