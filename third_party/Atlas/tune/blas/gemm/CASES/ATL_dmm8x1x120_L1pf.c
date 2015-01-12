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
 * This routine hastily adapted to the 3rd gen Opteron from the Core2 kernel,
 * ATL_dmm8x1x120.c.  Main difference is the prefetch strategy, which fetches
 * to the L1 on the Opt3rdGen, and the handling of loads, where MOVUPD
 * is preferred over MOVLPD/MOVHPD pair.  We also turn on unaligned mem
 * MULPD/ADDPD.  This all makes the kernal about 8% faster, and allows us
 * to use a smaller block factor (for better application performance).
 * This kernel does not do well for complex.
 */
#ifndef ATL_GAS_x8664
   #error "This kernel requires x86-64 assembly!"
#endif
#if !defined(MB)
   #define MB 0
#endif
#if !defined(NB)
   #define NB 0
#endif
#if !defined(KB)
   #define KB 0
#endif
#if KB == 0
   #error "KB must be compile time constant!"
#endif
/*
 *Register usage
 */
#define pA0     %rcx
#define lda     %rbx
#define lda3    %rbp
#define lda5    %rdx
#define lda7    %rdi
#define pB0     %rax
#define pC0     %rsi
#define ldb     %r8
#define ldc     %r9
#define pfA     %r10
#define MM      %r11
#define NN      %r12
#define incAn   %r13
#define incCn   %r14
#define pfB     %r15

#define rA0     %xmm0
#define rB0     %xmm1
#define rC00    %xmm2
#define rC01    %xmm3
#define rC02    %xmm4
#define rC03    %xmm5
#define rC04    %xmm6
#define rC05    %xmm7
#define rC06    %xmm8
#define rC07    %xmm9
#define rC0     %xmm10
#define rC2     %xmm11
#define rC4     %xmm12
#define rC6     %xmm13
#define rb0     %xmm14
#define BETA    %xmm15

/*
 * Prefetch defines
 */
#if 1
   #define pref2(mem) prefetcht0        mem
   #define prefB(mem) prefetcht0        mem
   #define prefC(mem) prefetchw         mem
#else
   #define pref2(mem)
   #define prefB(mem)
   #define prefC(mem)
#endif

#define PFAINC -64
#define PFBINC -64

#ifdef DCPLX
   #define CMUL(arg_) 2*arg_
#else
   #define CMUL(arg_) arg_
#endif

#define ATL_Kiter(off_) \
	movapd	off_(pB0), rB0 ; \
	movapd	off_(pA0), rA0 ; \
	mulpd	rB0,rA0 ; \
	addpd	rA0,rC00 ; \
	movapd	off_(pA0,lda), rA0 ; \
	mulpd	rB0,rA0 ; \
	addpd	rA0,rC01 ; \
	movapd	off_(pA0,lda,2), rA0 ; \
	mulpd	rB0,rA0 ; \
	addpd	rA0,rC02 ; \
	movapd	off_(pA0,lda3), rA0 ; \
	mulpd	rB0,rA0 ; \
	addpd	rA0,rC03 ; \
	movapd	off_(pA0,lda,4), rA0 ; \
	mulpd	rB0,rA0 ; \
	addpd	rA0,rC04 ; \
	movapd	off_(pA0,lda5), rA0 ; \
	mulpd	rB0,rA0 ; \
	addpd	rA0,rC05 ; \
	movapd	off_(pA0,lda3,2), rA0 ; \
	mulpd	rB0,rA0 ; \
	addpd	rA0,rC06 ; \
	mulpd	off_(pA0,lda7), rB0 ; \
	addpd	rB0,rC07 ; \

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
ALIGN64
ATL_asmdecor(ATL_USERMM):
/*
 *      Enable use of unaligned memory addpd/mulpd instructions
 */
        stmxcsr -4(%rsp)
        orl     $131072, -4(%rsp)
        ldmxcsr -4(%rsp)

/*
 *      Save callee-saved iregs
 */
        movq    %rbp, -8(%rsp)
        movq    %rbx, -16(%rsp)
        movq    %r12, -24(%rsp)
        movq    %r13, -32(%rsp)
        movq    %r14, -40(%rsp)
        movq    %r15, -48(%rsp)
        #define MMOFF -56
#ifdef BETAX
        pshufd  $0x44, %xmm1, BETA
/*        pshufd  $0b01000100, %xmm1, BETA */
#endif
/*
 *      Setup input parameters
 */
        movq    %rdi, MM
        movq    %rsi, NN
        movq    %r9, pB0
        movq    %r8, lda
        movslq  8(%rsp), ldb
        movq    16(%rsp), pC0
        movslq  24(%rsp), incCn
/*
 *      ldx *= sizeof; lda3 = 3*lda, lda5=5*lda, lda7=7*lda
 */
#ifndef DCPLX
        movq    incCn, ldc
#endif
        shl     $3, lda
        shl     $3, ldb
        lea     (lda,lda,2), lda3
        lea     (lda,lda,4), lda5
        lea     (lda3,lda,4), lda7
/*
 *      pA3 += 128, pB0 += 128
 */
        sub     $-128, pA0
        sub     $-128, pB0
/*
 *      incAn = lda*M*sizeof, pfB = ldb*N*sizeof
 */
        movq    lda, incAn
        imulq   MM, incAn
        lea     (pA0,incAn,2), pfA
        movq    ldb, pfB
        imulq   NN, pfB
        lea     (pB0,pfB,2), pfB
/*
 *      incCn = (ldc - M)*sizeof
 */
        sub     MM, incCn
        movq    MM, MMOFF(%rsp)
#ifdef DCPLX
        shl     $4, incCn
#else
        shl     $3, incCn
#endif
ALIGN32
UNLOOP:
UMLOOP:
/*
 *      For complex, get C loaded to rCx regs; for real, put BETA in here, and
 *      we will use unaligned mulpd at end of loop, or unaligned addpd for BETA=1
 */
        #ifndef BETA0
           #ifdef DCPLX
              movsd    (pC0), rC0
              movsd    CMUL(16)(pC0), rC2
              movsd    CMUL(32)(pC0), rC4
              movsd    CMUL(48)(pC0), rC6
              movhpd    CMUL(8)(pC0), rC0
              movhpd    CMUL(24)(pC0), rC2
              movhpd    CMUL(40)(pC0), rC4
              movhpd    CMUL(56)(pC0), rC6
           #elif defined(BETAX)
              movapd    BETA, rC0
              movapd    BETA, rC2
              movapd    BETA, rC4
              movapd    BETA, rC6
           #endif
        #endif
/*KLOOP: */
	movapd	-128(pB0), rB0
                                        pref2((pfA))
                                        add     $PFAINC, pfA
	movapd	-128(pA0), rC00
	mulpd	rB0,rC00
	movapd	-128(pA0,lda), rC01
	mulpd	rB0,rC01
	movapd	-128(pA0,lda,2), rC02
	mulpd	rB0,rC02
	movapd	-128(pA0,lda3), rC03
	mulpd	rB0,rC03
	movapd	-128(pA0,lda,4), rC04
	mulpd	rB0,rC04
	movapd	-128(pA0,lda5), rC05
	mulpd	rB0,rC05
	movapd	-128(pA0,lda3,2), rC06
	mulpd	rB0,rC06
	movapd	-128(pA0,lda7), rC07
	mulpd	rB0,rC07

   #if KB > 2
	ATL_Kiter(-112)
   #endif
   #if KB > 4
	ATL_Kiter(-96)
   #endif
   #if KB > 6
	ATL_Kiter(-80)
   #endif
   #if KB > 8
	ATL_Kiter(-64)
   #endif
   #if KB > 10
	ATL_Kiter(-48)
   #endif
   #if KB > 12
	ATL_Kiter(-32)
   #endif
   #if KB > 14
	ATL_Kiter(-16)
   #endif
   #if KB > 16
	ATL_Kiter(0)
   #endif
   #if KB > 18
	ATL_Kiter(16)
   #endif
   #if KB > 20
	ATL_Kiter(32)
   #endif
   #if KB > 22
	ATL_Kiter(48)
   #endif
   #if KB > 24
	ATL_Kiter(64)
   #endif
   #if KB > 26
	ATL_Kiter(80)
   #endif
   #if KB > 28
	ATL_Kiter(96)
   #endif
   #if KB > 30
	ATL_Kiter(112)
   #endif
   #if KB > 32
	ATL_Kiter(128)
   #endif
   #if KB > 34
	ATL_Kiter(144)
   #endif
   #if KB > 36
	ATL_Kiter(160)
   #endif
   #if KB > 38
	ATL_Kiter(176)
   #endif
   #if KB > 40
	ATL_Kiter(192)
   #endif
   #if KB > 42
	ATL_Kiter(208)
   #endif
   #if KB > 44
	ATL_Kiter(224)
   #endif
   #if KB > 46
	ATL_Kiter(240)
   #endif
   #if KB > 48
	ATL_Kiter(256)
   #endif
   #if KB > 50
	ATL_Kiter(272)
   #endif
   #if KB > 52
	ATL_Kiter(288)
   #endif
   #if KB > 54
	ATL_Kiter(304)
   #endif
   #if KB > 56
	ATL_Kiter(320)
   #endif
   #if KB > 58
	ATL_Kiter(336)
   #endif
   #if KB > 60
	ATL_Kiter(352)
   #endif
   #if KB > 62
	ATL_Kiter(368)
   #endif
   #if KB > 64
	ATL_Kiter(384)
   #endif
   #if KB > 66
	ATL_Kiter(400)
   #endif
   #if KB > 68
	ATL_Kiter(416)
   #endif
   #if KB > 70
	ATL_Kiter(432)
   #endif
   #if KB > 72
	ATL_Kiter(448)
   #endif
   #if KB > 74
	ATL_Kiter(464)
   #endif
   #if KB > 76
	ATL_Kiter(480)
   #endif
   #if KB > 78
	ATL_Kiter(496)
   #endif
   #if KB > 80
	ATL_Kiter(512)
   #endif
   #if KB > 82
	ATL_Kiter(528)
   #endif
   #if KB > 84
	ATL_Kiter(544)
   #endif
   #if KB > 86
	ATL_Kiter(560)
   #endif
   #if KB > 88
	ATL_Kiter(576)
   #endif
   #if KB > 90
	ATL_Kiter(592)
   #endif
   #if KB > 92
	ATL_Kiter(608)
   #endif
   #if KB > 94
	ATL_Kiter(624)
   #endif
   #if KB > 96
	ATL_Kiter(640)
   #endif
   #if KB > 98
	ATL_Kiter(656)
   #endif
   #if KB > 100
	ATL_Kiter(672)
   #endif
   #if KB > 102
	ATL_Kiter(688)
   #endif
   #if KB > 104
	ATL_Kiter(704)
   #endif
   #if KB > 106
	ATL_Kiter(720)
   #endif
   #if KB > 108
	ATL_Kiter(736)
   #endif
   #if KB > 110
	ATL_Kiter(752)
   #endif
   #if KB > 112
	ATL_Kiter(768)
   #endif
   #if KB > 114
	ATL_Kiter(784)
   #endif
   #if KB > 116
	ATL_Kiter(800)
   #endif
   #if KB > 118
	ATL_Kiter(816)
   #endif
   #if KB > 120
	ATL_Kiter(832)
   #endif
   #if KB > 122
	ATL_Kiter(848)
   #endif
   #if KB > 124
	ATL_Kiter(864)
   #endif
   #if KB > 126
	ATL_Kiter(880)
   #endif
/* KLOOP end */

/*       jne KLOOP */
/*
 *      pC[0-8] = rC[0-8]
 */

        #ifdef BETAX
           #ifdef DCPLX
              mulpd BETA, rC0
           #else
              mulpd (pC0), rC0
           #endif
       #endif
        haddpd  rC01,rC00
                                prefB((pfB))
                                add     $PFBINC, pfB
                                lea     (pA0,lda,8), pA0
        #if defined(BETA1) && !defined(DCPLX)
           addpd   (pC0), rC00
        #elif !defined(BETA0)
           addpd   rC0, rC00
        #endif
        movlpd  rC00, (pC0)
        movhpd  rC00, CMUL(8)(pC0)

        #ifdef BETAX
           #ifdef DCPLX
              mulpd BETA, rC2
           #else
              mulpd 16(pC0), rC2
           #endif
       #endif
        haddpd  rC03,rC02
        #if defined(BETA1) && !defined(DCPLX)
           addpd   16(pC0), rC02
        #elif !defined(BETA0)
           addpd   rC2, rC02
        #endif
        movlpd  rC02, CMUL(16)(pC0)
        movhpd  rC02, CMUL(24)(pC0)

        #ifdef BETAX
           #ifdef DCPLX
              mulpd BETA, rC4
           #else
              mulpd 32(pC0), rC4
           #endif
       #endif
        haddpd  rC05,rC04
        #if defined(BETA1) && !defined(DCPLX)
           addpd   32(pC0), rC04
        #elif !defined(BETA0)
           addpd   rC4, rC04
        #endif
        movlpd  rC04, CMUL(32)(pC0)
        movhpd  rC04, CMUL(40)(pC0)

        #ifdef BETAX
           #ifdef DCPLX
              mulpd BETA, rC6
           #else
              mulpd 48(pC0), rC6
           #endif
       #endif
        haddpd  rC07,rC06
        #if defined(BETA1) && !defined(DCPLX)
           addpd   48(pC0), rC06
        #elif !defined(BETA0)
           addpd   rC6, rC06
        #endif
        movlpd  rC06, CMUL(48)(pC0)
        movhpd  rC06, CMUL(56)(pC0)

        add     $8*CMUL(8), pC0
        sub     $8, MM
        jnz     UMLOOP

        movq    MMOFF(%rsp), MM
        sub     incAn, pA0
        add     incCn, pC0
        add     ldb, pB0
        sub     $1, NN

        jnz     UNLOOP

#UDONE:
        movq    -8(%rsp), %rbp
        movq    -16(%rsp), %rbx
        movq    -24(%rsp), %r12
        movq    -32(%rsp), %r13
        movq    -40(%rsp), %r14
        movq    -48(%rsp), %r15
        ret
