/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2003 R. Clint Whaley
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
#ifndef ATL_GAS_x8664
   #error "This kernel requires x86-64 assembly!"
#endif
#include "atlas_asm.h"


#ifndef ATL_SSE2
   #error "This routine requires SSE2!"
#endif

#if !defined(MB)
   #define MB 0
#endif
#if !defined(NB)
   #define NB 0
#endif

#if !defined(KB) || (KB == 0)
   #error "KB must be a compile-time constant!"
#endif
#if (KB != 56)
   #error "KB must be 56!"
#endif
#if (NB/14)*14 != NB
   #error "NB must be multiple of 14!"
#endif
/*
 * Integer register usage shown be these defines
 */
#define pA      %rcx
#define pA10	%rbx
#define ldab	%rbp
#define mldab	%rdx
#define mldab5  %rax
#define pB      %rdi
#define pC      %rsi
#define stM     %r9
#define stN     %r11
#define pfA	%r8
#define pA5 	pA
#define pB0	pB
#define pAS     %r13
#define ldc     %r10
#define mldc    %r14
#define ldc3    %r15
/*       rax     used in 32/64 conversion */

#define NBso	(KB*8)
#define MBKBso  (MB*KB*8)
#define NB2so   (NBso+NBso)
#define NB3so   (NBso+NBso+NBso)
#define NB4so   (NBso+NBso+NBso+NBso)
#define NB5so   (NBso+NBso+NBso+NBso+NBso)
#define NB6so   (NB3so+NB3so)
#define NB7so   (NB3so+NB4so)
#define NB8so   (NB4so+NB4so)
#define NB9so   (NB4so+NB5so)
#define NB10so   (NB5so+NB5so)
#define NB11so   (NB6so+NB5so)
#define NB12so   (NB7so+NB5so)
#define NB13so   (NB8so+NB5so)
#define NB14so   (NB9so+NB5so)

/*
 * SSE2 register usage shown be these defines
 */
#define rA0	%xmm0
#define rB0	%xmm1
#define rC0	%xmm2
#define rC1	%xmm3
#define rC2	%xmm4
#define rC3	%xmm5
#define rC4	%xmm6
#define rC5	%xmm7
#define rC6	%xmm8
#define rC7	%xmm9
#define rC8	%xmm10
#define rC9	%xmm11
#define rC10	%xmm12
#define rC11	%xmm13
#define rC12	%xmm14
#define rC13	%xmm15
/*
 * Prefetch defines
 */
#if 1
#define pref2(mem) prefetcht1   mem
#define prefB(mem) prefetcht0   mem
#define prefC(mem) prefetchw	mem
#else
#define pref2(mem)
#define prefB(mem)
#define prefC(mem)
#endif

	.text
.global ATL_asmdecor(ATL_USERMM)
ATL_asmdecor(ATL_USERMM):
/*
 *      Save callee-saved iregs
 */
	movq	%rbp, -8(%rsp)
	movq	%rbx, -16(%rsp)
	movq	%r12, -32(%rsp)
	movq	%r13, -40(%rsp)
	movq	%r14, -48(%rsp)
	movq	%r15, -56(%rsp)
#define SOFF -64
#ifdef BETAX
   #define BOF -24
	movlpd	%xmm1, BOF(%rsp)
#endif
/*
 *      pA already comes in right reg;  load stN,
 *      Initialize pB = B; pC = C; NBso = NB * sizeof;
 */
	movq	%rsi, stN
	movq	%rdi, %r12
	movq	%r9, pB
			prefB((pB))
			prefB(64(pB))
	movq	16(%rsp), pC
			prefC((pC))
/*
 *      stM = M/14; stN = N
 */
#if MB != 0
	movq	$MB, stM
#else
	movq	%r12, stM
#endif
/*
 *      convert ldc to 64 bits, and mul by size
 */
	movl	24(%rsp), %eax
	cltq
	movq	%rax, ldc
#ifdef DREAL
	shl	$3, ldc
#else
	shl	$4, ldc
#endif
/*
 *      At this point, pA5 has pA, pB0 has pB, stN has N, stM has M; swap
 *      them so that we can reverse loops
 */
        movq    pA5, mldc
        movq    pB0, pA5
        movq    mldc, pB0
			prefB(128(pB))
        movq    stN, mldc
        movq    stM, stN
        movq    mldc, stM
/*
 *      pfA = pA + M*KBso
 */
	movq	stM, pfA
	imulq	$NBso, pfA
	addq	pA5, pfA
/*
 *      Calculate and store incCn = sizeof*(N*ldc - 1)
 */
        movq    stM, mldc
        imulq   ldc, mldc
#ifdef DREAL
        subq    $8, mldc
#else
        subq    $16, mldc
#endif
        movq    mldc, SOFF(%rsp)
/*
 *      mldc = -ldc; ldc3 = ldc*3, pC = pC + ldc*2
 */
        movq    ldc, mldc
        neg     mldc
        lea     (ldc,ldc,2), ldc3
        lea     (pC, ldc,2), pC

	addq	$120, pA5
	addq	$120, pB0
	movq	$KB*8, ldab
	movq	$-KB*5*8, mldab5
	movq	$-KB*8, mldab
	subq	mldab5, pA5
	lea	KB*8(pA5, ldab,4), pA10
	movq	pA10, pAS

	movq	stM, %r12
UNLOOP:
#if NB == 0
	movq	%r12, stM
        sub     $14, stM
        jz      UMLOOPCU
#else
        movq    $NB-14, stM
#endif
#if NB != 14
UMLOOP:
/*
 *      rC[0-13] = pC[0-13] * beta
 */
	ALIGN16
/*UKLOOP: */
#ifdef BETA1
	movapd	0-120(pA10,mldab5,2), rC0
	movapd	0-120(pB0), rB0
	mulpd	rB0, rC0
	addsd	(pC,mldc,2), rC0
	movapd	0-120(pA5, mldab,4), rC1
	mulpd	rB0, rC1
	addsd	(pC,mldc), rC1
	movapd	0-120(pA10, mldab,8), rC2
	mulpd	rB0, rC2
	addsd	(pC), rC2
	movapd	0-120(pA5, mldab,2), rC3
	mulpd	rB0, rC3
	addsd	(pC,ldc), rC3
	movapd	0-120(pA5, mldab), rC4
	mulpd	rB0, rC4
	addsd	(pC,ldc,2), rC4
	movapd	0-120(pA5), rC5
	mulpd	rB0, rC5
	addsd	(pC,ldc3), rC5
	movapd	0-120(pA5, ldab), rC6
	mulpd	rB0, rC6
	addsd	(pC,ldc,4), rC6
                                                addq    ldc, pC
	movapd	0-120(pA5, ldab,2), rC7
                                                lea     (pC,ldc3,2), pC
	mulpd	rB0, rC7

	addsd	(pC,mldc,2), rC7
	movapd	0-120(pA10, mldab,2), rC8
	mulpd	rB0, rC8
	addsd	(pC,mldc), rC8
	movapd	0-120(pA5,ldab,4), rC9
	mulpd	rB0, rC9
	addsd	(pC), rC9
	movapd	0-120(pA10), rC10
	mulpd	rB0, rC10
	addsd	(pC,ldc), rC10
	movapd	0-120(pA10,ldab), rC11
	mulpd	rB0, rC11
	addsd	(pC,ldc,2), rC11
	movapd	0-120(pA10,ldab,2), rC12
	mulpd	rB0, rC12
	addsd	(pC,ldc3), rC12
	movapd	0-120(pA5,ldab,8), rC13
	mulpd	rB0, rC13
	addsd	(pC,ldc,4), rC13
#elif defined(BETA0)
	movapd	0-120(pA10,mldab5,2), rC0
	movapd	0-120(pB0), rC13
	mulpd	rC13, rC0
	movapd	0-120(pA5, mldab,4), rC1
	mulpd	rC13, rC1
	movapd	0-120(pA10, mldab,8), rC2
	mulpd	rC13, rC2
	movapd	0-120(pA5, mldab,2), rC3
	mulpd	rC13, rC3
	movapd	0-120(pA5, mldab), rC4
	mulpd	rC13, rC4
	movapd	0-120(pA5), rC5
	mulpd	rC13, rC5
	movapd	0-120(pA5, ldab), rC6
	mulpd	rC13, rC6
	movapd	0-120(pA5, ldab,2), rC7
	mulpd	rC13, rC7
	movapd	0-120(pA10, mldab,2), rC8
	mulpd	rC13, rC8
	movapd	0-120(pA5,ldab,4), rC9
	mulpd	rC13, rC9
	movapd	0-120(pA10), rC10
	mulpd	rC13, rC10
	movapd	0-120(pA10,ldab), rC11
	mulpd	rC13, rC11
	movapd	0-120(pA10,ldab,2), rC12
	mulpd	rC13, rC12
	mulpd 	0-120(pA5,ldab,8), rC13
#else
	movsd	BOF(%rsp), rC0
	movapd	rC0, rC1
	movapd	rC0, rC2
	movapd	rC0, rC3
	movapd	rC0, rC4
	movapd	rC0, rC5
	movapd	rC0, rC6
	movapd	rC0, rC7
	movapd	rC0, rC8
	movapd	rC0, rC9
	movapd	rC0, rC10
	movapd	rC0, rC11
	movapd	rC0, rC12
	movapd	rC0, rC13
	mulsd	(pC,mldc,2), rC0
	mulsd	(pC,mldc), rC1
	mulsd	(pC), rC2
	mulsd	(pC,ldc), rC3
	mulsd	(pC,ldc,2), rC4
	mulsd	(pC,ldc3), rC5
	mulsd	(pC,ldc,4), rC6
                                                add     ldc, pC
                                                lea     (pC,ldc3,2), pC

	mulsd	(pC,mldc,2), rC7
	mulsd	(pC,mldc), rC8
	mulsd	(pC), rC9
	mulsd	(pC,ldc), rC10
	mulsd	(pC,ldc,2), rC11
	mulsd	(pC,ldc3), rC12
	mulsd	(pC,ldc,4), rC13

	movapd	0-120(pA10,mldab5,2), rA0
	movapd	0-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
                                        lea     (pC,mldc,8), pC
	movapd	0-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
                                        addq    ldc, pC
	movapd	0-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	0-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	0-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	0-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	0-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	0-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	0-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	0-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	0-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	0-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	0-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	0-120(pA5,ldab,8), rB0
	addpd	rB0, rC13
#endif

	movapd	16-120(pA10,mldab5,2), rA0
	movapd	16-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
#ifdef BETA1
                                        lea     (pC,mldc,8), pC
#endif
	movapd	16-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
#ifdef BETA1
                                        addq    ldc, pC
#endif
	movapd	16-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	16-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	16-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	16-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	16-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	16-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	16-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	16-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	16-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	16-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	16-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	16-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	32-120(pA10,mldab5,2), rA0
	movapd	32-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	32-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	32-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	32-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	32-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	32-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	32-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	32-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	32-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	32-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	32-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	32-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	32-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	32-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	48-120(pA10,mldab5,2), rA0
	movapd	48-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	48-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	48-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	48-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	48-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	48-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	48-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	48-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	48-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	48-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	48-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	48-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	48-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	48-120(pA5,ldab,8), rB0
	addpd	rB0, rC13
#ifndef DREAL
						pref2((pfA))
						pref2(64(pfA))
#endif


	movapd	64-120(pA10,mldab5,2), rA0
	movapd	64-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	64-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	64-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	64-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	64-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	64-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	64-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	64-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	64-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	64-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	64-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	64-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	64-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	64-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	80-120(pA10,mldab5,2), rA0
	movapd	80-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	80-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	80-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	80-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	80-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	80-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	80-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	80-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	80-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	80-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	80-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	80-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	80-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	80-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	96-120(pA10,mldab5,2), rA0
	movapd	96-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	96-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	96-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	96-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	96-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	96-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	96-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	96-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	96-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	96-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	96-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	96-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	96-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	96-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	112-120(pA10,mldab5,2), rA0
	movapd	112-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	112-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	112-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	112-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	112-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	112-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	112-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	112-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	112-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	112-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	112-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	112-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	112-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	112-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	128-120(pA10,mldab5,2), rA0
	movapd	128-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	128-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	128-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	128-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	128-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	128-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	128-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	128-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	128-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	128-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	128-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	128-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
                                                lea     (pC,ldc3,4), pC
	movapd	128-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	128-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

                                                prefC((pC))
                                                prefC((pC,ldc))
	movapd	144-120(pA10,mldab5,2), rA0
	movapd	144-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	144-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	144-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	144-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	144-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	144-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	144-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	144-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	144-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	144-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	144-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	144-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	144-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	144-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	160-120(pA10,mldab5,2), rA0
	movapd	160-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	160-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	160-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	160-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	160-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	160-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	160-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	160-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	160-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	160-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	160-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	160-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	160-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	160-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

                                                prefC((pC,ldc,2))
                                                prefC((pC,ldc3,))
	movapd	176-120(pA10,mldab5,2), rA0
	movapd	176-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	176-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	176-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	176-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	176-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	176-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	176-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	176-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	176-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	176-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	176-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	176-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	176-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	176-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	192-120(pA10,mldab5,2), rA0
	movapd	192-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	192-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	192-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	192-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	192-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	192-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	192-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	192-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	192-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	192-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	192-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	192-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	192-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	192-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

                                                prefC((pC,ldc,4))
                                                prefC((pC,ldc3,2))
	movapd	208-120(pA10,mldab5,2), rA0
	movapd	208-120(pB0), rB0
	mulpd	rB0, rA0
				addq	$224, pB0
	addpd	rA0, rC0
	movapd	208-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	208-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	208-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	208-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	208-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	208-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	208-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	208-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	208-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	208-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	208-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	208-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
				addq	$224, pA10
	addpd	rA0, rC12
	mulpd	208-120(pA5,ldab,8), rB0
	addpd	rB0, rC13
				addq	$224, pA5

	movapd	0-120(pA10,mldab5,2), rA0
	movapd	0-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	0-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	0-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	0-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	0-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	0-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	0-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	0-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	0-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	0-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	0-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	0-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	0-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	0-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

                                                prefC((pC,ldc,8))
                                                prefC((pC,ldc3,4))
	movapd	16-120(pA10,mldab5,2), rA0
	movapd	16-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	16-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	16-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	16-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	16-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	16-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	16-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	16-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	16-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	16-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	16-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	16-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	16-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	16-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	32-120(pA10,mldab5,2), rA0
	movapd	32-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	32-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	32-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	32-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	32-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	32-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	32-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	32-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	32-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	32-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	32-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	32-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	32-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	32-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	48-120(pA10,mldab5,2), rA0
	movapd	48-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	48-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	48-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	48-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	48-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	48-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	48-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	48-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	48-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	48-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	48-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
                                                lea     (pC,ldc,8), pC
	movapd	48-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
                                                addq    ldc, pC
	movapd	48-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	48-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

                                                prefC((pC,mldc,2))
                                                prefC((pC))
	movapd	64-120(pA10,mldab5,2), rA0
	movapd	64-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	64-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	64-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	64-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	64-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	64-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	64-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	64-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	64-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	64-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	64-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	64-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	64-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	64-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	80-120(pA10,mldab5,2), rA0
	movapd	80-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	80-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	80-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	80-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	80-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	80-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	80-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	80-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	80-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	80-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	80-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	80-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	80-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	80-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	96-120(pA10,mldab5,2), rA0
	movapd	96-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	96-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	96-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	96-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	96-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	96-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	96-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	96-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	96-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	96-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	96-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	96-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	96-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	96-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	112-120(pA10,mldab5,2), rA0
	movapd	112-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	112-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	112-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	112-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	112-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	112-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	112-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	112-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	112-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	112-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	112-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	112-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	112-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	112-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

                                                prefC((pC,ldc))
                                                prefC((pC,ldc,2))
	movapd	128-120(pA10,mldab5,2), rA0
	movapd	128-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	128-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	128-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	128-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	128-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	128-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	128-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	128-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	128-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	128-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	128-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	128-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	128-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	128-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	144-120(pA10,mldab5,2), rA0
	movapd	144-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	144-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	144-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	144-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	144-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	144-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	144-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	144-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	144-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	144-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	144-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	144-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	144-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	144-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

                                                prefC((pC,ldc3))
	movapd	160-120(pA10,mldab5,2), rA0
	movapd	160-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
                                                lea     (pC,mldc,8), pC
	movapd	160-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
                                                lea     (pC,mldc,8), pC
	movapd	160-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
                                                lea     (pC,mldc,4), pC
	movapd	160-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
                                                subq    ldc, pC
	movapd	160-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	160-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	160-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	160-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	160-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	160-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	160-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	160-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	160-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	160-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	176-120(pA10,mldab5,2), rA0
	movapd	176-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	176-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	176-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	176-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	176-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	176-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	176-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	176-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	176-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	176-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	176-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	176-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	176-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	176-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	192-120(pA10,mldab5,2), rA0
	movapd	192-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	192-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	192-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	192-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	192-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	192-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	192-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	192-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	192-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	192-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	192-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	192-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	192-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	192-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	208-120(pA10,mldab5,2), rA0
	movapd	208-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	208-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	208-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	208-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	208-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	208-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	208-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	208-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	208-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	208-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	208-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	208-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	208-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	208-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

/*
 *      While (pB != stK);
 */
/*	cmp	pB, stK */
/*	jne	UKLOOP */

/*
 *      Get these bastard things summed up correctly
 */
                                        /* rC0 = c0a  c0b */
                                        /* rC1 = c1a  c1b */
                                        /* rC2 = c2a  c2b */
                                        /* rC3 = c3a  c3b */
/* */
                                        /* rC4 = c4a  c4b */
                                        /* rC5 = c5a  c5b */
                                        /* rC6 = c6a  c6b */
                                        /* rC7 = c7a  c7b */
   #ifdef DREAL
				pref2((pfA))
				pref2(64(pfA))
   #endif
        movapd          rC0, rA0
        movapd          rC4, rB0
        unpcklpd        rC1, rC0        /* rC0 = c0a  c1a */
        unpcklpd        rC5, rC4        /* rC4 = c4a  c5a */
        unpckhpd        rC1, rA0        /* rA0 = c0b  c1b */
        unpckhpd        rC5, rB0        /* rB0 = c4b  c5b */
        addpd           rA0, rC0        /* rC0 = c0ab c1ab */
        addpd           rB0, rC4        /* rC4 = c4ab c5ab */
        movapd          rC2, rA0
        movapd          rC6, rB0
        unpcklpd        rC3, rC2        /* rC2 = c2a  c3a */
        unpcklpd        rC7, rC6        /* rC6 = c6a  c7a */
					addq	$150, pfA
        unpckhpd        rC3, rA0        /* rA0 = c2b  c3b */
					pref2(-22(pfA))
        unpckhpd        rC7, rB0        /* rB0 = c6b  c7b */
        addpd           rA0, rC2        /* rC2 = c2ab c3ab */
        addpd           rB0, rC6        /* rC6 = c6ab c7ab */
/* */
                                        /* rC8 = c08a  c08b */
                                        /* rC9 = c09a  c09b */
                                        /* rC10 = c10a  c10b */
                                        /* rC11 = c11a  c11b */
/* */
					/* rC12 = c12a c12b */
					/* rC13 = c13a c13b */
        movapd          rC8, rA0
        movapd          rC10, rC1
	movapd		rC12, rC3
        unpcklpd        rC9, rC8        /* rC8 = c08a  c09a */
		addq	$NB14so-224, pA5
        unpcklpd        rC11, rC10      /* rC10 = c10a  c11a */
		addq	$NB14so-224, pA10
	unpcklpd	rC13, rC12	/* rC12 = c12a c13a */
        unpckhpd        rC11, rC1       /* rC1 = c10b  c11b */
        unpckhpd        rC9, rA0        /* rA0 = c08b  c09b */
	unpckhpd	rC13, rC3	/* rC3  = c12b c13b */
        addpd           rA0, rC8        /* rC8 = c08ab c09ab */
		subq	$224, pB0
        addpd           rC1, rC10       /* rC10 = c10ab c11ab */
	addpd		rC3, rC12	/* rc12 = c12ab c13ab */
/*
 *      Write results back to C
 */
        movlpd  rC0, (pC,mldc,2)
        movhpd  rC0, (pC, mldc)
        movlpd  rC2, (pC)
        movhpd  rC2, (pC,ldc)
        movlpd  rC4, (pC,ldc,2)
        movhpd  rC4, (pC,ldc3)
        movlpd  rC6, (pC,ldc,4)
        subq    ldc, pC
        lea     (pC,ldc,8), pC

        movhpd  rC6, (pC,mldc,2)
        movlpd  rC8, (pC,mldc)
        movhpd  rC8, (pC)
        movlpd  rC10, (pC,ldc)
        movhpd  rC10, (pC,ldc,2)
        movlpd  rC12, (pC,ldc3)
        movhpd  rC12, (pC,ldc,4)
        add     ldc, pC
        lea     (pC,ldc3,2), pC
/*
 *      pC += 14;  pA += 14*NB; pB -= NB;
 */
/*
 *      while (pA != stM);
 */
	subq	$14, stM
	jne	UMLOOP
#endif

/*
 *      Last iteration of M-loop unrolled to prefetch next col of B
 */
#if NB == 0
UMLOOPCU:
#endif
/*UKLOOP: */

#ifdef BETA1
	movapd	0-120(pA10,mldab5,2), rC0
	movapd	0-120(pB0), rB0
	mulpd	rB0, rC0
	addsd	(pC,mldc,2), rC0
	movapd	0-120(pA5, mldab,4), rC1
	mulpd	rB0, rC1
	addsd	(pC,mldc), rC1
	movapd	0-120(pA10, mldab,8), rC2
	mulpd	rB0, rC2
	addsd	(pC), rC2
	movapd	0-120(pA5, mldab,2), rC3
	mulpd	rB0, rC3
	addsd	(pC,ldc), rC3
	movapd	0-120(pA5, mldab), rC4
	mulpd	rB0, rC4
	addsd	(pC,ldc,2), rC4
	movapd	0-120(pA5), rC5
	mulpd	rB0, rC5
	addsd	(pC,ldc3), rC5
	movapd	0-120(pA5, ldab), rC6
	mulpd	rB0, rC6
	addsd	(pC,ldc,4), rC6
                                                addq    ldc, pC
	movapd	0-120(pA5, ldab,2), rC7
                                                lea     (pC,ldc3,2), pC
	mulpd	rB0, rC7

	addsd	(pC,mldc,2), rC7
	movapd	0-120(pA10, mldab,2), rC8
	mulpd	rB0, rC8
	addsd	(pC,mldc), rC8
	movapd	0-120(pA5,ldab,4), rC9
	mulpd	rB0, rC9
	addsd	(pC), rC9
	movapd	0-120(pA10), rC10
	mulpd	rB0, rC10
	addsd	(pC,ldc), rC10
	movapd	0-120(pA10,ldab), rC11
	mulpd	rB0, rC11
	addsd	(pC,ldc,2), rC11
	movapd	0-120(pA10,ldab,2), rC12
	mulpd	rB0, rC12
	addsd	(pC,ldc3), rC12
	movapd	0-120(pA5,ldab,8), rC13
	mulpd	rB0, rC13
	addsd	(pC,ldc,4), rC13
#elif defined(BETA0)
	movapd	0-120(pA10,mldab5,2), rC0
	movapd	0-120(pB0), rC13
	mulpd	rC13, rC0
	movapd	0-120(pA5, mldab,4), rC1
	mulpd	rC13, rC1
	movapd	0-120(pA10, mldab,8), rC2
	mulpd	rC13, rC2
	movapd	0-120(pA5, mldab,2), rC3
	mulpd	rC13, rC3
	movapd	0-120(pA5, mldab), rC4
	mulpd	rC13, rC4
	movapd	0-120(pA5), rC5
	mulpd	rC13, rC5
	movapd	0-120(pA5, ldab), rC6
	mulpd	rC13, rC6
	movapd	0-120(pA5, ldab,2), rC7
	mulpd	rC13, rC7
	movapd	0-120(pA10, mldab,2), rC8
	mulpd	rC13, rC8
	movapd	0-120(pA5,ldab,4), rC9
	mulpd	rC13, rC9
	movapd	0-120(pA10), rC10
	mulpd	rC13, rC10
	movapd	0-120(pA10,ldab), rC11
	mulpd	rC13, rC11
	movapd	0-120(pA10,ldab,2), rC12
	mulpd	rC13, rC12
	mulpd 	0-120(pA5,ldab,8), rC13
#else
	movsd	BOF(%rsp), rC0
	movapd	rC0, rC1
	movapd	rC0, rC2
	movapd	rC0, rC3
	movapd	rC0, rC4
	movapd	rC0, rC5
	movapd	rC0, rC6
	movapd	rC0, rC7
	movapd	rC0, rC8
	movapd	rC0, rC9
	movapd	rC0, rC10
	movapd	rC0, rC11
	movapd	rC0, rC12
	movapd	rC0, rC13
	mulsd	(pC,mldc,2), rC0
	mulsd	(pC,mldc), rC1
	mulsd	(pC), rC2
	mulsd	(pC,ldc), rC3
	mulsd	(pC,ldc,2), rC4
	mulsd	(pC,ldc3), rC5
	mulsd	(pC,ldc,4), rC6
                                                add     ldc, pC
                                                lea     (pC,ldc3,2), pC

	mulsd	(pC,mldc,2), rC7
	mulsd	(pC,mldc), rC8
	mulsd	(pC), rC9
	mulsd	(pC,ldc), rC10
	mulsd	(pC,ldc,2), rC11
	mulsd	(pC,ldc3), rC12
	mulsd	(pC,ldc,4), rC13

	movapd	0-120(pA10,mldab5,2), rA0
	movapd	0-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
                                        lea     (pC,mldc,8), pC
	movapd	0-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
                                        addq    ldc, pC
	movapd	0-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	0-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	0-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	0-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	0-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	0-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	0-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	0-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	0-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	0-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	0-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	0-120(pA5,ldab,8), rB0
	addpd	rB0, rC13
#endif
						prefB(-120(pB0,ldab))
						prefB(64-120(pB0,ldab))
	movapd	16-120(pA10,mldab5,2), rA0
	movapd	16-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
#ifdef BETA1
                                        lea     (pC,mldc,8), pC
#endif
	movapd	16-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
#ifdef BETA1
                                        addq    ldc, pC
#endif
	movapd	16-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	16-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	16-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	16-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	16-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	16-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	16-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	16-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	16-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	16-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	16-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	16-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	32-120(pA10,mldab5,2), rA0
	movapd	32-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	32-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	32-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	32-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	32-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	32-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	32-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	32-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	32-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	32-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	32-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	32-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	32-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	32-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	48-120(pA10,mldab5,2), rA0
	movapd	48-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	48-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	48-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	48-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	48-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	48-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	48-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	48-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	48-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	48-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	48-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	48-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	48-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	48-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	64-120(pA10,mldab5,2), rA0
	movapd	64-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	64-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	64-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	64-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	64-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	64-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	64-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	64-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	64-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	64-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	64-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	64-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	64-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	64-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	80-120(pA10,mldab5,2), rA0
	movapd	80-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	80-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	80-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	80-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	80-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	80-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	80-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	80-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	80-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	80-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	80-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	80-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	80-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	80-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	96-120(pA10,mldab5,2), rA0
	movapd	96-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	96-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	96-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	96-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	96-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	96-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	96-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	96-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	96-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	96-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	96-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	96-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	96-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	96-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	112-120(pA10,mldab5,2), rA0
	movapd	112-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	112-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	112-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	112-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	112-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	112-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	112-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	112-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	112-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	112-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	112-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	112-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	112-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	112-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	128-120(pA10,mldab5,2), rA0
	movapd	128-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	128-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	128-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	128-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	128-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	128-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	128-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	128-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	128-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	128-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	128-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	128-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	128-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	128-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

						prefB(128-120(pB0,ldab))
						prefB(192-120(pB0,ldab))
	movapd	144-120(pA10,mldab5,2), rA0
	movapd	144-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	144-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	144-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	144-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	144-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	144-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	144-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	144-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	144-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	144-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	144-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	144-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	144-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	144-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	160-120(pA10,mldab5,2), rA0
	movapd	160-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	160-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	160-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	160-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	160-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	160-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	160-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	160-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	160-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	160-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	160-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	160-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	160-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	160-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	176-120(pA10,mldab5,2), rA0
	movapd	176-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	176-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	176-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	176-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	176-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	176-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	176-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	176-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	176-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	176-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	176-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	176-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	176-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	176-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	192-120(pA10,mldab5,2), rA0
	movapd	192-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	192-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	192-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	192-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	192-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	192-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	192-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	192-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	192-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	192-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	192-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	192-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	192-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	192-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	208-120(pA10,mldab5,2), rA0
	movapd	208-120(pB0), rB0
	mulpd	rB0, rA0
			addq	$224, pB0
	addpd	rA0, rC0
	movapd	208-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	208-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	208-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	208-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	208-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	208-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	208-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	208-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	208-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	208-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	208-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	208-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
			addq	$224, pA10
	addpd	rA0, rC12
	mulpd	208-120(pA5,ldab,8), rB0
	addpd	rB0, rC13
			addq	$224, pA5

/* UKLOOP / 2 */

	movapd	0-120(pA10,mldab5,2), rA0
	movapd	0-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	0-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	0-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	0-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	0-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	0-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	0-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	0-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	0-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	0-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	0-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	0-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	0-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	0-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

						prefB(32-120(pB0,ldab))
						prefB(96-120(pB0,ldab))
	movapd	16-120(pA10,mldab5,2), rA0
	movapd	16-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	16-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	16-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	16-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	16-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	16-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	16-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	16-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	16-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	16-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	16-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	16-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	16-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	16-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	32-120(pA10,mldab5,2), rA0
	movapd	32-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	32-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	32-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	32-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	32-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	32-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	32-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	32-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	32-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	32-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	32-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	32-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	32-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	32-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	48-120(pA10,mldab5,2), rA0
	movapd	48-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	48-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	48-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	48-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	48-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	48-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	48-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	48-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	48-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	48-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	48-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	48-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	48-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	48-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	64-120(pA10,mldab5,2), rA0
	movapd	64-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	64-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	64-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	64-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	64-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	64-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	64-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	64-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	64-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	64-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	64-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	64-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	64-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	64-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	80-120(pA10,mldab5,2), rA0
	movapd	80-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	80-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	80-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	80-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	80-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	80-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	80-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	80-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	80-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	80-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	80-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	80-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	80-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	80-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	96-120(pA10,mldab5,2), rA0
	movapd	96-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	96-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	96-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	96-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	96-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	96-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	96-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	96-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	96-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	96-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	96-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	96-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	96-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	96-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	112-120(pA10,mldab5,2), rA0
	movapd	112-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	112-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	112-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	112-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	112-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	112-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	112-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	112-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	112-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	112-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	112-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	112-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	112-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	112-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	128-120(pA10,mldab5,2), rA0
	movapd	128-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	128-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	128-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	128-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	128-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	128-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	128-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	128-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	128-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	128-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	128-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	128-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	128-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	128-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	144-120(pA10,mldab5,2), rA0
	movapd	144-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	144-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	144-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	144-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	144-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	144-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	144-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	144-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	144-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	144-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	144-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	144-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	144-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	144-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	160-120(pA10,mldab5,2), rA0
	movapd	160-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	160-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	160-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	160-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	160-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	160-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	160-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	160-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	160-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	160-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	160-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	160-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	160-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	160-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	176-120(pA10,mldab5,2), rA0
	movapd	176-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	176-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	176-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	176-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	176-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	176-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	176-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	176-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	176-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	176-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	176-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	176-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	176-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	176-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	192-120(pA10,mldab5,2), rA0
	movapd	192-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	192-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	192-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	192-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	192-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	192-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	192-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	192-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	192-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	192-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	192-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	192-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	192-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	192-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

	movapd	208-120(pA10,mldab5,2), rA0
	movapd	208-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	208-120(pA5, mldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	208-120(pA10, mldab,8), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	208-120(pA5, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	208-120(pA5, mldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	208-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	208-120(pA5, ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	208-120(pA5, ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	208-120(pA10, mldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	208-120(pA5,ldab,4), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	208-120(pA10), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	208-120(pA10,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	208-120(pA10,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	mulpd	208-120(pA5,ldab,8), rB0
	addpd	rB0, rC13

/*
 *      While (pB != stK);
 */
/*	cmp	pB, stK */
/*	jne	UKLOOP */

/*
 *      Get these bastard things summed up correctly
 */
                                        /* rC0 = c0a  c0b */
                                        /* rC1 = c1a  c1b */
                                        /* rC2 = c2a  c2b */
                                        /* rC3 = c3a  c3b */
/* */
                                        /* rC4 = c4a  c4b */
                                        /* rC5 = c5a  c5b */
                                        /* rC6 = c6a  c6b */
                                        /* rC7 = c7a  c7b */
        movapd          rC0, rA0
        movapd          rC4, rB0
        unpcklpd        rC1, rC0        /* rC0 = c0a  c1a */
        unpcklpd        rC5, rC4        /* rC4 = c4a  c5a */
        unpckhpd        rC1, rA0        /* rA0 = c0b  c1b */
        unpckhpd        rC5, rB0        /* rB0 = c4b  c5b */
        addpd           rA0, rC0        /* rC0 = c0ab c1ab */
        addpd           rB0, rC4        /* rC4 = c4ab c5ab */
        movapd          rC2, rA0
        movapd          rC6, rB0
        unpcklpd        rC3, rC2        /* rC2 = c2a  c3a */
        unpcklpd        rC7, rC6        /* rC6 = c6a  c7a */
				prefB(160-120(pB,ldab))
        unpckhpd        rC3, rA0        /* rA0 = c2b  c3b */
        unpckhpd        rC7, rB0        /* rB0 = c6b  c7b */
        addpd           rA0, rC2        /* rC2 = c2ab c3ab */
        addpd           rB0, rC6        /* rC6 = c6ab c7ab */
/* */
                                        /* rC8 = c08a  c08b */
                                        /* rC9 = c09a  c09b */
                                        /* rC10 = c10a  c10b */
                                        /* rC11 = c11a  c11b */
/* */
					/* rC12 = c12a c12b */
					/* rC13 = c13a c13b */
        movapd          rC8, rA0
        movapd          rC10, rC1
	movapd		rC12, rC3
        unpcklpd        rC9, rC8        /* rC8 = c08a  c09a */
	movq	pAS, pA5
	addq	mldab5, pA5
/*	subq	$MBKBso-NB14so+224, pA5 */
        unpcklpd        rC11, rC10      /* rC10 = c10a  c11a */
	movq	pAS, pA10
/*	subq	$MBKBso-NB14so+224, pA10 */
	unpcklpd	rC13, rC12	/* rC12 = c12a c13a */
	addq	$NBso-224, pB0
        unpckhpd        rC11, rC1       /* rC1 = c10b  c11b */
        unpckhpd        rC9, rA0        /* rA0 = c08b  c09b */
	unpckhpd	rC13, rC3	/* rC3  = c12b c13b */
        addpd           rA0, rC8        /* rC8 = c08ab c09ab */
        addpd           rC1, rC10       /* rC10 = c10ab c11ab */
	addpd		rC3, rC12	/* rc12 = c12ab c13ab */
/*
 *      Write results back to C
 */
        movlpd  rC0, (pC,mldc,2)
        movhpd  rC0, (pC, mldc)
        movlpd  rC2, (pC)
        movhpd  rC2, (pC,ldc)
        movlpd  rC4, (pC,ldc,2)
        movhpd  rC4, (pC,ldc3)
        movlpd  rC6, (pC,ldc,4)
        subq    ldc, pC
        lea     (pC,ldc,8), pC

        movhpd  rC6, (pC,mldc,2)
        movlpd  rC8, (pC,mldc)
        movhpd  rC8, (pC)
        movlpd  rC10, (pC,ldc)
        movhpd  rC10, (pC,ldc,2)
        movlpd  rC12, (pC,ldc3)
        movhpd  rC12, (pC,ldc,4)
        add     ldc, pC
        lea     (pC,ldc3,2), pC
/*
 *      pC += 14;  pA += 14*NB; pB -= NB;
 */
/*
 *      while (pA != stM);
 */
/*	subq	$1, stM */
/*	jne	UMLOOP */
/*
 *      pC += incCn;  pA -= NBNB;  pB += NB;
 */
        subq    SOFF(%rsp), pC
/*
 *      while (pB != stN);
 */
	sub	$1, stN
	jne	UNLOOP

/*
 *      Restore callee-saved iregs
 */
DONE:
	movq	-8(%rsp), %rbp
	movq	-16(%rsp), %rbx
	movq	-32(%rsp), %r12
	movq	-40(%rsp), %r13
	movq	-48(%rsp), %r14
	movq	-56(%rsp), %r15
	ret
