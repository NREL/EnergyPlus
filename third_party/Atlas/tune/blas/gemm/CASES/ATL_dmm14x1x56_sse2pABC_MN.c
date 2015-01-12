/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2002 R. Clint Whaley
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
#if (MB/14)*14 != MB
   #error "MB must be multiple of 14!"
#endif
#ifdef DREAL
   #define CMUL(arg_) arg_
#else
   #define CMUL(arg_) 2*arg_
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
#define incCn   %r10
#define stM     %r9
#define stN     %r11
#define pfA	%r8
#define pA5 	pA
#define pB0	pB
#define pAS     %r13
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
#define prefC(mem) prefetchw    mem
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
			prefC(64(pC))
/*
 *      stM = M/14; stN = N
 */
#if MB != 0
	movq	$MB, stM
#else
	movq	%r12, stM
#endif
/*
 *      pfA = pA + NBNBso;  stN = pB + NBNBso;
 */
	movq	stM, pfA
	imulq	$NBso, pfA
/*	movq	$MBKBso, pfA */
	addq	pA5, pfA
			prefB(128(pB))
/*
 *      convert ldc to 64 bits, and then set incCn = (ldc - MB)*sizeof
 */
	movl	24(%rsp), %eax
	cltq
	movq	%rax, incCn
#ifdef DREAL
	movq	%rax, %r12
	subq	stM, incCn
	addq	$14, incCn
	shl	$3, incCn
#else
	subq	stM, incCn
	addq	$14, incCn
	shl	$4, incCn
			prefC(128(pC))
			prefC(192(pC))
#endif
/*
 *      Find M / 14 if MB is not set
 */
#if MB == 0
	cmp	$56, stM
	jne	MB_LT56
/*	movq	$56/14, stM */
	movq	$4, stM
MBFOUND:
	subq	$1, stM
#endif
	addq	$120, pA5
	addq	$120, pB0
	movq	$KB*8, ldab
	movq	$-KB*5*8, mldab5
	movq	$-KB*8, mldab
	subq	mldab5, pA5
	lea	KB*8(pA5, ldab,4), pA10
	movq	pA10, pAS
#ifdef DREAL
	test	$1, %r12
	jnz	UNALIGNED
	test	$15, pC
	jnz	UNALIGNED
#if MB == 0
	movq	stM, %r12
#endif
NLOOP:
#if MB == 0
	movq	%r12, stM
	cmp	$0, stM
	je	MLOOPCU
#elif MB > 14
   #ifdef ATL_DivAns
	movq	$ATL_DivAns-1, stM
   #else
	movq	$MB/14-1, stM
   #endif
#endif
#if ((MB == 0) || (MB > 14))
MLOOP:
/*
 *      rC[0-13] = pC[0-13] * beta
 */
	ALIGN16
/*KLOOP: */
#ifdef BETA1
	movapd	0-120(pA10,mldab5,2), rC0
	movapd	0-120(pB0), rB0
	mulpd	rB0, rC0
	addsd	(pC), rC0
	movapd	0-120(pA5, mldab,4), rC1
	mulpd	rB0, rC1
	addsd	CMUL(8)(pC), rC1
	movapd	0-120(pA10, mldab,8), rC2
	mulpd	rB0, rC2
	addsd	CMUL(16)(pC), rC2
	movapd	0-120(pA5, mldab,2), rC3
	mulpd	rB0, rC3
	addsd	CMUL(24)(pC), rC3
	movapd	0-120(pA5, mldab), rC4
	mulpd	rB0, rC4
	addsd	CMUL(32)(pC), rC4
	movapd	0-120(pA5), rC5
	mulpd	rB0, rC5
	addsd	CMUL(40)(pC), rC5
	movapd	0-120(pA5, ldab), rC6
	mulpd	rB0, rC6
	addsd	CMUL(48)(pC), rC6
	movapd	0-120(pA5, ldab,2), rC7
	mulpd	rB0, rC7
	addsd	CMUL(56)(pC), rC7
	movapd	0-120(pA10, mldab,2), rC8
	mulpd	rB0, rC8
	addsd	CMUL(64)(pC), rC8
	movapd	0-120(pA5,ldab,4), rC9
	mulpd	rB0, rC9
	addsd	CMUL(72)(pC), rC9
	movapd	0-120(pA10), rC10
	mulpd	rB0, rC10
	addsd	CMUL(80)(pC), rC10
	movapd	0-120(pA10,ldab), rC11
	mulpd	rB0, rC11
	addsd	CMUL(88)(pC), rC11
	movapd	0-120(pA10,ldab,2), rC12
	mulpd	rB0, rC12
	addsd	CMUL(96)(pC), rC12
	movapd	0-120(pA5,ldab,8), rC13
	mulpd	rB0, rC13
	addsd	CMUL(104)(pC), rC13
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
	mulsd	(pC), rC0
	mulsd	8(pC), rC1
	mulsd	16(pC), rC2
	mulsd	24(pC), rC3
	mulsd	32(pC), rC4
	mulsd	40(pC), rC5
	mulsd	48(pC), rC6
	mulsd	56(pC), rC7
	mulsd	64(pC), rC8
	mulsd	72(pC), rC9
	mulsd	80(pC), rC10
	mulsd	88(pC), rC11
	mulsd	96(pC), rC12
	mulsd	104(pC), rC13

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
#endif

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
/*	jne	KLOOP */

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
				pref2((pfA))
				pref2(64(pfA))
        movapd          rC0, rA0
        movapd          rC4, rB0
        unpcklpd        rC1, rC0        /* rC0 = c0a  c1a */
        unpcklpd        rC5, rC4        /* rC4 = c4a  c5a */
	 	 	 	prefC(112(pC))
	 	 	 	prefC(176(pC))
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
		addq	$112, pC
	addpd		rC3, rC12	/* rc12 = c12ab c13ab */
/*
 *      Write results back to C
 */
	movapd	rC0, -112(pC)
	movapd	rC2, 16-112(pC)
	movapd	rC4, 32-112(pC)
	movapd	rC6, 48-112(pC)
	movapd	rC8, 64-112(pC)
	movapd	rC10, 80-112(pC)
	movapd	rC12, 96-112(pC)
/*
 *      pC += 14;  pA += 14*NB; pB -= NB;
 */
/*
 *      while (pA != stM);
 */
	subq	$1, stM
	jne	MLOOP
#endif

/*
 *      Last iteration of M-loop unrolled to prefetch next col of B
 */
#if MB == 0
MLOOPCU:
#endif
/*KLOOP: */
#ifdef BETA1
	movapd	0-120(pA10,mldab5,2), rC0
	movapd	0-120(pB0), rB0
	mulpd	rB0, rC0
	addsd	(pC), rC0
	movapd	0-120(pA5, mldab,4), rC1
	mulpd	rB0, rC1
	addsd	CMUL(8)(pC), rC1
	movapd	0-120(pA10, mldab,8), rC2
	mulpd	rB0, rC2
	addsd	CMUL(16)(pC), rC2
	movapd	0-120(pA5, mldab,2), rC3
	mulpd	rB0, rC3
	addsd	CMUL(24)(pC), rC3
	movapd	0-120(pA5, mldab), rC4
	mulpd	rB0, rC4
	addsd	CMUL(32)(pC), rC4
	movapd	0-120(pA5), rC5
	mulpd	rB0, rC5
	addsd	CMUL(40)(pC), rC5
	movapd	0-120(pA5, ldab), rC6
	mulpd	rB0, rC6
	addsd	CMUL(48)(pC), rC6
	movapd	0-120(pA5, ldab,2), rC7
	mulpd	rB0, rC7
	addsd	CMUL(56)(pC), rC7
	movapd	0-120(pA10, mldab,2), rC8
	mulpd	rB0, rC8
	addsd	CMUL(64)(pC), rC8
	movapd	0-120(pA5,ldab,4), rC9
	mulpd	rB0, rC9
	addsd	CMUL(72)(pC), rC9
	movapd	0-120(pA10), rC10
	mulpd	rB0, rC10
	addsd	CMUL(80)(pC), rC10
	movapd	0-120(pA10,ldab), rC11
	mulpd	rB0, rC11
	addsd	CMUL(88)(pC), rC11
	movapd	0-120(pA10,ldab,2), rC12
	mulpd	rB0, rC12
	addsd	CMUL(96)(pC), rC12
	movapd	0-120(pA5,ldab,8), rC13
	mulpd	rB0, rC13
	addsd	CMUL(104)(pC), rC13
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
	mulsd	(pC), rC0
	mulsd	8(pC), rC1
	mulsd	16(pC), rC2
	mulsd	24(pC), rC3
	mulsd	32(pC), rC4
	mulsd	40(pC), rC5
	mulsd	48(pC), rC6
	mulsd	56(pC), rC7
	mulsd	64(pC), rC8
	mulsd	72(pC), rC9
	mulsd	80(pC), rC10
	mulsd	88(pC), rC11
	mulsd	96(pC), rC12
	mulsd	104(pC), rC13

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
#endif
						prefB(-120(pB0,ldab))
						prefB(64-120(pB0,ldab))
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

/* KLOOP / 2 */

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
/*	jne	KLOOP */

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
				prefC((pC,incCn))
				prefC(64(pC, incCn))
        movapd          rC0, rA0
        movapd          rC4, rB0
        unpcklpd        rC1, rC0        /* rC0 = c0a  c1a */
        unpcklpd        rC5, rC4        /* rC4 = c4a  c5a */
        unpckhpd        rC1, rA0        /* rA0 = c0b  c1b */
        unpckhpd        rC5, rB0        /* rB0 = c4b  c5b */
        addpd           rA0, rC0        /* rC0 = c0ab c1ab */
				prefC(128(pC,incCn))
				prefC(192(pC, incCn))
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
/*	subq	$MBKBso-NB14so+224, pA5 */
	movq	pAS, pA5
	addq	mldab5, pA5
        unpcklpd        rC11, rC10      /* rC10 = c10a  c11a */
/*	subq	$MBKBso-NB14so+224, pA10 */
	movq	pAS, pA10
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
	movapd	rC0, (pC)
	movapd	rC2, 16(pC)
	movapd	rC4, 32(pC)
	movapd	rC6, 48(pC)
	movapd	rC8, 64(pC)
	movapd	rC10, 80(pC)
	movapd	rC12, 96(pC)
/*
 *      pC += 14;  pA += 14*NB; pB -= NB;
 */
/*
 *      while (pA != stM);
 */
/*	subq	$1, stM */
/*	jne	MLOOP */
/*
 *      pC += incCn;  pA -= NBNB;  pB += NB;
 */
	addq	incCn, pC
/*
 *      while (pB != stN);
 */
	sub	$1, stN
	jne	NLOOP

/*
 *      Restore callee-saved iregs
 */
	movq	-8(%rsp), %rbp
	movq	-16(%rsp), %rbx
	movq	-32(%rsp), %r12
	movq	-40(%rsp), %r13
	ret
UNALIGNED:
#endif

#if MB == 0
	movq	stM, %r12
#endif
UNLOOP:
#if MB == 0
	movq	%r12, stM
	cmp	$0, stM
	je	UMLOOPCU
#elif MB > 14
   #ifdef ATL_DivAns
	movq	$ATL_DivAns-1, stM
   #else
	movq	$MB/14-1, stM
   #endif
#endif
#if ((MB == 0) || (MB > 14))
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
	addsd	(pC), rC0
	movapd	0-120(pA5, mldab,4), rC1
	mulpd	rB0, rC1
	addsd	CMUL(8)(pC), rC1
	movapd	0-120(pA10, mldab,8), rC2
	mulpd	rB0, rC2
	addsd	CMUL(16)(pC), rC2
	movapd	0-120(pA5, mldab,2), rC3
	mulpd	rB0, rC3
	addsd	CMUL(24)(pC), rC3
	movapd	0-120(pA5, mldab), rC4
	mulpd	rB0, rC4
	addsd	CMUL(32)(pC), rC4
	movapd	0-120(pA5), rC5
	mulpd	rB0, rC5
	addsd	CMUL(40)(pC), rC5
	movapd	0-120(pA5, ldab), rC6
	mulpd	rB0, rC6
	addsd	CMUL(48)(pC), rC6
	movapd	0-120(pA5, ldab,2), rC7
	mulpd	rB0, rC7
	addsd	CMUL(56)(pC), rC7
	movapd	0-120(pA10, mldab,2), rC8
	mulpd	rB0, rC8
	addsd	CMUL(64)(pC), rC8
	movapd	0-120(pA5,ldab,4), rC9
	mulpd	rB0, rC9
	addsd	CMUL(72)(pC), rC9
	movapd	0-120(pA10), rC10
	mulpd	rB0, rC10
	addsd	CMUL(80)(pC), rC10
	movapd	0-120(pA10,ldab), rC11
	mulpd	rB0, rC11
	addsd	CMUL(88)(pC), rC11
	movapd	0-120(pA10,ldab,2), rC12
	mulpd	rB0, rC12
	addsd	CMUL(96)(pC), rC12
	movapd	0-120(pA5,ldab,8), rC13
	mulpd	rB0, rC13
	addsd	CMUL(104)(pC), rC13
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
	mulsd	(pC), rC0
	mulsd	CMUL(8)(pC), rC1
	mulsd	CMUL(16)(pC), rC2
	mulsd	CMUL(24)(pC), rC3
	mulsd	CMUL(32)(pC), rC4
	mulsd	CMUL(40)(pC), rC5
	mulsd	CMUL(48)(pC), rC6
	mulsd	CMUL(56)(pC), rC7
	mulsd	CMUL(64)(pC), rC8
	mulsd	CMUL(72)(pC), rC9
	mulsd	CMUL(80)(pC), rC10
	mulsd	CMUL(88)(pC), rC11
	mulsd	CMUL(96)(pC), rC12
	mulsd	CMUL(104)(pC), rC13

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
#endif

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
   #ifdef DREAL
				pref2((pfA))
				pref2(64(pfA))
   #else
				prefC(224(pC))
				prefC(288(pC))
   #endif
        movapd          rC0, rA0
        movapd          rC4, rB0
        unpcklpd        rC1, rC0        /* rC0 = c0a  c1a */
        unpcklpd        rC5, rC4        /* rC4 = c4a  c5a */
   #ifdef DREAL
	 	 	 	prefC(112(pC))
	 	 	 	prefC(176(pC))
   #else
	 	 	 	prefC(352(pC))
	 	 	 	prefC(416(pC))
   #endif
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
#ifdef DREAL
		addq	$112, pC
#endif
	addpd		rC3, rC12	/* rc12 = c12ab c13ab */
/*
 *      Write results back to C
 */
#ifdef DREAL
	movupd	rC0, -112(pC)
	movupd	rC2, 16-112(pC)
	movupd	rC4, 32-112(pC)
	movupd	rC6, 48-112(pC)
	movupd	rC8, 64-112(pC)
	movupd	rC10, 80-112(pC)
	movupd	rC12, 96-112(pC)
#else
        movlpd  rC0, (pC)
        movhpd  rC0, 16(pC)
        movlpd  rC2, 32(pC)
        movhpd  rC2, 48(pC)
        movlpd  rC4, 64(pC)
        movhpd  rC4, 80(pC)
        movlpd  rC6, 96(pC)
        movhpd  rC6, 112(pC)
        movlpd  rC8, 128(pC)
        movhpd  rC8, 144(pC)
        movlpd  rC10, 160(pC)
        movhpd  rC10, 176(pC)
        movlpd  rC12, 192(pC)
        movhpd  rC12, 208(pC)
		addq	$224, pC
#endif
/*
 *      pC += 14;  pA += 14*NB; pB -= NB;
 */
/*
 *      while (pA != stM);
 */
	subq	$1, stM
	jne	UMLOOP
#endif

/*
 *      Last iteration of M-loop unrolled to prefetch next col of B
 */
#if MB == 0
UMLOOPCU:
#endif
/*UKLOOP: */
#ifdef BETA1
	movapd	0-120(pA10,mldab5,2), rC0
	movapd	0-120(pB0), rB0
	mulpd	rB0, rC0
	addsd	(pC), rC0
	movapd	0-120(pA5, mldab,4), rC1
	mulpd	rB0, rC1
	addsd	CMUL(8)(pC), rC1
	movapd	0-120(pA10, mldab,8), rC2
	mulpd	rB0, rC2
	addsd	CMUL(16)(pC), rC2
	movapd	0-120(pA5, mldab,2), rC3
	mulpd	rB0, rC3
	addsd	CMUL(24)(pC), rC3
	movapd	0-120(pA5, mldab), rC4
	mulpd	rB0, rC4
	addsd	CMUL(32)(pC), rC4
	movapd	0-120(pA5), rC5
	mulpd	rB0, rC5
	addsd	CMUL(40)(pC), rC5
	movapd	0-120(pA5, ldab), rC6
	mulpd	rB0, rC6
	addsd	CMUL(48)(pC), rC6
	movapd	0-120(pA5, ldab,2), rC7
	mulpd	rB0, rC7
	addsd	CMUL(56)(pC), rC7
	movapd	0-120(pA10, mldab,2), rC8
	mulpd	rB0, rC8
	addsd	CMUL(64)(pC), rC8
	movapd	0-120(pA5,ldab,4), rC9
	mulpd	rB0, rC9
	addsd	CMUL(72)(pC), rC9
	movapd	0-120(pA10), rC10
	mulpd	rB0, rC10
	addsd	CMUL(80)(pC), rC10
	movapd	0-120(pA10,ldab), rC11
	mulpd	rB0, rC11
	addsd	CMUL(88)(pC), rC11
	movapd	0-120(pA10,ldab,2), rC12
	mulpd	rB0, rC12
	addsd	CMUL(96)(pC), rC12
	movapd	0-120(pA5,ldab,8), rC13
	mulpd	rB0, rC13
	addsd	CMUL(104)(pC), rC13
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
	mulsd	(pC), rC0
	mulsd	CMUL(8)(pC), rC1
	mulsd	CMUL(16)(pC), rC2
	mulsd	CMUL(24)(pC), rC3
	mulsd	CMUL(32)(pC), rC4
	mulsd	CMUL(40)(pC), rC5
	mulsd	CMUL(48)(pC), rC6
	mulsd	CMUL(56)(pC), rC7
	mulsd	CMUL(64)(pC), rC8
	mulsd	CMUL(72)(pC), rC9
	mulsd	CMUL(80)(pC), rC10
	mulsd	CMUL(88)(pC), rC11
	mulsd	CMUL(96)(pC), rC12
	mulsd	CMUL(104)(pC), rC13

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
#endif
						prefB(-120(pB0,ldab))
						prefB(64-120(pB0,ldab))
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
				prefC((pC,incCn))
				prefC(64(pC, incCn))
        movapd          rC0, rA0
        movapd          rC4, rB0
        unpcklpd        rC1, rC0        /* rC0 = c0a  c1a */
        unpcklpd        rC5, rC4        /* rC4 = c4a  c5a */
        unpckhpd        rC1, rA0        /* rA0 = c0b  c1b */
        unpckhpd        rC5, rB0        /* rB0 = c4b  c5b */
        addpd           rA0, rC0        /* rC0 = c0ab c1ab */
				prefC(128(pC,incCn))
				prefC(192(pC, incCn))
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
#ifdef DREAL
	movupd	rC0, (pC)
	movupd	rC2, 16(pC)
	movupd	rC4, 32(pC)
	movupd	rC6, 48(pC)
	movupd	rC8, 64(pC)
	movupd	rC10, 80(pC)
	movupd	rC12, 96(pC)
#else
        movlpd  rC0, (pC)
        movhpd  rC0, 16(pC)
        movlpd  rC2, 32(pC)
        movhpd  rC2, 48(pC)
        movlpd  rC4, 64(pC)
        movhpd  rC4, 80(pC)
        movlpd  rC6, 96(pC)
        movhpd  rC6, 112(pC)
        movlpd  rC8, 128(pC)
        movhpd  rC8, 144(pC)
        movlpd  rC10, 160(pC)
        movhpd  rC10, 176(pC)
        movlpd  rC12, 192(pC)
        movhpd  rC12, 208(pC)
#endif
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
	addq	incCn, pC
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
	ret
#if MB == 0
MB_LT56:
	cmp	$42, stM
	jne	MB_LT42
/*	movq	$42/14, stM */
	movq	$3, stM
	jmp	MBFOUND
MB_LT42:
	cmp	$28, stM
	jne	MB_LT28
	movq	$2, stM
	jmp	MBFOUND
MB_LT28:
	cmp	$14, stM
	jne	DONE
	movq	$1, stM
	jmp	MBFOUND
#endif
