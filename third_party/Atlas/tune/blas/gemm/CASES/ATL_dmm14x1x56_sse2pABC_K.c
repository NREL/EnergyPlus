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
#ifdef ATL_OS_SunOS
   #define ATL_DIV_NUM MB
   #define ATL_DIV_DEN 14
#endif
#include "atlas_asm.h"


#ifndef ATL_SSE2
   #error "This routine requires SSE2!"
#endif
#if !defined(KB) || (KB == 0)
   #error "KB must be a compile-time constant!"
#endif

#if ((KB/2)*2 == KB)

#if NB != MB
   #error "For this kernel, MB = NB required!"
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
 *  Prefetch defines
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
/*	movq	%r12, -32(%rsp) */
/*	movq	%r13, -40(%rsp) */
#ifdef BETAX
   #define BOF -24
	movlpd	%xmm1, BOF(%rsp)
#endif
/*
 *      pA already comes in right reg
 *      Initialize pB = B; pC = C;
 */
	movq	16(%rsp), pC
			prefC((pC))
			prefC(64(pC))
	movq	%r9, pB
			prefB((pB))
			prefB(64(pB))
/*
 *      setup prefetch ptr for next blk of A
 */
	movq	$MBKBso, pfA
	addq	pA5, pfA
			prefB(128(pB))
/*
 *      convert ldc to 64 bits, and then set incCn = (ldc - MB)*sizeof
 */
	movl	24(%rsp), %eax
	cltq
	movq	%rax, incCn
#ifdef DREAL
	subq	$MB-14, incCn
	shl	$3, incCn
#else
	subq	$(MB-14), incCn
	shl	$4, incCn
			prefC(128(pC))
			prefC(192(pC))
#endif

	addq	$120, pA5
	addq	$120, pB0
	movq	$KB*8, ldab
	movq	$-KB*5*8, mldab5
	movq	$-KB*8, mldab
	subq	mldab5, pA5
	lea	KB*8(pA5, ldab,4), pA10
	movq	$NB, stN

UNLOOP:
   #ifdef ATL_DivAns
	movq	$ATL_DivAns-1, stM
   #else
	movq	$MB/14-1, stM
   #endif
UMLOOP:
/*
 *	rC[0-13] = pC[0-13] * beta
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

#if KB > 2
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
#endif

#if KB > 4
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
#endif

#if KB > 6
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
#endif
#ifndef DREAL
						pref2((pfA))
						pref2(64(pfA))
#endif


#if KB > 8
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
#endif

#if KB > 10
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
#endif

#if KB > 12
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
#endif

#if KB > 14
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
#endif

#if KB > 16
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
#endif

#if KB > 18
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
#endif

#if KB > 20
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
#endif

#if KB > 22
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
#endif

#if KB > 24
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
#endif

#if KB > 26
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
#endif

#if KB > 28
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

#if KB > 30
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
#endif

#if KB > 32
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
#endif

#if KB > 34
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
#endif

#if KB > 36
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
#endif

#if KB > 38
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
#endif

#if KB > 40
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
#endif

#if KB > 42
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
#endif

#if KB > 44
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
#endif

#if KB > 46
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
#endif

#if KB > 48
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
#endif

#if KB > 50
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
#endif

#if KB > 52
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
#endif

#if KB > 54
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
#endif

/*
 *      While (pB != stK);
 *
 *	cmp	pB, stK
 *	jne	UKLOOP
 */

/*
 *      Get these bastard things summed up correctly
 */
                                        /* rC0 = c0a  c0b */
                                        /* rC1 = c1a  c1b */
                                        /* rC2 = c2a  c2b */
                                        /* rC3 = c3a  c3b */

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

                                        /* rC8 = c08a  c08b */
                                        /* rC9 = c09a  c09b */
                                        /* rC10 = c10a  c10b */
                                        /* rC11 = c11a  c11b */

					/* rC12 = c12a c12b */
					/* rC13 = c13a c13b */
        movapd          rC8, rA0
        movapd          rC10, rC1
	movapd		rC12, rC3
        unpcklpd        rC9, rC8        /* rC8 = c08a  c09a */
   #if KB > 26
		addq	$NB14so-224, pA5
   #else
		addq	$NB14so, pA5
   #endif
        unpcklpd        rC11, rC10      /* rC10 = c10a  c11a */
   #if KB > 26
		addq	$NB14so-224, pA10
   #else
		addq	$NB14so, pA10
   #endif
	unpcklpd	rC13, rC12	/* rC12 = c12a c13a */
        unpckhpd        rC11, rC1       /* rC1 = c10b  c11b */
        unpckhpd        rC9, rA0        /* rA0 = c08b  c09b */
	unpckhpd	rC13, rC3	/* rC3  = c12b c13b */
        addpd           rA0, rC8        /* rC8 = c08ab c09ab */
   #if KB > 26
		subq	$224, pB0
   #endif
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
 *
 *	while (pA != stM);
 */
	subq	$1, stM
	jne	UMLOOP

/*
 *	Last iteration of M-loop unrolled to prefetch next col of B
 */
/*UMLOOP: */
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
#if KB > 2
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
#endif

#if KB > 4
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
#endif

#if KB > 6
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
#endif

#if KB > 8
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
#endif

#if KB > 10
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
#endif

#if KB > 12
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
#endif

#if KB > 14
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
#endif

#if KB > 16
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
#endif

						prefB(128-120(pB0,ldab))
						prefB(192-120(pB0,ldab))
#if KB > 18
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
#endif

#if KB > 20
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
#endif

#if KB > 22
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
#endif

#if KB > 24
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
#endif

#if KB > 26
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
#endif

/* UKLOOP / 2 */

#if KB > 28
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
#endif

#if KB > 30
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
#endif

#if KB > 32
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
#endif

#if KB > 34
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
#endif

#if KB > 36
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
#endif

#if KB > 38
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
#endif

#if KB > 40
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
#endif

#if KB > 42
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
#endif

#if KB > 44
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
#endif

#if KB > 46
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
#endif

#if KB > 48
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
#endif

#if KB > 50
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
#endif

#if KB > 52
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
#endif

#if KB > 54
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
#endif

/*
 *      While (pB != stK);
 *
 *	cmp	pB, stK
 *	jne	UKLOOP
 */

/*
 *      Get these bastard things summed up correctly
 */
                                        /* rC0 = c0a  c0b */
                                        /* rC1 = c1a  c1b */
                                        /* rC2 = c2a  c2b */
                                        /* rC3 = c3a  c3b */

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
   #if KB > 26
				prefB(160-120(pB,ldab))
   #endif
        unpckhpd        rC3, rA0        /* rA0 = c2b  c3b */
        unpckhpd        rC7, rB0        /* rB0 = c6b  c7b */
        addpd           rA0, rC2        /* rC2 = c2ab c3ab */
        addpd           rB0, rC6        /* rC6 = c6ab c7ab */

                                        /* rC8 = c08a  c08b */
                                        /* rC9 = c09a  c09b */
                                        /* rC10 = c10a  c10b */
                                        /* rC11 = c11a  c11b */

					/* rC12 = c12a c12b */
					/* rC13 = c13a c13b */
        movapd          rC8, rA0
        movapd          rC10, rC1
	movapd		rC12, rC3
        unpcklpd        rC9, rC8        /* rC8 = c08a  c09a */
   #if KB > 26
	subq	$MBKBso-NB14so+224, pA5
   #else
	subq	$MBKBso-NB14so, pA5
   #endif
        unpcklpd        rC11, rC10      /* rC10 = c10a  c11a */
   #if KB > 26
	subq	$MBKBso-NB14so+224, pA10
   #else
	subq	$MBKBso-NB14so, pA10
   #endif
	unpcklpd	rC13, rC12	/* rC12 = c12a c13a */
   #if KB > 26
	addq	$NBso-224, pB0
   #else
	addq	$NBso, pB0
   #endif
        unpckhpd        rC11, rC1       /* rC1 = c10b  c11b */
        unpckhpd        rC9, rA0        /* rA0 = c08b  c09b */
	unpckhpd	rC13, rC3	/* rC3  = c12b c13b */
        addpd           rA0, rC8        /* rC8 = c08ab c09ab */
        addpd           rC1, rC10       /* rC10 = c10ab c11ab */
	addpd		rC3, rC12	/* rc12 = c12ab c13ab */
/*
 *	Write results back to C
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
 *
 *	while (pA != stM);
 *
 *	subq	$1, stM
 *	jne	UMLOOP
 *
 *	pC += incCn;  pA -= NBNB;  pB += NB;
 */
	addq	incCn, pC
/*
 *	while (pB != stN);
 */
	sub	$1, stN
	jne	UNLOOP

/*
 *	Restore callee-saved iregs
 */
DONE:
	movq	-8(%rsp), %rbp
	movq	-16(%rsp), %rbx
/*	movq	-32(%rsp), %r12 */
/*	movq	-40(%rsp), %r13 */
	ret

#else

/*
 * Kernel for odd lda/ldb
 */

#if (KB - ((KB/2)*2) != 1)
   #error "KB must be an odd number!"
#endif
#if (MB/28)*28 != MB
   #error "MB must be multiple of 28!"
#endif
#ifdef DREAL
   #define CMUL(arg_) arg_
#else
   #define CMUL(arg_) 2*(arg_)
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
#define NB15so   (NB9so+NB6so)
#define NB28so   (NB14so+NB14so)

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
 *	Save callee-saved iregs
 */
	movq	%rbp, -8(%rsp)
	movq	%rbx, -16(%rsp)
/*	movq	%r12, -32(%rsp) */
/*	movq	%r13, -40(%rsp) */
#ifdef BETAX
   #define BOF -24
	movlpd	%xmm1, BOF(%rsp)
#endif
/*
 *      pA already comes in right reg
 *      Initialize pB = B; pC = C;
 */
	movq	16(%rsp), pC
			prefC((pC))
			prefC(64(pC))
	movq	%r9, pB
			prefB((pB))
			prefB(64(pB))
/*
 *      setup prefetch ptr for next blk of A
 */
	movq	$MBKBso, pfA
	addq	pA5, pfA
			prefB(128(pB))
/*
 *      convert ldc to 64 bits, and then set incCn = (ldc - MB)*sizeof
 */
	movl	24(%rsp), %eax
	cltq
	movq	%rax, incCn
#ifdef DREAL
	subq	$MB-28, incCn
	shl	$3, incCn
#else
	subq	$(MB-28), incCn
	shl	$4, incCn
			prefC(128(pC))
			prefC(192(pC))
#endif


	addq	$120, pA5
	addq	$120, pB0
	movq	$KB*8*2, ldab
	movq	$-KB*5*8*2, mldab5
	movq	$-KB*8*2, mldab
#if KB != 1
	subq	mldab5, pA5
#endif
	lea	KB*8*2(pA5, ldab,4), pA10
	movq	$NB, stN

#if KB == 1
   #ifdef DCPLX
      #define PFD 224
   #else
      #define PFD 224
   #endif
	subq	$120, pB0
	subq	$CMUL(224), incCn
	movq	pA5, pA10
NLOOP:
   #ifdef ATL_DivAns
	movq	$(ATL_DivAns>>1), stM
   #else
	movq	$MB/28, stM
   #endif
	movlpd	(pB0), rB0
        unpcklpd	rB0, rB0
MLOOP:
#ifdef BETA0
	movapd	-120(pA5), rC0
	mulpd	rB0, rC0
	movapd	16-120(pA5), rC1
	mulpd	rB0, rC1
	movapd	32-120(pA5), rC2
	mulpd	rB0, rC2
	movapd	48-120(pA5), rC3
	mulpd	rB0, rC3
	movapd	64-120(pA5), rC4
	mulpd	rB0, rC4
	movapd	80-120(pA5), rC5
	mulpd	rB0, rC5
	movapd	96-120(pA5), rC6
	mulpd	rB0, rC6
	movapd	112-120(pA5), rC7
	mulpd	rB0, rC7
	movapd	128-120(pA5), rC8
	mulpd	rB0, rC8
	movapd	144-120(pA5), rC9
	mulpd	rB0, rC9
	movapd	160-120(pA5), rC10
	mulpd	rB0, rC10
	movapd	176-120(pA5), rC11
	mulpd	rB0, rC11
	movapd	192-120(pA5), rC12
	mulpd	rB0, rC12
	movapd	208-120(pA5), rC13
	mulpd	rB0, rC13
#else
   #ifdef DCPLX
	movlpd	(pC), rC0
	movhpd	16(pC), rC0
	movlpd	32(pC), rC1
	movhpd	48(pC), rC1
	movlpd	64(pC), rC2
	movhpd	80(pC), rC2
	movlpd	96(pC), rC3
	movhpd	112(pC), rC3
	movlpd	128(pC), rC4
	movhpd	144(pC), rC4
	movlpd	160(pC), rC5
	movhpd	176(pC), rC5
	movlpd	192(pC), rC6
	movhpd	208(pC), rC6
	movlpd	224(pC), rC7
	movhpd	240(pC), rC7
	movlpd	256(pC), rC8
	movhpd	272(pC), rC8
	movlpd	288(pC), rC9
	movhpd	304(pC), rC9
	movlpd	320(pC), rC10
	movhpd	336(pC), rC10
	movlpd	352(pC), rC11
	movhpd	368(pC), rC11
	movlpd	384(pC), rC12
	movhpd	400(pC), rC12
	movlpd	416(pC), rC13
	movhpd	432(pC), rC13
   #else
	movupd	(pC), rC0
	movupd	16(pC), rC1
	movupd	32(pC), rC2
	movupd	48(pC), rC3
	movupd	64(pC), rC4
	movupd	80(pC), rC5
	movupd	96(pC), rC6
	movupd	112(pC), rC7
	movupd	128(pC), rC8
	movupd	144(pC), rC9
	movupd	160(pC), rC10
	movupd	176(pC), rC11
	movupd	192(pC), rC12
	movupd	208(pC), rC13
   #endif
   #ifdef BETAX
	movsd	BOF(%rsp), rA0
	unpcklpd	rA0, rA0
	mulpd	rA0, rC0
	mulpd	rA0, rC1
	mulpd	rA0, rC2
	mulpd	rA0, rC3
	mulpd	rA0, rC4
	mulpd	rA0, rC5
	mulpd	rA0, rC6
	mulpd	rA0, rC7
	mulpd	rA0, rC8
	mulpd	rA0, rC9
	mulpd	rA0, rC10
	mulpd	rA0, rC11
	mulpd	rA0, rC12
	mulpd	rA0, rC13
   #endif
	movapd	-120(pA5), rA0
	mulpd	rB0, rA0
				prefC(PFD(pC))
				prefC(PFD+64(pC))
	addpd	rA0, rC0
	movapd	16-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	32-120(pA5), rA0
	mulpd	rB0, rA0
				prefC(PFD+128(pC))
				prefC(PFD+192(pC))
	addpd	rA0, rC2
	movapd	48-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	64-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	movapd	80-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC5
	movapd	96-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC6
	movapd	112-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC7
	movapd	128-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC8
	movapd	144-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC9
	movapd	160-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC10
	movapd	176-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC11
	movapd	192-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC12
	movapd	208-120(pA5), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC13
#endif
   #ifdef DCPLX
	movlpd	rC0, (pC)
	movhpd	rC0, 16(pC)
	movlpd	rC1, 32(pC)
	movhpd	rC1, 48(pC)
	movlpd	rC2, 64(pC)
	movhpd	rC2, 80(pC)
	movlpd	rC3, 96(pC)
	movhpd	rC3, 112(pC)
	movlpd	rC4, 128(pC)
	movhpd	rC4, 144(pC)
	movlpd	rC5, 160(pC)
	movhpd	rC5, 176(pC)
	movlpd	rC6, 192(pC)
	movhpd	rC6, 208(pC)
	movlpd	rC7, 224(pC)
	movhpd	rC7, 240(pC)
	movlpd	rC8, 256(pC)
	movhpd	rC8, 272(pC)
	movlpd	rC9, 288(pC)
	movhpd	rC9, 304(pC)
	movlpd	rC10, 320(pC)
	movhpd	rC10, 336(pC)
	movlpd	rC11, 352(pC)
	movhpd	rC11, 368(pC)
	movlpd	rC12, 384(pC)
	movhpd	rC12, 400(pC)
	movlpd	rC13, 416(pC)
	movhpd	rC13, 432(pC)
   #else
	movupd	rC0, (pC)
	movupd	rC1, CMUL(16)(pC)
	movupd	rC2, CMUL(32)(pC)
	movupd	rC3, CMUL(48)(pC)
	movupd	rC4, CMUL(64)(pC)
	movupd	rC5, CMUL(80)(pC)
	movupd	rC6, CMUL(96)(pC)
	movupd	rC7, CMUL(112)(pC)
	movupd	rC8, CMUL(128)(pC)
	movupd	rC9, CMUL(144)(pC)
	movupd	rC10, CMUL(160)(pC)
	movupd	rC11, CMUL(176)(pC)
	movupd	rC12, CMUL(192)(pC)
	movupd	rC13, CMUL(208)(pC)
   #endif
	addq	$CMUL(224), pC
	addq	$224, pA5
	subq	$1, stM
	jne	MLOOP
	addq	incCn, pC
	movq	pA10, pA5
	addq	$8, pB0
	subq	$1, stN
	jne	NLOOP
   #undef PFD
#else
UNLOOP:
#if MB <= 28
	jmp	UMLOOPCU
#endif
   #ifdef ATL_DivAns
	movq	$(ATL_DivAns >> 1)-1, stM
   #else
	movq	$MB/28-1, stM
   #endif
UMLOOP:

/*	rC[0-13] = pC[0-13] * beta */
#ifdef BETA1
	movapd	0-120(pA10,mldab5,2), rC0
	movupd	0-120(pB0), rB0
	mulpd	rB0, rC0
	addsd	(pC), rC0
	movapd	0-120(pA5, mldab,4), rC1
	mulpd	rB0, rC1
	addsd	CMUL(16)(pC), rC1
	movapd	0-120(pA10, mldab,8), rC2
	mulpd	rB0, rC2
	addsd	CMUL(32)(pC), rC2
	movapd	0-120(pA5, mldab,2), rC3
	mulpd	rB0, rC3
	addsd	CMUL(48)(pC), rC3
	movapd	0-120(pA5, mldab), rC4
	mulpd	rB0, rC4
	addsd	CMUL(64)(pC), rC4
	movapd	0-120(pA5), rC5
	mulpd	rB0, rC5
	addsd	CMUL(80)(pC), rC5
	movapd	0-120(pA5, ldab), rC6
	mulpd	rB0, rC6
	addsd	CMUL(96)(pC), rC6
	movapd	0-120(pA5, ldab,2), rC7
	mulpd	rB0, rC7
	addsd	CMUL(112)(pC), rC7
	movapd	0-120(pA10, mldab,2), rC8
	mulpd	rB0, rC8
	addsd	CMUL(128)(pC), rC8
	movapd	0-120(pA5,ldab,4), rC9
	mulpd	rB0, rC9
	addsd	CMUL(144)(pC), rC9
	movapd	0-120(pA10), rC10
	mulpd	rB0, rC10
	addsd	CMUL(160)(pC), rC10
	movapd	0-120(pA10,ldab), rC11
	mulpd	rB0, rC11
	addsd	CMUL(176)(pC), rC11
	movapd	0-120(pA10,ldab,2), rC12
	mulpd	rB0, rC12
	addsd	CMUL(192)(pC), rC12
	movapd	0-120(pA5,ldab,8), rC13
	mulpd	rB0, rC13
	addsd	CMUL(208)(pC), rC13
#elif defined(BETA0)
	movapd	0-120(pA10,mldab5,2), rC0
	movupd	0-120(pB0), rC13
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
	mulsd	CMUL(16)(pC), rC1
	mulsd	CMUL(32)(pC), rC2
	mulsd	CMUL(48)(pC), rC3
	mulsd	CMUL(64)(pC), rC4
	mulsd	CMUL(80)(pC), rC5
	mulsd	CMUL(96)(pC), rC6
	mulsd	CMUL(112)(pC), rC7
	mulsd	CMUL(128)(pC), rC8
	mulsd	CMUL(144)(pC), rC9
	mulsd	CMUL(160)(pC), rC10
	mulsd	CMUL(176)(pC), rC11
	mulsd	CMUL(192)(pC), rC12
	mulsd	CMUL(208)(pC), rC13

	movapd	0-120(pA10,mldab5,2), rA0
	movupd	0-120(pB0), rB0
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
				pref2((pfA))
				pref2(64(pfA))
#if KB-1 > 2
	movapd	16-120(pA10,mldab5,2), rA0
	movupd	16-120(pB0), rB0
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
#endif

#if KB-1 > 4
	movapd	32-120(pA10,mldab5,2), rA0
	movupd	32-120(pB0), rB0
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
#endif

#if KB-1 > 6
	movapd	48-120(pA10,mldab5,2), rA0
	movupd	48-120(pB0), rB0
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
#endif

#if KB-1 > 8
	movapd	64-120(pA10,mldab5,2), rA0
	movupd	64-120(pB0), rB0
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
#endif

#if KB-1 > 10
	movapd	80-120(pA10,mldab5,2), rA0
	movupd	80-120(pB0), rB0
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
#endif

#if KB-1 > 12
	movapd	96-120(pA10,mldab5,2), rA0
	movupd	96-120(pB0), rB0
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
#endif

#if KB-1 > 14
	movapd	112-120(pA10,mldab5,2), rA0
	movupd	112-120(pB0), rB0
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
#endif

#if KB-1 > 16
	movapd	128-120(pA10,mldab5,2), rA0
	movupd	128-120(pB0), rB0
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
#endif

#if KB-1 > 18
	movapd	144-120(pA10,mldab5,2), rA0
	movupd	144-120(pB0), rB0
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
#endif

#if KB-1 > 20
	movapd	160-120(pA10,mldab5,2), rA0
	movupd	160-120(pB0), rB0
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
#endif

#if KB-1 > 22
	movapd	176-120(pA10,mldab5,2), rA0
	movupd	176-120(pB0), rB0
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
#endif

#if KB-1 > 24
	movapd	192-120(pA10,mldab5,2), rA0
	movupd	192-120(pB0), rB0
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
#endif

#if KB-1 > 26
	movapd	208-120(pA10,mldab5,2), rA0
	movupd	208-120(pB0), rB0
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
#endif

/* UKLOOP / 2 */

#if KB-1 > 28
	movapd	0-120(pA10,mldab5,2), rA0
	movupd	0-120(pB0), rB0
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

#if KB-1 > 30
	movapd	16-120(pA10,mldab5,2), rA0
	movupd	16-120(pB0), rB0
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
#endif

#if KB-1 > 32
	movapd	32-120(pA10,mldab5,2), rA0
	movupd	32-120(pB0), rB0
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
#endif

#if KB-1 > 34
	movapd	48-120(pA10,mldab5,2), rA0
	movupd	48-120(pB0), rB0
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
#endif

#if KB-1 > 36
	movapd	64-120(pA10,mldab5,2), rA0
	movupd	64-120(pB0), rB0
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
#endif

#if KB-1 > 38
	movapd	80-120(pA10,mldab5,2), rA0
	movupd	80-120(pB0), rB0
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
#endif

#if KB-1 > 40
	movapd	96-120(pA10,mldab5,2), rA0
	movupd	96-120(pB0), rB0
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
#endif

#if KB-1 > 42
	movapd	112-120(pA10,mldab5,2), rA0
	movupd	112-120(pB0), rB0
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
#endif

#if KB-1 > 44
	movapd	128-120(pA10,mldab5,2), rA0
	movupd	128-120(pB0), rB0
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
#endif

#if KB-1 > 46
	movapd	144-120(pA10,mldab5,2), rA0
	movupd	144-120(pB0), rB0
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
#endif

#if KB-1 > 48
	movapd	160-120(pA10,mldab5,2), rA0
	movupd	160-120(pB0), rB0
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
#endif

#if KB-1 > 50
	movapd	176-120(pA10,mldab5,2), rA0
	movupd	176-120(pB0), rB0
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
#endif

#if KB-1 > 52
	movapd	192-120(pA10,mldab5,2), rA0
	movupd	192-120(pB0), rB0
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
#endif

#if KB-1 > 54
	movapd	208-120(pA10,mldab5,2), rA0
	movupd	208-120(pB0), rB0
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
#endif
/*
 *      Handle last odd element
 */
#if KB-1 > 26
   #define KOFF NBso-8-224
#else
   #define KOFF NBso-8
#endif
	movsd	KOFF-120(pA10,mldab5,2), rA0
	movsd	KOFF-120(pB0), rB0
	mulsd	rB0, rA0
	addsd	rA0, rC0
	movsd	KOFF-120(pA5, mldab,4), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC1
	movsd	KOFF-120(pA10, mldab,8), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC2
	movsd	KOFF-120(pA5, mldab,2), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC3
	movsd	KOFF-120(pA5, mldab), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC4
	movsd	KOFF-120(pA5), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC5
	movsd	KOFF-120(pA5, ldab), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC6
	movsd	KOFF-120(pA5, ldab,2), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC7
	movsd	KOFF-120(pA10, mldab,2), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC8
	movsd	KOFF-120(pA5,ldab,4), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC9
	movsd	KOFF-120(pA10), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC10
	movsd	KOFF-120(pA10,ldab), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC11
	movsd	KOFF-120(pA10,ldab,2), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC12
	mulsd	KOFF-120(pA5,ldab,8), rB0
	addsd	rB0, rC13
   #undef KOFF

/*
 *      While (pB != stK);
 *
 *	cmp	pB, stK
 *	jne	UKLOOP
 */

/*
 *      Get these bastard things summed up correctly
 */
                                        /* rC0 = c0a  c0b */
                                        /* rC1 = c1a  c1b */
                                        /* rC2 = c2a  c2b */
                                        /* rC3 = c3a  c3b */

                                        /* rC4 = c4a  c4b */
                                        /* rC5 = c5a  c5b */
                                        /* rC6 = c6a  c6b */
                                        /* rC7 = c7a  c7b */
        movapd          rC0, rA0
        movapd          rC4, rB0
        unpcklpd        rC1, rC0        /* rC0 = c0a  c1a */
        unpcklpd        rC5, rC4        /* rC4 = c4a  c5a */
        unpckhpd        rC1, rA0        /* rA0 = c0b  c1b */
#ifdef DCPLX
				prefC(256(pC))
				prefC(320(pC))
#endif
        unpckhpd        rC5, rB0        /* rB0 = c4b  c5b */
        addpd           rA0, rC0        /* rC0 = c0ab c1ab */
        addpd           rB0, rC4        /* rC4 = c4ab c5ab */
        movapd          rC2, rA0
        movapd          rC6, rB0
        unpcklpd        rC3, rC2        /* rC2 = c2a  c3a */
        unpcklpd        rC7, rC6        /* rC6 = c6a  c7a */
				addq	$150, pfA
        unpckhpd        rC3, rA0        /* rA0 = c2b  c3b */
        unpckhpd        rC7, rB0        /* rB0 = c6b  c7b */
        addpd           rA0, rC2        /* rC2 = c2ab c3ab */
#ifdef DCPLX
				prefC(384(pC))
				prefC(448(pC))
#endif
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
   #if KB-1 > 26
	addq	$NBso-224-8, pA5
   #else
	addq	$NBso-8, pA5
   #endif
        unpcklpd        rC11, rC10      /* rC10 = c10a  c11a */
   #if KB-1 > 26
	addq	$NBso-224-8, pA10
   #else
	addq	$NBso-8, pA10
   #endif
	unpcklpd	rC13, rC12	/* rC12 = c12a c13a */
   #if KB-1 > 26
	subq	$224+8, pB0
   #else
	subq	$8, pB0
   #endif
        unpckhpd        rC11, rC1       /* rC1 = c10b  c11b */
        unpckhpd        rC9, rA0        /* rA0 = c08b  c09b */
	unpckhpd	rC13, rC3	/* rC3  = c12b c13b */
        addpd           rA0, rC8        /* rC8 = c08ab c09ab */
        addpd           rC1, rC10       /* rC10 = c10ab c11ab */
				pref2(-22(pfA))
	addpd		rC3, rC12	/* rc12 = c12ab c13ab */
/* */
/*	Write results back to C */
/* */
        movlpd  rC0, (pC)
        movhpd  rC0, CMUL(16)(pC)
        movlpd  rC2, CMUL(32)(pC)
        movhpd  rC2, CMUL(48)(pC)
        movlpd  rC4, CMUL(64)(pC)
        movhpd  rC4, CMUL(80)(pC)
        movlpd  rC6, CMUL(96)(pC)
        movhpd  rC6, CMUL(112)(pC)
        movlpd  rC8, CMUL(128)(pC)
        movhpd  rC8, CMUL(144)(pC)
        movlpd  rC10, CMUL(160)(pC)
        movhpd  rC10, CMUL(176)(pC)
        movlpd  rC12, CMUL(192)(pC)
        movhpd  rC12, CMUL(208)(pC)

#ifdef BETA1
	movsd	8-120(pA10,mldab5,2), rC0
	movsd	8-120(pB0), rB0
	mulsd	rB0, rC0
	addsd	CMUL(8)(pC), rC0
	movsd	8-120(pA5, mldab,4), rC1
	mulsd	rB0, rC1
	addsd	CMUL(8+16)(pC), rC1
	movsd	8-120(pA10, mldab,8), rC2
	mulpd	rB0, rC2
	addsd	CMUL(8+32)(pC), rC2
	movsd	8-120(pA5, mldab,2), rC3
	mulpd	rB0, rC3
	addsd	CMUL(8+48)(pC), rC3
	movsd	8-120(pA5, mldab), rC4
	mulsd	rB0, rC4
	addsd	CMUL(8+64)(pC), rC4
	movsd	8-120(pA5), rC5
	mulsd	rB0, rC5
	addsd	CMUL(8+80)(pC), rC5
	movsd	8-120(pA5, ldab), rC6
	mulsd	rB0, rC6
	addsd	CMUL(8+96)(pC), rC6
	movsd	8-120(pA5, ldab,2), rC7
	mulsd	rB0, rC7
	addsd	CMUL(8+112)(pC), rC7
	movsd	8-120(pA10, mldab,2), rC8
	mulsd	rB0, rC8
	addsd	CMUL(8+128)(pC), rC8
	movsd	8-120(pA5,ldab,4), rC9
	mulsd	rB0, rC9
	addsd	CMUL(8+144)(pC), rC9
	movsd	8-120(pA10), rC10
	mulsd	rB0, rC10
	addsd	CMUL(8+160)(pC), rC10
	movsd	8-120(pA10,ldab), rC11
	mulsd	rB0, rC11
	addsd	CMUL(8+176)(pC), rC11
	movsd	8-120(pA10,ldab,2), rC12
	mulsd	rB0, rC12
	addsd	CMUL(8+192)(pC), rC12
	movsd	8-120(pA5,ldab,8), rC13
	mulsd	rB0, rC13
	addsd	CMUL(8+208)(pC), rC13
#elif defined(BETA0)
	movsd	8-120(pA10,mldab5,2), rC0
	movsd	8-120(pB0), rC13
	mulsd	rC13, rC0
	movsd	8-120(pA5, mldab,4), rC1
	mulsd	rC13, rC1
	movsd	8-120(pA10, mldab,8), rC2
	mulsd	rC13, rC2
	movsd	8-120(pA5, mldab,2), rC3
	mulsd	rC13, rC3
	movsd	8-120(pA5, mldab), rC4
	mulsd	rC13, rC4
	movsd	8-120(pA5), rC5
	mulsd	rC13, rC5
	movsd	8-120(pA5, ldab), rC6
	mulsd	rC13, rC6
	movsd	8-120(pA5, ldab,2), rC7
	mulsd	rC13, rC7
	movsd	8-120(pA10, mldab,2), rC8
	mulsd	rC13, rC8
	movsd	8-120(pA5,ldab,4), rC9
	mulsd	rC13, rC9
	movsd	8-120(pA10), rC10
	mulsd	rC13, rC10
	movsd	8-120(pA10,ldab), rC11
	mulsd	rC13, rC11
	movsd	8-120(pA10,ldab,2), rC12
	mulsd	rC13, rC12
	mulsd 	8-120(pA5,ldab,8), rC13
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
	mulsd	CMUL(8)(pC), rC0
	mulsd	CMUL(8+16)(pC), rC1
	mulsd	CMUL(8+32)(pC), rC2
	mulsd	CMUL(8+48)(pC), rC3
	mulsd	CMUL(8+64)(pC), rC4
	mulsd	CMUL(8+80)(pC), rC5
	mulsd	CMUL(8+96)(pC), rC6
	mulsd	CMUL(8+112)(pC), rC7
	mulsd	CMUL(8+128)(pC), rC8
	mulsd	CMUL(8+144)(pC), rC9
	mulsd	CMUL(8+160)(pC), rC10
	mulsd	CMUL(8+176)(pC), rC11
	mulsd	CMUL(8+192)(pC), rC12
	mulsd	CMUL(8+208)(pC), rC13

	movsd	8-120(pA10,mldab5,2), rA0
	movsd	8-120(pB0), rB0
	mulsd	rB0, rA0
	addsd	rA0, rC0
	movsd	8-120(pA5, mldab,4), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC1
	movsd	8-120(pA10, mldab,8), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC2
	movsd	8-120(pA5, mldab,2), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC3
	movsd	8-120(pA5, mldab), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC4
	movsd	8-120(pA5), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC5
	movsd	8-120(pA5, ldab), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC6
	movsd	8-120(pA5, ldab,2), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC7
	movsd	8-120(pA10, mldab,2), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC8
	movsd	8-120(pA5,ldab,4), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC9
	movsd	8-120(pA10), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC10
	movsd	8-120(pA10,ldab), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC11
	movsd	8-120(pA10,ldab,2), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC12
	mulsd	8-120(pA5,ldab,8), rB0
	addsd	rB0, rC13
#endif
#if KB > 2
	movapd	16-120(pA10,mldab5,2), rA0
	movupd	16-120(pB0), rB0
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
#endif

#if KB > 4
	movapd	32-120(pA10,mldab5,2), rA0
	movupd	32-120(pB0), rB0
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
#endif

#if KB > 6
	movapd	48-120(pA10,mldab5,2), rA0
	movupd	48-120(pB0), rB0
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
#endif

#if KB > 8
	movapd	64-120(pA10,mldab5,2), rA0
	movupd	64-120(pB0), rB0
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
#endif

#if KB > 10
	movapd	80-120(pA10,mldab5,2), rA0
	movupd	80-120(pB0), rB0
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
#endif

#if KB > 12
	movapd	96-120(pA10,mldab5,2), rA0
	movupd	96-120(pB0), rB0
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
#endif

#if KB > 14
	movapd	112-120(pA10,mldab5,2), rA0
	movupd	112-120(pB0), rB0
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
#endif

#if KB > 16
	movapd	128-120(pA10,mldab5,2), rA0
	movupd	128-120(pB0), rB0
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
#endif

#if KB > 18
	movapd	144-120(pA10,mldab5,2), rA0
	movupd	144-120(pB0), rB0
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
#endif

#if KB > 20
	movapd	160-120(pA10,mldab5,2), rA0
	movupd	160-120(pB0), rB0
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
#endif

#if KB > 22
	movapd	176-120(pA10,mldab5,2), rA0
	movupd	176-120(pB0), rB0
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
#endif

#if KB > 24
	movapd	192-120(pA10,mldab5,2), rA0
	movupd	192-120(pB0), rB0
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
#endif

#if KB > 26
	movapd	208-120(pA10,mldab5,2), rA0
	movupd	208-120(pB0), rB0
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
#endif

/* UKLOOP / 2 */

#if KB > 28
	movapd	0-120(pA10,mldab5,2), rA0
	movupd	0-120(pB0), rB0
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

#if KB > 30
	movapd	16-120(pA10,mldab5,2), rA0
	movupd	16-120(pB0), rB0
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
#endif

#if KB > 32
	movapd	32-120(pA10,mldab5,2), rA0
	movupd	32-120(pB0), rB0
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
#endif

#if KB > 34
	movapd	48-120(pA10,mldab5,2), rA0
	movupd	48-120(pB0), rB0
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
#endif

#if KB > 36
	movapd	64-120(pA10,mldab5,2), rA0
	movupd	64-120(pB0), rB0
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
#endif

#if KB > 38
	movapd	80-120(pA10,mldab5,2), rA0
	movupd	80-120(pB0), rB0
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
#endif

#if KB > 40
	movapd	96-120(pA10,mldab5,2), rA0
	movupd	96-120(pB0), rB0
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
#endif

#if KB > 42
	movapd	112-120(pA10,mldab5,2), rA0
	movupd	112-120(pB0), rB0
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
#endif

#if KB > 44
	movapd	128-120(pA10,mldab5,2), rA0
	movupd	128-120(pB0), rB0
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
#endif

#if KB > 46
	movapd	144-120(pA10,mldab5,2), rA0
	movupd	144-120(pB0), rB0
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
#endif

#if KB > 48
	movapd	160-120(pA10,mldab5,2), rA0
	movupd	160-120(pB0), rB0
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
   #ifdef DCPLX
				prefC(512(pC))
				prefC(575(pC))
   #else
				prefC(208(pC))
				prefC(272(pC))
   #endif
	addpd	rA0, rC12
	mulpd	160-120(pA5,ldab,8), rB0
	addpd	rB0, rC13
#else
   #ifdef DCPLX
				prefC(512(pC))
				prefC(575(pC))
   #else
				prefC(208(pC))
				prefC(272(pC))
   #endif
#endif

#if KB > 50
	movapd	176-120(pA10,mldab5,2), rA0
	movupd	176-120(pB0), rB0
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
#endif

#if KB > 52
	movapd	192-120(pA10,mldab5,2), rA0
	movupd	192-120(pB0), rB0
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
#endif

#if KB > 54
	movapd	208-120(pA10,mldab5,2), rA0
	movupd	208-120(pB0), rB0
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
#endif

/* */
/*	While (pB != stK); */
/* */
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
   #if KB > 26
   #endif
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
/* HERE HERE prefC */
        movapd          rC8, rA0
        movapd          rC10, rC1
	movapd		rC12, rC3
        unpcklpd        rC9, rC8        /* rC8 = c08a  c09a */
#ifdef DCPLX
				prefC(640(pC))
				prefC(704(pC))
#else
				prefC(208(pC))
				prefC(272(pC))
#endif
        unpcklpd        rC11, rC10      /* rC10 = c10a  c11a */
	unpcklpd	rC13, rC12	/* rC12 = c12a c13a */
        unpckhpd        rC11, rC1       /* rC1 = c10b  c11b */
        unpckhpd        rC9, rA0        /* rA0 = c08b  c09b */
	unpckhpd	rC13, rC3	/* rC3  = c12b c13b */
        addpd           rA0, rC8        /* rC8 = c08ab c09ab */
        addpd           rC1, rC10       /* rC10 = c10ab c11ab */
#ifdef DCPLX
				prefC(768(pC))
				prefC(832(pC))
#else
				prefC(336(pC))
				prefC(400(pC))
#endif
	addpd		rC3, rC12	/* rc12 = c12ab c13ab */
/* */
/*	Write results back to C */
/* */
        movlpd  rC0, CMUL(8)(pC)
        movhpd  rC0, CMUL(8+16)(pC)
        movlpd  rC2, CMUL(8+32)(pC)
        movhpd  rC2, CMUL(8+48)(pC)
        movlpd  rC4, CMUL(8+64)(pC)
        movhpd  rC4, CMUL(8+80)(pC)
        movlpd  rC6, CMUL(8+96)(pC)
        movhpd  rC6, CMUL(8+112)(pC)
        movlpd  rC8, CMUL(8+128)(pC)
        movhpd  rC8, CMUL(8+144)(pC)
        movlpd  rC10, CMUL(8+160)(pC)
        movhpd  rC10, CMUL(8+176)(pC)
        movlpd  rC12, CMUL(8+192)(pC)
        movhpd  rC12, CMUL(8+208)(pC)
/*
 *      pC += 28;  pA += 28*NB; pB -= NB;
 */
		addq	$CMUL(224), pC
#if KB > 26
		addq	$NB28so-NBso-224+8, pA5
#else
		addq	$NB28so-NBso+8, pA5
#endif
#if KB > 26
		addq	$NB28so-NBso-224+8, pA10
#else
		addq	$NB28so-NBso+8, pA10
#endif
#if KB > 26
		subq	$224-8, pB0
#else
		addq	$8, pB0
#endif
/*
 *      while (pA != stM);
 */
	subq	$1, stM
	jne	UMLOOP

/*
 *	Last iteration of M-loop unrolled to prefetch next col of B
 */
UMLOOPCU:
/*UKLOOP: */
#ifdef BETA1
	movapd	0-120(pA10,mldab5,2), rC0
	movupd	0-120(pB0), rB0
	mulpd	rB0, rC0
	addsd	(pC), rC0
	movapd	0-120(pA5, mldab,4), rC1
	mulpd	rB0, rC1
	addsd	CMUL(16)(pC), rC1
	movapd	0-120(pA10, mldab,8), rC2
	mulpd	rB0, rC2
	addsd	CMUL(32)(pC), rC2
	movapd	0-120(pA5, mldab,2), rC3
	mulpd	rB0, rC3
	addsd	CMUL(48)(pC), rC3
	movapd	0-120(pA5, mldab), rC4
	mulpd	rB0, rC4
	addsd	CMUL(64)(pC), rC4
	movapd	0-120(pA5), rC5
	mulpd	rB0, rC5
	addsd	CMUL(80)(pC), rC5
	movapd	0-120(pA5, ldab), rC6
	mulpd	rB0, rC6
	addsd	CMUL(96)(pC), rC6
	movapd	0-120(pA5, ldab,2), rC7
	mulpd	rB0, rC7
	addsd	CMUL(112)(pC), rC7
	movapd	0-120(pA10, mldab,2), rC8
	mulpd	rB0, rC8
	addsd	CMUL(128)(pC), rC8
	movapd	0-120(pA5,ldab,4), rC9
	mulpd	rB0, rC9
	addsd	CMUL(144)(pC), rC9
	movapd	0-120(pA10), rC10
	mulpd	rB0, rC10
	addsd	CMUL(160)(pC), rC10
	movapd	0-120(pA10,ldab), rC11
	mulpd	rB0, rC11
	addsd	CMUL(176)(pC), rC11
	movapd	0-120(pA10,ldab,2), rC12
	mulpd	rB0, rC12
	addsd	CMUL(192)(pC), rC12
	movapd	0-120(pA5,ldab,8), rC13
	mulpd	rB0, rC13
	addsd	CMUL(208)(pC), rC13
#elif defined(BETA0)
	movapd	0-120(pA10,mldab5,2), rC0
	movupd	0-120(pB0), rC13
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
	mulsd	CMUL(16)(pC), rC1
	mulsd	CMUL(32)(pC), rC2
	mulsd	CMUL(48)(pC), rC3
	mulsd	CMUL(64)(pC), rC4
	mulsd	CMUL(80)(pC), rC5
	mulsd	CMUL(96)(pC), rC6
	mulsd	CMUL(112)(pC), rC7
	mulsd	CMUL(128)(pC), rC8
	mulsd	CMUL(144)(pC), rC9
	mulsd	CMUL(160)(pC), rC10
	mulsd	CMUL(176)(pC), rC11
	mulsd	CMUL(192)(pC), rC12
	mulsd	CMUL(208)(pC), rC13

	movapd	0-120(pA10,mldab5,2), rA0
	movupd	0-120(pB0), rB0
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
#if KB-1 > 2
	movapd	16-120(pA10,mldab5,2), rA0
	movupd	16-120(pB0), rB0
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
#endif

#if KB-1 > 4
	movapd	32-120(pA10,mldab5,2), rA0
	movupd	32-120(pB0), rB0
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
#endif

#if KB-1 > 6
	movapd	48-120(pA10,mldab5,2), rA0
	movupd	48-120(pB0), rB0
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
#endif

#if KB-1 > 8
	movapd	64-120(pA10,mldab5,2), rA0
	movupd	64-120(pB0), rB0
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
#endif

#if KB-1 > 10
	movapd	80-120(pA10,mldab5,2), rA0
	movupd	80-120(pB0), rB0
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
#endif

#if KB-1 > 12
	movapd	96-120(pA10,mldab5,2), rA0
	movupd	96-120(pB0), rB0
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
#endif

#if KB-1 > 14
	movapd	112-120(pA10,mldab5,2), rA0
	movupd	112-120(pB0), rB0
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
#endif

#if KB-1 > 16
	movapd	128-120(pA10,mldab5,2), rA0
	movupd	128-120(pB0), rB0
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
#endif

						prefB(128-120(pB0,ldab))
						prefB(192-120(pB0,ldab))
#if KB-1 > 18
	movapd	144-120(pA10,mldab5,2), rA0
	movupd	144-120(pB0), rB0
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
#endif

#if KB-1 > 20
	movapd	160-120(pA10,mldab5,2), rA0
	movupd	160-120(pB0), rB0
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
#endif

#if KB-1 > 22
	movapd	176-120(pA10,mldab5,2), rA0
	movupd	176-120(pB0), rB0
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
#endif

#if KB-1 > 24
	movapd	192-120(pA10,mldab5,2), rA0
	movupd	192-120(pB0), rB0
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
#endif

#if KB-1 > 26
	movapd	208-120(pA10,mldab5,2), rA0
	movupd	208-120(pB0), rB0
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
#endif

/* UKLOOP / 2 */

#if KB-1 > 28
	movapd	0-120(pA10,mldab5,2), rA0
	movupd	0-120(pB0), rB0
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
#endif

#if KB-1 > 30
	movapd	16-120(pA10,mldab5,2), rA0
	movupd	16-120(pB0), rB0
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
#endif

#if KB-1 > 32
	movapd	32-120(pA10,mldab5,2), rA0
	movupd	32-120(pB0), rB0
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
#endif

#if KB-1 > 34
	movapd	48-120(pA10,mldab5,2), rA0
	movupd	48-120(pB0), rB0
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
#endif

#if KB-1 > 36
	movapd	64-120(pA10,mldab5,2), rA0
	movupd	64-120(pB0), rB0
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
#endif

#if KB-1 > 38
	movapd	80-120(pA10,mldab5,2), rA0
	movupd	80-120(pB0), rB0
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
#endif

#if KB-1 > 40
	movapd	96-120(pA10,mldab5,2), rA0
	movupd	96-120(pB0), rB0
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
#endif

#if KB-1 > 42
	movapd	112-120(pA10,mldab5,2), rA0
	movupd	112-120(pB0), rB0
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
#endif

#if KB-1 > 44
	movapd	128-120(pA10,mldab5,2), rA0
	movupd	128-120(pB0), rB0
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
#endif

#if KB-1 > 46
	movapd	144-120(pA10,mldab5,2), rA0
	movupd	144-120(pB0), rB0
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
#endif

#if KB-1 > 48
	movapd	160-120(pA10,mldab5,2), rA0
	movupd	160-120(pB0), rB0
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
#endif

#if KB-1 > 50
	movapd	176-120(pA10,mldab5,2), rA0
	movupd	176-120(pB0), rB0
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
#endif

#if KB-1 > 52
	movapd	192-120(pA10,mldab5,2), rA0
	movupd	192-120(pB0), rB0
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
#endif

#if KB-1 > 54
	movapd	208-120(pA10,mldab5,2), rA0
	movupd	208-120(pB0), rB0
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
#endif
/*
 *      Handle last odd element
 */
#if KB-1 > 26
   #define KOFF NBso-8-224
#else
   #define KOFF NBso-8
#endif
	movsd	KOFF-120(pA10,mldab5,2), rA0
	movsd	KOFF-120(pB0), rB0
	mulsd	rB0, rA0
	addsd	rA0, rC0
	movsd	KOFF-120(pA5, mldab,4), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC1
	movsd	KOFF-120(pA10, mldab,8), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC2
	movsd	KOFF-120(pA5, mldab,2), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC3
	movsd	KOFF-120(pA5, mldab), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC4
	movsd	KOFF-120(pA5), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC5
	movsd	KOFF-120(pA5, ldab), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC6
	movsd	KOFF-120(pA5, ldab,2), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC7
	movsd	KOFF-120(pA10, mldab,2), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC8
	movsd	KOFF-120(pA5,ldab,4), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC9
	movsd	KOFF-120(pA10), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC10
	movsd	KOFF-120(pA10,ldab), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC11
	movsd	KOFF-120(pA10,ldab,2), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC12
	mulsd	KOFF-120(pA5,ldab,8), rB0
	addsd	rB0, rC13
   #undef KOFF

/* */
/*	While (pB != stK); */
/* */
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
   #if KB-1 > 26
				prefB(160-120(pB,ldab))
   #endif
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
   #if KB-1 > 26
	addq	$NBso-224-8, pA5
   #else
	addq	$NBso-8, pA5
   #endif
        unpcklpd        rC11, rC10      /* rC10 = c10a  c11a */
   #if KB-1 > 26
	addq	$NBso-224-8, pA10
   #else
	addq	$NBso-8, pA10
   #endif
	unpcklpd	rC13, rC12	/* rC12 = c12a c13a */
   #if KB-1 > 26
	subq	$224+8, pB0
   #else
	subq	$8, pB0
   #endif
        unpckhpd        rC11, rC1       /* rC1 = c10b  c11b */
        unpckhpd        rC9, rA0        /* rA0 = c08b  c09b */
	unpckhpd	rC13, rC3	/* rC3  = c12b c13b */
        addpd           rA0, rC8        /* rC8 = c08ab c09ab */
        addpd           rC1, rC10       /* rC10 = c10ab c11ab */
	addpd		rC3, rC12	/* rc12 = c12ab c13ab */
/* */
/*	Write results back to C */
/* */
        movlpd  rC0, (pC)
        movhpd  rC0, CMUL(16)(pC)
        movlpd  rC2, CMUL(32)(pC)
        movhpd  rC2, CMUL(48)(pC)
        movlpd  rC4, CMUL(64)(pC)
        movhpd  rC4, CMUL(80)(pC)
        movlpd  rC6, CMUL(96)(pC)
        movhpd  rC6, CMUL(112)(pC)
        movlpd  rC8, CMUL(128)(pC)
        movhpd  rC8, CMUL(144)(pC)
        movlpd  rC10, CMUL(160)(pC)
        movhpd  rC10, CMUL(176)(pC)
        movlpd  rC12, CMUL(192)(pC)
        movhpd  rC12, CMUL(208)(pC)

#ifdef BETA1
	movsd	8-120(pA10,mldab5,2), rC0
	movsd	8-120(pB0), rB0
	mulsd	rB0, rC0
	addsd	CMUL(8)(pC), rC0
	movsd	8-120(pA5, mldab,4), rC1
	mulsd	rB0, rC1
	addsd	CMUL(8+16)(pC), rC1
	movsd	8-120(pA10, mldab,8), rC2
	mulpd	rB0, rC2
	addsd	CMUL(8+32)(pC), rC2
	movsd	8-120(pA5, mldab,2), rC3
	mulpd	rB0, rC3
	addsd	CMUL(8+48)(pC), rC3
	movsd	8-120(pA5, mldab), rC4
	mulsd	rB0, rC4
	addsd	CMUL(8+64)(pC), rC4
	movsd	8-120(pA5), rC5
	mulsd	rB0, rC5
	addsd	CMUL(8+80)(pC), rC5
	movsd	8-120(pA5, ldab), rC6
	mulsd	rB0, rC6
	addsd	CMUL(8+96)(pC), rC6
	movsd	8-120(pA5, ldab,2), rC7
	mulsd	rB0, rC7
	addsd	CMUL(8+112)(pC), rC7
	movsd	8-120(pA10, mldab,2), rC8
	mulsd	rB0, rC8
	addsd	CMUL(8+128)(pC), rC8
	movsd	8-120(pA5,ldab,4), rC9
	mulsd	rB0, rC9
	addsd	CMUL(8+144)(pC), rC9
	movsd	8-120(pA10), rC10
	mulsd	rB0, rC10
	addsd	CMUL(8+160)(pC), rC10
	movsd	8-120(pA10,ldab), rC11
	mulsd	rB0, rC11
	addsd	CMUL(8+176)(pC), rC11
	movsd	8-120(pA10,ldab,2), rC12
	mulsd	rB0, rC12
	addsd	CMUL(8+192)(pC), rC12
	movsd	8-120(pA5,ldab,8), rC13
	mulsd	rB0, rC13
	addsd	CMUL(8+208)(pC), rC13
#elif defined(BETA0)
	movsd	8-120(pA10,mldab5,2), rC0
	movsd	8-120(pB0), rC13
	mulsd	rC13, rC0
	movsd	8-120(pA5, mldab,4), rC1
	mulsd	rC13, rC1
	movsd	8-120(pA10, mldab,8), rC2
	mulsd	rC13, rC2
	movsd	8-120(pA5, mldab,2), rC3
	mulsd	rC13, rC3
	movsd	8-120(pA5, mldab), rC4
	mulsd	rC13, rC4
	movsd	8-120(pA5), rC5
	mulsd	rC13, rC5
	movsd	8-120(pA5, ldab), rC6
	mulsd	rC13, rC6
	movsd	8-120(pA5, ldab,2), rC7
	mulsd	rC13, rC7
	movsd	8-120(pA10, mldab,2), rC8
	mulsd	rC13, rC8
	movsd	8-120(pA5,ldab,4), rC9
	mulsd	rC13, rC9
	movsd	8-120(pA10), rC10
	mulsd	rC13, rC10
	movsd	8-120(pA10,ldab), rC11
	mulsd	rC13, rC11
	movsd	8-120(pA10,ldab,2), rC12
	mulsd	rC13, rC12
	mulsd 	8-120(pA5,ldab,8), rC13
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
	mulsd	CMUL(8)(pC), rC0
	mulsd	CMUL(8+16)(pC), rC1
	mulsd	CMUL(8+32)(pC), rC2
	mulsd	CMUL(8+48)(pC), rC3
	mulsd	CMUL(8+64)(pC), rC4
	mulsd	CMUL(8+80)(pC), rC5
	mulsd	CMUL(8+96)(pC), rC6
	mulsd	CMUL(8+112)(pC), rC7
	mulsd	CMUL(8+128)(pC), rC8
	mulsd	CMUL(8+144)(pC), rC9
	mulsd	CMUL(8+160)(pC), rC10
	mulsd	CMUL(8+176)(pC), rC11
	mulsd	CMUL(8+192)(pC), rC12
	mulsd	CMUL(8+208)(pC), rC13

	movsd	8-120(pA10,mldab5,2), rA0
	movsd	8-120(pB0), rB0
	mulsd	rB0, rA0
	addsd	rA0, rC0
	movsd	8-120(pA5, mldab,4), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC1
	movsd	8-120(pA10, mldab,8), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC2
	movsd	8-120(pA5, mldab,2), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC3
	movsd	8-120(pA5, mldab), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC4
	movsd	8-120(pA5), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC5
	movsd	8-120(pA5, ldab), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC6
	movsd	8-120(pA5, ldab,2), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC7
	movsd	8-120(pA10, mldab,2), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC8
	movsd	8-120(pA5,ldab,4), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC9
	movsd	8-120(pA10), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC10
	movsd	8-120(pA10,ldab), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC11
	movsd	8-120(pA10,ldab,2), rA0
	mulsd	rB0, rA0
	addsd	rA0, rC12
	mulsd	8-120(pA5,ldab,8), rB0
	addsd	rB0, rC13
#endif
						prefB(-120(pB0,ldab))
						prefB(64-120(pB0,ldab))
#if KB > 2
	movapd	16-120(pA10,mldab5,2), rA0
	movupd	16-120(pB0), rB0
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
#endif

#if KB > 4
	movapd	32-120(pA10,mldab5,2), rA0
	movupd	32-120(pB0), rB0
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
#endif

#if KB > 6
	movapd	48-120(pA10,mldab5,2), rA0
	movupd	48-120(pB0), rB0
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
#endif

#if KB > 8
	movapd	64-120(pA10,mldab5,2), rA0
	movupd	64-120(pB0), rB0
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
#endif

#if KB > 10
	movapd	80-120(pA10,mldab5,2), rA0
	movupd	80-120(pB0), rB0
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
#endif

#if KB > 12
	movapd	96-120(pA10,mldab5,2), rA0
	movupd	96-120(pB0), rB0
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
#endif

#if KB > 14
	movapd	112-120(pA10,mldab5,2), rA0
	movupd	112-120(pB0), rB0
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
#endif

#if KB > 16
	movapd	128-120(pA10,mldab5,2), rA0
	movupd	128-120(pB0), rB0
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
#endif

						prefB(128-120(pB0,ldab))
						prefB(192-120(pB0,ldab))
#if KB > 18
	movapd	144-120(pA10,mldab5,2), rA0
	movupd	144-120(pB0), rB0
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
#endif

#if KB > 20
	movapd	160-120(pA10,mldab5,2), rA0
	movupd	160-120(pB0), rB0
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
#endif

#if KB > 22
	movapd	176-120(pA10,mldab5,2), rA0
	movupd	176-120(pB0), rB0
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
#endif

#if KB > 24
	movapd	192-120(pA10,mldab5,2), rA0
	movupd	192-120(pB0), rB0
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
#endif

#if KB > 26
	movapd	208-120(pA10,mldab5,2), rA0
	movupd	208-120(pB0), rB0
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
#endif

/* UKLOOP / 2 */

#if KB > 28
	movapd	0-120(pA10,mldab5,2), rA0
	movupd	0-120(pB0), rB0
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
#endif

#if KB > 30
	movapd	16-120(pA10,mldab5,2), rA0
	movupd	16-120(pB0), rB0
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
#endif

#if KB > 32
	movapd	32-120(pA10,mldab5,2), rA0
	movupd	32-120(pB0), rB0
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
#endif

#if KB > 34
	movapd	48-120(pA10,mldab5,2), rA0
	movupd	48-120(pB0), rB0
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
#endif

#if KB > 36
	movapd	64-120(pA10,mldab5,2), rA0
	movupd	64-120(pB0), rB0
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
#endif

#if KB > 38
	movapd	80-120(pA10,mldab5,2), rA0
	movupd	80-120(pB0), rB0
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
#endif

#if KB > 40
	movapd	96-120(pA10,mldab5,2), rA0
	movupd	96-120(pB0), rB0
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
#endif

#if KB > 42
	movapd	112-120(pA10,mldab5,2), rA0
	movupd	112-120(pB0), rB0
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
#endif

#if KB > 44
	movapd	128-120(pA10,mldab5,2), rA0
	movupd	128-120(pB0), rB0
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
#endif

#if KB > 46
	movapd	144-120(pA10,mldab5,2), rA0
	movupd	144-120(pB0), rB0
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
#endif

#if KB > 48
	movapd	160-120(pA10,mldab5,2), rA0
	movupd	160-120(pB0), rB0
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
#endif

#if KB > 50
	movapd	176-120(pA10,mldab5,2), rA0
	movupd	176-120(pB0), rB0
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
#endif

#if KB > 52
	movapd	192-120(pA10,mldab5,2), rA0
	movupd	192-120(pB0), rB0
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
#endif

#if KB > 54
	movapd	208-120(pA10,mldab5,2), rA0
	movupd	208-120(pB0), rB0
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
#endif

/* */
/*	While (pB != stK); */
/* */
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
   #if KB > 26
				prefB(160-120(pB,ldab))
   #endif
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
   #if KB > 26
	subq	$MBKBso-NB28so+NBso+224-8, pA5
   #else
	subq	$MBKBso-NB28so+NBso-8, pA5
   #endif
        unpcklpd        rC11, rC10      /* rC10 = c10a  c11a */
   #if KB > 26
	subq	$MBKBso-NB28so+NBso+224-8, pA10
   #else
	subq	$MBKBso-NB28so+NBso-8, pA10
   #endif
	unpcklpd	rC13, rC12	/* rC12 = c12a c13a */
   #if KB > 26
	addq	$NBso-224+8, pB0
   #else
	addq	$NBso+8, pB0
   #endif
        unpckhpd        rC11, rC1       /* rC1 = c10b  c11b */
        unpckhpd        rC9, rA0        /* rA0 = c08b  c09b */
	unpckhpd	rC13, rC3	/* rC3  = c12b c13b */
        addpd           rA0, rC8        /* rC8 = c08ab c09ab */
        addpd           rC1, rC10       /* rC10 = c10ab c11ab */
	addpd		rC3, rC12	/* rc12 = c12ab c13ab */
/* */
/*	Write results back to C */
/* */
        movlpd  rC0, CMUL(8)(pC)
        movhpd  rC0, CMUL(8+16)(pC)
        movlpd  rC2, CMUL(8+32)(pC)
        movhpd  rC2, CMUL(8+48)(pC)
        movlpd  rC4, CMUL(8+64)(pC)
        movhpd  rC4, CMUL(8+80)(pC)
        movlpd  rC6, CMUL(8+96)(pC)
        movhpd  rC6, CMUL(8+112)(pC)
        movlpd  rC8, CMUL(8+128)(pC)
        movhpd  rC8, CMUL(8+144)(pC)
        movlpd  rC10, CMUL(8+160)(pC)
        movhpd  rC10, CMUL(8+176)(pC)
        movlpd  rC12, CMUL(8+192)(pC)
        movhpd  rC12, CMUL(8+208)(pC)
/*	subq	$1, stM */
/*	jne	UMLOOP */
/* */
/*	pC += incCn;  pA -= NBNB;  pB += NB; */
/* */
/*	subq	$CMUL(MB*8-224-8), pC */
/*
 *      Handle odd cols of A
 */

/*
 *      pC += incCn;  pA -= NBNB;  pB += NB;
 */
	addq	incCn, pC
/*
 *	while (pB != stN);
 */

	sub	$1, stN
	jne	UNLOOP
#endif

/*
 *	Restore callee-saved iregs
 */
DONE:
	movq	-8(%rsp), %rbp
	movq	-16(%rsp), %rbx
/*	movq	-32(%rsp), %r12 */
/*	movq	-40(%rsp), %r13 */
	ret
#endif
