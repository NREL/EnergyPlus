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
#ifdef ATL_OS_SunOS
   #define ATL_DIV_NUM MB
   #define ATL_DIV_DEN 14
#endif
#include "atlas_asm.h"


#if !defined(NB) || (NB == 0)
   #error "NB must be a compile-time constant!"
#endif
#ifndef ATL_SSE1
   #error "This routine requires SSE1!"
#endif
#if NB != MB || NB != KB
   #error "For this kernel, MB = NB = KB required!"
#endif
#if (NB != 84)
   #error "NB must be 84!"
#endif
#if (NB/14)*14 != NB
   #error "NB must be multiple of 14!"
#endif
#ifdef SREAL
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

#define NBso	(NB*4)
#define NBNBso  (NB*NB*4)
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
ALIGN4
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
   #define BOF -56
	movss	%xmm1, BOF(%rsp)
	movss	%xmm1, BOF+4(%rsp)
	movss	%xmm1, BOF+8(%rsp)
	movss	%xmm1, BOF+12(%rsp)
#endif
/*
 *      pA already comes in right reg
 *	Initialize pB = B; pC = C; NBso = NB * sizeof;
 */
	movq	16(%rsp), pC
			prefC((pC))
			prefC(64(pC))
	movq	%r9, pB
			prefB((pB))
			prefB(64(pB))
/*
 *      stM = pA + NBNBso;  stN = pB + NBNBso;
 */
	movq	$NBNBso, pfA
	addq	pA5, pfA
			prefB(128(pB))
/*
 *      convert ldc to 64 bits, and then set incCn = (ldc - MB)*sizeof
 */
	movl	24(%rsp), %eax
	cltq
	movq	%rax, incCn
#ifdef SREAL
	movq	%rax, stM
	subq	$MB-14, incCn
	shl	$2, incCn
#else
	subq	$MB-14, incCn
	shl	$3, incCn
			prefC(128(pC))
			prefC(192(pC))
#endif
/*
 *
 */
	addq	$120, pA5
	addq	$120, pB0
	movq	$KB*4, ldab
	movq	$-KB*5*4, mldab5
	movq	$-KB*4, mldab
	subq	mldab5, pA5
	lea	KB*4(pA5, ldab,4), pA10
	movq	$NB, stN
#ifdef SREAL
	test	$1, stM
	jnz	UNLOOP
	test	$15, pC
	jnz	UNLOOP
#endif

UNLOOP:
   #ifdef ATL_DivAns
	movq	$ATL_DivAns-1, stM
   #else
	movq	$MB/14-1, stM
   #endif
UMLOOP:
/*
 *      rC[0-13] = pC[0-13] * beta
 */
        prefC((pC))
	ALIGN16
/*UKLOOP: */
	movaps	0-120(pA10,mldab5,2), rC0
	movaps	0-120(pB0), rC13
	mulps	rC13, rC0
	movaps	0-120(pA5, mldab,4), rC1
	mulps	rC13, rC1
	movaps	0-120(pA10, mldab,8), rC2
	mulps	rC13, rC2
	movaps	0-120(pA5, mldab,2), rC3
	mulps	rC13, rC3
	movaps	0-120(pA5, mldab), rC4
	mulps	rC13, rC4
	movaps	0-120(pA5), rC5
	mulps	rC13, rC5
	movaps	0-120(pA5, ldab), rC6
	mulps	rC13, rC6
	movaps	0-120(pA5, ldab,2), rC7
	mulps	rC13, rC7
	movaps	0-120(pA10, mldab,2), rC8
	mulps	rC13, rC8
	movaps	0-120(pA5,ldab,4), rC9
	mulps	rC13, rC9
	movaps	0-120(pA10), rC10
	mulps	rC13, rC10
	movaps	0-120(pA10,ldab), rC11
	mulps	rC13, rC11
	movaps	0-120(pA10,ldab,2), rC12
	mulps	rC13, rC12
	mulps 	0-120(pA5,ldab,8), rC13

	movaps	16-120(pA10,mldab5,2), rA0
	movaps	16-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	16-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	16-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	16-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	16-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	16-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	16-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	16-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	16-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	16-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	16-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	16-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	16-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	16-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	32-120(pA10,mldab5,2), rA0
	movaps	32-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	32-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	32-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	32-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	32-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	32-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	32-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	32-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	32-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	32-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	32-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	32-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	32-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	32-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	48-120(pA10,mldab5,2), rA0
	movaps	48-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	48-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	48-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	48-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	48-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	48-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	48-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	48-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	48-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	48-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	48-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	48-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	48-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	48-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	64-120(pA10,mldab5,2), rA0
	movaps	64-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	64-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	64-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	64-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	64-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	64-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	64-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	64-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	64-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	64-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	64-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	64-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	64-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	64-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	80-120(pA10,mldab5,2), rA0
	movaps	80-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	80-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	80-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	80-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	80-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	80-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	80-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	80-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	80-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	80-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	80-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	80-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	80-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	80-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	96-120(pA10,mldab5,2), rA0
	movaps	96-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	96-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	96-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	96-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	96-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	96-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	96-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	96-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	96-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	96-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	96-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	96-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	96-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	96-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	112-120(pA10,mldab5,2), rA0
	movaps	112-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	112-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	112-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	112-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	112-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	112-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	112-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	112-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	112-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	112-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	112-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	112-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	112-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	112-120(pA5,ldab,8), rB0
	addps	rB0, rC13
#ifndef SREAL
					pref2((pfA))
					pref2(64(pfA))
#endif

	movaps	128-120(pA10,mldab5,2), rA0
	movaps	128-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	128-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	128-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	128-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	128-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	128-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	128-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	128-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	128-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	128-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	128-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	128-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	128-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	128-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	144-120(pA10,mldab5,2), rA0
	movaps	144-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	144-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	144-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	144-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	144-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	144-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	144-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	144-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	144-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	144-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	144-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	144-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	144-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	144-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	160-120(pA10,mldab5,2), rA0
	movaps	160-120(pB0), rB0
	mulps	rB0, rA0
				addq $176, pB0
	addps	rA0, rC0
	movaps	160-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	160-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	160-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	160-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	160-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	160-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	160-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	160-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	160-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	160-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	160-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	160-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
				addq $176, pA10
	addps	rA0, rC12
	mulps	160-120(pA5,ldab,8), rB0
	addps	rB0, rC13
				addq $176, pA5

	movaps	0-120(pA10,mldab5,2), rA0
	movaps	0-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	0-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	0-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	0-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	0-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	0-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	0-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	0-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	0-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	0-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	0-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	0-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	0-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	0-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	16-120(pA10,mldab5,2), rA0
	movaps	16-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	16-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	16-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	16-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	16-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	16-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	16-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	16-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	16-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	16-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	16-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	16-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	16-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	16-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	32-120(pA10,mldab5,2), rA0
	movaps	32-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	32-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	32-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	32-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	32-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	32-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	32-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	32-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	32-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	32-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	32-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	32-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	32-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	32-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	48-120(pA10,mldab5,2), rA0
	movaps	48-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	48-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	48-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	48-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	48-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	48-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	48-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	48-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	48-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	48-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	48-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	48-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	48-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	48-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	64-120(pA10,mldab5,2), rA0
	movaps	64-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	64-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	64-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	64-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	64-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	64-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	64-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	64-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	64-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	64-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	64-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	64-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	64-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	64-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	80-120(pA10,mldab5,2), rA0
	movaps	80-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	80-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	80-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	80-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	80-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	80-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	80-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	80-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	80-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	80-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	80-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	80-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	80-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	80-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	96-120(pA10,mldab5,2), rA0
	movaps	96-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	96-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	96-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	96-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	96-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	96-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	96-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	96-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	96-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	96-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	96-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	96-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	96-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	96-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	112-120(pA10,mldab5,2), rA0
	movaps	112-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	112-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	112-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	112-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	112-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	112-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	112-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	112-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	112-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	112-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	112-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	112-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	112-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	112-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	128-120(pA10,mldab5,2), rA0
	movaps	128-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	128-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	128-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	128-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	128-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	128-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	128-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	128-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	128-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	128-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	128-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	128-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	128-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	128-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	144-120(pA10,mldab5,2), rA0
	movaps	144-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	144-120(pA5, mldab,4), rA0
#ifdef BETA1
                addss   (pC), rC0
#endif
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	144-120(pA10, mldab,8), rA0
#ifdef BETA1
                addss   CMUL(4)(pC), rC1
#endif
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	144-120(pA5, mldab,2), rA0
#ifdef BETA1
                addss   CMUL(8)(pC), rC2
#endif
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	144-120(pA5, mldab), rA0
#ifdef BETA1
                addss   CMUL(12)(pC), rC3
#endif
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	144-120(pA5), rA0
#ifdef BETA1
                addss   CMUL(16)(pC), rC4
#endif
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	144-120(pA5, ldab), rA0
#ifdef BETA1
                addss   CMUL(20)(pC), rC5
#endif
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	144-120(pA5, ldab,2), rA0
#ifdef BETA1
                addss   CMUL(24)(pC), rC6
#endif
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	144-120(pA10, mldab,2), rA0
#ifdef BETA1
                addss   CMUL(28)(pC), rC7
#endif
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	144-120(pA5,ldab,4), rA0
#ifdef BETA1
                addss   CMUL(32)(pC), rC8
#endif
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	144-120(pA10), rA0
#ifdef BETA1
                addss   CMUL(36)(pC), rC9
#endif
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	144-120(pA10,ldab), rA0
#ifdef BETA1
                addss   CMUL(40)(pC), rC10
#endif
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	144-120(pA10,ldab,2), rA0
#ifdef BETA1
                addss   CMUL(44)(pC), rC11
#endif
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	144-120(pA5,ldab,8), rB0
#ifdef BETA1
                addss   CMUL(48)(pC), rC12
#endif
	addps	rB0, rC13

/*UKLOOP */
/*
 *      Get these bastard things summed up correctly
 */

					/* rC0 = c0a    c0b    c0c    c0d */
					/* rC1 = c1a    c1b    c1c    c1d */
					/* rC2 = c2a    c2b    c2c    c2d */
					/* rC3 = c3a    c3b    c3c    c3d */
/* */
	movaps		rC2, rB0	/* rB0 = c2a    c2b    c2c    c2d */
						prefC((pC))
						prefC(64(pC))
	movaps		rC0, rA0	/* rA0 = c0a    c0b    c0c    c0d */
	unpckhps	rC3, rB0	/* rB0 = c2c    c3c    c2d    c3d */
#ifdef BETA1
                addss   CMUL(52)(pC), rC13
#endif
	unpckhps	rC1, rA0	/* rA0 = c0c    c1c    c0d    c1d */
	unpcklps	rC3, rC2	/* rC2 = c2a    c3a    c2b    c3b */
	movlhps		rB0, rC3	/* rC3 = c3a    c3b    c2c    c3c */
	unpcklps	rC1, rC0	/* rC0 = c0a    c1a    c0b    c1b */
	movhlps		rA0, rC3	/* rC3 = c0d    c1d    c2c    c3c */
	movlhps		rC2, rA0	/* rA0 = c0c    c1c    c2a    c3a */
	movhlps		rC0, rB0	/* rB0 = c0b    c1b    c2d    c3d */
	addps		rA0, rC3	/* rC3 = c0cd   c1cd   c2ac   c3ac */
	movlhps		rC0, rC1	/* rC1 = c1a    c1b    c0a    c1a */
	movhlps		rC1, rC2	/* rC2 = c0a    c1a    c2b    c3b */
	movaps		rC4, rA0	/* rA0 = c4a    c4b    c4c    c4d */
	addps		rB0, rC2	/* rC2 = c0ab   c1ab   c2bd   c3bd */
	movaps		rC6, rB0	/* rB0 = c6a    c6b    c6c    c6d */
	addps		rC2, rC3	/* rC3 = c0abcd c1abcd c2bdac c3bdac */


					/* rC4 = c4a    c4b    c4c    c4d */
					/* rC5 = c5a    c5b    c5c    c5d */
					/* rC6 = c6a    c6b    c6c    c6d */
					/* rC7 = c7a    c7b    c7c    c7d */
					/* rC8  = c08a    c08b    c08c    c08d */
					/* rC9  = c09a    c09b    c09c    c09d */
					/* rC10 = c10a    c10b    c10c    c10d */
					/* rC11 = c11a    c11b    c11c    c11d */
					/* rC12 = c12a    c12b    c12c    c12d */
					/* rC13 = c13a    c13b    c13c    c13d */
/* */
	movaps		rC10, rC0	/* rC0 = c10a    c10b    c10c    c10d */
						prefC(128(pC))
#ifdef SREAL
						pref2((pfA))
#else
						prefC(192(pC))
#endif
	movaps		rC8 , rC1	/* rC1 = c08a    c08b    c08c    c08d */
	movaps          rC12, rC2 	/* rC2  = c12a    c12b    c12c    c12d */
	unpckhps	rC7, rB0	/* rB0 = c6c    c7c    c6d    c7d */
	unpckhps	rC5, rA0	/* rA0 = c4c    c5c    c4d    c5d */
	unpcklps	rC7, rC6	/* rC6 = c6a    c7a    c6b    c7b */
	unpckhps	rC11, rC0	/* rC0 = c10c    c11c    c10d    c11d */
	unpckhps	rC9 , rC1	/* rC1 = c08c    c09c    c08d    c09d */
	movlhps		rB0, rC7	/* rC7 = c7a    c7b    c6c    c7c */
	unpcklps	rC5, rC4	/* rC4 = c4a    c5a    c4b    c5b */
	movhlps		rA0, rC7	/* rC7 = c4d    c5d    c6c    c7c */
	movlhps		rC6, rA0	/* rA0 = c4c    c5c    c6a    c7a */
	unpcklps	rC11, rC10	/* rC10 = c10a    c11a    c10b    c11b */
	movhlps		rC4, rB0	/* rB0 = c4b    c5b    c6d    c7d */
	movlhps		rC0, rC11	/* rC11 = c11a    c11b    c10c    c11c */
	addps		rA0, rC7	/* rC7 = c4cd   c5cd   c6ac   c7ac */
#ifdef BETAX
   #ifdef SREAL
			movups	(pC), rA0
	movlhps		rC4, rC5	/* rC5 = c5a    c5b    c4a    c5a */
			movups	16(pC), rC4
	unpcklps	rC9 , rC8 	/* rC8  = c08a    c09a    c08b    c09b */
	movhlps		rC1, rC11	/* rC11 = c08d    c09d    c10c    c11c */
	movlhps		rC10, rC1	/* rC1 = c08c    c09c    c10a    c11a */
	movhlps		rC5, rC6	/* rC6 = c4a    c5a    c6b    c7b */
			movups	32(pC), rC5
	movhlps		rC8 , rC0	/* rC0 = c08b    c09b    c10d    c11d */
	unpcklps	rC13, rC2	/* rC2  = c12a    c13a    c12b    c13b */
	addps		rC1, rC11	/* rC11 = c08cd   c09cd   c10ac   c11ac */
			movlps	48(pC), rC1
	addps		rB0, rC6	/* rC6 = c4ab   c5ab   c6bd   c7bd */
	movlhps		rC8 , rC9 	/* rC9  = c09a    c09b    c08a    c09a */
	unpckhps	rC13, rC12	/* rC12 = c12c    c13c    c12d    c13d */
	movhlps		rC9 , rC10	/* rC10 = c08a    c09a    c10b    c11b */
	addps		rC6, rC7	/* rC7 = c4abcd c5abcd c6bdac c7bdac */
						pref2(64(pfA))
			mulps	BOF(%rsp), rA0
	addps		rC0, rC10	/* rC10 = c08ab   c09ab   c10bd   c11bd */
			mulps	BOF(%rsp), rC4
	addps		rC2, rC12	/* rC12 = c12ac   c13ac   c12bd   c13bd */
			mulps	BOF(%rsp), rC5
	addps		rC10, rC11	/* rC11 = c08abcd c09abcd c10bdac c11bdac */
			mulps	BOF(%rsp), rC1

/* */

	movhlps		rC12, rC13	/* rC13 = c12bd   c13bd   X       X */
			addps	rA0, rC3
						addq	$68, pfA
	addps		rC13, rC12	/* rC12 = c12abcd c13abcd X       X */
			addps	rC4, rC7
			addps	rC5, rC11
			addps	rC1, rC12
   #else  /* BETA = X, complex type */
			movups	(pC), rA0
	movlhps		rC4, rC5	/* rC5 = c5a    c5b    c4a    c5a */
			movups	16(pC), rC4
	unpcklps	rC9 , rC8 	/* rC8  = c08a    c09a    c08b    c09b */
 			shufps	$0x88, rC4, rA0    /* rA0 = c0 c1 c2 c3 */
			movups	32(pC), rC4        /* rC4 = c4 X  c5 X */
	movhlps		rC1, rC11	/* rC11 = c08d    c09d    c10c    c11c */
	movlhps		rC10, rC1	/* rC1 = c08c    c09c    c10a    c11a */
	movhlps		rC5, rC6	/* rC6 = c4a    c5a    c6b    c7b */
			movups	48(pC), rC5        /* rC5 = c6 X c7 X */
	movhlps		rC8 , rC0	/* rC0 = c08b    c09b    c10d    c11d */
	unpcklps	rC13, rC2	/* rC2  = c12a    c13a    c12b    c13b */
	addps		rC1, rC11	/* rC11 = c08cd   c09cd   c10ac   c11ac */
 			shufps	$0x88, rC5, rC4    /* rC4 = c4 c5 c6 c7 */
			movups	64(pC), rC5	   /* rC5 = c8 X  c9 X */
			movups	80(pC), rC1        /* rC1 = c10 X c11 X */
	addps		rB0, rC6	/* rC6 = c4ab   c5ab   c6bd   c7bd */
 			shufps	$0x88, rC1, rC5    /* rC5 = c8 c9 c10 c11 */
	movlhps		rC8 , rC9 	/* rC9  = c09a    c09b    c08a    c09a */
			movss	96(pC), rC1
	unpckhps	rC13, rC12	/* rC12 = c12c    c13c    c12d    c13d */
			movss	104(pC), rB0
	movhlps		rC9 , rC10	/* rC10 = c08a    c09a    c10b    c11b */
			unpcklps	rB0, rC1
	addps		rC6, rC7	/* rC7 = c4abcd c5abcd c6bdac c7bdac */
						prefC(256(pC))
			mulps	BOF(%rsp), rA0
	addps		rC0, rC10	/* rC10 = c08ab   c09ab   c10bd   c11bd */
			mulps	BOF(%rsp), rC4
	addps		rC2, rC12	/* rC12 = c12ac   c13ac   c12bd   c13bd */
			mulps	BOF(%rsp), rC5
	addps		rC10, rC11	/* rC11 = c08abcd c09abcd c10bdac c11bdac */
			mulps	BOF(%rsp), rC1

/* */

	movhlps		rC12, rC13	/* rC13 = c12bd   c13bd   X       X */
			addps	rA0, rC3
						prefC(192(pC))
						addq	$68, pfA
	addps		rC13, rC12	/* rC12 = c12abcd c13abcd X       X */
			addps	rC4, rC7
			addps	rC5, rC11
			addps	rC1, rC12
   #endif

#else
	movlhps		rC4, rC5	/* rC5 = c5a    c5b    c4a    c5a */
	unpcklps	rC9 , rC8 	/* rC8  = c08a    c09a    c08b    c09b */
	movhlps		rC1, rC11	/* rC11 = c08d    c09d    c10c    c11c */
	movlhps		rC10, rC1	/* rC1 = c08c    c09c    c10a    c11a */
	movhlps		rC5, rC6	/* rC6 = c4a    c5a    c6b    c7b */
	movhlps		rC8 , rC0	/* rC0 = c08b    c09b    c10d    c11d */
	unpcklps	rC13, rC2	/* rC2  = c12a    c13a    c12b    c13b */
	addps		rC1, rC11	/* rC11 = c08cd   c09cd   c10ac   c11ac */
	addps		rB0, rC6	/* rC6 = c4ab   c5ab   c6bd   c7bd */
	movlhps		rC8 , rC9 	/* rC9  = c09a    c09b    c08a    c09a */
	unpckhps	rC13, rC12	/* rC12 = c12c    c13c    c12d    c13d */
	movhlps		rC9 , rC10	/* rC10 = c08a    c09a    c10b    c11b */
	addps		rC6, rC7	/* rC7 = c4abcd c5abcd c6bdac c7bdac */
   #ifdef SREAL
						pref2(64(pfA))
   #else
						prefC(256(pC))
   #endif
	addps		rC0, rC10	/* rC10 = c08ab   c09ab   c10bd   c11bd */
	addps		rC2, rC12	/* rC12 = c12ac   c13ac   c12bd   c13bd */
	addps		rC10, rC11	/* rC11 = c08abcd c09abcd c10bdac c11bdac */

/* */

	movhlps		rC12, rC13	/* rC13 = c12bd   c13bd   X       X */
   #ifndef SREAL
						prefC(192(pC))
   #endif
						addq	$68, pfA
	addps		rC13, rC12	/* rC12 = c12abcd c13abcd X       X */

#endif
/*
 *      Write results back to C;  pC += 14;
 */
#ifdef SREAL
	movups	rC3, (pC)
	movups	rC7, 16(pC)
	movups	rC11, 32(pC)
	movlps	rC12, 48(pC)
	addq	$56, pC
#else
	movss	rC3, (pC)
	movss	rC7, 32(pC)
	movhlps	rC3, rC0
	movhlps	rC7, rC6
	movss	rC0, 16(pC)
	movss	rC6, 48(pC)
	shufps	$0x55, rC3, rC3
	shufps	$0x55, rC7, rC7
	movss	rC3, 8(pC)
	movss	rC7, 40(pC)
	shufps	$0x55, rC0, rC0
	shufps	$0x55, rC6, rC6
	movss	rC0, 24(pC)
	movss	rC6, 56(pC)

	movss	rC11, 64(pC)
	movhlps	rC11, rC2
	movss	rC12, 96(pC)
	movss	rC2, 80(pC)
	shufps	$0x55, rC11, rC11
	shufps	$0x55, rC12, rC12
	movss	rC11, 72(pC)
	shufps	$0x55, rC2, rC2
	movss	rC12, 104(pC)
	movss	rC2, 88(pC)

	addq	$112, pC
#endif
/*
 *      Write results back to C
 */
		addq	$NB14so-176, pA5
		addq	$NB14so-176, pA10
		subq	$176, pB0
/*
 *      pC += 14;  pA += 14*NB; pB -= NB;
 */
/*
 *      while (pA != stM);
 */
	subq	$1, stM
ALIGN4
	jne	UMLOOP

/*
 *      Last iteration of M-loop unrolled to prefetch next col of B
 */

ALIGN8
/*UKLOOP: */
	movaps	0-120(pA10,mldab5,2), rC0
	movaps	0-120(pB0), rC13
	mulps	rC13, rC0
	movaps	0-120(pA5, mldab,4), rC1
	mulps	rC13, rC1
	movaps	0-120(pA10, mldab,8), rC2
	mulps	rC13, rC2
	movaps	0-120(pA5, mldab,2), rC3
	mulps	rC13, rC3
	movaps	0-120(pA5, mldab), rC4
	mulps	rC13, rC4
	movaps	0-120(pA5), rC5
	mulps	rC13, rC5
	movaps	0-120(pA5, ldab), rC6
	mulps	rC13, rC6
	movaps	0-120(pA5, ldab,2), rC7
	mulps	rC13, rC7
	movaps	0-120(pA10, mldab,2), rC8
	mulps	rC13, rC8
	movaps	0-120(pA5,ldab,4), rC9
	mulps	rC13, rC9
	movaps	0-120(pA10), rC10
	mulps	rC13, rC10
	movaps	0-120(pA10,ldab), rC11
	mulps	rC13, rC11
	movaps	0-120(pA10,ldab,2), rC12
	mulps	rC13, rC12
	mulps 	0-120(pA5,ldab,8), rC13

	movaps	16-120(pA10,mldab5,2), rA0
	movaps	16-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	16-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	16-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	16-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	16-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	16-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	16-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	16-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	16-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	16-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	16-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	16-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	16-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	16-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	32-120(pA10,mldab5,2), rA0
	movaps	32-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	32-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	32-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	32-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	32-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	32-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	32-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	32-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	32-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	32-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	32-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	32-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	32-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	32-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	48-120(pA10,mldab5,2), rA0
	movaps	48-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	48-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	48-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	48-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	48-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	48-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	48-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	48-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	48-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	48-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	48-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	48-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	48-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	48-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	64-120(pA10,mldab5,2), rA0
	movaps	64-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	64-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	64-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	64-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	64-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	64-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	64-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	64-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	64-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	64-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	64-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	64-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	64-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	64-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	80-120(pA10,mldab5,2), rA0
	movaps	80-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	80-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	80-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	80-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	80-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	80-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	80-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	80-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	80-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	80-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	80-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	80-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	80-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	80-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	96-120(pA10,mldab5,2), rA0
	movaps	96-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	96-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	96-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	96-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	96-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	96-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	96-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	96-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	96-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	96-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	96-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	96-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	96-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	96-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	112-120(pA10,mldab5,2), rA0
	movaps	112-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	112-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	112-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	112-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	112-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	112-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	112-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	112-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	112-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	112-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	112-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	112-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	112-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	112-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	128-120(pA10,mldab5,2), rA0
	movaps	128-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	128-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	128-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	128-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	128-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	128-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	128-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	128-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	128-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	128-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	128-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	128-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	128-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	128-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	144-120(pA10,mldab5,2), rA0
	movaps	144-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	144-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	144-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	144-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	144-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	144-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	144-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	144-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	144-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	144-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	144-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	144-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	144-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	144-120(pA5,ldab,8), rB0
	addps	rB0, rC13
						prefB((pB,ldab))
						prefB(64(pB,ldab))

	movaps	160-120(pA10,mldab5,2), rA0
	movaps	160-120(pB0), rB0
	mulps	rB0, rA0
				addq $176, pB0
	addps	rA0, rC0
	movaps	160-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	160-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	160-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	160-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	160-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	160-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	160-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	160-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	160-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	160-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	160-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	160-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
				addq $176, pA10
	addps	rA0, rC12
	mulps	160-120(pA5,ldab,8), rB0
	addps	rB0, rC13
				addq $176, pA5

	movaps	0-120(pA10,mldab5,2), rA0
	movaps	0-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	0-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	0-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	0-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	0-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	0-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	0-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	0-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	0-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	0-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	0-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	0-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	0-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	0-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	16-120(pA10,mldab5,2), rA0
	movaps	16-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	16-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	16-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	16-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	16-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	16-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	16-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	16-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	16-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	16-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	16-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	16-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	16-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	16-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	32-120(pA10,mldab5,2), rA0
	movaps	32-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	32-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	32-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	32-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	32-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	32-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	32-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	32-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	32-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	32-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	32-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	32-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	32-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	32-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	48-120(pA10,mldab5,2), rA0
	movaps	48-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	48-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	48-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	48-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	48-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	48-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	48-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	48-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	48-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	48-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	48-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	48-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	48-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	48-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	64-120(pA10,mldab5,2), rA0
	movaps	64-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	64-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	64-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	64-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	64-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	64-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	64-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	64-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	64-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	64-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	64-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	64-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	64-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	64-120(pA5,ldab,8), rB0
	addps	rB0, rC13
						prefB(128-176(pB,ldab))
						prefB(192-176(pB,ldab))

	movaps	80-120(pA10,mldab5,2), rA0
	movaps	80-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	80-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	80-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	80-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	80-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	80-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	80-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	80-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	80-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	80-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	80-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	80-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	80-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	80-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	96-120(pA10,mldab5,2), rA0
	movaps	96-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	96-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	96-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	96-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	96-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	96-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	96-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	96-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	96-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	96-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	96-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	96-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	96-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	96-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	112-120(pA10,mldab5,2), rA0
	movaps	112-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	112-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	112-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	112-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	112-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	112-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	112-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	112-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	112-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	112-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	112-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	112-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	112-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	112-120(pA5,ldab,8), rB0
						prefC((pC))
						prefC((pC,incCn))
	addps	rB0, rC13

	movaps	128-120(pA10,mldab5,2), rA0
	movaps	128-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	128-120(pA5, mldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	128-120(pA10, mldab,8), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	128-120(pA5, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	128-120(pA5, mldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	128-120(pA5), rA0
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	128-120(pA5, ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	128-120(pA5, ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	128-120(pA10, mldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	128-120(pA5,ldab,4), rA0
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	128-120(pA10), rA0
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	128-120(pA10,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	128-120(pA10,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	128-120(pA5,ldab,8), rB0
	addps	rB0, rC13

	movaps	144-120(pA10,mldab5,2), rA0
	movaps	144-120(pB0), rB0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	144-120(pA5, mldab,4), rA0
#ifdef BETA1
                addss   (pC), rC0
#endif
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	144-120(pA10, mldab,8), rA0
#ifdef BETA1
                addss   CMUL(4)(pC), rC1
#endif
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	144-120(pA5, mldab,2), rA0
#ifdef BETA1
                addss   CMUL(8)(pC), rC2
#endif
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	144-120(pA5, mldab), rA0
#ifdef BETA1
                addss   CMUL(12)(pC), rC3
#endif
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	144-120(pA5), rA0
#ifdef BETA1
                addss   CMUL(16)(pC), rC4
#endif
	mulps	rB0, rA0
	addps	rA0, rC5
	movaps	144-120(pA5, ldab), rA0
#ifdef BETA1
                addss   CMUL(20)(pC), rC5
#endif
	mulps	rB0, rA0
	addps	rA0, rC6
	movaps	144-120(pA5, ldab,2), rA0
#ifdef BETA1
                addss   CMUL(24)(pC), rC6
#endif
	mulps	rB0, rA0
	addps	rA0, rC7
	movaps	144-120(pA10, mldab,2), rA0
#ifdef BETA1
                addss   CMUL(28)(pC), rC7
#endif
	mulps	rB0, rA0
	addps	rA0, rC8
	movaps	144-120(pA5,ldab,4), rA0
#ifdef BETA1
                addss   CMUL(32)(pC), rC8
#endif
	mulps	rB0, rA0
	addps	rA0, rC9
	movaps	144-120(pA10), rA0
#ifdef BETA1
                addss   CMUL(36)(pC), rC9
#endif
	mulps	rB0, rA0
	addps	rA0, rC10
	movaps	144-120(pA10,ldab), rA0
#ifdef BETA1
                addss   CMUL(40)(pC), rC10
#endif
	mulps	rB0, rA0
	addps	rA0, rC11
	movaps	144-120(pA10,ldab,2), rA0
#ifdef BETA1
                addss   CMUL(44)(pC), rC11
#endif
	mulps	rB0, rA0
	addps	rA0, rC12
	mulps	144-120(pA5,ldab,8), rB0
	addps	rB0, rC13

/*UKLOOP */
/*
 *      Get these bastard things summed up correctly
 */

					/* rC0 = c0a    c0b    c0c    c0d */
					/* rC1 = c1a    c1b    c1c    c1d */
					/* rC2 = c2a    c2b    c2c    c2d */
					/* rC3 = c3a    c3b    c3c    c3d */
/* */
	movaps		rC2, rB0	/* rB0 = c2a    c2b    c2c    c2d */
						prefC(64(pC,incCn))
						prefB(256-176(pB,ldab))
	movaps		rC0, rA0	/* rA0 = c0a    c0b    c0c    c0d */
	unpckhps	rC3, rB0	/* rB0 = c2c    c3c    c2d    c3d */
#ifdef BETA1
                addss   CMUL(52)(pC), rC13
#endif
	unpckhps	rC1, rA0	/* rA0 = c0c    c1c    c0d    c1d */
#ifdef BETA1
                addss   CMUL(48)(pC), rC12
#endif
	unpcklps	rC3, rC2	/* rC2 = c2a    c3a    c2b    c3b */
	movlhps		rB0, rC3	/* rC3 = c3a    c3b    c2c    c3c */
	unpcklps	rC1, rC0	/* rC0 = c0a    c1a    c0b    c1b */
	movhlps		rA0, rC3	/* rC3 = c0d    c1d    c2c    c3c */
	movlhps		rC2, rA0	/* rA0 = c0c    c1c    c2a    c3a */
	movhlps		rC0, rB0	/* rB0 = c0b    c1b    c2d    c3d */
	addps		rA0, rC3	/* rC3 = c0cd   c1cd   c2ac   c3ac */
	movlhps		rC0, rC1	/* rC1 = c1a    c1b    c0a    c1a */
	movhlps		rC1, rC2	/* rC2 = c0a    c1a    c2b    c3b */
	movaps		rC4, rA0	/* rA0 = c4a    c4b    c4c    c4d */
	addps		rB0, rC2	/* rC2 = c0ab   c1ab   c2bd   c3bd */
	movaps		rC6, rB0	/* rB0 = c6a    c6b    c6c    c6d */
	addps		rC2, rC3	/* rC3 = c0abcd c1abcd c2bdac c3bdac */


					/* rC4 = c4a    c4b    c4c    c4d */
					/* rC5 = c5a    c5b    c5c    c5d */
					/* rC6 = c6a    c6b    c6c    c6d */
					/* rC7 = c7a    c7b    c7c    c7d */
					/* rC8  = c08a    c08b    c08c    c08d */
					/* rC9  = c09a    c09b    c09c    c09d */
					/* rC10 = c10a    c10b    c10c    c10d */
					/* rC11 = c11a    c11b    c11c    c11d */
					/* rC12 = c12a    c12b    c12c    c12d */
					/* rC13 = c13a    c13b    c13c    c13d */
/* */
	movaps		rC10, rC0	/* rC0 = c10a    c10b    c10c    c10d */
	movaps		rC8 , rC1	/* rC1 = c08a    c08b    c08c    c08d */
	movaps          rC12, rC2 	/* rC2  = c12a    c12b    c12c    c12d */
	unpckhps	rC7, rB0	/* rB0 = c6c    c7c    c6d    c7d */
	unpckhps	rC5, rA0	/* rA0 = c4c    c5c    c4d    c5d */
	unpcklps	rC7, rC6	/* rC6 = c6a    c7a    c6b    c7b */
	unpckhps	rC11, rC0	/* rC0 = c10c    c11c    c10d    c11d */
	unpckhps	rC9 , rC1	/* rC1 = c08c    c09c    c08d    c09d */
	movlhps		rB0, rC7	/* rC7 = c7a    c7b    c6c    c7c */
	unpcklps	rC5, rC4	/* rC4 = c4a    c5a    c4b    c5b */
	movhlps		rA0, rC7	/* rC7 = c4d    c5d    c6c    c7c */
	movlhps		rC6, rA0	/* rA0 = c4c    c5c    c6a    c7a */
	unpcklps	rC11, rC10	/* rC10 = c10a    c11a    c10b    c11b */
	movhlps		rC4, rB0	/* rB0 = c4b    c5b    c6d    c7d */
	movlhps		rC0, rC11	/* rC11 = c11a    c11b    c10c    c11c */
	addps		rA0, rC7	/* rC7 = c4cd   c5cd   c6ac   c7ac */
#ifdef BETAX
   #ifdef SREAL
			movups	(pC), rA0
	movlhps		rC4, rC5	/* rC5 = c5a    c5b    c4a    c5a */
			movups	16(pC), rC4
	unpcklps	rC9 , rC8 	/* rC8  = c08a    c09a    c08b    c09b */
	movhlps		rC1, rC11	/* rC11 = c08d    c09d    c10c    c11c */
	movlhps		rC10, rC1	/* rC1 = c08c    c09c    c10a    c11a */
	movhlps		rC5, rC6	/* rC6 = c4a    c5a    c6b    c7b */
			movups	32(pC), rC5
	movhlps		rC8 , rC0	/* rC0 = c08b    c09b    c10d    c11d */
	unpcklps	rC13, rC2	/* rC2  = c12a    c13a    c12b    c13b */
	addps		rC1, rC11	/* rC11 = c08cd   c09cd   c10ac   c11ac */
			movlps	48(pC), rC1
	addps		rB0, rC6	/* rC6 = c4ab   c5ab   c6bd   c7bd */
	movlhps		rC8 , rC9 	/* rC9  = c09a    c09b    c08a    c09a */
	unpckhps	rC13, rC12	/* rC12 = c12c    c13c    c12d    c13d */
	movhlps		rC9 , rC10	/* rC10 = c08a    c09a    c10b    c11b */
	addps		rC6, rC7	/* rC7 = c4abcd c5abcd c6bdac c7bdac */
			mulps	BOF(%rsp), rA0
	addps		rC0, rC10	/* rC10 = c08ab   c09ab   c10bd   c11bd */
			mulps	BOF(%rsp), rC4
	addps		rC2, rC12	/* rC12 = c12ac   c13ac   c12bd   c13bd */
			mulps	BOF(%rsp), rC5
	addps		rC10, rC11	/* rC11 = c08abcd c09abcd c10bdac c11bdac */
			mulps	BOF(%rsp), rC1

/* */

	movhlps		rC12, rC13	/* rC13 = c12bd   c13bd   X       X */
			addps	rA0, rC3
	addps		rC13, rC12	/* rC12 = c12abcd c13abcd X       X */
			addps	rC4, rC7
			addps	rC5, rC11
						prefB(320-176(pB,ldab))
			addps	rC1, rC12
   #else  /* BETA = X, complex type */
			movups	(pC), rA0
	movlhps		rC4, rC5	/* rC5 = c5a    c5b    c4a    c5a */
			movups	16(pC), rC4
	unpcklps	rC9 , rC8 	/* rC8  = c08a    c09a    c08b    c09b */
 			shufps	$0x88, rC4, rA0    /* rA0 = c0 c1 c2 c3 */
			movups	32(pC), rC4        /* rC4 = c4 X  c5 X */
	movhlps		rC1, rC11	/* rC11 = c08d    c09d    c10c    c11c */
	movlhps		rC10, rC1	/* rC1 = c08c    c09c    c10a    c11a */
	movhlps		rC5, rC6	/* rC6 = c4a    c5a    c6b    c7b */
			movups	48(pC), rC5        /* rC5 = c6 X c7 X */
	movhlps		rC8 , rC0	/* rC0 = c08b    c09b    c10d    c11d */
	unpcklps	rC13, rC2	/* rC2  = c12a    c13a    c12b    c13b */
	addps		rC1, rC11	/* rC11 = c08cd   c09cd   c10ac   c11ac */
 			shufps	$0x88, rC5, rC4    /* rC4 = c4 c5 c6 c7 */
			movups	64(pC), rC5	   /* rC5 = c8 X  c9 X */
			movups	80(pC), rC1        /* rC1 = c10 X c11 X */
	addps		rB0, rC6	/* rC6 = c4ab   c5ab   c6bd   c7bd */
 			shufps	$0x88, rC1, rC5    /* rC5 = c8 c9 c10 c11 */
	movlhps		rC8 , rC9 	/* rC9  = c09a    c09b    c08a    c09a */
			movss	96(pC), rC1
	unpckhps	rC13, rC12	/* rC12 = c12c    c13c    c12d    c13d */
			movss	104(pC), rB0
	movhlps		rC9 , rC10	/* rC10 = c08a    c09a    c10b    c11b */
			unpcklps	rB0, rC1
	addps		rC6, rC7	/* rC7 = c4abcd c5abcd c6bdac c7bdac */
			mulps	BOF(%rsp), rA0
	addps		rC0, rC10	/* rC10 = c08ab   c09ab   c10bd   c11bd */
			mulps	BOF(%rsp), rC4
	addps		rC2, rC12	/* rC12 = c12ac   c13ac   c12bd   c13bd */
			mulps	BOF(%rsp), rC5
	addps		rC10, rC11	/* rC11 = c08abcd c09abcd c10bdac c11bdac */
			mulps	BOF(%rsp), rC1

/* */

	movhlps		rC12, rC13	/* rC13 = c12bd   c13bd   X       X */
			addps	rA0, rC3
	addps		rC13, rC12	/* rC12 = c12abcd c13abcd X       X */
			addps	rC4, rC7
			addps	rC5, rC11
						prefB(320-176(pB,ldab))
			addps	rC1, rC12
   #endif

#else
	movlhps		rC4, rC5	/* rC5 = c5a    c5b    c4a    c5a */
	unpcklps	rC9 , rC8 	/* rC8  = c08a    c09a    c08b    c09b */
	movhlps		rC1, rC11	/* rC11 = c08d    c09d    c10c    c11c */
	movlhps		rC10, rC1	/* rC1 = c08c    c09c    c10a    c11a */
	movhlps		rC5, rC6	/* rC6 = c4a    c5a    c6b    c7b */
	movhlps		rC8 , rC0	/* rC0 = c08b    c09b    c10d    c11d */
	unpcklps	rC13, rC2	/* rC2  = c12a    c13a    c12b    c13b */
	addps		rC1, rC11	/* rC11 = c08cd   c09cd   c10ac   c11ac */
	addps		rB0, rC6	/* rC6 = c4ab   c5ab   c6bd   c7bd */
	movlhps		rC8 , rC9 	/* rC9  = c09a    c09b    c08a    c09a */
	unpckhps	rC13, rC12	/* rC12 = c12c    c13c    c12d    c13d */
	movhlps		rC9 , rC10	/* rC10 = c08a    c09a    c10b    c11b */
	addps		rC6, rC7	/* rC7 = c4abcd c5abcd c6bdac c7bdac */
	addps		rC0, rC10	/* rC10 = c08ab   c09ab   c10bd   c11bd */
	addps		rC2, rC12	/* rC12 = c12ac   c13ac   c12bd   c13bd */
	addps		rC10, rC11	/* rC11 = c08abcd c09abcd c10bdac c11bdac */

/* */

	movhlps		rC12, rC13	/* rC13 = c12bd   c13bd   X       X */
						prefB(320-176(pB,ldab))
	addps		rC13, rC12	/* rC12 = c12abcd c13abcd X       X */

#endif
/*
 *      Write results back to C;  pC += 14;
 */
#ifdef SREAL
	movups	rC3, (pC)
	movups	rC7, 16(pC)
	movups	rC11, 32(pC)
	movlps	rC12, 48(pC)
/*	addq	$56, pC */
#else
	movss	rC3, (pC)
	movss	rC7, 32(pC)
	movhlps	rC3, rC0
	movhlps	rC7, rC6
	movss	rC0, 16(pC)
	movss	rC6, 48(pC)
	shufps	$0x55, rC3, rC3
	shufps	$0x55, rC7, rC7
	movss	rC3, 8(pC)
	movss	rC7, 40(pC)
	shufps	$0x55, rC0, rC0
	shufps	$0x55, rC6, rC6
	movss	rC0, 24(pC)
	movss	rC6, 56(pC)

	movss	rC11, 64(pC)
	movhlps	rC11, rC2
	movss	rC12, 96(pC)
	movss	rC2, 80(pC)
	shufps	$0x55, rC11, rC11
	shufps	$0x55, rC12, rC12
	movss	rC11, 72(pC)
	shufps	$0x55, rC2, rC2
	movss	rC12, 104(pC)
	movss	rC2, 88(pC)

/*	addq	$112, pC */
#endif
/*
 *      Write results back to C
 */
/*
 *      pC += 14;  pA += 14*NB; pB -= NB;
 */
/*
 *      while (pA != stM);
 */
/*	subq	$1, stM */
/*	jne	UMLOOP */
/*
 *      pC += 14;  pA += 14*NB; pB -= NB;
 */
	subq	$NBNBso-NB14so+176, pA5
	subq	$NBNBso-NB14so+176, pA10
	addq	$NBso-176, pB0
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
/*	movq	-32(%rsp), %r12 */
/*	movq	-40(%rsp), %r13 */
	ret
