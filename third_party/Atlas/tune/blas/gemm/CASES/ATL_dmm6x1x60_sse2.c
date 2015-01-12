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
#include "atlas_asm.h"


#ifndef ATL_GAS_x8632
   #error "This kernel requires gas x86-32 assembler!"
#endif
#if !defined(NB) || (NB == 0)
   #error "NB must be a compile-time constant!"
#endif
#if (NB != 60)
   #error "NB must be 60!"
#endif
#if (NB/6)*6 != NB
   #error "NB must be multiple of 6!"
#endif
#ifndef ATL_SSE2
   #error "This routine requires SSE2!"
#endif

/*
 * Integer register usage shown be these defines
 */
#define pC      %esi
#define pA      %ecx
#define pB      %edi
#define incCn   %eax
#define stM     %edx
#define stN     %ebx
#define pfA     %ebp

#define rC0	%xmm0
#define rC1	%xmm1
#define rC2	%xmm2
#define rC3	%xmm3
#define rC4	%xmm4
#define rC5	%xmm5
#define rA0	%xmm6
#define rA1	%xmm7

#define NBso	(NB*8)
#define NBNBso  (NB*NB*8)
#define NB2so   (NBso+NBso)
#define NB3so   (NBso+NBso+NBso)
#define NB4so   (NBso+NBso+NBso+NBso)
#define NB5so   (NBso+NBso+NBso+NBso+NBso)
#define NB6so   (NBso+NBso+NBso+NBso+NBso+NBso)
#define NB7so   (NB6so+NBso)
#define NB8so   (NB6so+NB2so)
#define NB9so   (NB6so+NB3so)
#define NB10so   (NB6so+NB4so)
#define NB11so   (NB6so+NB5so)

/*
 * Prefetch defines
 */
#define pref2(mem) prefetcht1	mem
#define prefB(mem) prefetcht0	mem
#define prefC(mem) prefetchw	mem
/*
 *void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha,
 *                const TYPE *A, const int lda, const TYPE *B, const int ldb,
 *                const TYPE beta, TYPE *C, const int ldc)
 */
	.text
.global ATL_asmdecor(ATL_USERMM)
ATL_asmdecor(ATL_USERMM):
/*
 *      Save callee-saved iregs; Save old stack pointer in eax,
 *      so we can adjust for BETA alignment
 */
	movl %esp, %eax
   #ifdef BETAX
	subl	$36, %esp
	shr	$4, %esp
	shl	$4, %esp
	movl	%ebp, 32(%esp)
	movl	%ebx, 28(%esp)
	movl	%esi, 24(%esp)
	movl	%edi, 20(%esp)
	movl	%eax, 16(%esp)
	movlpd	40(%eax), rC0
	unpcklpd	rC0, rC0
	movapd	rC0, (%esp)
      #define BETAOFF 0
   #else
	subl	$16, %esp
	movl	%ebp, 12(%esp)
	movl	%ebx,  8(%esp)
	movl	%esi,  4(%esp)
	movl	%edi,   (%esp)
   #endif
/*
 *      Initialize pA = A;  pB = B; pC = C;
 */
	movl	48(%eax), pC
			prefC((pC))
			prefC(64(pC))
	movl	24(%eax), pA
	movl	32(%eax), pB
/*
 *      stM = pA + NBNB-6*NB;  pfA = pA+NBNB;  stN = pB + NBNB;
 */
	movl	$NBNBso-NB6so, stM
	addl	pA, stM
	xor	pfA, pfA
	movl	$NBNBso, stN
	addl	pB, stN
/*
 *      Set incCn = (ldc - NB + 6)*sizeof
 */
	movl	52(%eax), incCn
	subl	$MB-6, incCn
   #ifdef DCPLX
	shl	$4, incCn
   #else
	shl	$3, incCn
   #endif
NLOOP:
MLOOP:
#ifdef BETA0
	xorpd	rC0, rC0
	xorpd	rC1, rC1
	xorpd	rC2, rC2
	xorpd	rC3, rC3
	xorpd	rC4, rC4
	xorpd	rC5, rC5
#else
   #ifdef DCPLX
	movsd	(pC), rC0
	movsd	16(pC), rC1
	movsd	32(pC), rC2
	movsd	48(pC), rC3
	movsd	64(pC), rC4
	movsd	80(pC), rC5
   #else
	movsd	(pC), rC0
	movsd	8(pC), rC1
	movsd	16(pC), rC2
	movsd	24(pC), rC3
	movsd	32(pC), rC4
	movsd	40(pC), rC5
   #endif
   #ifdef BETAX
	movlpd	(%esp), rA0
	mulsd	rA0, rC0
	mulsd	rA0, rC1
	mulsd	rA0, rC2
	mulsd	rA0, rC3
	mulsd	rA0, rC4
	mulsd	rA0, rC5
   #endif
#endif
	movapd	0(pA), rA0
	mulpd	0(pB), rA0
	addpd	rA0, rC0
	movapd	0+NBso(pA), rA0
	mulpd	0(pB), rA0
	addpd	rA0, rC1
	movapd	0+NB2so(pA), rA0
	mulpd	0(pB), rA0
	addpd	rA0, rC2
	movapd	0+NB3so(pA), rA0
	mulpd	0(pB), rA0
	addpd	rA0, rC3
	movapd	0+NB4so(pA), rA0
	mulpd	0(pB), rA0
	addpd	rA0, rC4
	movapd	0+NB5so(pA), rA0
	mulpd	0(pB), rA0
	addpd	rA0, rC5

	movapd	16(pA), rA0
	mulpd	16(pB), rA0
	addpd	rA0, rC0
	movapd	16+NBso(pA), rA0
	mulpd	16(pB), rA0
	addpd	rA0, rC1
	movapd	16+NB2so(pA), rA0
	mulpd	16(pB), rA0
	addpd	rA0, rC2
	movapd	16+NB3so(pA), rA0
	mulpd	16(pB), rA0
	addpd	rA0, rC3
	movapd	16+NB4so(pA), rA0
	mulpd	16(pB), rA0
	addpd	rA0, rC4
	movapd	16+NB5so(pA), rA0
	mulpd	16(pB), rA0
	addpd	rA0, rC5

	movapd	32(pA), rA0
	mulpd	32(pB), rA0
	addpd	rA0, rC0
	movapd	32+NBso(pA), rA0
	mulpd	32(pB), rA0
	addpd	rA0, rC1
	movapd	32+NB2so(pA), rA0
	mulpd	32(pB), rA0
	addpd	rA0, rC2
	movapd	32+NB3so(pA), rA0
	mulpd	32(pB), rA0
	addpd	rA0, rC3
	movapd	32+NB4so(pA), rA0
	mulpd	32(pB), rA0
	addpd	rA0, rC4
	movapd	32+NB5so(pA), rA0
	mulpd	32(pB), rA0
	addpd	rA0, rC5

	movapd	48(pA), rA0
	mulpd	48(pB), rA0
	addpd	rA0, rC0
	movapd	48+NBso(pA), rA0
	mulpd	48(pB), rA0
	addpd	rA0, rC1
	movapd	48+NB2so(pA), rA0
	mulpd	48(pB), rA0
	addpd	rA0, rC2
	movapd	48+NB3so(pA), rA0
	mulpd	48(pB), rA0
	addpd	rA0, rC3
	movapd	48+NB4so(pA), rA0
	mulpd	48(pB), rA0
	addpd	rA0, rC4
	movapd	48+NB5so(pA), rA0
	mulpd	48(pB), rA0
	addpd	rA0, rC5

	movapd	64(pA), rA0
	mulpd	64(pB), rA0
	addpd	rA0, rC0
	movapd	64+NBso(pA), rA0
	mulpd	64(pB), rA0
	addpd	rA0, rC1
	movapd	64+NB2so(pA), rA0
	mulpd	64(pB), rA0
	addpd	rA0, rC2
	movapd	64+NB3so(pA), rA0
	mulpd	64(pB), rA0
	addpd	rA0, rC3
	movapd	64+NB4so(pA), rA0
	mulpd	64(pB), rA0
	addpd	rA0, rC4
	movapd	64+NB5so(pA), rA0
	mulpd	64(pB), rA0
	addpd	rA0, rC5

	movapd	80(pA), rA0
	mulpd	80(pB), rA0
	addpd	rA0, rC0
	movapd	80+NBso(pA), rA0
	mulpd	80(pB), rA0
	addpd	rA0, rC1
	movapd	80+NB2so(pA), rA0
	mulpd	80(pB), rA0
	addpd	rA0, rC2
	movapd	80+NB3so(pA), rA0
	mulpd	80(pB), rA0
	addpd	rA0, rC3
	movapd	80+NB4so(pA), rA0
	mulpd	80(pB), rA0
	addpd	rA0, rC4
	movapd	80+NB5so(pA), rA0
	mulpd	80(pB), rA0
	addpd	rA0, rC5

	movapd	96(pA), rA0
	mulpd	96(pB), rA0
	addpd	rA0, rC0
	movapd	96+NBso(pA), rA0
	mulpd	96(pB), rA0
	addpd	rA0, rC1
	movapd	96+NB2so(pA), rA0
	mulpd	96(pB), rA0
	addpd	rA0, rC2
	movapd	96+NB3so(pA), rA0
	mulpd	96(pB), rA0
	addpd	rA0, rC3
	movapd	96+NB4so(pA), rA0
	mulpd	96(pB), rA0
	addpd	rA0, rC4
	movapd	96+NB5so(pA), rA0
	mulpd	96(pB), rA0
	addpd	rA0, rC5

	movapd	112(pA), rA0
	mulpd	112(pB), rA0
	addpd	rA0, rC0
	movapd	112+NBso(pA), rA0
	mulpd	112(pB), rA0
	addpd	rA0, rC1
	movapd	112+NB2so(pA), rA0
	mulpd	112(pB), rA0
	addpd	rA0, rC2
	movapd	112+NB3so(pA), rA0
	mulpd	112(pB), rA0
	addpd	rA0, rC3
	movapd	112+NB4so(pA), rA0
	mulpd	112(pB), rA0
	addpd	rA0, rC4
	movapd	112+NB5so(pA), rA0
	mulpd	112(pB), rA0
	addpd	rA0, rC5

	movapd	128(pA), rA0
	mulpd	128(pB), rA0
	addpd	rA0, rC0
	movapd	128+NBso(pA), rA0
	mulpd	128(pB), rA0
	addpd	rA0, rC1
	movapd	128+NB2so(pA), rA0
	mulpd	128(pB), rA0
	addpd	rA0, rC2
	movapd	128+NB3so(pA), rA0
	mulpd	128(pB), rA0
	addpd	rA0, rC3
	movapd	128+NB4so(pA), rA0
	mulpd	128(pB), rA0
	addpd	rA0, rC4
	movapd	128+NB5so(pA), rA0
	mulpd	128(pB), rA0
	addpd	rA0, rC5

	movapd	144(pA), rA0
	mulpd	144(pB), rA0
	addpd	rA0, rC0
	movapd	144+NBso(pA), rA0
	mulpd	144(pB), rA0
	addpd	rA0, rC1
	movapd	144+NB2so(pA), rA0
	mulpd	144(pB), rA0
	addpd	rA0, rC2
	movapd	144+NB3so(pA), rA0
	mulpd	144(pB), rA0
	addpd	rA0, rC3
	movapd	144+NB4so(pA), rA0
	mulpd	144(pB), rA0
	addpd	rA0, rC4
	movapd	144+NB5so(pA), rA0
	mulpd	144(pB), rA0
	addpd	rA0, rC5

	movapd	160(pA), rA0
	mulpd	160(pB), rA0
	addpd	rA0, rC0
	movapd	160+NBso(pA), rA0
	mulpd	160(pB), rA0
	addpd	rA0, rC1
	movapd	160+NB2so(pA), rA0
	mulpd	160(pB), rA0
	addpd	rA0, rC2
	movapd	160+NB3so(pA), rA0
	mulpd	160(pB), rA0
	addpd	rA0, rC3
	movapd	160+NB4so(pA), rA0
	mulpd	160(pB), rA0
	addpd	rA0, rC4
	movapd	160+NB5so(pA), rA0
	mulpd	160(pB), rA0
	addpd	rA0, rC5

	movapd	176(pA), rA0
	mulpd	176(pB), rA0
	addpd	rA0, rC0
	movapd	176+NBso(pA), rA0
	mulpd	176(pB), rA0
	addpd	rA0, rC1
	movapd	176+NB2so(pA), rA0
	mulpd	176(pB), rA0
	addpd	rA0, rC2
	movapd	176+NB3so(pA), rA0
	mulpd	176(pB), rA0
	addpd	rA0, rC3
	movapd	176+NB4so(pA), rA0
	mulpd	176(pB), rA0
	addpd	rA0, rC4
	movapd	176+NB5so(pA), rA0
	mulpd	176(pB), rA0
	addpd	rA0, rC5

	movapd	192(pA), rA0
	mulpd	192(pB), rA0
	addpd	rA0, rC0
	movapd	192+NBso(pA), rA0
	mulpd	192(pB), rA0
	addpd	rA0, rC1
	movapd	192+NB2so(pA), rA0
	mulpd	192(pB), rA0
	addpd	rA0, rC2
	movapd	192+NB3so(pA), rA0
	mulpd	192(pB), rA0
	addpd	rA0, rC3
	movapd	192+NB4so(pA), rA0
	mulpd	192(pB), rA0
	addpd	rA0, rC4
	movapd	192+NB5so(pA), rA0
	mulpd	192(pB), rA0
	addpd	rA0, rC5

	movapd	208(pA), rA0
	mulpd	208(pB), rA0
	addpd	rA0, rC0
	movapd	208+NBso(pA), rA0
	mulpd	208(pB), rA0
	addpd	rA0, rC1
	movapd	208+NB2so(pA), rA0
	mulpd	208(pB), rA0
	addpd	rA0, rC2
	movapd	208+NB3so(pA), rA0
	mulpd	208(pB), rA0
	addpd	rA0, rC3
	movapd	208+NB4so(pA), rA0
	mulpd	208(pB), rA0
	addpd	rA0, rC4
	movapd	208+NB5so(pA), rA0
	mulpd	208(pB), rA0
	addpd	rA0, rC5

	movapd	224(pA), rA0
	mulpd	224(pB), rA0
	addpd	rA0, rC0
	movapd	224+NBso(pA), rA0
	mulpd	224(pB), rA0
	addpd	rA0, rC1
	movapd	224+NB2so(pA), rA0
	mulpd	224(pB), rA0
	addpd	rA0, rC2
	movapd	224+NB3so(pA), rA0
	mulpd	224(pB), rA0
	addpd	rA0, rC3
	movapd	224+NB4so(pA), rA0
	mulpd	224(pB), rA0
	addpd	rA0, rC4
	movapd	224+NB5so(pA), rA0
	mulpd	224(pB), rA0
	addpd	rA0, rC5

	movapd	240(pA), rA0
	mulpd	240(pB), rA0
	addpd	rA0, rC0
	movapd	240+NBso(pA), rA0
	mulpd	240(pB), rA0
	addpd	rA0, rC1
	movapd	240+NB2so(pA), rA0
	mulpd	240(pB), rA0
	addpd	rA0, rC2
	movapd	240+NB3so(pA), rA0
	mulpd	240(pB), rA0
	addpd	rA0, rC3
	movapd	240+NB4so(pA), rA0
	mulpd	240(pB), rA0
	addpd	rA0, rC4
	movapd	240+NB5so(pA), rA0
	mulpd	240(pB), rA0
	addpd	rA0, rC5

	movapd	256(pA), rA0
	mulpd	256(pB), rA0
	addpd	rA0, rC0
	movapd	256+NBso(pA), rA0
	mulpd	256(pB), rA0
	addpd	rA0, rC1
	movapd	256+NB2so(pA), rA0
	mulpd	256(pB), rA0
	addpd	rA0, rC2
	movapd	256+NB3so(pA), rA0
	mulpd	256(pB), rA0
	addpd	rA0, rC3
	movapd	256+NB4so(pA), rA0
	mulpd	256(pB), rA0
	addpd	rA0, rC4
	movapd	256+NB5so(pA), rA0
	mulpd	256(pB), rA0
	addpd	rA0, rC5

	movapd	272(pA), rA0
	mulpd	272(pB), rA0
	addpd	rA0, rC0
	movapd	272+NBso(pA), rA0
	mulpd	272(pB), rA0
	addpd	rA0, rC1
	movapd	272+NB2so(pA), rA0
	mulpd	272(pB), rA0
	addpd	rA0, rC2
	movapd	272+NB3so(pA), rA0
	mulpd	272(pB), rA0
	addpd	rA0, rC3
	movapd	272+NB4so(pA), rA0
	mulpd	272(pB), rA0
	addpd	rA0, rC4
	movapd	272+NB5so(pA), rA0
	mulpd	272(pB), rA0
	addpd	rA0, rC5

	movapd	288(pA), rA0
	mulpd	288(pB), rA0
	addpd	rA0, rC0
	movapd	288+NBso(pA), rA0
	mulpd	288(pB), rA0
	addpd	rA0, rC1
	movapd	288+NB2so(pA), rA0
	mulpd	288(pB), rA0
	addpd	rA0, rC2
	movapd	288+NB3so(pA), rA0
	mulpd	288(pB), rA0
	addpd	rA0, rC3
	movapd	288+NB4so(pA), rA0
	mulpd	288(pB), rA0
	addpd	rA0, rC4
	movapd	288+NB5so(pA), rA0
	mulpd	288(pB), rA0
	addpd	rA0, rC5

	movapd	304(pA), rA0
	mulpd	304(pB), rA0
	addpd	rA0, rC0
	movapd	304+NBso(pA), rA0
	mulpd	304(pB), rA0
	addpd	rA0, rC1
	movapd	304+NB2so(pA), rA0
	mulpd	304(pB), rA0
	addpd	rA0, rC2
	movapd	304+NB3so(pA), rA0
	mulpd	304(pB), rA0
	addpd	rA0, rC3
	movapd	304+NB4so(pA), rA0
	mulpd	304(pB), rA0
	addpd	rA0, rC4
	movapd	304+NB5so(pA), rA0
	mulpd	304(pB), rA0
	addpd	rA0, rC5

	movapd	320(pA), rA0
	mulpd	320(pB), rA0
	addpd	rA0, rC0
	movapd	320+NBso(pA), rA0
	mulpd	320(pB), rA0
	addpd	rA0, rC1
	movapd	320+NB2so(pA), rA0
	mulpd	320(pB), rA0
	addpd	rA0, rC2
	movapd	320+NB3so(pA), rA0
	mulpd	320(pB), rA0
	addpd	rA0, rC3
	movapd	320+NB4so(pA), rA0
	mulpd	320(pB), rA0
	addpd	rA0, rC4
	movapd	320+NB5so(pA), rA0
	mulpd	320(pB), rA0
	addpd	rA0, rC5

	movapd	336(pA), rA0
	mulpd	336(pB), rA0
	addpd	rA0, rC0
	movapd	336+NBso(pA), rA0
	mulpd	336(pB), rA0
	addpd	rA0, rC1
	movapd	336+NB2so(pA), rA0
	mulpd	336(pB), rA0
	addpd	rA0, rC2
	movapd	336+NB3so(pA), rA0
	mulpd	336(pB), rA0
	addpd	rA0, rC3
	movapd	336+NB4so(pA), rA0
	mulpd	336(pB), rA0
	addpd	rA0, rC4
	movapd	336+NB5so(pA), rA0
	mulpd	336(pB), rA0
	addpd	rA0, rC5

	movapd	352(pA), rA0
	mulpd	352(pB), rA0
	addpd	rA0, rC0
	movapd	352+NBso(pA), rA0
	mulpd	352(pB), rA0
	addpd	rA0, rC1
	movapd	352+NB2so(pA), rA0
	mulpd	352(pB), rA0
	addpd	rA0, rC2
	movapd	352+NB3so(pA), rA0
	mulpd	352(pB), rA0
	addpd	rA0, rC3
	movapd	352+NB4so(pA), rA0
	mulpd	352(pB), rA0
	addpd	rA0, rC4
	movapd	352+NB5so(pA), rA0
	mulpd	352(pB), rA0
	addpd	rA0, rC5

	movapd	368(pA), rA0
	mulpd	368(pB), rA0
	addpd	rA0, rC0
	movapd	368+NBso(pA), rA0
	mulpd	368(pB), rA0
	addpd	rA0, rC1
	movapd	368+NB2so(pA), rA0
	mulpd	368(pB), rA0
	addpd	rA0, rC2
	movapd	368+NB3so(pA), rA0
	mulpd	368(pB), rA0
	addpd	rA0, rC3
	movapd	368+NB4so(pA), rA0
	mulpd	368(pB), rA0
	addpd	rA0, rC4
	movapd	368+NB5so(pA), rA0
	mulpd	368(pB), rA0
	addpd	rA0, rC5

	movapd	384(pA), rA0
	mulpd	384(pB), rA0
	addpd	rA0, rC0
	movapd	384+NBso(pA), rA0
	mulpd	384(pB), rA0
	addpd	rA0, rC1
	movapd	384+NB2so(pA), rA0
	mulpd	384(pB), rA0
	addpd	rA0, rC2
	movapd	384+NB3so(pA), rA0
	mulpd	384(pB), rA0
	addpd	rA0, rC3
	movapd	384+NB4so(pA), rA0
	mulpd	384(pB), rA0
	addpd	rA0, rC4
	movapd	384+NB5so(pA), rA0
	mulpd	384(pB), rA0
	addpd	rA0, rC5

	movapd	400(pA), rA0
	mulpd	400(pB), rA0
	addpd	rA0, rC0
	movapd	400+NBso(pA), rA0
	mulpd	400(pB), rA0
	addpd	rA0, rC1
	movapd	400+NB2so(pA), rA0
	mulpd	400(pB), rA0
	addpd	rA0, rC2
	movapd	400+NB3so(pA), rA0
	mulpd	400(pB), rA0
	addpd	rA0, rC3
	movapd	400+NB4so(pA), rA0
	mulpd	400(pB), rA0
	addpd	rA0, rC4
	movapd	400+NB5so(pA), rA0
	mulpd	400(pB), rA0
	addpd	rA0, rC5

	movapd	416(pA), rA0
	mulpd	416(pB), rA0
	addpd	rA0, rC0
	movapd	416+NBso(pA), rA0
	mulpd	416(pB), rA0
	addpd	rA0, rC1
	movapd	416+NB2so(pA), rA0
	mulpd	416(pB), rA0
	addpd	rA0, rC2
	movapd	416+NB3so(pA), rA0
	mulpd	416(pB), rA0
	addpd	rA0, rC3
	movapd	416+NB4so(pA), rA0
	mulpd	416(pB), rA0
	addpd	rA0, rC4
	movapd	416+NB5so(pA), rA0
	mulpd	416(pB), rA0
	addpd	rA0, rC5

	movapd	432(pA), rA0
	mulpd	432(pB), rA0
	addpd	rA0, rC0
	movapd	432+NBso(pA), rA0
	mulpd	432(pB), rA0
	addpd	rA0, rC1
	movapd	432+NB2so(pA), rA0
	mulpd	432(pB), rA0
	addpd	rA0, rC2
	movapd	432+NB3so(pA), rA0
	mulpd	432(pB), rA0
	addpd	rA0, rC3
	movapd	432+NB4so(pA), rA0
	mulpd	432(pB), rA0
	addpd	rA0, rC4
	movapd	432+NB5so(pA), rA0
	mulpd	432(pB), rA0
	addpd	rA0, rC5

	movapd	448(pA), rA0
	mulpd	448(pB), rA0
	addpd	rA0, rC0
	movapd	448+NBso(pA), rA0
	mulpd	448(pB), rA0
	addpd	rA0, rC1
	movapd	448+NB2so(pA), rA0
	mulpd	448(pB), rA0
	addpd	rA0, rC2
	movapd	448+NB3so(pA), rA0
	mulpd	448(pB), rA0
	addpd	rA0, rC3
	movapd	448+NB4so(pA), rA0
	mulpd	448(pB), rA0
	addpd	rA0, rC4
	movapd	448+NB5so(pA), rA0
	mulpd	448(pB), rA0
	addpd	rA0, rC5
			prefC(64(pC))

	movapd	464(pA), rA0
	mulpd	464(pB), rA0
	addpd	rA0, rC0
	movapd	464+NBso(pA), rA0
	mulpd	464(pB), rA0
	addpd	rA0, rC1
	movapd	464+NB2so(pA), rA0
	mulpd	464(pB), rA0
	addpd	rA0, rC2
	movapd	464+NB3so(pA), rA0
	mulpd	464(pB), rA0
	addpd	rA0, rC3
	movapd	464+NB4so(pA), rA0
	mulpd	464(pB), rA0
	addpd	rA0, rC4
	movapd	464+NB5so(pA), rA0
	mulpd	464(pB), rA0
	addpd	rA0, rC5

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
        movapd          rC0, rA0
        movapd          rC4, rA1
        unpcklpd        rC1, rC0        /* rC0 = c0a  c1a */
        unpcklpd        rC5, rC4        /* rC4 = c4a  c5a */
/*			pref2((stN,pfA)) */
        unpckhpd        rC1, rA0        /* rA0 = c0b  c1b */
        unpckhpd        rC5, rA1        /* rA1 = c4b  c5b */
        addpd           rA0, rC0        /* rC0 = c0ab c1ab */
        addpd           rA1, rC4        /* rC4 = c4ab c5ab */
        movapd          rC2, rA0
        unpcklpd        rC3, rC2        /* rC2 = c2a  c3a */
			pref2(NB6so(stM,pfA))
			addl	$54, pfA
        unpckhpd        rC3, rA0        /* rA0 = c2b  c3b */
        addpd           rA0, rC2        /* rC2 = c2ab c3ab */
/*
 *      Write results back to C
 */
   #ifdef DCPLX
	movlpd	rC0, (pC)
	movhpd	rC0, 16(pC)
	movlpd	rC2, 32(pC)
	movhpd	rC2, 48(pC)
	movlpd	rC4, 64(pC)
	movhpd	rC4, 80(pC)
   #else
	movupd	rC0, (pC)
	movupd	rC2, 16(pC)
	movupd	rC4, 32(pC)
   #endif
/*
 *      pC += 6;   pA += 6*NB
 */
   #ifdef DCPLX
	addl	$96, pC
   #else
	addl	$48, pC
   #endif
	addl	$NB6so, pA
/*
 *      while (pA != stM);
 */
	cmp	pA, stM
	jne	MLOOP
/*
 *      Last M-loop iteration unrolled to prefetch next col of B
 */
			prefB(NBso(pB))
#ifdef BETA0
	xorpd	rC0, rC0
	xorpd	rC1, rC1
	xorpd	rC2, rC2
	xorpd	rC3, rC3
	xorpd	rC4, rC4
	xorpd	rC5, rC5
#else
   #ifdef DCPLX
	movsd	(pC), rC0
	movsd	16(pC), rC1
	movsd	32(pC), rC2
	movsd	48(pC), rC3
	movsd	64(pC), rC4
	movsd	80(pC), rC5
   #else
	movsd	(pC), rC0
	movsd	8(pC), rC1
	movsd	16(pC), rC2
	movsd	24(pC), rC3
	movsd	32(pC), rC4
	movsd	40(pC), rC5
   #endif
   #ifdef BETAX
	movlpd	(%esp), rA0
	mulsd	rA0, rC0
	mulsd	rA0, rC1
	mulsd	rA0, rC2
	mulsd	rA0, rC3
	mulsd	rA0, rC4
	mulsd	rA0, rC5
   #endif
#endif
	movapd	0(pA), rA0
	mulpd	0(pB), rA0
	addpd	rA0, rC0
	movapd	0+NBso(pA), rA0
	mulpd	0(pB), rA0
	addpd	rA0, rC1
	movapd	0+NB2so(pA), rA0
	mulpd	0(pB), rA0
	addpd	rA0, rC2
	movapd	0+NB3so(pA), rA0
	mulpd	0(pB), rA0
	addpd	rA0, rC3
	movapd	0+NB4so(pA), rA0
	mulpd	0(pB), rA0
	addpd	rA0, rC4
	movapd	0+NB5so(pA), rA0
	mulpd	0(pB), rA0
	addpd	rA0, rC5
			prefB(64+NBso(pB))

	movapd	16(pA), rA0
	mulpd	16(pB), rA0
	addpd	rA0, rC0
	movapd	16+NBso(pA), rA0
	mulpd	16(pB), rA0
	addpd	rA0, rC1
	movapd	16+NB2so(pA), rA0
	mulpd	16(pB), rA0
	addpd	rA0, rC2
	movapd	16+NB3so(pA), rA0
	mulpd	16(pB), rA0
	addpd	rA0, rC3
	movapd	16+NB4so(pA), rA0
	mulpd	16(pB), rA0
	addpd	rA0, rC4
	movapd	16+NB5so(pA), rA0
	mulpd	16(pB), rA0
	addpd	rA0, rC5
			prefB(128+NBso(pB))

	movapd	32(pA), rA0
	mulpd	32(pB), rA0
	addpd	rA0, rC0
	movapd	32+NBso(pA), rA0
	mulpd	32(pB), rA0
	addpd	rA0, rC1
	movapd	32+NB2so(pA), rA0
	mulpd	32(pB), rA0
	addpd	rA0, rC2
	movapd	32+NB3so(pA), rA0
	mulpd	32(pB), rA0
	addpd	rA0, rC3
	movapd	32+NB4so(pA), rA0
	mulpd	32(pB), rA0
	addpd	rA0, rC4
	movapd	32+NB5so(pA), rA0
	mulpd	32(pB), rA0
	addpd	rA0, rC5

	movapd	48(pA), rA0
	mulpd	48(pB), rA0
	addpd	rA0, rC0
	movapd	48+NBso(pA), rA0
	mulpd	48(pB), rA0
	addpd	rA0, rC1
	movapd	48+NB2so(pA), rA0
	mulpd	48(pB), rA0
	addpd	rA0, rC2
	movapd	48+NB3so(pA), rA0
	mulpd	48(pB), rA0
	addpd	rA0, rC3
	movapd	48+NB4so(pA), rA0
	mulpd	48(pB), rA0
	addpd	rA0, rC4
	movapd	48+NB5so(pA), rA0
	mulpd	48(pB), rA0
	addpd	rA0, rC5

	movapd	64(pA), rA0
	mulpd	64(pB), rA0
	addpd	rA0, rC0
	movapd	64+NBso(pA), rA0
	mulpd	64(pB), rA0
	addpd	rA0, rC1
	movapd	64+NB2so(pA), rA0
	mulpd	64(pB), rA0
	addpd	rA0, rC2
	movapd	64+NB3so(pA), rA0
	mulpd	64(pB), rA0
	addpd	rA0, rC3
	movapd	64+NB4so(pA), rA0
	mulpd	64(pB), rA0
	addpd	rA0, rC4
	movapd	64+NB5so(pA), rA0
	mulpd	64(pB), rA0
	addpd	rA0, rC5

	movapd	80(pA), rA0
	mulpd	80(pB), rA0
	addpd	rA0, rC0
	movapd	80+NBso(pA), rA0
	mulpd	80(pB), rA0
	addpd	rA0, rC1
	movapd	80+NB2so(pA), rA0
	mulpd	80(pB), rA0
	addpd	rA0, rC2
	movapd	80+NB3so(pA), rA0
	mulpd	80(pB), rA0
	addpd	rA0, rC3
	movapd	80+NB4so(pA), rA0
	mulpd	80(pB), rA0
	addpd	rA0, rC4
	movapd	80+NB5so(pA), rA0
	mulpd	80(pB), rA0
	addpd	rA0, rC5
			prefB(192+NBso(pB))

	movapd	96(pA), rA0
	mulpd	96(pB), rA0
	addpd	rA0, rC0
	movapd	96+NBso(pA), rA0
	mulpd	96(pB), rA0
	addpd	rA0, rC1
	movapd	96+NB2so(pA), rA0
	mulpd	96(pB), rA0
	addpd	rA0, rC2
	movapd	96+NB3so(pA), rA0
	mulpd	96(pB), rA0
	addpd	rA0, rC3
	movapd	96+NB4so(pA), rA0
	mulpd	96(pB), rA0
	addpd	rA0, rC4
	movapd	96+NB5so(pA), rA0
	mulpd	96(pB), rA0
	addpd	rA0, rC5

	movapd	112(pA), rA0
	mulpd	112(pB), rA0
	addpd	rA0, rC0
	movapd	112+NBso(pA), rA0
	mulpd	112(pB), rA0
	addpd	rA0, rC1
	movapd	112+NB2so(pA), rA0
	mulpd	112(pB), rA0
	addpd	rA0, rC2
	movapd	112+NB3so(pA), rA0
	mulpd	112(pB), rA0
	addpd	rA0, rC3
	movapd	112+NB4so(pA), rA0
	mulpd	112(pB), rA0
	addpd	rA0, rC4
	movapd	112+NB5so(pA), rA0
	mulpd	112(pB), rA0
	addpd	rA0, rC5

	movapd	128(pA), rA0
	mulpd	128(pB), rA0
	addpd	rA0, rC0
	movapd	128+NBso(pA), rA0
	mulpd	128(pB), rA0
	addpd	rA0, rC1
	movapd	128+NB2so(pA), rA0
	mulpd	128(pB), rA0
	addpd	rA0, rC2
	movapd	128+NB3so(pA), rA0
	mulpd	128(pB), rA0
	addpd	rA0, rC3
	movapd	128+NB4so(pA), rA0
	mulpd	128(pB), rA0
	addpd	rA0, rC4
	movapd	128+NB5so(pA), rA0
	mulpd	128(pB), rA0
	addpd	rA0, rC5

	movapd	144(pA), rA0
	mulpd	144(pB), rA0
	addpd	rA0, rC0
	movapd	144+NBso(pA), rA0
	mulpd	144(pB), rA0
	addpd	rA0, rC1
	movapd	144+NB2so(pA), rA0
	mulpd	144(pB), rA0
	addpd	rA0, rC2
	movapd	144+NB3so(pA), rA0
	mulpd	144(pB), rA0
	addpd	rA0, rC3
	movapd	144+NB4so(pA), rA0
	mulpd	144(pB), rA0
	addpd	rA0, rC4
	movapd	144+NB5so(pA), rA0
	mulpd	144(pB), rA0
	addpd	rA0, rC5

	movapd	160(pA), rA0
	mulpd	160(pB), rA0
	addpd	rA0, rC0
	movapd	160+NBso(pA), rA0
	mulpd	160(pB), rA0
	addpd	rA0, rC1
	movapd	160+NB2so(pA), rA0
	mulpd	160(pB), rA0
	addpd	rA0, rC2
	movapd	160+NB3so(pA), rA0
	mulpd	160(pB), rA0
	addpd	rA0, rC3
	movapd	160+NB4so(pA), rA0
	mulpd	160(pB), rA0
	addpd	rA0, rC4
	movapd	160+NB5so(pA), rA0
	mulpd	160(pB), rA0
	addpd	rA0, rC5

	movapd	176(pA), rA0
	mulpd	176(pB), rA0
	addpd	rA0, rC0
	movapd	176+NBso(pA), rA0
	mulpd	176(pB), rA0
	addpd	rA0, rC1
	movapd	176+NB2so(pA), rA0
	mulpd	176(pB), rA0
	addpd	rA0, rC2
	movapd	176+NB3so(pA), rA0
	mulpd	176(pB), rA0
	addpd	rA0, rC3
	movapd	176+NB4so(pA), rA0
	mulpd	176(pB), rA0
	addpd	rA0, rC4
	movapd	176+NB5so(pA), rA0
	mulpd	176(pB), rA0
	addpd	rA0, rC5

	movapd	192(pA), rA0
	mulpd	192(pB), rA0
	addpd	rA0, rC0
	movapd	192+NBso(pA), rA0
	mulpd	192(pB), rA0
	addpd	rA0, rC1
	movapd	192+NB2so(pA), rA0
	mulpd	192(pB), rA0
	addpd	rA0, rC2
	movapd	192+NB3so(pA), rA0
	mulpd	192(pB), rA0
	addpd	rA0, rC3
	movapd	192+NB4so(pA), rA0
	mulpd	192(pB), rA0
	addpd	rA0, rC4
	movapd	192+NB5so(pA), rA0
	mulpd	192(pB), rA0
	addpd	rA0, rC5

	movapd	208(pA), rA0
	mulpd	208(pB), rA0
	addpd	rA0, rC0
	movapd	208+NBso(pA), rA0
	mulpd	208(pB), rA0
	addpd	rA0, rC1
	movapd	208+NB2so(pA), rA0
	mulpd	208(pB), rA0
	addpd	rA0, rC2
	movapd	208+NB3so(pA), rA0
	mulpd	208(pB), rA0
	addpd	rA0, rC3
	movapd	208+NB4so(pA), rA0
	mulpd	208(pB), rA0
	addpd	rA0, rC4
	movapd	208+NB5so(pA), rA0
	mulpd	208(pB), rA0
	addpd	rA0, rC5

	movapd	224(pA), rA0
	mulpd	224(pB), rA0
	addpd	rA0, rC0
	movapd	224+NBso(pA), rA0
	mulpd	224(pB), rA0
	addpd	rA0, rC1
	movapd	224+NB2so(pA), rA0
	mulpd	224(pB), rA0
	addpd	rA0, rC2
	movapd	224+NB3so(pA), rA0
	mulpd	224(pB), rA0
	addpd	rA0, rC3
	movapd	224+NB4so(pA), rA0
	mulpd	224(pB), rA0
	addpd	rA0, rC4
	movapd	224+NB5so(pA), rA0
	mulpd	224(pB), rA0
	addpd	rA0, rC5

	movapd	240(pA), rA0
	mulpd	240(pB), rA0
	addpd	rA0, rC0
	movapd	240+NBso(pA), rA0
	mulpd	240(pB), rA0
	addpd	rA0, rC1
	movapd	240+NB2so(pA), rA0
	mulpd	240(pB), rA0
	addpd	rA0, rC2
	movapd	240+NB3so(pA), rA0
	mulpd	240(pB), rA0
	addpd	rA0, rC3
	movapd	240+NB4so(pA), rA0
	mulpd	240(pB), rA0
	addpd	rA0, rC4
	movapd	240+NB5so(pA), rA0
	mulpd	240(pB), rA0
	addpd	rA0, rC5

	movapd	256(pA), rA0
	mulpd	256(pB), rA0
	addpd	rA0, rC0
	movapd	256+NBso(pA), rA0
	mulpd	256(pB), rA0
	addpd	rA0, rC1
	movapd	256+NB2so(pA), rA0
	mulpd	256(pB), rA0
	addpd	rA0, rC2
	movapd	256+NB3so(pA), rA0
	mulpd	256(pB), rA0
	addpd	rA0, rC3
	movapd	256+NB4so(pA), rA0
	mulpd	256(pB), rA0
	addpd	rA0, rC4
	movapd	256+NB5so(pA), rA0
	mulpd	256(pB), rA0
	addpd	rA0, rC5

	movapd	272(pA), rA0
	mulpd	272(pB), rA0
	addpd	rA0, rC0
	movapd	272+NBso(pA), rA0
	mulpd	272(pB), rA0
	addpd	rA0, rC1
	movapd	272+NB2so(pA), rA0
	mulpd	272(pB), rA0
	addpd	rA0, rC2
	movapd	272+NB3so(pA), rA0
	mulpd	272(pB), rA0
	addpd	rA0, rC3
	movapd	272+NB4so(pA), rA0
	mulpd	272(pB), rA0
	addpd	rA0, rC4
	movapd	272+NB5so(pA), rA0
	mulpd	272(pB), rA0
	addpd	rA0, rC5

	movapd	288(pA), rA0
	mulpd	288(pB), rA0
	addpd	rA0, rC0
	movapd	288+NBso(pA), rA0
	mulpd	288(pB), rA0
	addpd	rA0, rC1
	movapd	288+NB2so(pA), rA0
	mulpd	288(pB), rA0
	addpd	rA0, rC2
	movapd	288+NB3so(pA), rA0
	mulpd	288(pB), rA0
	addpd	rA0, rC3
	movapd	288+NB4so(pA), rA0
	mulpd	288(pB), rA0
	addpd	rA0, rC4
	movapd	288+NB5so(pA), rA0
	mulpd	288(pB), rA0
	addpd	rA0, rC5

	movapd	304(pA), rA0
	mulpd	304(pB), rA0
	addpd	rA0, rC0
	movapd	304+NBso(pA), rA0
	mulpd	304(pB), rA0
	addpd	rA0, rC1
	movapd	304+NB2so(pA), rA0
	mulpd	304(pB), rA0
	addpd	rA0, rC2
	movapd	304+NB3so(pA), rA0
	mulpd	304(pB), rA0
	addpd	rA0, rC3
	movapd	304+NB4so(pA), rA0
	mulpd	304(pB), rA0
	addpd	rA0, rC4
	movapd	304+NB5so(pA), rA0
	mulpd	304(pB), rA0
	addpd	rA0, rC5

	movapd	320(pA), rA0
	mulpd	320(pB), rA0
	addpd	rA0, rC0
	movapd	320+NBso(pA), rA0
	mulpd	320(pB), rA0
	addpd	rA0, rC1
	movapd	320+NB2so(pA), rA0
	mulpd	320(pB), rA0
	addpd	rA0, rC2
	movapd	320+NB3so(pA), rA0
	mulpd	320(pB), rA0
	addpd	rA0, rC3
	movapd	320+NB4so(pA), rA0
	mulpd	320(pB), rA0
	addpd	rA0, rC4
	movapd	320+NB5so(pA), rA0
	mulpd	320(pB), rA0
	addpd	rA0, rC5

	movapd	336(pA), rA0
	mulpd	336(pB), rA0
	addpd	rA0, rC0
	movapd	336+NBso(pA), rA0
	mulpd	336(pB), rA0
	addpd	rA0, rC1
	movapd	336+NB2so(pA), rA0
	mulpd	336(pB), rA0
	addpd	rA0, rC2
	movapd	336+NB3so(pA), rA0
	mulpd	336(pB), rA0
	addpd	rA0, rC3
	movapd	336+NB4so(pA), rA0
	mulpd	336(pB), rA0
	addpd	rA0, rC4
	movapd	336+NB5so(pA), rA0
	mulpd	336(pB), rA0
	addpd	rA0, rC5

	movapd	352(pA), rA0
	mulpd	352(pB), rA0
	addpd	rA0, rC0
	movapd	352+NBso(pA), rA0
	mulpd	352(pB), rA0
	addpd	rA0, rC1
	movapd	352+NB2so(pA), rA0
	mulpd	352(pB), rA0
	addpd	rA0, rC2
	movapd	352+NB3so(pA), rA0
	mulpd	352(pB), rA0
	addpd	rA0, rC3
	movapd	352+NB4so(pA), rA0
	mulpd	352(pB), rA0
	addpd	rA0, rC4
	movapd	352+NB5so(pA), rA0
	mulpd	352(pB), rA0
	addpd	rA0, rC5

	movapd	368(pA), rA0
	mulpd	368(pB), rA0
	addpd	rA0, rC0
	movapd	368+NBso(pA), rA0
	mulpd	368(pB), rA0
	addpd	rA0, rC1
	movapd	368+NB2so(pA), rA0
	mulpd	368(pB), rA0
	addpd	rA0, rC2
	movapd	368+NB3so(pA), rA0
	mulpd	368(pB), rA0
	addpd	rA0, rC3
	movapd	368+NB4so(pA), rA0
	mulpd	368(pB), rA0
	addpd	rA0, rC4
	movapd	368+NB5so(pA), rA0
	mulpd	368(pB), rA0
	addpd	rA0, rC5

	movapd	384(pA), rA0
	mulpd	384(pB), rA0
	addpd	rA0, rC0
	movapd	384+NBso(pA), rA0
	mulpd	384(pB), rA0
	addpd	rA0, rC1
	movapd	384+NB2so(pA), rA0
	mulpd	384(pB), rA0
	addpd	rA0, rC2
	movapd	384+NB3so(pA), rA0
	mulpd	384(pB), rA0
	addpd	rA0, rC3
	movapd	384+NB4so(pA), rA0
	mulpd	384(pB), rA0
	addpd	rA0, rC4
	movapd	384+NB5so(pA), rA0
	mulpd	384(pB), rA0
	addpd	rA0, rC5

	movapd	400(pA), rA0
	mulpd	400(pB), rA0
	addpd	rA0, rC0
	movapd	400+NBso(pA), rA0
	mulpd	400(pB), rA0
	addpd	rA0, rC1
	movapd	400+NB2so(pA), rA0
	mulpd	400(pB), rA0
	addpd	rA0, rC2
	movapd	400+NB3so(pA), rA0
	mulpd	400(pB), rA0
	addpd	rA0, rC3
	movapd	400+NB4so(pA), rA0
	mulpd	400(pB), rA0
	addpd	rA0, rC4
	movapd	400+NB5so(pA), rA0
	mulpd	400(pB), rA0
	addpd	rA0, rC5
			prefB(256+NBso(pB))
			prefB(320+NBso(pB))

	movapd	416(pA), rA0
	mulpd	416(pB), rA0
	addpd	rA0, rC0
	movapd	416+NBso(pA), rA0
	mulpd	416(pB), rA0
	addpd	rA0, rC1
	movapd	416+NB2so(pA), rA0
	mulpd	416(pB), rA0
	addpd	rA0, rC2
	movapd	416+NB3so(pA), rA0
	mulpd	416(pB), rA0
	addpd	rA0, rC3
	movapd	416+NB4so(pA), rA0
	mulpd	416(pB), rA0
	addpd	rA0, rC4
	movapd	416+NB5so(pA), rA0
	mulpd	416(pB), rA0
	addpd	rA0, rC5

	movapd	432(pA), rA0
	mulpd	432(pB), rA0
	addpd	rA0, rC0
	movapd	432+NBso(pA), rA0
	mulpd	432(pB), rA0
	addpd	rA0, rC1
	movapd	432+NB2so(pA), rA0
	mulpd	432(pB), rA0
	addpd	rA0, rC2
	movapd	432+NB3so(pA), rA0
	mulpd	432(pB), rA0
	addpd	rA0, rC3
	movapd	432+NB4so(pA), rA0
	mulpd	432(pB), rA0
	addpd	rA0, rC4
	movapd	432+NB5so(pA), rA0
	mulpd	432(pB), rA0
	addpd	rA0, rC5

	movapd	448(pA), rA0
	mulpd	448(pB), rA0
	addpd	rA0, rC0
	movapd	448+NBso(pA), rA0
	mulpd	448(pB), rA0
	addpd	rA0, rC1
	movapd	448+NB2so(pA), rA0
	mulpd	448(pB), rA0
	addpd	rA0, rC2
	movapd	448+NB3so(pA), rA0
	mulpd	448(pB), rA0
	addpd	rA0, rC3
	movapd	448+NB4so(pA), rA0
	mulpd	448(pB), rA0
	addpd	rA0, rC4
	movapd	448+NB5so(pA), rA0
	mulpd	448(pB), rA0
	addpd	rA0, rC5

	movapd	464(pA), rA0
	mulpd	464(pB), rA0
	addpd	rA0, rC0
	movapd	464+NBso(pA), rA0
	mulpd	464(pB), rA0
	addpd	rA0, rC1
	movapd	464+NB2so(pA), rA0
	mulpd	464(pB), rA0
	addpd	rA0, rC2
	movapd	464+NB3so(pA), rA0
	mulpd	464(pB), rA0
	addpd	rA0, rC3
			prefC((pC,incCn))
	movapd	464+NB4so(pA), rA0
	mulpd	464(pB), rA0
	addpd	rA0, rC4
	movapd	464+NB5so(pA), rA0
	mulpd	464(pB), rA0
	addpd	rA0, rC5

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
        movapd          rC0, rA0
        movapd          rC4, rA1
        unpcklpd        rC1, rC0        /* rC0 = c0a  c1a */
        unpcklpd        rC5, rC4        /* rC4 = c4a  c5a */
        unpckhpd        rC1, rA0        /* rA0 = c0b  c1b */
        unpckhpd        rC5, rA1        /* rA1 = c4b  c5b */
        addpd           rA0, rC0        /* rC0 = c0ab c1ab */
        addpd           rA1, rC4        /* rC4 = c4ab c5ab */
        movapd          rC2, rA0
        unpcklpd        rC3, rC2        /* rC2 = c2a  c3a */
			prefB(384+NBso(pB))
			prefB(448+NBso(pB))
        unpckhpd        rC3, rA0        /* rA0 = c2b  c3b */
        addpd           rA0, rC2        /* rC2 = c2ab c3ab */
/*
 *      Write results back to C
 */
   #ifdef DCPLX
	movlpd	rC0, (pC)
	movhpd	rC0, 16(pC)
	movlpd	rC2, 32(pC)
	movhpd	rC2, 48(pC)
	movlpd	rC4, 64(pC)
	movhpd	rC4, 80(pC)
   #else
	movupd	rC0, (pC)
	movupd	rC2, 16(pC)
	movupd	rC4, 32(pC)
   #endif
/*
 *      pC += 6;   pA += 6*NB
 */
/*	addl	$NB6so, pA */

/*
 *      pC += incCn;  pA -= NBNB;  pB += NB;
 */
	addl	incCn, pC
	subl	$NBNBso-NB6so, pA
	addl	$NBso, pB
/*
 *      while (pB != stN);
 */
	cmp	pB, stN
	jne	NLOOP

/*
 *      Restore callee-saved iregs
 */
   #ifdef BETAX
	movl	32(%esp), %ebp
	movl	28(%esp), %ebx
	movl	24(%esp), %esi
	movl	20(%esp), %edi
	movl	16(%esp), %esp
   #else
	movl	12(%esp), %ebp
	movl	 8(%esp), %ebx
	movl	 4(%esp), %esi
	movl	  (%esp), %edi
	addl	$16, %esp
   #endif
	ret
