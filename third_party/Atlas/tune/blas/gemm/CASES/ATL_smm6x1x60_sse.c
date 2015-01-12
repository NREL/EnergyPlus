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
#ifndef ATL_SSE1
   #error "This routine requires SSE1!"
#endif

#if !defined(NB) || (NB == 0)
   #error "NB must be a compile-time constant!"
#endif
/*
#if (NB != 60)
   #error "NB must be 60!"
#endif
*/
#if (NB/6)*6 != NB
   #error "NB must be multiple of 6!"
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
#define rB0	%xmm7

#define NBso	(NB*4)
#define NBNBso  (NB*NB*4)
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
#define prefA(mem) prefetcht0	mem
#define prefB(mem) prefetcht0	mem
#define prefC(mem) prefetcht0	mem
/*
 *BYTE:                     4            8           12                16
 *void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha,
 *BYTE:                      20            24             28             32
 *                const TYPE *A, const int lda, const TYPE *B, const int ldb,
 *BYTE:                       36        40             44
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
        movl    36(%eax), %ebp
        movl    %ebp, (%esp)
        movl    %ebp, 4(%esp)
        movl    %ebp, 8(%esp)
        movl    %ebp, 12(%esp)
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
	movl	20(%eax), pA
	movl	28(%eax), pB
	movl	40(%eax), pC
/*
 *      stM = pA + NBNB-6*NB;  pfA = pA+NBNB;  stN = pB + NBNB;
 */
	movl	$NBNBso-NB6so, stM
	addl	pA, stM
        xor     pfA, pfA
	movl	$NBNBso, stN
	addl	pB, stN
/*
 *      Set incCn = (ldc - NB)*sizeof
 */
	movl	44(%eax), incCn
	subl	$MB-6, incCn
   #ifdef SCPLX
	shl	$3, incCn
   #else
	shl	$2, incCn
   #endif
NLOOP:
MLOOP:
#ifdef BETA0
	xorps	rC0, rC0
	xorps	rC1, rC1
	xorps	rC2, rC2
	xorps	rC3, rC3
	xorps	rC4, rC4
	xorps	rC5, rC5
#else
   #ifdef SCPLX
	movss	(pC), rC0
	movss	8(pC), rC1
	movss	16(pC), rC2
	movss	24(pC), rC3
	movss	32(pC), rC4
	movss	40(pC), rC5
   #else
	movss	(pC), rC0
	movss	4(pC), rC1
	movss	8(pC), rC2
	movss	12(pC), rC3
	movss	16(pC), rC4
	movss	20(pC), rC5
   #endif
   #ifdef BETAX
	movss	(%esp), rA0
	mulss	rA0, rC0
	mulss	rA0, rC1
	mulss	rA0, rC2
	mulss	rA0, rC3
	mulss	rA0, rC4
	mulss	rA0, rC5
   #endif
#endif
	ALIGN16
	movaps	0(pB), rB0
	movaps	0(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	0+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	0+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	0+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	0+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	0+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	16(pB), rB0
	movaps	16(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	16+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	16+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	16+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	16+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	16+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	32(pB), rB0
	movaps	32(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	32+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	32+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	32+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	32+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	32+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	48(pB), rB0
	movaps	48(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	48+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	48+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	48+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	48+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	48+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	64(pB), rB0
	movaps	64(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	64+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	64+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	64+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	64+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	64+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	80(pB), rB0
	movaps	80(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	80+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	80+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	80+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	80+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	80+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	96(pB), rB0
	movaps	96(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	96+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	96+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	96+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	96+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	96+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	112(pB), rB0
	movaps	112(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	112+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	112+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	112+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	112+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	112+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	128(pB), rB0
	movaps	128(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	128+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	128+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	128+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	128+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	128+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	144(pB), rB0
	movaps	144(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	144+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	144+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	144+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	144+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	144+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	160(pB), rB0
	movaps	160(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	160+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	160+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	160+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	160+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	160+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	176(pB), rB0
	movaps	176(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	176+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	176+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	176+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	176+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	176+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	192(pB), rB0
	movaps	192(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	192+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	192+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	192+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	192+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	192+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	208(pB), rB0
	movaps	208(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	208+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	208+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	208+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	208+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	208+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	224(pB), rB0
	movaps	224(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	224+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	224+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	224+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	224+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	224+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

/*
 *      Get these bastard things summed up correctly
 */
#ifdef ATL_SSE3
        haddps  rC1, rC0        /* rC0  = c0ab c0cd c1ab c1cd */
        haddps  rC3, rC2        /* rC2  = c2ab c2cd c3ab c3cd */
        haddps  rC5, rC4        /* rC4  = c4ab c4cd c5ab c5cd */
        haddps  rC2, rC0        /* rC0  = c0abcd c1abcd c2abcd c3abcd */
        haddps  rC4, rC4        /* rC4  = c4abcd c5abcd c4abcd c5abcd */
				pref2(NB6so(stM,pfA))
				addl	$27, pfA
   #ifdef SREAL
	movups		rC0, (pC)
        movlps          rC4, 16(pC)
   #else
                                        /* rC4 = c5 c4 c5 c4 */
                                        /* rC0 = c3 c2 c1 c0 */
        movshdup        rC0, rC1        /* rC1 = c3 c3 c1 c1 */
        movss           rC0, (pC)
        movhlps         rC0, rC2
        movss           rC1, 8(pC)
        movhlps         rC1, rC3
        movss           rC2, 16(pC)
        movshdup        rC4, rC5        /* rc5 = c5 c5 c5 c5 */
        movss           rC3, 24(pC)
        movss           rC4, 32(pC)
        movss           rC5, 40(pC)
   #endif
#else
	movaps		rC2, rB0
	movaps		rC0, rA0
	unpckhps	rC3, rB0
	unpckhps	rC1, rA0
	unpcklps	rC3, rC2
	movlhps		rB0, rC3
	unpcklps	rC1, rC0
	movhlps		rA0, rC3
	movlhps		rC2, rA0
	movhlps		rC0, rB0
	addps		rA0, rC3
	movlhps		rC0, rC1
	movhlps		rC1, rC2
        movaps          rC4, rA0    /* xmm6 = c4a  c4b  c4c  c4d */
	addps		rB0, rC2
        unpcklps        rC5, rA0    /* xmm6 = c4a  c5a  c4b  c5b */
        unpckhps        rC5, rC4    /* xmm4 = c4c  c5c  c4d  c5d */
				pref2(NB6so(stM,pfA))
        addps           rA0, rC4    /* xmm4 = c4ac c5ac c4bd c5bd */
	addps		rC2, rC3
        movhlps         rC4, rC5    /* xmm5 = c4bd    c5bd    X X */
				addl	$27, pfA
        addps           rC5, rC4    /* xmm4 = c4abcd  c5abcd  X X */
/*
 *      Write results back to C
 */

   #ifdef SREAL
	movups		rC3, (pC)
        movlps          rC4, 16(pC)
   #else
	movss		rC3, (pC)
        movhlps         rC3, rC0
	movss		rC0, 16(pC)
        shufps          $0x55, rC3, rC3
	movss		rC3, 8(pC)
        shufps          $0x55, rC0, rC0
	movss		rC0, 24(pC)
	movss           rC4, 32(pC)
	shufps          $0x55, rC4, rC4
	movss           rC4, 40(pC)
   #endif
#endif
/*
 *      pC += 6;   pA += 6*NB
 */
   #ifdef SCPLX
	addl	$48, pC
   #else
	addl	$24, pC
   #endif
	addl	$NB6so, pA
/*
 *      while (pA != stM);
 */
	cmp	pA, stM
	jne	MLOOP
/*
 *      Unroll last iteration of MLOOP in order to prefetch next col of B
 */
				prefB(NBso(pB))
				prefB(32+NBso(pB))
				prefB(64+NBso(pB))
				prefB(96+NBso(pB))
#ifdef BETA0
	xorps	rC0, rC0
	xorps	rC1, rC1
	xorps	rC2, rC2
	xorps	rC3, rC3
	xorps	rC4, rC4
	xorps	rC5, rC5
#else
   #ifdef SCPLX
	movss	(pC), rC0
	movss	8(pC), rC1
	movss	16(pC), rC2
	movss	24(pC), rC3
	movss	32(pC), rC4
	movss	40(pC), rC5
   #else
	movss	(pC), rC0
	movss	4(pC), rC1
	movss	8(pC), rC2
	movss	12(pC), rC3
	movss	16(pC), rC4
	movss	20(pC), rC5
   #endif
   #ifdef BETAX
	movss	(%esp), rA0
	mulss	rA0, rC0
	mulss	rA0, rC1
	mulss	rA0, rC2
	mulss	rA0, rC3
	mulss	rA0, rC4
	mulss	rA0, rC5
   #endif
#endif
	ALIGN16
	movaps	0(pB), rB0
	movaps	0(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	0+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	0+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	0+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	0+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	0+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	16(pB), rB0
	movaps	16(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	16+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	16+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	16+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	16+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	16+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	32(pB), rB0
	movaps	32(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	32+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	32+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	32+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	32+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	32+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	48(pB), rB0
	movaps	48(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	48+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	48+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	48+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	48+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	48+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	64(pB), rB0
	movaps	64(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	64+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	64+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	64+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	64+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	64+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	80(pB), rB0
	movaps	80(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	80+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	80+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	80+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	80+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	80+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	96(pB), rB0
	movaps	96(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	96+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	96+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	96+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	96+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	96+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	112(pB), rB0
	movaps	112(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	112+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	112+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	112+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	112+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	112+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	128(pB), rB0
	movaps	128(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	128+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	128+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	128+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	128+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	128+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	144(pB), rB0
	movaps	144(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	144+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	144+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	144+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	144+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	144+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	160(pB), rB0
	movaps	160(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	160+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	160+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	160+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	160+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	160+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	176(pB), rB0
	movaps	176(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	176+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	176+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	176+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	176+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	176+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	192(pB), rB0
	movaps	192(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	192+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	192+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	192+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	192+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	192+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	208(pB), rB0
	movaps	208(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	208+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	208+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	208+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	208+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	208+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

	movaps	224(pB), rB0
	movaps	224(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	224+NBso(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	224+NB2so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	224+NB3so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	224+NB4so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	movaps	224+NB5so(pA), rA0
	mulps	rB0, rA0
	addps	rA0, rC5

/*
 *      Get these bastard things summed up correctly
 */
#ifdef ATL_SSE3
        haddps  rC1, rC0        /* rC0  = c0ab c0cd c1ab c1cd */
        haddps  rC3, rC2        /* rC2  = c2ab c2cd c3ab c3cd */
        haddps  rC5, rC4        /* rC4  = c4ab c4cd c5ab c5cd */
        haddps  rC2, rC0        /* rC0  = c0abcd c1abcd c2abcd c3abcd */
        haddps  rC4, rC4        /* rC4  = c4abcd c5abcd c4abcd c5abcd */
				prefB(128+NBso(pB))
				prefB(160+NBso(pB))
				prefB(196+NBso(pB))
				prefB(224+NBso(pB))
   #ifdef SREAL
	movups		rC0, (pC)
        movlps          rC4, 16(pC)
   #else
                                        /* rC4 = c5 c4 c5 c4 */
                                        /* rC0 = c3 c2 c1 c0 */
        movshdup        rC0, rC1        /* rC1 = c3 c3 c1 c1 */
        movss           rC0, (pC)
        movhlps         rC0, rC2
        movss           rC1, 8(pC)
        movhlps         rC1, rC3
        movss           rC2, 16(pC)
        movshdup        rC4, rC5        /* rc5 = c5 c5 c5 c5 */
        movss           rC3, 24(pC)
        movss           rC4, 32(pC)
        movss           rC5, 40(pC)
   #endif
#else
	movaps		rC2, rB0
	movaps		rC2, rB0
	movaps		rC0, rA0
	unpckhps	rC3, rB0
	unpckhps	rC1, rA0
	unpcklps	rC3, rC2
	movlhps		rB0, rC3
	unpcklps	rC1, rC0
	movhlps		rA0, rC3
	movlhps		rC2, rA0
	movhlps		rC0, rB0
	addps		rA0, rC3
	movlhps		rC0, rC1
	movhlps		rC1, rC2
        movaps          rC4, rA0    /* xmm6 = c4a  c4b  c4c  c4d */
	addps		rB0, rC2
        unpcklps        rC5, rA0    /* xmm6 = c4a  c5a  c4b  c5b */
        unpckhps        rC5, rC4    /* xmm4 = c4c  c5c  c4d  c5d */
        addps           rA0, rC4    /* xmm4 = c4ac c5ac c4bd c5bd */
				prefB(128+NBso(pB))
				prefB(160+NBso(pB))
				prefB(196+NBso(pB))
				prefB(224+NBso(pB))
	addps		rC2, rC3
        movhlps         rC4, rC5    /* xmm5 = c4bd    c5bd    X X */
        addps           rC5, rC4    /* xmm4 = c4abcd  c5abcd  X X */
/*
 *      Write results back to C
 */

   #ifdef SREAL
	movups		rC3, (pC)
        movlps          rC4, 16(pC)
   #else
	movss		rC3, (pC)
        movhlps         rC3, rC0
	movss		rC0, 16(pC)
        shufps          $0x55, rC3, rC3
	movss		rC3, 8(pC)
        shufps          $0x55, rC0, rC0
	movss		rC0, 24(pC)
	movss           rC4, 32(pC)
	shufps          $0x55, rC4, rC4
	movss           rC4, 40(pC)
   #endif
#endif
/*
 *      End of unrolled final iteration of MLOOP
 */

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
