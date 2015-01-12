/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2001 R. Clint Whaley
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
   #error "This kernel requires a gas x86-32 assembler!"
#endif
#if !defined(NB) || (NB == 0)
   #error "NB must be a compile-time constant!"
#endif
#if (NB != 24)
   #error "NB must be 24!"
#endif
#if (NB/2)*2 != NB
   #error "NB must be multiple of 2!"
#endif
#ifdef DCPLX
   #define incCm 32
   #define OFF   16
#else
   #define incCm 16
   #define OFF    8
#endif
/*
 * Integer register usage shown be these defines
 */
#define pC      %esi
#define pA0     %ecx
#define pB0     %edi
#define incCn   %eax
#define pA1     %edx
#define stN     %ebx
#define pfA     %ebp

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
#if 1
#define pref2(mem) prefetcht1	mem
#define prefB(mem) prefetchnta  mem
#define prefA(mem) prefetcht0	mem
#define prefC(mem) prefetcht0	mem
#else
#define pref2(mem)
#define prefB(mem)
#define prefA(mem)
#define prefC(mem)
#endif
/*
 *void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha,
 *                const TYPE *A, const int lda, const TYPE *B, const int ldb,
 *                const TYPE beta, TYPE *C, const int ldc)
 */
	.text
.global ATL_asmdecor(ATL_USERMM)
ATL_asmdecor(ATL_USERMM):
/*
 *      Save callee-saved iregs
 */
	subl	$24, %esp
	movl	%ebp, 20(%esp)
	movl	%ebx, 16(%esp)
	movl	%esi, 12(%esp)
	movl	%edi,  8(%esp)
   #ifdef BETAX
   	fldl	64(%esp)
	fstpl	(%esp)
      #define BETAOFF 0
   #endif
/*
 *      Initialize pA = A;  pB = B; pC = C;
 */
	movl	56(%esp), pB0
                                prefB(NBso(pB0))
                                prefB(32+NBso(pB0))
	movl	48(%esp), pA0
	movl	72(%esp), pC
/*
 *      stM = pA + NBNB-6*NB;  pfA = pA+NBNB;  stN = pB0 + NBNB;
 */
	movl	$NBNBso, pfA
	addl	pA0, pfA
                                prefB(64+NBso(pB0))
                                prefB(96+NBso(pB0))
/*	movl	pfA, stM */
	movl	$NBNBso, stN
	addl	pB0, stN
/*
 *      Set incCn = (ldc - NB)*sizeof
 */
	movl	76(%esp), incCn
	subl	$MB-2, incCn
   #ifdef DCPLX
	shl	$4, incCn
   #else
	shl	$3, incCn
   #endif
                                prefB(128+NBso(pB0))
                                prefB(160+NBso(pB0))
/*
 *      Unroll the first iteration of N-loop in order to prefetch A
 */
        addl    $96, pB0
        addl    $96, stN
        addl    $96, pA0
        movl    pA0, pA1
        addl    $NBso, pA1
        addl    $96, pfA
MLOOP:
	fldl	-96(pB0)
	fldl	-96(pA0)
	fmul %st(1),%st
	fldl	-96(pA1)
	fmulp %st,%st(2)
	fldl	-88(pB0)
	fldl	-88(pA0)
	fmul %st(1),%st
	fldl	-88(pA1)
	fmulp %st,%st(2)
	fldl	-80(pB0)
	fldl	-80(pA0)
	fmul %st(1),%st
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl (pC)
   #endif
	faddp %st,%st(5)
	fldl	-80(pA1)
	fmulp %st,%st(2)
	fldl	-72(pB0)
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
      #ifdef DCPLX
	fldl 16(pC)
      #else
	fldl OFF(pC)
      #endif
   #endif
	faddp %st,%st(7)
	fldl	-72(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-72(pA1)
	fmulp %st,%st(1)
	fldl	-64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-64(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-64(pA1)
	fmulp %st,%st(7)
	fldl	-56(pB0)
	fxch %st(5)
                                prefA(NB2so-96(pA0))
                                prefA(NB3so-96(pA0))
	faddp %st,%st(3)
	fldl	-56(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-56(pA1)
	fmulp %st,%st(5)
	fldl	-48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-48(pA1)
	fmulp %st,%st(3)
	fldl	-40(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-40(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-40(pA1)
	fmulp %st,%st(1)
	fldl	-32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-32(pA0)
	fmul %st(7),%st
	fxch %st(2)
                                prefA(32+NB2so-96(pA0))
                                prefA(32+NB3so-96(pA0))
	faddp %st,%st(6)
	fldl	-32(pA1)
	fmulp %st,%st(7)
	fldl	-24(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	-24(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-24(pA1)
	fmulp %st,%st(5)
	fldl	-16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-16(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-16(pA1)
	fmulp %st,%st(3)
	fldl	-8(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-8(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-8(pA1)
	fmulp %st,%st(1)
	fldl	(pB0)
	fxch %st(7)
                                prefA(64+NB2so-96(pA0))
                                prefA(64+NB3so-96(pA0))
	faddp %st,%st(5)
	fldl	(pA0)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	(pA1)
	fmulp %st,%st(7)
	fldl	8(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	8(pA0)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	8(pA1)
	fmulp %st,%st(5)
	fldl	16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	16(pA0)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	16(pA1)
	fmulp %st,%st(3)
	fldl	24(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	24(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	24(pA1)
	fmulp %st,%st(1)
	fldl	32(pB0)
	fxch %st(7)
                                prefA(96+NB2so-96(pA0))
                                prefA(96+NB3so-96(pA0))
	faddp %st,%st(5)
	fldl	32(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	32(pA1)
	fmulp %st,%st(7)
	fldl	40(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	40(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	40(pA1)
	fmulp %st,%st(5)
	fldl	48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	48(pA1)
	fmulp %st,%st(3)
	fldl	56(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	56(pA0)
	fmul %st(1),%st
	fxch %st(4)
                                prefA(128+NB2so-96(pA0))
                                prefA(128+NB3so-96(pA0))
	faddp %st,%st(2)
	fldl	56(pA1)
	fmulp %st,%st(1)
	fldl	64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	64(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	64(pA1)
	fmulp %st,%st(7)
	fldl	72(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	72(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	72(pA1)
	fmulp %st,%st(5)
	fldl	80(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	80(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	80(pA1)
	fmulp %st,%st(3)
	fldl	88(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	88(pA0)
	fmul %st(1),%st
	fxch %st(2)
                                prefA(160+NB2so-96(pA0))
                                prefA(160+NB3so-96(pA0))
	faddp %st,%st(6)
	fldl	88(pA1)
	fmulp %st,%st(1)
	fxch %st(6)
	        addl	$NB2so, pA0
	faddp %st,%st(4)
	fxch %st(4)
	        addl	$NB2so, pA1
                                prefC(32(pC))
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
/*
 *      Write results back to C
 */
   #ifdef BETAX
        fldl    (pC)
      #ifdef DCPLX
        fldl    16(pC)
      #else
        fldl    OFF(pC)
      #endif
        fldl    BETAOFF(%esp)
        fmul    %st, %st(1)
        fmulp   %st, %st(2)
        faddp   %st, %st(3)
        faddp   %st, %st(1)
   #endif
	fstpl (pC)
	fstpl OFF(pC)
	addl	$incCm, pC
/*
 *      while (pA != stM);
 */
	cmp	pA0, pfA
	jne	MLOOP
        subl    $96, pfA
/*
 *      pC += incCn;  pA -= NBNB;  pB += NB;
 */
	addl	incCn, pC
        subl    $incCm, pC
	subl	$NBNBso, pA0
	subl	$NBNBso, pA1
	addl	$NBso, pB0
NLOOP:
        ALIGN8
/*MLOOP: */
	fldl	-96(pB0)
	fldl	-96(pA0)
	fmul %st(1),%st
	fldl	-96(pA1)
	fmulp %st,%st(2)
	fldl	-88(pB0)
	fldl	-88(pA0)
	fmul %st(1),%st
	fldl	-88(pA1)
	fmulp %st,%st(2)
	fldl	-80(pB0)
	fldl	-80(pA0)
	fmul %st(1),%st
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl (pC)
   #endif
	faddp %st,%st(5)
	fldl	-80(pA1)
	fmulp %st,%st(2)
	fldl	-72(pB0)
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl OFF(pC)
   #endif
	faddp %st,%st(7)
	fldl	-72(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-72(pA1)
	fmulp %st,%st(1)
	fldl	-64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-64(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-64(pA1)
	fmulp %st,%st(7)
	fldl	-56(pB0)
	fxch %st(5)
                                pref2((pfA))
                                pref2(32(pfA))
	faddp %st,%st(3)
	fldl	-56(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-56(pA1)
	fmulp %st,%st(5)
	fldl	-48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-48(pA1)
	fmulp %st,%st(3)
	fldl	-40(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-40(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-40(pA1)
	fmulp %st,%st(1)
	fldl	-32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-32(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-32(pA1)
	fmulp %st,%st(7)
	fldl	-24(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	-24(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-24(pA1)
	fmulp %st,%st(5)
	fldl	-16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-16(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-16(pA1)
	fmulp %st,%st(3)
	fldl	-8(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-8(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-8(pA1)
	fmulp %st,%st(1)
	fldl	(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	(pA0)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	(pA1)
	fmulp %st,%st(7)
	fldl	8(pB0)
	fxch %st(5)
                                pref2(64(pfA))
                                pref2(96(pfA))
	faddp %st,%st(3)
	fldl	8(pA0)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	8(pA1)
	fmulp %st,%st(5)
	fldl	16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	16(pA0)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	16(pA1)
	fmulp %st,%st(3)
	fldl	24(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	24(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	24(pA1)
	fmulp %st,%st(1)
	fldl	32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	32(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	32(pA1)
	fmulp %st,%st(7)
	fldl	40(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	40(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	40(pA1)
	fmulp %st,%st(5)
	fldl	48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	48(pA1)
	fmulp %st,%st(3)
	fldl	56(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	56(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	56(pA1)
	fmulp %st,%st(1)
	fldl	64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	64(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	64(pA1)
	fmulp %st,%st(7)
	fldl	72(pB0)
	fxch %st(5)
                                pref2(128(pfA))
                                pref2(160(pfA))
	faddp %st,%st(3)
	fldl	72(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	72(pA1)
	fmulp %st,%st(5)
	fldl	80(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	80(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	80(pA1)
	fmulp %st,%st(3)
	fldl	88(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	88(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	88(pA1)
	fmulp %st,%st(1)
	fxch %st(6)
	addl	$NB2so, pA0
	faddp %st,%st(4)
	fxch %st(4)
	addl	$NB2so, pA1
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
/*
 *      Write results back to C
 */
   #ifdef BETAX
        fldl    (pC)
        fldl    OFF(pC)
        fldl    BETAOFF(%esp)
        fmul    %st, %st(1)
        fmulp   %st, %st(2)
        faddp   %st, %st(3)
        faddp   %st, %st(1)
   #endif
	fstpl (pC)
	fstpl OFF(pC)
/*
 *      pC += 2;  pA += 2*NB; pB -= NB;
 */
	addl	$incCm, pC
	fldl	-96(pB0)
	fldl	-96(pA0)
	fmul %st(1),%st
	fldl	-96(pA1)
	fmulp %st,%st(2)
	fldl	-88(pB0)
	fldl	-88(pA0)
	fmul %st(1),%st
	fldl	-88(pA1)
	fmulp %st,%st(2)
	fldl	-80(pB0)
	fldl	-80(pA0)
	fmul %st(1),%st
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl (pC)
   #endif
	faddp %st,%st(5)
	fldl	-80(pA1)
	fmulp %st,%st(2)
	fldl	-72(pB0)
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl OFF(pC)
   #endif
	faddp %st,%st(7)
	fldl	-72(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-72(pA1)
	fmulp %st,%st(1)
	fldl	-64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-64(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-64(pA1)
	fmulp %st,%st(7)
	fldl	-56(pB0)
	fxch %st(5)
                                addl    $192, pfA
	faddp %st,%st(3)
	fldl	-56(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-56(pA1)
	fmulp %st,%st(5)
	fldl	-48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-48(pA1)
	fmulp %st,%st(3)
	fldl	-40(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-40(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-40(pA1)
	fmulp %st,%st(1)
	fldl	-32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-32(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-32(pA1)
	fmulp %st,%st(7)
	fldl	-24(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	-24(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-24(pA1)
	fmulp %st,%st(5)
	fldl	-16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-16(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-16(pA1)
	fmulp %st,%st(3)
	fldl	-8(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-8(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-8(pA1)
	fmulp %st,%st(1)
	fldl	(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	(pA0)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	(pA1)
	fmulp %st,%st(7)
	fldl	8(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	8(pA0)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	8(pA1)
	fmulp %st,%st(5)
	fldl	16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	16(pA0)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	16(pA1)
	fmulp %st,%st(3)
	fldl	24(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	24(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	24(pA1)
	fmulp %st,%st(1)
	fldl	32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	32(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	32(pA1)
	fmulp %st,%st(7)
	fldl	40(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	40(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	40(pA1)
	fmulp %st,%st(5)
	fldl	48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	48(pA1)
	fmulp %st,%st(3)
	fldl	56(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	56(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	56(pA1)
	fmulp %st,%st(1)
	fldl	64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	64(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	64(pA1)
	fmulp %st,%st(7)
	fldl	72(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	72(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	72(pA1)
	fmulp %st,%st(5)
	fldl	80(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	80(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	80(pA1)
	fmulp %st,%st(3)
	fldl	88(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	88(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	88(pA1)
	fmulp %st,%st(1)
	fxch %st(6)
	addl	$NB2so, pA0
	faddp %st,%st(4)
	fxch %st(4)
	addl	$NB2so, pA1
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
/*
 *      Write results back to C
 */
   #ifdef BETAX
        fldl    (pC)
        fldl    OFF(pC)
        fldl    BETAOFF(%esp)
        fmul    %st, %st(1)
        fmulp   %st, %st(2)
        faddp   %st, %st(3)
        faddp   %st, %st(1)
   #endif
	fstpl (pC)
	fstpl OFF(pC)
/*
 *      pC += 2;  pA += 2*NB; pB -= NB;
 */
	addl	$incCm, pC
	fldl	-96(pB0)
	fldl	-96(pA0)
	fmul %st(1),%st
	fldl	-96(pA1)
	fmulp %st,%st(2)
	fldl	-88(pB0)
	fldl	-88(pA0)
	fmul %st(1),%st
	fldl	-88(pA1)
	fmulp %st,%st(2)
	fldl	-80(pB0)
	fldl	-80(pA0)
	fmul %st(1),%st
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl (pC)
   #endif
	faddp %st,%st(5)
	fldl	-80(pA1)
	fmulp %st,%st(2)
	fldl	-72(pB0)
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl OFF(pC)
   #endif
	faddp %st,%st(7)
	fldl	-72(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-72(pA1)
	fmulp %st,%st(1)
	fldl	-64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-64(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-64(pA1)
	fmulp %st,%st(7)
	fldl	-56(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	-56(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-56(pA1)
	fmulp %st,%st(5)
	fldl	-48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-48(pA1)
	fmulp %st,%st(3)
	fldl	-40(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-40(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-40(pA1)
	fmulp %st,%st(1)
	fldl	-32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-32(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-32(pA1)
	fmulp %st,%st(7)
	fldl	-24(pB0)
	fxch %st(5)
                                prefB(NBso-96(pB0))
                                prefB(32+NBso-96(pB0))
	faddp %st,%st(3)
	fldl	-24(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-24(pA1)
	fmulp %st,%st(5)
	fldl	-16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-16(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-16(pA1)
	fmulp %st,%st(3)
	fldl	-8(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-8(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-8(pA1)
	fmulp %st,%st(1)
	fldl	(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	(pA0)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	(pA1)
	fmulp %st,%st(7)
	fldl	8(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	8(pA0)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	8(pA1)
	fmulp %st,%st(5)
	fldl	16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	16(pA0)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	16(pA1)
	fmulp %st,%st(3)
	fldl	24(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	24(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	24(pA1)
	fmulp %st,%st(1)
	fldl	32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	32(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	32(pA1)
	fmulp %st,%st(7)
	fldl	40(pB0)
	fxch %st(5)
                                prefB(64+NBso-96(pB0))
                                prefB(96+NBso-96(pB0))
	faddp %st,%st(3)
	fldl	40(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	40(pA1)
	fmulp %st,%st(5)
	fldl	48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	48(pA1)
	fmulp %st,%st(3)
	fldl	56(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	56(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	56(pA1)
	fmulp %st,%st(1)
	fldl	64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	64(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	64(pA1)
	fmulp %st,%st(7)
	fldl	72(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	72(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	72(pA1)
	fmulp %st,%st(5)
	fldl	80(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	80(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	80(pA1)
	fmulp %st,%st(3)
	fldl	88(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	88(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	88(pA1)
	fmulp %st,%st(1)
	fxch %st(6)
	addl	$NB2so, pA0
	faddp %st,%st(4)
	fxch %st(4)
	addl	$NB2so, pA1
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
/*
 *      Write results back to C
 */
   #ifdef BETAX
        fldl    (pC)
        fldl    OFF(pC)
        fldl    BETAOFF(%esp)
        fmul    %st, %st(1)
        fmulp   %st, %st(2)
        faddp   %st, %st(3)
        faddp   %st, %st(1)
   #endif
	fstpl (pC)
	fstpl OFF(pC)
/*
 *      pC += 2;  pA += 2*NB; pB -= NB;
 */
	addl	$incCm, pC
	fldl	-96(pB0)
	fldl	-96(pA0)
	fmul %st(1),%st
	fldl	-96(pA1)
	fmulp %st,%st(2)
	fldl	-88(pB0)
	fldl	-88(pA0)
	fmul %st(1),%st
	fldl	-88(pA1)
	fmulp %st,%st(2)
	fldl	-80(pB0)
	fldl	-80(pA0)
	fmul %st(1),%st
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl (pC)
   #endif
	faddp %st,%st(5)
	fldl	-80(pA1)
	fmulp %st,%st(2)
	fldl	-72(pB0)
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl OFF(pC)
   #endif
	faddp %st,%st(7)
	fldl	-72(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-72(pA1)
	fmulp %st,%st(1)
	fldl	-64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-64(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-64(pA1)
	fmulp %st,%st(7)
	fldl	-56(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	-56(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-56(pA1)
	fmulp %st,%st(5)
	fldl	-48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-48(pA1)
	fmulp %st,%st(3)
	fldl	-40(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-40(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-40(pA1)
	fmulp %st,%st(1)
	fldl	-32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-32(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-32(pA1)
	fmulp %st,%st(7)
	fldl	-24(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	-24(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-24(pA1)
	fmulp %st,%st(5)
	fldl	-16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-16(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-16(pA1)
	fmulp %st,%st(3)
	fldl	-8(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-8(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-8(pA1)
	fmulp %st,%st(1)
	fldl	(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	(pA0)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	(pA1)
	fmulp %st,%st(7)
	fldl	8(pB0)
	fxch %st(5)
                                prefB(128+NBso-96(pB0))
                                prefB(160+NBso-96(pB0))
	faddp %st,%st(3)
	fldl	8(pA0)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	8(pA1)
	fmulp %st,%st(5)
	fldl	16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	16(pA0)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	16(pA1)
	fmulp %st,%st(3)
	fldl	24(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	24(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	24(pA1)
	fmulp %st,%st(1)
	fldl	32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	32(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	32(pA1)
	fmulp %st,%st(7)
	fldl	40(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	40(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	40(pA1)
	fmulp %st,%st(5)
	fldl	48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	48(pA1)
	fmulp %st,%st(3)
	fldl	56(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	56(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	56(pA1)
	fmulp %st,%st(1)
	fldl	64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	64(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	64(pA1)
	fmulp %st,%st(7)
	fldl	72(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	72(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	72(pA1)
	fmulp %st,%st(5)
	fldl	80(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	80(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	80(pA1)
	fmulp %st,%st(3)
	fldl	88(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	88(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	88(pA1)
	fmulp %st,%st(1)
	fxch %st(6)
	addl	$NB2so, pA0
	faddp %st,%st(4)
	fxch %st(4)
	addl	$NB2so, pA1
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
/*
 *      Write results back to C
 */
   #ifdef BETAX
        fldl    (pC)
        fldl    OFF(pC)
        fldl    BETAOFF(%esp)
        fmul    %st, %st(1)
        fmulp   %st, %st(2)
        faddp   %st, %st(3)
        faddp   %st, %st(1)
   #endif
	fstpl (pC)
	fstpl OFF(pC)
/*
 *      pC += 2;  pA += 2*NB; pB -= NB;
 */
	addl	$incCm, pC
	fldl	-96(pB0)
	fldl	-96(pA0)
	fmul %st(1),%st
	fldl	-96(pA1)
	fmulp %st,%st(2)
	fldl	-88(pB0)
	fldl	-88(pA0)
	fmul %st(1),%st
	fldl	-88(pA1)
	fmulp %st,%st(2)
	fldl	-80(pB0)
	fldl	-80(pA0)
	fmul %st(1),%st
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl (pC)
   #endif
	faddp %st,%st(5)
	fldl	-80(pA1)
	fmulp %st,%st(2)
	fldl	-72(pB0)
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl OFF(pC)
   #endif
	faddp %st,%st(7)
	fldl	-72(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-72(pA1)
	fmulp %st,%st(1)
	fldl	-64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-64(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-64(pA1)
	fmulp %st,%st(7)
	fldl	-56(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	-56(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-56(pA1)
	fmulp %st,%st(5)
	fldl	-48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-48(pA1)
	fmulp %st,%st(3)
	fldl	-40(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-40(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-40(pA1)
	fmulp %st,%st(1)
	fldl	-32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-32(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-32(pA1)
	fmulp %st,%st(7)
	fldl	-24(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	-24(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-24(pA1)
	fmulp %st,%st(5)
	fldl	-16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-16(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-16(pA1)
	fmulp %st,%st(3)
	fldl	-8(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-8(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-8(pA1)
	fmulp %st,%st(1)
	fldl	(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	(pA0)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	(pA1)
	fmulp %st,%st(7)
	fldl	8(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	8(pA0)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	8(pA1)
	fmulp %st,%st(5)
	fldl	16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	16(pA0)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	16(pA1)
	fmulp %st,%st(3)
	fldl	24(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	24(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	24(pA1)
	fmulp %st,%st(1)
	fldl	32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	32(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	32(pA1)
	fmulp %st,%st(7)
	fldl	40(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	40(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	40(pA1)
	fmulp %st,%st(5)
	fldl	48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	48(pA1)
	fmulp %st,%st(3)
	fldl	56(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	56(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	56(pA1)
	fmulp %st,%st(1)
	fldl	64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	64(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	64(pA1)
	fmulp %st,%st(7)
	fldl	72(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	72(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	72(pA1)
	fmulp %st,%st(5)
	fldl	80(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	80(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	80(pA1)
	fmulp %st,%st(3)
	fldl	88(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	88(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	88(pA1)
	fmulp %st,%st(1)
	fxch %st(6)
	addl	$NB2so, pA0
	faddp %st,%st(4)
	fxch %st(4)
	addl	$NB2so, pA1
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
/*
 *      Write results back to C
 */
   #ifdef BETAX
        fldl    (pC)
        fldl    OFF(pC)
        fldl    BETAOFF(%esp)
        fmul    %st, %st(1)
        fmulp   %st, %st(2)
        faddp   %st, %st(3)
        faddp   %st, %st(1)
   #endif
	fstpl (pC)
	fstpl OFF(pC)
/*
 *      pC += 2;  pA += 2*NB; pB -= NB;
 */
	addl	$incCm, pC
	fldl	-96(pB0)
	fldl	-96(pA0)
	fmul %st(1),%st
	fldl	-96(pA1)
	fmulp %st,%st(2)
	fldl	-88(pB0)
	fldl	-88(pA0)
	fmul %st(1),%st
	fldl	-88(pA1)
	fmulp %st,%st(2)
	fldl	-80(pB0)
	fldl	-80(pA0)
	fmul %st(1),%st
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl (pC)
   #endif
	faddp %st,%st(5)
	fldl	-80(pA1)
	fmulp %st,%st(2)
	fldl	-72(pB0)
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl OFF(pC)
   #endif
	faddp %st,%st(7)
	fldl	-72(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-72(pA1)
	fmulp %st,%st(1)
	fldl	-64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-64(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-64(pA1)
	fmulp %st,%st(7)
	fldl	-56(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	-56(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-56(pA1)
	fmulp %st,%st(5)
	fldl	-48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-48(pA1)
	fmulp %st,%st(3)
	fldl	-40(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-40(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-40(pA1)
	fmulp %st,%st(1)
	fldl	-32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-32(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-32(pA1)
	fmulp %st,%st(7)
	fldl	-24(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	-24(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-24(pA1)
	fmulp %st,%st(5)
	fldl	-16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-16(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-16(pA1)
	fmulp %st,%st(3)
	fldl	-8(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-8(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-8(pA1)
	fmulp %st,%st(1)
	fldl	(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	(pA0)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	(pA1)
	fmulp %st,%st(7)
	fldl	8(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	8(pA0)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	8(pA1)
	fmulp %st,%st(5)
	fldl	16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	16(pA0)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	16(pA1)
	fmulp %st,%st(3)
	fldl	24(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	24(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	24(pA1)
	fmulp %st,%st(1)
	fldl	32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	32(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	32(pA1)
	fmulp %st,%st(7)
	fldl	40(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	40(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	40(pA1)
	fmulp %st,%st(5)
	fldl	48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	48(pA1)
	fmulp %st,%st(3)
	fldl	56(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	56(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	56(pA1)
	fmulp %st,%st(1)
	fldl	64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	64(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	64(pA1)
	fmulp %st,%st(7)
	fldl	72(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	72(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	72(pA1)
	fmulp %st,%st(5)
	fldl	80(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	80(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	80(pA1)
	fmulp %st,%st(3)
	fldl	88(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	88(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	88(pA1)
	fmulp %st,%st(1)
	fxch %st(6)
	addl	$NB2so, pA0
	faddp %st,%st(4)
	fxch %st(4)
	addl	$NB2so, pA1
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
/*
 *      Write results back to C
 */
   #ifdef BETAX
        fldl    (pC)
        fldl    OFF(pC)
        fldl    BETAOFF(%esp)
        fmul    %st, %st(1)
        fmulp   %st, %st(2)
        faddp   %st, %st(3)
        faddp   %st, %st(1)
   #endif
	fstpl (pC)
	fstpl OFF(pC)
/*
 *      pC += 2;  pA += 2*NB; pB -= NB;
 */
	addl	$incCm, pC
	fldl	-96(pB0)
	fldl	-96(pA0)
	fmul %st(1),%st
	fldl	-96(pA1)
	fmulp %st,%st(2)
	fldl	-88(pB0)
	fldl	-88(pA0)
	fmul %st(1),%st
	fldl	-88(pA1)
	fmulp %st,%st(2)
	fldl	-80(pB0)
	fldl	-80(pA0)
	fmul %st(1),%st
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl (pC)
   #endif
	faddp %st,%st(5)
	fldl	-80(pA1)
	fmulp %st,%st(2)
	fldl	-72(pB0)
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl OFF(pC)
   #endif
	faddp %st,%st(7)
	fldl	-72(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-72(pA1)
	fmulp %st,%st(1)
	fldl	-64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-64(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-64(pA1)
	fmulp %st,%st(7)
	fldl	-56(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	-56(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-56(pA1)
	fmulp %st,%st(5)
	fldl	-48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-48(pA1)
	fmulp %st,%st(3)
	fldl	-40(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-40(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-40(pA1)
	fmulp %st,%st(1)
	fldl	-32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-32(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-32(pA1)
	fmulp %st,%st(7)
	fldl	-24(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	-24(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-24(pA1)
	fmulp %st,%st(5)
	fldl	-16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-16(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-16(pA1)
	fmulp %st,%st(3)
	fldl	-8(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-8(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-8(pA1)
	fmulp %st,%st(1)
	fldl	(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	(pA0)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	(pA1)
	fmulp %st,%st(7)
	fldl	8(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	8(pA0)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	8(pA1)
	fmulp %st,%st(5)
	fldl	16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	16(pA0)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	16(pA1)
	fmulp %st,%st(3)
	fldl	24(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	24(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	24(pA1)
	fmulp %st,%st(1)
	fldl	32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	32(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	32(pA1)
	fmulp %st,%st(7)
	fldl	40(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	40(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	40(pA1)
	fmulp %st,%st(5)
	fldl	48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	48(pA1)
	fmulp %st,%st(3)
	fldl	56(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	56(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	56(pA1)
	fmulp %st,%st(1)
	fldl	64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	64(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	64(pA1)
	fmulp %st,%st(7)
	fldl	72(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	72(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	72(pA1)
	fmulp %st,%st(5)
	fldl	80(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	80(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	80(pA1)
	fmulp %st,%st(3)
	fldl	88(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	88(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	88(pA1)
	fmulp %st,%st(1)
	fxch %st(6)
	addl	$NB2so, pA0
	faddp %st,%st(4)
	fxch %st(4)
	addl	$NB2so, pA1
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
/*
 *      Write results back to C
 */
   #ifdef BETAX
        fldl    (pC)
        fldl    OFF(pC)
        fldl    BETAOFF(%esp)
        fmul    %st, %st(1)
        fmulp   %st, %st(2)
        faddp   %st, %st(3)
        faddp   %st, %st(1)
   #endif
	fstpl (pC)
	fstpl OFF(pC)
/*
 *      pC += 2;  pA += 2*NB; pB -= NB;
 */
	addl	$incCm, pC
	fldl	-96(pB0)
	fldl	-96(pA0)
	fmul %st(1),%st
	fldl	-96(pA1)
	fmulp %st,%st(2)
	fldl	-88(pB0)
	fldl	-88(pA0)
	fmul %st(1),%st
	fldl	-88(pA1)
	fmulp %st,%st(2)
	fldl	-80(pB0)
	fldl	-80(pA0)
	fmul %st(1),%st
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl (pC)
   #endif
	faddp %st,%st(5)
	fldl	-80(pA1)
	fmulp %st,%st(2)
	fldl	-72(pB0)
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl OFF(pC)
   #endif
	faddp %st,%st(7)
	fldl	-72(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-72(pA1)
	fmulp %st,%st(1)
	fldl	-64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-64(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-64(pA1)
	fmulp %st,%st(7)
	fldl	-56(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	-56(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-56(pA1)
	fmulp %st,%st(5)
	fldl	-48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-48(pA1)
	fmulp %st,%st(3)
	fldl	-40(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-40(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-40(pA1)
	fmulp %st,%st(1)
	fldl	-32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-32(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-32(pA1)
	fmulp %st,%st(7)
	fldl	-24(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	-24(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-24(pA1)
	fmulp %st,%st(5)
	fldl	-16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-16(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-16(pA1)
	fmulp %st,%st(3)
	fldl	-8(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-8(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-8(pA1)
	fmulp %st,%st(1)
	fldl	(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	(pA0)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	(pA1)
	fmulp %st,%st(7)
	fldl	8(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	8(pA0)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	8(pA1)
	fmulp %st,%st(5)
	fldl	16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	16(pA0)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	16(pA1)
	fmulp %st,%st(3)
	fldl	24(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	24(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	24(pA1)
	fmulp %st,%st(1)
	fldl	32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	32(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	32(pA1)
	fmulp %st,%st(7)
	fldl	40(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	40(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	40(pA1)
	fmulp %st,%st(5)
	fldl	48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	48(pA1)
	fmulp %st,%st(3)
	fldl	56(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	56(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	56(pA1)
	fmulp %st,%st(1)
	fldl	64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	64(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	64(pA1)
	fmulp %st,%st(7)
	fldl	72(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	72(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	72(pA1)
	fmulp %st,%st(5)
	fldl	80(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	80(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	80(pA1)
	fmulp %st,%st(3)
	fldl	88(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	88(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	88(pA1)
	fmulp %st,%st(1)
	fxch %st(6)
	addl	$NB2so, pA0
	faddp %st,%st(4)
	fxch %st(4)
	addl	$NB2so, pA1
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
/*
 *      Write results back to C
 */
   #ifdef BETAX
        fldl    (pC)
        fldl    OFF(pC)
        fldl    BETAOFF(%esp)
        fmul    %st, %st(1)
        fmulp   %st, %st(2)
        faddp   %st, %st(3)
        faddp   %st, %st(1)
   #endif
	fstpl (pC)
	fstpl OFF(pC)
/*
 *      pC += 2;  pA += 2*NB; pB -= NB;
 */
	addl	$incCm, pC
	fldl	-96(pB0)
	fldl	-96(pA0)
	fmul %st(1),%st
	fldl	-96(pA1)
	fmulp %st,%st(2)
	fldl	-88(pB0)
	fldl	-88(pA0)
	fmul %st(1),%st
	fldl	-88(pA1)
	fmulp %st,%st(2)
	fldl	-80(pB0)
	fldl	-80(pA0)
	fmul %st(1),%st
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl (pC)
   #endif
	faddp %st,%st(5)
	fldl	-80(pA1)
	fmulp %st,%st(2)
	fldl	-72(pB0)
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl OFF(pC)
   #endif
	faddp %st,%st(7)
	fldl	-72(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-72(pA1)
	fmulp %st,%st(1)
	fldl	-64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-64(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-64(pA1)
	fmulp %st,%st(7)
	fldl	-56(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	-56(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-56(pA1)
	fmulp %st,%st(5)
	fldl	-48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-48(pA1)
	fmulp %st,%st(3)
	fldl	-40(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-40(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-40(pA1)
	fmulp %st,%st(1)
	fldl	-32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-32(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-32(pA1)
	fmulp %st,%st(7)
	fldl	-24(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	-24(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-24(pA1)
	fmulp %st,%st(5)
	fldl	-16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-16(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-16(pA1)
	fmulp %st,%st(3)
	fldl	-8(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-8(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-8(pA1)
	fmulp %st,%st(1)
	fldl	(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	(pA0)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	(pA1)
	fmulp %st,%st(7)
	fldl	8(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	8(pA0)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	8(pA1)
	fmulp %st,%st(5)
	fldl	16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	16(pA0)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	16(pA1)
	fmulp %st,%st(3)
	fldl	24(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	24(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	24(pA1)
	fmulp %st,%st(1)
	fldl	32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	32(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	32(pA1)
	fmulp %st,%st(7)
	fldl	40(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	40(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	40(pA1)
	fmulp %st,%st(5)
	fldl	48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	48(pA1)
	fmulp %st,%st(3)
	fldl	56(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	56(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	56(pA1)
	fmulp %st,%st(1)
	fldl	64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	64(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	64(pA1)
	fmulp %st,%st(7)
	fldl	72(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	72(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	72(pA1)
	fmulp %st,%st(5)
	fldl	80(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	80(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	80(pA1)
	fmulp %st,%st(3)
	fldl	88(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	88(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	88(pA1)
	fmulp %st,%st(1)
	fxch %st(6)
	addl	$NB2so, pA0
	faddp %st,%st(4)
	fxch %st(4)
	addl	$NB2so, pA1
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
/*
 *      Write results back to C
 */
   #ifdef BETAX
        fldl    (pC)
        fldl    OFF(pC)
        fldl    BETAOFF(%esp)
        fmul    %st, %st(1)
        fmulp   %st, %st(2)
        faddp   %st, %st(3)
        faddp   %st, %st(1)
   #endif
	fstpl (pC)
	fstpl OFF(pC)
/*
 *      pC += 2;  pA += 2*NB; pB -= NB;
 */
	addl	$incCm, pC
	fldl	-96(pB0)
	fldl	-96(pA0)
	fmul %st(1),%st
	fldl	-96(pA1)
	fmulp %st,%st(2)
	fldl	-88(pB0)
	fldl	-88(pA0)
	fmul %st(1),%st
	fldl	-88(pA1)
	fmulp %st,%st(2)
	fldl	-80(pB0)
	fldl	-80(pA0)
	fmul %st(1),%st
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl (pC)
   #endif
	faddp %st,%st(5)
	fldl	-80(pA1)
	fmulp %st,%st(2)
	fldl	-72(pB0)
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl OFF(pC)
   #endif
	faddp %st,%st(7)
	fldl	-72(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-72(pA1)
	fmulp %st,%st(1)
	fldl	-64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-64(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-64(pA1)
	fmulp %st,%st(7)
	fldl	-56(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	-56(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-56(pA1)
	fmulp %st,%st(5)
	fldl	-48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-48(pA1)
	fmulp %st,%st(3)
	fldl	-40(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-40(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-40(pA1)
	fmulp %st,%st(1)
	fldl	-32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-32(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-32(pA1)
	fmulp %st,%st(7)
	fldl	-24(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	-24(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-24(pA1)
	fmulp %st,%st(5)
	fldl	-16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-16(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-16(pA1)
	fmulp %st,%st(3)
	fldl	-8(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-8(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-8(pA1)
	fmulp %st,%st(1)
	fldl	(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	(pA0)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	(pA1)
	fmulp %st,%st(7)
	fldl	8(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	8(pA0)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	8(pA1)
	fmulp %st,%st(5)
	fldl	16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	16(pA0)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	16(pA1)
	fmulp %st,%st(3)
	fldl	24(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	24(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	24(pA1)
	fmulp %st,%st(1)
	fldl	32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	32(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	32(pA1)
	fmulp %st,%st(7)
	fldl	40(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	40(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	40(pA1)
	fmulp %st,%st(5)
	fldl	48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	48(pA1)
	fmulp %st,%st(3)
	fldl	56(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	56(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	56(pA1)
	fmulp %st,%st(1)
	fldl	64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	64(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	64(pA1)
	fmulp %st,%st(7)
	fldl	72(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	72(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	72(pA1)
	fmulp %st,%st(5)
	fldl	80(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	80(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	80(pA1)
	fmulp %st,%st(3)
	fldl	88(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	88(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	88(pA1)
	fmulp %st,%st(1)
	fxch %st(6)
	addl	$NB2so, pA0
	faddp %st,%st(4)
	fxch %st(4)
	addl	$NB2so, pA1
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
/*
 *      Write results back to C
 */
   #ifdef BETAX
        fldl    (pC)
        fldl    OFF(pC)
        fldl    BETAOFF(%esp)
        fmul    %st, %st(1)
        fmulp   %st, %st(2)
        faddp   %st, %st(3)
        faddp   %st, %st(1)
   #endif
	fstpl (pC)
	fstpl OFF(pC)
/*
 *      pC += 2;  pA += 2*NB; pB -= NB;
 */
	addl	$incCm, pC
	fldl	-96(pB0)
	fldl	-96(pA0)
	fmul %st(1),%st
	fldl	-96(pA1)
	fmulp %st,%st(2)
	fldl	-88(pB0)
	fldl	-88(pA0)
	fmul %st(1),%st
	fldl	-88(pA1)
	fmulp %st,%st(2)
	fldl	-80(pB0)
	fldl	-80(pA0)
	fmul %st(1),%st
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl (pC)
   #endif
	faddp %st,%st(5)
	fldl	-80(pA1)
	fmulp %st,%st(2)
	fldl	-72(pB0)
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl OFF(pC)
   #endif
	faddp %st,%st(7)
	fldl	-72(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-72(pA1)
	fmulp %st,%st(1)
	fldl	-64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-64(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-64(pA1)
	fmulp %st,%st(7)
	fldl	-56(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	-56(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-56(pA1)
	fmulp %st,%st(5)
	fldl	-48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-48(pA1)
	fmulp %st,%st(3)
	fldl	-40(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-40(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-40(pA1)
	fmulp %st,%st(1)
	fldl	-32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-32(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-32(pA1)
	fmulp %st,%st(7)
	fldl	-24(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	-24(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-24(pA1)
	fmulp %st,%st(5)
	fldl	-16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-16(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-16(pA1)
	fmulp %st,%st(3)
	fldl	-8(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-8(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-8(pA1)
	fmulp %st,%st(1)
	fldl	(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	(pA0)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	(pA1)
	fmulp %st,%st(7)
	fldl	8(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	8(pA0)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	8(pA1)
	fmulp %st,%st(5)
	fldl	16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	16(pA0)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	16(pA1)
	fmulp %st,%st(3)
	fldl	24(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	24(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	24(pA1)
	fmulp %st,%st(1)
	fldl	32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	32(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	32(pA1)
	fmulp %st,%st(7)
	fldl	40(pB0)
	fxch %st(5)
/*                                prefA(-NBNBso(stM)) */
/*                                prefA(NBso-NBNBso(stM)) */
	faddp %st,%st(3)
	fldl	40(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	40(pA1)
	fmulp %st,%st(5)
	fldl	48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	48(pA1)
	fmulp %st,%st(3)
	fldl	56(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	56(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	56(pA1)
	fmulp %st,%st(1)
	fldl	64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	64(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	64(pA1)
	fmulp %st,%st(7)
	fldl	72(pB0)
	fxch %st(5)
	faddp %st,%st(3)
	fldl	72(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	72(pA1)
	fmulp %st,%st(5)
	fldl	80(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	80(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	80(pA1)
	fmulp %st,%st(3)
	fldl	88(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	88(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	88(pA1)
	fmulp %st,%st(1)
	fxch %st(6)
	addl	$NB2so, pA0
	faddp %st,%st(4)
	fxch %st(4)
	addl	$NB2so, pA1
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
/*
 *      Write results back to C
 */
   #ifdef BETAX
        fldl    (pC)
        fldl    OFF(pC)
        fldl    BETAOFF(%esp)
        fmul    %st, %st(1)
        fmulp   %st, %st(2)
        faddp   %st, %st(3)
        faddp   %st, %st(1)
   #endif
	fstpl (pC)
	fstpl OFF(pC)
/*
 *      pC += 2;  pA += 2*NB; pB -= NB;
 */
	addl	$incCm, pC
	fldl	-96(pB0)
	fldl	-96(pA0)
	fmul %st(1),%st
	fldl	-96(pA1)
	fmulp %st,%st(2)
	fldl	-88(pB0)
	fldl	-88(pA0)
	fmul %st(1),%st
	fldl	-88(pA1)
	fmulp %st,%st(2)
	fldl	-80(pB0)
	fldl	-80(pA0)
	fmul %st(1),%st
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl (pC)
   #endif
	faddp %st,%st(5)
	fldl	-80(pA1)
	fmulp %st,%st(2)
	fldl	-72(pB0)
   #if defined(BETA0) || defined(BETAX)
        fldz
   #else
	fldl OFF(pC)
   #endif
	faddp %st,%st(7)
	fldl	-72(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-72(pA1)
	fmulp %st,%st(1)
	fldl	-64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-64(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-64(pA1)
	fmulp %st,%st(7)
	fldl	-56(pB0)
	fxch %st(5)
/*                                prefA(32-NBNBso(stM)) */
/*                                prefA(32+NBso-NBNBso(stM)) */
	faddp %st,%st(3)
	fldl	-56(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-56(pA1)
	fmulp %st,%st(5)
	fldl	-48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-48(pA1)
	fmulp %st,%st(3)
	fldl	-40(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-40(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-40(pA1)
	fmulp %st,%st(1)
	fldl	-32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	-32(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-32(pA1)
	fmulp %st,%st(7)
	fldl	-24(pB0)
	fxch %st(5)
/*                                prefA(64-NBNBso(stM)) */
/*                                prefA(64+NBso-NBNBso(stM)) */
	faddp %st,%st(3)
	fldl	-24(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	-24(pA1)
	fmulp %st,%st(5)
	fldl	-16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	-16(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	-16(pA1)
	fmulp %st,%st(3)
	fldl	-8(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	-8(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	-8(pA1)
	fmulp %st,%st(1)
	fldl	(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	(pA0)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	(pA1)
	fmulp %st,%st(7)
	fldl	8(pB0)
	fxch %st(5)
/*                                prefA(96-NBNBso(stM)) */
/*                                prefA(96+NBso-NBNBso(stM)) */
	faddp %st,%st(3)
	fldl	8(pA0)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	8(pA1)
	fmulp %st,%st(5)
	fldl	16(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	16(pA0)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	16(pA1)
	fmulp %st,%st(3)
	fldl	24(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	24(pA0)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	24(pA1)
	fmulp %st,%st(1)
	fldl	32(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	32(pA0)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	32(pA1)
	fmulp %st,%st(7)
	fldl	40(pB0)
	fxch %st(5)
/*                                prefA(128-NBNBso(stM)) */
/*                                prefA(128+NBso-NBNBso(stM)) */
	faddp %st,%st(3)
	fldl	40(pA0)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	40(pA1)
	fmulp %st,%st(5)
	fldl	48(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	48(pA0)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	48(pA1)
	fmulp %st,%st(3)
	fldl	56(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	56(pA0)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	56(pA1)
	fmulp %st,%st(1)
	fldl	64(pB0)
	fxch %st(7)
	faddp %st,%st(5)
	fldl	64(pA0)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	64(pA1)
	fmulp %st,%st(7)
	fldl	72(pB0)
	fxch %st(5)
/*                                prefA(160-NBNBso(stM)) */
/*                                prefA(160+NBso-NBNBso(stM)) */
	faddp %st,%st(3)
	fldl	72(pA0)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl	72(pA1)
	fmulp %st,%st(5)
	fldl	80(pB0)
	fxch %st(3)
	faddp %st,%st(1)
	fldl	80(pA0)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl	80(pA1)
	fmulp %st,%st(3)
	fldl	88(pB0)
	fxch %st(1)
	faddp %st,%st(7)
	fldl	88(pA0)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl	88(pA1)
	fmulp %st,%st(1)
	fxch %st(6)
	faddp %st,%st(4)
	fxch %st(4)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
/*
 *      Write results back to C
 */
   #ifdef BETAX
        fldl    (pC)
        fldl    OFF(pC)
        fldl    BETAOFF(%esp)
        fmul    %st, %st(1)
        fmulp   %st, %st(2)
        faddp   %st, %st(3)
        faddp   %st, %st(1)
   #endif
	fstpl (pC)
	fstpl OFF(pC)
/*
 *      pC += 2;  pA += 2*NB; pB -= NB;
 */
/*	addl	$incCm, pC */
/*
 *      while (pA != stM);
 */
/*	cmp	pA, stM */
/*	jne	MLOOP */

/*
 *      pC += incCn;  pA -= NBNB;  pB += NB;
 */
	addl	incCn, pC
	subl	$NBNBso-NB2so, pA0
	subl	$NBNBso-NB2so, pA1
	addl	$NBso, pB0
/*
 *      while (pB != stN);
 */
	cmp	pB0, stN
	jne	NLOOP

/*
 *      Restore callee-saved iregs
 */
	movl	20(%esp), %ebp
	movl	16(%esp), %ebx
	movl	12(%esp), %esi
	movl	8(%esp), %edi
   	addl	$24,%esp
	ret
