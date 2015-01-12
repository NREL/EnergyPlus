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


#ifdef ATL_GAS_x8632
   #define movq movl
   #define addq addl
   #define subq subl
   #define rsp  esp
#elif !defined(ATL_GAS_x8664)
   #error "This kernel requires a gas x86 assembler!"
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
/*
 * Integer register usage shown be these defines
 */
#ifdef ATL_GAS_x8632
   #define pC      %esi
   #define pA      %ecx
   #define pB      %edi
   #define incCn   %eax
   #define stM     %edx
   #define stN     %ebx
   #define pfA     %ebp
#else
   #define pC      %rsi
   #define pA      %rcx
   #define pB      %rdi
   #define incCn   %rax
   #define stM     %rdx
   #define stN     %rbx
   #define pfA	   %rbp
   /*       rax     used in 32/64 conversion */
#endif

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
 *void ATL_AUSERMM(const int M, const int N, const int K, const TYPE alpha,
 *                const TYPE *A, const int lda, const TYPE *B, const int ldb,
 *                const TYPE beta, TYPE *C, const int ldc)
 */
	.text
.global ATL_asmdecor(ATL_USERMM)
ATL_asmdecor(ATL_USERMM):
#ifdef ATL_GAS_x8632
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
	movl	48(%esp), pA
	movl	56(%esp), pB
	movl	72(%esp), pC
	prefC((pC))
	prefC(64(pC))
/*
 *      stM = pA + NBNB-6*NB;  pfA = pA+NBNB;  stN = pB + NBNB;
 */
	movl	$NBNBso-NB6so, stM
	addl	pA, stM
	movl	stM, pfA
	addl	$NB6so, pfA
	movl	$NBNBso, stN
	addl	pB, stN
/*
 *      Set incCn = (ldc - NB)*sizeof
 */
	movl	76(%esp), incCn
	prefB((pB))
	prefB(64(pB))
	subl	$(MB-6), incCn
	shl	$3, incCn
#else
/*
 *      Save callee-saved iregs
 */
	movq	%rbp, -8(%rsp)
	movq	%rbx, -16(%rsp)
   #ifdef BETAX
	movsd	%xmm1, -24(%rsp)
      #define BETAOFF -24
   #endif
/*
 *      pA already comes in right reg
 *      Initialize pB = B; pC = C; NBso = NB * sizeof;
 */
	movq	%r9, pB
	movq	16(%rsp), pC
	prefC((pC))
	prefC(64(pC))
/*
 *      stM = pA + NBNBso;  stN = pB + NBNBso;
 */
	movq	$NBNBso-NB6so, stM
	addq	pA, stM
	movq	stM, pfA
	addq	$NB6so, pfA
	movq	$NBNBso, stN
	addq	pB, stN
/*
 *      convert ldc to 64 bits, and then set incCn = (ldc - NB)*sizeof
 */
	movl	24(%rsp), %eax
	cltq
	prefB((pB))
	prefB(64(pB))
/*	movq	%rax, incCn */
	subq	$NB, incCn
	shl	$3, incCn
	addq	$48, incCn
#endif
NLOOP:
/*
 *      stK = pB + NBso
 */
/*	movq	pB, stK */
/*	addq	$NBso, stK */
MLOOP:
/*
 *Load C, apply beta.  Stack will be:
 * st(0)  temp
 * st(1)  temp
 * st(2)  pC[0]
 * st(3)  pC[1]
 * st(4)  pC[2]
 * st(5)  pC[3]
 * st(6)  pC[4]
 * st(7)  pC[5]
 */
#ifdef BETA0
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz
#else
	fldl	40(pC)
	fldl	32(pC)
	fldl	24(pC)
	fldl	16(pC)
	fldl	8(pC)
	fldl	(pC)
   #ifdef BETAX
	fldl	BETAOFF(%rsp)
	fmul	%st, %st(1)
	fmul	%st, %st(2)
	fmul	%st, %st(3)
	fmul	%st, %st(4)
	fmul	%st, %st(5)
	fmulp	%st, %st(6)
   #endif
#endif
	ALIGN16
/*KLOOP: */
	pref2((pfA))
	fldl	(pB)
	fldl	(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	8(pB)
	fldl	8(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	8+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	8+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	8+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	8+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	8+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)
	pref2(64(pfA))

	fldl	16(pB)
	fldl	16(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	16+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	16+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	16+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	16+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	16+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	24(pB)
	fldl	24(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	24+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	24+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	24+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	24+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	24+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)
	pref2(128(pfA))
	addq	$160, pfA

	fldl	32(pB)
	fldl	32(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	32+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	32+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	32+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	32+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	32+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	40(pB)
	fldl	40(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	40+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	40+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	40+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	40+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	40+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	48(pB)
	fldl	48(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	48+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	48+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	48+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	48+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	48+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	56(pB)
	fldl	56(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	56+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	56+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	56+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	56+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	56+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	64(pB)
	fldl	64(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	64+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	64+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	64+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	64+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	64+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	72(pB)
	fldl	72(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	72+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	72+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	72+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	72+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	72+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	80(pB)
	fldl	80(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	80+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	80+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	80+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	80+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	80+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	88(pB)
	fldl	88(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	88+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	88+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	88+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	88+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	88+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	96(pB)
	fldl	96(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	96+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	96+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	96+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	96+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	96+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	104(pB)
	fldl	104(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	104+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	104+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	104+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	104+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	104+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	112(pB)
	fldl	112(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	112+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	112+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	112+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	112+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	112+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	120(pB)
	fldl	120(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	120+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	120+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	120+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	120+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	120+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	128(pB)
	fldl	128(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	128+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	128+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	128+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	128+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	128+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	136(pB)
	fldl	136(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	136+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	136+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	136+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	136+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	136+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	144(pB)
	fldl	144(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	144+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	144+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	144+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	144+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	144+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	152(pB)
	fldl	152(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	152+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	152+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	152+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	152+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	152+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	160(pB)
	fldl	160(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	160+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	160+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	160+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	160+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	160+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	168(pB)
	fldl	168(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	168+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	168+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	168+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	168+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	168+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	176(pB)
	fldl	176(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	176+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	176+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	176+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	176+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	176+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	184(pB)
	fldl	184(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	184+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	184+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	184+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	184+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	184+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	192(pB)
	fldl	192(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	192+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	192+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	192+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	192+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	192+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	200(pB)
	fldl	200(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	200+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	200+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	200+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	200+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	200+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	208(pB)
	fldl	208(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	208+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	208+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	208+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	208+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	208+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	216(pB)
	fldl	216(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	216+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	216+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	216+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	216+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	216+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	224(pB)
	fldl	224(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	224+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	224+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	224+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	224+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	224+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	232(pB)
	fldl	232(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	232+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	232+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	232+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	232+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	232+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	240(pB)
	fldl	240(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	240+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	240+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	240+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	240+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	240+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	248(pB)
	fldl	248(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	248+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	248+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	248+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	248+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	248+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	256(pB)
	fldl	256(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	256+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	256+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	256+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	256+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	256+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	264(pB)
	fldl	264(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	264+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	264+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	264+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	264+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	264+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	272(pB)
	fldl	272(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	272+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	272+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	272+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	272+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	272+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	280(pB)
	fldl	280(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	280+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	280+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	280+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	280+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	280+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	288(pB)
	fldl	288(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	288+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	288+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	288+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	288+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	288+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	296(pB)
	fldl	296(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	296+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	296+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	296+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	296+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	296+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	304(pB)
	fldl	304(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	304+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	304+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	304+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	304+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	304+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	312(pB)
	fldl	312(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	312+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	312+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	312+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	312+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	312+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	320(pB)
	fldl	320(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	320+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	320+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	320+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	320+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	320+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	328(pB)
	fldl	328(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	328+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	328+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	328+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	328+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	328+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	336(pB)
	fldl	336(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	336+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	336+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	336+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	336+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	336+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	344(pB)
	fldl	344(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	344+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	344+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	344+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	344+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	344+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	352(pB)
	fldl	352(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	352+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	352+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	352+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	352+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	352+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	360(pB)
	fldl	360(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	360+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	360+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	360+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	360+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	360+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	368(pB)
	fldl	368(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	368+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	368+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	368+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	368+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	368+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	376(pB)
	fldl	376(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	376+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	376+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	376+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	376+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	376+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	384(pB)
	fldl	384(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	384+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	384+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	384+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	384+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	384+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	392(pB)
	fldl	392(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	392+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	392+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	392+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	392+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	392+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	400(pB)
	fldl	400(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	400+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	400+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	400+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	400+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	400+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	408(pB)
	fldl	408(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	408+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	408+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	408+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	408+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	408+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	416(pB)
	fldl	416(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	416+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	416+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	416+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	416+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	416+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	424(pB)
	fldl	424(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	424+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	424+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	424+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	424+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	424+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	432(pB)
	fldl	432(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	432+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	432+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	432+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	432+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	432+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	440(pB)
	fldl	440(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	440+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	440+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	440+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	440+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	440+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	448(pB)
	fldl	448(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	448+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	448+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	448+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	448+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	448+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)
	prefC(48(pC))

	fldl	456(pB)
	fldl	456(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	456+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	456+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	456+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	456+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	456+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	464(pB)
	fldl	464(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	464+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	464+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	464+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	464+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	464+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	472(pB)
	fldl	472(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	472+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	472+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	472+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	472+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	472+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

/*	addq	$480, pA */
/*	addq	$480, pB */
/*
 *      While (pB != stK);
 */
/*	cmp	pB, stK */
/*	jne	KLOOP */
/*
 *      Write results back to C
 */
        fstpl	(pC)
        fstpl	8(pC)
        fstpl	16(pC)
        fstpl	24(pC)
        fstpl	32(pC)
        fstpl	40(pC)

/*
 *      pC += 6;  pA += 5*NB; pB -= NB;
 */
	addq	$48, pC
	addq	$NB6so, pA
#ifdef BETA0
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz
#else
	fldl	40(pC)
	fldl	32(pC)
	fldl	24(pC)
	fldl	16(pC)
	fldl	8(pC)
	fldl	(pC)
   #ifdef BETAX
	fldl	BETAOFF(%rsp)
	fmul	%st, %st(1)
	fmul	%st, %st(2)
	fmul	%st, %st(3)
	fmul	%st, %st(4)
	fmul	%st, %st(5)
	fmulp	%st, %st(6)
   #endif
#endif
	ALIGN16
/*KLOOP: */
	fldl	(pB)
	fldl	(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	8(pB)
	fldl	8(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	8+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	8+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	8+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	8+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	8+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	16(pB)
	fldl	16(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	16+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	16+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	16+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	16+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	16+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	24(pB)
	fldl	24(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	24+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	24+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	24+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	24+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	24+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	32(pB)
	fldl	32(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	32+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	32+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	32+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	32+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	32+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	40(pB)
	fldl	40(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	40+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	40+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	40+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	40+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	40+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	48(pB)
	fldl	48(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	48+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	48+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	48+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	48+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	48+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	56(pB)
	fldl	56(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	56+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	56+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	56+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	56+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	56+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	64(pB)
	fldl	64(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	64+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	64+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	64+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	64+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	64+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	72(pB)
	fldl	72(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	72+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	72+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	72+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	72+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	72+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	80(pB)
	fldl	80(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	80+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	80+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	80+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	80+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	80+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	88(pB)
	fldl	88(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	88+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	88+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	88+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	88+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	88+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	96(pB)
	fldl	96(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	96+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	96+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	96+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	96+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	96+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	104(pB)
	fldl	104(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	104+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	104+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	104+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	104+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	104+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	112(pB)
	fldl	112(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	112+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	112+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	112+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	112+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	112+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	120(pB)
	fldl	120(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	120+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	120+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	120+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	120+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	120+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	128(pB)
	fldl	128(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	128+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	128+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	128+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	128+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	128+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	136(pB)
	fldl	136(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	136+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	136+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	136+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	136+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	136+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	144(pB)
	fldl	144(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	144+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	144+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	144+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	144+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	144+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	152(pB)
	fldl	152(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	152+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	152+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	152+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	152+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	152+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	160(pB)
	fldl	160(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	160+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	160+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	160+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	160+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	160+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	168(pB)
	fldl	168(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	168+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	168+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	168+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	168+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	168+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	176(pB)
	fldl	176(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	176+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	176+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	176+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	176+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	176+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	184(pB)
	fldl	184(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	184+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	184+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	184+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	184+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	184+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	192(pB)
	fldl	192(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	192+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	192+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	192+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	192+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	192+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	200(pB)
	fldl	200(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	200+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	200+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	200+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	200+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	200+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	208(pB)
	fldl	208(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	208+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	208+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	208+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	208+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	208+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	216(pB)
	fldl	216(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	216+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	216+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	216+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	216+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	216+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	224(pB)
	fldl	224(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	224+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	224+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	224+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	224+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	224+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	232(pB)
	fldl	232(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	232+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	232+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	232+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	232+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	232+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	240(pB)
	fldl	240(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	240+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	240+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	240+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	240+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	240+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	248(pB)
	fldl	248(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	248+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	248+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	248+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	248+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	248+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	256(pB)
	fldl	256(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	256+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	256+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	256+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	256+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	256+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	264(pB)
	fldl	264(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	264+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	264+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	264+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	264+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	264+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	272(pB)
	fldl	272(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	272+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	272+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	272+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	272+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	272+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	280(pB)
	fldl	280(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	280+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	280+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	280+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	280+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	280+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	288(pB)
	fldl	288(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	288+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	288+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	288+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	288+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	288+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	296(pB)
	fldl	296(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	296+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	296+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	296+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	296+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	296+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	304(pB)
	fldl	304(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	304+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	304+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	304+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	304+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	304+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	312(pB)
	fldl	312(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	312+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	312+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	312+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	312+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	312+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	320(pB)
	fldl	320(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	320+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	320+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	320+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	320+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	320+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	328(pB)
	fldl	328(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	328+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	328+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	328+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	328+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	328+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	336(pB)
	fldl	336(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	336+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	336+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	336+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	336+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	336+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	344(pB)
	fldl	344(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	344+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	344+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	344+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	344+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	344+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	352(pB)
	fldl	352(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	352+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	352+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	352+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	352+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	352+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	360(pB)
	fldl	360(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	360+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	360+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	360+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	360+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	360+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	368(pB)
	fldl	368(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	368+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	368+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	368+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	368+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	368+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	376(pB)
	fldl	376(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	376+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	376+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	376+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	376+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	376+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	384(pB)
	fldl	384(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	384+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	384+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	384+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	384+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	384+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	392(pB)
	fldl	392(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	392+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	392+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	392+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	392+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	392+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	400(pB)
	fldl	400(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	400+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	400+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	400+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	400+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	400+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	408(pB)
	fldl	408(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	408+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	408+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	408+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	408+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	408+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	416(pB)
	fldl	416(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	416+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	416+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	416+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	416+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	416+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	424(pB)
	fldl	424(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	424+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	424+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	424+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	424+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	424+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	432(pB)
	fldl	432(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	432+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	432+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	432+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	432+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	432+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	440(pB)
	fldl	440(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	440+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	440+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	440+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	440+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	440+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	448(pB)
	fldl	448(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	448+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	448+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	448+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	448+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	448+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)
	prefC(48(pC))

	fldl	456(pB)
	fldl	456(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	456+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	456+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	456+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	456+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	456+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	464(pB)
	fldl	464(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	464+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	464+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	464+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	464+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	464+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	472(pB)
	fldl	472(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	472+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	472+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	472+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	472+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	472+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

/*	addq	$480, pA */
/*	addq	$480, pB */
/*
 *      While (pB != stK);
 */
/*	cmp	pB, stK */
/*	jne	KLOOP */
/*
 *      Write results back to C
 */
        fstpl	(pC)
        fstpl	8(pC)
        fstpl	16(pC)
        fstpl	24(pC)
        fstpl	32(pC)
        fstpl	40(pC)

/*
 *      pC += 6;  pA += 5*NB; pB -= NB;
 */
	addq	$48, pC
	addq	$NB6so, pA
#ifdef BETA0
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz
#else
	fldl	40(pC)
	fldl	32(pC)
	fldl	24(pC)
	fldl	16(pC)
	fldl	8(pC)
	fldl	(pC)
   #ifdef BETAX
	fldl	BETAOFF(%rsp)
	fmul	%st, %st(1)
	fmul	%st, %st(2)
	fmul	%st, %st(3)
	fmul	%st, %st(4)
	fmul	%st, %st(5)
	fmulp	%st, %st(6)
   #endif
#endif
	ALIGN16
/*KLOOP: */
	fldl	(pB)
	fldl	(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	8(pB)
	fldl	8(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	8+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	8+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	8+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	8+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	8+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	16(pB)
	fldl	16(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	16+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	16+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	16+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	16+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	16+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	24(pB)
	fldl	24(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	24+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	24+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	24+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	24+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	24+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	32(pB)
	fldl	32(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	32+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	32+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	32+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	32+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	32+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	40(pB)
	fldl	40(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	40+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	40+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	40+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	40+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	40+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	48(pB)
	fldl	48(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	48+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	48+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	48+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	48+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	48+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	56(pB)
	fldl	56(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	56+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	56+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	56+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	56+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	56+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	64(pB)
	fldl	64(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	64+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	64+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	64+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	64+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	64+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	72(pB)
	fldl	72(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	72+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	72+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	72+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	72+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	72+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	80(pB)
	fldl	80(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	80+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	80+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	80+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	80+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	80+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	88(pB)
	fldl	88(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	88+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	88+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	88+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	88+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	88+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	96(pB)
	fldl	96(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	96+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	96+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	96+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	96+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	96+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	104(pB)
	fldl	104(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	104+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	104+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	104+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	104+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	104+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	112(pB)
	fldl	112(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	112+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	112+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	112+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	112+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	112+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	120(pB)
	fldl	120(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	120+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	120+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	120+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	120+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	120+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	128(pB)
	fldl	128(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	128+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	128+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	128+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	128+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	128+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	136(pB)
	fldl	136(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	136+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	136+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	136+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	136+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	136+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	144(pB)
	fldl	144(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	144+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	144+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	144+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	144+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	144+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	152(pB)
	fldl	152(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	152+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	152+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	152+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	152+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	152+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	160(pB)
	fldl	160(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	160+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	160+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	160+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	160+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	160+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	168(pB)
	fldl	168(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	168+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	168+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	168+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	168+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	168+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	176(pB)
	fldl	176(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	176+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	176+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	176+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	176+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	176+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	184(pB)
	fldl	184(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	184+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	184+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	184+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	184+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	184+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	192(pB)
	fldl	192(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	192+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	192+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	192+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	192+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	192+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	200(pB)
	fldl	200(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	200+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	200+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	200+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	200+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	200+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	208(pB)
	fldl	208(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	208+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	208+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	208+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	208+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	208+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	216(pB)
	fldl	216(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	216+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	216+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	216+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	216+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	216+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	224(pB)
	fldl	224(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	224+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	224+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	224+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	224+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	224+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	232(pB)
	fldl	232(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	232+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	232+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	232+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	232+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	232+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	240(pB)
	fldl	240(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	240+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	240+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	240+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	240+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	240+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	248(pB)
	fldl	248(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	248+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	248+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	248+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	248+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	248+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	256(pB)
	fldl	256(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	256+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	256+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	256+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	256+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	256+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	264(pB)
	fldl	264(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	264+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	264+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	264+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	264+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	264+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	272(pB)
	fldl	272(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	272+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	272+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	272+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	272+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	272+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	280(pB)
	fldl	280(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	280+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	280+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	280+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	280+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	280+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	288(pB)
	fldl	288(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	288+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	288+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	288+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	288+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	288+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	296(pB)
	fldl	296(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	296+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	296+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	296+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	296+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	296+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	304(pB)
	fldl	304(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	304+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	304+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	304+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	304+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	304+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	312(pB)
	fldl	312(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	312+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	312+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	312+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	312+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	312+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	320(pB)
	fldl	320(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	320+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	320+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	320+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	320+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	320+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	328(pB)
	fldl	328(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	328+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	328+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	328+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	328+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	328+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	336(pB)
	fldl	336(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	336+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	336+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	336+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	336+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	336+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	344(pB)
	fldl	344(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	344+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	344+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	344+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	344+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	344+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	352(pB)
	fldl	352(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	352+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	352+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	352+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	352+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	352+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	360(pB)
	fldl	360(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	360+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	360+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	360+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	360+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	360+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	368(pB)
	fldl	368(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	368+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	368+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	368+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	368+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	368+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	376(pB)
	fldl	376(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	376+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	376+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	376+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	376+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	376+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	384(pB)
	fldl	384(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	384+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	384+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	384+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	384+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	384+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	392(pB)
	fldl	392(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	392+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	392+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	392+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	392+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	392+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	400(pB)
	fldl	400(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	400+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	400+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	400+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	400+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	400+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	408(pB)
	fldl	408(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	408+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	408+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	408+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	408+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	408+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	416(pB)
	fldl	416(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	416+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	416+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	416+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	416+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	416+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	424(pB)
	fldl	424(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	424+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	424+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	424+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	424+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	424+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	432(pB)
	fldl	432(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	432+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	432+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	432+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	432+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	432+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	440(pB)
	fldl	440(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	440+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	440+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	440+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	440+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	440+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	448(pB)
	fldl	448(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	448+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	448+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	448+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	448+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	448+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)
	prefC(48(pC))

	fldl	456(pB)
	fldl	456(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	456+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	456+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	456+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	456+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	456+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	464(pB)
	fldl	464(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	464+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	464+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	464+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	464+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	464+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	472(pB)
	fldl	472(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	472+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	472+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	472+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	472+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	472+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

/*	addq	$480, pA */
/*	addq	$480, pB */
/*
 *      While (pB != stK);
 */
/*	cmp	pB, stK */
/*	jne	KLOOP */
/*
 *      Write results back to C
 */
        fstpl	(pC)
        fstpl	8(pC)
        fstpl	16(pC)
        fstpl	24(pC)
        fstpl	32(pC)
        fstpl	40(pC)

/*
 *      pC += 6;  pA += 5*NB; pB -= NB;
 */
	addq	$48, pC
	addq	$NB6so, pA
/*
 *      while (pA != stM);
 */
	cmp	pA, stM
	jne	MLOOP
#ifdef BETA0
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz
#else
	fldl	40(pC)
	fldl	32(pC)
	fldl	24(pC)
	fldl	16(pC)
	fldl	8(pC)
	fldl	(pC)
   #ifdef BETAX
	fldl	BETAOFF(%rsp)
	fmul	%st, %st(1)
	fmul	%st, %st(2)
	fmul	%st, %st(3)
	fmul	%st, %st(4)
	fmul	%st, %st(5)
	fmulp	%st, %st(6)
   #endif
#endif
	ALIGN16
/*KLOOP: */
	prefB(NBso(pB))
	fldl	(pB)
	fldl	(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	8(pB)
	fldl	8(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	8+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	8+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	8+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	8+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	8+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	16(pB)
	fldl	16(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	16+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	16+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	16+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	16+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	16+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)
	prefB(64+NBso(pB))
	prefB(128+NBso(pB))

	fldl	24(pB)
	fldl	24(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	24+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	24+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	24+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	24+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	24+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	32(pB)
	fldl	32(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	32+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	32+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	32+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	32+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	32+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)
	prefB(192+NBso(pB))
	prefB(256+NBso(pB))

	fldl	40(pB)
	fldl	40(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	40+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	40+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	40+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	40+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	40+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	48(pB)
	fldl	48(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	48+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	48+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	48+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	48+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	48+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)
	prefB(320+NBso(pB))

	fldl	56(pB)
	fldl	56(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	56+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	56+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	56+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	56+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	56+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	64(pB)
	fldl	64(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	64+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	64+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	64+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	64+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	64+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	72(pB)
	fldl	72(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	72+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	72+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	72+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	72+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	72+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	80(pB)
	fldl	80(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	80+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	80+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	80+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	80+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	80+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	88(pB)
	fldl	88(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	88+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	88+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	88+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	88+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	88+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	96(pB)
	fldl	96(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	96+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	96+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	96+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	96+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	96+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	104(pB)
	fldl	104(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	104+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	104+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	104+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	104+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	104+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	112(pB)
	fldl	112(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	112+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	112+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	112+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	112+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	112+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	120(pB)
	fldl	120(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	120+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	120+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	120+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	120+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	120+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	128(pB)
	fldl	128(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	128+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	128+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	128+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	128+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	128+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	136(pB)
	fldl	136(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	136+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	136+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	136+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	136+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	136+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	144(pB)
	fldl	144(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	144+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	144+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	144+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	144+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	144+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	152(pB)
	fldl	152(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	152+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	152+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	152+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	152+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	152+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	160(pB)
	fldl	160(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	160+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	160+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	160+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	160+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	160+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	168(pB)
	fldl	168(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	168+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	168+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	168+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	168+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	168+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	176(pB)
	fldl	176(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	176+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	176+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	176+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	176+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	176+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	184(pB)
	fldl	184(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	184+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	184+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	184+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	184+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	184+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	192(pB)
	fldl	192(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	192+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	192+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	192+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	192+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	192+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	200(pB)
	fldl	200(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	200+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	200+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	200+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	200+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	200+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	208(pB)
	fldl	208(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	208+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	208+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	208+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	208+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	208+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	216(pB)
	fldl	216(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	216+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	216+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	216+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	216+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	216+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	224(pB)
	fldl	224(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	224+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	224+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	224+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	224+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	224+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	232(pB)
	fldl	232(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	232+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	232+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	232+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	232+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	232+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	240(pB)
	fldl	240(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	240+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	240+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	240+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	240+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	240+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	248(pB)
	fldl	248(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	248+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	248+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	248+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	248+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	248+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	256(pB)
	fldl	256(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	256+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	256+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	256+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	256+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	256+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	264(pB)
	fldl	264(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	264+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	264+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	264+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	264+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	264+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	272(pB)
	fldl	272(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	272+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	272+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	272+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	272+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	272+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	280(pB)
	fldl	280(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	280+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	280+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	280+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	280+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	280+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	288(pB)
	fldl	288(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	288+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	288+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	288+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	288+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	288+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	296(pB)
	fldl	296(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	296+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	296+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	296+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	296+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	296+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	304(pB)
	fldl	304(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	304+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	304+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	304+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	304+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	304+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	312(pB)
	fldl	312(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	312+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	312+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	312+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	312+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	312+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	320(pB)
	fldl	320(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	320+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	320+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	320+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	320+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	320+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	328(pB)
	fldl	328(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	328+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	328+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	328+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	328+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	328+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	336(pB)
	fldl	336(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	336+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	336+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	336+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	336+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	336+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	344(pB)
	fldl	344(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	344+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	344+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	344+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	344+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	344+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	352(pB)
	fldl	352(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	352+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	352+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	352+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	352+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	352+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	360(pB)
	fldl	360(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	360+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	360+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	360+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	360+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	360+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	368(pB)
	fldl	368(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	368+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	368+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	368+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	368+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	368+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	376(pB)
	fldl	376(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	376+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	376+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	376+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	376+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	376+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	384(pB)
	fldl	384(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	384+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	384+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	384+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	384+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	384+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	392(pB)
	fldl	392(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	392+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	392+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	392+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	392+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	392+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	400(pB)
	fldl	400(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	400+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	400+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	400+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	400+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	400+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	408(pB)
	fldl	408(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	408+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	408+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	408+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	408+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	408+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	416(pB)
	fldl	416(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	416+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	416+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	416+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	416+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	416+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	424(pB)
	fldl	424(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	424+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	424+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	424+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	424+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	424+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)
	prefB(384+NBso(pB))

	fldl	432(pB)
	fldl	432(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	432+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	432+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	432+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	432+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	432+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)
	prefB(448+NBso(pB))

	fldl	440(pB)
	fldl	440(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	440+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	440+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	440+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	440+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	440+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	448(pB)
	fldl	448(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	448+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	448+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	448+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	448+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	448+NB5so(pA)
	fmulp	%st, %st(1)
	addq	incCn, pC
	faddp	%st, %st(6)
	prefC((pC))

	fldl	456(pB)
	fldl	456(pA)
	fmul	%st(1), %st
	subq	incCn, pC
	faddp	%st, %st(2)
	fldl	456+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	456+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	456+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	456+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	456+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	464(pB)
	fldl	464(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	464+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	464+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	464+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	464+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	464+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

	fldl	472(pB)
	fldl	472(pA)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	472+NBso(pA)
	fmul	%st(1), %st
	faddp	%st, %st(3)
	fldl	472+NB2so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(4)
	fldl	472+NB3so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(5)
	fldl	472+NB4so(pA)
	fmul	%st(1), %st
	faddp	%st, %st(6)
	fldl	472+NB5so(pA)
	fmulp	%st, %st(1)
	faddp	%st, %st(6)

/*
 *      While (pB != stK);
 */
/*	cmp	pB, stK */
/*	jne	KLOOP */
/*
 *      Write results back to C
 */
        fstpl	(pC)
        fstpl	8(pC)
        fstpl	16(pC)
        fstpl	24(pC)
        fstpl	32(pC)
        fstpl	40(pC)
/*
 *      pC += incCn;  pA -= NBNB;  pB += NB;
 */
	addq	incCn, pC
	subq	$NBNBso-NB6so, pA
	addq	$NBso, pB
/*
 *      while (pB != stN);
 */
	cmp	pB, stN
	jne	NLOOP

/*
 *      Restore callee-saved iregs
 */
#ifdef ATL_GAS_x8632
	movl	20(%esp), %ebp
	movl	16(%esp), %ebx
	movl	12(%esp), %esi
	movl	8(%esp), %edi
   	addl	$24,%esp
#else
	movq	-8(%rsp), %rbp
	movq	-16(%rsp), %rbx
#endif
	ret
