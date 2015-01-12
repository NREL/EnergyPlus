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
#ifdef ATL_OS_SunOS
   #define ATL_DIV_NUM NB
   #define ATL_DIV_DEN 6
#endif
#include "atlas_asm.h"


#ifndef ATL_GAS_x8632
   #error "This kernel requires x86-32 assembly!"
#endif
#ifndef ATL_SSE2
   #error "This routine requires SSE2!"
#endif

#if !defined(KB) || (KB == 0)
   #error "KB must be a compile-time constant!"
#endif
#if (KB != 72)
   #error "KB must be 72!"
#endif
#if NB != KB
   #error "NB must equal KB!"
#endif
#if (NB/6)*6 != NB
   #error "NB must be evenly divisable by 6!"
#endif
#if !defined(MB)
   #define MB 0
#endif
/*
 * Integer register usage shown be these defines
 */
#define pC      %esi
#define pA      %ecx
#define pB      %edi
#define pfA     %eax
#define stM	%bl
#define stN	%bh
#define ldab	%edx
#define pA3	%ebp

#define pA0	pA
#define pB0	pB
#define ldc     ldab

#define rC0	%xmm0
#define rC1	%xmm1
#define rC2	%xmm2
#define rC3	%xmm3
#define rC4	%xmm4
#define rC5	%xmm5
#define rA0	%xmm6
#define rB0	%xmm7

#define NBso	(KB*8)
#define NBNBso  (KB*KB*8)
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
#define prefB(mem) prefetcht1	mem
#define prefC(mem) prefetcht0	mem
#else
#define pref2(mem)
#define prefB(mem)
#define prefC(mem)
#endif
/*offsets                   4            8          12               16
 *void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha,
 *offsets                    24             28             32            36
 *                const TYPE *A, const int lda, const TYPE *B, const int ldb,
 * offsets                     40       48             52
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
	subl	$44, %esp
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
	#define COFF 36
      #define BETAOFF 0
   #else
	subl	$24, %esp
	movl	%ebp, 12(%esp)
	movl	%ebx,  8(%esp)
	movl	%esi,  4(%esp)
	movl	%edi,   (%esp)
      #define COFF 16
   #endif
/*
 *      Initialize pA = A;  pB = B; pC = C;
 */
	movl	32(%eax), pA
	movl	24(%eax), pB
	movl	48(%eax), pC
        movb    4(%eax), stN
	addl	$120, pA
	addl	$120, pB
/*
 *      Set ldc = ldc*sizeof
 */
	movl	52(%eax), ldc
   #ifdef DCPLX
	shl	$4, ldc
   #else
	shl	$3, ldc
   #endif
   	movl	ldc, COFF(%esp)
	movl	pA0, pA3
	addl	$NB3so, pA3
	movl	$NBNBso-120, pfA
	addl	pA0, pfA
        movl    pC, COFF+4(%esp)
NLOOP:
   #ifdef ATL_DivAns
	mov	$ATL_DivAns-1, stM
   #else
	mov	$NB/6-1, stM
   #endif
MLOOP:
#ifdef BETA0
	xorpd	rC0, rC0
	xorpd	rC1, rC1
	xorpd	rC2, rC2
	xorpd	rC3, rC3
	xorpd	rC4, rC4
	xorpd	rC5, rC5
#else
	movsd	(pC), rC0
	movsd	(pC,ldc), rC1
	movsd	(pC,ldc,2), rC2
	movsd	(pC,ldc,4), rC4
        addl    ldc, pC
	movsd	(pC,ldc,2), rC3
	movsd	(pC,ldc,4), rC5
        subl    ldc, pC
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
	                        movl	$NBso, ldab
	movapd	0-120(pB0), rB0
	movapd	0-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	0-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	0-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	0-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	0-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	0-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	16-120(pB0), rB0
	movapd	16-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	16-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	16-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	16-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	16-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	16-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	32-120(pB0), rB0
	movapd	32-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	32-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	32-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	32-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	32-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	32-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	48-120(pB0), rB0
	movapd	48-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	48-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	48-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	48-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	48-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	48-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	64-120(pB0), rB0
	movapd	64-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	64-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	64-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	64-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	64-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	64-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	80-120(pB0), rB0
	movapd	80-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	80-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	80-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	80-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	80-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	80-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	96-120(pB0), rB0
	movapd	96-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	96-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	96-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	96-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	96-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	96-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	112-120(pB0), rB0
	movapd	112-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	112-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	112-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	112-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	112-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	112-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	128-120(pB0), rB0
	movapd	128-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	128-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	128-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	128-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	128-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	128-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	144-120(pB0), rB0
	movapd	144-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	144-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	144-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	144-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	144-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	144-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	160-120(pB0), rB0
	movapd	160-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	160-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	160-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	160-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	160-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	160-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	176-120(pB0), rB0
	movapd	176-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	176-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	176-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	176-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	176-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	176-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	192-120(pB0), rB0
	movapd	192-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	192-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	192-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	192-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	192-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	192-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	208-120(pB0), rB0
	movapd	208-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	208-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	208-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	208-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	208-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	208-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	224-120(pB0), rB0
	movapd	224-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	224-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	224-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	224-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	224-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	224-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	240-120(pB0), rB0
	movapd	240-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	240-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	240-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	240-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	240-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	240-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	256-120(pB0), rB0
	movapd	256-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	256-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	256-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	256-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	256-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	256-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	272-120(pB0), rB0
	movapd	272-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	272-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	272-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	272-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	272-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	272-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	288-120(pB0), rB0
	movapd	288-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	288-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	288-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	288-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	288-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	288-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	304-120(pB0), rB0
	movapd	304-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	304-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	304-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	304-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	304-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	304-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	320-120(pB0), rB0
	movapd	320-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	320-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	320-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	320-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	320-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	320-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	336-120(pB0), rB0
	movapd	336-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	336-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	336-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	336-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	336-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	336-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	352-120(pB0), rB0
	movapd	352-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	352-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	352-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	352-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	352-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	352-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	368-120(pB0), rB0
	movapd	368-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	368-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	368-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	368-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	368-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	368-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	384-120(pB0), rB0
	movapd	384-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	384-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	384-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	384-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	384-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	384-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	400-120(pB0), rB0
	movapd	400-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	400-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	400-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	400-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	400-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	400-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	416-120(pB0), rB0
	movapd	416-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	416-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	416-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	416-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	416-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	416-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	432-120(pB0), rB0
	movapd	432-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	432-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	432-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	432-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	432-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	432-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	448-120(pB0), rB0
	movapd	448-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	448-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	448-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	448-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	448-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	448-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	464-120(pB0), rB0
	movapd	464-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	464-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	464-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	464-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	464-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	464-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	480-120(pB0), rB0
	movapd	480-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	480-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	480-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	480-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	480-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	480-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	496-120(pB0), rB0
	movapd	496-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	496-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	496-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	496-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	496-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	496-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	512-120(pB0), rB0
	movapd	512-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	512-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	512-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	512-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	512-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	512-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	528-120(pB0), rB0
	movapd	528-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	528-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	528-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	528-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	528-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	528-120(pA3,ldab,2), rB0
	addpd	rB0, rC5
						pref2((pfA))
						addl	$52, pfA
	movapd	544-120(pB0), rB0
	movapd	544-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	544-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	544-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	544-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	544-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	544-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	560-120(pB0), rB0
	movapd	560-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	560-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	560-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	560-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	560-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	560-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

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
        movapd          rC4, rB0
        unpcklpd        rC1, rC0        /* rC0 = c0a  c1a */
        unpcklpd        rC5, rC4        /* rC4 = c4a  c5a */
        unpckhpd        rC1, rA0        /* rA0 = c0b  c1b */
        unpckhpd        rC5, rB0        /* rB0 = c4b  c5b */
        addpd           rA0, rC0        /* rC0 = c0ab c1ab */
        addpd           rB0, rC4        /* rC4 = c4ab c5ab */
        movapd          rC2, rA0
        unpcklpd        rC3, rC2        /* rC2 = c2a  c3a */
				addl	$NB6so, pA0
				addl	$NB6so, pA3
        unpckhpd        rC3, rA0        /* rA0 = c2b  c3b */
        addpd           rA0, rC2        /* rC2 = c2ab c3ab */
/*
 *      Write results back to C
 */
                                movl    COFF(%esp), ldc
	movlpd	rC0, (pC)
	movhpd	rC0, (pC,ldc)
	movlpd	rC2, (pC,ldc,2)
	movlpd	rC4, (pC,ldc,4)
                                addl    ldc, pC
	movhpd	rC2, (pC,ldc,2)
	movhpd	rC4, (pC,ldc,4)
/*
 *      pC += 6*ldc;   pA += 6*NB
 */
        lea     (pC,ldc,4), pC
        addl    ldc, pC
/*
 *      while (pA != stM);
 */
	sub	$1, stM
	jnz	MLOOP
/*
 *       last iteration of MLOOP unrolled for prefetch
 */
#ifdef BETA0
	xorpd	rC0, rC0
	xorpd	rC1, rC1
	xorpd	rC2, rC2
	xorpd	rC3, rC3
	xorpd	rC4, rC4
	xorpd	rC5, rC5
#else
	movsd	(pC), rC0
	movsd	(pC,ldc), rC1
	movsd	(pC,ldc,2), rC2
	movsd	(pC,ldc,4), rC4
        addl    ldc, pC
	movsd	(pC,ldc,2), rC3
	movsd	(pC,ldc,4), rC5
        subl    ldc, pC
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
	                        movl	$NBso, ldab
	movapd	0-120(pB0), rB0
	movapd	0-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	0-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	0-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	0-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	0-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	0-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	16-120(pB0), rB0
	movapd	16-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	16-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	16-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	16-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	16-120(pA3,ldab), rA0
	mulpd	rB0, rA0
						prefB((pB0,ldab))
						prefB(128(pB0,ldab))
	addpd	rA0, rC4
	mulpd	16-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	32-120(pB0), rB0
	movapd	32-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	32-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	32-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	32-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	32-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	32-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	48-120(pB0), rB0
	movapd	48-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	48-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	48-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	48-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	48-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	48-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	64-120(pB0), rB0
	movapd	64-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	64-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	64-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	64-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	64-120(pA3,ldab), rA0
	mulpd	rB0, rA0
						prefB(256(pB0,ldab))
						prefB(384(pB0,ldab))
	addpd	rA0, rC4
	mulpd	64-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	80-120(pB0), rB0
	movapd	80-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	80-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	80-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	80-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	80-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	80-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	96-120(pB0), rB0
	movapd	96-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	96-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	96-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	96-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	96-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	96-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	112-120(pB0), rB0
	movapd	112-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	112-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	112-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	112-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	112-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	112-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	128-120(pB0), rB0
	movapd	128-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	128-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	128-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	128-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	128-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	128-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	144-120(pB0), rB0
	movapd	144-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	144-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	144-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	144-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	144-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	144-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	160-120(pB0), rB0
	movapd	160-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	160-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	160-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	160-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	160-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	160-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	176-120(pB0), rB0
	movapd	176-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	176-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	176-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	176-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	176-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	176-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	192-120(pB0), rB0
	movapd	192-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	192-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	192-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	192-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	192-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	192-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	208-120(pB0), rB0
	movapd	208-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	208-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	208-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	208-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	208-120(pA3,ldab), rA0
	mulpd	rB0, rA0
						prefB(448(pB0,ldab))
	addpd	rA0, rC4
	mulpd	208-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	224-120(pB0), rB0
	movapd	224-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	224-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	224-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	224-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	224-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	224-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	240-120(pB0), rB0
	movapd	240-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	240-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	240-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	240-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	240-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	240-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	256-120(pB0), rB0
	movapd	256-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	256-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	256-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	256-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	256-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	256-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	272-120(pB0), rB0
	movapd	272-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	272-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	272-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	272-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	272-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	272-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	288-120(pB0), rB0
	movapd	288-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	288-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	288-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	288-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	288-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	288-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	304-120(pB0), rB0
	movapd	304-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	304-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	304-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	304-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	304-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	304-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	320-120(pB0), rB0
	movapd	320-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	320-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	320-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	320-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	320-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	320-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	336-120(pB0), rB0
	movapd	336-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	336-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	336-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	336-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	336-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	336-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	352-120(pB0), rB0
	movapd	352-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	352-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	352-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	352-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	352-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	352-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	368-120(pB0), rB0
	movapd	368-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	368-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	368-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	368-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	368-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	368-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	384-120(pB0), rB0
	movapd	384-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	384-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	384-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	384-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	384-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	384-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	400-120(pB0), rB0
	movapd	400-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	400-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	400-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	400-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	400-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	400-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	416-120(pB0), rB0
	movapd	416-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	416-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	416-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	416-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	416-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	416-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	432-120(pB0), rB0
	movapd	432-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	432-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	432-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	432-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	432-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	432-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	448-120(pB0), rB0
	movapd	448-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	448-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	448-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	448-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	448-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	448-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	464-120(pB0), rB0
	movapd	464-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	464-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	464-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	464-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	464-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	464-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	480-120(pB0), rB0
	movapd	480-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	480-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	480-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	480-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	480-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	480-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	496-120(pB0), rB0
	movapd	496-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	496-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	496-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	496-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	496-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	496-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	512-120(pB0), rB0
	movapd	512-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	512-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	512-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	512-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	512-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	512-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	528-120(pB0), rB0
	movapd	528-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	528-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	528-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	528-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	528-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	528-120(pA3,ldab,2), rB0
	addpd	rB0, rC5
/*						pref2((pfA)) */
/*						addl	$48, pfA */
	movapd	544-120(pB0), rB0
	movapd	544-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	544-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	544-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	544-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	544-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	544-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

	movapd	560-120(pB0), rB0
	movapd	560-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	560-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	560-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	560-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	560-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	560-120(pA3,ldab,2), rB0
	addpd	rB0, rC5

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
				addl	$NBso, pB0
        movapd          rC4, rB0
        unpcklpd        rC1, rC0        /* rC0 = c0a  c1a */
				subl	$NBNBso-NB6so, pA0
        unpcklpd        rC5, rC4        /* rC4 = c4a  c5a */
        unpckhpd        rC1, rA0        /* rA0 = c0b  c1b */
        unpckhpd        rC5, rB0        /* rB0 = c4b  c5b */
        addpd           rA0, rC0        /* rC0 = c0ab c1ab */
        addpd           rB0, rC4        /* rC4 = c4ab c5ab */
        movapd          rC2, rA0
        unpcklpd        rC3, rC2        /* rC2 = c2a  c3a */
        unpckhpd        rC3, rA0        /* rA0 = c2b  c3b */
				subl	$NBNBso-NB6so, pA3
        addpd           rA0, rC2        /* rC2 = c2ab c3ab */
/*
 *      Write results back to C
 */
                                movl    COFF(%esp), ldc
	movlpd	rC0, (pC)
	movhpd	rC0, (pC,ldc)
	movlpd	rC2, (pC,ldc,2)
	movlpd	rC4, (pC,ldc,4)
                                addl    ldc, pC
	movhpd	rC2, (pC,ldc,2)
	movhpd	rC4, (pC,ldc,4)
/*
 *      pC += 6;   pA += 6*NB
 */

/*
 *      pC += ldc;  pA -= NBNB;  pB += NB;
 */
        movl    COFF+4(%esp), pC
#ifdef DCPLX
        addl    $16, pC
#else
        addl    $8, pC
#endif
        movl    pC, COFF+4(%esp)
/*
 *      while (pB != stN);
 */
	sub	$1, stN
	jnz	NLOOP

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
	addl	$24, %esp
   #endif
	ret
