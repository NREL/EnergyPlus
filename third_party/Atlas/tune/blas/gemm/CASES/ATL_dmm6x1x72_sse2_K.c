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
#include "atlas_asm.h"


#ifndef ATL_GAS_x8632
   #error "This kernel requires gas x86-32 assembler!"
#endif
#ifndef ATL_SSE2
   #error "This routine requires SSE2!"
#endif
#if !defined(KB) || (KB == 0)
   #error "KB must be a compile-time constant!"
#endif
#if !defined(NB)
   #define NB 0
#endif
#if !defined(MB)
   #define MB 0
#endif
#if (MB/6)*6 != MB
   #error "MB must be multiple of 6!"
#endif
/*
 * Integer register usage shown be these defines
 */
#define pC      %esi
#define pA      %ecx
#define pB      %edi
#define incCn   %eax
#define stM	%bl
#define stN	%bh
#define ldab	%edx
#define pA3	%ebp

#define pA0	pA
#define pB0	pB
#define pfA	incCn

#define rC0	%xmm0
#define rC1	%xmm1
#define rC2	%xmm2
#define rC3	%xmm3
#define rC4	%xmm4
#define rC5	%xmm5
#define rA0	%xmm6
#define rB0	%xmm7

#define NBso	(KB*8)
#if MB != 0
   #define MBKBso  (MB*KB*8)
#endif
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
/*offset                    4            8           12                16
 *void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha,
 *offset                     24             28             32            36
 *                const TYPE *A, const int lda, const TYPE *B, const int ldb,
 *offset                       40       48             52
 *                const TYPE beta, TYPE *C, const int ldc)
 */
	.text
.global ATL_asmdecor(ATL_USERMM)
ATL_asmdecor(ATL_USERMM):
/*
 *	Save callee-saved iregs; Save old stack pointer in eax,
 *      so we can adjust for BETA alignment
 */
	movl %esp, %eax
   #ifdef BETAX
	subl	$48, %esp
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
	subl	$28, %esp
	movl	%ebp, 12(%esp)
	movl	%ebx,  8(%esp)
	movl	%esi,  4(%esp)
	movl	%edi,   (%esp)
      #define COFF 16
   #endif
/*
 *      Initialize pA = A;  pB = B; pC = C;
 */
#if MB == 0
        movl    4(%eax), %ebx
        movl    %ebx, COFF+4(%esp)
        imul    $NBso, %ebx
        movl    %ebx, COFF+8(%esp)
#endif
	movl	24(%eax), pA
	movl	32(%eax), pB
	movl	48(%eax), pC
#if NB == 0
        movb    8(%eax), stN
#else
        movb    $NB, stN
#endif
	addl	$120, pA
	addl	$120, pB
/*
 *      Set incCn = (ldc - NB)*sizeof
 */
	movl	52(%eax), incCn
#if MB == 0
        subl    COFF+4(%esp), incCn
#else
	subl	$MB, incCn
#endif
   #ifdef DCPLX
	shl	$4, incCn
   #else
	shl	$3, incCn
   #endif
   	movl	incCn, COFF(%esp)
	movl	$NBso, ldab
	movl	pA0, pA3
	addl	$NB3so, pA3
        movl    pA0, pfA
#if MB == 0
        subl    $120, pfA
        addl    COFF+8(%esp), pfA
#else
	addl	$MBKBso-120, pfA
#endif
UNLOOP:
#if MB == 0
        movb    COFF+4(%esp), stM
#else
        movb    $MB, stM
#endif
UMLOOP:
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

#if KB > 2
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
#endif

#if KB > 4
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
#endif

#if KB > 6
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
#endif

#if KB > 8
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
#endif

#if KB > 10
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
#endif

#if KB > 12
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
#endif

#if KB > 14
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
#endif

#if KB > 16
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
#endif

#if KB > 18
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
#endif

#if KB > 20
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
#endif

#if KB > 22
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
#endif

#if KB > 24
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
#endif

#if KB > 26
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
#endif

#if KB > 28
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
#endif

#if KB > 30
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
#endif

#if KB > 32
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
#endif

#if KB > 34
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
#endif

#if KB > 36
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
#endif

#if KB > 38
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
#endif

#if KB > 40
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
#endif

#if KB > 42
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
#endif

#if KB > 44
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
#endif

#if KB > 46
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
#endif

#if KB > 48
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
#endif

#if KB > 50
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
#endif

#if KB > 52
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
#endif

#if KB > 54
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
#endif

#if KB > 56
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
#endif

#if KB > 58
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
#endif

#if KB > 60
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
#endif

#if KB > 62
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
#endif

#if KB > 64
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
#endif

#if KB > 66
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
#endif
						pref2((pfA))
						addl	$52, pfA
#if KB > 68
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
#endif

#if KB > 70
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
#endif

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
/*
 *      while (pA != stM);
 */
	subb	$6, stM
	jnz	UMLOOP
/*
 *      pC += incCn;  pA -= NBNB;  pB += NB;
 */
   	addl	COFF(%esp), pC
        addl    $NBso, pB0
#if MB == 0
        subl    COFF+8(%esp), pA0
        lea     NBso(pA0,ldab,2), pA3
#else
        subl    $MBKBso, pA0
        subl    $MBKBso, pA3
#endif
/*
 *      while (pB != stN);
 */
	sub	$1, stN
	jnz	UNLOOP

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
	addl	$28, %esp
   #endif
	ret
