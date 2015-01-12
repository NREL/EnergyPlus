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


#if !defined(ATL_GAS_x8632) && !defined(ATL_GAS_x8664)
   #error "This kernel requires x86 gas 32 or 64 bit x86 assembler!"
#endif
#ifndef ATL_SSE2
   #error "This kernel requires SSE2"
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

#if MB == 0 || defined(ATL_OS_SunOS)  /* retarded gcc on SunOS has no divis */
   #define PFAINC 52
#else
   #define PFAINC ((MB*8+MB/6-2)/(MB/6-1))
#endif

#ifdef ATL_GAS_x8664
   #define movL movl
   #define movl movq
   #define subl subq
   #define addl addq
   #define movb movq
   #define subb subq
#endif
/*
 * Integer register usage shown be these defines
 */
#ifdef ATL_GAS_x8664
   #define pC      %r10
   #define pA      %rcx
   #define pB      %r9
   #define incCn   %rax
   #define stM	   %rdi
   #define stN	   %rsi
   #define ldab	   %r8
   #define pA3	   %rdx
   #define pfA     %r11
   #define M_m     %rbx
   #define incAn_m %rbp
   #define incCn_m incCn
#else
   #define pC      %esi
   #define pA      %ecx
   #define pB      %edi
   #define incCn   %eax
   #define stM	   %bl
   #define stN	   %bh
   #define ldab	   %edx
   #define pA3	   %ebp
   #define pfA     incCn
   #ifdef BETAX
      #define COFF 36
   #else
      #define COFF 16
   #endif
   #define incCn_m COFF(%esp)
   #define M_m     COFF+4(%esp)
   #define incAn_m COFF+8(%esp)
#endif

#define pA0	pA
#define pB0	pB

#define rC0	%xmm0
#define rC1	%xmm1
#define rC2	%xmm2
#define rC3	%xmm3
#define rC4	%xmm4
#define rC5	%xmm5
#define rA0	%xmm6
#define rB0	%xmm7
#ifdef ATL_GAS_x8664
   #define rbeta %xmm8
#else
   #define rbeta rA0
#endif

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
#if defined(ATL_ARCH_Efficeon) && KB <= 44
   #define pref2(mem) prefetcht0	mem
   #define prefB(mem) prefetcht0	mem
   #define prefC(mem) prefetcht0	mem
#elif 1
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
#ifdef ATL_GAS_x8664
        movq    %rbx, -8(%rsp)
        movq    %rbp, -16(%rsp)
   #ifdef BETAX
/*        pshufd  $0b01000100, %xmm1, rbeta */
        pshufd  $0x44, %xmm1, rbeta
   #endif
        movq    16(%rsp), pC
        mov     24(%rsp), %eax    /* incCn = ldc */
        cltq
   #if MB == 0
        movq    stM, M_m
   #endif
        movq    stM, incAn_m
        subq    $6, incAn_m
        imul    $NBso, incAn_m
#else
/*
 *      Save callee-saved iregs; Save old stack pointer in eax,
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
      #define BETAOFF 0
   #else
	subl	$28, %esp
	movl	%ebp, 12(%esp)
	movl	%ebx,  8(%esp)
	movl	%esi,  4(%esp)
	movl	%edi,   (%esp)
   #endif
/*
 *      Initialize pA = A;  pB = B; pC = C;
 */
   #if MB == 0
        movl    4(%eax), %ebx
        movl    %ebx, COFF+4(%esp)
        imul    $NBso, %ebx
        subl    $NB6so, %ebx
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
#endif
	addl	$120, pA
	addl	$120, pB
/*
 *      Set incCn = (ldc - NB)*sizeof
 */
#ifndef ATL_GAS_x8664
	movl	52(%eax), incCn
#endif
#ifdef DREAL
	test	$1, incCn
	jnz	UNALIGNED
	test	$15, pC
	jnz	UNALIGNED
#if MB == 0
        subl    M_m, incCn
	addl	$6, incCn
#else
	subl	$MB-6, incCn
#endif
   #ifdef DCPLX
	shl	$4, incCn
   #else
	shl	$3, incCn
   #endif
#ifndef ATL_GAS_x8664
   	movl	incCn, COFF(%esp)
#endif
/*	mov	$NB, stN */
	movl	$NBso, ldab
/*	lea	NBso(pA0, ldab,2), pA3 */
	movl	pA0, pA3
	addl	$NB3so, pA3
        movl    pA0, pfA
#if MB == 0
        addl    $NB6so-120, pfA
        addl    incAn_m, pfA
#else
        addl    $MBKBso-120, pfA
#endif
NLOOP:
#if MB == 0
        movb    M_m, stM
        subb     $6, stM
        jz      LMLOOP
#else
        movb     $MB-6, stM
#endif
#if MB != 6
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
      #ifndef ATL_GAS_x8664
	movlpd	(%esp), rbeta
      #endif
	mulsd	rbeta, rC0
	mulsd	rbeta, rC1
	mulsd	rbeta, rC2
	mulsd	rbeta, rC3
	mulsd	rbeta, rC4
	mulsd	rbeta, rC5
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
						pref2((pfA))
						addl	$PFAINC, pfA
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

#if KB > 72
	movapd	576-120(pB0), rB0
	movapd	576-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	576-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	576-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	576-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	576-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	576-120(pA3,ldab,2), rB0
	addpd	rB0, rC5
#endif

#if KB > 74
	movapd	592-120(pB0), rB0
	movapd	592-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	592-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	592-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	592-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	592-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	592-120(pA3,ldab,2), rB0
	addpd	rB0, rC5
#endif

#if KB > 76
	movapd	608-120(pB0), rB0
	movapd	608-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	608-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	608-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	608-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	608-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	608-120(pA3,ldab,2), rB0
	addpd	rB0, rC5
#endif

#if KB > 78
	movapd	624-120(pB0), rB0
	movapd	624-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	624-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	624-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	624-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	624-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	624-120(pA3,ldab,2), rB0
	addpd	rB0, rC5
#endif

/*
 *      Get these bastard things summed up correctly
 */
#ifdef ATL_SSE3
        haddpd          rC1, rC0        /* rC0 = c0ab c1ab */
                                addl    $NB6so, pA0
        haddpd          rC3, rC2        /* rC2 = c2ab c3ab */
                                addl    $NB6so, pA3
        haddpd          rC5, rC4        /* rC4 = c4ab c3ab */
#else
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
#endif
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
	movapd	rC0, (pC)
	movapd	rC2, 16(pC)
	movapd	rC4, 32(pC)
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
	jnz	MLOOP
#endif
/*
 *       last iteration of MLOOP unrolled for prefetch
 */
#if MB == 0
LMLOOP:
#endif
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
      #ifndef ATL_GAS_x8664
	movlpd	(%esp), rbeta
      #endif
	mulsd	rbeta, rC0
	mulsd	rbeta, rC1
	mulsd	rbeta, rC2
	mulsd	rbeta, rC3
	mulsd	rbeta, rC4
	mulsd	rbeta, rC5
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
						prefB((pB0,ldab))
						prefB(128(pB0,ldab))
	addpd	rA0, rC4
	mulpd	16-120(pA3,ldab,2), rB0
	addpd	rB0, rC5
#else
						prefB((pB0,ldab))
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
						prefB(256(pB0,ldab))
   #if KB > 48
						prefB(384(pB0,ldab))
   #endif
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
   #if KB > 56
						prefB(448(pB0,ldab))
   #endif
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
/*						pref2((pfA)) */
/*						addl	$48, pfA */
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

#if KB > 72
	movapd	576-120(pB0), rB0
	movapd	576-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	576-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	576-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	576-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	576-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	576-120(pA3,ldab,2), rB0
	addpd	rB0, rC5
#endif

#if KB > 74
	movapd	592-120(pB0), rB0
	movapd	592-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	592-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	592-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	592-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	592-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	592-120(pA3,ldab,2), rB0
	addpd	rB0, rC5
#endif

#if KB > 76
	movapd	608-120(pB0), rB0
	movapd	608-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	608-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	608-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	608-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	608-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	608-120(pA3,ldab,2), rB0
	addpd	rB0, rC5
#endif

#if KB > 78
	movapd	624-120(pB0), rB0
	movapd	624-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	624-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	624-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	624-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	624-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	624-120(pA3,ldab,2), rB0
	addpd	rB0, rC5
#endif
/*
 *      Get these bastard things summed up correctly
 */
#ifdef ATL_SSE3
        haddpd          rC1, rC0        /* rC0 = c0ab c1ab */
				addl	$NBso, pB0
        haddpd          rC3, rC2        /* rC2 = c2ab c3ab */
        #if MB == 0
                                subl    incAn_m, pA0
        #else
				subl	$MBKBso-NB6so, pA0
        #endif
        haddpd          rC5, rC4        /* rC4 = c4ab c3ab */
        #if MB == 0
                                movl    pA0, pA3
                                addl    $NB3so, pA3
        #else
				subl	$MBKBso-NB6so, pA3
        #endif
#else
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
        #if MB == 0
                                subl    incAn_m, pA0
        #else
				subl	$MBKBso-NB6so, pA0
        #endif
        unpcklpd        rC5, rC4        /* rC4 = c4a  c5a */
        unpckhpd        rC1, rA0        /* rA0 = c0b  c1b */
        unpckhpd        rC5, rB0        /* rB0 = c4b  c5b */
        addpd           rA0, rC0        /* rC0 = c0ab c1ab */
        addpd           rB0, rC4        /* rC4 = c4ab c5ab */
        movapd          rC2, rA0
        unpcklpd        rC3, rC2        /* rC2 = c2a  c3a */
        unpckhpd        rC3, rA0        /* rA0 = c2b  c3b */
        #if MB == 0
                                movl    pA0, pA3
                                addl    $NB3so, pA3
        #else
				subl	$MBKBso-NB6so, pA3
        #endif
        addpd           rA0, rC2        /* rC2 = c2ab c3ab */
#endif
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
	movapd	rC0, (pC)
	movapd	rC2, 16(pC)
	movapd	rC4, 32(pC)
   #endif
/*
 *      pC += 6;   pA += 6*NB
 */

/*
 *      pC += incCn;  pA -= NBNB;  pB += NB;
 */
/*	addl	incCn, pC */
   	addl	incCn_m, pC
/*
 *      while (pB != stN);
 */
	sub	$1, stN
	jnz	NLOOP

/*
 *      Restore callee-saved iregs
 */
   #ifdef ATL_GAS_x8664
        movq    -8(%rsp), %rbx
        movq    -16(%rsp), %rbp
   #elif defined(BETAX)
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
#endif
UNALIGNED:
#if MB == 0
        subl    M_m, incCn
	addl	$6, incCn
#else
	subl	$MB-6, incCn
#endif
   #ifdef DCPLX
	shl	$4, incCn
   #else
	shl	$3, incCn
   #endif
   #ifndef ATL_GAS_x8664
   	movl	incCn, COFF(%esp)
   #endif
	movl	$NBso, ldab
	movl	pA0, pA3
	addl	$NB3so, pA3
        movl    pA0, pfA
#if MB == 0
        addl    $NB6so-120, pfA
        addl    incAn_m, pfA
#else
	addl	$MBKBso-120, pfA
#endif
UNLOOP:
#if MB == 0
        movb    M_m, stM
        subb     $6, stM
        jz      ULMLOOP
#else
        movb    $MB-6, stM
#endif
#if MB != 6
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
      #ifndef ATL_GAS_x8664
	movlpd	(%esp), rbeta
      #endif
	mulsd	rbeta, rC0
	mulsd	rbeta, rC1
	mulsd	rbeta, rC2
	mulsd	rbeta, rC3
	mulsd	rbeta, rC4
	mulsd	rbeta, rC5
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
						pref2((pfA))
						addl	$PFAINC, pfA

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

#if KB > 72
	movapd	576-120(pB0), rB0
	movapd	576-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	576-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	576-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	576-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	576-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	576-120(pA3,ldab,2), rB0
	addpd	rB0, rC5
#endif

#if KB > 74
	movapd	592-120(pB0), rB0
	movapd	592-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	592-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	592-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	592-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	592-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	592-120(pA3,ldab,2), rB0
	addpd	rB0, rC5
#endif

#if KB > 76
	movapd	608-120(pB0), rB0
	movapd	608-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	608-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	608-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	608-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	608-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	608-120(pA3,ldab,2), rB0
	addpd	rB0, rC5
#endif

#if KB > 78
	movapd	624-120(pB0), rB0
	movapd	624-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	624-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	624-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	624-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	624-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	624-120(pA3,ldab,2), rB0
	addpd	rB0, rC5
#endif
/*
 *      Get these bastard things summed up correctly
 */
#ifdef ATL_SSE3
        haddpd          rC1, rC0        /* rC0 = c0ab c1ab */
                                addl    $NB6so, pA0
        haddpd          rC3, rC2        /* rC2 = c2ab c3ab */
                                addl    $NB6so, pA3
        haddpd          rC5, rC4        /* rC4 = c4ab c3ab */
#else
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
#endif
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
#endif
/*
 *       last iteration of UMLOOP unrolled for prefetch
 */
#if MB == 0
ULMLOOP:
#endif
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
      #ifndef ATL_GAS_x8664
	movlpd	(%esp), rA0
      #endif
	mulsd	rbeta, rC0
	mulsd	rbeta, rC1
	mulsd	rbeta, rC2
	mulsd	rbeta, rC3
	mulsd	rbeta, rC4
	mulsd	rbeta, rC5
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
						prefB((pB0,ldab))
						prefB(128(pB0,ldab))
	addpd	rA0, rC4
	mulpd	16-120(pA3,ldab,2), rB0
	addpd	rB0, rC5
#else
						prefB((pB0,ldab))
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
						prefB(256(pB0,ldab))
   #if KB > 48
						prefB(384(pB0,ldab))
   #endif
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
   #if KB > 56
						prefB(448(pB0,ldab))
   #endif
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
/*						pref2((pfA)) */
/*						addl	$48, pfA */
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

#if KB > 72
	movapd	576-120(pB0), rB0
	movapd	576-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	576-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	576-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	576-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	576-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	576-120(pA3,ldab,2), rB0
	addpd	rB0, rC5
#endif

#if KB > 74
	movapd	592-120(pB0), rB0
	movapd	592-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	592-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	592-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	592-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	592-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	592-120(pA3,ldab,2), rB0
	addpd	rB0, rC5
#endif

#if KB > 76
	movapd	608-120(pB0), rB0
	movapd	608-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	608-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	608-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	608-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	608-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	608-120(pA3,ldab,2), rB0
	addpd	rB0, rC5
#endif

#if KB > 78
	movapd	624-120(pB0), rB0
	movapd	624-120(pA0), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	624-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
	movapd	624-120(pA0,ldab,2), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC2
	movapd	624-120(pA3), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC3
	movapd	624-120(pA3,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC4
	mulpd	624-120(pA3,ldab,2), rB0
	addpd	rB0, rC5
#endif
/*
 *      Get these bastard things summed up correctly
 */
#ifdef ATL_SSE3
				addl	$NBso, pB0
        haddpd          rC1, rC0        /* rC0 = c0ab c1ab */
        #if MB == 0
                                subl    incAn_m, pA0
        #else
				subl	$MBKBso-NB6so, pA0
        #endif
        haddpd          rC3, rC2        /* rC2 = c2ab c3ab */
        #if MB == 0
                                movl    pA0, pA3
                                addl    $NB3so, pA3
        #else
				subl	$MBKBso-NB6so, pA3
        #endif
        haddpd          rC5, rC4        /* rC4 = c4ab c3ab */
#else
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
        #if MB == 0
                                subl    incAn_m, pA0
        #else
				subl	$MBKBso-NB6so, pA0
        #endif
        unpcklpd        rC5, rC4        /* rC4 = c4a  c5a */
        unpckhpd        rC1, rA0        /* rA0 = c0b  c1b */
        unpckhpd        rC5, rB0        /* rB0 = c4b  c5b */
        addpd           rA0, rC0        /* rC0 = c0ab c1ab */
        addpd           rB0, rC4        /* rC4 = c4ab c5ab */
        movapd          rC2, rA0
        unpcklpd        rC3, rC2        /* rC2 = c2a  c3a */
        unpckhpd        rC3, rA0        /* rA0 = c2b  c3b */
        #if MB == 0
                                movl    pA0, pA3
                                addl    $NB3so, pA3
        #else
				subl	$MBKBso-NB6so, pA3
        #endif
        addpd           rA0, rC2        /* rC2 = c2ab c3ab */
#endif
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

/*
 *      pC += incCn;  pA -= NBNB;  pB += NB;
 */
/*	addl	incCn, pC */
   	addl	incCn_m, pC
/*
 *      while (pB != stN);
 */
	sub	$1, stN
	jnz	UNLOOP

/*
 *      Restore callee-saved iregs
 */
   #ifdef ATL_GAS_x8664
        movq    -8(%rsp), %rbx
        movq    -16(%rsp), %rbp
   #elif defined(BETAX)
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
