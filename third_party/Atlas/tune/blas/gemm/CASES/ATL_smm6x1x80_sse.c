/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2004 R. Clint Whaley
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
   #error "This kernel requires x86 gas 32 or 64 bit assembler!"
#endif
#ifndef ATL_SSE1
   #error "This routine requires SSE1!"
#endif

#ifdef SCPLX
   #define CMUL(i_) ((i_)+(i_))
#else
   #define CMUL(i_) i_
#endif

#if !defined(KB) || (KB == 0)
   #error "KB must be a compile-time constant!"
#endif
#if KB > 80
   #error "Max KB is 80!"
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

#if MB <= 6 || defined(ATL_OS_SunOS)  /* retarded gcc on SunOS has no divis */
   #define PFAINC 64
#else
   #define PFAINC ((MB*4+MB/6-2)/(MB/6-1))
#endif
#if PFAINC < -6400
   #undef PFAINC
   #define PFAINC 64
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
   #define stM     %rdi
   #define stN     %rsi
   #define ldab    %r8
   #define pA3     %rdx
   #define pfA     %r11
   #define M_m     %rbx
   #define incAn_m %rbp
   #define incCn_m incCn
#else
   #define pC      %esi
   #define pA      %ecx
   #define pB      %edi
   #define incCn   %eax
   #define stM	%bl
   #define stN	%bh
   #define ldab	%edx
   #define pA3	%ebp
   #ifdef BETAX
      #define COFF 36
   #else
      #define COFF 16
   #endif
   #define M_m     COFF(%esp)
   #define incAn_m COFF+4(%esp)
   #define incCn_m COFF+8(%esp)
   #define pfA	incCn
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

#define NBso	(KB*4)
#if MB != 0
   #define MBKBso  (MB*KB*4)
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
#define prefB(mem) prefetchnta	mem
#define prefC(mem) prefetcht0	mem
#else
#define pref2(mem)
#define prefB(mem)
#define prefC(mem)
#endif
/*offset                rdi/4        rsi/8       rdx/12           xmm0/16
 *void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha,
 *offset                rcx/ 20          r8/24          r9/28         8/ 32
 *                const TYPE *A, const int lda, const TYPE *B, const int ldb,
 *offset                  xmm1/36    16/40          24/44
 *                const TYPE beta, TYPE *C, const int ldc)
 */
	.text
.global ATL_asmdecor(ATL_USERMM)
ATL_asmdecor(ATL_USERMM):
#ifdef ATL_GAS_x8664
        movq    %rbx, -8(%rsp)
        movq    %rbp, -16(%rsp)
   #ifdef BETAX
        movapd  %xmm1, rbeta
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
	movss	36(%eax), rC0
	movaps	rC0, (%esp)
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
        movl    %ebx, M_m
        imul    $NBso, %ebx
        subl    $NB6so, %ebx
        movl    %ebx, incAn_m
   #endif
	movl	20(%eax), pA
	movl	28(%eax), pB
	movl	40(%eax), pC
   #if NB == 0
        movb    8(%eax), stN
   #else
        movb    $NB, stN
   #endif
	movl	44(%eax), incCn
#endif
	addl	$120, pA
	addl	$120, pB
/*
 *      Set incCn = (ldc - NB)*sizeof
 */
#if MB == 0
        subl    M_m, incCn
	addl	$6, incCn
#else
	subl	$MB-6, incCn
#endif
   #ifdef SCPLX
	shl	$3, incCn
   #else
	shl	$2, incCn
   #endif
   #ifndef ATL_GAS_x8664
   	movl	incCn, incCn_m
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
        ALIGN16
UMLOOP:
#ifdef BETA0
	xorps	rC0, rC0
	xorps	rC1, rC1
	xorps	rC2, rC2
	xorps	rC3, rC3
	xorps	rC4, rC4
	xorps	rC5, rC5
#else
	movss	(pC), rC0
	movss	CMUL(4)(pC), rC1
	movss	CMUL(8)(pC), rC2
	movss	CMUL(12)(pC), rC3
	movss	CMUL(16)(pC), rC4
	movss	CMUL(20)(pC), rC5
   #ifdef BETAX
      #ifndef ATL_GAS_x8664
	movss	(%esp), rbeta
      #endif
	mulss	rbeta, rC0
	mulss	rbeta, rC1
	mulss	rbeta, rC2
	mulss	rbeta, rC3
	mulss	rbeta, rC4
	mulss	rbeta, rC5
   #endif
#endif
/*
 *      Completely unrolled K-loop
 */
        ALIGN16
	movaps	0-120(pB0), rB0
	movaps	0-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	0-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	0-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	0-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	0-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	0-120(pA3,ldab,2), rB0
	addps	rB0, rC5

#if KB > 4
	movaps	16-120(pB0), rB0
	movaps	16-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	16-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	16-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	16-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	16-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	16-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 8
	movaps	32-120(pB0), rB0
	movaps	32-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	32-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	32-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	32-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	32-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	32-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 12
	movaps	48-120(pB0), rB0
	movaps	48-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	48-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	48-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	48-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	48-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	48-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 16
	movaps	64-120(pB0), rB0
	movaps	64-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	64-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	64-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	64-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	64-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	64-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 20
	movaps	80-120(pB0), rB0
	movaps	80-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	80-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	80-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	80-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	80-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	80-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 24
	movaps	96-120(pB0), rB0
	movaps	96-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	96-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	96-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	96-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	96-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	96-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 28
	movaps	112-120(pB0), rB0
	movaps	112-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	112-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	112-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	112-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	112-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	112-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 32
	movaps	128-120(pB0), rB0
	movaps	128-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	128-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	128-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	128-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	128-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	128-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 36
	movaps	144-120(pB0), rB0
	movaps	144-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	144-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	144-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	144-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	144-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	144-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 40
	movaps	160-120(pB0), rB0
	movaps	160-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	160-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	160-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	160-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	160-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	160-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 44
	movaps	176-120(pB0), rB0
	movaps	176-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	176-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	176-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	176-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	176-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	176-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 48
	movaps	192-120(pB0), rB0
	movaps	192-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	192-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	192-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	192-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	192-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	192-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 52
	movaps	208-120(pB0), rB0
	movaps	208-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	208-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	208-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	208-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	208-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	208-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif
                        pref2((pfA))
                        addl    $PFAINC, pfA

#if KB > 56
	movaps	224-120(pB0), rB0
	movaps	224-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	224-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	224-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	224-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	224-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	224-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 60
	movaps	240-120(pB0), rB0
	movaps	240-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	240-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	240-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	240-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	240-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	240-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 64
	movaps	256-120(pB0), rB0
	movaps	256-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	256-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	256-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	256-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	256-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	256-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 68
	movaps	272-120(pB0), rB0
	movaps	272-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	272-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	272-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	272-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	272-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	272-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 72
	movaps	288-120(pB0), rB0
	movaps	288-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	288-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	288-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	288-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	288-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	288-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 76
	movaps	304-120(pB0), rB0
	movaps	304-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	304-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	304-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	304-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	304-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	304-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

/*
 *      Get these bastard things summed up correctly
 *      Note this initial quad summation is Camm's, as his sequence was faster
 *      than the piece of crap I came up with
 */
                                        /* rC0 = c0a  c0b */
                                        /* rC1 = c1a  c1b */
                                        /* rC2 = c2a  c2b */
                                        /* rC3 = c3a  c3b */
/* */
                                        /* rC4 = c4a  c4b */
                                        /* rC5 = c5a  c5b */
        movaps          rC0, rA0        /* rA0 = c0d    c0c    c0b    c0a      */
        unpcklps        rC1, rC0        /* rC0 = c1b    c0b    c1a    c0d */
        movaps          rC2, rB0        /* rB0 = c2d    c2c    c2b    c2a */
        unpckhps        rC1, rA0        /* rA0 = c1d    c0d    c1c    c0c */
        unpcklps        rC3, rC2        /* rC2 = c3b    c2b    c3a    c2a */
        addps           rA0, rC0        /* rC0 = c1bd   c0bd   c1ac   c0ac */
        unpckhps        rC3, rB0        /* rB0 = c3d    c2d    c3c    c2c */
        movaps          rC0, rA0        /* rA0 = c1bd   c0bd   c1ac   c0ac */
        addps           rB0, rC2        /* rC2 = c3bd   c2bd   c3ac   c2ac */
        shufps          $0x44,rC2,rC0   /* rC0 = c3ac   c2ac   c1ac   c0ac */
        shufps          $0xEE,rC2,rA0   /* rA0 = c3bd   c2bd   c1bd   c0bd */
        addps           rA0, rC0        /* rC0 = c3abcd c2abcd c1abcd c0abcd */
                                        /* rC4 = c4d    c4c    c4b    c4a */
                                        /* rC5 = c5d    c5c    c5b    c5a */
        movaps          rC4, rB0        /* rB0 = c4d    c4c    c4b    c4a */
        unpcklps        rC5, rC4        /* rC4 = c5b    c4b    c5a    c4a */
        unpckhps        rC5, rB0        /* rB0 = c5d    c4d    c5c    c4c */
        addps           rB0, rC4        /* rC4 = c5bd   c4bd   c5ac   c4ac */
        movhlps         rC4, rA0        /* rA0 = X      X      c5bd   c4bd */
        addps           rA0, rC4        /* rC4 = X      X      c5abcd c4abcd */
   #ifdef SCPLX
                                        /* rC0 = c3 c2 c1 c0 */
                                        /* rC4 =  X  X c5 c4 */
        pshufd  $0xB1, rC0, rC1         /* rC1 = c2 c3 c0 c1 */
        movhlps rC0, rC2                /* rC2 =  X  X c3 c2 */
        movhlps rC1, rC3                /* rC3 =  X  X c2 c3 */
        pshufd  $0xC1, rC4, rC5         /* rC5 =  X  X c4 c5 */
        movss   rC0, (pC)
        movss   rC1, 8(pC)
        movss   rC2, 16(pC)
        movss   rC3, 24(pC)
        movss   rC4, 32(pC)
        movss   rC5, 40(pC)
   #else
	movups		rC0, (pC)
        movlps          rC4, 16(pC)
   #endif
                                addl    $NB6so, pA0
                                addl    $NB6so, pA3
/*
 *      Write results back to C
 */

/*
 *      pC += 6;   pA += 6*NB
 */
	addl	$CMUL(24), pC
/*
 *      while (pA != stM);
 */
	subb	$6, stM
	jnz	UMLOOP
#endif

/*
 *      Last iteration of MLOOP unrolled to prefetch C & B
 */
#if MB == 0
ULMLOOP:
#endif
#ifdef BETA0
	xorps	rC0, rC0
	xorps	rC1, rC1
	xorps	rC2, rC2
	xorps	rC3, rC3
	xorps	rC4, rC4
	xorps	rC5, rC5
#else
	movss	(pC), rC0
	movss	CMUL(4)(pC), rC1
	movss	CMUL(8)(pC), rC2
	movss	CMUL(12)(pC), rC3
	movss	CMUL(16)(pC), rC4
	movss	CMUL(20)(pC), rC5
   #ifdef BETAX
      #ifndef ATL_GAS_x8664
	movss	(%esp), rbeta
      #endif
	mulss	rbeta, rC0
	mulss	rbeta, rC1
	mulss	rbeta, rC2
	mulss	rbeta, rC3
	mulss	rbeta, rC4
	mulss	rbeta, rC5
   #endif
#endif
/*
 *      Completely unrolled K-loop
 */
	movaps	0-120(pB0), rB0
	movaps	0-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	0-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	0-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	0-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	0-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	0-120(pA3,ldab,2), rB0
	addps	rB0, rC5

#if KB > 4
	movaps	16-120(pB0), rB0
	movaps	16-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	16-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	16-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	16-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	16-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	16-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif
                                        prefB((pB0,ldab))
                                        prefB(128(pB0,ldab))
                                   #if KB > 64
                                        prefB(256(pB0,ldab))
                                   #endif

#if KB > 8
	movaps	32-120(pB0), rB0
	movaps	32-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	32-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	32-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	32-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	32-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	32-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 12
	movaps	48-120(pB0), rB0
	movaps	48-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	48-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	48-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	48-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	48-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	48-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 16
	movaps	64-120(pB0), rB0
	movaps	64-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	64-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	64-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	64-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	64-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	64-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 20
	movaps	80-120(pB0), rB0
	movaps	80-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	80-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	80-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	80-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	80-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	80-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 24
	movaps	96-120(pB0), rB0
	movaps	96-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	96-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	96-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	96-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	96-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	96-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 28
	movaps	112-120(pB0), rB0
	movaps	112-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	112-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	112-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	112-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	112-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	112-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 32
	movaps	128-120(pB0), rB0
	movaps	128-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	128-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	128-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	128-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	128-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	128-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 36
	movaps	144-120(pB0), rB0
	movaps	144-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	144-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	144-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	144-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	144-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	144-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 40
	movaps	160-120(pB0), rB0
	movaps	160-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	160-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	160-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	160-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	160-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	160-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 44
	movaps	176-120(pB0), rB0
	movaps	176-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	176-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	176-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	176-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	176-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	176-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 48
	movaps	192-120(pB0), rB0
	movaps	192-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	192-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	192-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	192-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	192-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	192-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 52
	movaps	208-120(pB0), rB0
	movaps	208-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	208-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	208-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	208-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	208-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	208-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif
/*                        pref2((pfA)) */
/*                        addl    $PFAINC, pfA */

#if KB > 56
	movaps	224-120(pB0), rB0
	movaps	224-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	224-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	224-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	224-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	224-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	224-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 60
	movaps	240-120(pB0), rB0
	movaps	240-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	240-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	240-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	240-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	240-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	240-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 64
	movaps	256-120(pB0), rB0
	movaps	256-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	256-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	256-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	256-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	256-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	256-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 68
	movaps	272-120(pB0), rB0
	movaps	272-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	272-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	272-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	272-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	272-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	272-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 72
	movaps	288-120(pB0), rB0
	movaps	288-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	288-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	288-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	288-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	288-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	288-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

#if KB > 76
	movaps	304-120(pB0), rB0
	movaps	304-120(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	304-120(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	304-120(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	movaps	304-120(pA3), rA0
	mulps	rB0, rA0
	addps	rA0, rC3
	movaps	304-120(pA3,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC4
	mulps	304-120(pA3,ldab,2), rB0
	addps	rB0, rC5
#endif

/*
 *      Get these bastard things summed up correctly
 *      Note this initial quad summation is Camm's, as his sequence was faster
 *      than the piece of crap I came up with
 */
                                        /* rC0 = c0a  c0b */
                                        /* rC1 = c1a  c1b */
                                        /* rC2 = c2a  c2b */
                                        /* rC3 = c3a  c3b */
/* */
                                        /* rC4 = c4a  c4b */
                                        /* rC5 = c5a  c5b */
        movaps          rC0, rA0        /* rA0 = c0d    c0c    c0b    c0a      */
        unpcklps        rC1, rC0        /* rC0 = c1b    c0b    c1a    c0a */
        movaps          rC2, rB0        /* rB0 = c2d    c2c    c2b    c2a */
        unpckhps        rC1, rA0        /* rA0 = c1d    c0d    c1c    c0c */
        unpcklps        rC3, rC2        /* rC2 = c3b    c2b    c3a    c2a */
        addps           rA0, rC0        /* rC0 = c1bd   c0bd   c1ac   c0ac */
        unpckhps        rC3, rB0        /* rB0 = c3d    c2d    c3c    c2c */
        movaps          rC0, rA0        /* rA0 = c1bd   c0bd   c1ac   c0ac */
        addps           rB0, rC2        /* rC2 = c3bd   c2bd   c3ac   c2ac */
        shufps          $0x44,rC2,rC0   /* rC0 = c3ac   c2ac   c1ac   c0ac */
        shufps          $0xEE,rC2,rA0   /* rA0 = c3bd   c2bd   c1bd   c0bd */
        addps           rA0, rC0        /* rC0 = c3abcd c2abcd c1abcd c0abcd */
                                        /* rC4 = c4d    c4c    c4b    c4a */
                                        /* rC5 = c5d    c5c    c5b    c5a */
        movaps          rC4, rB0        /* rB0 = c4d    c4c    c4b    c4a */
        unpcklps        rC5, rC4        /* rC4 = c5b    c4b    c5a    c4a */
        unpckhps        rC5, rB0        /* rB0 = c5d    c4d    c5c    c4c */
        addps           rB0, rC4        /* rC4 = c5bd   c4bd   c5ac   c4ac */
        movhlps         rC4, rA0        /* rA0 = X      X      c5bd   c4bd */
        addps           rA0, rC4        /* rC4 = X      X      c5abcd c4abcd */

   #ifdef SCPLX
                                        /* rC0 = c3 c2 c1 c0 */
                                        /* rC4 =  X  X c5 c4 */
        pshufd  $0xB1, rC0, rC1         /* rC1 = c2 c3 c0 c1 */
        movhlps rC0, rC2                /* rC2 =  X  X c3 c2 */
        movhlps rC1, rC3                /* rC3 =  X  X c2 c3 */
        pshufd  $0xC1, rC4, rC5         /* rC5 =  X  X c4 c5 */
        movss   rC0, (pC)
        movss   rC1, 8(pC)
        movss   rC2, 16(pC)
        movss   rC3, 24(pC)
        movss   rC4, 32(pC)
        movss   rC5, 40(pC)
   #else
	movups		rC0, (pC)
        movlps          rC4, 16(pC)
   #endif
				addl	$NBso, pB0
        #if MB == 0
                                subl    incAn_m, pA0
        #else
				subl	$MBKBso-NB6so, pA0
        #endif
        #if MB == 0
                                movl    pA0, pA3
                                addl    $NB3so, pA3
        #else
				subl	$MBKBso-NB6so, pA3
        #endif
/*
 *      Write results back to C
 */

/*
 *      while (pA != stM);
 */
/*	subb	$6, stM */
/*	jnz	UMLOOP */
/*
 *      pC += incCn;   pA += -MBKB; pB0 += NB
 */
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
