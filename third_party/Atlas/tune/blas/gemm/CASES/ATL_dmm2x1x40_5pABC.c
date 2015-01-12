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


#ifdef ATL_GAS_x8632
   #define movq movl
   #define addq addl
   #define subq subl
   #define rsp  esp
#elif defined(ATL_GAS_x8664)
   #error "This kernel not debugged under x8664 yet"
#elif !defined(ATL_GAS_x8664)
   #error "This kernel requires a gas x86 assembler!"
#endif
#if !defined(KB) || (KB == 0)
   #error "KB must be a compile-time constant!"
#endif
#if (KB != 40)
   #error "KB must be 40!"
#endif
#if (MB/2)*2 != MB
   #error "MB must be multiple of 2!"
#endif
#if defined(NB) && NB == 0
   #undef NB
#endif
#if defined(MB) && MB == 0
   #undef MB
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
   #define pC      %rbx
   #define pA      %rcx
   #define pB      %rdx
   #define incCn   %rax
   #define stM     %rdi
   #define stN     %rsi
   #define pfA	   %rbp
   #define pA0     %r11
   /*       rax     used in 32/64 conversion */
#endif
#ifdef DCPLX
   #define incCm 32
   #define OFF   16
#else
   #define incCm 16
   #define OFF    8
#endif

#define NBso	(KB*8)
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
#ifdef ATL_SSE1
   #define pref2(mem) prefetcht1	mem
   #define prefA(mem) prefetcht0	mem
   #define prefB(mem) prefetcht0	mem
   #define prefC(mem) prefetcht0	mem
#else
   #define pref2(mem)
   #define prefA(mem)
   #define prefB(mem)
   #define prefC(mem)
#endif
/*                     %rdi/4       %rsi/8       %rdx/12          %xmm0/16
 *void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha,
 *                      %rcx/24         %r8/28         %r9/32           8/36
 *                const TYPE *A, const int lda, const TYPE *B, const int ldb,
 *                       %xmm1/40    16/48          24/52
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
 *      Initialize stM = (M-4)*KB, stN = N*KB
 *      Set incCn = (ldc - M)*sizeof
 */
        movl    28(%esp), stM
	movl	76(%esp), incCn
	subl	stM, incCn
        addl    $2, incCn
   #ifdef DCPLX
	shl	$4, incCn
   #else
	shl	$3, incCn
   #endif
        subl    $4, stM
        imull   $NBso, stM
        movl    32(%esp), stN
        imull   $NBso, stN
/*
 *      Initialize pA = A;  pB = B; pC = C;
 */
	movl	48(%esp), pA
	movl	56(%esp), pB
	movl	72(%esp), pC
/*	prefC((pC)) */
/*	prefC(64(pC)) */
/*
 *      stM = pA + NBNB-4*NB;  pfA = pA+NBNB;  stN = pB + NBNB;
 */
	addl	pA, stM
	movl	stM, pfA
        addl    $NB4so, pfA
	addl	pB, stN
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
 *	Initialize pB = B; pC = C;
 */
        movq    pA, pA0
	movq	%r9, pB
	movq	16(%rsp), pC
/*	prefC((pC)) */
/*	prefC(64(pC)) */
/*
 *      stM = pA + NBNBso;  stN = pB + NBNBso;
 */
#ifndef MB
        imul    $KB*8, stM
        addq    $-4*KB*8, stM
#else
	movq	$MB*KB*8-4*KB*8, stM
#endif
	addq	pA, stM
	movq	stM, pfA
	addq	$4*KB*8, pfA
#ifndef NB
        imul    $KB*8, stN
#else
	movq	$NB*KB*8, stN
#endif
	addq	pB, stN
/*
 *      convert ldc to 64 bits, and then set incCn = (ldc - NB)*sizeof
 */
	movl	24(%rsp), %eax
	cltq
/*	prefB((pB)) */
/*	prefB(64(pB)) */
/*	movq	%rax, incCn */
/*	subq	$NB, incCn */
	shl	$3, incCn
	addq	$16, incCn
#endif
/*
 *      Unroll the 1st iterations of N-loop so we can prefetch A
 */
   #ifndef MB
        cmp     stM, pA
        je      MLOOP_DRAIN_UN
   #endif
#if !defined(MB) || MB > 4
MLOOP_UN:
	fldl (pB)
	fldl (pA)
	fmul %st(1),%st
	fldl 320(pA)
	fmulp %st,%st(2)
	fldl 8(pB)
	fldl 8(pA)
	fmul %st(1),%st
	fldl 328(pA)
	fmulp %st,%st(2)
	fldl 16(pB)
	fldl 16(pA)
	fmul %st(1),%st
   #if defined(BETA0) || defined (BETAX)
        prefC((pC))
	fldz
   #else
	fldl (pC)
   #endif
	faddp %st,%st(5)
	fldl 336(pA)
	fmulp %st,%st(2)
	fldl 24(pB)
   #if defined(BETA0) || defined (BETAX)
	fldz
   #else
	fldl OFF(pC)
   #endif
	faddp %st,%st(7)
	fldl 24(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 344(pA)
	fmulp %st,%st(1)
	fldl 32(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 32(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 352(pA)
	fmulp %st,%st(7)
	fldl 40(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 40(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 360(pA)
	fmulp %st,%st(5)
	fldl 48(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 48(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 368(pA)
	fmulp %st,%st(3)
	fldl 56(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 56(pA)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 376(pA)
	fmulp %st,%st(1)
	fldl 64(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 64(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 384(pA)
	fmulp %st,%st(7)
	fldl 72(pB)
	fxch %st(5)
                prefA(NB2so(pA))
                prefA(NB3so(pA))
	faddp %st,%st(3)
	fldl 72(pA)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 392(pA)
	fmulp %st,%st(5)
	fldl 80(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 80(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 400(pA)
	fmulp %st,%st(3)
	fldl 88(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 88(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 408(pA)
	fmulp %st,%st(1)
	fldl 96(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 96(pA)
	fmul %st(7),%st
	fxch %st(6)
                prefA(32+NB2so(pA))
                prefA(32+NB3so(pA))
	faddp %st,%st(4)
	fldl 416(pA)
	fmulp %st,%st(7)
	fldl 104(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 104(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 424(pA)
	fmulp %st,%st(5)
	fldl 112(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 112(pA)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 432(pA)
	fmulp %st,%st(3)
	fldl 120(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 120(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 440(pA)
	fmulp %st,%st(1)
	fldl 128(pB)
	fxch %st(7)
                prefA(64+NB2so(pA))
                prefA(64+NB3so(pA))
	faddp %st,%st(5)
	fldl 128(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 448(pA)
	fmulp %st,%st(7)
	fldl 136(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 136(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 456(pA)
	fmulp %st,%st(5)
	fldl 144(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 144(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 464(pA)
	fmulp %st,%st(3)
	fldl 152(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 152(pA)
	fmul %st(1),%st
	fxch %st(4)
                prefA(96+NB2so(pA))
                prefA(96+NB3so(pA))
	faddp %st,%st(2)
	fldl 472(pA)
	fmulp %st,%st(1)
	fldl 160(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 160(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 480(pA)
	fmulp %st,%st(7)
	fldl 168(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 168(pA)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 488(pA)
	fmulp %st,%st(5)
	fldl 176(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 176(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 496(pA)
	fmulp %st,%st(3)
	fldl 184(pB)
	fxch %st(1)
                prefA(128+NB2so(pA))
                prefA(128+NB3so(pA))
	faddp %st,%st(7)
	fldl 184(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 504(pA)
	fmulp %st,%st(1)
	fldl 192(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 192(pA)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 512(pA)
	fmulp %st,%st(7)
	fldl 200(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 200(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 520(pA)
	fmulp %st,%st(5)
	fldl 208(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 208(pA)
	fmul %st(3),%st
	fxch %st(2)
                prefA(160+NB2so(pA))
                prefA(160+NB3so(pA))
	faddp %st,%st(6)
	fldl 528(pA)
	fmulp %st,%st(3)
	fldl 216(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 216(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 536(pA)
	fmulp %st,%st(1)
	fldl 224(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 224(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 544(pA)
	fmulp %st,%st(7)
	fldl 232(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 232(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 552(pA)
	fmulp %st,%st(5)
	fldl 240(pB)
	fxch %st(3)
                prefA(192+NB2so(pA))
                prefA(192+NB3so(pA))
	faddp %st,%st(1)
	fldl 240(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 560(pA)
	fmulp %st,%st(3)
	fldl 248(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 248(pA)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 568(pA)
	fmulp %st,%st(1)
	fldl 256(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 256(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 576(pA)
	fmulp %st,%st(7)
	fldl 264(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 264(pA)
	fmul %st(5),%st
	fxch %st(6)
                prefA(224+NB2so(pA))
                prefA(224+NB3so(pA))
	faddp %st,%st(4)
	fldl 584(pA)
	fmulp %st,%st(5)
	fldl 272(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 272(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 592(pA)
	fmulp %st,%st(3)
	fldl 280(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 280(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 600(pA)
	fmulp %st,%st(1)
	fldl 288(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 288(pA)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 608(pA)
	fmulp %st,%st(7)
	fldl 296(pB)
	fxch %st(5)
                prefA(256+NB2so(pA))
                prefA(256+NB3so(pA))
	faddp %st,%st(3)
	fldl 296(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 616(pA)
	fmulp %st,%st(5)
	fldl 304(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 304(pA)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 624(pA)
	fmulp %st,%st(3)
	fldl 312(pB)
	fxch %st(1)
                        prefC(32(pC))
	faddp %st,%st(7)
	fldl 312(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 632(pA)
	fmulp %st,%st(1)
	fxch %st(6)
	faddp %st,%st(4)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
                prefA(288+NB2so(pA))
                prefA(288+NB3so(pA))
	faddp %st,%st(2)
/*
 *      While (pB != stK);
 */
/*	cmp	pB, stK */
/*	jne	KLOOP */
/*
 *      Write results back to C
 */
   #ifdef BETAX
        fldl    (pC)
        fldl    OFF(pC)
        fldl    BETAOFF(%rsp)
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
	addq	$incCm, pC
	addq	$NB2so, pA
	fldl (pB)
	fldl (pA)
	fmul %st(1),%st
	fldl 320(pA)
	fmulp %st,%st(2)
	fldl 8(pB)
	fldl 8(pA)
	fmul %st(1),%st
	fldl 328(pA)
	fmulp %st,%st(2)
	fldl 16(pB)
	fldl 16(pA)
	fmul %st(1),%st
   #if defined(BETA0) || defined (BETAX)
	fldz
   #else
	fldl (pC)
   #endif
	faddp %st,%st(5)
	fldl 336(pA)
	fmulp %st,%st(2)
	fldl 24(pB)
   #if defined(BETA0) || defined (BETAX)
	fldz
   #else
	fldl OFF(pC)
   #endif
	faddp %st,%st(7)
	fldl 24(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 344(pA)
	fmulp %st,%st(1)
	fldl 32(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 32(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 352(pA)
	fmulp %st,%st(7)
	fldl 40(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 40(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 360(pA)
	fmulp %st,%st(5)
	fldl 48(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 48(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 368(pA)
	fmulp %st,%st(3)
	fldl 56(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 56(pA)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 376(pA)
	fmulp %st,%st(1)
	fldl 64(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 64(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 384(pA)
	fmulp %st,%st(7)
	fldl 72(pB)
	fxch %st(5)
                prefA(NB2so(pA))
                prefA(NB3so(pA))
	faddp %st,%st(3)
	fldl 72(pA)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 392(pA)
	fmulp %st,%st(5)
	fldl 80(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 80(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 400(pA)
	fmulp %st,%st(3)
	fldl 88(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 88(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 408(pA)
	fmulp %st,%st(1)
	fldl 96(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 96(pA)
	fmul %st(7),%st
	fxch %st(6)
                prefA(32+NB2so(pA))
                prefA(32+NB3so(pA))
	faddp %st,%st(4)
	fldl 416(pA)
	fmulp %st,%st(7)
	fldl 104(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 104(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 424(pA)
	fmulp %st,%st(5)
	fldl 112(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 112(pA)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 432(pA)
	fmulp %st,%st(3)
	fldl 120(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 120(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 440(pA)
	fmulp %st,%st(1)
	fldl 128(pB)
	fxch %st(7)
                prefA(64+NB2so(pA))
                prefA(64+NB3so(pA))
	faddp %st,%st(5)
	fldl 128(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 448(pA)
	fmulp %st,%st(7)
	fldl 136(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 136(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 456(pA)
	fmulp %st,%st(5)
	fldl 144(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 144(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 464(pA)
	fmulp %st,%st(3)
	fldl 152(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 152(pA)
	fmul %st(1),%st
	fxch %st(4)
                prefA(96+NB2so(pA))
                prefA(96+NB3so(pA))
	faddp %st,%st(2)
	fldl 472(pA)
	fmulp %st,%st(1)
	fldl 160(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 160(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 480(pA)
	fmulp %st,%st(7)
	fldl 168(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 168(pA)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 488(pA)
	fmulp %st,%st(5)
	fldl 176(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 176(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 496(pA)
	fmulp %st,%st(3)
	fldl 184(pB)
	fxch %st(1)
                prefA(128+NB2so(pA))
                prefA(128+NB3so(pA))
	faddp %st,%st(7)
	fldl 184(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 504(pA)
	fmulp %st,%st(1)
	fldl 192(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 192(pA)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 512(pA)
	fmulp %st,%st(7)
	fldl 200(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 200(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 520(pA)
	fmulp %st,%st(5)
	fldl 208(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 208(pA)
	fmul %st(3),%st
	fxch %st(2)
                prefA(160+NB2so(pA))
                prefA(160+NB3so(pA))
	faddp %st,%st(6)
	fldl 528(pA)
	fmulp %st,%st(3)
	fldl 216(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 216(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 536(pA)
	fmulp %st,%st(1)
	fldl 224(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 224(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 544(pA)
	fmulp %st,%st(7)
	fldl 232(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 232(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 552(pA)
	fmulp %st,%st(5)
	fldl 240(pB)
	fxch %st(3)
                prefA(192+NB2so(pA))
                prefA(192+NB3so(pA))
	faddp %st,%st(1)
	fldl 240(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 560(pA)
	fmulp %st,%st(3)
	fldl 248(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 248(pA)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 568(pA)
	fmulp %st,%st(1)
	fldl 256(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 256(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 576(pA)
	fmulp %st,%st(7)
	fldl 264(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 264(pA)
	fmul %st(5),%st
	fxch %st(6)
                prefA(224+NB2so(pA))
                prefA(224+NB3so(pA))
	faddp %st,%st(4)
	fldl 584(pA)
	fmulp %st,%st(5)
	fldl 272(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 272(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 592(pA)
	fmulp %st,%st(3)
	fldl 280(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 280(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 600(pA)
	fmulp %st,%st(1)
	fldl 288(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 288(pA)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 608(pA)
	fmulp %st,%st(7)
	fldl 296(pB)
	fxch %st(5)
                prefA(256+NB2so(pA))
                prefA(256+NB3so(pA))
	faddp %st,%st(3)
	fldl 296(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 616(pA)
	fmulp %st,%st(5)
	fldl 304(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 304(pA)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 624(pA)
	fmulp %st,%st(3)
	fldl 312(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 312(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 632(pA)
	fmulp %st,%st(1)
	fxch %st(6)
	faddp %st,%st(4)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
                prefA(288+NB2so(pA))
                prefA(288+NB3so(pA))
	faddp %st,%st(2)
/*
 *      While (pB != stK);
 */
/*	cmp	pB, stK */
/*	jne	KLOOP */
/*
 *      Write results back to C
 */
   #ifdef BETAX
        fldl    (pC)
        fldl    OFF(pC)
        fldl    BETAOFF(%rsp)
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
	addq	$incCm, pC
	addq	$NB2so, pA
/*
 *      while (pA != stM);
 */
	cmp	pA, stM
	jne	MLOOP_UN
#endif

MLOOP_DRAIN_UN:
	fldl (pB)
	fldl (pA)
	fmul %st(1),%st
	fldl 320(pA)
	fmulp %st,%st(2)
	fldl 8(pB)
	fldl 8(pA)
	fmul %st(1),%st
	fldl 328(pA)
	fmulp %st,%st(2)
	fldl 16(pB)
	fldl 16(pA)
	fmul %st(1),%st
   #if defined(BETA0) || defined (BETAX)
	fldz
   #else
	fldl (pC)
   #endif
	faddp %st,%st(5)
	fldl 336(pA)
	fmulp %st,%st(2)
	fldl 24(pB)
   #if defined(BETA0) || defined (BETAX)
	fldz
   #else
	fldl OFF(pC)
   #endif
	faddp %st,%st(7)
	fldl 24(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 344(pA)
	fmulp %st,%st(1)
	fldl 32(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 32(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 352(pA)
	fmulp %st,%st(7)
	fldl 40(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 40(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 360(pA)
	fmulp %st,%st(5)
	fldl 48(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 48(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 368(pA)
	fmulp %st,%st(3)
	fldl 56(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 56(pA)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 376(pA)
	fmulp %st,%st(1)
	fldl 64(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 64(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 384(pA)
	fmulp %st,%st(7)
	fldl 72(pB)
	fxch %st(5)
                                prefB(NBso(pB))
                                prefB(32+NBso(pB))
	faddp %st,%st(3)
	fldl 72(pA)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 392(pA)
	fmulp %st,%st(5)
	fldl 80(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 80(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 400(pA)
	fmulp %st,%st(3)
	fldl 88(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 88(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 408(pA)
	fmulp %st,%st(1)
	fldl 96(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 96(pA)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 416(pA)
	fmulp %st,%st(7)
	fldl 104(pB)
	fxch %st(5)
                                prefB(64+NBso(pB))
                                prefB(96+NBso(pB))
	faddp %st,%st(3)
	fldl 104(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 424(pA)
	fmulp %st,%st(5)
	fldl 112(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 112(pA)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 432(pA)
	fmulp %st,%st(3)
	fldl 120(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 120(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 440(pA)
	fmulp %st,%st(1)
	fldl 128(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 128(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 448(pA)
	fmulp %st,%st(7)
	fldl 136(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 136(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 456(pA)
	fmulp %st,%st(5)
	fldl 144(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 144(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 464(pA)
	fmulp %st,%st(3)
	fldl 152(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 152(pA)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 472(pA)
	fmulp %st,%st(1)
	fldl 160(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 160(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 480(pA)
	fmulp %st,%st(7)
	fldl 168(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 168(pA)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 488(pA)
	fmulp %st,%st(5)
	fldl 176(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 176(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 496(pA)
	fmulp %st,%st(3)
	fldl 184(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 184(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 504(pA)
	fmulp %st,%st(1)
	fldl 192(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 192(pA)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 512(pA)
	fmulp %st,%st(7)
	fldl 200(pB)
	fxch %st(5)
                                prefB(128+NBso(pB))
                                prefB(160+NBso(pB))
	faddp %st,%st(3)
	fldl 200(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 520(pA)
	fmulp %st,%st(5)
	fldl 208(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 208(pA)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 528(pA)
	fmulp %st,%st(3)
	fldl 216(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 216(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 536(pA)
	fmulp %st,%st(1)
	fldl 224(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 224(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 544(pA)
	fmulp %st,%st(7)
	fldl 232(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 232(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 552(pA)
	fmulp %st,%st(5)
	fldl 240(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 240(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 560(pA)
	fmulp %st,%st(3)
	fldl 248(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 248(pA)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 568(pA)
	fmulp %st,%st(1)
	fldl 256(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 256(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 576(pA)
	fmulp %st,%st(7)
	fldl 264(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 264(pA)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 584(pA)
	fmulp %st,%st(5)
	fldl 272(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 272(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 592(pA)
	fmulp %st,%st(3)
	fldl 280(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 280(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 600(pA)
	fmulp %st,%st(1)
	fldl 288(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 288(pA)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 608(pA)
	fmulp %st,%st(7)
	fldl 296(pB)
	fxch %st(5)
                                prefB(192+NBso(pB))
                                prefB(224+NBso(pB))
	faddp %st,%st(3)
	fldl 296(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 616(pA)
	fmulp %st,%st(5)
	fldl 304(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 304(pA)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 624(pA)
	fmulp %st,%st(3)
	fldl 312(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 312(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 632(pA)
	fmulp %st,%st(1)
	fxch %st(6)
	faddp %st,%st(4)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
/*
 *      While (pB != stK);
 */
/*	cmp	pB, stK */
/*	jne	KLOOP */
/*
 *      Write results back to C
 */
   #ifdef BETAX
        fldl    (pC)
        fldl    OFF(pC)
        fldl    BETAOFF(%rsp)
        fmul    %st, %st(1)
        fmulp   %st, %st(2)
        faddp   %st, %st(3)
        faddp   %st, %st(1)
   #endif
	fstpl (pC)
	fstpl OFF(pC)

/*
 *      pC += 2;  pA += 2*NB
 */
	addq	$incCm, pC
	addq	$NB2so, pA

	fldl (pB)
	fldl (pA)
	fmul %st(1),%st
	fldl 320(pA)
	fmulp %st,%st(2)
	fldl 8(pB)
	fldl 8(pA)
	fmul %st(1),%st
	fldl 328(pA)
	fmulp %st,%st(2)
	fldl 16(pB)
	fldl 16(pA)
	fmul %st(1),%st
   #if defined(BETA0) || defined (BETAX)
	fldz
   #else
	fldl (pC)
   #endif
	faddp %st,%st(5)
	fldl 336(pA)
	fmulp %st,%st(2)
	fldl 24(pB)
   #if defined(BETA0) || defined (BETAX)
	fldz
   #else
	fldl OFF(pC)
   #endif
	faddp %st,%st(7)
	fldl 24(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 344(pA)
	fmulp %st,%st(1)
	fldl 32(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 32(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 352(pA)
	fmulp %st,%st(7)
	fldl 40(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 40(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 360(pA)
	fmulp %st,%st(5)
	fldl 48(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 48(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 368(pA)
	fmulp %st,%st(3)
	fldl 56(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 56(pA)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 376(pA)
	fmulp %st,%st(1)
	fldl 64(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 64(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 384(pA)
	fmulp %st,%st(7)
	fldl 72(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 72(pA)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 392(pA)
	fmulp %st,%st(5)
	fldl 80(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 80(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 400(pA)
	fmulp %st,%st(3)
	fldl 88(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 88(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 408(pA)
	fmulp %st,%st(1)
	fldl 96(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 96(pA)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 416(pA)
	fmulp %st,%st(7)
	fldl 104(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 104(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 424(pA)
	fmulp %st,%st(5)
	fldl 112(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 112(pA)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 432(pA)
	fmulp %st,%st(3)
	fldl 120(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 120(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 440(pA)
	fmulp %st,%st(1)
	fldl 128(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 128(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 448(pA)
	fmulp %st,%st(7)
	fldl 136(pB)
	fxch %st(5)
                                prefB(256+NBso(pB))
                                prefB(288+NBso(pB))
	faddp %st,%st(3)
	fldl 136(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 456(pA)
	fmulp %st,%st(5)
	fldl 144(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 144(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 464(pA)
	fmulp %st,%st(3)
	fldl 152(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 152(pA)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 472(pA)
	fmulp %st,%st(1)
	fldl 160(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 160(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 480(pA)
	fmulp %st,%st(7)
	fldl 168(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 168(pA)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 488(pA)
	fmulp %st,%st(5)
	fldl 176(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 176(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 496(pA)
	fmulp %st,%st(3)
	fldl 184(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 184(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 504(pA)
	fmulp %st,%st(1)
	fldl 192(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 192(pA)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 512(pA)
	fmulp %st,%st(7)
	fldl 200(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 200(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 520(pA)
	fmulp %st,%st(5)
	fldl 208(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 208(pA)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 528(pA)
	fmulp %st,%st(3)
	fldl 216(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 216(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 536(pA)
	fmulp %st,%st(1)
	fldl 224(pB)
	fxch %st(7)
                                addq    incCn, pC
	faddp %st,%st(5)
	fldl 224(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 544(pA)
	fmulp %st,%st(7)
	fldl 232(pB)
	fxch %st(5)
                                prefC((pC))
	faddp %st,%st(3)
	fldl 232(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 552(pA)
	fmulp %st,%st(5)
	fldl 240(pB)
	fxch %st(3)
                                subq    incCn, pC
	faddp %st,%st(1)
	fldl 240(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 560(pA)
	fmulp %st,%st(3)
	fldl 248(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 248(pA)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 568(pA)
	fmulp %st,%st(1)
	fldl 256(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 256(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 576(pA)
	fmulp %st,%st(7)
	fldl 264(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 264(pA)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 584(pA)
	fmulp %st,%st(5)
	fldl 272(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 272(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 592(pA)
	fmulp %st,%st(3)
	fldl 280(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 280(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 600(pA)
	fmulp %st,%st(1)
	fldl 288(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 288(pA)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 608(pA)
	fmulp %st,%st(7)
	fldl 296(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 296(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 616(pA)
	fmulp %st,%st(5)
	fldl 304(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 304(pA)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 624(pA)
	fmulp %st,%st(3)
	fldl 312(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 312(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 632(pA)
	fmulp %st,%st(1)
	fxch %st(6)
	faddp %st,%st(4)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
/*
 *      While (pB != stK);
 */
/*	cmp	pB, stK */
/*	jne	KLOOP */
/*
 *      Write results back to C
 */
   #ifdef BETAX
        fldl    (pC)
        fldl    OFF(pC)
        fldl    BETAOFF(%rsp)
        fmul    %st, %st(1)
        fmulp   %st, %st(2)
        faddp   %st, %st(3)
        faddp   %st, %st(1)
   #endif
	fstpl (pC)
	fstpl OFF(pC)

/*	end	MLOOP_DRAIN */

/*
 *      pC += incCn;  pB += NB;
 */
	addq	incCn, pC
	addq	$NBso, pB
#ifndef NB
        cmp     pB, stN
        je      DONE
#endif
/*
 *      End unrolled 1st iteration of NLOOP
 */
NLOOP:
   #ifdef ATL_GAS_x8632
	movl	48(%esp), pA
   #else
        movq    pA0, pA
   #endif
   #ifndef MB
        cmp     stM, pA
        je      MLOOP_DRAIN
   #endif
#if !defined(MB) || MB > 4
MLOOP:
	fldl (pB)
	fldl (pA)
	fmul %st(1),%st
	fldl 320(pA)
	fmulp %st,%st(2)
	fldl 8(pB)
	fldl 8(pA)
	fmul %st(1),%st
	fldl 328(pA)
	fmulp %st,%st(2)
	fldl 16(pB)
	fldl 16(pA)
	fmul %st(1),%st
   #if defined(BETA0) || defined (BETAX)
        prefC((pC))
	fldz
   #else
	fldl (pC)
   #endif
	faddp %st,%st(5)
	fldl 336(pA)
	fmulp %st,%st(2)
	fldl 24(pB)
   #if defined(BETA0) || defined (BETAX)
	fldz
   #else
	fldl OFF(pC)
   #endif
	faddp %st,%st(7)
	fldl 24(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 344(pA)
	fmulp %st,%st(1)
	fldl 32(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 32(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 352(pA)
	fmulp %st,%st(7)
	fldl 40(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 40(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 360(pA)
	fmulp %st,%st(5)
	fldl 48(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 48(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 368(pA)
	fmulp %st,%st(3)
	fldl 56(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 56(pA)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 376(pA)
	fmulp %st,%st(1)
	fldl 64(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 64(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 384(pA)
	fmulp %st,%st(7)
	fldl 72(pB)
	fxch %st(5)
                pref2((pfA))
                pref2(32(pfA))
	faddp %st,%st(3)
	fldl 72(pA)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 392(pA)
	fmulp %st,%st(5)
	fldl 80(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 80(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 400(pA)
	fmulp %st,%st(3)
	fldl 88(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 88(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 408(pA)
	fmulp %st,%st(1)
	fldl 96(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 96(pA)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 416(pA)
	fmulp %st,%st(7)
	fldl 104(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 104(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 424(pA)
	fmulp %st,%st(5)
	fldl 112(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 112(pA)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 432(pA)
	fmulp %st,%st(3)
	fldl 120(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 120(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 440(pA)
	fmulp %st,%st(1)
	fldl 128(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 128(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 448(pA)
	fmulp %st,%st(7)
	fldl 136(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 136(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 456(pA)
	fmulp %st,%st(5)
	fldl 144(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 144(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 464(pA)
	fmulp %st,%st(3)
	fldl 152(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 152(pA)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 472(pA)
	fmulp %st,%st(1)
	fldl 160(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 160(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 480(pA)
	fmulp %st,%st(7)
	fldl 168(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 168(pA)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 488(pA)
	fmulp %st,%st(5)
	fldl 176(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 176(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 496(pA)
	fmulp %st,%st(3)
	fldl 184(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 184(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 504(pA)
	fmulp %st,%st(1)
	fldl 192(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 192(pA)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 512(pA)
	fmulp %st,%st(7)
	fldl 200(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 200(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 520(pA)
	fmulp %st,%st(5)
	fldl 208(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 208(pA)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 528(pA)
	fmulp %st,%st(3)
	fldl 216(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 216(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 536(pA)
	fmulp %st,%st(1)
	fldl 224(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 224(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 544(pA)
	fmulp %st,%st(7)
	fldl 232(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 232(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 552(pA)
	fmulp %st,%st(5)
	fldl 240(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 240(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 560(pA)
	fmulp %st,%st(3)
	fldl 248(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 248(pA)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 568(pA)
	fmulp %st,%st(1)
	fldl 256(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 256(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 576(pA)
	fmulp %st,%st(7)
	fldl 264(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 264(pA)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 584(pA)
	fmulp %st,%st(5)
	fldl 272(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 272(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 592(pA)
	fmulp %st,%st(3)
	fldl 280(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 280(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 600(pA)
	fmulp %st,%st(1)
	fldl 288(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 288(pA)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 608(pA)
	fmulp %st,%st(7)
	fldl 296(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 296(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 616(pA)
	fmulp %st,%st(5)
	fldl 304(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 304(pA)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 624(pA)
	fmulp %st,%st(3)
	fldl 312(pB)
	fxch %st(1)
                        prefC(32(pC))
	faddp %st,%st(7)
	fldl 312(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 632(pA)
	fmulp %st,%st(1)
	fxch %st(6)
	faddp %st,%st(4)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
/*
 *      While (pB != stK);
 */
/*	cmp	pB, stK */
/*	jne	KLOOP */
/*
 *      Write results back to C
 */
   #ifdef BETAX
        fldl    (pC)
        fldl    OFF(pC)
        fldl    BETAOFF(%rsp)
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
	addq	$incCm, pC
	addq	$NB2so, pA
	fldl (pB)
	fldl (pA)
	fmul %st(1),%st
	fldl 320(pA)
	fmulp %st,%st(2)
	fldl 8(pB)
	fldl 8(pA)
	fmul %st(1),%st
	fldl 328(pA)
	fmulp %st,%st(2)
	fldl 16(pB)
	fldl 16(pA)
	fmul %st(1),%st
   #if defined(BETA0) || defined (BETAX)
	fldz
   #else
	fldl (pC)
   #endif
	faddp %st,%st(5)
	fldl 336(pA)
	fmulp %st,%st(2)
	fldl 24(pB)
   #if defined(BETA0) || defined (BETAX)
	fldz
   #else
	fldl OFF(pC)
   #endif
	faddp %st,%st(7)
	fldl 24(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 344(pA)
	fmulp %st,%st(1)
	fldl 32(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 32(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 352(pA)
	fmulp %st,%st(7)
	fldl 40(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 40(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 360(pA)
	fmulp %st,%st(5)
	fldl 48(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 48(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 368(pA)
	fmulp %st,%st(3)
	fldl 56(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 56(pA)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 376(pA)
	fmulp %st,%st(1)
	fldl 64(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 64(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 384(pA)
	fmulp %st,%st(7)
	fldl 72(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 72(pA)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 392(pA)
	fmulp %st,%st(5)
	fldl 80(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 80(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 400(pA)
	fmulp %st,%st(3)
	fldl 88(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 88(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 408(pA)
	fmulp %st,%st(1)
	fldl 96(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 96(pA)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 416(pA)
	fmulp %st,%st(7)
	fldl 104(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 104(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 424(pA)
	fmulp %st,%st(5)
	fldl 112(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 112(pA)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 432(pA)
	fmulp %st,%st(3)
	fldl 120(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 120(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 440(pA)
	fmulp %st,%st(1)
	fldl 128(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 128(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 448(pA)
	fmulp %st,%st(7)
	fldl 136(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 136(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 456(pA)
	fmulp %st,%st(5)
	fldl 144(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 144(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 464(pA)
	fmulp %st,%st(3)
	fldl 152(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 152(pA)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 472(pA)
	fmulp %st,%st(1)
	fldl 160(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 160(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 480(pA)
	fmulp %st,%st(7)
	fldl 168(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 168(pA)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 488(pA)
	fmulp %st,%st(5)
	fldl 176(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 176(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 496(pA)
	fmulp %st,%st(3)
	fldl 184(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 184(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 504(pA)
	fmulp %st,%st(1)
	fldl 192(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 192(pA)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 512(pA)
	fmulp %st,%st(7)
	fldl 200(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 200(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 520(pA)
	fmulp %st,%st(5)
	fldl 208(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 208(pA)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 528(pA)
	fmulp %st,%st(3)
	fldl 216(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 216(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 536(pA)
	fmulp %st,%st(1)
	fldl 224(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 224(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 544(pA)
	fmulp %st,%st(7)
	fldl 232(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 232(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 552(pA)
	fmulp %st,%st(5)
	fldl 240(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 240(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 560(pA)
	fmulp %st,%st(3)
	fldl 248(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 248(pA)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 568(pA)
	fmulp %st,%st(1)
	fldl 256(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 256(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 576(pA)
	fmulp %st,%st(7)
	fldl 264(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 264(pA)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 584(pA)
	fmulp %st,%st(5)
	fldl 272(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 272(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 592(pA)
	fmulp %st,%st(3)
	fldl 280(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 280(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 600(pA)
	fmulp %st,%st(1)
	fldl 288(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 288(pA)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 608(pA)
	fmulp %st,%st(7)
	fldl 296(pB)
	fxch %st(5)
/*                addl    $64, pfA */
                add    $36, pfA
	faddp %st,%st(3)
	fldl 296(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 616(pA)
	fmulp %st,%st(5)
	fldl 304(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 304(pA)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 624(pA)
	fmulp %st,%st(3)
	fldl 312(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 312(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 632(pA)
	fmulp %st,%st(1)
	fxch %st(6)
	faddp %st,%st(4)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
/*
 *      While (pB != stK);
 */
/*	cmp	pB, stK */
/*	jne	KLOOP */
/*
 *      Write results back to C
 */
   #ifdef BETAX
        fldl    (pC)
        fldl    OFF(pC)
        fldl    BETAOFF(%rsp)
        fmul    %st, %st(1)
        fmulp   %st, %st(2)
        faddp   %st, %st(3)
        faddp   %st, %st(1)
   #endif
	fstpl (pC)
	fstpl OFF(pC)

/*       pC += 2;  pA += 2*NB; pB -= NB; */
/* */
	addq	$incCm, pC
	addq	$NB2so, pA
/*
 *      while (pA != stM);
 */
	cmp	pA, stM
	jne	MLOOP
#endif

MLOOP_DRAIN:
	fldl (pB)
	fldl (pA)
	fmul %st(1),%st
	fldl 320(pA)
	fmulp %st,%st(2)
	fldl 8(pB)
	fldl 8(pA)
	fmul %st(1),%st
	fldl 328(pA)
	fmulp %st,%st(2)
	fldl 16(pB)
	fldl 16(pA)
	fmul %st(1),%st
   #if defined(BETA0) || defined (BETAX)
	fldz
   #else
	fldl (pC)
   #endif
	faddp %st,%st(5)
	fldl 336(pA)
	fmulp %st,%st(2)
	fldl 24(pB)
   #if defined(BETA0) || defined (BETAX)
	fldz
   #else
	fldl OFF(pC)
   #endif
	faddp %st,%st(7)
	fldl 24(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 344(pA)
	fmulp %st,%st(1)
	fldl 32(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 32(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 352(pA)
	fmulp %st,%st(7)
	fldl 40(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 40(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 360(pA)
	fmulp %st,%st(5)
	fldl 48(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 48(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 368(pA)
	fmulp %st,%st(3)
	fldl 56(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 56(pA)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 376(pA)
	fmulp %st,%st(1)
	fldl 64(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 64(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 384(pA)
	fmulp %st,%st(7)
	fldl 72(pB)
	fxch %st(5)
                                prefB(NBso(pB))
                                prefB(32+NBso(pB))
	faddp %st,%st(3)
	fldl 72(pA)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 392(pA)
	fmulp %st,%st(5)
	fldl 80(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 80(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 400(pA)
	fmulp %st,%st(3)
	fldl 88(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 88(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 408(pA)
	fmulp %st,%st(1)
	fldl 96(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 96(pA)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 416(pA)
	fmulp %st,%st(7)
	fldl 104(pB)
	fxch %st(5)
                                prefB(64+NBso(pB))
                                prefB(96+NBso(pB))
	faddp %st,%st(3)
	fldl 104(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 424(pA)
	fmulp %st,%st(5)
	fldl 112(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 112(pA)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 432(pA)
	fmulp %st,%st(3)
	fldl 120(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 120(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 440(pA)
	fmulp %st,%st(1)
	fldl 128(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 128(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 448(pA)
	fmulp %st,%st(7)
	fldl 136(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 136(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 456(pA)
	fmulp %st,%st(5)
	fldl 144(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 144(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 464(pA)
	fmulp %st,%st(3)
	fldl 152(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 152(pA)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 472(pA)
	fmulp %st,%st(1)
	fldl 160(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 160(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 480(pA)
	fmulp %st,%st(7)
	fldl 168(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 168(pA)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 488(pA)
	fmulp %st,%st(5)
	fldl 176(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 176(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 496(pA)
	fmulp %st,%st(3)
	fldl 184(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 184(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 504(pA)
	fmulp %st,%st(1)
	fldl 192(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 192(pA)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 512(pA)
	fmulp %st,%st(7)
	fldl 200(pB)
	fxch %st(5)
                                prefB(128+NBso(pB))
                                prefB(160+NBso(pB))
	faddp %st,%st(3)
	fldl 200(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 520(pA)
	fmulp %st,%st(5)
	fldl 208(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 208(pA)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 528(pA)
	fmulp %st,%st(3)
	fldl 216(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 216(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 536(pA)
	fmulp %st,%st(1)
	fldl 224(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 224(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 544(pA)
	fmulp %st,%st(7)
	fldl 232(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 232(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 552(pA)
	fmulp %st,%st(5)
	fldl 240(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 240(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 560(pA)
	fmulp %st,%st(3)
	fldl 248(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 248(pA)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 568(pA)
	fmulp %st,%st(1)
	fldl 256(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 256(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 576(pA)
	fmulp %st,%st(7)
	fldl 264(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 264(pA)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 584(pA)
	fmulp %st,%st(5)
	fldl 272(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 272(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 592(pA)
	fmulp %st,%st(3)
	fldl 280(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 280(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 600(pA)
	fmulp %st,%st(1)
	fldl 288(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 288(pA)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 608(pA)
	fmulp %st,%st(7)
	fldl 296(pB)
	fxch %st(5)
                                prefB(192+NBso(pB))
                                prefB(224+NBso(pB))
	faddp %st,%st(3)
	fldl 296(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 616(pA)
	fmulp %st,%st(5)
	fldl 304(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 304(pA)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 624(pA)
	fmulp %st,%st(3)
	fldl 312(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 312(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 632(pA)
	fmulp %st,%st(1)
	fxch %st(6)
	faddp %st,%st(4)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
/*
 *      While (pB != stK);
 */
/*	cmp	pB, stK */
/*	jne	KLOOP */
/*
 *      Write results back to C
 */
   #ifdef BETAX
        fldl    (pC)
        fldl    OFF(pC)
        fldl    BETAOFF(%rsp)
        fmul    %st, %st(1)
        fmulp   %st, %st(2)
        faddp   %st, %st(3)
        faddp   %st, %st(1)
   #endif
	fstpl (pC)
	fstpl OFF(pC)

/*       pC += 2;  pA += 2*NB */
/* */
	addq	$incCm, pC
	addq	$NB2so, pA

	fldl (pB)
	fldl (pA)
	fmul %st(1),%st
	fldl 320(pA)
	fmulp %st,%st(2)
	fldl 8(pB)
	fldl 8(pA)
	fmul %st(1),%st
	fldl 328(pA)
	fmulp %st,%st(2)
	fldl 16(pB)
	fldl 16(pA)
	fmul %st(1),%st
   #if defined(BETA0) || defined (BETAX)
	fldz
   #else
	fldl (pC)
   #endif
	faddp %st,%st(5)
	fldl 336(pA)
	fmulp %st,%st(2)
	fldl 24(pB)
   #if defined(BETA0) || defined (BETAX)
	fldz
   #else
	fldl OFF(pC)
   #endif
	faddp %st,%st(7)
	fldl 24(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 344(pA)
	fmulp %st,%st(1)
	fldl 32(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 32(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 352(pA)
	fmulp %st,%st(7)
	fldl 40(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 40(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 360(pA)
	fmulp %st,%st(5)
	fldl 48(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 48(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 368(pA)
	fmulp %st,%st(3)
	fldl 56(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 56(pA)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 376(pA)
	fmulp %st,%st(1)
	fldl 64(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 64(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 384(pA)
	fmulp %st,%st(7)
	fldl 72(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 72(pA)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 392(pA)
	fmulp %st,%st(5)
	fldl 80(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 80(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 400(pA)
	fmulp %st,%st(3)
	fldl 88(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 88(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 408(pA)
	fmulp %st,%st(1)
	fldl 96(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 96(pA)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 416(pA)
	fmulp %st,%st(7)
	fldl 104(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 104(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 424(pA)
	fmulp %st,%st(5)
	fldl 112(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 112(pA)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 432(pA)
	fmulp %st,%st(3)
	fldl 120(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 120(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 440(pA)
	fmulp %st,%st(1)
	fldl 128(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 128(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 448(pA)
	fmulp %st,%st(7)
	fldl 136(pB)
	fxch %st(5)
                                prefB(256+NBso(pB))
                                prefB(288+NBso(pB))
	faddp %st,%st(3)
	fldl 136(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 456(pA)
	fmulp %st,%st(5)
	fldl 144(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 144(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 464(pA)
	fmulp %st,%st(3)
	fldl 152(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 152(pA)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 472(pA)
	fmulp %st,%st(1)
	fldl 160(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 160(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 480(pA)
	fmulp %st,%st(7)
	fldl 168(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 168(pA)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 488(pA)
	fmulp %st,%st(5)
	fldl 176(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 176(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 496(pA)
	fmulp %st,%st(3)
	fldl 184(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 184(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 504(pA)
	fmulp %st,%st(1)
	fldl 192(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 192(pA)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 512(pA)
	fmulp %st,%st(7)
	fldl 200(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 200(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 520(pA)
	fmulp %st,%st(5)
	fldl 208(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 208(pA)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 528(pA)
	fmulp %st,%st(3)
	fldl 216(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 216(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 536(pA)
	fmulp %st,%st(1)
	fldl 224(pB)
	fxch %st(7)
                                addq    incCn, pC
	faddp %st,%st(5)
	fldl 224(pA)
	fmul %st(7),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 544(pA)
	fmulp %st,%st(7)
	fldl 232(pB)
	fxch %st(5)
                                prefC((pC))
	faddp %st,%st(3)
	fldl 232(pA)
	fmul %st(5),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 552(pA)
	fmulp %st,%st(5)
	fldl 240(pB)
	fxch %st(3)
                                subq    incCn, pC
	faddp %st,%st(1)
	fldl 240(pA)
	fmul %st(3),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 560(pA)
	fmulp %st,%st(3)
	fldl 248(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 248(pA)
	fmul %st(1),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 568(pA)
	fmulp %st,%st(1)
	fldl 256(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 256(pA)
	fmul %st(7),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 576(pA)
	fmulp %st,%st(7)
	fldl 264(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 264(pA)
	fmul %st(5),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 584(pA)
	fmulp %st,%st(5)
	fldl 272(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 272(pA)
	fmul %st(3),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 592(pA)
	fmulp %st,%st(3)
	fldl 280(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 280(pA)
	fmul %st(1),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 600(pA)
	fmulp %st,%st(1)
	fldl 288(pB)
	fxch %st(7)
	faddp %st,%st(5)
	fldl 288(pA)
	fmul %st(7),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 608(pA)
	fmulp %st,%st(7)
	fldl 296(pB)
	fxch %st(5)
	faddp %st,%st(3)
	fldl 296(pA)
	fmul %st(5),%st
	fxch %st(4)
	faddp %st,%st(2)
	fldl 616(pA)
	fmulp %st,%st(5)
	fldl 304(pB)
	fxch %st(3)
	faddp %st,%st(1)
	fldl 304(pA)
	fmul %st(3),%st
	fxch %st(2)
	faddp %st,%st(6)
	fldl 624(pA)
	fmulp %st,%st(3)
	fldl 312(pB)
	fxch %st(1)
	faddp %st,%st(7)
	fldl 312(pA)
	fmul %st(1),%st
	fxch %st(6)
	faddp %st,%st(4)
	fldl 632(pA)
	fmulp %st,%st(1)
	fxch %st(6)
	faddp %st,%st(4)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
	faddp %st,%st(2)
/*
 *      While (pB != stK);
 */
/*	cmp	pB, stK */
/*	jne	KLOOP */
/*
 *      Write results back to C
 */
   #ifdef BETAX
        fldl    (pC)
        fldl    OFF(pC)
        fldl    BETAOFF(%rsp)
        fmul    %st, %st(1)
        fmulp   %st, %st(2)
        faddp   %st, %st(3)
        faddp   %st, %st(1)
   #endif
	fstpl (pC)
	fstpl OFF(pC)

/*	end	MLOOP_DRAIN */

/*
 *      pC += incCn;  pB += NB;
 */
	addq	incCn, pC
	addq	$NBso, pB
/*
 *      while (pB != stN);
 */
	cmp	pB, stN
	jne	NLOOP
DONE:
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
