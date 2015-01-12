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


#ifdef ATL_GAS_x8664
   #define movl movq
   #define addl addq
   #define subl subq
   #define esp  rsp
   #define X8664
#elif defined(ATL_GAS_x8632)
   #define X8632
#else
   #error "This kernel requires a gas x86 assembler!"
#endif
#ifndef ATL_SSE1
   #error "This routine requires SSE1!"
#endif
#ifndef MB
   #ifdef X8664
      #define MB 0
   #else
      #error "MB must be compile-time constant!"
   #endif
#endif
#ifndef NB
   #define NB 0
#endif
#if !defined(KB) || KB != 120
   #error "KB must be compile-time constant of 120!"
#endif

#ifdef ATL_GAS_x8632
   #define pC0     %esi
   #define pA0     %ecx
   #define pA3     %eax
   #define pB0     %edi
   #define ldab    %edx
   #define pfA     %ebp
   #define stN     %bh
   #define stM     %bl
#else
   #define pC0     %rsi
   #define pA0     %rcx
   #define pA3     %rax
   #define pB0     %rbx
   #define ldab    %rdx
   #define pfA     %rbp
   #define stM     %rdi
   #define stN     %r8
   #define MM      %r9
   #define MBKB    %r10
   #define incCn   %r11

   #define rBETA   %xmm8
   #define rc0     %xmm9
   #define rc1     %xmm10
   #define rtmp    %xmm11
#endif

#define rC0     %xmm0
#define rC1     %xmm1
#define rC2     %xmm2
#define rC3     %xmm3
#define rC4     %xmm4
#define rC5     %xmm5
#define rA0     %xmm6
#define rB0     %xmm7

#define NB6so   (6*KB*4)

/*
 * Prefetch defines
 */
#if 1
   #define pref2(mem) prefetcht1   mem
   #define prefB(mem) prefetcht0   mem
   #if defined(ATL_ARCH_HAMMER32) || defined(ATL_ARCH_HAMMER64)
      #define prefC(mem) prefetchw    mem
   #else
      #define prefC(mem) prefetcht0    mem
   #endif
#else
   #define pref2(mem)
   #define prefB(mem)
   #define prefC(mem)
#endif
#ifdef SCPLX
   #define CMUL(arg_) 2*arg_
#else
   #define CMUL(arg_) arg_
#endif

/*
                      %rdi/4       %rsi/8       %rdx/12          %xmm0/16
 void ATL_AUSERMM(const int M, const int N, const int K, const TYPE alpha,
                       %rcx/20         %r8/24         %r9/28             32
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                        %xmm1/36    16/40          24/44
                 const TYPE beta, TYPE *C, const int ldc)
*/

        .text
.global ATL_asmdecor(ATL_USERMM)
ALIGN16
ATL_asmdecor(ATL_USERMM):
#ifdef X8632
        subl    $28, %esp
        movl    %ebp, 24(%esp)
        movl    %ebx, 20(%esp)
        movl    %esi, 16(%esp)
        movl    %edi, 12(%esp)
/*
 *      Store incCn = (ldc-MB)*sizeof and BETA to stack
 */
        movl    72(%esp), %eax
                                        movb %al, stM
        subl    $MB, %eax
#ifdef SCPLX
        shl     $3, %eax
#else
        shl     $2, %eax
#endif
        movl    %eax, 8(%esp)
   #ifdef BETAX
        flds    64(%esp)
        fstps   (%esp)
      #define BETAOFF 0
   #endif
/*
 *      Initialize pA = A;  pB = B; pC = C;
 */
        movl    68(%esp), pC0
                                        prefC((pC0))
                                        prefC(64(pC0))
        movl    48(%esp), pA0
        movl    56(%esp), pB0
                                        prefB((pB0))
                                        prefB(64(pB0))
        addl    $120, pA0
        addl    $120, pB0
/*
 *      ldab = K * 4;
 */
        movl    40(%esp), ldab
        shl     $2, ldab
/*
 *      pfA = pA + NBNB
 */
        movl    pA0, pfA
        addl    $MB*KB*4-120, pfA
        movb    36(%esp), stN
#else
/*
 *      Save callee-saved iregs
 */
        movq    %rbp, -8(%rsp)
        movq    %rbx, -16(%rsp)
/*
 *      Initilize beta
 */
   #ifdef BETAX
        unpcklps        %xmm1, %xmm1
        movlhps         %xmm1, %xmm1
        movaps  %xmm1, rBETA
   #endif
/*
 *      On entry, pA0 is correct, ldab has K, stM has M, pC0 has stN; MM has pB0
 *      Set ldab = K*4, stN=N, and MBKB = M*K*4
 */
        shlq    $2, ldab
        movq    pC0, stN
   #if MB == 0
        movq    stM, MBKB
        imulq   ldab, MBKB
   #else
        movq    $MB*KB*4, MBKB
   #endif
/*
 *      Initialize incCn = (ldc-MB)*sizeof; pC0 = C; pB0 = B, pfA = pA0+MBKB
 */
        movl    24(%rsp), %rsi
        movq    %rsi, incCn
        shlq    $32, incCn
        sarq    $32, incCn
        subq    stM, incCn
   #ifdef SCPLX
        shlq    $3, incCn
   #else
        shlq    $2, incCn
   #endif
        movq    16(%rsp), pC0
        movq    MM, pB0
        movq    stM, MM
        subq    $6, MM
        movq    pA0, pfA
        addq    MBKB, pfA
/*
 *      Bias ptrs to max 1-byte range
 */
        addq    $120, pA0
        addq    $120, pB0
/*
 *      MBKB henceforth used to increment pA0, so subtract NB6so+240 from it
 */
        subq    $NB6so-240, MBKB
#endif
                                        prefB((pA0))
                                        prefB(64(pA0))
        ALIGN8
NLOOP:
	lea	0(pA0, ldab,2), pA3
	addl	ldab, pA3
#if MB == 0 || MB > 6
   #ifdef X8632
      #ifdef ATL_DivAns
	movb	$ATL_DivAns-1, stM
      #else
	movb	$MB/6-1, stM
      #endif
   #else
        movq    MM, stM
      #if MB == 0
        cmp     $0, stM
        je      MLOOPCU
      #endif
   #endif
	ALIGN8
MLOOP:
#ifdef X8664
   #if defined(BETAX)
      #ifdef SCPLX
        movups  (pC0), rc0
        movups  16(pC0), rtmp
        shufps  $0xE8, rc0, rc0
        shufps  $0xE8, rtmp, rtmp
        movlhps rtmp, rc0
        movss   32(pC0), rc1
        movss   40(pC0), rtmp
        unpcklps        rtmp, rc1
      #else
        movups  (pC0), rc0
        xorps   rc1, rc1
        movlps  16(pC0), rc1
      #endif
        mulps   rBETA, rc0
        mulps   rBETA, rc1
   #endif
        ALIGN16
#elif defined(BETAX)
        movss   (pC0), rC0
        movss   CMUL(4)(pC0), rC1
        movss   CMUL(8)(pC0), rC2
        movss   CMUL(12)(pC0), rC3
        movss   CMUL(16)(pC0), rC4
        movss   CMUL(20)(pC0), rC5
        movss   BETAOFF(%esp), rA0
        mulss   rA0, rC0
        mulss   rA0, rC1
        mulss   rA0, rC2
        mulss   rA0, rC3
        mulss   rA0, rC4
        mulss   rA0, rC5
        ALIGN16
#endif

/*KLOOP */
#if defined(BETA1)
	movaps	0-120(pA0), rC0
	movaps	0-120(pB0), rB0
	mulps	rB0, rC0
        addss   (pC0), rC0
	movaps	0-120(pA0,ldab), rC1
	mulps	rB0, rC1
        addss   CMUL(4)(pC0), rC1
	movaps	0-120(pA0,ldab,2), rC2
	mulps	rB0, rC2
        addss   CMUL(8)(pC0), rC2
	movaps	0-120(pA3), rC3
	mulps	rB0, rC3
        addss   CMUL(12)(pC0), rC3
	movaps	0-120(pA3,ldab), rC4
	mulps	rB0, rC4
        addss   CMUL(16)(pC0), rC4
	movaps	0-120(pA3,ldab,2), rC5
	mulps	rB0, rC5
        addss   CMUL(20)(pC0), rC5
#elif defined(BETA0) || defined(X8664)
	movaps	0-120(pA0), rC0
	movaps	0-120(pB0), rB0
	mulps	rB0, rC0
	movaps	0-120(pA0,ldab), rC1
	mulps	rB0, rC1
	movaps	0-120(pA0,ldab,2), rC2
	mulps	rB0, rC2
	movaps	0-120(pA3), rC3
	mulps	rB0, rC3
	movaps	0-120(pA3,ldab), rC4
	mulps	rB0, rC4
	movaps	0-120(pA3,ldab,2), rC5
	mulps	rB0, rC5
#else
	movaps	0-120(pA0), rA0
	movaps	0-120(pB0), rB0
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
#endif
	movaps	16-120(pA0), rA0
	movaps	16-120(pB0), rB0
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

	movaps	32-120(pA0), rA0
	movaps	32-120(pB0), rB0
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

	movaps	48-120(pA0), rA0
	movaps	48-120(pB0), rB0
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

	movaps	64-120(pA0), rA0
	movaps	64-120(pB0), rB0
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

	movaps	80-120(pA0), rA0
	movaps	80-120(pB0), rB0
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

	movaps	96-120(pA0), rA0
	movaps	96-120(pB0), rB0
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

	movaps	112-120(pA0), rA0
	movaps	112-120(pB0), rB0
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
                                                pref2((pfA))
                                                addl    $25, pfA

	movaps	128-120(pA0), rA0
	movaps	128-120(pB0), rB0
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

	movaps	144-120(pA0), rA0
	movaps	144-120(pB0), rB0
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

	movaps	160-120(pA0), rA0
	movaps	160-120(pB0), rB0
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

	movaps	176-120(pA0), rA0
	movaps	176-120(pB0), rB0
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

	movaps	192-120(pA0), rA0
	movaps	192-120(pB0), rB0
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

	movaps	208-120(pA0), rA0
	movaps	208-120(pB0), rB0
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

	movaps	224-120(pA0), rA0
	movaps	224-120(pB0), rB0
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
                                        addl    $240, pA0
                                        addl    $240, pA3
                                        addl    $240, pB0

	movaps	0-120(pA0), rA0
	movaps	0-120(pB0), rB0
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

	movaps	16-120(pA0), rA0
	movaps	16-120(pB0), rB0
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

	movaps	32-120(pA0), rA0
	movaps	32-120(pB0), rB0
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

	movaps	48-120(pA0), rA0
	movaps	48-120(pB0), rB0
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

	movaps	64-120(pA0), rA0
	movaps	64-120(pB0), rB0
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

	movaps	80-120(pA0), rA0
	movaps	80-120(pB0), rB0
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

	movaps	96-120(pA0), rA0
	movaps	96-120(pB0), rB0
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

	movaps	112-120(pA0), rA0
	movaps	112-120(pB0), rB0
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

	movaps	128-120(pA0), rA0
	movaps	128-120(pB0), rB0
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

	movaps	144-120(pA0), rA0
	movaps	144-120(pB0), rB0
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

	movaps	160-120(pA0), rA0
	movaps	160-120(pB0), rB0
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

	movaps	176-120(pA0), rA0
	movaps	176-120(pB0), rB0
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

	movaps	192-120(pA0), rA0
	movaps	192-120(pB0), rB0
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

	movaps	208-120(pA0), rA0
	movaps	208-120(pB0), rB0
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

	movaps	224-120(pA0), rA0
	movaps	224-120(pB0), rB0
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

/*ENDKLOOP */
/*
 *      Get these bastard things summed up
 */
                                        /* rC0 = c0a    c0b    c0c    c0d */
                                        /* rC1 = c1a    c1b    c1c    c1d */
                                        /* rC2 = c2a    c2b    c2c    c2d */
                                        /* rC3 = c3a    c3b    c3c    c3d */
                                        /* rC4 = c4a    c4b    c4c    c4d */
                                        /* rC5 = c5a    c5b    c5c    c5d */
#ifdef ATL_SSE3
        haddps          rC1, rC0        /* rC0 = c1cd   c1ab   c0cd   c0ab */
        haddps          rC3, rC2        /* rC2 = c3cd   c3ab   c2cd   c2ab */
        haddps          rC5, rC4        /* rC4 = c5cd   c5ab   c4cd   c4ab */
        haddps          rC2, rC0        /* rC0 = c3abcd c2abcd c1abcd c0abcd */
        haddps          rC4, rC4        /* rC4 = c5abcd c4abcd c5abcd c4abcd */
/*
 *      Write results back to C
 */
   #if defined(X8664) && defined(BETAX)
        addps   rc0, rC0
        addps   rc1, rC4
   #endif
   #ifdef SCPLX                         /* rC0 = c3 c2 c1 c0 */
        pshufd  $0x71, rC0, rC3         /* rC3 = c1 c3 c0 c1 */
        movss   rC0, (pC0)
        pshufd  $0x71, rC4, rC5         /* rC5 = c4 c5 c4 c5 */
        movss   rC4, 4*8(pC0)
        movhlps rC0, rC0                /* rC0 = c3 c2 c3 c2 */
        movss   rC3, 1*8(pC0)
        movhlps rC3, rC3                /* rC3 = c1 c3 c1 c3 */
        movss   rC5, 5*8(pC0)
        movss   rC0, 2*8(pC0)
        movss   rC3, 3*8(pC0)
   #else
	movups	rC0, (pC0)
	movlps	rC4, 16(pC0)
   #endif
#else
        movaps          rC2, rB0        /* rB0 = c2a    c2b    c2c    c2d */
                                        prefC((pC0))
                                        prefC(64(pC0))
        movaps          rC0, rA0        /* rA0 = c0a    c0b    c0c    c0d */
        unpckhps        rC3, rB0        /* rB0 = c2c    c3c    c2d    c3d */
        unpckhps        rC1, rA0        /* rA0 = c0c    c1c    c0d    c1d */
        unpcklps        rC3, rC2        /* rC2 = c2a    c3a    c2b    c3b */
        movlhps         rB0, rC3        /* rC3 = c3a    c3b    c2c    c3c */
        unpcklps        rC1, rC0        /* rC0 = c0a    c1a    c0b    c1b */
        movhlps         rA0, rC3        /* rC3 = c0d    c1d    c2c    c3c */
        movlhps         rC2, rA0        /* rA0 = c0c    c1c    c2a    c3a */
        movhlps         rC0, rB0        /* rB0 = c0b    c1b    c2d    c3d */
        addps           rA0, rC3        /* rC3 = c0cd   c1cd   c2ac   c3ac */
        movlhps         rC0, rC1        /* rC1 = c1a    c1b    c0a    c1a */
        movaps          rC4, rA0        /* rA0 = c4a    c4b    c4c    c4d */
        movhlps         rC1, rC2        /* rC2 = c0a    c1a    c2b    c3b */
        unpcklps        rC5, rA0        /* rA0 = c4a    c5a    c4b    c5b */
        addps           rB0, rC2        /* rC2 = c0ab   c1ab   c2bd   c3bd */
        unpckhps        rC5, rC4        /* rC4 = c4c    c5c    c4d    c5d */
        addps           rC2, rC3        /* rC3 = c0abcd c1abcd c2bdac c3bdac */
        addps           rA0, rC4        /* rC4 = c4ac   c5ac   c4bd   c5bd */
        movhlps         rC4, rC5        /* rC5 = c4bd   c5bd   X       X */
        addps           rC5, rC4        /* rC4 = c4abcd c5abcd X       X */



/*
 *      Write results back to C
 */
   #if defined(X8664) && defined(BETAX)
        addps   rc0, rC3
        addps   rc1, rC4
   #endif
   #ifdef SCPLX
        movss   rC3, (pC0)
        movhlps rC3, rC0
        movss   rC0, 16(pC0)
        shufps  $0x55, rC3, rC3
        shufps  $0x55, rC0, rC0
        movss   rC3, 8(pC0)
        movss   rC0, 24(pC0)
        movss   rC4, 32(pC0)
        shufps  $0x55, rC4, rC4
        movss   rC4, 40(pC0)
   #else
	movups	rC3, (pC0)
	movlps	rC4, 16(pC0)
   #endif
#endif
/*
 *      pC += 6;   pA += 6*NB
 */
        addl    $CMUL(24), pC0
        addl    $NB6so-240, pA0
        addl    $NB6so-240, pA3
	subl	$240, pB0
   #ifdef X8632
	subb	$1, stM
   #else
        subq    $6, stM
   #endif
        jnz     MLOOP
#endif
/*
 * Last iteration of MLOOP unrolled to prefetch B
 */
#if MB == 0
MLOOPCU:
#endif
#ifdef X8664
   #if defined(BETAX)
      #ifdef SCPLX
        movups  (pC0), rc0
        movups  16(pC0), rtmp
        shufps  $0xE8, rc0, rc0
        shufps  $0xE8, rtmp, rtmp
        movlhps rtmp, rc0
        movss   32(pC0), rc1
        movss   40(pC0), rtmp
        unpcklps        rtmp, rc1
      #else
        movups  (pC0), rc0
        xorps   rc1, rc1
        movlps  16(pC0), rc1
      #endif
        mulps   rBETA, rc0
        mulps   rBETA, rc1
   #endif
        ALIGN16
#elif defined(BETAX)
        movss   (pC0), rC0
        movss   CMUL(4)(pC0), rC1
        movss   CMUL(8)(pC0), rC2
        movss   CMUL(12)(pC0), rC3
        movss   CMUL(16)(pC0), rC4
        movss   CMUL(20)(pC0), rC5
        movss   BETAOFF(%esp), rA0
        mulss   rA0, rC0
        mulss   rA0, rC1
        mulss   rA0, rC2
        mulss   rA0, rC3
        mulss   rA0, rC4
        mulss   rA0, rC5
        ALIGN16
#endif

/*KLOOP */
#if defined(BETA1)
	movaps	0-120(pA0), rC0
	movaps	0-120(pB0), rB0
	mulps	rB0, rC0
        addss   (pC0), rC0
	movaps	0-120(pA0,ldab), rC1
	mulps	rB0, rC1
        addss   CMUL(4)(pC0), rC1
	movaps	0-120(pA0,ldab,2), rC2
	mulps	rB0, rC2
        addss   CMUL(8)(pC0), rC2
	movaps	0-120(pA3), rC3
	mulps	rB0, rC3
        addss   CMUL(12)(pC0), rC3
	movaps	0-120(pA3,ldab), rC4
	mulps	rB0, rC4
        addss   CMUL(16)(pC0), rC4
	movaps	0-120(pA3,ldab,2), rC5
	mulps	rB0, rC5
        addss   CMUL(20)(pC0), rC5
#elif defined(BETA0) || defined(X8664)
	movaps	0-120(pA0), rC0
	movaps	0-120(pB0), rB0
	mulps	rB0, rC0
	movaps	0-120(pA0,ldab), rC1
	mulps	rB0, rC1
	movaps	0-120(pA0,ldab,2), rC2
	mulps	rB0, rC2
	movaps	0-120(pA3), rC3
	mulps	rB0, rC3
	movaps	0-120(pA3,ldab), rC4
	mulps	rB0, rC4
	movaps	0-120(pA3,ldab,2), rC5
	mulps	rB0, rC5
#else
	movaps	0-120(pA0), rA0
	movaps	0-120(pB0), rB0
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
#endif
	movaps	16-120(pA0), rA0
	movaps	16-120(pB0), rB0
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

	movaps	32-120(pA0), rA0
	movaps	32-120(pB0), rB0
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

	movaps	48-120(pA0), rA0
	movaps	48-120(pB0), rB0
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

	movaps	64-120(pA0), rA0
	movaps	64-120(pB0), rB0
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

	movaps	80-120(pA0), rA0
	movaps	80-120(pB0), rB0
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

	movaps	96-120(pA0), rA0
	movaps	96-120(pB0), rB0
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

	movaps	112-120(pA0), rA0
	movaps	112-120(pB0), rB0
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

	movaps	128-120(pA0), rA0
	movaps	128-120(pB0), rB0
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

	movaps	144-120(pA0), rA0
	movaps	144-120(pB0), rB0
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

	movaps	160-120(pA0), rA0
	movaps	160-120(pB0), rB0
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
                                                prefB((pB0,ldab))
                                                prefB(64(pB0,ldab))

	movaps	176-120(pA0), rA0
	movaps	176-120(pB0), rB0
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

	movaps	192-120(pA0), rA0
	movaps	192-120(pB0), rB0
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

	movaps	208-120(pA0), rA0
	movaps	208-120(pB0), rB0
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

	movaps	224-120(pA0), rA0
	movaps	224-120(pB0), rB0
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
                                        addl    $240, pA0
                                        addl    $240, pA3
                                        addl    $240, pB0

	movaps	0-120(pA0), rA0
	movaps	0-120(pB0), rB0
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

	movaps	16-120(pA0), rA0
	movaps	16-120(pB0), rB0
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

	movaps	32-120(pA0), rA0
	movaps	32-120(pB0), rB0
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

	movaps	48-120(pA0), rA0
	movaps	48-120(pB0), rB0
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

	movaps	64-120(pA0), rA0
	movaps	64-120(pB0), rB0
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
                                                prefB(128(pB0,ldab))
                                                prefB(192(pB0,ldab))

	movaps	80-120(pA0), rA0
	movaps	80-120(pB0), rB0
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

	movaps	96-120(pA0), rA0
	movaps	96-120(pB0), rB0
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

	movaps	112-120(pA0), rA0
	movaps	112-120(pB0), rB0
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
                                                prefC((pC0))
                                                prefB(256(pB0,ldab))
	addps	rB0, rC5

	movaps	128-120(pA0), rA0
	movaps	128-120(pB0), rB0
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

	movaps	144-120(pA0), rA0
	movaps	144-120(pB0), rB0
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

	movaps	160-120(pA0), rA0
	movaps	160-120(pB0), rB0
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
                                                prefB(320(pB0,ldab))
                                                prefB(384(pB0,ldab))

	movaps	176-120(pA0), rA0
	movaps	176-120(pB0), rB0
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

	movaps	192-120(pA0), rA0
	movaps	192-120(pB0), rB0
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

	movaps	208-120(pA0), rA0
	movaps	208-120(pB0), rB0
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

	movaps	224-120(pA0), rA0
	movaps	224-120(pB0), rB0
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
   #ifdef X8632
                                        subl	$MB*KB*4-NB6so+240, pA0
                                        movl	8(%esp), pA3
                                        lea     CMUL(24)(pC0, pA3), pA3
   #else
                                        subq    MBKB, pA0
                                        lea     CMUL(24)(pC0, incCn), pA3
   #endif

/*ENDKLOOP */
/*
 *      Get these bastard things summed up
 */
                                        /* rC0 = c0a    c0b    c0c    c0d */
                                        /* rC1 = c1a    c1b    c1c    c1d */
                                        /* rC2 = c2a    c2b    c2c    c2d */
                                        /* rC3 = c3a    c3b    c3c    c3d */
                                        /* rC4 = c4a    c4b    c4c    c4d */
                                        /* rC5 = c5a    c5b    c5c    c5d */
/* */
        movaps          rC2, rB0        /* rB0 = c2a    c2b    c2c    c2d */
                                                prefC((pA3))
                                                prefC(64(pA3))
        movaps          rC0, rA0        /* rA0 = c0a    c0b    c0c    c0d */
        unpckhps        rC3, rB0        /* rB0 = c2c    c3c    c2d    c3d */
        unpckhps        rC1, rA0        /* rA0 = c0c    c1c    c0d    c1d */
        unpcklps        rC3, rC2        /* rC2 = c2a    c3a    c2b    c3b */
        movlhps         rB0, rC3        /* rC3 = c3a    c3b    c2c    c3c */
        unpcklps        rC1, rC0        /* rC0 = c0a    c1a    c0b    c1b */
        movhlps         rA0, rC3        /* rC3 = c0d    c1d    c2c    c3c */
        movlhps         rC2, rA0        /* rA0 = c0c    c1c    c2a    c3a */
        movhlps         rC0, rB0        /* rB0 = c0b    c1b    c2d    c3d */
        addps           rA0, rC3        /* rC3 = c0cd   c1cd   c2ac   c3ac */
        movlhps         rC0, rC1        /* rC1 = c1a    c1b    c0a    c1a */
        movaps          rC4, rA0        /* rA0 = c4a    c4b    c4c    c4d */
        movhlps         rC1, rC2        /* rC2 = c0a    c1a    c2b    c3b */
        unpcklps        rC5, rA0        /* rA0 = c4a    c5a    c4b    c5b */
        addps           rB0, rC2        /* rC2 = c0ab   c1ab   c2bd   c3bd */
        unpckhps        rC5, rC4        /* rC4 = c4c    c5c    c4d    c5d */
        addps           rC2, rC3        /* rC3 = c0abcd c1abcd c2bdac c3bdac */
                                                prefB(448(pB0,ldab))
        addps           rA0, rC4        /* rC4 = c4ac   c5ac   c4bd   c5bd */
        movhlps         rC4, rC5        /* rC5 = c4bd   c5bd   X       X */
        addps           rC5, rC4        /* rC4 = c4abcd c5abcd X       X */



/*
 *      Write results back to C
 */
   #if defined(X8664) && defined(BETAX)
        addps   rc0, rC3
        addps   rc1, rC4
   #endif
   #ifdef SCPLX
        movss   rC3, (pC0)
        movhlps rC3, rC0
        movss   rC0, 16(pC0)
        shufps  $0x55, rC3, rC3
        shufps  $0x55, rC0, rC0
        movss   rC3, 8(pC0)
        movss   rC0, 24(pC0)
        movss   rC4, 32(pC0)
        shufps  $0x55, rC4, rC4
        movss   rC4, 40(pC0)
   #else
	movups	rC3, (pC0)
	movlps	rC4, 16(pC0)
   #endif
/*
 *      pC += 6;   pA += 6*NB
 */
/*        jnz     MLOOP */
/*
 *      Increment pointers, and continue N-LOOP
 */
        lea     -240(pB0, ldab), pB0
/*ENDKLOOP */
        movl    pA3, pC0
   #ifdef X8632
	subb	$1, stN
   #else
        subq    $1, stN
   #endif
	jnz	NLOOP

/* DONE: */
#ifdef X8632
        movl    24(%esp), %ebp
        movl    20(%esp), %ebx
        movl    16(%esp), %esi
        movl    12(%esp), %edi
        addl    $28, %esp
        ret
#else
        movq    -8(%rsp), %rbp
        movq    -16(%rsp), %rbx
        ret
#endif
