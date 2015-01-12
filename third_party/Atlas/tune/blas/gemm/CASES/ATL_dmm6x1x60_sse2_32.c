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
#ifdef ATL_OS_SunOS
   #define ATL_DIV_NUM MB
   #define ATL_DIV_DEN 6
#endif
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
#ifndef ATL_SSE2
   #error "This routine requires SSE2!"
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
#if !defined(KB) || KB != 60
   #error "KB must be compile-time constant of 60!"
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
   #define rc2     %xmm11
#endif

#define rC0     %xmm0
#define rC1     %xmm1
#define rC2     %xmm2
#define rC3     %xmm3
#define rC4     %xmm4
#define rC5     %xmm5
#define rA0     %xmm6
#define rB0     %xmm7

#define NB6so   (6*KB*8)

/*
 * Prefetch defines
 */
#if 1
   #define pref2(mem) prefetcht1   mem
   #define prefB(mem) prefetcht0   mem
   #define prefC(mem) prefetchw    mem
#else
   #define pref2(mem)
   #define prefB(mem)
   #define prefC(mem)
#endif
#ifdef DCPLX
   #define CMUL(arg_) 2*arg_
#else
   #define CMUL(arg_) arg_
#endif

/*
                      %rdi/4       %rsi/8       %rdx/12          %xmm0/16
 void ATL_AUSERMM(const int M, const int N, const int K, const TYPE alpha,
                       %rcx/24         %r8/28         %r9/32             36
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                        %xmm1/40    16/48          24/52
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
 *      Store incCn = (ldc-NB)*sizeof and BETA to stack
 */
        movl    80(%esp), %eax
                                        movb %al, stM
        subl    $MB, %eax
#ifdef DCPLX
        shl     $4, %eax
#else
        shl     $3, %eax
#endif
        movl    %eax, 8(%esp)
   #ifdef BETAX
        fldl    68(%esp)
        fstpl   (%esp)
      #define BETAOFF 0
   #endif
/*
 *      Initialize pA = A;  pB = B; pC = C;
 */
        movl    76(%esp), pC0
                                        prefC((pC0))
                                        prefC(64(pC0))
        movl    52(%esp), pA0
        movl    60(%esp), pB0
                                        prefB((pB0))
                                        prefB(64(pB0))
        addl    $120, pA0
        addl    $120, pB0
/*
 *      ldab = K * 8;
 */
        movl    40(%esp), ldab
        shl     $3, ldab
/*
 *      pfA = pA + NBNB
 */
        movl    pA0, pfA
        addl    $MB*KB*8-120, pfA
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
        unpcklpd        %xmm1, %xmm1
        movapd  %xmm1, rBETA
/*
 *      On entry, pA0 is correct, ldab has K, stM has M, pC0 has stN; MM has pB0
 *      Set ldab = K*8, stN=N, and MBKB = M*K*8
 */
        shlq    $3, ldab
        movq    pC0, stN
   #if MB == 0
        movq    stM, MBKB
        imulq   ldab, MBKB
   #else
        movq    $MB*KB*8, MBKB
   #endif
/*
 *      Initialize incCn = (ldc-MB)*sizeof; pC0 = C; pB0 = B, pfA = pA0+MBKB
 */
        movl    24(%rsp), %rsi
        movq    %rsi, incCn
        shlq    $32, incCn
        sarq    $32, incCn
        subq    stM, incCn
   #ifdef TCPLX
        shlq    $4, incCn
   #else
        shlq    $3, incCn
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
#ifdef BETAX
   #ifdef X8632
        movsd   (pC0), rC0
        movsd   CMUL(8)(pC0), rC1
        movsd   CMUL(16)(pC0), rC2
        movsd   CMUL(24)(pC0), rC3
        movsd   CMUL(32)(pC0), rC4
        movsd   CMUL(40)(pC0), rC5
        movlpd  BETAOFF(%esp), rA0
        mulsd   rA0, rC0
        mulsd   rA0, rC1
        mulsd   rA0, rC2
        mulsd   rA0, rC3
        mulsd   rA0, rC4
        mulsd   rA0, rC5
   #else
      #ifdef DCPLX
        movlpd  (pC0), rc0
        movhpd  16(pC0), rc0
        movlpd  32(pC0), rc1
        movhpd  48(pC0), rc1
        movlpd  64(pC0), rc1
        movhpd  80(pC0), rc1
      #else
        movupd  (pC0), rc0
        movupd  16(pC0), rc1
        movupd  32(pC0), rc2
      #endif
        mulpd   rBETA, rc0
        mulpd   rBETA, rc1
        mulpd   rBETA, rc2
   #endif
        ALIGN16
#endif

/*KLOOP */
#ifdef BETA1
	movapd	0-120(pA0), rC0
	movapd	0-120(pB0), rB0
	mulpd	rB0, rC0
        addsd   (pC0), rC0
	movapd	0-120(pA0,ldab), rC1
	mulpd	rB0, rC1
        addsd   CMUL(8)(pC0), rC1
	movapd	0-120(pA0,ldab,2), rC2
	mulpd	rB0, rC2
        addsd   CMUL(16)(pC0), rC2
	movapd	0-120(pA3), rC3
	mulpd	rB0, rC3
        addsd   CMUL(24)(pC0), rC3
	movapd	0-120(pA3,ldab), rC4
	mulpd	rB0, rC4
        addsd   CMUL(32)(pC0), rC4
	movapd	0-120(pA3,ldab,2), rC5
	mulpd	rB0, rC5
        addsd   CMUL(40)(pC0), rC5
#elif defined(BETA0) || defined(X8664)
	movapd	0-120(pA0), rC0
	movapd	0-120(pB0), rB0
	mulpd	rB0, rC0
	movapd	0-120(pA0,ldab), rC1
	mulpd	rB0, rC1
	movapd	0-120(pA0,ldab,2), rC2
	mulpd	rB0, rC2
	movapd	0-120(pA3), rC3
	mulpd	rB0, rC3
	movapd	0-120(pA3,ldab), rC4
	mulpd	rB0, rC4
	movapd	0-120(pA3,ldab,2), rC5
	mulpd	rB0, rC5
#else
	movapd	0-120(pA0), rA0
	movapd	0-120(pB0), rB0
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
#endif

	movapd	16-120(pA0), rA0
	movapd	16-120(pB0), rB0
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
                                                        prefC((pC0))
                                                        prefC(64(pC0))

	movapd	32-120(pA0), rA0
	movapd	32-120(pB0), rB0
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

	movapd	48-120(pA0), rA0
	movapd	48-120(pB0), rB0
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

	movapd	64-120(pA0), rA0
	movapd	64-120(pB0), rB0
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

	movapd	80-120(pA0), rA0
	movapd	80-120(pB0), rB0
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

	movapd	96-120(pA0), rA0
	movapd	96-120(pB0), rB0
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

	movapd	112-120(pA0), rA0
	movapd	112-120(pB0), rB0
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

	movapd	128-120(pA0), rA0
	movapd	128-120(pB0), rB0
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

	movapd	144-120(pA0), rA0
	movapd	144-120(pB0), rB0
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

	movapd	160-120(pA0), rA0
	movapd	160-120(pB0), rB0
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

	movapd	176-120(pA0), rA0
	movapd	176-120(pB0), rB0
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

	movapd	192-120(pA0), rA0
	movapd	192-120(pB0), rB0
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

	movapd	208-120(pA0), rA0
	movapd	208-120(pB0), rB0
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

	movapd	224-120(pA0), rA0
	movapd	224-120(pB0), rB0
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

	                                        addl	$240, pA0
                                        	addl	$240, pB0

	movapd	0-120(pA0), rA0
	movapd	0-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	                                        addl	$240, pA3
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

	movapd	16-120(pA0), rA0
	movapd	16-120(pB0), rB0
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

	movapd	32-120(pA0), rA0
	movapd	32-120(pB0), rB0
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

	movapd	48-120(pA0), rA0
	movapd	48-120(pB0), rB0
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

	movapd	64-120(pA0), rA0
	movapd	64-120(pB0), rB0
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

	movapd	80-120(pA0), rA0
	movapd	80-120(pB0), rB0
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

	movapd	96-120(pA0), rA0
	movapd	96-120(pB0), rB0
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

	movapd	112-120(pA0), rA0
	movapd	112-120(pB0), rB0
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

	movapd	128-120(pA0), rA0
	movapd	128-120(pB0), rB0
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

	movapd	144-120(pA0), rA0
	movapd	144-120(pB0), rB0
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

	movapd	160-120(pA0), rA0
	movapd	160-120(pB0), rB0
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

	movapd	176-120(pA0), rA0
	movapd	176-120(pB0), rB0
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

	movapd	192-120(pA0), rA0
	movapd	192-120(pB0), rB0
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

	movapd	208-120(pA0), rA0
	movapd	208-120(pB0), rB0
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

	movapd	224-120(pA0), rA0
	movapd	224-120(pB0), rB0
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

/*ENDKLOOP */
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
                                                prefC(112(pC0))
                                                prefC(176(pC0))
        unpcklpd        rC5, rC4        /* rC4 = c4a  c5a */
        unpckhpd        rC1, rA0        /* rA0 = c0b  c1b */
        unpckhpd        rC5, rB0        /* rB0 = c4b  c5b */
        addpd           rA0, rC0        /* rC0 = c0ab c1ab */
   #if defined(X8632) || !defined(BETAX)
        addpd           rB0, rC4        /* rC4 = c4ab c5ab */
        movapd          rC2, rA0
        unpcklpd        rC3, rC2        /* rC2 = c2a  c3a */
                                                pref2((pfA))
                                                addl    $54, pfA
        unpckhpd        rC3, rA0        /* rA0 = c2b  c3b */
        addpd           rA0, rC2        /* rC2 = c2ab c3ab */
   #else
        addpd           rB0, rC4        /* rC4 = c4ab c5ab */
        movapd          rC2, rA0
                                  addpd rc0, rC0
        unpcklpd        rC3, rC2        /* rC2 = c2a  c3a */
                                  addpd rc2, rC4
                                                pref2((pfA))
                                                addl    $54, pfA
        unpckhpd        rC3, rA0        /* rA0 = c2b  c3b */
        addpd           rA0, rC2        /* rC2 = c2ab c3ab */
                                  addpd rc1, rC2
   #endif
/*
 *      Write results back to C
 */
   #ifdef DCPLX
	movlpd	rC0, (pC0)
	movhpd	rC0, 16(pC0)
	movlpd	rC2, 32(pC0)
	movhpd	rC2, 48(pC0)
	movlpd	rC4, 64(pC0)
	movhpd	rC4, 80(pC0)
   #else
	movupd	rC0, (pC0)
	movupd	rC2, 16(pC0)
	movupd	rC4, 32(pC0)
   #endif
/*
 *      pC += 6;   pA += 6*NB
 */
        addl    $CMUL(48), pC0
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
 *Last iteration of loop unrolled for prefetch of B
 */
#if MB == 0
MLOOPCU:
#endif
#ifdef BETAX
   #ifdef X8632
        movsd   (pC0), rC0
        movsd   CMUL(8)(pC0), rC1
        movsd   CMUL(16)(pC0), rC2
        movsd   CMUL(24)(pC0), rC3
        movsd   CMUL(32)(pC0), rC4
        movsd   CMUL(40)(pC0), rC5
        movlpd  BETAOFF(%esp), rA0
        mulsd   rA0, rC0
        mulsd   rA0, rC1
        mulsd   rA0, rC2
        mulsd   rA0, rC3
        mulsd   rA0, rC4
        mulsd   rA0, rC5
   #else
      #ifdef DCPLX
        movlpd  (pC0), rc0
        movhpd  16(pC0), rc0
        movlpd  32(pC0), rc1
        movhpd  48(pC0), rc1
        movlpd  64(pC0), rc1
        movhpd  80(pC0), rc1
      #else
        movupd  (pC0), rc0
        movupd  16(pC0), rc1
        movupd  32(pC0), rc2
      #endif
        mulpd   rBETA, rc0
        mulpd   rBETA, rc1
        mulpd   rBETA, rc2
   #endif
        ALIGN16
#endif
/*KLOOP */
#ifdef BETA1
	movapd	0-120(pA0), rC0
	movapd	0-120(pB0), rB0
	mulpd	rB0, rC0
        addsd   (pC0), rC0
	movapd	0-120(pA0,ldab), rC1
	mulpd	rB0, rC1
        addsd   CMUL(8)(pC0), rC1
	movapd	0-120(pA0,ldab,2), rC2
	mulpd	rB0, rC2
        addsd   CMUL(16)(pC0), rC2
	movapd	0-120(pA3), rC3
	mulpd	rB0, rC3
        addsd   CMUL(24)(pC0), rC3
	movapd	0-120(pA3,ldab), rC4
	mulpd	rB0, rC4
        addsd   CMUL(32)(pC0), rC4
	movapd	0-120(pA3,ldab,2), rC5
	mulpd	rB0, rC5
        addsd   CMUL(40)(pC0), rC5
#elif defined(BETA0) || defined(X8664)
	movapd	0-120(pA0), rC0
	movapd	0-120(pB0), rB0
	mulpd	rB0, rC0
	movapd	0-120(pA0,ldab), rC1
	mulpd	rB0, rC1
	movapd	0-120(pA0,ldab,2), rC2
	mulpd	rB0, rC2
	movapd	0-120(pA3), rC3
	mulpd	rB0, rC3
	movapd	0-120(pA3,ldab), rC4
	mulpd	rB0, rC4
	movapd	0-120(pA3,ldab,2), rC5
	mulpd	rB0, rC5
#else
	movapd	0-120(pA0), rA0
	movapd	0-120(pB0), rB0
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
#endif
                                                prefB(-120(pB0,ldab))
                                                prefB(64-120(pB0,ldab))

	movapd	16-120(pA0), rA0
	movapd	16-120(pB0), rB0
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

	movapd	32-120(pA0), rA0
	movapd	32-120(pB0), rB0
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

	movapd	48-120(pA0), rA0
	movapd	48-120(pB0), rB0
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

	movapd	64-120(pA0), rA0
	movapd	64-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	64-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
                                                prefB(128-120(pB0,ldab))
                                                prefB(192-120(pB0,ldab))
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

	movapd	80-120(pA0), rA0
	movapd	80-120(pB0), rB0
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

	movapd	96-120(pA0), rA0
	movapd	96-120(pB0), rB0
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

	movapd	112-120(pA0), rA0
	movapd	112-120(pB0), rB0
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

	movapd	128-120(pA0), rA0
	movapd	128-120(pB0), rB0
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

	movapd	144-120(pA0), rA0
	movapd	144-120(pB0), rB0
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

	movapd	160-120(pA0), rA0
	movapd	160-120(pB0), rB0
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

	movapd	176-120(pA0), rA0
	movapd	176-120(pB0), rB0
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

	movapd	192-120(pA0), rA0
	movapd	192-120(pB0), rB0
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

	movapd	208-120(pA0), rA0
	movapd	208-120(pB0), rB0
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

	movapd	224-120(pA0), rA0
	movapd	224-120(pB0), rB0
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

	                                        addl	$240, pA0
	                                        addl	$240, pA3
                                        	addl	$240, pB0

	movapd	0-120(pA0), rA0
	movapd	0-120(pB0), rB0
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
                                                prefB(32-120(pB0,ldab))
                                                prefB(96-120(pB0,ldab))

	movapd	16-120(pA0), rA0
	movapd	16-120(pB0), rB0
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

	movapd	32-120(pA0), rA0
	movapd	32-120(pB0), rB0
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

	movapd	48-120(pA0), rA0
	movapd	48-120(pB0), rB0
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

	movapd	64-120(pA0), rA0
	movapd	64-120(pB0), rB0
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

	movapd	80-120(pA0), rA0
	movapd	80-120(pB0), rB0
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

	movapd	96-120(pA0), rA0
	movapd	96-120(pB0), rB0
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

	movapd	112-120(pA0), rA0
	movapd	112-120(pB0), rB0
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

	movapd	128-120(pA0), rA0
	movapd	128-120(pB0), rB0
	mulpd	rB0, rA0
	addpd	rA0, rC0
	movapd	128-120(pA0,ldab), rA0
	mulpd	rB0, rA0
	addpd	rA0, rC1
                                                prefB(160-120(pB0,ldab))
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

	movapd	144-120(pA0), rA0
	movapd	144-120(pB0), rB0
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

	movapd	160-120(pA0), rA0
	movapd	160-120(pB0), rB0
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

	movapd	176-120(pA0), rA0
	movapd	176-120(pB0), rB0
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

	movapd	192-120(pA0), rA0
	movapd	192-120(pB0), rB0
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

	movapd	208-120(pA0), rA0
	movapd	208-120(pB0), rB0
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

	movapd	224-120(pA0), rA0
	movapd	224-120(pB0), rB0
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

   #ifdef X8632
                                        subl	$MB*KB*8-NB6so+240, pA0
                                        movl	8(%esp), pA3
                                        lea     CMUL(48)(pC0, pA3), pA3
   #else
                                        subq    MBKB, pA0
                                        lea     CMUL(48)(pC0, incCn), pA3
   #endif
/*ENDKLOOP */
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
                                                prefC((pA3))
                                                prefC(64(pA3))
        movapd          rC0, rA0
        movapd          rC4, rB0
        unpcklpd        rC1, rC0        /* rC0 = c0a  c1a */
        unpcklpd        rC5, rC4        /* rC4 = c4a  c5a */
        unpckhpd        rC1, rA0        /* rA0 = c0b  c1b */
        unpckhpd        rC5, rB0        /* rB0 = c4b  c5b */
        addpd           rA0, rC0        /* rC0 = c0ab c1ab */
                                                prefC(128(pA3))
                                                prefC(192(pA3))
#if defined(X8632) || !defined(BETAX)
        addpd           rB0, rC4        /* rC4 = c4ab c5ab */
        movapd          rC2, rA0
        unpcklpd        rC3, rC2        /* rC2 = c2a  c3a */
        unpckhpd        rC3, rA0        /* rA0 = c2b  c3b */
                                                lea     -240(pB0, ldab), pB0
        addpd           rA0, rC2        /* rC2 = c2ab c3ab */
#else
        addpd           rB0, rC4        /* rC4 = c4ab c5ab */
                                                lea     -240(pB0, ldab), pB0
        movapd          rC2, rA0
                                  addpd rc0, rC0
        unpcklpd        rC3, rC2        /* rC2 = c2a  c3a */
                                  addpd rc2, rC4
        unpckhpd        rC3, rA0        /* rA0 = c2b  c3b */
        addpd           rA0, rC2        /* rC2 = c2ab c3ab */
                                  addpd rc1, rC2
#endif
/*
 *      Write results back to C
 */
   #ifdef DCPLX
	movlpd	rC0, (pC0)
	movhpd	rC0, 16(pC0)
	movlpd	rC2, 32(pC0)
	movhpd	rC2, 48(pC0)
	movlpd	rC4, 64(pC0)
	movhpd	rC4, 80(pC0)
   #else
	movupd	rC0, (pC0)
	movupd	rC2, 16(pC0)
	movupd	rC4, 32(pC0)
   #endif
/*
 *      pC += 6;   pA += 6*NB
 */
/*	jnz 	MLOOPCU */
/*
 *      Increment pointers, and continue N-LOOP
 */
        movl    pA3, pC0
   #ifdef X8632
	subb	$1, stN
   #else
        subq    $1, stN
   #endif
	jnz	NLOOP

#DONE:
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
