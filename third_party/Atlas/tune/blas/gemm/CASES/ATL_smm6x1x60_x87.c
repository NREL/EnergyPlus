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


/*
 * The basic outline of this file came from the x87 kernel I wrote for the
 * Hammer processor.  However, the key to good athlon performance comes from
 * instruction alignment, and I got this key from Julian Ruhe's explanation
 * of athlon optimization tips.
 */
#ifndef ATL_GAS_x8632
   #error "This kernel requires a gas x86 assembler!"
#endif
#define BOFF 120

#if !defined(MB) || (MB == 0)
   #error "MB must be a compile-time constant!"
#endif
#if !defined(KB) || (KB == 0)
   #error "KB must be a compile-time constant!"
#endif
#if (KB > 60)
   #error "KB must less than 61!"
#endif
#if (MB/6)*6 != MB
   #error "MB must be multiple of 6!"
#endif
/*
 * Integer register usage shown be these defines
 */
#ifdef ATL_GAS_x8632
   #define pC0     %esi
   #define pA0     %ecx
   #define pA1     %eax
   #define pB0     %edi
   #define ldab    %edx
   #define pfA     %ebp
   #define stN     %bh
/*   #define stM     %bl */
/* lower 16 bits of %ebx used for M & N loop counters */
/* incCn overwrites pA1 */
#endif

/*
 * Prefetch defines
 */
#if defined(ATL_SSE1) || defined(ATL_SSE2)
   #define pref2(mem) prefetcht1   mem
   #define prefB(mem) prefetcht0   mem
   #ifdef ATL_3DNow
      #define prefC(mem) prefetchw  mem
   #else
      #define prefC(mem) prefetchnta  mem
   #endif
#elif defined(ATL_3DNow)
   #define pref2(mem) prefetch   mem
   #define prefB(mem) prefetch   mem
   #define prefC(mem) prefetchw  mem
#else
   #define pref2(mem)
   #define prefB(mem)
   #define prefC(mem)
#endif
#ifdef SCPLX
   #define CMUL(arg_) (2*(arg_))
#else
   #define CMUL(arg_) arg_
#endif
/*
                           4            8           12                16
 void ATL_AUSERMM(const int M, const int N, const int K, const TYPE alpha,
                            20             24             28             32
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                              36       40             44
                 const TYPE beta, TYPE *C, const int ldc)
*/
.text
.global ATL_asmdecor(ATL_USERMM)
ALIGN16
ATL_asmdecor(ATL_USERMM):
        subl    $28, %esp
        movl    %ebp, 24(%esp)
        movl    %ebx, 20(%esp)
        movl    %esi, 16(%esp)
        movl    %edi, 12(%esp)
/*
 *      Store incCn = (ldc-NB)*sizeof and BETA to stack
 */
        movl    72(%esp), %eax
        subl    $MB-6, %eax
#ifdef SCPLX
        shl     $3, %eax
#else
        shl     $2, %eax
#endif
        movl    %eax, 8(%esp)
   #ifdef BETAX
        movl    64(%esp), %eax
        movl    %eax, (%esp)
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
        addl    $BOFF, pA0
        addl    $BOFF, pB0
/*
 *      ldab = K * 8;
 */
        movl    40(%esp), ldab
        shl     $2, ldab
        movl    $KB*4, ldab
/*
 *      pfA = pA + NBNB
 */
        movl    pA0, pfA
        addl    $MB*KB*4, pfA
                                        prefB((pB0))
                                        prefB(64(pB0))
        movb    36(%esp), stN
        lea     0(pA0, ldab), pA1
        ALIGN16
NLOOP:
/*        movb    $MB/6-1, stM */
#if (MB > 6)
        ALIGN16
/*MLOOP: */
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
/*KLOOP: */
#ifdef BETA0
	flds	0-BOFF(pB0)
	flds    0-BOFF(pA1,ldab,4)
	fmul	%st(1), %st
        fxch
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fmuls	0-BOFF(pA0)
        ALIGN8
#elif defined(BETA1)
	flds	0-BOFF(pB0)
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        fadds   CMUL(16)(pC0)
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(2), %st
        fadds   CMUL(12)(pC0)
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(3), %st
        fadds   CMUL(8)(pC0)
	flds	0-BOFF(pA0,ldab)
	fmul	%st(4), %st
        fadds   CMUL(4)(pC0)
	flds	0-BOFF(pA0)
	fmul	%st(5), %st
        fadds   0(pC0)
	flds    0-BOFF(pA1,ldab,4)
	fmul	%st(6), %st
        fadds   CMUL(20)(pC0)
        fstp    %st(6)
#else
        flds    BETAOFF(%esp)
        flds    CMUL(16)(pC0)
        fmul    %st(1), %st

        flds    CMUL(12)(pC0)
        fmul    %st(2), %st
        flds    CMUL(8)(pC0)
        fmul    %st(3), %st
        flds    CMUL(4)(pC0)
        fmul    %st(4), %st
        ALIGN8
        flds    0(pC0)
        fmul    %st(5), %st
        flds    CMUL(20)(pC0)
        fmul    %st(6), %st
        fxch    %st(6)
        fstp    %st
        ALIGN8
	flds	0-BOFF(pB0)
	flds	0-BOFF(pA0)
	fmul	%st(1), %st

	faddp	%st, %st(2)
	flds	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(3)
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(4)
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(5)
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(6)
	fmuls   0-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
        ALIGN8
#endif

#if (KB > 1)
	flds	4-BOFF(pB0)
	flds	4-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	4-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	4-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	4-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	4-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	4-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 2)
	flds	8-BOFF(pB0)
	flds	8-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	8-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	8-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	8-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	8-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	8-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 3)
	flds	12-BOFF(pB0)
	flds	12-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	12-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	12-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	12-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	12-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	12-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 4)
	flds	16-BOFF(pB0)
	flds	16-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	16-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	16-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	16-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	16-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	16-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 5)
	flds	20-BOFF(pB0)
	flds	20-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	20-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	20-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	20-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	20-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	20-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 6)
	flds	24-BOFF(pB0)
	flds	24-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	24-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	24-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	24-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	24-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	24-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 7)
	flds	28-BOFF(pB0)
	flds	28-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	28-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	28-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	28-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	28-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	28-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 8)
	flds	32-BOFF(pB0)
	flds	32-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	32-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	32-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	32-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	32-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	32-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 9)
	flds	36-BOFF(pB0)
	flds	36-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	36-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	36-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	36-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	36-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	36-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 10)
	flds	40-BOFF(pB0)
	flds	40-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	40-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	40-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	40-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	40-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	40-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 11)
	flds	44-BOFF(pB0)
	flds	44-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	44-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	44-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	44-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	44-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	44-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 12)
	flds	48-BOFF(pB0)
	flds	48-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	48-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	48-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	48-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	48-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	48-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 13)
	flds	52-BOFF(pB0)
	flds	52-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	52-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	52-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	52-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	52-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	52-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 14)
	flds	56-BOFF(pB0)
	flds	56-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	56-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	56-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	56-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	56-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	56-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 15)
	flds	60-BOFF(pB0)
	flds	60-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	60-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	60-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	60-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	60-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	60-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 16)
	flds	64-BOFF(pB0)
	flds	64-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	64-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	64-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	64-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	64-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	64-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 17)
	flds	68-BOFF(pB0)
	flds	68-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	68-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	68-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	68-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	68-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	68-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 18)
	flds	72-BOFF(pB0)
	flds	72-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	72-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	72-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	72-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	72-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	72-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 19)
	flds	76-BOFF(pB0)
	flds	76-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	76-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	76-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	76-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	76-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	76-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 20)
	flds	80-BOFF(pB0)
	flds	80-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	80-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	80-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	80-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	80-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	80-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 21)
	flds	84-BOFF(pB0)
	flds	84-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	84-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	84-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	84-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	84-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	84-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 22)
	flds	88-BOFF(pB0)
	flds	88-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	88-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	88-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	88-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	88-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	88-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 23)
	flds	92-BOFF(pB0)
	flds	92-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	92-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	92-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	92-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	92-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	92-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 24)
	flds	96-BOFF(pB0)
	flds	96-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	96-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	96-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	96-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	96-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	96-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 25)
	flds	100-BOFF(pB0)
	flds	100-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	100-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	100-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	100-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	100-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	100-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 26)
	flds	104-BOFF(pB0)
	flds	104-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	104-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	104-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	104-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	104-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	104-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 27)
	flds	108-BOFF(pB0)
	flds	108-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	108-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	108-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	108-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	108-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	108-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 28)
	flds	112-BOFF(pB0)
	flds	112-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	112-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	112-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	112-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	112-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	112-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 29)
	flds	116-BOFF(pB0)
	flds	116-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	116-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	116-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	116-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	116-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	116-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 30)
	flds	120-BOFF(pB0)
	flds	120-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	120-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	120-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	120-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	120-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	120-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 31)
	flds	124-BOFF(pB0)
	flds	124-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	124-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	124-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	124-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	124-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	124-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 32)
	flds	128-BOFF(pB0)
	flds	128-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	128-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	128-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	128-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	128-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	128-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 33)
	flds	132-BOFF(pB0)
	flds	132-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	132-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	132-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	132-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	132-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	132-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 34)
	flds	136-BOFF(pB0)
	flds	136-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	136-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	136-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	136-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	136-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	136-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
/*	ALIGN8 */
#endif
        pref2(-BOFF(pfA))

#if (KB > 35)
	flds	140-BOFF(pB0)
	flds	140-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	140-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	140-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	140-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	140-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	140-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 36)
	flds	144-BOFF(pB0)
	flds	144-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	144-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	144-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	144-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	144-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	144-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 37)
	flds	148-BOFF(pB0)
	flds	148-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	148-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	148-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	148-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	148-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	148-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 38)
	flds	152-BOFF(pB0)
	flds	152-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	152-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	152-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	152-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	152-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	152-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 39)
	flds	156-BOFF(pB0)
	flds	156-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	156-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	156-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	156-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	156-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	156-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 40)
	flds	160-BOFF(pB0)
	flds	160-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	160-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	160-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	160-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	160-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	160-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 41)
	flds	164-BOFF(pB0)
	flds	164-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	164-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	164-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	164-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	164-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	164-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 42)
	flds	168-BOFF(pB0)
	flds	168-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	168-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	168-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	168-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	168-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	168-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 43)
	flds	172-BOFF(pB0)
	flds	172-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	172-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	172-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	172-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	172-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	172-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 44)
	flds	176-BOFF(pB0)
	flds	176-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	176-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	176-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	176-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	176-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	176-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif
        prefC(CMUL(24)(pC0));
        prefC(CMUL(24)+64(pC0));
        ALIGN8

#if (KB > 45)
	flds	180-BOFF(pB0)
	flds	180-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	180-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	180-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	180-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	180-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	180-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 46)
	flds	184-BOFF(pB0)
	flds	184-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	184-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	184-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	184-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	184-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	184-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 47)
	flds	188-BOFF(pB0)
	flds	188-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	188-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	188-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	188-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	188-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	188-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 48)
	flds	192-BOFF(pB0)
	flds	192-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	192-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	192-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	192-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	192-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	192-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 49)
	flds	196-BOFF(pB0)
	flds	196-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	196-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	196-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	196-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	196-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	196-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 50)
	flds	200-BOFF(pB0)
	flds	200-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	200-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	200-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	200-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	200-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	200-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 51)
	flds	204-BOFF(pB0)
	flds	204-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	204-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	204-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	204-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	204-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	204-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 52)
	flds	208-BOFF(pB0)
	flds	208-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	208-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	208-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	208-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	208-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	208-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 53)
	flds	212-BOFF(pB0)
	flds	212-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	212-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	212-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	212-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	212-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	212-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 54)
	flds	216-BOFF(pB0)
	flds	216-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	216-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	216-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	216-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	216-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	216-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 55)
	flds	220-BOFF(pB0)
	flds	220-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	220-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	220-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	220-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	220-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	220-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 56)
	flds	224-BOFF(pB0)
	flds	224-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	224-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	224-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	224-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	224-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	224-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 57)
	flds	228-BOFF(pB0)
	flds	228-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	228-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	228-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	228-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	228-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	228-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 58)
	flds	232-BOFF(pB0)
	flds	232-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	232-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	232-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	232-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	232-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	232-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 59)
	flds	236-BOFF(pB0)
	flds	236-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	236-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	236-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	236-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	236-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	236-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
/*	ALIGN8 */
#endif

/*
 *End KLOOP
 */
/*
 *      Write results back to C
 */
        fstps   0(pC0)
        fstps   CMUL(4)(pC0)
                lea     (pA1, ldab, 4), pA0
        ALIGN8
        fstps   CMUL(8)(pC0)
        fstps   CMUL(12)(pC0)
                lea     (pA0, ldab, 2), pA1
        ALIGN8
        fstps   CMUL(16)(pC0)
        fstps   CMUL(20)(pC0)
        addl    ldab, pA0
        ALIGN8
        addl    $CMUL(24), pC0
#endif
#if (MB > 12)
/*KLOOP: */
#ifdef BETA0
	flds	0-BOFF(pB0)
	flds    0-BOFF(pA1,ldab,4)
	fmul	%st(1), %st
        fxch
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fmuls	0-BOFF(pA0)
        ALIGN8
#elif defined(BETA1)
	flds	0-BOFF(pB0)
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        fadds   CMUL(16)(pC0)
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(2), %st
        fadds   CMUL(12)(pC0)
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(3), %st
        fadds   CMUL(8)(pC0)
	flds	0-BOFF(pA0,ldab)
	fmul	%st(4), %st
        fadds   CMUL(4)(pC0)
	flds	0-BOFF(pA0)
	fmul	%st(5), %st
        fadds   0(pC0)
	flds    0-BOFF(pA1,ldab,4)
	fmul	%st(6), %st
        fadds   CMUL(20)(pC0)
        fstp    %st(6)
#else
        flds    BETAOFF(%esp)
        flds    CMUL(16)(pC0)
        fmul    %st(1), %st

        flds    CMUL(12)(pC0)
        fmul    %st(2), %st
        flds    CMUL(8)(pC0)
        fmul    %st(3), %st
        flds    CMUL(4)(pC0)
        fmul    %st(4), %st
        ALIGN8
        flds    0(pC0)
        fmul    %st(5), %st
        flds    CMUL(20)(pC0)
        fmul    %st(6), %st
        fxch    %st(6)
        fstp    %st
        ALIGN8
	flds	0-BOFF(pB0)
	flds	0-BOFF(pA0)
	fmul	%st(1), %st

	faddp	%st, %st(2)
	flds	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(3)
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(4)
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(5)
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(6)
	fmuls   0-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
        ALIGN8
#endif

#if (KB > 1)
	flds	4-BOFF(pB0)
	flds	4-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	4-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	4-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	4-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	4-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	4-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 2)
	flds	8-BOFF(pB0)
	flds	8-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	8-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	8-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	8-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	8-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	8-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 3)
	flds	12-BOFF(pB0)
	flds	12-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	12-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	12-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	12-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	12-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	12-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 4)
	flds	16-BOFF(pB0)
	flds	16-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	16-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	16-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	16-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	16-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	16-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 5)
	flds	20-BOFF(pB0)
	flds	20-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	20-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	20-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	20-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	20-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	20-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 6)
	flds	24-BOFF(pB0)
	flds	24-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	24-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	24-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	24-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	24-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	24-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 7)
	flds	28-BOFF(pB0)
	flds	28-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	28-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	28-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	28-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	28-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	28-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 8)
	flds	32-BOFF(pB0)
	flds	32-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	32-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	32-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	32-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	32-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	32-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 9)
	flds	36-BOFF(pB0)
	flds	36-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	36-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	36-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	36-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	36-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	36-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 10)
	flds	40-BOFF(pB0)
	flds	40-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	40-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	40-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	40-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	40-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	40-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 11)
	flds	44-BOFF(pB0)
	flds	44-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	44-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	44-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	44-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	44-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	44-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 12)
	flds	48-BOFF(pB0)
	flds	48-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	48-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	48-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	48-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	48-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	48-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 13)
	flds	52-BOFF(pB0)
	flds	52-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	52-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	52-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	52-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	52-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	52-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 14)
	flds	56-BOFF(pB0)
	flds	56-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	56-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	56-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	56-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	56-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	56-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 15)
	flds	60-BOFF(pB0)
	flds	60-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	60-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	60-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	60-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	60-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	60-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 16)
	flds	64-BOFF(pB0)
	flds	64-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	64-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	64-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	64-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	64-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	64-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 17)
	flds	68-BOFF(pB0)
	flds	68-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	68-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	68-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	68-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	68-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	68-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 18)
	flds	72-BOFF(pB0)
	flds	72-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	72-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	72-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	72-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	72-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	72-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 19)
	flds	76-BOFF(pB0)
	flds	76-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	76-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	76-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	76-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	76-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	76-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 20)
	flds	80-BOFF(pB0)
	flds	80-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	80-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	80-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	80-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	80-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	80-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 21)
	flds	84-BOFF(pB0)
	flds	84-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	84-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	84-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	84-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	84-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	84-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 22)
	flds	88-BOFF(pB0)
	flds	88-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	88-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	88-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	88-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	88-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	88-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 23)
	flds	92-BOFF(pB0)
	flds	92-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	92-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	92-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	92-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	92-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	92-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 24)
	flds	96-BOFF(pB0)
	flds	96-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	96-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	96-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	96-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	96-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	96-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 25)
	flds	100-BOFF(pB0)
	flds	100-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	100-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	100-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	100-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	100-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	100-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 26)
	flds	104-BOFF(pB0)
	flds	104-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	104-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	104-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	104-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	104-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	104-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 27)
	flds	108-BOFF(pB0)
	flds	108-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	108-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	108-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	108-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	108-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	108-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 28)
	flds	112-BOFF(pB0)
	flds	112-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	112-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	112-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	112-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	112-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	112-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 29)
	flds	116-BOFF(pB0)
	flds	116-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	116-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	116-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	116-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	116-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	116-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 30)
	flds	120-BOFF(pB0)
	flds	120-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	120-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	120-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	120-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	120-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	120-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 31)
	flds	124-BOFF(pB0)
	flds	124-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	124-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	124-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	124-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	124-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	124-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 32)
	flds	128-BOFF(pB0)
	flds	128-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	128-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	128-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	128-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	128-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	128-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 33)
	flds	132-BOFF(pB0)
	flds	132-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	132-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	132-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	132-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	132-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	132-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 34)
	flds	136-BOFF(pB0)
	flds	136-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	136-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	136-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	136-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	136-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	136-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
/*	ALIGN8 */
#endif
        pref2(64-BOFF(pfA))

#if (KB > 35)
	flds	140-BOFF(pB0)
	flds	140-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	140-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	140-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	140-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	140-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	140-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 36)
	flds	144-BOFF(pB0)
	flds	144-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	144-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	144-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	144-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	144-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	144-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 37)
	flds	148-BOFF(pB0)
	flds	148-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	148-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	148-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	148-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	148-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	148-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 38)
	flds	152-BOFF(pB0)
	flds	152-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	152-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	152-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	152-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	152-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	152-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 39)
	flds	156-BOFF(pB0)
	flds	156-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	156-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	156-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	156-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	156-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	156-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 40)
	flds	160-BOFF(pB0)
	flds	160-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	160-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	160-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	160-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	160-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	160-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 41)
	flds	164-BOFF(pB0)
	flds	164-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	164-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	164-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	164-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	164-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	164-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 42)
	flds	168-BOFF(pB0)
	flds	168-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	168-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	168-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	168-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	168-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	168-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 43)
	flds	172-BOFF(pB0)
	flds	172-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	172-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	172-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	172-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	172-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	172-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 44)
	flds	176-BOFF(pB0)
	flds	176-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	176-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	176-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	176-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	176-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	176-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif
        prefC(CMUL(24)(pC0));
        prefC(CMUL(24)+64(pC0));
        ALIGN8

#if (KB > 45)
	flds	180-BOFF(pB0)
	flds	180-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	180-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	180-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	180-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	180-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	180-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 46)
	flds	184-BOFF(pB0)
	flds	184-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	184-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	184-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	184-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	184-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	184-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 47)
	flds	188-BOFF(pB0)
	flds	188-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	188-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	188-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	188-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	188-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	188-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 48)
	flds	192-BOFF(pB0)
	flds	192-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	192-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	192-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	192-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	192-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	192-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 49)
	flds	196-BOFF(pB0)
	flds	196-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	196-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	196-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	196-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	196-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	196-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 50)
	flds	200-BOFF(pB0)
	flds	200-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	200-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	200-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	200-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	200-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	200-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 51)
	flds	204-BOFF(pB0)
	flds	204-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	204-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	204-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	204-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	204-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	204-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 52)
	flds	208-BOFF(pB0)
	flds	208-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	208-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	208-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	208-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	208-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	208-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 53)
	flds	212-BOFF(pB0)
	flds	212-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	212-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	212-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	212-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	212-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	212-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 54)
	flds	216-BOFF(pB0)
	flds	216-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	216-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	216-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	216-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	216-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	216-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 55)
	flds	220-BOFF(pB0)
	flds	220-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	220-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	220-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	220-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	220-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	220-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 56)
	flds	224-BOFF(pB0)
	flds	224-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	224-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	224-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	224-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	224-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	224-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 57)
	flds	228-BOFF(pB0)
	flds	228-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	228-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	228-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	228-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	228-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	228-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 58)
	flds	232-BOFF(pB0)
	flds	232-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	232-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	232-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	232-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	232-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	232-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 59)
	flds	236-BOFF(pB0)
	flds	236-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	236-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	236-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	236-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	236-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	236-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
/*	ALIGN8 */
#endif

/*
 *End KLOOP
 */
/*
 *      Write results back to C
 */
        fstps   0(pC0)
        fstps   CMUL(4)(pC0)
                lea     (pA1, ldab, 4), pA0
        ALIGN8
        fstps   CMUL(8)(pC0)
        fstps   CMUL(12)(pC0)
                lea     (pA0, ldab, 2), pA1
        ALIGN8
        fstps   CMUL(16)(pC0)
        fstps   CMUL(20)(pC0)
        addl    ldab, pA0
        ALIGN8
        addl    $CMUL(24), pC0
#endif
#if (MB > 18)
/*KLOOP: */
#ifdef BETA0
	flds	0-BOFF(pB0)
	flds    0-BOFF(pA1,ldab,4)
	fmul	%st(1), %st
        fxch
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fmuls	0-BOFF(pA0)
        ALIGN8
#elif defined(BETA1)
	flds	0-BOFF(pB0)
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        fadds   CMUL(16)(pC0)
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(2), %st
        fadds   CMUL(12)(pC0)
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(3), %st
        fadds   CMUL(8)(pC0)
	flds	0-BOFF(pA0,ldab)
	fmul	%st(4), %st
        fadds   CMUL(4)(pC0)
	flds	0-BOFF(pA0)
	fmul	%st(5), %st
        fadds   0(pC0)
	flds    0-BOFF(pA1,ldab,4)
	fmul	%st(6), %st
        fadds   CMUL(20)(pC0)
        fstp    %st(6)
#else
        flds    BETAOFF(%esp)
        flds    CMUL(16)(pC0)
        fmul    %st(1), %st

        flds    CMUL(12)(pC0)
        fmul    %st(2), %st
        flds    CMUL(8)(pC0)
        fmul    %st(3), %st
        flds    CMUL(4)(pC0)
        fmul    %st(4), %st
        ALIGN8
        flds    0(pC0)
        fmul    %st(5), %st
        flds    CMUL(20)(pC0)
        fmul    %st(6), %st
        fxch    %st(6)
        fstp    %st
        ALIGN8
	flds	0-BOFF(pB0)
	flds	0-BOFF(pA0)
	fmul	%st(1), %st

	faddp	%st, %st(2)
	flds	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(3)
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(4)
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(5)
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(6)
	fmuls   0-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
        ALIGN8
#endif

#if (KB > 1)
	flds	4-BOFF(pB0)
	flds	4-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	4-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	4-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	4-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	4-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	4-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 2)
	flds	8-BOFF(pB0)
	flds	8-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	8-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	8-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	8-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	8-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	8-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 3)
	flds	12-BOFF(pB0)
	flds	12-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	12-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	12-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	12-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	12-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	12-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 4)
	flds	16-BOFF(pB0)
	flds	16-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	16-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	16-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	16-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	16-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	16-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 5)
	flds	20-BOFF(pB0)
	flds	20-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	20-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	20-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	20-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	20-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	20-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 6)
	flds	24-BOFF(pB0)
	flds	24-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	24-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	24-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	24-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	24-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	24-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 7)
	flds	28-BOFF(pB0)
	flds	28-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	28-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	28-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	28-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	28-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	28-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 8)
	flds	32-BOFF(pB0)
	flds	32-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	32-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	32-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	32-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	32-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	32-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 9)
	flds	36-BOFF(pB0)
	flds	36-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	36-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	36-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	36-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	36-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	36-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 10)
	flds	40-BOFF(pB0)
	flds	40-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	40-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	40-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	40-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	40-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	40-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 11)
	flds	44-BOFF(pB0)
	flds	44-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	44-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	44-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	44-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	44-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	44-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 12)
	flds	48-BOFF(pB0)
	flds	48-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	48-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	48-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	48-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	48-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	48-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 13)
	flds	52-BOFF(pB0)
	flds	52-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	52-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	52-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	52-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	52-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	52-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 14)
	flds	56-BOFF(pB0)
	flds	56-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	56-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	56-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	56-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	56-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	56-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 15)
	flds	60-BOFF(pB0)
	flds	60-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	60-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	60-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	60-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	60-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	60-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 16)
	flds	64-BOFF(pB0)
	flds	64-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	64-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	64-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	64-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	64-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	64-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 17)
	flds	68-BOFF(pB0)
	flds	68-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	68-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	68-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	68-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	68-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	68-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 18)
	flds	72-BOFF(pB0)
	flds	72-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	72-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	72-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	72-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	72-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	72-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 19)
	flds	76-BOFF(pB0)
	flds	76-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	76-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	76-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	76-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	76-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	76-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 20)
	flds	80-BOFF(pB0)
	flds	80-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	80-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	80-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	80-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	80-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	80-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 21)
	flds	84-BOFF(pB0)
	flds	84-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	84-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	84-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	84-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	84-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	84-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 22)
	flds	88-BOFF(pB0)
	flds	88-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	88-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	88-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	88-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	88-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	88-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 23)
	flds	92-BOFF(pB0)
	flds	92-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	92-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	92-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	92-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	92-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	92-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 24)
	flds	96-BOFF(pB0)
	flds	96-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	96-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	96-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	96-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	96-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	96-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 25)
	flds	100-BOFF(pB0)
	flds	100-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	100-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	100-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	100-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	100-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	100-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 26)
	flds	104-BOFF(pB0)
	flds	104-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	104-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	104-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	104-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	104-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	104-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 27)
	flds	108-BOFF(pB0)
	flds	108-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	108-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	108-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	108-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	108-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	108-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 28)
	flds	112-BOFF(pB0)
	flds	112-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	112-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	112-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	112-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	112-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	112-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 29)
	flds	116-BOFF(pB0)
	flds	116-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	116-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	116-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	116-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	116-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	116-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 30)
	flds	120-BOFF(pB0)
	flds	120-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	120-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	120-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	120-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	120-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	120-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 31)
	flds	124-BOFF(pB0)
	flds	124-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	124-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	124-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	124-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	124-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	124-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 32)
	flds	128-BOFF(pB0)
	flds	128-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	128-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	128-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	128-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	128-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	128-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 33)
	flds	132-BOFF(pB0)
	flds	132-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	132-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	132-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	132-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	132-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	132-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 34)
	flds	136-BOFF(pB0)
	flds	136-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	136-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	136-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	136-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	136-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	136-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
/*	ALIGN8 */
#endif
        pref2(128-BOFF(pfA))

#if (KB > 35)
	flds	140-BOFF(pB0)
	flds	140-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	140-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	140-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	140-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	140-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	140-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 36)
	flds	144-BOFF(pB0)
	flds	144-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	144-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	144-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	144-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	144-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	144-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 37)
	flds	148-BOFF(pB0)
	flds	148-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	148-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	148-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	148-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	148-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	148-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 38)
	flds	152-BOFF(pB0)
	flds	152-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	152-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	152-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	152-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	152-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	152-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 39)
	flds	156-BOFF(pB0)
	flds	156-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	156-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	156-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	156-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	156-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	156-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 40)
	flds	160-BOFF(pB0)
	flds	160-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	160-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	160-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	160-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	160-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	160-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 41)
	flds	164-BOFF(pB0)
	flds	164-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	164-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	164-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	164-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	164-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	164-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 42)
	flds	168-BOFF(pB0)
	flds	168-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	168-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	168-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	168-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	168-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	168-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 43)
	flds	172-BOFF(pB0)
	flds	172-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	172-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	172-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	172-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	172-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	172-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 44)
	flds	176-BOFF(pB0)
	flds	176-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	176-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	176-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	176-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	176-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	176-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif
        prefC(CMUL(24)(pC0));
        prefC(CMUL(24)+64(pC0));
        ALIGN8

#if (KB > 45)
	flds	180-BOFF(pB0)
	flds	180-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	180-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	180-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	180-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	180-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	180-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 46)
	flds	184-BOFF(pB0)
	flds	184-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	184-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	184-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	184-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	184-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	184-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 47)
	flds	188-BOFF(pB0)
	flds	188-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	188-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	188-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	188-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	188-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	188-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 48)
	flds	192-BOFF(pB0)
	flds	192-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	192-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	192-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	192-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	192-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	192-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 49)
	flds	196-BOFF(pB0)
	flds	196-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	196-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	196-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	196-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	196-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	196-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 50)
	flds	200-BOFF(pB0)
	flds	200-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	200-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	200-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	200-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	200-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	200-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 51)
	flds	204-BOFF(pB0)
	flds	204-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	204-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	204-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	204-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	204-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	204-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 52)
	flds	208-BOFF(pB0)
	flds	208-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	208-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	208-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	208-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	208-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	208-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 53)
	flds	212-BOFF(pB0)
	flds	212-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	212-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	212-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	212-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	212-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	212-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 54)
	flds	216-BOFF(pB0)
	flds	216-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	216-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	216-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	216-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	216-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	216-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 55)
	flds	220-BOFF(pB0)
	flds	220-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	220-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	220-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	220-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	220-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	220-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 56)
	flds	224-BOFF(pB0)
	flds	224-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	224-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	224-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	224-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	224-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	224-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 57)
	flds	228-BOFF(pB0)
	flds	228-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	228-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	228-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	228-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	228-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	228-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 58)
	flds	232-BOFF(pB0)
	flds	232-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	232-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	232-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	232-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	232-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	232-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 59)
	flds	236-BOFF(pB0)
	flds	236-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	236-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	236-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	236-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	236-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	236-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
/*	ALIGN8 */
#endif

/*
 *End KLOOP
 */
/*
 *      Write results back to C
 */
        fstps   0(pC0)
        fstps   CMUL(4)(pC0)
                lea     (pA1, ldab, 4), pA0
        ALIGN8
        fstps   CMUL(8)(pC0)
        fstps   CMUL(12)(pC0)
                lea     (pA0, ldab, 2), pA1
        ALIGN8
        fstps   CMUL(16)(pC0)
        fstps   CMUL(20)(pC0)
        addl    ldab, pA0
        ALIGN8
        addl    $CMUL(24), pC0
#endif
#if (MB > 24)
/*KLOOP: */
#ifdef BETA0
	flds	0-BOFF(pB0)
	flds    0-BOFF(pA1,ldab,4)
	fmul	%st(1), %st
        fxch
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fmuls	0-BOFF(pA0)
        ALIGN8
#elif defined(BETA1)
	flds	0-BOFF(pB0)
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        fadds   CMUL(16)(pC0)
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(2), %st
        fadds   CMUL(12)(pC0)
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(3), %st
        fadds   CMUL(8)(pC0)
	flds	0-BOFF(pA0,ldab)
	fmul	%st(4), %st
        fadds   CMUL(4)(pC0)
	flds	0-BOFF(pA0)
	fmul	%st(5), %st
        fadds   0(pC0)
	flds    0-BOFF(pA1,ldab,4)
	fmul	%st(6), %st
        fadds   CMUL(20)(pC0)
        fstp    %st(6)
#else
        flds    BETAOFF(%esp)
        flds    CMUL(16)(pC0)
        fmul    %st(1), %st

        flds    CMUL(12)(pC0)
        fmul    %st(2), %st
        flds    CMUL(8)(pC0)
        fmul    %st(3), %st
        flds    CMUL(4)(pC0)
        fmul    %st(4), %st
        ALIGN8
        flds    0(pC0)
        fmul    %st(5), %st
        flds    CMUL(20)(pC0)
        fmul    %st(6), %st
        fxch    %st(6)
        fstp    %st
        ALIGN8
	flds	0-BOFF(pB0)
	flds	0-BOFF(pA0)
	fmul	%st(1), %st

	faddp	%st, %st(2)
	flds	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(3)
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(4)
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(5)
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(6)
	fmuls   0-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
        ALIGN8
#endif

#if (KB > 1)
	flds	4-BOFF(pB0)
	flds	4-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	4-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	4-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	4-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	4-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	4-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 2)
	flds	8-BOFF(pB0)
	flds	8-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	8-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	8-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	8-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	8-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	8-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 3)
	flds	12-BOFF(pB0)
	flds	12-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	12-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	12-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	12-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	12-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	12-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 4)
	flds	16-BOFF(pB0)
	flds	16-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	16-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	16-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	16-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	16-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	16-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 5)
	flds	20-BOFF(pB0)
	flds	20-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	20-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	20-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	20-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	20-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	20-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 6)
	flds	24-BOFF(pB0)
	flds	24-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	24-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	24-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	24-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	24-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	24-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 7)
	flds	28-BOFF(pB0)
	flds	28-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	28-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	28-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	28-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	28-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	28-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 8)
	flds	32-BOFF(pB0)
	flds	32-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	32-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	32-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	32-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	32-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	32-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 9)
	flds	36-BOFF(pB0)
	flds	36-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	36-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	36-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	36-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	36-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	36-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 10)
	flds	40-BOFF(pB0)
	flds	40-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	40-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	40-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	40-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	40-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	40-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 11)
	flds	44-BOFF(pB0)
	flds	44-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	44-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	44-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	44-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	44-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	44-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 12)
	flds	48-BOFF(pB0)
	flds	48-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	48-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	48-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	48-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	48-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	48-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 13)
	flds	52-BOFF(pB0)
	flds	52-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	52-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	52-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	52-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	52-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	52-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 14)
	flds	56-BOFF(pB0)
	flds	56-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	56-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	56-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	56-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	56-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	56-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 15)
	flds	60-BOFF(pB0)
	flds	60-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	60-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	60-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	60-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	60-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	60-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 16)
	flds	64-BOFF(pB0)
	flds	64-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	64-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	64-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	64-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	64-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	64-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 17)
	flds	68-BOFF(pB0)
	flds	68-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	68-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	68-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	68-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	68-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	68-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 18)
	flds	72-BOFF(pB0)
	flds	72-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	72-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	72-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	72-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	72-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	72-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 19)
	flds	76-BOFF(pB0)
	flds	76-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	76-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	76-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	76-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	76-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	76-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 20)
	flds	80-BOFF(pB0)
	flds	80-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	80-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	80-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	80-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	80-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	80-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 21)
	flds	84-BOFF(pB0)
	flds	84-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	84-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	84-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	84-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	84-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	84-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 22)
	flds	88-BOFF(pB0)
	flds	88-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	88-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	88-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	88-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	88-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	88-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 23)
	flds	92-BOFF(pB0)
	flds	92-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	92-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	92-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	92-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	92-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	92-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 24)
	flds	96-BOFF(pB0)
	flds	96-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	96-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	96-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	96-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	96-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	96-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 25)
	flds	100-BOFF(pB0)
	flds	100-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	100-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	100-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	100-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	100-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	100-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 26)
	flds	104-BOFF(pB0)
	flds	104-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	104-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	104-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	104-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	104-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	104-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 27)
	flds	108-BOFF(pB0)
	flds	108-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	108-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	108-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	108-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	108-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	108-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 28)
	flds	112-BOFF(pB0)
	flds	112-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	112-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	112-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	112-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	112-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	112-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 29)
	flds	116-BOFF(pB0)
	flds	116-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	116-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	116-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	116-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	116-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	116-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 30)
	flds	120-BOFF(pB0)
	flds	120-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	120-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	120-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	120-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	120-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	120-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 31)
	flds	124-BOFF(pB0)
	flds	124-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	124-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	124-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	124-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	124-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	124-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 32)
	flds	128-BOFF(pB0)
	flds	128-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	128-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	128-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	128-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	128-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	128-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 33)
	flds	132-BOFF(pB0)
	flds	132-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	132-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	132-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	132-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	132-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	132-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 34)
	flds	136-BOFF(pB0)
	flds	136-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	136-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	136-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	136-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	136-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	136-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
/*	ALIGN8 */
#endif
        pref2(192-BOFF(pfA))

#if (KB > 35)
	flds	140-BOFF(pB0)
	flds	140-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	140-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	140-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	140-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	140-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	140-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 36)
	flds	144-BOFF(pB0)
	flds	144-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	144-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	144-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	144-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	144-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	144-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 37)
	flds	148-BOFF(pB0)
	flds	148-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	148-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	148-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	148-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	148-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	148-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 38)
	flds	152-BOFF(pB0)
	flds	152-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	152-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	152-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	152-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	152-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	152-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 39)
	flds	156-BOFF(pB0)
	flds	156-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	156-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	156-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	156-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	156-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	156-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 40)
	flds	160-BOFF(pB0)
	flds	160-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	160-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	160-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	160-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	160-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	160-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 41)
	flds	164-BOFF(pB0)
	flds	164-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	164-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	164-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	164-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	164-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	164-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 42)
	flds	168-BOFF(pB0)
	flds	168-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	168-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	168-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	168-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	168-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	168-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 43)
	flds	172-BOFF(pB0)
	flds	172-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	172-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	172-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	172-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	172-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	172-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 44)
	flds	176-BOFF(pB0)
	flds	176-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	176-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	176-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	176-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	176-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	176-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif
        prefC(CMUL(24)(pC0));
        prefC(CMUL(24)+64(pC0));
        ALIGN8

#if (KB > 45)
	flds	180-BOFF(pB0)
	flds	180-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	180-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	180-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	180-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	180-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	180-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 46)
	flds	184-BOFF(pB0)
	flds	184-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	184-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	184-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	184-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	184-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	184-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 47)
	flds	188-BOFF(pB0)
	flds	188-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	188-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	188-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	188-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	188-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	188-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 48)
	flds	192-BOFF(pB0)
	flds	192-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	192-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	192-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	192-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	192-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	192-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 49)
	flds	196-BOFF(pB0)
	flds	196-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	196-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	196-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	196-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	196-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	196-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 50)
	flds	200-BOFF(pB0)
	flds	200-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	200-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	200-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	200-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	200-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	200-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 51)
	flds	204-BOFF(pB0)
	flds	204-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	204-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	204-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	204-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	204-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	204-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 52)
	flds	208-BOFF(pB0)
	flds	208-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	208-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	208-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	208-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	208-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	208-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 53)
	flds	212-BOFF(pB0)
	flds	212-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	212-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	212-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	212-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	212-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	212-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 54)
	flds	216-BOFF(pB0)
	flds	216-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	216-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	216-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	216-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	216-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	216-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 55)
	flds	220-BOFF(pB0)
	flds	220-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	220-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	220-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	220-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	220-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	220-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 56)
	flds	224-BOFF(pB0)
	flds	224-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	224-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	224-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	224-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	224-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	224-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 57)
	flds	228-BOFF(pB0)
	flds	228-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	228-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	228-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	228-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	228-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	228-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 58)
	flds	232-BOFF(pB0)
	flds	232-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	232-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	232-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	232-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	232-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	232-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 59)
	flds	236-BOFF(pB0)
	flds	236-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	236-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	236-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	236-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	236-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	236-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
/*	ALIGN8 */
#endif

/*
 *End KLOOP
 */
/*
 *      Write results back to C
 */
        fstps   0(pC0)
        fstps   CMUL(4)(pC0)
                lea     (pA1, ldab, 4), pA0
        ALIGN8
        fstps   CMUL(8)(pC0)
        fstps   CMUL(12)(pC0)
                lea     (pA0, ldab, 2), pA1
        ALIGN8
        fstps   CMUL(16)(pC0)
        fstps   CMUL(20)(pC0)
        addl    ldab, pA0
        ALIGN8
        addl    $CMUL(24), pC0
        addl    $120, pfA
#endif
#if (MB > 30)
/*KLOOP: */
#ifdef BETA0
	flds	0-BOFF(pB0)
	flds    0-BOFF(pA1,ldab,4)
	fmul	%st(1), %st
        fxch
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fmuls	0-BOFF(pA0)
        ALIGN8
#elif defined(BETA1)
	flds	0-BOFF(pB0)
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        fadds   CMUL(16)(pC0)
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(2), %st
        fadds   CMUL(12)(pC0)
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(3), %st
        fadds   CMUL(8)(pC0)
	flds	0-BOFF(pA0,ldab)
	fmul	%st(4), %st
        fadds   CMUL(4)(pC0)
	flds	0-BOFF(pA0)
	fmul	%st(5), %st
        fadds   0(pC0)
	flds    0-BOFF(pA1,ldab,4)
	fmul	%st(6), %st
        fadds   CMUL(20)(pC0)
        fstp    %st(6)
#else
        flds    BETAOFF(%esp)
        flds    CMUL(16)(pC0)
        fmul    %st(1), %st

        flds    CMUL(12)(pC0)
        fmul    %st(2), %st
        flds    CMUL(8)(pC0)
        fmul    %st(3), %st
        flds    CMUL(4)(pC0)
        fmul    %st(4), %st
        ALIGN8
        flds    0(pC0)
        fmul    %st(5), %st
        flds    CMUL(20)(pC0)
        fmul    %st(6), %st
        fxch    %st(6)
        fstp    %st
        ALIGN8
	flds	0-BOFF(pB0)
	flds	0-BOFF(pA0)
	fmul	%st(1), %st

	faddp	%st, %st(2)
	flds	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(3)
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(4)
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(5)
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(6)
	fmuls   0-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
        ALIGN8
#endif

#if (KB > 1)
	flds	4-BOFF(pB0)
	flds	4-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	4-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	4-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	4-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	4-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	4-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 2)
	flds	8-BOFF(pB0)
	flds	8-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	8-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	8-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	8-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	8-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	8-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 3)
	flds	12-BOFF(pB0)
	flds	12-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	12-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	12-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	12-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	12-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	12-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 4)
	flds	16-BOFF(pB0)
	flds	16-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	16-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	16-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	16-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	16-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	16-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 5)
	flds	20-BOFF(pB0)
	flds	20-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	20-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	20-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	20-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	20-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	20-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 6)
	flds	24-BOFF(pB0)
	flds	24-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	24-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	24-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	24-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	24-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	24-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 7)
	flds	28-BOFF(pB0)
	flds	28-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	28-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	28-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	28-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	28-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	28-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 8)
	flds	32-BOFF(pB0)
	flds	32-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	32-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	32-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	32-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	32-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	32-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 9)
	flds	36-BOFF(pB0)
	flds	36-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	36-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	36-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	36-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	36-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	36-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 10)
	flds	40-BOFF(pB0)
	flds	40-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	40-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	40-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	40-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	40-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	40-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 11)
	flds	44-BOFF(pB0)
	flds	44-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	44-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	44-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	44-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	44-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	44-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 12)
	flds	48-BOFF(pB0)
	flds	48-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	48-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	48-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	48-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	48-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	48-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 13)
	flds	52-BOFF(pB0)
	flds	52-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	52-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	52-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	52-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	52-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	52-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 14)
	flds	56-BOFF(pB0)
	flds	56-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	56-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	56-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	56-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	56-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	56-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 15)
	flds	60-BOFF(pB0)
	flds	60-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	60-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	60-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	60-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	60-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	60-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 16)
	flds	64-BOFF(pB0)
	flds	64-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	64-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	64-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	64-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	64-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	64-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 17)
	flds	68-BOFF(pB0)
	flds	68-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	68-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	68-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	68-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	68-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	68-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 18)
	flds	72-BOFF(pB0)
	flds	72-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	72-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	72-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	72-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	72-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	72-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 19)
	flds	76-BOFF(pB0)
	flds	76-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	76-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	76-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	76-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	76-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	76-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 20)
	flds	80-BOFF(pB0)
	flds	80-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	80-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	80-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	80-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	80-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	80-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 21)
	flds	84-BOFF(pB0)
	flds	84-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	84-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	84-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	84-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	84-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	84-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 22)
	flds	88-BOFF(pB0)
	flds	88-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	88-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	88-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	88-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	88-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	88-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 23)
	flds	92-BOFF(pB0)
	flds	92-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	92-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	92-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	92-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	92-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	92-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 24)
	flds	96-BOFF(pB0)
	flds	96-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	96-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	96-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	96-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	96-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	96-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 25)
	flds	100-BOFF(pB0)
	flds	100-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	100-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	100-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	100-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	100-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	100-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 26)
	flds	104-BOFF(pB0)
	flds	104-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	104-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	104-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	104-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	104-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	104-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 27)
	flds	108-BOFF(pB0)
	flds	108-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	108-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	108-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	108-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	108-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	108-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 28)
	flds	112-BOFF(pB0)
	flds	112-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	112-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	112-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	112-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	112-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	112-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 29)
	flds	116-BOFF(pB0)
	flds	116-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	116-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	116-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	116-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	116-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	116-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 30)
	flds	120-BOFF(pB0)
	flds	120-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	120-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	120-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	120-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	120-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	120-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 31)
	flds	124-BOFF(pB0)
	flds	124-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	124-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	124-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	124-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	124-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	124-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 32)
	flds	128-BOFF(pB0)
	flds	128-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	128-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	128-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	128-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	128-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	128-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 33)
	flds	132-BOFF(pB0)
	flds	132-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	132-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	132-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	132-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	132-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	132-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 34)
	flds	136-BOFF(pB0)
	flds	136-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	136-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	136-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	136-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	136-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	136-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
/*	ALIGN8 */
#endif

#if (KB > 35)
	flds	140-BOFF(pB0)
	flds	140-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	140-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	140-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	140-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	140-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	140-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 36)
	flds	144-BOFF(pB0)
	flds	144-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	144-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	144-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	144-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	144-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	144-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 37)
	flds	148-BOFF(pB0)
	flds	148-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	148-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	148-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	148-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	148-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	148-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 38)
	flds	152-BOFF(pB0)
	flds	152-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	152-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	152-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	152-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	152-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	152-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 39)
	flds	156-BOFF(pB0)
	flds	156-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	156-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	156-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	156-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	156-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	156-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 40)
	flds	160-BOFF(pB0)
	flds	160-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	160-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	160-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	160-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	160-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	160-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 41)
	flds	164-BOFF(pB0)
	flds	164-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	164-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	164-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	164-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	164-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	164-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 42)
	flds	168-BOFF(pB0)
	flds	168-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	168-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	168-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	168-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	168-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	168-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 43)
	flds	172-BOFF(pB0)
	flds	172-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	172-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	172-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	172-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	172-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	172-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 44)
	flds	176-BOFF(pB0)
	flds	176-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	176-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	176-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	176-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	176-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	176-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif
        prefC(CMUL(24)(pC0));
        prefC(CMUL(24)+64(pC0));
        ALIGN8

#if (KB > 45)
	flds	180-BOFF(pB0)
	flds	180-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	180-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	180-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	180-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	180-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	180-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 46)
	flds	184-BOFF(pB0)
	flds	184-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	184-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	184-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	184-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	184-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	184-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 47)
	flds	188-BOFF(pB0)
	flds	188-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	188-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	188-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	188-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	188-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	188-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 48)
	flds	192-BOFF(pB0)
	flds	192-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	192-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	192-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	192-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	192-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	192-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 49)
	flds	196-BOFF(pB0)
	flds	196-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	196-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	196-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	196-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	196-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	196-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 50)
	flds	200-BOFF(pB0)
	flds	200-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	200-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	200-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	200-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	200-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	200-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 51)
	flds	204-BOFF(pB0)
	flds	204-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	204-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	204-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	204-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	204-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	204-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 52)
	flds	208-BOFF(pB0)
	flds	208-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	208-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	208-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	208-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	208-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	208-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 53)
	flds	212-BOFF(pB0)
	flds	212-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	212-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	212-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	212-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	212-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	212-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 54)
	flds	216-BOFF(pB0)
	flds	216-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	216-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	216-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	216-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	216-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	216-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 55)
	flds	220-BOFF(pB0)
	flds	220-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	220-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	220-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	220-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	220-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	220-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 56)
	flds	224-BOFF(pB0)
	flds	224-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	224-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	224-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	224-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	224-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	224-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 57)
	flds	228-BOFF(pB0)
	flds	228-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	228-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	228-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	228-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	228-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	228-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 58)
	flds	232-BOFF(pB0)
	flds	232-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	232-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	232-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	232-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	232-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	232-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 59)
	flds	236-BOFF(pB0)
	flds	236-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	236-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	236-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	236-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	236-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	236-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
/*	ALIGN8 */
#endif

/*
 *End KLOOP
 */
/*
 *      Write results back to C
 */
        fstps   0(pC0)
        fstps   CMUL(4)(pC0)
                lea     (pA1, ldab, 4), pA0
        ALIGN8
        fstps   CMUL(8)(pC0)
        fstps   CMUL(12)(pC0)
                lea     (pA0, ldab, 2), pA1
        ALIGN8
        fstps   CMUL(16)(pC0)
        fstps   CMUL(20)(pC0)
        addl    ldab, pA0
        ALIGN8
        addl    $CMUL(24), pC0
        addl    $120, pfA
#endif
#if (MB > 36)
/*KLOOP: */
#ifdef BETA0
	flds	0-BOFF(pB0)
	flds    0-BOFF(pA1,ldab,4)
	fmul	%st(1), %st
        fxch
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fmuls	0-BOFF(pA0)
        ALIGN8
#elif defined(BETA1)
	flds	0-BOFF(pB0)
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        fadds   CMUL(16)(pC0)
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(2), %st
        fadds   CMUL(12)(pC0)
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(3), %st
        fadds   CMUL(8)(pC0)
	flds	0-BOFF(pA0,ldab)
	fmul	%st(4), %st
        fadds   CMUL(4)(pC0)
	flds	0-BOFF(pA0)
	fmul	%st(5), %st
        fadds   0(pC0)
	flds    0-BOFF(pA1,ldab,4)
	fmul	%st(6), %st
        fadds   CMUL(20)(pC0)
        fstp    %st(6)
#else
        flds    BETAOFF(%esp)
        flds    CMUL(16)(pC0)
        fmul    %st(1), %st

        flds    CMUL(12)(pC0)
        fmul    %st(2), %st
        flds    CMUL(8)(pC0)
        fmul    %st(3), %st
        flds    CMUL(4)(pC0)
        fmul    %st(4), %st
        ALIGN8
        flds    0(pC0)
        fmul    %st(5), %st
        flds    CMUL(20)(pC0)
        fmul    %st(6), %st
        fxch    %st(6)
        fstp    %st
        ALIGN8
	flds	0-BOFF(pB0)
	flds	0-BOFF(pA0)
	fmul	%st(1), %st

	faddp	%st, %st(2)
	flds	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(3)
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(4)
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(5)
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(6)
	fmuls   0-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
        ALIGN8
#endif

#if (KB > 1)
	flds	4-BOFF(pB0)
	flds	4-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	4-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	4-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	4-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	4-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	4-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 2)
	flds	8-BOFF(pB0)
	flds	8-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	8-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	8-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	8-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	8-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	8-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 3)
	flds	12-BOFF(pB0)
	flds	12-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	12-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	12-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	12-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	12-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	12-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 4)
	flds	16-BOFF(pB0)
	flds	16-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	16-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	16-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	16-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	16-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	16-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 5)
	flds	20-BOFF(pB0)
	flds	20-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	20-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	20-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	20-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	20-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	20-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 6)
	flds	24-BOFF(pB0)
	flds	24-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	24-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	24-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	24-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	24-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	24-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 7)
	flds	28-BOFF(pB0)
	flds	28-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	28-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	28-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	28-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	28-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	28-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 8)
	flds	32-BOFF(pB0)
	flds	32-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	32-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	32-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	32-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	32-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	32-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 9)
	flds	36-BOFF(pB0)
	flds	36-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	36-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	36-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	36-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	36-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	36-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 10)
	flds	40-BOFF(pB0)
	flds	40-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	40-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	40-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	40-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	40-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	40-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 11)
	flds	44-BOFF(pB0)
	flds	44-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	44-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	44-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	44-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	44-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	44-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 12)
	flds	48-BOFF(pB0)
	flds	48-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	48-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	48-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	48-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	48-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	48-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 13)
	flds	52-BOFF(pB0)
	flds	52-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	52-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	52-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	52-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	52-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	52-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 14)
	flds	56-BOFF(pB0)
	flds	56-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	56-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	56-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	56-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	56-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	56-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 15)
	flds	60-BOFF(pB0)
	flds	60-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	60-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	60-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	60-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	60-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	60-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 16)
	flds	64-BOFF(pB0)
	flds	64-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	64-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	64-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	64-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	64-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	64-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 17)
	flds	68-BOFF(pB0)
	flds	68-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	68-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	68-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	68-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	68-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	68-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 18)
	flds	72-BOFF(pB0)
	flds	72-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	72-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	72-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	72-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	72-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	72-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 19)
	flds	76-BOFF(pB0)
	flds	76-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	76-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	76-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	76-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	76-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	76-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 20)
	flds	80-BOFF(pB0)
	flds	80-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	80-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	80-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	80-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	80-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	80-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 21)
	flds	84-BOFF(pB0)
	flds	84-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	84-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	84-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	84-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	84-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	84-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 22)
	flds	88-BOFF(pB0)
	flds	88-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	88-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	88-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	88-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	88-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	88-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 23)
	flds	92-BOFF(pB0)
	flds	92-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	92-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	92-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	92-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	92-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	92-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 24)
	flds	96-BOFF(pB0)
	flds	96-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	96-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	96-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	96-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	96-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	96-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 25)
	flds	100-BOFF(pB0)
	flds	100-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	100-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	100-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	100-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	100-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	100-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 26)
	flds	104-BOFF(pB0)
	flds	104-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	104-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	104-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	104-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	104-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	104-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 27)
	flds	108-BOFF(pB0)
	flds	108-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	108-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	108-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	108-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	108-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	108-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 28)
	flds	112-BOFF(pB0)
	flds	112-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	112-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	112-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	112-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	112-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	112-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 29)
	flds	116-BOFF(pB0)
	flds	116-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	116-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	116-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	116-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	116-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	116-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 30)
	flds	120-BOFF(pB0)
	flds	120-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	120-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	120-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	120-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	120-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	120-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 31)
	flds	124-BOFF(pB0)
	flds	124-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	124-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	124-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	124-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	124-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	124-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 32)
	flds	128-BOFF(pB0)
	flds	128-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	128-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	128-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	128-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	128-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	128-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 33)
	flds	132-BOFF(pB0)
	flds	132-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	132-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	132-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	132-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	132-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	132-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 34)
	flds	136-BOFF(pB0)
	flds	136-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	136-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	136-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	136-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	136-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	136-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
/*	ALIGN8 */
#endif

#if (KB > 35)
	flds	140-BOFF(pB0)
	flds	140-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	140-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	140-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	140-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	140-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	140-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 36)
	flds	144-BOFF(pB0)
	flds	144-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	144-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	144-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	144-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	144-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	144-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 37)
	flds	148-BOFF(pB0)
	flds	148-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	148-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	148-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	148-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	148-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	148-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 38)
	flds	152-BOFF(pB0)
	flds	152-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	152-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	152-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	152-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	152-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	152-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 39)
	flds	156-BOFF(pB0)
	flds	156-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	156-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	156-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	156-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	156-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	156-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 40)
	flds	160-BOFF(pB0)
	flds	160-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	160-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	160-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	160-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	160-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	160-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 41)
	flds	164-BOFF(pB0)
	flds	164-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	164-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	164-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	164-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	164-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	164-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 42)
	flds	168-BOFF(pB0)
	flds	168-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	168-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	168-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	168-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	168-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	168-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 43)
	flds	172-BOFF(pB0)
	flds	172-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	172-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	172-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	172-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	172-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	172-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 44)
	flds	176-BOFF(pB0)
	flds	176-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	176-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	176-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	176-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	176-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	176-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif
        prefC(CMUL(24)(pC0));
        prefC(CMUL(24)+64(pC0));
        ALIGN8

#if (KB > 45)
	flds	180-BOFF(pB0)
	flds	180-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	180-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	180-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	180-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	180-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	180-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 46)
	flds	184-BOFF(pB0)
	flds	184-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	184-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	184-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	184-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	184-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	184-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 47)
	flds	188-BOFF(pB0)
	flds	188-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	188-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	188-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	188-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	188-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	188-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 48)
	flds	192-BOFF(pB0)
	flds	192-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	192-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	192-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	192-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	192-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	192-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 49)
	flds	196-BOFF(pB0)
	flds	196-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	196-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	196-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	196-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	196-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	196-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 50)
	flds	200-BOFF(pB0)
	flds	200-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	200-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	200-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	200-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	200-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	200-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 51)
	flds	204-BOFF(pB0)
	flds	204-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	204-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	204-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	204-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	204-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	204-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 52)
	flds	208-BOFF(pB0)
	flds	208-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	208-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	208-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	208-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	208-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	208-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 53)
	flds	212-BOFF(pB0)
	flds	212-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	212-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	212-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	212-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	212-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	212-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 54)
	flds	216-BOFF(pB0)
	flds	216-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	216-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	216-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	216-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	216-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	216-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 55)
	flds	220-BOFF(pB0)
	flds	220-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	220-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	220-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	220-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	220-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	220-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 56)
	flds	224-BOFF(pB0)
	flds	224-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	224-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	224-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	224-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	224-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	224-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 57)
	flds	228-BOFF(pB0)
	flds	228-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	228-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	228-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	228-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	228-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	228-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 58)
	flds	232-BOFF(pB0)
	flds	232-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	232-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	232-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	232-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	232-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	232-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 59)
	flds	236-BOFF(pB0)
	flds	236-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	236-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	236-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	236-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	236-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	236-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
/*	ALIGN8 */
#endif

/*
 *End KLOOP
 */
/*
 *      Write results back to C
 */
        fstps   0(pC0)
        fstps   CMUL(4)(pC0)
                lea     (pA1, ldab, 4), pA0
        ALIGN8
        fstps   CMUL(8)(pC0)
        fstps   CMUL(12)(pC0)
                lea     (pA0, ldab, 2), pA1
        ALIGN8
        fstps   CMUL(16)(pC0)
        fstps   CMUL(20)(pC0)
        addl    ldab, pA0
        ALIGN8
        addl    $CMUL(24), pC0
#endif
#if (MB > 42)
/*KLOOP: */
#ifdef BETA0
	flds	0-BOFF(pB0)
	flds    0-BOFF(pA1,ldab,4)
	fmul	%st(1), %st
        fxch
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fmuls	0-BOFF(pA0)
        ALIGN8
#elif defined(BETA1)
	flds	0-BOFF(pB0)
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        fadds   CMUL(16)(pC0)
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(2), %st
        fadds   CMUL(12)(pC0)
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(3), %st
        fadds   CMUL(8)(pC0)
	flds	0-BOFF(pA0,ldab)
	fmul	%st(4), %st
        fadds   CMUL(4)(pC0)
	flds	0-BOFF(pA0)
	fmul	%st(5), %st
        fadds   0(pC0)
	flds    0-BOFF(pA1,ldab,4)
	fmul	%st(6), %st
        fadds   CMUL(20)(pC0)
        fstp    %st(6)
#else
        flds    BETAOFF(%esp)
        flds    CMUL(16)(pC0)
        fmul    %st(1), %st

        flds    CMUL(12)(pC0)
        fmul    %st(2), %st
        flds    CMUL(8)(pC0)
        fmul    %st(3), %st
        flds    CMUL(4)(pC0)
        fmul    %st(4), %st
        ALIGN8
        flds    0(pC0)
        fmul    %st(5), %st
        flds    CMUL(20)(pC0)
        fmul    %st(6), %st
        fxch    %st(6)
        fstp    %st
        ALIGN8
	flds	0-BOFF(pB0)
	flds	0-BOFF(pA0)
	fmul	%st(1), %st

	faddp	%st, %st(2)
	flds	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(3)
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(4)
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(5)
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(6)
	fmuls   0-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
        ALIGN8
#endif

#if (KB > 1)
	flds	4-BOFF(pB0)
	flds	4-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	4-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	4-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	4-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	4-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	4-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 2)
	flds	8-BOFF(pB0)
	flds	8-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	8-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	8-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	8-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	8-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	8-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 3)
	flds	12-BOFF(pB0)
	flds	12-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	12-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	12-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	12-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	12-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	12-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 4)
	flds	16-BOFF(pB0)
	flds	16-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	16-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	16-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	16-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	16-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	16-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 5)
	flds	20-BOFF(pB0)
	flds	20-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	20-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	20-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	20-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	20-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	20-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 6)
	flds	24-BOFF(pB0)
	flds	24-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	24-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	24-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	24-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	24-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	24-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 7)
	flds	28-BOFF(pB0)
	flds	28-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	28-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	28-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	28-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	28-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	28-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 8)
	flds	32-BOFF(pB0)
	flds	32-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	32-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	32-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	32-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	32-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	32-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 9)
	flds	36-BOFF(pB0)
	flds	36-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	36-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	36-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	36-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	36-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	36-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 10)
	flds	40-BOFF(pB0)
	flds	40-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	40-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	40-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	40-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	40-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	40-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 11)
	flds	44-BOFF(pB0)
	flds	44-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	44-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	44-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	44-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	44-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	44-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 12)
	flds	48-BOFF(pB0)
	flds	48-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	48-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	48-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	48-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	48-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	48-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 13)
	flds	52-BOFF(pB0)
	flds	52-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	52-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	52-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	52-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	52-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	52-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 14)
	flds	56-BOFF(pB0)
	flds	56-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	56-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	56-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	56-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	56-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	56-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 15)
	flds	60-BOFF(pB0)
	flds	60-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	60-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	60-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	60-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	60-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	60-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 16)
	flds	64-BOFF(pB0)
	flds	64-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	64-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	64-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	64-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	64-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	64-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 17)
	flds	68-BOFF(pB0)
	flds	68-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	68-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	68-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	68-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	68-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	68-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 18)
	flds	72-BOFF(pB0)
	flds	72-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	72-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	72-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	72-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	72-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	72-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 19)
	flds	76-BOFF(pB0)
	flds	76-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	76-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	76-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	76-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	76-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	76-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 20)
	flds	80-BOFF(pB0)
	flds	80-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	80-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	80-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	80-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	80-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	80-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 21)
	flds	84-BOFF(pB0)
	flds	84-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	84-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	84-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	84-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	84-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	84-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 22)
	flds	88-BOFF(pB0)
	flds	88-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	88-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	88-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	88-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	88-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	88-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 23)
	flds	92-BOFF(pB0)
	flds	92-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	92-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	92-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	92-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	92-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	92-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 24)
	flds	96-BOFF(pB0)
	flds	96-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	96-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	96-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	96-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	96-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	96-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 25)
	flds	100-BOFF(pB0)
	flds	100-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	100-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	100-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	100-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	100-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	100-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 26)
	flds	104-BOFF(pB0)
	flds	104-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	104-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	104-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	104-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	104-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	104-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 27)
	flds	108-BOFF(pB0)
	flds	108-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	108-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	108-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	108-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	108-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	108-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 28)
	flds	112-BOFF(pB0)
	flds	112-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	112-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	112-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	112-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	112-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	112-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 29)
	flds	116-BOFF(pB0)
	flds	116-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	116-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	116-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	116-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	116-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	116-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 30)
	flds	120-BOFF(pB0)
	flds	120-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	120-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	120-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	120-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	120-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	120-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 31)
	flds	124-BOFF(pB0)
	flds	124-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	124-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	124-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	124-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	124-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	124-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 32)
	flds	128-BOFF(pB0)
	flds	128-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	128-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	128-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	128-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	128-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	128-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 33)
	flds	132-BOFF(pB0)
	flds	132-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	132-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	132-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	132-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	132-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	132-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 34)
	flds	136-BOFF(pB0)
	flds	136-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	136-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	136-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	136-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	136-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	136-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
/*	ALIGN8 */
#endif

#if (KB > 35)
	flds	140-BOFF(pB0)
	flds	140-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	140-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	140-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	140-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	140-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	140-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 36)
	flds	144-BOFF(pB0)
	flds	144-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	144-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	144-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	144-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	144-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	144-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 37)
	flds	148-BOFF(pB0)
	flds	148-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	148-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	148-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	148-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	148-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	148-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 38)
	flds	152-BOFF(pB0)
	flds	152-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	152-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	152-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	152-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	152-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	152-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 39)
	flds	156-BOFF(pB0)
	flds	156-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	156-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	156-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	156-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	156-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	156-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 40)
	flds	160-BOFF(pB0)
	flds	160-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	160-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	160-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	160-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	160-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	160-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 41)
	flds	164-BOFF(pB0)
	flds	164-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	164-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	164-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	164-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	164-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	164-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 42)
	flds	168-BOFF(pB0)
	flds	168-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	168-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	168-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	168-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	168-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	168-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 43)
	flds	172-BOFF(pB0)
	flds	172-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	172-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	172-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	172-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	172-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	172-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 44)
	flds	176-BOFF(pB0)
	flds	176-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	176-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	176-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	176-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	176-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	176-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif
        prefC(CMUL(24)(pC0));
        prefC(CMUL(24)+64(pC0));
        ALIGN8

#if (KB > 45)
	flds	180-BOFF(pB0)
	flds	180-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	180-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	180-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	180-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	180-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	180-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 46)
	flds	184-BOFF(pB0)
	flds	184-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	184-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	184-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	184-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	184-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	184-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 47)
	flds	188-BOFF(pB0)
	flds	188-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	188-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	188-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	188-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	188-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	188-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 48)
	flds	192-BOFF(pB0)
	flds	192-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	192-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	192-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	192-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	192-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	192-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 49)
	flds	196-BOFF(pB0)
	flds	196-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	196-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	196-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	196-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	196-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	196-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 50)
	flds	200-BOFF(pB0)
	flds	200-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	200-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	200-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	200-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	200-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	200-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 51)
	flds	204-BOFF(pB0)
	flds	204-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	204-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	204-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	204-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	204-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	204-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 52)
	flds	208-BOFF(pB0)
	flds	208-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	208-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	208-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	208-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	208-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	208-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 53)
	flds	212-BOFF(pB0)
	flds	212-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	212-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	212-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	212-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	212-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	212-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 54)
	flds	216-BOFF(pB0)
	flds	216-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	216-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	216-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	216-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	216-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	216-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 55)
	flds	220-BOFF(pB0)
	flds	220-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	220-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	220-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	220-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	220-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	220-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 56)
	flds	224-BOFF(pB0)
	flds	224-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	224-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	224-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	224-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	224-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	224-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 57)
	flds	228-BOFF(pB0)
	flds	228-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	228-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	228-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	228-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	228-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	228-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 58)
	flds	232-BOFF(pB0)
	flds	232-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	232-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	232-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	232-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	232-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	232-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 59)
	flds	236-BOFF(pB0)
	flds	236-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	236-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	236-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	236-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	236-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	236-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
/*	ALIGN8 */
#endif

/*
 *End KLOOP
 */
/*
 *      Write results back to C
 */
        fstps   0(pC0)
        fstps   CMUL(4)(pC0)
                lea     (pA1, ldab, 4), pA0
        ALIGN8
        fstps   CMUL(8)(pC0)
        fstps   CMUL(12)(pC0)
                lea     (pA0, ldab, 2), pA1
        ALIGN8
        fstps   CMUL(16)(pC0)
        fstps   CMUL(20)(pC0)
        addl    ldab, pA0
        ALIGN8
        addl    $CMUL(24), pC0
#endif
#if (MB > 48)
/*KLOOP: */
#ifdef BETA0
	flds	0-BOFF(pB0)
	flds    0-BOFF(pA1,ldab,4)
	fmul	%st(1), %st
        fxch
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fmuls	0-BOFF(pA0)
        ALIGN8
#elif defined(BETA1)
	flds	0-BOFF(pB0)
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        fadds   CMUL(16)(pC0)
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(2), %st
        fadds   CMUL(12)(pC0)
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(3), %st
        fadds   CMUL(8)(pC0)
	flds	0-BOFF(pA0,ldab)
	fmul	%st(4), %st
        fadds   CMUL(4)(pC0)
	flds	0-BOFF(pA0)
	fmul	%st(5), %st
        fadds   0(pC0)
	flds    0-BOFF(pA1,ldab,4)
	fmul	%st(6), %st
        fadds   CMUL(20)(pC0)
        fstp    %st(6)
#else
        flds    BETAOFF(%esp)
        flds    CMUL(16)(pC0)
        fmul    %st(1), %st

        flds    CMUL(12)(pC0)
        fmul    %st(2), %st
        flds    CMUL(8)(pC0)
        fmul    %st(3), %st
        flds    CMUL(4)(pC0)
        fmul    %st(4), %st
        ALIGN8
        flds    0(pC0)
        fmul    %st(5), %st
        flds    CMUL(20)(pC0)
        fmul    %st(6), %st
        fxch    %st(6)
        fstp    %st
        ALIGN8
	flds	0-BOFF(pB0)
	flds	0-BOFF(pA0)
	fmul	%st(1), %st

	faddp	%st, %st(2)
	flds	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(3)
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(4)
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(5)
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(6)
	fmuls   0-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
        ALIGN8
#endif

#if (KB > 1)
	flds	4-BOFF(pB0)
	flds	4-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	4-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	4-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	4-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	4-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	4-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 2)
	flds	8-BOFF(pB0)
	flds	8-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	8-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	8-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	8-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	8-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	8-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 3)
	flds	12-BOFF(pB0)
	flds	12-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	12-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	12-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	12-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	12-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	12-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 4)
	flds	16-BOFF(pB0)
	flds	16-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	16-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	16-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	16-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	16-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	16-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 5)
	flds	20-BOFF(pB0)
	flds	20-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	20-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	20-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	20-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	20-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	20-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 6)
	flds	24-BOFF(pB0)
	flds	24-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	24-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	24-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	24-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	24-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	24-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 7)
	flds	28-BOFF(pB0)
	flds	28-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	28-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	28-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	28-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	28-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	28-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 8)
	flds	32-BOFF(pB0)
	flds	32-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	32-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	32-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	32-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	32-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	32-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 9)
	flds	36-BOFF(pB0)
	flds	36-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	36-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	36-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	36-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	36-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	36-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 10)
	flds	40-BOFF(pB0)
	flds	40-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	40-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	40-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	40-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	40-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	40-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 11)
	flds	44-BOFF(pB0)
	flds	44-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	44-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	44-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	44-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	44-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	44-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 12)
	flds	48-BOFF(pB0)
	flds	48-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	48-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	48-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	48-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	48-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	48-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 13)
	flds	52-BOFF(pB0)
	flds	52-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	52-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	52-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	52-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	52-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	52-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 14)
	flds	56-BOFF(pB0)
	flds	56-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	56-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	56-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	56-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	56-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	56-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 15)
	flds	60-BOFF(pB0)
	flds	60-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	60-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	60-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	60-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	60-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	60-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 16)
	flds	64-BOFF(pB0)
	flds	64-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	64-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	64-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	64-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	64-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	64-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 17)
	flds	68-BOFF(pB0)
	flds	68-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	68-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	68-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	68-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	68-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	68-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 18)
	flds	72-BOFF(pB0)
	flds	72-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	72-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	72-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	72-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	72-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	72-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 19)
	flds	76-BOFF(pB0)
	flds	76-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	76-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	76-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	76-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	76-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	76-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 20)
	flds	80-BOFF(pB0)
	flds	80-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	80-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	80-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	80-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	80-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	80-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 21)
	flds	84-BOFF(pB0)
	flds	84-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	84-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	84-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	84-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	84-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	84-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 22)
	flds	88-BOFF(pB0)
	flds	88-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	88-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	88-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	88-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	88-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	88-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 23)
	flds	92-BOFF(pB0)
	flds	92-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	92-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	92-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	92-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	92-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	92-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 24)
	flds	96-BOFF(pB0)
	flds	96-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	96-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	96-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	96-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	96-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	96-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 25)
	flds	100-BOFF(pB0)
	flds	100-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	100-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	100-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	100-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	100-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	100-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 26)
	flds	104-BOFF(pB0)
	flds	104-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	104-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	104-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	104-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	104-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	104-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 27)
	flds	108-BOFF(pB0)
	flds	108-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	108-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	108-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	108-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	108-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	108-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 28)
	flds	112-BOFF(pB0)
	flds	112-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	112-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	112-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	112-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	112-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	112-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 29)
	flds	116-BOFF(pB0)
	flds	116-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	116-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	116-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	116-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	116-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	116-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 30)
	flds	120-BOFF(pB0)
	flds	120-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	120-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	120-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	120-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	120-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	120-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 31)
	flds	124-BOFF(pB0)
	flds	124-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	124-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	124-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	124-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	124-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	124-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 32)
	flds	128-BOFF(pB0)
	flds	128-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	128-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	128-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	128-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	128-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	128-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 33)
	flds	132-BOFF(pB0)
	flds	132-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	132-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	132-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	132-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	132-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	132-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 34)
	flds	136-BOFF(pB0)
	flds	136-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	136-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	136-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	136-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	136-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	136-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
/*	ALIGN8 */
#endif

#if (KB > 35)
	flds	140-BOFF(pB0)
	flds	140-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	140-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	140-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	140-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	140-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	140-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 36)
	flds	144-BOFF(pB0)
	flds	144-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	144-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	144-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	144-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	144-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	144-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 37)
	flds	148-BOFF(pB0)
	flds	148-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	148-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	148-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	148-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	148-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	148-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 38)
	flds	152-BOFF(pB0)
	flds	152-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	152-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	152-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	152-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	152-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	152-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 39)
	flds	156-BOFF(pB0)
	flds	156-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	156-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	156-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	156-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	156-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	156-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 40)
	flds	160-BOFF(pB0)
	flds	160-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	160-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	160-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	160-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	160-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	160-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 41)
	flds	164-BOFF(pB0)
	flds	164-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	164-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	164-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	164-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	164-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	164-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 42)
	flds	168-BOFF(pB0)
	flds	168-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	168-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	168-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	168-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	168-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	168-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 43)
	flds	172-BOFF(pB0)
	flds	172-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	172-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	172-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	172-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	172-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	172-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 44)
	flds	176-BOFF(pB0)
	flds	176-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	176-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	176-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	176-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	176-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	176-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif
        prefC(CMUL(24)(pC0));
        prefC(CMUL(24)+64(pC0));
        ALIGN8

#if (KB > 45)
	flds	180-BOFF(pB0)
	flds	180-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	180-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	180-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	180-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	180-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	180-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 46)
	flds	184-BOFF(pB0)
	flds	184-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	184-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	184-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	184-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	184-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	184-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 47)
	flds	188-BOFF(pB0)
	flds	188-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	188-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	188-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	188-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	188-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	188-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 48)
	flds	192-BOFF(pB0)
	flds	192-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	192-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	192-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	192-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	192-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	192-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 49)
	flds	196-BOFF(pB0)
	flds	196-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	196-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	196-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	196-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	196-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	196-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 50)
	flds	200-BOFF(pB0)
	flds	200-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	200-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	200-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	200-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	200-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	200-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 51)
	flds	204-BOFF(pB0)
	flds	204-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	204-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	204-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	204-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	204-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	204-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 52)
	flds	208-BOFF(pB0)
	flds	208-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	208-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	208-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	208-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	208-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	208-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 53)
	flds	212-BOFF(pB0)
	flds	212-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	212-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	212-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	212-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	212-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	212-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 54)
	flds	216-BOFF(pB0)
	flds	216-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	216-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	216-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	216-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	216-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	216-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 55)
	flds	220-BOFF(pB0)
	flds	220-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	220-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	220-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	220-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	220-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	220-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 56)
	flds	224-BOFF(pB0)
	flds	224-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	224-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	224-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	224-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	224-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	224-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 57)
	flds	228-BOFF(pB0)
	flds	228-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	228-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	228-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	228-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	228-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	228-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 58)
	flds	232-BOFF(pB0)
	flds	232-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	232-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	232-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	232-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	232-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	232-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 59)
	flds	236-BOFF(pB0)
	flds	236-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	236-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	236-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	236-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	236-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	236-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
/*	ALIGN8 */
#endif

/*
 *End KLOOP
 */
/*
 *      Write results back to C
 */
        fstps   0(pC0)
        fstps   CMUL(4)(pC0)
                lea     (pA1, ldab, 4), pA0
        ALIGN8
        fstps   CMUL(8)(pC0)
        fstps   CMUL(12)(pC0)
                lea     (pA0, ldab, 2), pA1
        ALIGN8
        fstps   CMUL(16)(pC0)
        fstps   CMUL(20)(pC0)
        addl    ldab, pA0
        ALIGN8
        addl    $CMUL(24), pC0
#endif
#if (MB > 56)
/*KLOOP: */
#ifdef BETA0
	flds	0-BOFF(pB0)
	flds    0-BOFF(pA1,ldab,4)
	fmul	%st(1), %st
        fxch
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fmuls	0-BOFF(pA0)
        ALIGN8
#elif defined(BETA1)
	flds	0-BOFF(pB0)
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        fadds   CMUL(16)(pC0)
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(2), %st
        fadds   CMUL(12)(pC0)
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(3), %st
        fadds   CMUL(8)(pC0)
	flds	0-BOFF(pA0,ldab)
	fmul	%st(4), %st
        fadds   CMUL(4)(pC0)
	flds	0-BOFF(pA0)
	fmul	%st(5), %st
        fadds   0(pC0)
	flds    0-BOFF(pA1,ldab,4)
	fmul	%st(6), %st
        fadds   CMUL(20)(pC0)
        fstp    %st(6)
#else
        flds    BETAOFF(%esp)
        flds    CMUL(16)(pC0)
        fmul    %st(1), %st

        flds    CMUL(12)(pC0)
        fmul    %st(2), %st
        flds    CMUL(8)(pC0)
        fmul    %st(3), %st
        flds    CMUL(4)(pC0)
        fmul    %st(4), %st
        ALIGN8
        flds    0(pC0)
        fmul    %st(5), %st
        flds    CMUL(20)(pC0)
        fmul    %st(6), %st
        fxch    %st(6)
        fstp    %st
        ALIGN8
	flds	0-BOFF(pB0)
	flds	0-BOFF(pA0)
	fmul	%st(1), %st

	faddp	%st, %st(2)
	flds	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(3)
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(4)
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(5)
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(6)
	fmuls   0-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
        ALIGN8
#endif

#if (KB > 1)
	flds	4-BOFF(pB0)
	flds	4-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	4-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	4-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	4-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	4-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	4-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 2)
	flds	8-BOFF(pB0)
	flds	8-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	8-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	8-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	8-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	8-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	8-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 3)
	flds	12-BOFF(pB0)
	flds	12-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	12-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	12-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	12-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	12-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	12-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 4)
	flds	16-BOFF(pB0)
	flds	16-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	16-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	16-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	16-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	16-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	16-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 5)
	flds	20-BOFF(pB0)
	flds	20-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	20-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	20-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	20-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	20-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	20-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 6)
	flds	24-BOFF(pB0)
	flds	24-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	24-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	24-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	24-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	24-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	24-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 7)
	flds	28-BOFF(pB0)
	flds	28-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	28-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	28-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	28-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	28-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	28-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 8)
	flds	32-BOFF(pB0)
	flds	32-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	32-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	32-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	32-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	32-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	32-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 9)
	flds	36-BOFF(pB0)
	flds	36-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	36-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	36-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	36-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	36-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	36-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 10)
	flds	40-BOFF(pB0)
	flds	40-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	40-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	40-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	40-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	40-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	40-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 11)
	flds	44-BOFF(pB0)
	flds	44-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	44-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	44-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	44-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	44-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	44-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 12)
	flds	48-BOFF(pB0)
	flds	48-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	48-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	48-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	48-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	48-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	48-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 13)
	flds	52-BOFF(pB0)
	flds	52-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	52-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	52-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	52-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	52-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	52-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 14)
	flds	56-BOFF(pB0)
	flds	56-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	56-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	56-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	56-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	56-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	56-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 15)
	flds	60-BOFF(pB0)
	flds	60-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	60-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	60-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	60-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	60-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	60-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 16)
	flds	64-BOFF(pB0)
	flds	64-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	64-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	64-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	64-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	64-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	64-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 17)
	flds	68-BOFF(pB0)
	flds	68-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	68-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	68-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	68-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	68-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	68-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 18)
	flds	72-BOFF(pB0)
	flds	72-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	72-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	72-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	72-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	72-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	72-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 19)
	flds	76-BOFF(pB0)
	flds	76-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	76-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	76-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	76-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	76-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	76-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 20)
	flds	80-BOFF(pB0)
	flds	80-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	80-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	80-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	80-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	80-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	80-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 21)
	flds	84-BOFF(pB0)
	flds	84-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	84-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	84-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	84-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	84-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	84-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 22)
	flds	88-BOFF(pB0)
	flds	88-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	88-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	88-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	88-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	88-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	88-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 23)
	flds	92-BOFF(pB0)
	flds	92-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	92-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	92-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	92-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	92-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	92-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 24)
	flds	96-BOFF(pB0)
	flds	96-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	96-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	96-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	96-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	96-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	96-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 25)
	flds	100-BOFF(pB0)
	flds	100-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	100-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	100-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	100-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	100-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	100-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 26)
	flds	104-BOFF(pB0)
	flds	104-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	104-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	104-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	104-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	104-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	104-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 27)
	flds	108-BOFF(pB0)
	flds	108-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	108-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	108-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	108-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	108-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	108-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 28)
	flds	112-BOFF(pB0)
	flds	112-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	112-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	112-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	112-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	112-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	112-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 29)
	flds	116-BOFF(pB0)
	flds	116-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	116-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	116-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	116-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	116-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	116-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 30)
	flds	120-BOFF(pB0)
	flds	120-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	120-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	120-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	120-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	120-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	120-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 31)
	flds	124-BOFF(pB0)
	flds	124-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	124-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	124-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	124-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	124-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	124-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 32)
	flds	128-BOFF(pB0)
	flds	128-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	128-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	128-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	128-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	128-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	128-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 33)
	flds	132-BOFF(pB0)
	flds	132-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	132-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	132-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	132-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	132-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	132-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 34)
	flds	136-BOFF(pB0)
	flds	136-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	136-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	136-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	136-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	136-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	136-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
/*	ALIGN8 */
#endif

#if (KB > 35)
	flds	140-BOFF(pB0)
	flds	140-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	140-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	140-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	140-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	140-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	140-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 36)
	flds	144-BOFF(pB0)
	flds	144-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	144-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	144-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	144-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	144-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	144-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 37)
	flds	148-BOFF(pB0)
	flds	148-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	148-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	148-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	148-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	148-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	148-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 38)
	flds	152-BOFF(pB0)
	flds	152-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	152-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	152-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	152-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	152-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	152-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 39)
	flds	156-BOFF(pB0)
	flds	156-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	156-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	156-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	156-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	156-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	156-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 40)
	flds	160-BOFF(pB0)
	flds	160-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	160-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	160-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	160-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	160-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	160-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 41)
	flds	164-BOFF(pB0)
	flds	164-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	164-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	164-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	164-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	164-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	164-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 42)
	flds	168-BOFF(pB0)
	flds	168-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	168-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	168-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	168-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	168-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	168-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 43)
	flds	172-BOFF(pB0)
	flds	172-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	172-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	172-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	172-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	172-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	172-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 44)
	flds	176-BOFF(pB0)
	flds	176-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	176-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	176-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	176-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	176-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	176-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif
        prefC(CMUL(24)(pC0));
        prefC(CMUL(24)+64(pC0));
        ALIGN8

#if (KB > 45)
	flds	180-BOFF(pB0)
	flds	180-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	180-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	180-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	180-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	180-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	180-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 46)
	flds	184-BOFF(pB0)
	flds	184-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	184-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	184-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	184-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	184-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	184-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 47)
	flds	188-BOFF(pB0)
	flds	188-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	188-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	188-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	188-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	188-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	188-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 48)
	flds	192-BOFF(pB0)
	flds	192-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	192-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	192-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	192-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	192-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	192-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 49)
	flds	196-BOFF(pB0)
	flds	196-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	196-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	196-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	196-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	196-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	196-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 50)
	flds	200-BOFF(pB0)
	flds	200-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	200-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	200-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	200-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	200-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	200-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 51)
	flds	204-BOFF(pB0)
	flds	204-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	204-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	204-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	204-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	204-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	204-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 52)
	flds	208-BOFF(pB0)
	flds	208-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	208-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	208-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	208-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	208-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	208-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 53)
	flds	212-BOFF(pB0)
	flds	212-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	212-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	212-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	212-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	212-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	212-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 54)
	flds	216-BOFF(pB0)
	flds	216-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	216-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	216-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	216-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	216-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	216-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 55)
	flds	220-BOFF(pB0)
	flds	220-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	220-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	220-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	220-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	220-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	220-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 56)
	flds	224-BOFF(pB0)
	flds	224-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	224-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	224-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	224-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	224-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	224-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 57)
	flds	228-BOFF(pB0)
	flds	228-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	228-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	228-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	228-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	228-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	228-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 58)
	flds	232-BOFF(pB0)
	flds	232-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	232-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	232-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	232-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	232-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	232-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 59)
	flds	236-BOFF(pB0)
	flds	236-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	236-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	236-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	236-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	236-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	236-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
/*	ALIGN8 */
#endif

/*
 *End KLOOP
 */
/*
 *      Write results back to C
 */
        fstps   0(pC0)
        fstps   CMUL(4)(pC0)
                lea     (pA1, ldab, 4), pA0
        ALIGN8
        fstps   CMUL(8)(pC0)
        fstps   CMUL(12)(pC0)
                lea     (pA0, ldab, 2), pA1
        ALIGN8
        fstps   CMUL(16)(pC0)
        fstps   CMUL(20)(pC0)
        addl    ldab, pA0
        ALIGN8
        addl    $CMUL(24), pC0

/*        dec     stM */
/*        jnz     MLOOP */
#endif
/*KLOOP: */
#ifdef BETA0
	flds	0-BOFF(pB0)
	flds    0-BOFF(pA1,ldab,4)
	fmul	%st(1), %st
        fxch
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	flds	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fmuls	0-BOFF(pA0)
        ALIGN8
#elif defined(BETA1)
	flds	0-BOFF(pB0)
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        fadds   CMUL(16)(pC0)
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(2), %st
        fadds   CMUL(12)(pC0)
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(3), %st
        fadds   CMUL(8)(pC0)
	flds	0-BOFF(pA0,ldab)
	fmul	%st(4), %st
        fadds   CMUL(4)(pC0)
	flds	0-BOFF(pA0)
	fmul	%st(5), %st
        fadds   0(pC0)
	flds    0-BOFF(pA1,ldab,4)
	fmul	%st(6), %st
        fadds   CMUL(20)(pC0)
        fstp    %st(6)
#else
        flds    BETAOFF(%esp)
        flds    CMUL(16)(pC0)
        fmul    %st(1), %st

        flds    CMUL(12)(pC0)
        fmul    %st(2), %st
        flds    CMUL(8)(pC0)
        fmul    %st(3), %st
        flds    CMUL(4)(pC0)
        fmul    %st(4), %st
        ALIGN8
        flds    0(pC0)
        fmul    %st(5), %st
        flds    CMUL(20)(pC0)
        fmul    %st(6), %st
        fxch    %st(6)
        fstp    %st
        ALIGN8
	flds	0-BOFF(pB0)
	flds	0-BOFF(pA0)
	fmul	%st(1), %st

	faddp	%st, %st(2)
	flds	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(3)
	flds	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(4)
	flds	0-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(5)
	flds	0-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(6)
	fmuls   0-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
        ALIGN8
#endif

#if (KB > 1)
	flds	4-BOFF(pB0)
	flds	4-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	4-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	4-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	4-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	4-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	4-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 2)
	flds	8-BOFF(pB0)
	flds	8-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	8-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	8-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	8-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	8-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	8-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 3)
	flds	12-BOFF(pB0)
	flds	12-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	12-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	12-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	12-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	12-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	12-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif
        prefB(-BOFF(pB0,ldab,2))

#if (KB > 4)
	flds	16-BOFF(pB0)
	flds	16-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	16-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	16-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	16-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	16-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	16-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 5)
	flds	20-BOFF(pB0)
	flds	20-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	20-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	20-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	20-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	20-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	20-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 6)
	flds	24-BOFF(pB0)
	flds	24-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	24-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	24-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	24-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	24-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	24-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

        prefB(64-BOFF(pB0,ldab,2))
#if (KB > 7)
	flds	28-BOFF(pB0)
	flds	28-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	28-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	28-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	28-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	28-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	28-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 8)
	flds	32-BOFF(pB0)
	flds	32-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	32-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	32-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	32-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	32-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	32-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 9)
	flds	36-BOFF(pB0)
	flds	36-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	36-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	36-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	36-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	36-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	36-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

        prefB(128-BOFF(pB0,ldab,2))
#if (KB > 10)
	flds	40-BOFF(pB0)
	flds	40-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	40-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	40-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	40-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	40-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	40-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 11)
	flds	44-BOFF(pB0)
	flds	44-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	44-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	44-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	44-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	44-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	44-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 12)
	flds	48-BOFF(pB0)
	flds	48-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	48-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	48-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	48-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	48-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	48-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
/*	ALIGN8 */
#endif

        prefB(192-BOFF(pB0,ldab,2))
#if (KB > 13)
	flds	52-BOFF(pB0)
	flds	52-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	52-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	52-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	52-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	52-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	52-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 14)
	flds	56-BOFF(pB0)
	flds	56-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	56-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	56-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	56-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	56-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	56-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 15)
	flds	60-BOFF(pB0)
	flds	60-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	60-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	60-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	60-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	60-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	60-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 16)
	flds	64-BOFF(pB0)
	flds	64-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	64-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	64-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	64-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	64-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	64-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 17)
	flds	68-BOFF(pB0)
	flds	68-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	68-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	68-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	68-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	68-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	68-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 18)
	flds	72-BOFF(pB0)
	flds	72-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	72-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	72-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	72-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	72-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	72-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 19)
	flds	76-BOFF(pB0)
	flds	76-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	76-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	76-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	76-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	76-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	76-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 20)
	flds	80-BOFF(pB0)
	flds	80-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	80-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	80-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	80-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	80-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	80-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 21)
	flds	84-BOFF(pB0)
	flds	84-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	84-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	84-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	84-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	84-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	84-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 22)
	flds	88-BOFF(pB0)
	flds	88-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	88-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	88-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	88-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	88-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	88-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 23)
	flds	92-BOFF(pB0)
	flds	92-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	92-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	92-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	92-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	92-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	92-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 24)
	flds	96-BOFF(pB0)
	flds	96-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	96-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	96-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	96-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	96-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	96-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 25)
	flds	100-BOFF(pB0)
	flds	100-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	100-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	100-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	100-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	100-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	100-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 26)
	flds	104-BOFF(pB0)
	flds	104-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	104-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	104-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	104-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	104-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	104-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 27)
	flds	108-BOFF(pB0)
	flds	108-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	108-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	108-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	108-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	108-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	108-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 28)
	flds	112-BOFF(pB0)
	flds	112-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	112-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	112-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	112-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	112-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	112-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 29)
	flds	116-BOFF(pB0)
	flds	116-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	116-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	116-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	116-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	116-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	116-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 30)
	flds	120-BOFF(pB0)
	flds	120-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	120-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	120-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	120-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	120-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	120-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 31)
	flds	124-BOFF(pB0)
	flds	124-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	124-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	124-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	124-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	124-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	124-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 32)
	flds	128-BOFF(pB0)
	flds	128-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	128-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	128-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	128-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	128-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	128-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 33)
	flds	132-BOFF(pB0)
	flds	132-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	132-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	132-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	132-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	132-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	132-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 34)
	flds	136-BOFF(pB0)
	flds	136-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	136-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	136-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	136-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	136-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	136-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
/*	ALIGN8 */
#endif

#if (KB > 35)
	flds	140-BOFF(pB0)
	flds	140-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	140-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	140-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	140-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	140-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	140-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 36)
	flds	144-BOFF(pB0)
	flds	144-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	144-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	144-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	144-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	144-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	144-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 37)
	flds	148-BOFF(pB0)
	flds	148-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	148-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	148-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	148-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	148-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	148-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 38)
	flds	152-BOFF(pB0)
	flds	152-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	152-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	152-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	152-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	152-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	152-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 39)
	flds	156-BOFF(pB0)
	flds	156-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	156-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	156-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	156-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	156-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	156-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 40)
	flds	160-BOFF(pB0)
	flds	160-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	160-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	160-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	160-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	160-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	160-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

        addl    8(%esp), pC0
#if (KB > 41)
	flds	164-BOFF(pB0)
	flds	164-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	164-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	164-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	164-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	164-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	164-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 42)
	flds	168-BOFF(pB0)
	flds	168-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	168-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	168-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	168-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	168-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	168-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 43)
	flds	172-BOFF(pB0)
	flds	172-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	172-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	172-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	172-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	172-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	172-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 44)
	flds	176-BOFF(pB0)
	flds	176-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	176-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	176-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	176-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	176-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	176-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif
        prefC(0(pC0));
        prefC(64(pC0));
        ALIGN8

#if (KB > 45)
	flds	180-BOFF(pB0)
	flds	180-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	180-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	180-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	180-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	180-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	180-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 46)
	flds	184-BOFF(pB0)
	flds	184-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	184-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	184-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	184-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	184-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	184-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

        subl    8(%esp), pC0
#if (KB > 47)
	flds	188-BOFF(pB0)
	flds	188-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	188-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	188-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	188-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	188-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	188-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 48)
	flds	192-BOFF(pB0)
	flds	192-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	192-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	192-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	192-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	192-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	192-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 49)
	flds	196-BOFF(pB0)
	flds	196-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	196-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	196-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	196-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	196-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	196-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 50)
	flds	200-BOFF(pB0)
	flds	200-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	200-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	200-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	200-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	200-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	200-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 51)
	flds	204-BOFF(pB0)
	flds	204-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	204-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	204-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	204-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	204-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	204-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 52)
	flds	208-BOFF(pB0)
	flds	208-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	208-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	208-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	208-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	208-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	208-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 53)
	flds	212-BOFF(pB0)
	flds	212-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	212-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	212-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	212-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	212-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	212-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 54)
	flds	216-BOFF(pB0)
	flds	216-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	216-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	216-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	216-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	216-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	216-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 55)
	flds	220-BOFF(pB0)
	flds	220-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	220-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	220-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	220-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	220-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	220-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 56)
	flds	224-BOFF(pB0)
	flds	224-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	224-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	224-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	224-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	224-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	224-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 57)
	flds	228-BOFF(pB0)
	flds	228-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	228-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	228-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	228-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	228-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	228-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 58)
	flds	232-BOFF(pB0)
	flds	232-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	232-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	232-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	232-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	232-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	232-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 59)
	flds	236-BOFF(pB0)
	flds	236-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	flds	236-BOFF(pA1)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	flds	236-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	flds	236-BOFF(pA1,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	flds	236-BOFF(pA0,ldab,4)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmuls	236-BOFF(pA1,ldab,4)
	faddp	%st, %st(6)
/*	ALIGN8 */
#endif

/*
 *End KLOOP
 */
/*
 *      Write results back to C
 */
        fstps   0(pC0)
        fstps   CMUL(4)(pC0)
                lea     (pA1, ldab, 4), pA0
        ALIGN8
        fstps   CMUL(8)(pC0)
        fstps   CMUL(12)(pC0)
                lea     (pA0, ldab, 2), pA1
        ALIGN8
        fstps   CMUL(16)(pC0)
        fstps   CMUL(20)(pC0)
        addl    ldab, pA0
        ALIGN8
/*        addl    $CMUL(24), pC0 */

        subl    $MB*KB*4, pA0
        lea     (pA0, ldab), pA1
        addl    8(%esp), pC0
        addl    ldab, pB0
        dec     stN
        jnz     NLOOP
DONE:
        movl    24(%esp), %ebp
        movl    20(%esp), %ebx
        movl    16(%esp), %esi
        movl    12(%esp), %edi
        addl    $28, %esp
        ret
