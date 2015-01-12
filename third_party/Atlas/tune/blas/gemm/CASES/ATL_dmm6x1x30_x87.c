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
   #error "This kernel requires a gas 32 bit x86 assembler!"
#endif
#define BOFF ((KB/2)*8)

#if !defined(MB) || (MB == 0)
   #error "MB must be a compile-time constant!"
#endif
#if !defined(KB) || (KB == 0)
   #error "KB must be a compile-time constant!"
#endif
#if (KB > 30)
   #error "KB must less than 30!"
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
   #define pA3     %eax
   #define pB0     %edi
   #define ldab    %edx
   #define pfA     %ebp
   #define stN     %bh
   #define stM     %bl
/* lower 16 bits of %ebx used for M & N loop counters */
/* incCn overwrites pA3 */
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
#ifdef DCPLX
   #define CMUL(arg_) (2*(arg_))
#else
   #define CMUL(arg_) arg_
#endif
/*
                           4            8           12                16
 void ATL_AUSERMM(const int M, const int N, const int K, const TYPE alpha,
                            24             28             32             36
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                              40       48             52
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
        movl    80(%esp), %eax
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
        addl    $BOFF, pA0
        addl    $BOFF, pB0
/*
 *      ldab = K * 8;
 */
        movl    40(%esp), ldab
        shl     $3, ldab
/*        movl    $KB*8, ldab */
/*
 *      pfA = pA + NBNB
 */
        movl    pA0, pfA
        addl    $MB*KB*8, pfA
                                        prefB((pB0))
                                        prefB(64(pB0))
        movb    36(%esp), stN
        ALIGN16
NLOOP:
        lea     0(pA0, ldab,2), pA3
        addl    ldab, pA3
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
	fldl	0-BOFF(pB0)
	fldl    0-BOFF(pA3,ldab,2)
	fmul	%st(1), %st
        fxch
	fldl	0-BOFF(pA3,ldab)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fldl	0-BOFF(pA3)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fldl	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fldl	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fmull	0-BOFF(pA0)
        ALIGN8
#elif defined(BETA1)
	fldl	0-BOFF(pB0)
	fldl	0-BOFF(pA3,ldab)
	fmul	%st(1), %st
        faddl   CMUL(32)(pC0)
	fldl	0-BOFF(pA3)
	fmul	%st(2), %st
        faddl   CMUL(24)(pC0)
	fldl	0-BOFF(pA0,ldab,2)
	fmul	%st(3), %st
        faddl   CMUL(16)(pC0)
	fldl	0-BOFF(pA0,ldab)
	fmul	%st(4), %st
        faddl   CMUL(8)(pC0)
	fldl	0-BOFF(pA0)
	fmul	%st(5), %st
        faddl   0(pC0)
	fldl    0-BOFF(pA3,ldab,2)
	fmul	%st(6), %st
        faddl   CMUL(40)(pC0)
        fstp    %st(6)
#else
        fldl    BETAOFF(%esp)
        fldl    CMUL(32)(pC0)
        fmul    %st(1), %st

        fldl    CMUL(24)(pC0)
        fmul    %st(2), %st
        fldl    CMUL(16)(pC0)
        fmul    %st(3), %st
        fldl    CMUL(8)(pC0)
        fmul    %st(4), %st
        ALIGN8
        fldl    0(pC0)
        fmul    %st(5), %st
        fldl    CMUL(40)(pC0)
        fmul    %st(6), %st
        fxch    %st(6)
        fstp    %st
        ALIGN8
	fldl	0-BOFF(pB0)
	fldl	0-BOFF(pA0)
	fmul	%st(1), %st

	faddp	%st, %st(2)
	fldl	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(3)
	fldl	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(4)
	fldl	0-BOFF(pA3)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(5)
	fldl	0-BOFF(pA3,ldab)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(6)
	fmull   0-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
        ALIGN8
#endif

#if (KB > 1)
	fldl	8-BOFF(pB0)
	fldl	8-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	8-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	8-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	8-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	8-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	8-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 2)
	fldl	16-BOFF(pB0)
	fldl	16-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	16-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	16-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	16-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	16-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	16-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 3)
	fldl	24-BOFF(pB0)
	fldl	24-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	24-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	24-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	24-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	24-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	24-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 4)
	fldl	32-BOFF(pB0)
	fldl	32-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	32-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	32-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	32-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	32-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	32-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 5)
	fldl	40-BOFF(pB0)
	fldl	40-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	40-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	40-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	40-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	40-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	40-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 6)
	fldl	48-BOFF(pB0)
	fldl	48-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	48-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	48-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	48-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	48-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	48-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 7)
	fldl	56-BOFF(pB0)
	fldl	56-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	56-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	56-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	56-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	56-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	56-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 8)
	fldl	64-BOFF(pB0)
	fldl	64-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	64-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	64-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	64-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	64-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	64-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 9)
	fldl	72-BOFF(pB0)
	fldl	72-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	72-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	72-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	72-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	72-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	72-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 10)
	fldl	80-BOFF(pB0)
	fldl	80-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	80-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	80-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	80-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	80-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	80-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 11)
	fldl	88-BOFF(pB0)
	fldl	88-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	88-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	88-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	88-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	88-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	88-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 12)
	fldl	96-BOFF(pB0)
	fldl	96-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	96-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	96-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	96-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	96-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	96-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 13)
	fldl	104-BOFF(pB0)
	fldl	104-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	104-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	104-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	104-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	104-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	104-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 14)
	fldl	112-BOFF(pB0)
	fldl	112-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	112-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	112-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	112-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	112-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	112-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 15)
	fldl	120-BOFF(pB0)
	fldl	120-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	120-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	120-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	120-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	120-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	120-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 16)
	fldl	128-BOFF(pB0)
	fldl	128-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	128-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	128-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	128-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	128-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	128-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 17)
	fldl	136-BOFF(pB0)
	fldl	136-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	136-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	136-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	136-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	136-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	136-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 18)
	fldl	144-BOFF(pB0)
	fldl	144-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	144-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	144-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	144-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	144-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	144-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 19)
	fldl	152-BOFF(pB0)
	fldl	152-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	152-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	152-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	152-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	152-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	152-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 20)
	fldl	160-BOFF(pB0)
	fldl	160-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	160-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	160-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	160-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	160-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	160-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 21)
	fldl	168-BOFF(pB0)
	fldl	168-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	168-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	168-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	168-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	168-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	168-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 22)
	fldl	176-BOFF(pB0)
	fldl	176-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	176-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	176-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	176-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	176-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	176-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 23)
	fldl	184-BOFF(pB0)
	fldl	184-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	184-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	184-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	184-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	184-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	184-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 24)
	fldl	192-BOFF(pB0)
	fldl	192-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	192-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	192-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	192-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	192-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	192-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif
                                        prefC(48(pC0))
                                        pref2(0(pfA))
                                        addl    $60, pfA
                                        #ifdef DCPLX
                                            prefC(112(pC0))
                                        #endif
	ALIGN8

#if (KB > 25)
	fldl	200-BOFF(pB0)
	fldl	200-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	200-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	200-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	200-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	200-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	200-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 26)
	fldl	208-BOFF(pB0)
	fldl	208-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	208-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	208-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	208-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	208-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	208-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 27)
	fldl	216-BOFF(pB0)
	fldl	216-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	216-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	216-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	216-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	216-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	216-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 28)
	fldl	224-BOFF(pB0)
	fldl	224-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	224-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	224-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	224-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	224-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	224-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 29)
	fldl	232-BOFF(pB0)
	fldl	232-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	232-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	232-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	232-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	232-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	232-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

/*
 *End KLOOP
 */
/*
 *      Write results back to C
 */
        fstpl   0(pC0)
        fstpl   CMUL(8)(pC0)
                lea     (pA3, ldab, 2), pA0
        fstpl   CMUL(16)(pC0)
        fstpl   CMUL(24)(pC0)
                lea     (pA0, ldab, 4), pA3
        fstpl   CMUL(32)(pC0)
        fstpl   CMUL(40)(pC0)
        addl    ldab, pA0
        addl    $CMUL(48), pC0
#endif
#if (MB > 12)
/*KLOOP: */
#ifdef BETA0
	fldl	0-BOFF(pB0)
	fldl    0-BOFF(pA3,ldab,2)
	fmul	%st(1), %st
        fxch
	fldl	0-BOFF(pA3,ldab)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fldl	0-BOFF(pA3)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fldl	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fldl	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fmull	0-BOFF(pA0)
        ALIGN8
#elif defined(BETA1)
	fldl	0-BOFF(pB0)
	fldl	0-BOFF(pA3,ldab)
	fmul	%st(1), %st
        faddl   CMUL(32)(pC0)
	fldl	0-BOFF(pA3)
	fmul	%st(2), %st
        faddl   CMUL(24)(pC0)
	fldl	0-BOFF(pA0,ldab,2)
	fmul	%st(3), %st
        faddl   CMUL(16)(pC0)
	fldl	0-BOFF(pA0,ldab)
	fmul	%st(4), %st
        faddl   CMUL(8)(pC0)
	fldl	0-BOFF(pA0)
	fmul	%st(5), %st
        faddl   0(pC0)
	fldl    0-BOFF(pA3,ldab,2)
	fmul	%st(6), %st
        faddl   CMUL(40)(pC0)
        fstp    %st(6)
#else
        fldl    BETAOFF(%esp)
        fldl    CMUL(32)(pC0)
        fmul    %st(1), %st

        fldl    CMUL(24)(pC0)
        fmul    %st(2), %st
        fldl    CMUL(16)(pC0)
        fmul    %st(3), %st
        fldl    CMUL(8)(pC0)
        fmul    %st(4), %st
        ALIGN8
        fldl    0(pC0)
        fmul    %st(5), %st
        fldl    CMUL(40)(pC0)
        fmul    %st(6), %st
        fxch    %st(6)
        fstp    %st
        ALIGN8
	fldl	0-BOFF(pB0)
	fldl	0-BOFF(pA0)
	fmul	%st(1), %st

	faddp	%st, %st(2)
	fldl	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(3)
	fldl	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(4)
	fldl	0-BOFF(pA3)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(5)
	fldl	0-BOFF(pA3,ldab)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(6)
	fmull   0-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
        ALIGN8
#endif

#if (KB > 1)
	fldl	8-BOFF(pB0)
	fldl	8-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	8-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	8-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	8-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	8-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	8-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 2)
	fldl	16-BOFF(pB0)
	fldl	16-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	16-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	16-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	16-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	16-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	16-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 3)
	fldl	24-BOFF(pB0)
	fldl	24-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	24-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	24-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	24-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	24-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	24-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 4)
	fldl	32-BOFF(pB0)
	fldl	32-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	32-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	32-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	32-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	32-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	32-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 5)
	fldl	40-BOFF(pB0)
	fldl	40-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	40-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	40-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	40-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	40-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	40-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 6)
	fldl	48-BOFF(pB0)
	fldl	48-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	48-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	48-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	48-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	48-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	48-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 7)
	fldl	56-BOFF(pB0)
	fldl	56-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	56-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	56-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	56-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	56-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	56-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 8)
	fldl	64-BOFF(pB0)
	fldl	64-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	64-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	64-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	64-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	64-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	64-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 9)
	fldl	72-BOFF(pB0)
	fldl	72-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	72-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	72-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	72-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	72-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	72-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 10)
	fldl	80-BOFF(pB0)
	fldl	80-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	80-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	80-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	80-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	80-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	80-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 11)
	fldl	88-BOFF(pB0)
	fldl	88-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	88-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	88-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	88-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	88-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	88-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 12)
	fldl	96-BOFF(pB0)
	fldl	96-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	96-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	96-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	96-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	96-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	96-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 13)
	fldl	104-BOFF(pB0)
	fldl	104-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	104-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	104-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	104-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	104-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	104-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 14)
	fldl	112-BOFF(pB0)
	fldl	112-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	112-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	112-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	112-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	112-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	112-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 15)
	fldl	120-BOFF(pB0)
	fldl	120-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	120-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	120-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	120-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	120-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	120-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 16)
	fldl	128-BOFF(pB0)
	fldl	128-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	128-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	128-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	128-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	128-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	128-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 17)
	fldl	136-BOFF(pB0)
	fldl	136-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	136-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	136-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	136-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	136-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	136-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 18)
	fldl	144-BOFF(pB0)
	fldl	144-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	144-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	144-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	144-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	144-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	144-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 19)
	fldl	152-BOFF(pB0)
	fldl	152-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	152-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	152-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	152-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	152-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	152-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 20)
	fldl	160-BOFF(pB0)
	fldl	160-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	160-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	160-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	160-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	160-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	160-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 21)
	fldl	168-BOFF(pB0)
	fldl	168-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	168-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	168-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	168-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	168-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	168-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 22)
	fldl	176-BOFF(pB0)
	fldl	176-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	176-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	176-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	176-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	176-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	176-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 23)
	fldl	184-BOFF(pB0)
	fldl	184-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	184-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	184-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	184-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	184-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	184-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 24)
	fldl	192-BOFF(pB0)
	fldl	192-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	192-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	192-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	192-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	192-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	192-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif
                                        prefC(48(pC0))
                                        pref2((pfA))
                                        addl    $60, pfA
                                        #ifdef DCPLX
                                            prefC(112(pC0))
                                        #endif
	ALIGN8

#if (KB > 25)
	fldl	200-BOFF(pB0)
	fldl	200-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	200-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	200-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	200-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	200-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	200-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 26)
	fldl	208-BOFF(pB0)
	fldl	208-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	208-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	208-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	208-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	208-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	208-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 27)
	fldl	216-BOFF(pB0)
	fldl	216-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	216-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	216-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	216-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	216-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	216-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 28)
	fldl	224-BOFF(pB0)
	fldl	224-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	224-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	224-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	224-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	224-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	224-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 29)
	fldl	232-BOFF(pB0)
	fldl	232-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	232-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	232-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	232-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	232-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	232-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

/*
 *End KLOOP
 */
/*
 *      Write results back to C
 */
        fstpl   0(pC0)
        fstpl   CMUL(8)(pC0)
                lea     (pA3, ldab, 2), pA0
        fstpl   CMUL(16)(pC0)
        fstpl   CMUL(24)(pC0)
                lea     (pA0, ldab, 4), pA3
        fstpl   CMUL(32)(pC0)
        fstpl   CMUL(40)(pC0)
        addl    ldab, pA0
        addl    $CMUL(48), pC0
#endif
#if (MB > 18)
/*KLOOP: */
#ifdef BETA0
	fldl	0-BOFF(pB0)
	fldl    0-BOFF(pA3,ldab,2)
	fmul	%st(1), %st
        fxch
	fldl	0-BOFF(pA3,ldab)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fldl	0-BOFF(pA3)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fldl	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fldl	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fmull	0-BOFF(pA0)
        ALIGN8
#elif defined(BETA1)
	fldl	0-BOFF(pB0)
	fldl	0-BOFF(pA3,ldab)
	fmul	%st(1), %st
        faddl   CMUL(32)(pC0)
	fldl	0-BOFF(pA3)
	fmul	%st(2), %st
        faddl   CMUL(24)(pC0)
	fldl	0-BOFF(pA0,ldab,2)
	fmul	%st(3), %st
        faddl   CMUL(16)(pC0)
	fldl	0-BOFF(pA0,ldab)
	fmul	%st(4), %st
        faddl   CMUL(8)(pC0)
	fldl	0-BOFF(pA0)
	fmul	%st(5), %st
        faddl   0(pC0)
	fldl    0-BOFF(pA3,ldab,2)
	fmul	%st(6), %st
        faddl   CMUL(40)(pC0)
        fstp    %st(6)
#else
        fldl    BETAOFF(%esp)
        fldl    CMUL(32)(pC0)
        fmul    %st(1), %st

        fldl    CMUL(24)(pC0)
        fmul    %st(2), %st
        fldl    CMUL(16)(pC0)
        fmul    %st(3), %st
        fldl    CMUL(8)(pC0)
        fmul    %st(4), %st
        ALIGN8
        fldl    0(pC0)
        fmul    %st(5), %st
        fldl    CMUL(40)(pC0)
        fmul    %st(6), %st
        fxch    %st(6)
        fstp    %st
        ALIGN8
	fldl	0-BOFF(pB0)
	fldl	0-BOFF(pA0)
	fmul	%st(1), %st

	faddp	%st, %st(2)
	fldl	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(3)
	fldl	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(4)
	fldl	0-BOFF(pA3)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(5)
	fldl	0-BOFF(pA3,ldab)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(6)
	fmull   0-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
        ALIGN8
#endif

#if (KB > 1)
	fldl	8-BOFF(pB0)
	fldl	8-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	8-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	8-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	8-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	8-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	8-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 2)
	fldl	16-BOFF(pB0)
	fldl	16-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	16-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	16-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	16-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	16-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	16-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 3)
	fldl	24-BOFF(pB0)
	fldl	24-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	24-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	24-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	24-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	24-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	24-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 4)
	fldl	32-BOFF(pB0)
	fldl	32-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	32-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	32-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	32-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	32-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	32-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 5)
	fldl	40-BOFF(pB0)
	fldl	40-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	40-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	40-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	40-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	40-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	40-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 6)
	fldl	48-BOFF(pB0)
	fldl	48-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	48-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	48-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	48-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	48-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	48-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 7)
	fldl	56-BOFF(pB0)
	fldl	56-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	56-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	56-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	56-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	56-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	56-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 8)
	fldl	64-BOFF(pB0)
	fldl	64-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	64-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	64-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	64-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	64-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	64-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 9)
	fldl	72-BOFF(pB0)
	fldl	72-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	72-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	72-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	72-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	72-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	72-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 10)
	fldl	80-BOFF(pB0)
	fldl	80-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	80-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	80-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	80-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	80-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	80-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 11)
	fldl	88-BOFF(pB0)
	fldl	88-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	88-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	88-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	88-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	88-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	88-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 12)
	fldl	96-BOFF(pB0)
	fldl	96-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	96-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	96-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	96-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	96-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	96-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 13)
	fldl	104-BOFF(pB0)
	fldl	104-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	104-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	104-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	104-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	104-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	104-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 14)
	fldl	112-BOFF(pB0)
	fldl	112-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	112-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	112-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	112-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	112-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	112-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 15)
	fldl	120-BOFF(pB0)
	fldl	120-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	120-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	120-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	120-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	120-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	120-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 16)
	fldl	128-BOFF(pB0)
	fldl	128-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	128-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	128-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	128-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	128-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	128-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 17)
	fldl	136-BOFF(pB0)
	fldl	136-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	136-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	136-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	136-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	136-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	136-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 18)
	fldl	144-BOFF(pB0)
	fldl	144-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	144-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	144-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	144-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	144-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	144-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 19)
	fldl	152-BOFF(pB0)
	fldl	152-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	152-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	152-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	152-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	152-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	152-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 20)
	fldl	160-BOFF(pB0)
	fldl	160-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	160-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	160-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	160-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	160-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	160-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 21)
	fldl	168-BOFF(pB0)
	fldl	168-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	168-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	168-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	168-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	168-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	168-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 22)
	fldl	176-BOFF(pB0)
	fldl	176-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	176-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	176-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	176-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	176-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	176-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 23)
	fldl	184-BOFF(pB0)
	fldl	184-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	184-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	184-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	184-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	184-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	184-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 24)
	fldl	192-BOFF(pB0)
	fldl	192-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	192-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	192-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	192-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	192-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	192-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif
                                        prefC(48(pC0))
                                        pref2((pfA))
                                        addl    $60, pfA
                                        #ifdef DCPLX
                                            prefC(112(pC0))
                                        #endif
	ALIGN8

#if (KB > 25)
	fldl	200-BOFF(pB0)
	fldl	200-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	200-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	200-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	200-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	200-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	200-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 26)
	fldl	208-BOFF(pB0)
	fldl	208-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	208-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	208-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	208-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	208-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	208-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 27)
	fldl	216-BOFF(pB0)
	fldl	216-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	216-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	216-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	216-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	216-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	216-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 28)
	fldl	224-BOFF(pB0)
	fldl	224-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	224-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	224-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	224-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	224-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	224-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 29)
	fldl	232-BOFF(pB0)
	fldl	232-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	232-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	232-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	232-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	232-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	232-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

/*
 *End KLOOP
 */
/*
 *      Write results back to C
 */
        fstpl   0(pC0)
        fstpl   CMUL(8)(pC0)
                lea     (pA3, ldab, 2), pA0
        fstpl   CMUL(16)(pC0)
        fstpl   CMUL(24)(pC0)
                lea     (pA0, ldab, 4), pA3
        fstpl   CMUL(32)(pC0)
        fstpl   CMUL(40)(pC0)
        addl    ldab, pA0
        addl    $CMUL(48), pC0
/*KLOOP: */
#endif
#if (MB > 24)
#ifdef BETA0
	fldl	0-BOFF(pB0)
	fldl    0-BOFF(pA3,ldab,2)
	fmul	%st(1), %st
        fxch
	fldl	0-BOFF(pA3,ldab)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fldl	0-BOFF(pA3)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fldl	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fldl	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fmull	0-BOFF(pA0)
        ALIGN8
#elif defined(BETA1)
	fldl	0-BOFF(pB0)
	fldl	0-BOFF(pA3,ldab)
	fmul	%st(1), %st
        faddl   CMUL(32)(pC0)
	fldl	0-BOFF(pA3)
	fmul	%st(2), %st
        faddl   CMUL(24)(pC0)
	fldl	0-BOFF(pA0,ldab,2)
	fmul	%st(3), %st
        faddl   CMUL(16)(pC0)
	fldl	0-BOFF(pA0,ldab)
	fmul	%st(4), %st
        faddl   CMUL(8)(pC0)
	fldl	0-BOFF(pA0)
	fmul	%st(5), %st
        faddl   0(pC0)
	fldl    0-BOFF(pA3,ldab,2)
	fmul	%st(6), %st
        faddl   CMUL(40)(pC0)
        fstp    %st(6)
#else
        fldl    BETAOFF(%esp)
        fldl    CMUL(32)(pC0)
        fmul    %st(1), %st

        fldl    CMUL(24)(pC0)
        fmul    %st(2), %st
        fldl    CMUL(16)(pC0)
        fmul    %st(3), %st
        fldl    CMUL(8)(pC0)
        fmul    %st(4), %st
        ALIGN8
        fldl    0(pC0)
        fmul    %st(5), %st
        fldl    CMUL(40)(pC0)
        fmul    %st(6), %st
        fxch    %st(6)
        fstp    %st
        ALIGN8
	fldl	0-BOFF(pB0)
	fldl	0-BOFF(pA0)
	fmul	%st(1), %st

	faddp	%st, %st(2)
	fldl	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(3)
	fldl	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(4)
	fldl	0-BOFF(pA3)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(5)
	fldl	0-BOFF(pA3,ldab)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(6)
	fmull   0-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
        ALIGN8
#endif

#if (KB > 1)
	fldl	8-BOFF(pB0)
	fldl	8-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	8-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	8-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	8-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	8-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	8-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 2)
	fldl	16-BOFF(pB0)
	fldl	16-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	16-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	16-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	16-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	16-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	16-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 3)
	fldl	24-BOFF(pB0)
	fldl	24-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	24-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	24-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	24-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	24-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	24-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 4)
	fldl	32-BOFF(pB0)
	fldl	32-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	32-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	32-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	32-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	32-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	32-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 5)
	fldl	40-BOFF(pB0)
	fldl	40-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	40-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	40-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	40-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	40-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	40-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 6)
	fldl	48-BOFF(pB0)
	fldl	48-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	48-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	48-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	48-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	48-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	48-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 7)
	fldl	56-BOFF(pB0)
	fldl	56-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	56-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	56-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	56-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	56-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	56-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 8)
	fldl	64-BOFF(pB0)
	fldl	64-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	64-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	64-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	64-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	64-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	64-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 9)
	fldl	72-BOFF(pB0)
	fldl	72-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	72-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	72-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	72-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	72-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	72-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 10)
	fldl	80-BOFF(pB0)
	fldl	80-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	80-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	80-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	80-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	80-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	80-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 11)
	fldl	88-BOFF(pB0)
	fldl	88-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	88-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	88-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	88-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	88-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	88-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 12)
	fldl	96-BOFF(pB0)
	fldl	96-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	96-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	96-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	96-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	96-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	96-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 13)
	fldl	104-BOFF(pB0)
	fldl	104-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	104-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	104-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	104-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	104-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	104-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 14)
	fldl	112-BOFF(pB0)
	fldl	112-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	112-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	112-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	112-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	112-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	112-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 15)
	fldl	120-BOFF(pB0)
	fldl	120-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	120-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	120-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	120-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	120-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	120-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 16)
	fldl	128-BOFF(pB0)
	fldl	128-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	128-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	128-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	128-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	128-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	128-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 17)
	fldl	136-BOFF(pB0)
	fldl	136-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	136-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	136-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	136-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	136-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	136-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 18)
	fldl	144-BOFF(pB0)
	fldl	144-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	144-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	144-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	144-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	144-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	144-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 19)
	fldl	152-BOFF(pB0)
	fldl	152-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	152-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	152-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	152-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	152-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	152-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 20)
	fldl	160-BOFF(pB0)
	fldl	160-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	160-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	160-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	160-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	160-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	160-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 21)
	fldl	168-BOFF(pB0)
	fldl	168-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	168-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	168-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	168-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	168-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	168-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 22)
	fldl	176-BOFF(pB0)
	fldl	176-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	176-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	176-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	176-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	176-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	176-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 23)
	fldl	184-BOFF(pB0)
	fldl	184-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	184-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	184-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	184-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	184-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	184-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 24)
	fldl	192-BOFF(pB0)
	fldl	192-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	192-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	192-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	192-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	192-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	192-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif
                                        prefC(48(pC0))
                                        pref2((pfA))
                                        addl    $60, pfA
                                        #ifdef DCPLX
                                            prefC(112(pC0))
                                        #endif
	ALIGN8

#if (KB > 25)
	fldl	200-BOFF(pB0)
	fldl	200-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	200-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	200-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	200-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	200-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	200-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 26)
	fldl	208-BOFF(pB0)
	fldl	208-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	208-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	208-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	208-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	208-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	208-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 27)
	fldl	216-BOFF(pB0)
	fldl	216-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	216-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	216-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	216-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	216-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	216-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 28)
	fldl	224-BOFF(pB0)
	fldl	224-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	224-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	224-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	224-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	224-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	224-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 29)
	fldl	232-BOFF(pB0)
	fldl	232-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	232-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	232-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	232-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	232-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	232-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

/*
 *End KLOOP
 */
/*
 *      Write results back to C
 */
        fstpl   0(pC0)
        fstpl   CMUL(8)(pC0)
                lea     (pA3, ldab, 2), pA0
        fstpl   CMUL(16)(pC0)
        fstpl   CMUL(24)(pC0)
                lea     (pA0, ldab, 4), pA3
        fstpl   CMUL(32)(pC0)
        fstpl   CMUL(40)(pC0)
        addl    ldab, pA0
        addl    $CMUL(48), pC0

/*        dec     stM */
/*        jnz     MLOOP */
#endif

#ifdef BETA0
	fldl	0-BOFF(pB0)
	fldl    0-BOFF(pA3,ldab,2)
	fmul	%st(1), %st
        fxch
	fldl	0-BOFF(pA3,ldab)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fldl	0-BOFF(pA3)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fldl	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fldl	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
        fxch
	fmull	0-BOFF(pA0)
        ALIGN8
#elif defined(BETA1)
	fldl	0-BOFF(pB0)
	fldl	0-BOFF(pA3,ldab)
	fmul	%st(1), %st
        faddl   CMUL(32)(pC0)
	fldl	0-BOFF(pA3)
	fmul	%st(2), %st
        faddl   CMUL(24)(pC0)
	fldl	0-BOFF(pA0,ldab,2)
	fmul	%st(3), %st
        faddl   CMUL(16)(pC0)
	fldl	0-BOFF(pA0,ldab)
	fmul	%st(4), %st
        faddl   CMUL(8)(pC0)
	fldl	0-BOFF(pA0)
	fmul	%st(5), %st
        faddl   0(pC0)
	fldl    0-BOFF(pA3,ldab,2)
	fmul	%st(6), %st
        faddl   CMUL(40)(pC0)
        fstp    %st(6)
#else
        fldl    BETAOFF(%esp)
        fldl    CMUL(32)(pC0)
        fmul    %st(1), %st

        fldl    CMUL(24)(pC0)
        fmul    %st(2), %st
        fldl    CMUL(16)(pC0)
        fmul    %st(3), %st
        fldl    CMUL(8)(pC0)
        fmul    %st(4), %st
        ALIGN8
        fldl    0(pC0)
        fmul    %st(5), %st
        fldl    CMUL(40)(pC0)
        fmul    %st(6), %st
        fxch    %st(6)
        fstp    %st
        ALIGN8
	fldl	0-BOFF(pB0)
	fldl	0-BOFF(pA0)
	fmul	%st(1), %st

	faddp	%st, %st(2)
	fldl	0-BOFF(pA0,ldab)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(3)
	fldl	0-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(4)
	fldl	0-BOFF(pA3)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(5)
	fldl	0-BOFF(pA3,ldab)
	fmul	%st(1), %st
        ALIGN8
	faddp	%st, %st(6)
	fmull   0-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
        ALIGN8
#endif
#if (KB > 1)
	fldl	8-BOFF(pB0)
	fldl	8-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	8-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	8-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	8-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	8-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	8-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif
                        prefB((pB0,ldab))
	ALIGN8

#if (KB > 2)
	fldl	16-BOFF(pB0)
	fldl	16-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	16-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	16-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	16-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	16-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	16-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 3)
	fldl	24-BOFF(pB0)
	fldl	24-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	24-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	24-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	24-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	24-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	24-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif
                        prefB(64(pB0,ldab))
	ALIGN8

#if (KB > 4)
	fldl	32-BOFF(pB0)
	fldl	32-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	32-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	32-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	32-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	32-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	32-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif
                        prefB(128(pB0,ldab))
        ALIGN8

#if (KB > 5)
	fldl	40-BOFF(pB0)
	fldl	40-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	40-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	40-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	40-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	40-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	40-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 6)
	fldl	48-BOFF(pB0)
	fldl	48-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	48-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	48-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	48-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	48-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	48-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 7)
	fldl	56-BOFF(pB0)
	fldl	56-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	56-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	56-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	56-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	56-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	56-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 8)
	fldl	64-BOFF(pB0)
	fldl	64-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	64-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	64-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	64-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	64-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	64-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 9)
	fldl	72-BOFF(pB0)
	fldl	72-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	72-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	72-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	72-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	72-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	72-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 10)
	fldl	80-BOFF(pB0)
	fldl	80-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	80-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	80-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	80-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	80-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	80-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif
                        addl $72, pB0
                        prefB(120(pB0,ldab))
                        subl $72, pB0
	ALIGN8

#if (KB > 11)
	fldl	88-BOFF(pB0)
	fldl	88-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	88-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	88-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	88-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	88-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	88-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 12)
	fldl	96-BOFF(pB0)
	fldl	96-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	96-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	96-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	96-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	96-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	96-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 13)
	fldl	104-BOFF(pB0)
	fldl	104-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	104-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	104-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	104-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	104-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	104-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 14)
	fldl	112-BOFF(pB0)
	fldl	112-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	112-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	112-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	112-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	112-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	112-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 15)
	fldl	120-BOFF(pB0)
	fldl	120-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	120-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	120-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	120-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	120-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	120-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 16)
	fldl	128-BOFF(pB0)
	fldl	128-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	128-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	128-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	128-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	128-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	128-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 17)
	fldl	136-BOFF(pB0)
	fldl	136-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	136-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	136-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	136-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	136-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	136-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 18)
	fldl	144-BOFF(pB0)
	fldl	144-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	144-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	144-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	144-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	144-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	144-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 19)
	fldl	152-BOFF(pB0)
	fldl	152-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	152-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	152-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	152-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	152-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	152-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 20)
	fldl	160-BOFF(pB0)
	fldl	160-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	160-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	160-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	160-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	160-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	160-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 21)
	fldl	168-BOFF(pB0)
	fldl	168-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	168-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	168-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	168-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	168-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	168-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 22)
	fldl	176-BOFF(pB0)
	fldl	176-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	176-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	176-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	176-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	176-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	176-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif
        addl    8(%esp), pC0
	ALIGN8

#if (KB > 23)
	fldl	184-BOFF(pB0)
	fldl	184-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	184-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	184-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	184-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	184-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	184-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 24)
	fldl	192-BOFF(pB0)
	fldl	192-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	192-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	192-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	192-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	192-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	192-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif
                                        prefC(48(pC0))
                                        #ifdef DCPLX
                                            prefC(112(pC0))
                                        #endif
        subl    8(%esp), pC0
        ALIGN8

#if (KB > 25)
	fldl	200-BOFF(pB0)
	fldl	200-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	200-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	200-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	200-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	200-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	200-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 26)
	fldl	208-BOFF(pB0)
	fldl	208-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	208-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	208-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	208-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	208-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	208-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 27)
	fldl	216-BOFF(pB0)
	fldl	216-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	216-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	216-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	216-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	216-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	216-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 28)
	fldl	224-BOFF(pB0)
	fldl	224-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	224-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	224-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	224-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	224-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	224-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif

#if (KB > 29)
	fldl	232-BOFF(pB0)
	fldl	232-BOFF(pA0)
	fmul	%st(1), %st
	faddp	%st, %st(2)
	fldl	232-BOFF(pA0,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(3)
	fldl	232-BOFF(pA0,ldab,2)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(4)
	fldl	232-BOFF(pA3)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(5)
	fldl	232-BOFF(pA3,ldab)
	fmul	%st(1), %st
	ALIGN8
	faddp	%st, %st(6)
	fmull	232-BOFF(pA3,ldab,2)
	faddp	%st, %st(6)
	ALIGN8
#endif
/*                                pref2(NB*KB*8(pB0)) */
/*                                pref2(64+NB*KB*8(pB0)) */
/* ignore .align 8 */
/*                                pref2(128+NB*KB*8(pB0)) */
/* ignore .align 8 */
/*                                pref2(192+NB*KB*8(pB0)) */
/* ignore .align 8 */

/*
 *End KLOOP
 */
/*
 *      Write results back to C
 */
        fstpl   0(pC0)
        fstpl   CMUL(8)(pC0)
        lea     (pA3, ldab, 2), pA0
        fstpl   CMUL(16)(pC0)
        fstpl   CMUL(24)(pC0)
        lea     (pA0, ldab, 4), pA3
        fstpl   CMUL(32)(pC0)
        fstpl   CMUL(40)(pC0)
        addl    ldab, pA0

        addl    $CMUL(48), pC0
        addl    8(%esp), pC0
        addl    ldab, pB0
        subl    $MB*KB*8, pA0
        dec     stN
        jnz     NLOOP
DONE:
        movl    24(%esp), %ebp
        movl    20(%esp), %ebx
        movl    16(%esp), %esi
        movl    12(%esp), %edi
        addl    $28, %esp
        ret
