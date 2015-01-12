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


/*
 *Efficeon-optimized 4x1x60 SGEMM.  Pipelined to 4 (4 accumulators).
 *Prefetches the next col of B, and a col from the next block of A in the M-loop
 *Purposely kept small so it is retained in cache, and easy to translate when
 *not
 */

#ifndef ATL_GAS_x8632
   #error "This kernel requires gas x86-32 assembler!"
#endif
#ifndef ATL_SSE2
   #error "This routine requires SSE2!"
#endif

/*
#if KB != 60
   #error "KB must be 60!"
#endif
*/
#if !defined(KB) || (KB == 0)
   #error "KB must be a compile-time constant!"
#endif
#if !defined(NB)
   #define NB 0
#endif
#if !defined(MB)
   #define MB 0
#endif
#if (MB/4)*4 != MB
   #error "MB must be multiple of 4!"
#endif

#ifdef SCPLX
   #define OFF 16
   #define CMUL(i_) (2*(i_))
#else
   #define OFF 8
   #define CMUL(i_) i_
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
#define pfB  	%edx
#define pfA     %ebp

#define pA0	pA
#define pB0	pB

#define m0      %xmm0
#define m1      %xmm1
#define m2      %xmm2
#define m3      %xmm3
#define rC0     %xmm4
#define rC1     %xmm5
#define rC2     %xmm6
#define rC3     %xmm7

#define NB0so   0
#define NBso	(KB*4)
#define NB1so	(KB*4)
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
#if MB != 0
   #define MBKBso  (MB*KB*4)
#endif

/*
 * Prefetch defines
 */
#if 1
   #define pref2(mem) prefetcht0	mem
   #define prefB(mem) prefetcht0	mem
   #define prefC(mem) prefetcht0	mem
#else
   #define pref2(mem)
   #define prefB(mem)
   #define prefC(mem)
#endif
/*offset                    4            8           12                16
 *void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha,
 *offset                     20             24             28            32
 *                const TYPE *A, const int lda, const TYPE *B, const int ldb,
 *offset                       36       40             44
 *                const TYPE beta, TYPE *C, const int ldc)
 */
	.text
.global ATL_asmdecor(ATL_USERMM)
ATL_asmdecor(ATL_USERMM):
/*
 *      Save callee-saved iregs; Save old stack pointer in eax,
 *      so we can adjust for BETA alignment
 */
#define FSIZE 28
#define BETAOFF FSIZE+36
#define COFF 16
	subl	$FSIZE, %esp
	movl	%ebp, 12(%esp)
	movl	%ebx,  8(%esp)
	movl	%esi,  4(%esp)
	movl	%edi,   (%esp)
/*
 *      Initialize pA = A;  pB = B; pC = C;
 */
#if MB == 0
        movl    FSIZE+4(%esp), %ebx
        movl    %ebx, COFF+4(%esp)
        imul    $NBso, %ebx
        movl    %ebx, COFF+8(%esp)
#endif
	movl	FSIZE+20(%esp), pA
	movl	FSIZE+28(%esp), pB
	movl	FSIZE+40(%esp), pC
#if NB == 0
        movb    FSIZE+8(%esp), stN
#else
        movb    $NB, stN
#endif
/*
 *      Set incCn = (ldc - MB)*sizeof
 */
	movl	FSIZE+44(%esp), incCn
   #if MB == 0
        subl    COFF+4(%esp), incCn
   #else
	subl	$MB, incCn
   #endif
   #ifdef SCPLX
	shl	$3, incCn
   #else
	shl	$2, incCn
   #endif
/*   	movl	incCn, COFF(%esp) */
        movl    pA0, pfA
#if MB == 0
        addl    COFF+8(%esp), pfA
#else
        addl    $MBKBso, pfA
#endif
        addl    $120, pA0
        addl    $120, pB0
NLOOP:
#if MB == 0
        movb    COFF+4(%esp), stM
#else
        movb     $MB, stM
#endif
        lea     120+NBso(pB0), pfB
MLOOP:
#ifdef BETA0
	xorps	rC0, rC0
	xorps	rC1, rC1
	xorps	rC2, rC2
	xorps	rC3, rC3
#else
	movss	(pC), rC0
	movss	CMUL(4)(pC), rC1
	movss	CMUL(8)(pC), rC2
	movss	CMUL(12)(pC), rC3
   #ifdef BETAX
	movss	BETAOFF(%esp), m0
	mulss	m0, rC0
	mulss	m0, rC1
	mulss	m0, rC2
	mulss	m0, rC3
   #endif
#endif
	movaps	0-120(pB0), m3
	movaps	0-120(pA0), m0
	movaps	NBso+0-120(pA0), m1
	movaps	NB2so+0-120(pA0), m2
	mulps	m3, m0
	mulps	m3, m1
	mulps	m3, m2
/*
 *      Unrolled & pipelined K-loop
 */
	mulps	NB3so+0-120(pA0), m3
	addps	m0, rC0
	movaps	16-120(pB0), m0
	addps	m1, rC1
	movaps	NB0so+16-120(pA0), m1
	mulps	m0, m1
	addps	m2, rC2
	movaps	NB1so+16-120(pA0), m2
	mulps	m0, m2
	addps	m3, rC3
	movaps	NB2so+16-120(pA0), m3
	mulps	m0, m3

	mulps	NB3so+16-120(pA0), m0
	addps	m1, rC0
	movaps	32-120(pB0), m1
	addps	m2, rC1
	movaps	NB0so+32-120(pA0), m2
	mulps	m1, m2
	addps	m3, rC2
	movaps	NB1so+32-120(pA0), m3
	mulps	m1, m3
	addps	m0, rC3
	movaps	NB2so+32-120(pA0), m0
	mulps	m1, m0

	mulps	NB3so+32-120(pA0), m1
	addps	m2, rC0
	movaps	48-120(pB0), m2
	addps	m3, rC1
	movaps	NB0so+48-120(pA0), m3
	mulps	m2, m3
	addps	m0, rC2
	movaps	NB1so+48-120(pA0), m0
	mulps	m2, m0
	addps	m1, rC3
	movaps	NB2so+48-120(pA0), m1
	mulps	m2, m1

	mulps	NB3so+48-120(pA0), m2
	addps	m3, rC0
	movaps	64-120(pB0), m3
	addps	m0, rC1
	movaps	NB0so+64-120(pA0), m0
	mulps	m3, m0
	addps	m1, rC2
	movaps	NB1so+64-120(pA0), m1
	mulps	m3, m1
	addps	m2, rC3
	movaps	NB2so+64-120(pA0), m2
	mulps	m3, m2

	mulps	NB3so+64-120(pA0), m3
	addps	m0, rC0
	movaps	80-120(pB0), m0
	addps	m1, rC1
	movaps	NB0so+80-120(pA0), m1
	mulps	m0, m1
	addps	m2, rC2
	movaps	NB1so+80-120(pA0), m2
	mulps	m0, m2
	addps	m3, rC3
	movaps	NB2so+80-120(pA0), m3
	mulps	m0, m3

	mulps	NB3so+80-120(pA0), m0
	addps	m1, rC0
	movaps	96-120(pB0), m1
	addps	m2, rC1
	movaps	NB0so+96-120(pA0), m2
	mulps	m1, m2
	addps	m3, rC2
	movaps	NB1so+96-120(pA0), m3
	mulps	m1, m3
	addps	m0, rC3
	movaps	NB2so+96-120(pA0), m0
	mulps	m1, m0

	mulps	NB3so+96-120(pA0), m1
	addps	m2, rC0
	movaps	112-120(pB0), m2
	addps	m3, rC1
	movaps	NB0so+112-120(pA0), m3
	mulps	m2, m3
	addps	m0, rC2
	movaps	NB1so+112-120(pA0), m0
	mulps	m2, m0
	addps	m1, rC3
	movaps	NB2so+112-120(pA0), m1
	mulps	m2, m1

	mulps	NB3so+112-120(pA0), m2
	addps	m3, rC0
	movaps	128-120(pB0), m3
	addps	m0, rC1
	movaps	NB0so+128-120(pA0), m0
	mulps	m3, m0
	addps	m1, rC2
	movaps	NB1so+128-120(pA0), m1
	mulps	m3, m1
	addps	m2, rC3
	movaps	NB2so+128-120(pA0), m2
	mulps	m3, m2

	mulps	NB3so+128-120(pA0), m3
	addps	m0, rC0
	movaps	144-120(pB0), m0
	addps	m1, rC1
	movaps	NB0so+144-120(pA0), m1
	mulps	m0, m1
	addps	m2, rC2
	movaps	NB1so+144-120(pA0), m2
	mulps	m0, m2
	addps	m3, rC3
	movaps	NB2so+144-120(pA0), m3
	mulps	m0, m3

	mulps	NB3so+144-120(pA0), m0
	addps	m1, rC0
	movaps	160-120(pB0), m1
	addps	m2, rC1
	movaps	NB0so+160-120(pA0), m2
	mulps	m1, m2
	addps	m3, rC2
	movaps	NB1so+160-120(pA0), m3
	mulps	m1, m3
	addps	m0, rC3
	movaps	NB2so+160-120(pA0), m0
	mulps	m1, m0

	mulps	NB3so+160-120(pA0), m1
	addps	m2, rC0
	movaps	176-120(pB0), m2
	addps	m3, rC1
	movaps	NB0so+176-120(pA0), m3
	mulps	m2, m3
	addps	m0, rC2
	movaps	NB1so+176-120(pA0), m0
	mulps	m2, m0
	addps	m1, rC3
	movaps	NB2so+176-120(pA0), m1
	mulps	m2, m1

	mulps	NB3so+176-120(pA0), m2
	addps	m3, rC0
	movaps	192-120(pB0), m3
	addps	m0, rC1
	movaps	NB0so+192-120(pA0), m0
	mulps	m3, m0
	addps	m1, rC2
	movaps	NB1so+192-120(pA0), m1
	mulps	m3, m1
	addps	m2, rC3
	movaps	NB2so+192-120(pA0), m2
	mulps	m3, m2

	mulps	NB3so+192-120(pA0), m3
	addps	m0, rC0
	movaps	208-120(pB0), m0
	addps	m1, rC1
	movaps	NB0so+208-120(pA0), m1
	mulps	m0, m1
	addps	m2, rC2
	movaps	NB1so+208-120(pA0), m2
	mulps	m0, m2
	addps	m3, rC3
	movaps	NB2so+208-120(pA0), m3
	mulps	m0, m3

	mulps	NB3so+208-120(pA0), m0
	addps	m1, rC0
	movaps	224-120(pB0), m1
	addps	m2, rC1
	movaps	NB0so+224-120(pA0), m2
	mulps	m1, m2
	addps	m3, rC2
	movaps	NB1so+224-120(pA0), m3
	mulps	m1, m3
	addps	m0, rC3
	movaps	NB2so+224-120(pA0), m0
	mulps	m1, m0

	mulps	NB3so+224-120(pA0), m1
	addps	m2, rC0
	addps	m3, rC1
	addps	m0, rC2
	addps	m1, rC3

/*
 *      Get these bastard things summed up correctly
 *      Note this summation is Camm's, as his sequence was faster
 *      than the piece of crap I came up with
 */
        movaps          rC0, m0        /* m0 = c0d    c0c    c0b    c0a */
        unpcklps        rC1, rC0        /* rC0 = c1b    c0b    c1a    c0d */
        movaps          rC2, m1        /* m1 = c2d    c2c    c2b    c2a */
        unpckhps        rC1, m0        /* m0 = c1d    c0d    c1c    c0c */
                                        prefB((pfB))
        unpcklps        rC3, rC2        /* rC2 = c3b    c2b    c3a    c2a */
                                        addl    $16, pfB
        addps           m0, rC0        /* rC0 = c1bd   c0bd   c1ac   c0ac */
        unpckhps        rC3, m1        /* m1 = c3d    c2d    c3c    c2c */
	                                addl	$NB4so, pA0
        movaps          rC0, m0        /* m0 = c1bd   c0bd   c1ac   c0ac */
        addps           m1, rC2        /* rC2 = c3bd   c2bd   c3ac   c2ac */
        shufps          $0x44,rC2,rC0   /* rC0 = c3ac   c2ac   c1ac   c0ac */
                                        pref2((pfA))
        shufps          $0xEE,rC2,m0   /* m0 = c3bd   c2bd   c1bd   c0bd */
                                        addl    $16, pfA
        addps           m0, rC0        /* rC0 = c3abcd c2abcd c1abcd c0abcd */
                                        /* rC1 = c1a  c1b */
                                        /* rC2 = c2a  c2b */
/*
 *      Write results back to C
 */
   #ifdef SCPLX
                                        /* rC0 = c3 c2 c1 c0 */
        pshufd  $0xB1, rC0, rC1         /* rC1 = c2 c3 c0 c1 */
        movhlps rC0, rC2                /* rC2 =  X  X c3 c2 */
        movhlps rC1, rC3                /* rC3 =  X  X c2 c3 */
        movss   rC0, (pC)
        movss   rC1, 8(pC)
        movss   rC2, 16(pC)
        movss   rC3, 24(pC)
   #else
        movups  rC0, (pC)
   #endif
/*
 *      pC += 6;  pA += 2*NB
 */
	addl	$CMUL(16), pC
/*
 *      while (pA != stM);
 */
	subb	$4, stM
	jnz	MLOOP
/*
 *      pC += incCn;  pA -= NBNB;  pB += NB;
 */
	addl	incCn, pC
/*   	addl	COFF(%esp), pC */
   #if MB == 0
        subl    COFF+8(%esp), pA0
   #else
        subl    $MBKBso, pA0
   #endif
        addl    $NBso, pB
/*
 *      while (pB != stN);
 */
	sub	$1, stN
	jnz	NLOOP

/*
 *      Restore callee-saved iregs
 */
	movl	12(%esp), %ebp
	movl	 8(%esp), %ebx
	movl	 4(%esp), %esi
	movl	  (%esp), %edi
	addl	$FSIZE, %esp
	ret
