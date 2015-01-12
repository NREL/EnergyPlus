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
 *Efficeon-optimized 4x1x44 DGEMM.  Pipelined to 4 (4 accumulators).
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

#if KB != 44
   #error "KB must be 44!"
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
#if (MB/4)*4 != MB
   #error "MB must be multiple of 4!"
#endif

#ifdef DCPLX
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
#define pA      %ebp
#define pB      %edi
#define incCn   %eax
#define stM	%bl
#define stN	%bh
#define pfB  	%edx
#define pfA     %ecx

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
#define NBso	(KB*8)
#define NB1so	(KB*8)
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
   #define MBKBso  (MB*KB*8)
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
#ifdef BETAX
        movl    %esp, %eax
        subl    $48, %esp
        andl    $0xFFFFFFF0, %esp
   #define BETAOFF
   #define COFF 36
   #define MOFF 40
   #define MKOFF 44
	movl	%ebp, 32(%esp)
	movl	%ebx, 28(%esp)
	movl	%esi, 24(%esp)
	movl	%edi, 20(%esp)
	movl	%eax, 16(%esp)

        movlpd  40(%eax), rC0
        unpcklpd        rC0, rC0
        movapd  rC0, BETAOFF(%esp)
#else
   #define FSIZE 28
   #define BETAOFF FSIZE+40
   #define COFF 16
   #define MOFF FSIZE+4
   #define MKOFF COFF+8
	subl	$FSIZE, %esp
	movl	%ebp, 12(%esp)
	movl	%ebx,  8(%esp)
	movl	%esi,  4(%esp)
	movl	%edi,   (%esp)
#endif
/*
 *      Initialize pA = A;  pB = B; pC = C;
 */
#if MB == 0
   #ifdef BETAX
        movl    4(%eax), %ebx
        movl    %ebx, MOFF(%esp)
   #else
        movl    MOFF(%esp), %ebx
   #endif
        imul    $NBso, %ebx
        movl    %ebx, MKOFF(%esp)
#endif
#ifdef BETAX
	movl	24(%eax), pA
	movl	32(%eax), pB
	movl	48(%eax), pC
   #if NB == 0
        movb    8(%eax), stN
   #else
        movb    $NB, stN
   #endif
	movl	52(%eax), incCn
#else
	movl	FSIZE+24(%esp), pA
	movl	FSIZE+32(%esp), pB
	movl	FSIZE+48(%esp), pC
   #if NB == 0
        movb    FSIZE+8(%esp), stN
   #else
        movb    $NB, stN
   #endif
	movl	FSIZE+52(%esp), incCn
#endif
/*
 *      Set incCn = (ldc - MB)*sizeof
 */
   #if MB == 0
        subl    MOFF(%esp), incCn
   #else
	subl	$MB, incCn
   #endif
   #ifdef DCPLX
	shl	$4, incCn
   #else
	shl	$3, incCn
   #endif
/*   	movl	incCn, COFF(%esp) */
        movl    pA0, pfA
#if MB == 0
        addl    MKOFF(%esp), pfA
#else
        addl    $MBKBso, pfA
#endif
        addl    $120, pA0
        addl    $120, pB0
NLOOP:
#if MB == 0
        movb    MOFF(%esp), stM
#else
        movb     $MB, stM
#endif
        lea     120+NBso(pB0), pfB
#if MB != -5
MLOOP:
/*
 *      Unrolled & pipelined K-loop
 */
#ifdef BETA1
   #ifdef DCPLX
	movsd   (pC), rC0
	movsd   16(pC), rC1
	movapd	0-120(pB0), rC3
	movapd	0-120(pA0), m0
	movapd	NBso+0-120(pA0), m1
	movapd	NB2so+0-120(pA0), m2
	mulpd	rC3, m0
	mulpd	rC3, m1
	mulpd	rC3, m2
        movsd   32(pC), rC2
   #else
	movsd   (pC), rC0
	movsd   16(pC), rC2
	movapd	0-120(pB0), rC3
	movapd	0-120(pA0), m0
	movapd	NBso+0-120(pA0), m1
	movapd	NB2so+0-120(pA0), m2
	mulpd	rC3, m0
	mulpd	rC3, m1
	mulpd	rC3, m2
        movsd   8(pC), rC1
   #endif

	mulpd	NB3so+0-120(pA0), rC3
	addpd	m0, rC0
	movapd	16-120(pB0), m0
	addpd	m1, rC1
	movapd	NB0so+16-120(pA0), m1
	mulpd	m0, m1
        addpd   m2, rC2
	movapd	NB1so+16-120(pA0), m2
	mulpd	m0, m2
	movapd	NB2so+16-120(pA0), m3
	mulpd	m0, m3
#else
        nop
        nop
        nop
        nop
        movapd  0-120(pB0), rC3
        movapd  16-120(pB0), m0
        ALIGN4
        movapd  0-120(pA0), rC0
        movapd  NBso+0-120(pA0), rC1
        movapd  NB2so+0-120(pA0), rC2
        mulpd   rC3, rC0
        mulpd   rC3, rC1
        mulpd   rC3, rC2

        mulpd   NB3so+0-120(pA0), rC3
        movapd  NB0so+16-120(pA0), m1
        mulpd   m0, m1
        movapd  NB1so+16-120(pA0), m2
        mulpd   m0, m2
        movapd  NB2so+16-120(pA0), m3
        mulpd   m0, m3
#endif

	mulpd	NB3so+16-120(pA0), m0
	addpd	m1, rC0
	movapd	32-120(pB0), m1
	addpd	m2, rC1
	movapd	NB0so+32-120(pA0), m2
	mulpd	m1, m2
	addpd	m3, rC2
	movapd	NB1so+32-120(pA0), m3
	mulpd	m1, m3
	addpd	m0, rC3
	movapd	NB2so+32-120(pA0), m0
	mulpd	m1, m0
   #ifdef BETA100
        addsd   CMUL(16)(pC), rC2
   #endif

	mulpd	NB3so+32-120(pA0), m1
	addpd	m2, rC0
	movapd	48-120(pB0), m2
	addpd	m3, rC1
	movapd	NB0so+48-120(pA0), m3
	mulpd	m2, m3
	addpd	m0, rC2
	movapd	NB1so+48-120(pA0), m0
	mulpd	m2, m0
	addpd	m1, rC3
	movapd	NB2so+48-120(pA0), m1
	mulpd	m2, m1

	mulpd	NB3so+48-120(pA0), m2
	addpd	m3, rC0
	movapd	64-120(pB0), m3
	addpd	m0, rC1
	movapd	NB0so+64-120(pA0), m0
	mulpd	m3, m0
	addpd	m1, rC2
	movapd	NB1so+64-120(pA0), m1
	mulpd	m3, m1
	addpd	m2, rC3
	movapd	NB2so+64-120(pA0), m2
	mulpd	m3, m2

	mulpd	NB3so+64-120(pA0), m3
	addpd	m0, rC0
	movapd	80-120(pB0), m0
	addpd	m1, rC1
	movapd	NB0so+80-120(pA0), m1
	mulpd	m0, m1
	addpd	m2, rC2
	movapd	NB1so+80-120(pA0), m2
	mulpd	m0, m2
	addpd	m3, rC3
	movapd	NB2so+80-120(pA0), m3
	mulpd	m0, m3

	mulpd	NB3so+80-120(pA0), m0
	addpd	m1, rC0
	movapd	96-120(pB0), m1
	addpd	m2, rC1
	movapd	NB0so+96-120(pA0), m2
	mulpd	m1, m2
	addpd	m3, rC2
	movapd	NB1so+96-120(pA0), m3
	mulpd	m1, m3
	addpd	m0, rC3
	movapd	NB2so+96-120(pA0), m0
	mulpd	m1, m0

	mulpd	NB3so+96-120(pA0), m1
	addpd	m2, rC0
	movapd	112-120(pB0), m2
	addpd	m3, rC1
	movapd	NB0so+112-120(pA0), m3
	mulpd	m2, m3
	addpd	m0, rC2
	movapd	NB1so+112-120(pA0), m0
	mulpd	m2, m0
	addpd	m1, rC3
	movapd	NB2so+112-120(pA0), m1
	mulpd	m2, m1

	mulpd	NB3so+112-120(pA0), m2
	addpd	m3, rC0
	movapd	128-120(pB0), m3
	addpd	m0, rC1
	movapd	NB0so+128-120(pA0), m0
	mulpd	m3, m0
	addpd	m1, rC2
	movapd	NB1so+128-120(pA0), m1
	mulpd	m3, m1
	addpd	m2, rC3
	movapd	NB2so+128-120(pA0), m2
	mulpd	m3, m2

	mulpd	NB3so+128-120(pA0), m3
	addpd	m0, rC0
	movapd	144-120(pB0), m0
	addpd	m1, rC1
	movapd	NB0so+144-120(pA0), m1
	mulpd	m0, m1
	addpd	m2, rC2
	movapd	NB1so+144-120(pA0), m2
	mulpd	m0, m2
	addpd	m3, rC3
	movapd	NB2so+144-120(pA0), m3
	mulpd	m0, m3

	mulpd	NB3so+144-120(pA0), m0
	addpd	m1, rC0
	movapd	160-120(pB0), m1
	addpd	m2, rC1
	movapd	NB0so+160-120(pA0), m2
	mulpd	m1, m2
	addpd	m3, rC2
	movapd	NB1so+160-120(pA0), m3
	mulpd	m1, m3
	addpd	m0, rC3
	movapd	NB2so+160-120(pA0), m0
	mulpd	m1, m0

	mulpd	NB3so+160-120(pA0), m1
	addpd	m2, rC0
	movapd	176-120(pB0), m2
	addpd	m3, rC1
	movapd	NB0so+176-120(pA0), m3
	mulpd	m2, m3
	addpd	m0, rC2
	movapd	NB1so+176-120(pA0), m0
	mulpd	m2, m0
	addpd	m1, rC3
	movapd	NB2so+176-120(pA0), m1
	mulpd	m2, m1

	mulpd	NB3so+176-120(pA0), m2
	addpd	m3, rC0
	movapd	192-120(pB0), m3
	addpd	m0, rC1
	movapd	NB0so+192-120(pA0), m0
	mulpd	m3, m0
	addpd	m1, rC2
	movapd	NB1so+192-120(pA0), m1
	mulpd	m3, m1
	addpd	m2, rC3
	movapd	NB2so+192-120(pA0), m2
	mulpd	m3, m2

	mulpd	NB3so+192-120(pA0), m3
	addpd	m0, rC0
	movapd	208-120(pB0), m0
	addpd	m1, rC1
	movapd	NB0so+208-120(pA0), m1
	mulpd	m0, m1
	addpd	m2, rC2
	movapd	NB1so+208-120(pA0), m2
	mulpd	m0, m2
	addpd	m3, rC3
	movapd	NB2so+208-120(pA0), m3
	mulpd	m0, m3

	mulpd	NB3so+208-120(pA0), m0
	addpd	m1, rC0
	movapd	224-120(pB0), m1
	addpd	m2, rC1
	movapd	NB0so+224-120(pA0), m2
	mulpd	m1, m2
	addpd	m3, rC2
	movapd	NB1so+224-120(pA0), m3
	mulpd	m1, m3
	addpd	m0, rC3
	movapd	NB2so+224-120(pA0), m0
	mulpd	m1, m0

	mulpd	NB3so+224-120(pA0), m1
	addpd	m2, rC0
	movapd	240-120(pB0), m2
                                        addl $120, pB0
	addpd	m3, rC1
	movapd	NB0so+240-120(pA0), m3
	mulpd	m2, m3
	addpd	m0, rC2
	movapd	NB1so+240-120(pA0), m0
	mulpd	m2, m0
	addpd	m1, rC3
	movapd	NB2so+240-120(pA0), m1
	mulpd	m2, m1

	mulpd	NB3so+240-120(pA0), m2
                                        addl $120, pA0
	addpd	m3, rC0
	movapd	256-120-120(pB0), m3
	addpd	m0, rC1
	movapd	NB0so+256-120-120(pA0), m0
	mulpd	m3, m0
	addpd	m1, rC2
	movapd	NB1so+256-120-120(pA0), m1
	mulpd	m3, m1
	addpd	m2, rC3
	movapd	NB2so+256-120-120(pA0), m2
	mulpd	m3, m2

	mulpd	NB3so+256-120-120(pA0), m3
	addpd	m0, rC0
	movapd	272-120-120(pB0), m0
	addpd	m1, rC1
	movapd	NB0so+272-120-120(pA0), m1
	mulpd	m0, m1
	addpd	m2, rC2
	movapd	NB1so+272-120-120(pA0), m2
	mulpd	m0, m2
	addpd	m3, rC3
	movapd	NB2so+272-120-120(pA0), m3
	mulpd	m0, m3

	mulpd	NB3so+272-120-120(pA0), m0
	addpd	m1, rC0
	movapd	288-120-120(pB0), m1
	addpd	m2, rC1
	movapd	NB0so+288-120-120(pA0), m2
	mulpd	m1, m2
	addpd	m3, rC2
	movapd	NB1so+288-120-120(pA0), m3
	mulpd	m1, m3
	addpd	m0, rC3
	movapd	NB2so+288-120-120(pA0), m0
	mulpd	m1, m0

	mulpd	NB3so+288-120-120(pA0), m1
	addpd	m2, rC0
	movapd	304-120-120(pB0), m2
	addpd	m3, rC1
	movapd	NB0so+304-120-120(pA0), m3
	mulpd	m2, m3
	addpd	m0, rC2
	movapd	NB1so+304-120-120(pA0), m0
	mulpd	m2, m0
	addpd	m1, rC3
	movapd	NB2so+304-120-120(pA0), m1
	mulpd	m2, m1

	mulpd	NB3so+304-120-120(pA0), m2
	addpd	m3, rC0
	movapd	320-120-120(pB0), m3
	addpd	m0, rC1
	movapd	NB0so+320-120-120(pA0), m0
	mulpd	m3, m0
	addpd	m1, rC2
	movapd	NB1so+320-120-120(pA0), m1
	mulpd	m3, m1
	addpd	m2, rC3
	movapd	NB2so+320-120-120(pA0), m2
	mulpd	m3, m2

	mulpd	NB3so+320-120-120(pA0), m3
	addpd	m0, rC0
	movapd	336-120-120(pB0), m0
	addpd	m1, rC1
	movapd	NB0so+336-120-120(pA0), m1
	mulpd	m0, m1
	addpd	m2, rC2
	movapd	NB1so+336-120-120(pA0), m2
	mulpd	m0, m2
	addpd	m3, rC3
	movapd	NB2so+336-120-120(pA0), m3
	mulpd	m0, m3

	mulpd	NB3so+336-120-120(pA0), m0
#ifdef BETAX
   #ifdef DCPLX
	addpd	m1, rC0
        movlpd  32(pC), m1
	addpd	m2, rC1
        movhpd  48(pC), m1
        movlpd  (pC), m2
	addpd	m3, rC2
                movapd  BETAOFF(%esp), m3
        movhpd  16(pC), m2
	addpd	m0, rC3
        mulpd   m3, m2
        mulpd   m1, m3
   #else
	addpd	m1, rC0
                movapd  BETAOFF(%esp), m1
	addpd	m2, rC1
                movupd  (pC), m2
	addpd	m3, rC2
                movupd  16(pC), m3
	addpd	m0, rC3
        mulpd   m1, m2
        mulpd   m1, m3
   #endif
#else
	addpd	m1, rC0
	addpd	m2, rC1
	addpd	m3, rC2
	addpd	m0, rC3
#endif

/*
 *      Get these bastard things summed up
 */
                                        /* rC0 = c0a  c0b */
                                        /* rC1 = c1a  c1b */
                                        /* rC2 = c2a  c2b */
                                        /* rC3 = c3a  c3b */
/* */
        movapd          rC0, m0
   #ifdef BETA1
        addsd   CMUL(24)(pC), rC3
   #endif
        unpcklpd        rC1, rC0        /* rC0 = c0a  c1a */
                                        prefB((pfB))
        unpckhpd        rC1, m0         /*  m0 = c0b  c1b */
                                        addl    $32, pfB
        addpd           m0, rC0         /* rC0 = c0ab c1ab */
                                subl    $120, pB0
        movapd          rC2, m0
                                        pref2((pfA))
        unpcklpd        rC3, rC2        /* rC2 = c2a  c3a */
        unpckhpd        rC3, m0         /*  m0 = c2b  c3b */
                                        addl    $32, pfA
	                                addl	$NB4so-120, pA0
        addpd           m0, rC2         /* rC2 = c2ab c3ab */
/*
 *      Write results back to C
 */
   #ifdef BETAX
        addpd   m2, rC0
        addpd   m3, rC2
   #endif
   #ifdef DCPLX
	movlpd	rC0, (pC)
	movhpd	rC0, 16(pC)
	movlpd	rC2, 32(pC)
	movhpd	rC2, 48(pC)
   #else
	movupd	rC0, (pC)
	movupd	rC2, 16(pC)
   #endif
/*
 *      pC += 6;  pA += 2*NB
 */
	addl	$CMUL(32), pC
/*
 *      while (pA != stM);
 */
	subb	$4, stM
	jnz	MLOOP
#endif
/*
 *      pC += incCn;  pA -= NBNB;  pB += NB;
 */
	addl	incCn, pC
/*   	addl	COFF(%esp), pC */
   #if MB == 0
        subl    MKOFF(%esp), pA0
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
   #ifndef BETAX
	movl	12(%esp), %ebp
	movl	 8(%esp), %ebx
	movl	 4(%esp), %esi
	movl	  (%esp), %edi
	addl	$FSIZE, %esp
   #else
	movl	32(%esp), %ebp
	movl	28(%esp), %ebx
	movl	24(%esp), %esi
	movl	20(%esp), %edi
	movl	16(%esp), %esp
   #endif
	ret
