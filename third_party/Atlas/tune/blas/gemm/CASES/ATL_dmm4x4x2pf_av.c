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

#define Mjoin(pre, nam) my_join(pre, nam)
#define my_join(pre, nam) pre ## nam

#if !defined(ATL_AS_OSX_PPC) && !defined(ATL_GAS_LINUX_PPC)
   #error "This kernel requires PPC assembler"
#endif
#ifdef MB
   #if (MB/4)*4 != MB
      #error "MB must be multiple of 4!"
   #endif
#endif
#ifdef NB
   #if (NB/4)*4 != NB
      #error "NB must be multiple of 4!"
   #endif
#endif
#ifdef KB
   #if (KB/2)*2 != KB
      #error "KB must be multiple of 2!"
   #endif
#endif

#ifdef ATL_GAS_LINUX_PPC
   #define r0 0
   #define r1 1
   #define r2 2
   #define r3 3
   #define r4 4
   #define r5 5
   #define r6 6
   #define r7 7
   #define r8 8
   #define r9 9
   #define r10 10
   #define r11 11
   #define r12 12
   #define r13 13
   #define r14 14
   #define r15 15
   #define r16 16
   #define r17 17
   #define r18 18
   #define r19 19
   #define r20 20
   #define r21 21
   #define r22 22
   #define r23 23
   #define r24 24
   #define r25 25
   #define r26 26
   #define r27 27
   #define r28 28
   #define r29 29
   #define r30 30
   #define r31 31
   #define f0 0
   #define f1 1
   #define f2 2
   #define f3 3
   #define f4 4
   #define f5 5
   #define f6 6
   #define f7 7
   #define f8 8
   #define f9 9
   #define f10 10
   #define f11 11
   #define f12 12
   #define f13 13
   #define f14 14
   #define f15 15
   #define f16 16
   #define f17 17
   #define f18 18
   #define f19 19
   #define f20 20
   #define f21 21
   #define f22 22
   #define f23 23
   #define f24 24
   #define f25 25
   #define f26 26
   #define f27 27
   #define f28 28
   #define f29 29
   #define f30 30
   #define f31 31
#endif

/*
 * Integer register usage shown by these defines
 */
#ifdef ATL_GAS_LINUX_PPC
   #ifdef ATL_USE64BITS
      #define pC0     r6
      #define pC1     r15
      #define pC2     r22
      #define pC3     r11
      #define pA0     r7
      #define pA1     r16
      #define pA2     r17
      #define pA3     r18
      #define pB0     r9
      #define pB1     r19
      #define pB2     r20
      #define pB3     r21
      #define incAm   r8
      #define incAn   r23
      #define incBm   r24
      #define incBn   r10
      #define incCn   r5
      #define stK     r0
      #define stM     r14
      #define stN     r4
      #define M       r3
      #define ctlB    r12
      #define ctlC    r25
   #else
      #define pC0     r10
      #define pC1     r15
      #define pC2     r22
      #define pC3     r11
      #define pA0     r6
      #define pA1     r16
      #define pA2     r17
      #define pA3     r18
      #define pB0     r8
      #define pB1     r19
      #define pB2     r20
      #define pB3     r21
      #define incAm   r7
      #define incAn   r23
      #define incBm   r24
      #define incBn   r9
      #define incCn   r5
      #define stK     r0
      #define stM     r14
      #define stN     r4
      #define M       r3
      #define ctlB    r12
      #define ctlC    r25
   #endif
#else
   #define pC0     r6
   #define pC1     r25
   #define pC2     r15
   #define pC3     r14
   #define pA0     r8
   #define pA1     r16
   #define pA2     r17
   #define pA3     r18
   #define pB0     r10
   #define pB1     r19
   #define pB2     r20
   #define pB3     r21
   #define incAm   r9
   #define incAn   r22
   #define incBm   r23
   #define incBn   r24
   #define incCn   r5
   #define stK     r0
   #define stM     r7
   #define stN     r4
   #define M       r3
   #define ctlB    r11
   #define ctlC    r12
#endif

#ifdef DCPLX
   #define incCm 64
#else
   #define incCm 32
#endif
/*
 * fp register usage shown by these defines
 */
#define	beta	f2
#define rC00	f0
#define rC10	f1
#define rC20	f3
#define rC30	f4
#define rC01	f5
#define rC11	f6
#define rC21	f7
#define rC31	f8
#define rC02	f9
#define rC12	f10
#define rC22	f11
#define rC32	f12
#define rC03	f13
#define rC13	f14
#define rC23	f15
#define rC33	f16
#ifdef BETA0
#define ZERO	f17
#endif
#define ra0	f20
#define ra1	f21
#define ra2	f22
#define ra3	f23
#define rA0	f24
#define rA1	f25
#define rA2	f26
#define rA3	f27
#define rB0	f28
#define rB1	f29
#define rB2	f30
#define rB3	f31

/*
 * Offsets from stack pointer for integer register save area, fp reg area,
 */
#ifndef ATL_GAS_LINUX_PPC
   #define IROFF -220
   #define FROFF -144
#elif defined(ATL_USE64BITS)
   #define FROFF -288
   #define IROFF FROFF+144
#endif

#ifdef ATL_USE64BITS
   #define slwi         sldi
   #define srwi         srdi
   #define mullw        mulld
   #define cmpwi        cmpdi
#endif

#if 0
                         r3           r4           r5          r6-r7,f2
void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha,
                (r6)       r8  (r7)       r9  (r8)      r10  (r9)   56(r1)
                const TYPE *A, const int lda, const TYPE *B, const int ldb,
                             f2   68(r1)          72(r1)
                const TYPE beta, TYPE *C, const int ldc)
		                  (r10)    8(r1)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
NOTE: 64 bit Linux ABI wastes para-passing iregs and stack space like OS X:
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                         r3           r4           r5             r6/f1
void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha,
                           r7             r8             r9            r10
                const TYPE *A, const int lda, const TYPE *B, const int ldb,
                             f2   120(r1)        128(r1)
                const TYPE beta, TYPE *C, const int ldc)

#endif
.text
#ifdef ATL_GAS_LINUX_PPC
   #if defined(ATL_USE64BITS) && _CALL_ELF != 2
/*
 *       No idea what this does, but seg fault without it (I think it is
 *       partially resp for making code callable from both static & PIC code)
 */
        .align 2
        .globl  ATL_USERMM
        .section        ".opd","aw"
        .align  3
ATL_USERMM:
        .quad   Mjoin(.,ATL_USERMM),.TOC.@tocbase,0
        .previous
        .size   Mjoin(.,ATL_USERMM),24
        .type   Mjoin(.,ATL_USERMM),@function
        .globl  Mjoin(.,ATL_USERMM)
Mjoin(.,ATL_USERMM):
   #else
.globl	ATL_USERMM
ATL_USERMM:
      #define IROFF 8
      #define FROFF IROFF+48
      #define FSIZE (((IROFF+FROFF+144+15)/16)*16)
	mflr	r0
	stw	r0, 4(r1)
	stwu	r1, -FSIZE(r1)
   #endif
#else
.globl	Mjoin(_,ATL_USERMM)
Mjoin(_,ATL_USERMM):
/*
 *	Save iregs
 */
	mflr	r0
	stw	r0, 8(r1)
	mfcr	r0
	stw	r0, 4(r1)
#endif
#ifdef ATL_USE64BITS
	std	r14, IROFF(r1)
	std	r15, 8+IROFF(r1)
	std	r16, 16+IROFF(r1)
	std	r17, 32+IROFF(r1)
	std	r18, 40+IROFF(r1)
	std	r19, 48+IROFF(r1)
	std	r20, 56+IROFF(r1)
	std	r21, 64+IROFF(r1)
	std	r22, 72+IROFF(r1)
	std	r23, 80+IROFF(r1)
	std	r24, 88+IROFF(r1)
	std	r25, 96+IROFF(r1)
#else
	stw	r14, IROFF(r1)
	stw	r15, 4+IROFF(r1)
	stw	r16, 8+IROFF(r1)
	stw	r17, 12+IROFF(r1)
	stw	r18, 16+IROFF(r1)
	stw	r19, 20+IROFF(r1)
	stw	r20, 24+IROFF(r1)
	stw	r21, 28+IROFF(r1)
	stw	r22, 32+IROFF(r1)
	stw	r23, 36+IROFF(r1)
	stw	r24, 40+IROFF(r1)
	stw	r25, 44+IROFF(r1)
#endif
	mr	stK, r5
/*
 *      Setup ctrl reg for prefetch of A & B
 */
 	slwi	incAm, incAm, 3
	srwi	ctlB, stK, 1
	slwi	ctlB, ctlB, 8
	add	ctlB, ctlB, M
	slwi	ctlB, ctlB, 16
	or 	ctlB, ctlB, incAm
	dst	pA0, ctlB, 1

#ifndef ATL_GAS_LINUX_PPC
        lwz     incBn, 56(r1)
#endif
	slwi	incBn, incBn, 3
	srwi	ctlB, stK, 1
	slwi	ctlB, ctlB, 8
	addi	ctlB, ctlB, 4
	slwi	ctlB, ctlB, 16
	or	ctlB, ctlB, incBn
	dst	pB0, ctlB, 2
/*
 *	Save fregs
 */
	stfd	f14, FROFF(r1)
	stfd	f15, FROFF+8(r1)
	stfd	f16, FROFF+16(r1)
	stfd	f17, FROFF+24(r1)
	stfd	f18, FROFF+32(r1)
	stfd	f19, FROFF+40(r1)
	stfd	f20, FROFF+48(r1)
	stfd	f21, FROFF+56(r1)
	stfd	f22, FROFF+64(r1)
	stfd	f23, FROFF+72(r1)
	stfd	f24, FROFF+80(r1)
	stfd	f25, FROFF+88(r1)
	stfd	f26, FROFF+96(r1)
	stfd	f27, FROFF+104(r1)
	stfd	f28, FROFF+112(r1)
	stfd	f29, FROFF+120(r1)
	stfd	f30, FROFF+128(r1)
/*
 *      Store zero in freg for future use, and save last freg
 */
#ifdef BETA0
	xor	pC1, pC1, pC1
   #ifdef ATL_USE64BITS
	std	pC1, FROFF+136(r1)
   #else
	stw	pC1, FROFF+136(r1)
	stw	pC1, FROFF+140(r1)
   #endif
	lfd	ZERO, FROFF+136(r1)
#endif
	stfd	f31, FROFF+136(r1)
/*
 *      Setup C pointers and so on, setup C prefetch
 *      incCn = (ldc*4 - MB)*sizeof
 */
#ifdef ATL_GAS_LINUX_PPC
   #ifdef ATL_USE64BITS
	ld 	pC0, 120(r1)
	ld 	incCn, 128(r1)
   #else
	lwz	incCn, FSIZE+8(r1)
   #endif
#else
	lwz	pC0, 68(r1)
	lwz	incCn, 72(r1)
#endif
	mr	pC1, stK
	subi	incBm, pC1, 2
	slwi	incBm, incBm, 3
	srwi	pC1, pC1, 1
	addi	pC1, pC1, -1
	mr	stK, pC1
	srwi	pC1, incCn, 1
#ifdef DCPLX
	slwi	pC1, pC1, 4
#else
	slwi	pC1, pC1, 3
#endif
	andi.	pC1, pC1, 0xFFFF
	srwi	ctlC, stK, 1
	addi	ctlC, ctlC, 2
	slwi	ctlC, ctlC, 8
	addi	ctlC, ctlC, 4
	slwi	ctlC, ctlC, 16
	or 	ctlC, ctlC, pC1
#ifdef DCPLX
	slwi	incCn, incCn, 4
#else
	slwi	incCn, incCn, 3
#endif
	subi	pC0, pC0, incCm
	add	pC1, pC0, incCn
	add	pC2, pC1, incCn
	add	pC3, pC2, incCn
#ifdef DCPLX
	slwi	pA1, M, 4
#else
	slwi	pA1, M, 3
#endif
	slwi	incCn, incCn, 2
	sub 	incCn, incCn, pA1
	add	pA1, pA0, incAm
	add	pA2, pA1, incAm
	add	pA3, pA2, incAm
	add	pB1, pB0, incBn
	add	pB2, pB1, incBn
	add	pB3, pB2, incBn
	slwi	incBn, incBn, 2
	mullw	incAn, M, incAm
	slwi	incAm, incAm, 2
	sub	incAm, incAm, incBm
	srwi	M, M, 2
	srwi	stN, stN, 2
#ifdef ATL_GAS_LINUX_PPC
	.align 8
#endif
NLOOP:
	add 	pB0, pB0, incBn
	dst	pB0, ctlB, 2
	sub	pB0, pB0, incBn
	mr	stM, M
	addi	pC0, pC0, incCm
	dstst	pC0, ctlC, 3
	subi	pC0, pC0, incCm
MLOOP:
#ifdef BETA0
	fmr	rC00, ZERO
	mtctr	stK
	fmr	rC10, ZERO
        lfd     rA0, 0(pA0)
	fmr	rC20, ZERO
        lfd     rB0, 0(pB0)
	fmr	rC30, ZERO
        lfd     rA1, 0(pA1)
	fmr	rC01, ZERO
        lfd     rA2, 0(pA2)
	fmr	rC11, ZERO
	addi	pC0, pC0, incCm
	fmr	rC21, ZERO
	fmr	rC31, ZERO
	addi	pC1, pC1, incCm
	fmr	rC02, ZERO
	fmr	rC12, ZERO
        lfd     rA3, 0(pA3)
	fmr	rC22, ZERO
        lfd     rB1, 0(pB1)
	fmr	rC32, ZERO
	addi	pC2, pC2, incCm
	fmr	rC03, ZERO
        lfd     rB2, 0(pB2)
	fmr	rC13, ZERO
        lfd     rB3, 0(pB3)
	fmr	rC23, ZERO
	addi	pC3, pC3, incCm
	fmr	rC33, ZERO
#else
	lfdu	rC00, incCm(pC0)
	lfdu	rC01, incCm(pC1)
	lfdu	rC02, incCm(pC2)
	lfdu	rC03, incCm(pC3)
   #ifdef DCPLX
	lfd	rC10, 16(pC0)
	lfd	rC11, 16(pC1)
	lfd	rC12, 16(pC2)
	lfd	rC13, 16(pC3)
	lfd	rC20, 32(pC0)
	lfd	rC21, 32(pC1)
	lfd	rC22, 32(pC2)
	lfd	rC23, 32(pC3)
	lfd	rC30, 48(pC0)
	lfd	rC31, 48(pC1)
	lfd	rC32, 48(pC2)
	lfd	rC33, 48(pC3)
   #else
	lfd	rC10, 8(pC0)
	lfd	rC11, 8(pC1)
	lfd	rC12, 8(pC2)
	lfd	rC13, 8(pC3)
	lfd	rC20, 16(pC0)
	lfd	rC21, 16(pC1)
	lfd	rC22, 16(pC2)
	lfd	rC23, 16(pC3)
	lfd	rC30, 24(pC0)
	lfd	rC31, 24(pC1)
	lfd	rC32, 24(pC2)
	lfd	rC33, 24(pC3)
   #endif
   #ifdef BETAX
	fmul	rC00, rC00, beta
        lfd     rA0, 0(pA0)
	fmul	rC10, rC10, beta
        lfd     rB0, 0(pB0)
	fmul	rC20, rC20, beta
        lfd     rA1, 0(pA1)
	fmul	rC30, rC30, beta
        lfd     rA2, 0(pA2)
	fmul	rC01, rC01, beta
        lfd     rA3, 0(pA3)
	fmul	rC11, rC11, beta
        lfd     rB1, 0(pB1)
	fmul	rC21, rC21, beta
        lfd     rB2, 0(pB2)
	fmul	rC31, rC31, beta
	fmul	rC02, rC02, beta
        lfd     rB3, 0(pB3)
	fmul	rC12, rC12, beta
	mtctr	stK
	fmul	rC22, rC22, beta
	fmul	rC32, rC32, beta
	fmul	rC03, rC03, beta
	fmul	rC13, rC13, beta
	fmul	rC23, rC23, beta
	fmul	rC33, rC33, beta
   #endif
#endif
#ifdef BETA1
	mtctr	stK
        lfd     rA0, 0(pA0)
        lfd     rB0, 0(pB0)
        lfd     rA1, 0(pA1)
        lfd     rA2, 0(pA2)
        lfd     rA3, 0(pA3)
        lfd     rB1, 0(pB1)
        lfd     rB2, 0(pB2)
        lfd     rB3, 0(pB3)
#endif
#if !defined(KB) || KB == 0 || KB == 2
	cmpwi	cr0, stK, 0
	beq	cr0,DRAIN
#endif
KLOOP:
        fmadd   rC00, rA0, rB0, rC00
       		lfd     ra0, 8(pA0)
        fmadd   rC10, rA1, rB0, rC10
       		lfd     ra1, 8(pA1)
        fmadd   rC20, rA2, rB0, rC20
       		lfd     ra2, 8(pA2)
        fmadd   rC30, rA3, rB0, rC30
        fmadd   rC01, rA0, rB1, rC01
       		lfd     ra3, 8(pA3)
        fmadd   rC11, rA1, rB1, rC11
		lfd	rB0, 8(pB0)
        fmadd   rC21, rA2, rB1, rC21
        fmadd   rC31, rA3, rB1, rC31
        fmadd   rC02, rA0, rB2, rC02
		lfd	rB1, 8(pB1)
        fmadd   rC12, rA1, rB2, rC12
        fmadd   rC22, rA2, rB2, rC22
        fmadd   rC32, rA3, rB2, rC32
		lfd	rB2, 8(pB2)
        fmadd   rC03, rA0, rB3, rC03
        fmadd   rC13, rA1, rB3, rC13
        fmadd   rC23, rA2, rB3, rC23
        	lfdu     rA0, 16(pA0)
        fmadd   rC33, rA3, rB3, rC33

        fmadd   rC00, ra0, rB0, rC00
		lfd	rB3, 8(pB3)
        fmadd   rC10, ra1, rB0, rC10
        	lfdu    rA1, 16(pA1)
        fmadd   rC20, ra2, rB0, rC20
        fmadd   rC30, ra3, rB0, rC30
        	lfdu    rA2, 16(pA2)
        fmadd   rC01, ra0, rB1, rC01
        	lfdu    rB0, 16(pB0)
        fmadd   rC11, ra1, rB1, rC11
        	lfdu    rA3, 16(pA3)
        fmadd   rC21, ra2, rB1, rC21
        fmadd   rC31, ra3, rB1, rC31
        	lfdu    rB1, 16(pB1)
        fmadd   rC02, ra0, rB2, rC02
        fmadd   rC12, ra1, rB2, rC12
        fmadd   rC22, ra2, rB2, rC22
        fmadd   rC32, ra3, rB2, rC32
        fmadd   rC03, ra0, rB3, rC03
        	lfdu    rB2, 16(pB2)
        fmadd   rC13, ra1, rB3, rC13
        fmadd   rC23, ra2, rB3, rC23
        fmadd   rC33, ra3, rB3, rC33
        	lfdu    rB3, 16(pB3)
        bdnz    KLOOP
/*
 *      Drain pipe
 */
#if !defined(KB) || KB == 0 || KB == 2
DRAIN:
#endif
        fmadd   rC00, rA0, rB0, rC00
        fmadd   rC10, rA1, rB0, rC10
        	lfd     ra0, 8(pA0)
        fmadd   rC20, rA2, rB0, rC20
        fmadd   rC30, rA3, rB0, rC30

        fmadd   rC01, rA0, rB1, rC01
        	lfd     rB0, 8(pB0)
        fmadd   rC11, rA1, rB1, rC11
        fmadd   rC21, rA2, rB1, rC21
        	lfd     ra1, 8(pA1)
        fmadd   rC31, rA3, rB1, rC31

        fmadd   rC02, rA0, rB2, rC02
        	lfd     rB1, 8(pB1)
        fmadd   rC12, rA1, rB2, rC12
        fmadd   rC22, rA2, rB2, rC22
        	lfd     ra2, 8(pA2)
        fmadd   rC32, rA3, rB2, rC32

        fmadd   rC03, rA0, rB3, rC03
        	lfd     rB2, 8(pB2)
        fmadd   rC13, rA1, rB3, rC13
        fmadd   rC23, rA2, rB3, rC23
        	lfd     ra3, 8(pA3)
        fmadd   rC33, rA3, rB3, rC33

        fmadd   rC00, ra0, rB0, rC00
        	lfd     rB3, 8(pB3)
        fmadd   rC10, ra1, rB0, rC10
        fmadd   rC20, ra2, rB0, rC20
        fmadd   rC30, ra3, rB0, rC30

        fmadd   rC01, ra0, rB1, rC01
        fmadd   rC11, ra1, rB1, rC11
        fmadd   rC21, ra2, rB1, rC21
        fmadd   rC31, ra3, rB1, rC31

        fmadd   rC02, ra0, rB2, rC02
					add	pA0, pA0, incAm
        fmadd   rC12, ra1, rB2, rC12
					sub	pB0, pB0, incBm
        fmadd   rC22, ra2, rB2, rC22
					sub	pB1, pB1, incBm
        fmadd   rC32, ra3, rB2, rC32
					sub	pB2, pB2, incBm

        fmadd   rC03, ra0, rB3, rC03
					sub	pB3, pB3, incBm
        fmadd   rC13, ra1, rB3, rC13
					add	pA1, pA1, incAm
        fmadd   rC23, ra2, rB3, rC23
					add	pA2, pA2, incAm
        fmadd   rC33, ra3, rB3, rC33
					add	pA3, pA3, incAm

#ifdef DCPLX
	stfd	rC00, 0(pC0)
	stfd	rC01, 0(pC1)
	stfd	rC02, 0(pC2)
	stfd	rC03, 0(pC3)
	stfd	rC10, 16(pC0)
	stfd	rC11, 16(pC1)
	stfd	rC12, 16(pC2)
	stfd	rC13, 16(pC3)
	stfd	rC20, 32(pC0)
	stfd	rC21, 32(pC1)
	stfd	rC22, 32(pC2)
	stfd	rC23, 32(pC3)
	stfd	rC30, 48(pC0)
	stfd	rC31, 48(pC1)
	stfd	rC32, 48(pC2)
	stfd	rC33, 48(pC3)
#else
	stfd	rC00, 0(pC0)
	stfd	rC10, 8(pC0)
	stfd	rC20, 16(pC0)
	stfd	rC30, 24(pC0)
	stfd	rC01, 0(pC1)
	stfd	rC11, 8(pC1)
	stfd	rC21, 16(pC1)
	stfd	rC31, 24(pC1)
	stfd	rC02, 0(pC2)
	stfd	rC12, 8(pC2)
	stfd	rC22, 16(pC2)
	stfd	rC32, 24(pC2)
	stfd	rC03, 0(pC3)
	stfd	rC13, 8(pC3)
	stfd	rC23, 16(pC3)
	stfd	rC33, 24(pC3)
#endif
	subic.	stM, stM, 1
	bf	2, MLOOP

	sub	pA0, pA0, incAn
	sub	pA1, pA1, incAn
	sub	pA2, pA2, incAn
	sub	pA3, pA3, incAn
	add	pB0, pB0, incBn
	add	pB1, pB1, incBn
	add	pB2, pB2, incBn
	add	pB3, pB3, incBn
	add	pC0, pC0, incCn
	add	pC1, pC1, incCn
	add	pC2, pC2, incCn
	add	pC3, pC3, incCn
	subic.	stN, stN, 1
	bf	2, NLOOP

/*
 *      Prefetch selected portion of next iteration's data
 */
	add	pA0, pA0, incAn
	dst	pA0, ctlB, 1
	dstst	pC0, ctlC, 3
/*
 *	Restore regs from red zone
 */
DONE:
	lfd	f14, FROFF(r1)
	lfd	f15, FROFF+8(r1)
	lfd	f16, FROFF+16(r1)
	lfd	f17, FROFF+24(r1)
	lfd	f18, FROFF+32(r1)
	lfd	f19, FROFF+40(r1)
	lfd	f20, FROFF+48(r1)
	lfd	f21, FROFF+56(r1)
	lfd	f22, FROFF+64(r1)
	lfd	f23, FROFF+72(r1)
	lfd	f24, FROFF+80(r1)
	lfd	f25, FROFF+88(r1)
	lfd	f26, FROFF+96(r1)
	lfd	f27, FROFF+104(r1)
	lfd	f28, FROFF+112(r1)
	lfd	f29, FROFF+120(r1)
	lfd	f30, FROFF+128(r1)
	lfd	f31, FROFF+136(r1)
#ifdef ATL_USE64BITS
	ld	r14, IROFF(r1)
	ld	r15, 8+IROFF(r1)
	ld	r16, 16+IROFF(r1)
	ld	r17, 32+IROFF(r1)
	ld	r18, 40+IROFF(r1)
	ld	r19, 48+IROFF(r1)
	ld	r20, 56+IROFF(r1)
	ld	r21, 64+IROFF(r1)
	ld	r22, 72+IROFF(r1)
	ld	r23, 80+IROFF(r1)
	ld	r24, 88+IROFF(r1)
	ld	r25, 96+IROFF(r1)
#else
	lwz	r14, IROFF(r1)
	lwz	r15, 4+IROFF(r1)
	lwz	r16, 8+IROFF(r1)
	lwz	r17, 12+IROFF(r1)
	lwz	r18, 16+IROFF(r1)
	lwz	r19, 20+IROFF(r1)
	lwz	r20, 24+IROFF(r1)
	lwz	r21, 28+IROFF(r1)
	lwz	r22, 32+IROFF(r1)
	lwz	r23, 36+IROFF(r1)
	lwz	r24, 40+IROFF(r1)
	lwz	r25, 44+IROFF(r1)
   #ifdef ATL_GAS_LINUX_PPC
	lwz	r0, FSIZE+4(r1)
	mtlr	r0
	addi	r1, r1, FSIZE
   #else
	lwz	r0, 8(r1)
	mtlr	r0
	lwz	r0, 4(r1)
	mtcr	r0
   #endif
#endif
	blr
