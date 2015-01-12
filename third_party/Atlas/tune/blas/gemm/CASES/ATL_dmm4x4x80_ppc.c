/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2007 R. Clint Whaley
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
 * NOTE: this kernel written by R. Clint Whaley, but it uses two key ideas
 * discovered by Tony Castaldo for the PowerPC970:
 * (1) Instructions must be issued in groups of 4 like inst (eg. 4 iop/ld
 *     4 fp, etc.
 * (2) It is effective to somewhat intermix M-loop iterations
 */

#if !defined(ATL_AS_OSX_PPC) && !defined(ATL_GAS_LINUX_PPC) && \
    !defined(ATL_AS_AIX_PPC)
   #error "This kernel requires OS X, AIX, or Linux PPC assembler!"
#endif
#if !defined(KB) || KB == 0
   #error "This kernel requires KB be a compile-time constant!"
#endif
#ifndef MB
   #define MB 0
#endif

#ifdef DCPLX
   #define CMUL(i_) ((i_)*2)
   #define SHF  4
#else
   #define CMUL(i_) i_
   #define SHF  3
#endif

#ifdef ATL_USE64BITS
   #define slwi         sldi
   #define srwi         srdi
   #define cmpwi        cmpdi
#else
   #define std  stw
   #define ld   lwz
#endif

#if defined(ATL_USE64BITS)
   #define M       r3
   #define N       r4
   #define pA0     r7
   #define pB0     r9
   #define pC0     r10
   #define pC1     r5
   #define pC2     r6
   #define pC3     r8
   #define pfA     r11
   #define incAn   r0
   #define incCn   r12
   #define pfB     r14
   #define NEG(i_) -i_  /* 64 bit ABI defines red zone! */
   #define FSIZE     0
   #define BOFF    -160
#elif defined(ATL_AS_OSX_PPC) || defined(ATL_AS_AIX_PPC)
   #define M       r3
   #define N       r4
   #define pA0     r8
   #define pB0     r10
   #define pC0     r6
   #define pC1     r7
   #define pC2     r9
   #define pC3     r11
   #define pfA     r12
   #define incAn   r0
   #define incCn   r5
   #define pfB     r14
   #define FSIZE     0
   #define BOFF    -160
   #define NEG(i_) -i_
#else  /* 32-bit linux has no red zone */
   #define M       r3
   #define N       r4
   #define pA0     r6
   #define pB0     r8
   #define pC0     r10
   #define pC1     r5
   #define pC2     r7
   #define pC3     r9
   #define pfA     r11
   #define incAn   r0
   #define incCn   r12
   #define pfB     r14
   #define NEG(i_) i_
   #define FSIZE   172
   #define BOFF    FSIZE-8
#endif

#define rA0     f0
#define rA1     f1
#define rA2     f2
#define rA3     f3
#define rB0     f4
#define rB1     f5
#define rB2     f6
#define rB3     f7
#define ra0     f8
#define ra1     f9
#define ra2     f10
#define ra3     f11
#define rb0     f12
#define rb1     f13
#define rb2     f14
#define rb3     f15
#define rC00    f16
#define rC10    f17
#define rC20    f18
#define rC30    f19
#define rC01    f20
#define rC11    f21
#define rC21    f22
#define rC31    f23
#define rC02    f24
#define rC12    f25
#define rC22    f26
#define rC32    f27
#define rC03    f28
#define rC13    f29
#define rC23    f30
#define rC33    f31

#ifndef KB1
   #define KB1     KB
#endif
#ifndef KB2
   #define KB2     KB*2
   #define KB3     KB*3
   #define KB4     KB*4
   #define KB5     KB*5
   #define KB6     KB*6
   #define KB7     KB*7
#endif
#if 0
*******************************************************************************
32 bit ABIs:
                         r3           r4           r5          r6-r7,f1
void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha,
                (r6)       r8  (r7)       r9  (r8)      r10  (r9)   56(r1)
                const TYPE *A, const int lda, const TYPE *B, const int ldb,
                             f2   68(r1)          72(r1)
                const TYPE beta, TYPE *C, const int ldc)
                                  (r10)    8(r1)
*******************************************************************************
64 bit ABIs:
                         r3           r4           r5             r6/f1
void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha,
                           r7             r8             r9            r10
                const TYPE *A, const int lda, const TYPE *B, const int ldb,
                             f2   120(r1)        128(r1)
                const TYPE beta, TYPE *C, const int ldc)
#endif
#ifdef ATL_AS_AIX_PPC
        .csect .text[PR]
        .toc
        .csect .text[PR]
        .align 3
        .globl ATL_USERMM
        .globl Mjoin(.,ATL_USERMM)
   #ifdef ATL_USE64BITS
        .csect ATL_USERMM[DS],3
ATL_USERMM:
        .llong Mjoin(.,ATL_USERMM)
   #else
        .csect ATL_USERMM[DS]
ATL_USERMM:
        .long Mjoin(.,ATL_USERMM), TOC[tc0], 0
   #endif
        .csect .text[PR]
Mjoin(.,ATL_USERMM):
#else
.text
   #ifdef ATL_AS_OSX_PPC
	.globl  Mjoin(_,ATL_USERMM)
Mjoin(_,ATL_USERMM):
   #else
      #if defined(ATL_USE64BITS)
/*
 *      Official Program Descripter section, seg fault w/o it on Linux/PPC64
 */
        .section        ".opd","aw"
        .align  3
	.globl  ATL_USERMM
ATL_USERMM:
        .quad   Mjoin(.,ATL_USERMM),.TOC.@tocbase,0
        .previous
        .type   Mjoin(.,ATL_USERMM),@function
        .text
	.globl  Mjoin(.,ATL_USERMM)
Mjoin(.,ATL_USERMM):
      #else
	.globl  ATL_USERMM
ATL_USERMM:
      #endif
   #endif
#endif
/*      Save regs */
#if defined(ATL_GAS_LINUX_PPC) && !defined(ATL_USE64BITS)
        stwu    r1, -FSIZE(r1)
#endif
        stfd    f14, NEG(8)(r1)
        stfd    f15, NEG(16)(r1)
        stfd    f16, NEG(24)(r1)
        stfd    f17, NEG(32)(r1)
        stfd    f18, NEG(40)(r1)
        stfd    f19, NEG(48)(r1)
        stfd    f20, NEG(56)(r1)
        stfd    f21, NEG(64)(r1)
        stfd    f22, NEG(72)(r1)
        stfd    f23, NEG(80)(r1)
        stfd    f24, NEG(88)(r1)
        stfd    f25, NEG(96)(r1)
        stfd    f26, NEG(104)(r1)
        stfd    f27, NEG(112)(r1)
        stfd    f28, NEG(120)(r1)
        stfd    f29, NEG(128)(r1)
        stfd    f30, NEG(136)(r1)
        stfd    f31, NEG(144)(r1)
        std     r14, NEG(152)(r1)
#ifdef BETAX
        stfd    f2, BOFF(r1)
#elif defined(BETA0)
        xor     pfA, pfA, pfA
   #ifdef ATL_USE64BITS
        std     pfA, BOFF(r1)
   #else
        stw     pfA, BOFF(r1)
        stw     pfA, 4+BOFF(r1)
   #endif
#endif

#if defined (ATL_USE64BITS)
        ld      pC0, 120(r1)
        ld      incCn, 128(r1)
#elif defined(ATL_AS_OSX_PPC) || defined(ATL_AS_AIX_PPC)
        lwz     pC0, 68(r1)
        lwz     incCn,  72(r1)
#else
        lwz     incCn,  FSIZE+8(r1)
#endif
        slwi    incCn, incCn, SHF       /* incCn = ldc*sizeof */
        add     pC1, pC0, incCn
        add     pC2, pC1, incCn
        add     pC3, pC2, incCn
        slwi    pfA, M, SHF             /* pfA = M*sizeof() */
        slwi    incCn, incCn, 2
        sub     incCn, incCn, pfA       /* incCn = ldc*4 - M */
        mulli   incAn, M, KB*8          /* incAn = M*KB*sizeof() */
        add     pfA, pA0, incAn         /* pfA = A + M*KB */
        srwi    M, M, 2                 /* M /= 4 */
	addi	M, M, -1
#if MB == 0
        cmpwi   cr5, M, 0
#endif

//	.align 5
NLOOP:
        addi    pfB, pB0, KB4*8
        mtctr   M
	lfd	rB0, 0(pB0)
	lfd	rA0, 0(pA0)
	lfd	rA1, KB*8(pA0)
	lfd	rA2, KB2*8(pA0)
	lfd	rA3, KB3*8(pA0)
	lfd	rB1, KB*8(pB0)
	lfd	rB2, KB2*8(pB0)
	lfd	rB3, KB3*8(pB0)
#if MB == 0
        beq-    cr5, MPEELED
#endif
MLOOP:
/* Begin KLOOP */

#if KB > 0
   #ifdef BETA0
      #if KB > 1
		lfd	rb0, 8(pB0)
      #endif
      #if KB > 1
		lfd	ra0, 8(pA0)
      #endif
      #if KB > 1
		lfd	ra1, KB*8+8(pA0)
      #endif
      #if KB > 1
		lfd	ra2, KB2*8+8(pA0)
      #endif
	fmul 	rC00, rA0, rB0
	fmul 	rC10, rA1, rB0
	fmul 	rC20, rA2, rB0
	fmul 	rC30, rA3, rB0
      #if KB > 1
		lfd	ra3, KB3*8+8(pA0)
      #endif
      #if KB > 1
		lfd	rb1, KB*8+8(pB0)
      #endif
      #if KB > 1
		lfd	rb2, KB2*8+8(pB0)
      #endif
      #if KB > 1
		lfd	rb3, KB3*8+8(pB0)
      #endif
	fmul 	rC01, rA0, rB1
	fmul 	rC11, rA1, rB1
	fmul 	rC21, rA2, rB1
	fmul 	rC31, rA3, rB1
		dcbt	0, pfA, 0
        	addi    pfA, pfA, 128
		dcbt	0, pfB, 0
        	addi    pfB, pfB, 128
	fmul 	rC02, rA0, rB2
	fmul 	rC12, rA1, rB2
	fmul 	rC22, rA2, rB2
	fmul 	rC32, rA3, rB2
	fmul 	rC03, rA0, rB3
	fmul 	rC13, rA1, rB3
	fmul 	rC23, rA2, rB3
	fmul 	rC33, rA3, rB3
   #elif defined(BETAX)
	lfd	rb3, BOFF(r1)
        lfd     rC00, 0(pC0)
        lfd     rC10, CMUL(8)(pC0)
        lfd     rC20, CMUL(16)(pC0)
        lfd     rC30, CMUL(24)(pC0)
	nop
	nop
	nop
        lfd     rC01, 0(pC1)
        lfd     rC11, CMUL(8)(pC1)
        lfd     rC21, CMUL(16)(pC1)
        lfd     rC31, CMUL(24)(pC1)
	fmul	rC00, rC00, rb3
	fmul	rC10, rC10, rb3
	fmul	rC20, rC20, rb3
	fmul	rC30, rC30, rb3
        lfd     rC02, 0(pC2)
        lfd     rC12, CMUL(8)(pC2)
        lfd     rC22, CMUL(16)(pC2)
        lfd     rC32, CMUL(24)(pC2)
	fmul	rC01, rC01, rb3
	fmul	rC11, rC11, rb3
	fmul	rC21, rC21, rb3
	fmul	rC31, rC31, rb3
        lfd     rC03, 0(pC3)
        lfd     rC13, CMUL(8)(pC3)
        lfd     rC23, CMUL(16)(pC3)
        lfd     rC33, CMUL(24)(pC3)
	fmul	rC02, rC02, rb3
	fmul	rC12, rC12, rb3
	fmul	rC22, rC22, rb3
	fmul	rC32, rC32, rb3
	fmul	rC03, rC03, rb3
	fmul	rC13, rC13, rb3
	fmul	rC23, rC23, rb3
	fmul	rC33, rC33, rb3
      #if KB > 1
		lfd	rb0, 8(pB0)
      #endif
      #if KB > 1
		lfd	ra0, 8(pA0)
      #endif
      #if KB > 1
		lfd	ra1, KB*8+8(pA0)
      #endif
      #if KB > 1
		lfd	ra2, KB2*8+8(pA0)
      #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
      #if KB > 1
		lfd	ra3, KB3*8+8(pA0)
      #endif
      #if KB > 1
		lfd	rb1, KB*8+8(pB0)
      #endif
      #if KB > 1
		lfd	rb2, KB2*8+8(pB0)
      #endif
      #if KB > 1
		lfd	rb3, KB3*8+8(pB0)
      #endif
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
		dcbt	0, pfA, 0
        	addi    pfA, pfA, 128
		dcbt	0, pfB, 0
        	addi    pfB, pfB, 128
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
   #else  /* BETA == 1 */
        lfd     rC00, 0(pC0)
        lfd     rC10, CMUL(8)(pC0)
        lfd     rC20, CMUL(16)(pC0)
        lfd     rC30, CMUL(24)(pC0)
        lfd     rC01, 0(pC1)
        lfd     rC11, CMUL(8)(pC1)
        lfd     rC21, CMUL(16)(pC1)
        lfd     rC31, CMUL(24)(pC1)
        lfd     rC02, 0(pC2)
        lfd     rC12, CMUL(8)(pC2)
        lfd     rC22, CMUL(16)(pC2)
        lfd     rC32, CMUL(24)(pC2)
        	lfd     rC03, 0(pC3)
        	lfd     rC13, CMUL(8)(pC3)
        	lfd     rC23, CMUL(16)(pC3)
        	lfd     rC33, CMUL(24)(pC3)
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
		dcbt	0, pfA, 0
		dcbt	0, pfB, 0
        	addi    pfA, pfA, 128
        	addi    pfB, pfB, 128
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
      #if KB > 1
		lfd	rb0, 8(pB0)
      #endif
      #if KB > 1
		lfd	ra0, 8(pA0)
      #endif
      #if KB > 1
		lfd	ra1, KB*8+8(pA0)
      #endif
      #if KB > 1
		lfd	ra2, KB2*8+8(pA0)
      #endif
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
      #if KB > 1
		lfd	ra3, KB3*8+8(pA0)
      #endif
      #if KB > 1
		lfd	rb1, KB*8+8(pB0)
      #endif
      #if KB > 1
		lfd	rb2, KB2*8+8(pB0)
      #endif
      #if KB > 1
		lfd	rb3, KB3*8+8(pB0)
      #endif
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
   #endif /* done BETA specialization */
#endif  /* end K=1 block */
#if KB > 1
   #if KB > 2
		lfd	rB0, 16(pB0)
   #endif
   #if KB > 2
		lfd	rA0, 16(pA0)
   #endif
   #if KB > 2
		lfd	rA1, KB*8+16(pA0)
   #endif
   #if KB > 2
		lfd	rA2, KB2*8+16(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 2
		lfd	rA3, KB3*8+16(pA0)
   #endif
   #if KB > 2
		lfd	rB1, KB*8+16(pB0)
   #endif
   #if KB > 2
		lfd	rB2, KB2*8+16(pB0)
   #endif
   #if KB > 2
		lfd	rB3, KB3*8+16(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=2 block */
#if KB > 2
   #if KB > 3
		lfd	rb0, 24(pB0)
   #endif
   #if KB > 3
		lfd	ra0, 24(pA0)
   #endif
   #if KB > 3
		lfd	ra1, KB*8+24(pA0)
   #endif
   #if KB > 3
		lfd	ra2, KB2*8+24(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 3
		lfd	ra3, KB3*8+24(pA0)
   #endif
   #if KB > 3
		lfd	rb1, KB*8+24(pB0)
   #endif
   #if KB > 3
		lfd	rb2, KB2*8+24(pB0)
   #endif
   #if KB > 3
		lfd	rb3, KB3*8+24(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
        	addi    pC0, pC0, CMUL(4)*8     /* pC0 += 4 */
        	addi    pC1, pC1, CMUL(4)*8
        	addi    pC2, pC2, CMUL(4)*8
        	addi    pC3, pC3, CMUL(4)*8
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#else
        	addi    pC0, pC0, CMUL(4)*8     /* pC0 += 4 */
        	addi    pC1, pC1, CMUL(4)*8
        	addi    pC2, pC2, CMUL(4)*8
        	addi    pC3, pC3, CMUL(4)*8
#endif  /* end K=3 block */
#if KB > 3
   #if KB > 4
		lfd	rB0, 32(pB0)
   #endif
   #if KB > 4
		lfd	rA0, 32(pA0)
   #endif
   #if KB > 4
		lfd	rA1, KB*8+32(pA0)
   #endif
   #if KB > 4
		lfd	rA2, KB2*8+32(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 4
		lfd	rA3, KB3*8+32(pA0)
   #endif
   #if KB > 4
		lfd	rB1, KB*8+32(pB0)
   #endif
   #if KB > 4
		lfd	rB2, KB2*8+32(pB0)
   #endif
   #if KB > 4
		lfd	rB3, KB3*8+32(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=4 block */
#if KB > 4
   #if KB > 5
		lfd	rb0, 40(pB0)
   #endif
   #if KB > 5
		lfd	ra0, 40(pA0)
   #endif
   #if KB > 5
		lfd	ra1, KB*8+40(pA0)
   #endif
   #if KB > 5
		lfd	ra2, KB2*8+40(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 5
		lfd	ra3, KB3*8+40(pA0)
   #endif
   #if KB > 5
		lfd	rb1, KB*8+40(pB0)
   #endif
   #if KB > 5
		lfd	rb2, KB2*8+40(pB0)
   #endif
   #if KB > 5
		lfd	rb3, KB3*8+40(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=5 block */
#if KB > 5
   #if KB > 6
		lfd	rB0, 48(pB0)
   #endif
   #if KB > 6
		lfd	rA0, 48(pA0)
   #endif
   #if KB > 6
		lfd	rA1, KB*8+48(pA0)
   #endif
   #if KB > 6
		lfd	rA2, KB2*8+48(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 6
		lfd	rA3, KB3*8+48(pA0)
   #endif
   #if KB > 6
		lfd	rB1, KB*8+48(pB0)
   #endif
   #if KB > 6
		lfd	rB2, KB2*8+48(pB0)
   #endif
   #if KB > 6
		lfd	rB3, KB3*8+48(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=6 block */
#if KB > 6
   #if KB > 7
		lfd	rb0, 56(pB0)
   #endif
   #if KB > 7
		lfd	ra0, 56(pA0)
   #endif
   #if KB > 7
		lfd	ra1, KB*8+56(pA0)
   #endif
   #if KB > 7
		lfd	ra2, KB2*8+56(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 7
		lfd	ra3, KB3*8+56(pA0)
   #endif
   #if KB > 7
		lfd	rb1, KB*8+56(pB0)
   #endif
   #if KB > 7
		lfd	rb2, KB2*8+56(pB0)
   #endif
   #if KB > 7
		lfd	rb3, KB3*8+56(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=7 block */
#if KB > 7
   #if KB > 8
		lfd	rB0, 64(pB0)
   #endif
   #if KB > 8
		lfd	rA0, 64(pA0)
   #endif
   #if KB > 8
		lfd	rA1, KB*8+64(pA0)
   #endif
   #if KB > 8
		lfd	rA2, KB2*8+64(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 8
		lfd	rA3, KB3*8+64(pA0)
   #endif
   #if KB > 8
		lfd	rB1, KB*8+64(pB0)
   #endif
   #if KB > 8
		lfd	rB2, KB2*8+64(pB0)
   #endif
   #if KB > 8
		lfd	rB3, KB3*8+64(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=8 block */
#if KB > 8
   #if KB > 9
		lfd	rb0, 72(pB0)
   #endif
   #if KB > 9
		lfd	ra0, 72(pA0)
   #endif
   #if KB > 9
		lfd	ra1, KB*8+72(pA0)
   #endif
   #if KB > 9
		lfd	ra2, KB2*8+72(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 9
		lfd	ra3, KB3*8+72(pA0)
   #endif
   #if KB > 9
		lfd	rb1, KB*8+72(pB0)
   #endif
   #if KB > 9
		lfd	rb2, KB2*8+72(pB0)
   #endif
   #if KB > 9
		lfd	rb3, KB3*8+72(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=9 block */
#if KB > 9
   #if KB > 10
		lfd	rB0, 80(pB0)
   #endif
   #if KB > 10
		lfd	rA0, 80(pA0)
   #endif
   #if KB > 10
		lfd	rA1, KB*8+80(pA0)
   #endif
   #if KB > 10
		lfd	rA2, KB2*8+80(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 10
		lfd	rA3, KB3*8+80(pA0)
   #endif
   #if KB > 10
		lfd	rB1, KB*8+80(pB0)
   #endif
   #if KB > 10
		lfd	rB2, KB2*8+80(pB0)
   #endif
   #if KB > 10
		lfd	rB3, KB3*8+80(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=10 block */
#if KB > 10
   #if KB > 11
		lfd	rb0, 88(pB0)
   #endif
   #if KB > 11
		lfd	ra0, 88(pA0)
   #endif
   #if KB > 11
		lfd	ra1, KB*8+88(pA0)
   #endif
   #if KB > 11
		lfd	ra2, KB2*8+88(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 11
		lfd	ra3, KB3*8+88(pA0)
   #endif
   #if KB > 11
		lfd	rb1, KB*8+88(pB0)
   #endif
   #if KB > 11
		lfd	rb2, KB2*8+88(pB0)
   #endif
   #if KB > 11
		lfd	rb3, KB3*8+88(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=11 block */
#if KB > 11
   #if KB > 12
		lfd	rB0, 96(pB0)
   #endif
   #if KB > 12
		lfd	rA0, 96(pA0)
   #endif
   #if KB > 12
		lfd	rA1, KB*8+96(pA0)
   #endif
   #if KB > 12
		lfd	rA2, KB2*8+96(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 12
		lfd	rA3, KB3*8+96(pA0)
   #endif
   #if KB > 12
		lfd	rB1, KB*8+96(pB0)
   #endif
   #if KB > 12
		lfd	rB2, KB2*8+96(pB0)
   #endif
   #if KB > 12
		lfd	rB3, KB3*8+96(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=12 block */
#if KB > 12
   #if KB > 13
		lfd	rb0, 104(pB0)
   #endif
   #if KB > 13
		lfd	ra0, 104(pA0)
   #endif
   #if KB > 13
		lfd	ra1, KB*8+104(pA0)
   #endif
   #if KB > 13
		lfd	ra2, KB2*8+104(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 13
		lfd	ra3, KB3*8+104(pA0)
   #endif
   #if KB > 13
		lfd	rb1, KB*8+104(pB0)
   #endif
   #if KB > 13
		lfd	rb2, KB2*8+104(pB0)
   #endif
   #if KB > 13
		lfd	rb3, KB3*8+104(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=13 block */
#if KB > 13
   #if KB > 14
		lfd	rB0, 112(pB0)
   #endif
   #if KB > 14
		lfd	rA0, 112(pA0)
   #endif
   #if KB > 14
		lfd	rA1, KB*8+112(pA0)
   #endif
   #if KB > 14
		lfd	rA2, KB2*8+112(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 14
		lfd	rA3, KB3*8+112(pA0)
   #endif
   #if KB > 14
		lfd	rB1, KB*8+112(pB0)
   #endif
   #if KB > 14
		lfd	rB2, KB2*8+112(pB0)
   #endif
   #if KB > 14
		lfd	rB3, KB3*8+112(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=14 block */
#if KB > 14
   #if KB > 15
		lfd	rb0, 120(pB0)
   #endif
   #if KB > 15
		lfd	ra0, 120(pA0)
   #endif
   #if KB > 15
		lfd	ra1, KB*8+120(pA0)
   #endif
   #if KB > 15
		lfd	ra2, KB2*8+120(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 15
		lfd	ra3, KB3*8+120(pA0)
   #endif
   #if KB > 15
		lfd	rb1, KB*8+120(pB0)
   #endif
   #if KB > 15
		lfd	rb2, KB2*8+120(pB0)
   #endif
   #if KB > 15
		lfd	rb3, KB3*8+120(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=15 block */
#if KB > 15
   #if KB > 16
		lfd	rB0, 128(pB0)
   #endif
   #if KB > 16
		lfd	rA0, 128(pA0)
   #endif
   #if KB > 16
		lfd	rA1, KB*8+128(pA0)
   #endif
   #if KB > 16
		lfd	rA2, KB2*8+128(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 16
		lfd	rA3, KB3*8+128(pA0)
   #endif
   #if KB > 16
		lfd	rB1, KB*8+128(pB0)
   #endif
   #if KB > 16
		lfd	rB2, KB2*8+128(pB0)
   #endif
   #if KB > 16
		lfd	rB3, KB3*8+128(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=16 block */
#if KB > 16
   #if KB > 17
		lfd	rb0, 136(pB0)
   #endif
   #if KB > 17
		lfd	ra0, 136(pA0)
   #endif
   #if KB > 17
		lfd	ra1, KB*8+136(pA0)
   #endif
   #if KB > 17
		lfd	ra2, KB2*8+136(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 17
		lfd	ra3, KB3*8+136(pA0)
   #endif
   #if KB > 17
		lfd	rb1, KB*8+136(pB0)
   #endif
   #if KB > 17
		lfd	rb2, KB2*8+136(pB0)
   #endif
   #if KB > 17
		lfd	rb3, KB3*8+136(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=17 block */
#if KB > 17
   #if KB > 18
		lfd	rB0, 144(pB0)
   #endif
   #if KB > 18
		lfd	rA0, 144(pA0)
   #endif
   #if KB > 18
		lfd	rA1, KB*8+144(pA0)
   #endif
   #if KB > 18
		lfd	rA2, KB2*8+144(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 18
		lfd	rA3, KB3*8+144(pA0)
   #endif
   #if KB > 18
		lfd	rB1, KB*8+144(pB0)
   #endif
   #if KB > 18
		lfd	rB2, KB2*8+144(pB0)
   #endif
   #if KB > 18
		lfd	rB3, KB3*8+144(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=18 block */
#if KB > 18
   #if KB > 19
		lfd	rb0, 152(pB0)
   #endif
   #if KB > 19
		lfd	ra0, 152(pA0)
   #endif
   #if KB > 19
		lfd	ra1, KB*8+152(pA0)
   #endif
   #if KB > 19
		lfd	ra2, KB2*8+152(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 19
		lfd	ra3, KB3*8+152(pA0)
   #endif
   #if KB > 19
		lfd	rb1, KB*8+152(pB0)
   #endif
   #if KB > 19
		lfd	rb2, KB2*8+152(pB0)
   #endif
   #if KB > 19
		lfd	rb3, KB3*8+152(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=19 block */
#if KB > 19
   #if KB > 20
		lfd	rB0, 160(pB0)
   #endif
   #if KB > 20
		lfd	rA0, 160(pA0)
   #endif
   #if KB > 20
		lfd	rA1, KB*8+160(pA0)
   #endif
   #if KB > 20
		lfd	rA2, KB2*8+160(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 20
		lfd	rA3, KB3*8+160(pA0)
   #endif
   #if KB > 20
		lfd	rB1, KB*8+160(pB0)
   #endif
   #if KB > 20
		lfd	rB2, KB2*8+160(pB0)
   #endif
   #if KB > 20
		lfd	rB3, KB3*8+160(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=20 block */
#if KB > 20
   #if KB > 21
		lfd	rb0, 168(pB0)
   #endif
   #if KB > 21
		lfd	ra0, 168(pA0)
   #endif
   #if KB > 21
		lfd	ra1, KB*8+168(pA0)
   #endif
   #if KB > 21
		lfd	ra2, KB2*8+168(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 21
		lfd	ra3, KB3*8+168(pA0)
   #endif
   #if KB > 21
		lfd	rb1, KB*8+168(pB0)
   #endif
   #if KB > 21
		lfd	rb2, KB2*8+168(pB0)
   #endif
   #if KB > 21
		lfd	rb3, KB3*8+168(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=21 block */
#if KB > 21
   #if KB > 22
		lfd	rB0, 176(pB0)
   #endif
   #if KB > 22
		lfd	rA0, 176(pA0)
   #endif
   #if KB > 22
		lfd	rA1, KB*8+176(pA0)
   #endif
   #if KB > 22
		lfd	rA2, KB2*8+176(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 22
		lfd	rA3, KB3*8+176(pA0)
   #endif
   #if KB > 22
		lfd	rB1, KB*8+176(pB0)
   #endif
   #if KB > 22
		lfd	rB2, KB2*8+176(pB0)
   #endif
   #if KB > 22
		lfd	rB3, KB3*8+176(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=22 block */
#if KB > 22
   #if KB > 23
		lfd	rb0, 184(pB0)
   #endif
   #if KB > 23
		lfd	ra0, 184(pA0)
   #endif
   #if KB > 23
		lfd	ra1, KB*8+184(pA0)
   #endif
   #if KB > 23
		lfd	ra2, KB2*8+184(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 23
		lfd	ra3, KB3*8+184(pA0)
   #endif
   #if KB > 23
		lfd	rb1, KB*8+184(pB0)
   #endif
   #if KB > 23
		lfd	rb2, KB2*8+184(pB0)
   #endif
   #if KB > 23
		lfd	rb3, KB3*8+184(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=23 block */
#if KB > 23
   #if KB > 24
		lfd	rB0, 192(pB0)
   #endif
   #if KB > 24
		lfd	rA0, 192(pA0)
   #endif
   #if KB > 24
		lfd	rA1, KB*8+192(pA0)
   #endif
   #if KB > 24
		lfd	rA2, KB2*8+192(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 24
		lfd	rA3, KB3*8+192(pA0)
   #endif
   #if KB > 24
		lfd	rB1, KB*8+192(pB0)
   #endif
   #if KB > 24
		lfd	rB2, KB2*8+192(pB0)
   #endif
   #if KB > 24
		lfd	rB3, KB3*8+192(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=24 block */
#if KB > 24
   #if KB > 25
		lfd	rb0, 200(pB0)
   #endif
   #if KB > 25
		lfd	ra0, 200(pA0)
   #endif
   #if KB > 25
		lfd	ra1, KB*8+200(pA0)
   #endif
   #if KB > 25
		lfd	ra2, KB2*8+200(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 25
		lfd	ra3, KB3*8+200(pA0)
   #endif
   #if KB > 25
		lfd	rb1, KB*8+200(pB0)
   #endif
   #if KB > 25
		lfd	rb2, KB2*8+200(pB0)
   #endif
   #if KB > 25
		lfd	rb3, KB3*8+200(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=25 block */
#if KB > 25
   #if KB > 26
		lfd	rB0, 208(pB0)
   #endif
   #if KB > 26
		lfd	rA0, 208(pA0)
   #endif
   #if KB > 26
		lfd	rA1, KB*8+208(pA0)
   #endif
   #if KB > 26
		lfd	rA2, KB2*8+208(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 26
		lfd	rA3, KB3*8+208(pA0)
   #endif
   #if KB > 26
		lfd	rB1, KB*8+208(pB0)
   #endif
   #if KB > 26
		lfd	rB2, KB2*8+208(pB0)
   #endif
   #if KB > 26
		lfd	rB3, KB3*8+208(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=26 block */
#if KB > 26
   #if KB > 27
		lfd	rb0, 216(pB0)
   #endif
   #if KB > 27
		lfd	ra0, 216(pA0)
   #endif
   #if KB > 27
		lfd	ra1, KB*8+216(pA0)
   #endif
   #if KB > 27
		lfd	ra2, KB2*8+216(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 27
		lfd	ra3, KB3*8+216(pA0)
   #endif
   #if KB > 27
		lfd	rb1, KB*8+216(pB0)
   #endif
   #if KB > 27
		lfd	rb2, KB2*8+216(pB0)
   #endif
   #if KB > 27
		lfd	rb3, KB3*8+216(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=27 block */
#if KB > 27
   #if KB > 28
		lfd	rB0, 224(pB0)
   #endif
   #if KB > 28
		lfd	rA0, 224(pA0)
   #endif
   #if KB > 28
		lfd	rA1, KB*8+224(pA0)
   #endif
   #if KB > 28
		lfd	rA2, KB2*8+224(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 28
		lfd	rA3, KB3*8+224(pA0)
   #endif
   #if KB > 28
		lfd	rB1, KB*8+224(pB0)
   #endif
   #if KB > 28
		lfd	rB2, KB2*8+224(pB0)
   #endif
   #if KB > 28
		lfd	rB3, KB3*8+224(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=28 block */
#if KB > 28
   #if KB > 29
		lfd	rb0, 232(pB0)
   #endif
   #if KB > 29
		lfd	ra0, 232(pA0)
   #endif
   #if KB > 29
		lfd	ra1, KB*8+232(pA0)
   #endif
   #if KB > 29
		lfd	ra2, KB2*8+232(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 29
		lfd	ra3, KB3*8+232(pA0)
   #endif
   #if KB > 29
		lfd	rb1, KB*8+232(pB0)
   #endif
   #if KB > 29
		lfd	rb2, KB2*8+232(pB0)
   #endif
   #if KB > 29
		lfd	rb3, KB3*8+232(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=29 block */
#if KB > 29
   #if KB > 30
		lfd	rB0, 240(pB0)
   #endif
   #if KB > 30
		lfd	rA0, 240(pA0)
   #endif
   #if KB > 30
		lfd	rA1, KB*8+240(pA0)
   #endif
   #if KB > 30
		lfd	rA2, KB2*8+240(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 30
		lfd	rA3, KB3*8+240(pA0)
   #endif
   #if KB > 30
		lfd	rB1, KB*8+240(pB0)
   #endif
   #if KB > 30
		lfd	rB2, KB2*8+240(pB0)
   #endif
   #if KB > 30
		lfd	rB3, KB3*8+240(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=30 block */
#if KB > 30
   #if KB > 31
		lfd	rb0, 248(pB0)
   #endif
   #if KB > 31
		lfd	ra0, 248(pA0)
   #endif
   #if KB > 31
		lfd	ra1, KB*8+248(pA0)
   #endif
   #if KB > 31
		lfd	ra2, KB2*8+248(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 31
		lfd	ra3, KB3*8+248(pA0)
   #endif
   #if KB > 31
		lfd	rb1, KB*8+248(pB0)
   #endif
   #if KB > 31
		lfd	rb2, KB2*8+248(pB0)
   #endif
   #if KB > 31
		lfd	rb3, KB3*8+248(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=31 block */
#if KB > 31
   #if KB > 32
		lfd	rB0, 256(pB0)
   #endif
   #if KB > 32
		lfd	rA0, 256(pA0)
   #endif
   #if KB > 32
		lfd	rA1, KB*8+256(pA0)
   #endif
   #if KB > 32
		lfd	rA2, KB2*8+256(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 32
		lfd	rA3, KB3*8+256(pA0)
   #endif
   #if KB > 32
		lfd	rB1, KB*8+256(pB0)
   #endif
   #if KB > 32
		lfd	rB2, KB2*8+256(pB0)
   #endif
   #if KB > 32
		lfd	rB3, KB3*8+256(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=32 block */
#if KB > 32
   #if KB > 33
		lfd	rb0, 264(pB0)
   #endif
   #if KB > 33
		lfd	ra0, 264(pA0)
   #endif
   #if KB > 33
		lfd	ra1, KB*8+264(pA0)
   #endif
   #if KB > 33
		lfd	ra2, KB2*8+264(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 33
		lfd	ra3, KB3*8+264(pA0)
   #endif
   #if KB > 33
		lfd	rb1, KB*8+264(pB0)
   #endif
   #if KB > 33
		lfd	rb2, KB2*8+264(pB0)
   #endif
   #if KB > 33
		lfd	rb3, KB3*8+264(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=33 block */
#if KB > 33
   #if KB > 34
		lfd	rB0, 272(pB0)
   #endif
   #if KB > 34
		lfd	rA0, 272(pA0)
   #endif
   #if KB > 34
		lfd	rA1, KB*8+272(pA0)
   #endif
   #if KB > 34
		lfd	rA2, KB2*8+272(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 34
		lfd	rA3, KB3*8+272(pA0)
   #endif
   #if KB > 34
		lfd	rB1, KB*8+272(pB0)
   #endif
   #if KB > 34
		lfd	rB2, KB2*8+272(pB0)
   #endif
   #if KB > 34
		lfd	rB3, KB3*8+272(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=34 block */
#if KB > 34
   #if KB > 35
		lfd	rb0, 280(pB0)
   #endif
   #if KB > 35
		lfd	ra0, 280(pA0)
   #endif
   #if KB > 35
		lfd	ra1, KB*8+280(pA0)
   #endif
   #if KB > 35
		lfd	ra2, KB2*8+280(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 35
		lfd	ra3, KB3*8+280(pA0)
   #endif
   #if KB > 35
		lfd	rb1, KB*8+280(pB0)
   #endif
   #if KB > 35
		lfd	rb2, KB2*8+280(pB0)
   #endif
   #if KB > 35
		lfd	rb3, KB3*8+280(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=35 block */
#if KB > 35
   #if KB > 36
		lfd	rB0, 288(pB0)
   #endif
   #if KB > 36
		lfd	rA0, 288(pA0)
   #endif
   #if KB > 36
		lfd	rA1, KB*8+288(pA0)
   #endif
   #if KB > 36
		lfd	rA2, KB2*8+288(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 36
		lfd	rA3, KB3*8+288(pA0)
   #endif
   #if KB > 36
		lfd	rB1, KB*8+288(pB0)
   #endif
   #if KB > 36
		lfd	rB2, KB2*8+288(pB0)
   #endif
   #if KB > 36
		lfd	rB3, KB3*8+288(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
#if KB == 36
	lfd	rB0, 0(pB0)
	lfd	rA0, KB4*8+0(pA0)
	lfd	rA1, KB4*8+KB*8(pA0)
	lfd	rA2, KB4*8+KB2*8(pA0)
#endif
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
#if KB == 36
	lfd	rA3, KB4*8+KB3*8(pA0)
	lfd	rB1, KB*8(pB0)
	lfd	rB2, KB2*8(pB0)
	lfd	rB3, KB3*8(pB0)
#endif
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=36 block */
#if KB > 36
   #if KB > 37
		lfd	rb0, 296(pB0)
   #endif
   #if KB > 37
		lfd	ra0, 296(pA0)
   #endif
   #if KB > 37
		lfd	ra1, KB*8+296(pA0)
   #endif
   #if KB > 37
		lfd	ra2, KB2*8+296(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 37
		lfd	ra3, KB3*8+296(pA0)
   #endif
   #if KB > 37
		lfd	rb1, KB*8+296(pB0)
   #endif
   #if KB > 37
		lfd	rb2, KB2*8+296(pB0)
   #endif
   #if KB > 37
		lfd	rb3, KB3*8+296(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=37 block */
#if KB > 37
   #if KB > 38
		lfd	rB0, 304(pB0)
   #endif
   #if KB > 38
		lfd	rA0, 304(pA0)
   #endif
   #if KB > 38
		lfd	rA1, KB*8+304(pA0)
   #endif
   #if KB > 38
		lfd	rA2, KB2*8+304(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 38
		lfd	rA3, KB3*8+304(pA0)
   #endif
   #if KB > 38
		lfd	rB1, KB*8+304(pB0)
   #endif
   #if KB > 38
		lfd	rB2, KB2*8+304(pB0)
   #endif
   #if KB > 38
		lfd	rB3, KB3*8+304(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=38 block */
#if KB > 38
   #if KB > 39
		lfd	rb0, 312(pB0)
   #endif
   #if KB > 39
		lfd	ra0, 312(pA0)
   #endif
   #if KB > 39
		lfd	ra1, KB*8+312(pA0)
   #endif
   #if KB > 39
		lfd	ra2, KB2*8+312(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 39
		lfd	ra3, KB3*8+312(pA0)
   #endif
   #if KB > 39
		lfd	rb1, KB*8+312(pB0)
   #endif
   #if KB > 39
		lfd	rb2, KB2*8+312(pB0)
   #endif
   #if KB > 39
		lfd	rb3, KB3*8+312(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=39 block */
#if KB > 39
   #if KB > 40
		lfd	rB0, 320(pB0)
   #endif
   #if KB > 40
		lfd	rA0, 320(pA0)
   #endif
   #if KB > 40
		lfd	rA1, KB*8+320(pA0)
   #endif
   #if KB > 40
		lfd	rA2, KB2*8+320(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 40
		lfd	rA3, KB3*8+320(pA0)
   #endif
   #if KB > 40
		lfd	rB1, KB*8+320(pB0)
   #endif
   #if KB > 40
		lfd	rB2, KB2*8+320(pB0)
   #endif
   #if KB > 40
		lfd	rB3, KB3*8+320(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=40 block */
#if KB > 40
   #if KB > 41
		lfd	rb0, 328(pB0)
   #endif
   #if KB > 41
		lfd	ra0, 328(pA0)
   #endif
   #if KB > 41
		lfd	ra1, KB*8+328(pA0)
   #endif
   #if KB > 41
		lfd	ra2, KB2*8+328(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 41
		lfd	ra3, KB3*8+328(pA0)
   #endif
   #if KB > 41
		lfd	rb1, KB*8+328(pB0)
   #endif
   #if KB > 41
		lfd	rb2, KB2*8+328(pB0)
   #endif
   #if KB > 41
		lfd	rb3, KB3*8+328(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=41 block */
#if KB > 41
   #if KB > 42
		lfd	rB0, 336(pB0)
   #endif
   #if KB > 42
		lfd	rA0, 336(pA0)
   #endif
   #if KB > 42
		lfd	rA1, KB*8+336(pA0)
   #endif
   #if KB > 42
		lfd	rA2, KB2*8+336(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 42
		lfd	rA3, KB3*8+336(pA0)
   #endif
   #if KB > 42
		lfd	rB1, KB*8+336(pB0)
   #endif
   #if KB > 42
		lfd	rB2, KB2*8+336(pB0)
   #endif
   #if KB > 42
		lfd	rB3, KB3*8+336(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=42 block */
#if KB > 42
   #if KB > 43
		lfd	rb0, 344(pB0)
   #endif
   #if KB > 43
		lfd	ra0, 344(pA0)
   #endif
   #if KB > 43
		lfd	ra1, KB*8+344(pA0)
   #endif
   #if KB > 43
		lfd	ra2, KB2*8+344(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 43
		lfd	ra3, KB3*8+344(pA0)
   #endif
   #if KB > 43
		lfd	rb1, KB*8+344(pB0)
   #endif
   #if KB > 43
		lfd	rb2, KB2*8+344(pB0)
   #endif
   #if KB > 43
		lfd	rb3, KB3*8+344(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=43 block */
#if KB > 43
   #if KB > 44
		lfd	rB0, 352(pB0)
   #endif
   #if KB > 44
		lfd	rA0, 352(pA0)
   #endif
   #if KB > 44
		lfd	rA1, KB*8+352(pA0)
   #endif
   #if KB > 44
		lfd	rA2, KB2*8+352(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 44
		lfd	rA3, KB3*8+352(pA0)
   #endif
   #if KB > 44
		lfd	rB1, KB*8+352(pB0)
   #endif
   #if KB > 44
		lfd	rB2, KB2*8+352(pB0)
   #endif
   #if KB > 44
		lfd	rB3, KB3*8+352(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=44 block */
#if KB > 44
   #if KB > 45
		lfd	rb0, 360(pB0)
   #endif
   #if KB > 45
		lfd	ra0, 360(pA0)
   #endif
   #if KB > 45
		lfd	ra1, KB*8+360(pA0)
   #endif
   #if KB > 45
		lfd	ra2, KB2*8+360(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 45
		lfd	ra3, KB3*8+360(pA0)
   #endif
   #if KB > 45
		lfd	rb1, KB*8+360(pB0)
   #endif
   #if KB > 45
		lfd	rb2, KB2*8+360(pB0)
   #endif
   #if KB > 45
		lfd	rb3, KB3*8+360(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=45 block */
#if KB > 45
   #if KB > 46
		lfd	rB0, 368(pB0)
   #endif
   #if KB > 46
		lfd	rA0, 368(pA0)
   #endif
   #if KB > 46
		lfd	rA1, KB*8+368(pA0)
   #endif
   #if KB > 46
		lfd	rA2, KB2*8+368(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 46
		lfd	rA3, KB3*8+368(pA0)
   #endif
   #if KB > 46
		lfd	rB1, KB*8+368(pB0)
   #endif
   #if KB > 46
		lfd	rB2, KB2*8+368(pB0)
   #endif
   #if KB > 46
		lfd	rB3, KB3*8+368(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=46 block */
#if KB > 46
   #if KB > 47
		lfd	rb0, 376(pB0)
   #endif
   #if KB > 47
		lfd	ra0, 376(pA0)
   #endif
   #if KB > 47
		lfd	ra1, KB*8+376(pA0)
   #endif
   #if KB > 47
		lfd	ra2, KB2*8+376(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 47
		lfd	ra3, KB3*8+376(pA0)
   #endif
   #if KB > 47
		lfd	rb1, KB*8+376(pB0)
   #endif
   #if KB > 47
		lfd	rb2, KB2*8+376(pB0)
   #endif
   #if KB > 47
		lfd	rb3, KB3*8+376(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=47 block */
#if KB > 47
   #if KB > 48
		lfd	rB0, 384(pB0)
   #endif
   #if KB > 48
		lfd	rA0, 384(pA0)
   #endif
   #if KB > 48
		lfd	rA1, KB*8+384(pA0)
   #endif
   #if KB > 48
		lfd	rA2, KB2*8+384(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 48
		lfd	rA3, KB3*8+384(pA0)
   #endif
   #if KB > 48
		lfd	rB1, KB*8+384(pB0)
   #endif
   #if KB > 48
		lfd	rB2, KB2*8+384(pB0)
   #endif
   #if KB > 48
		lfd	rB3, KB3*8+384(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
   #if KB == 48
	lfd	rB0, 0(pB0)
	lfd	rA0, KB4*8+0(pA0)
	lfd	rA1, KB4*8+KB*8(pA0)
	lfd	rA2, KB4*8+KB2*8(pA0)
   #endif
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
   #if KB == 48
	lfd	rA3, KB4*8+KB3*8(pA0)
	lfd	rB1, KB*8(pB0)
	lfd	rB2, KB2*8(pB0)
	lfd	rB3, KB3*8(pB0)
   #endif
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=48 block */
#if KB > 48
   #if KB > 49
		lfd	rb0, 392(pB0)
   #endif
   #if KB > 49
		lfd	ra0, 392(pA0)
   #endif
   #if KB > 49
		lfd	ra1, KB*8+392(pA0)
   #endif
   #if KB > 49
		lfd	ra2, KB2*8+392(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 49
		lfd	ra3, KB3*8+392(pA0)
   #endif
   #if KB > 49
		lfd	rb1, KB*8+392(pB0)
   #endif
   #if KB > 49
		lfd	rb2, KB2*8+392(pB0)
   #endif
   #if KB > 49
		lfd	rb3, KB3*8+392(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=49 block */
#if KB > 49
   #if KB > 50
		lfd	rB0, 400(pB0)
   #endif
   #if KB > 50
		lfd	rA0, 400(pA0)
   #endif
   #if KB > 50
		lfd	rA1, KB*8+400(pA0)
   #endif
   #if KB > 50
		lfd	rA2, KB2*8+400(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 50
		lfd	rA3, KB3*8+400(pA0)
   #endif
   #if KB > 50
		lfd	rB1, KB*8+400(pB0)
   #endif
   #if KB > 50
		lfd	rB2, KB2*8+400(pB0)
   #endif
   #if KB > 50
		lfd	rB3, KB3*8+400(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=50 block */
#if KB > 50
   #if KB > 51
		lfd	rb0, 408(pB0)
   #endif
   #if KB > 51
		lfd	ra0, 408(pA0)
   #endif
   #if KB > 51
		lfd	ra1, KB*8+408(pA0)
   #endif
   #if KB > 51
		lfd	ra2, KB2*8+408(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 51
		lfd	ra3, KB3*8+408(pA0)
   #endif
   #if KB > 51
		lfd	rb1, KB*8+408(pB0)
   #endif
   #if KB > 51
		lfd	rb2, KB2*8+408(pB0)
   #endif
   #if KB > 51
		lfd	rb3, KB3*8+408(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=51 block */
#if KB > 51
   #if KB > 52
		lfd	rB0, 416(pB0)
   #endif
   #if KB > 52
		lfd	rA0, 416(pA0)
   #endif
   #if KB > 52
		lfd	rA1, KB*8+416(pA0)
   #endif
   #if KB > 52
		lfd	rA2, KB2*8+416(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 52
		lfd	rA3, KB3*8+416(pA0)
   #endif
   #if KB > 52
		lfd	rB1, KB*8+416(pB0)
   #endif
   #if KB > 52
		lfd	rB2, KB2*8+416(pB0)
   #endif
   #if KB > 52
		lfd	rB3, KB3*8+416(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=52 block */
#if KB > 52
   #if KB > 53
		lfd	rb0, 424(pB0)
   #endif
   #if KB > 53
		lfd	ra0, 424(pA0)
   #endif
   #if KB > 53
		lfd	ra1, KB*8+424(pA0)
   #endif
   #if KB > 53
		lfd	ra2, KB2*8+424(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 53
		lfd	ra3, KB3*8+424(pA0)
   #endif
   #if KB > 53
		lfd	rb1, KB*8+424(pB0)
   #endif
   #if KB > 53
		lfd	rb2, KB2*8+424(pB0)
   #endif
   #if KB > 53
		lfd	rb3, KB3*8+424(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=53 block */
#if KB > 53
   #if KB > 54
		lfd	rB0, 432(pB0)
   #endif
   #if KB > 54
		lfd	rA0, 432(pA0)
   #endif
   #if KB > 54
		lfd	rA1, KB*8+432(pA0)
   #endif
   #if KB > 54
		lfd	rA2, KB2*8+432(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 54
		lfd	rA3, KB3*8+432(pA0)
   #endif
   #if KB > 54
		lfd	rB1, KB*8+432(pB0)
   #endif
   #if KB > 54
		lfd	rB2, KB2*8+432(pB0)
   #endif
   #if KB > 54
		lfd	rB3, KB3*8+432(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=54 block */
#if KB > 54
   #if KB > 55
		lfd	rb0, 440(pB0)
   #endif
   #if KB > 55
		lfd	ra0, 440(pA0)
   #endif
   #if KB > 55
		lfd	ra1, KB*8+440(pA0)
   #endif
   #if KB > 55
		lfd	ra2, KB2*8+440(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 55
		lfd	ra3, KB3*8+440(pA0)
   #endif
   #if KB > 55
		lfd	rb1, KB*8+440(pB0)
   #endif
   #if KB > 55
		lfd	rb2, KB2*8+440(pB0)
   #endif
   #if KB > 55
		lfd	rb3, KB3*8+440(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=55 block */
#if KB > 55
   #if KB > 56
		lfd	rB0, 448(pB0)
   #endif
   #if KB > 56
		lfd	rA0, 448(pA0)
   #endif
   #if KB > 56
		lfd	rA1, KB*8+448(pA0)
   #endif
   #if KB > 56
		lfd	rA2, KB2*8+448(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 56
		lfd	rA3, KB3*8+448(pA0)
   #endif
   #if KB > 56
		lfd	rB1, KB*8+448(pB0)
   #endif
   #if KB > 56
		lfd	rB2, KB2*8+448(pB0)
   #endif
   #if KB > 56
		lfd	rB3, KB3*8+448(pB0)
   #endif
   #if KB == 56
	lfd	rB0, 0(pB0)
	lfd	rA0, KB4*8+0(pA0)
	lfd	rA1, KB4*8+KB*8(pA0)
	lfd	rA2, KB4*8+KB2*8(pA0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
   #if KB == 56
	lfd	rA3, KB4*8+KB3*8(pA0)
	lfd	rB1, KB*8(pB0)
	lfd	rB2, KB2*8(pB0)
	lfd	rB3, KB3*8(pB0)
   #endif
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=56 block */
#if KB > 56
   #if KB > 57
		lfd	rb0, 456(pB0)
   #endif
   #if KB > 57
		lfd	ra0, 456(pA0)
   #endif
   #if KB > 57
		lfd	ra1, KB*8+456(pA0)
   #endif
   #if KB > 57
		lfd	ra2, KB2*8+456(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 57
		lfd	ra3, KB3*8+456(pA0)
   #endif
   #if KB > 57
		lfd	rb1, KB*8+456(pB0)
   #endif
   #if KB > 57
		lfd	rb2, KB2*8+456(pB0)
   #endif
   #if KB > 57
		lfd	rb3, KB3*8+456(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=57 block */
#if KB > 57
   #if KB > 58
		lfd	rB0, 464(pB0)
   #endif
   #if KB > 58
		lfd	rA0, 464(pA0)
   #endif
   #if KB > 58
		lfd	rA1, KB*8+464(pA0)
   #endif
   #if KB > 58
		lfd	rA2, KB2*8+464(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 58
		lfd	rA3, KB3*8+464(pA0)
   #endif
   #if KB > 58
		lfd	rB1, KB*8+464(pB0)
   #endif
   #if KB > 58
		lfd	rB2, KB2*8+464(pB0)
   #endif
   #if KB > 58
		lfd	rB3, KB3*8+464(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=58 block */
#if KB > 58
   #if KB > 59
		lfd	rb0, 472(pB0)
   #endif
   #if KB > 59
		lfd	ra0, 472(pA0)
   #endif
   #if KB > 59
		lfd	ra1, KB*8+472(pA0)
   #endif
   #if KB > 59
		lfd	ra2, KB2*8+472(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 59
		lfd	ra3, KB3*8+472(pA0)
   #endif
   #if KB > 59
		lfd	rb1, KB*8+472(pB0)
   #endif
   #if KB > 59
		lfd	rb2, KB2*8+472(pB0)
   #endif
   #if KB > 59
		lfd	rb3, KB3*8+472(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=59 block */
#if KB > 59
   #if KB > 60
		lfd	rB0, 480(pB0)
   #endif
   #if KB > 60
		lfd	rA0, 480(pA0)
   #endif
   #if KB > 60
		lfd	rA1, KB*8+480(pA0)
   #endif
   #if KB > 60
		lfd	rA2, KB2*8+480(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 60
		lfd	rA3, KB3*8+480(pA0)
   #endif
   #if KB > 60
		lfd	rB1, KB*8+480(pB0)
   #endif
   #if KB > 60
		lfd	rB2, KB2*8+480(pB0)
   #endif
   #if KB > 60
		lfd	rB3, KB3*8+480(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=60 block */
#if KB > 60
   #if KB > 61
		lfd	rb0, 488(pB0)
   #endif
   #if KB > 61
		lfd	ra0, 488(pA0)
   #endif
   #if KB > 61
		lfd	ra1, KB*8+488(pA0)
   #endif
   #if KB > 61
		lfd	ra2, KB2*8+488(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 61
		lfd	ra3, KB3*8+488(pA0)
   #endif
   #if KB > 61
		lfd	rb1, KB*8+488(pB0)
   #endif
   #if KB > 61
		lfd	rb2, KB2*8+488(pB0)
   #endif
   #if KB > 61
		lfd	rb3, KB3*8+488(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=61 block */
#if KB > 61
   #if KB > 62
		lfd	rB0, 496(pB0)
   #endif
   #if KB > 62
		lfd	rA0, 496(pA0)
   #endif
   #if KB > 62
		lfd	rA1, KB*8+496(pA0)
   #endif
   #if KB > 62
		lfd	rA2, KB2*8+496(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 62
		lfd	rA3, KB3*8+496(pA0)
   #endif
   #if KB > 62
		lfd	rB1, KB*8+496(pB0)
   #endif
   #if KB > 62
		lfd	rB2, KB2*8+496(pB0)
   #endif
   #if KB > 62
		lfd	rB3, KB3*8+496(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=62 block */
#if KB > 62
   #if KB > 63
		lfd	rb0, 504(pB0)
   #endif
   #if KB > 63
		lfd	ra0, 504(pA0)
   #endif
   #if KB > 63
		lfd	ra1, KB*8+504(pA0)
   #endif
   #if KB > 63
		lfd	ra2, KB2*8+504(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 63
		lfd	ra3, KB3*8+504(pA0)
   #endif
   #if KB > 63
		lfd	rb1, KB*8+504(pB0)
   #endif
   #if KB > 63
		lfd	rb2, KB2*8+504(pB0)
   #endif
   #if KB > 63
		lfd	rb3, KB3*8+504(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=63 block */
#if KB > 63
   #if KB > 64
		lfd	rB0, 512(pB0)
   #endif
   #if KB > 64
		lfd	rA0, 512(pA0)
   #endif
   #if KB > 64
		lfd	rA1, KB*8+512(pA0)
   #endif
   #if KB > 64
		lfd	rA2, KB2*8+512(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 64
		lfd	rA3, KB3*8+512(pA0)
   #endif
   #if KB > 64
		lfd	rB1, KB*8+512(pB0)
   #endif
   #if KB > 64
		lfd	rB2, KB2*8+512(pB0)
   #endif
   #if KB > 64
		lfd	rB3, KB3*8+512(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=64 block */
#if KB > 64
   #if KB > 65
		lfd	rb0, 520(pB0)
   #endif
   #if KB > 65
		lfd	ra0, 520(pA0)
   #endif
   #if KB > 65
		lfd	ra1, KB*8+520(pA0)
   #endif
   #if KB > 65
		lfd	ra2, KB2*8+520(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 65
		lfd	ra3, KB3*8+520(pA0)
   #endif
   #if KB > 65
		lfd	rb1, KB*8+520(pB0)
   #endif
   #if KB > 65
		lfd	rb2, KB2*8+520(pB0)
   #endif
   #if KB > 65
		lfd	rb3, KB3*8+520(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=65 block */
#if KB > 65
   #if KB > 66
		lfd	rB0, 528(pB0)
   #endif
   #if KB > 66
		lfd	rA0, 528(pA0)
   #endif
   #if KB > 66
		lfd	rA1, KB*8+528(pA0)
   #endif
   #if KB > 66
		lfd	rA2, KB2*8+528(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 66
		lfd	rA3, KB3*8+528(pA0)
   #endif
   #if KB > 66
		lfd	rB1, KB*8+528(pB0)
   #endif
   #if KB > 66
		lfd	rB2, KB2*8+528(pB0)
   #endif
   #if KB > 66
		lfd	rB3, KB3*8+528(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=66 block */
#if KB > 66
   #if KB > 67
		lfd	rb0, 536(pB0)
   #endif
   #if KB > 67
		lfd	ra0, 536(pA0)
   #endif
   #if KB > 67
		lfd	ra1, KB*8+536(pA0)
   #endif
   #if KB > 67
		lfd	ra2, KB2*8+536(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 67
		lfd	ra3, KB3*8+536(pA0)
   #endif
   #if KB > 67
		lfd	rb1, KB*8+536(pB0)
   #endif
   #if KB > 67
		lfd	rb2, KB2*8+536(pB0)
   #endif
   #if KB > 67
		lfd	rb3, KB3*8+536(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=67 block */
#if KB > 67
   #if KB > 68
		lfd	rB0, 544(pB0)
   #endif
   #if KB > 68
		lfd	rA0, 544(pA0)
   #endif
   #if KB > 68
		lfd	rA1, KB*8+544(pA0)
   #endif
   #if KB > 68
		lfd	rA2, KB2*8+544(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 68
		lfd	rA3, KB3*8+544(pA0)
   #endif
   #if KB > 68
		lfd	rB1, KB*8+544(pB0)
   #endif
   #if KB > 68
		lfd	rB2, KB2*8+544(pB0)
   #endif
   #if KB > 68
		lfd	rB3, KB3*8+544(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=68 block */
#if KB > 68
   #if KB > 69
		lfd	rb0, 552(pB0)
   #endif
   #if KB > 69
		lfd	ra0, 552(pA0)
   #endif
   #if KB > 69
		lfd	ra1, KB*8+552(pA0)
   #endif
   #if KB > 69
		lfd	ra2, KB2*8+552(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 69
		lfd	ra3, KB3*8+552(pA0)
   #endif
   #if KB > 69
		lfd	rb1, KB*8+552(pB0)
   #endif
   #if KB > 69
		lfd	rb2, KB2*8+552(pB0)
   #endif
   #if KB > 69
		lfd	rb3, KB3*8+552(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=69 block */
#if KB > 69
   #if KB > 70
		lfd	rB0, 560(pB0)
   #endif
   #if KB > 70
		lfd	rA0, 560(pA0)
   #endif
   #if KB > 70
		lfd	rA1, KB*8+560(pA0)
   #endif
   #if KB > 70
		lfd	rA2, KB2*8+560(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 70
		lfd	rA3, KB3*8+560(pA0)
   #endif
   #if KB > 70
		lfd	rB1, KB*8+560(pB0)
   #endif
   #if KB > 70
		lfd	rB2, KB2*8+560(pB0)
   #endif
   #if KB > 70
		lfd	rB3, KB3*8+560(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=70 block */
#if KB > 70
   #if KB > 71
		lfd	rb0, 568(pB0)
   #endif
   #if KB > 71
		lfd	ra0, 568(pA0)
   #endif
   #if KB > 71
		lfd	ra1, KB*8+568(pA0)
   #endif
   #if KB > 71
		lfd	ra2, KB2*8+568(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 71
		lfd	ra3, KB3*8+568(pA0)
   #endif
   #if KB > 71
		lfd	rb1, KB*8+568(pB0)
   #endif
   #if KB > 71
		lfd	rb2, KB2*8+568(pB0)
   #endif
   #if KB > 71
		lfd	rb3, KB3*8+568(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=71 block */
#if KB > 71
   #if KB > 72
		lfd	rB0, 576(pB0)
   #endif
   #if KB > 72
		lfd	rA0, 576(pA0)
   #endif
   #if KB > 72
		lfd	rA1, KB*8+576(pA0)
   #endif
   #if KB > 72
		lfd	rA2, KB2*8+576(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 72
		lfd	rA3, KB3*8+576(pA0)
   #endif
   #if KB > 72
		lfd	rB1, KB*8+576(pB0)
   #endif
   #if KB > 72
		lfd	rB2, KB2*8+576(pB0)
   #endif
   #if KB > 72
		lfd	rB3, KB3*8+576(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=72 block */
#if KB > 72
   #if KB > 73
		lfd	rb0, 584(pB0)
   #endif
   #if KB > 73
		lfd	ra0, 584(pA0)
   #endif
   #if KB > 73
		lfd	ra1, KB*8+584(pA0)
   #endif
   #if KB > 73
		lfd	ra2, KB2*8+584(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 73
		lfd	ra3, KB3*8+584(pA0)
   #endif
   #if KB > 73
		lfd	rb1, KB*8+584(pB0)
   #endif
   #if KB > 73
		lfd	rb2, KB2*8+584(pB0)
   #endif
   #if KB > 73
		lfd	rb3, KB3*8+584(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=73 block */
#if KB > 73
   #if KB > 74
		lfd	rB0, 592(pB0)
   #endif
   #if KB > 74
		lfd	rA0, 592(pA0)
   #endif
   #if KB > 74
		lfd	rA1, KB*8+592(pA0)
   #endif
   #if KB > 74
		lfd	rA2, KB2*8+592(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 74
		lfd	rA3, KB3*8+592(pA0)
   #endif
   #if KB > 74
		lfd	rB1, KB*8+592(pB0)
   #endif
   #if KB > 74
		lfd	rB2, KB2*8+592(pB0)
   #endif
   #if KB > 74
		lfd	rB3, KB3*8+592(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=74 block */
#if KB > 74
   #if KB > 75
		lfd	rb0, 600(pB0)
   #endif
   #if KB > 75
		lfd	ra0, 600(pA0)
   #endif
   #if KB > 75
		lfd	ra1, KB*8+600(pA0)
   #endif
   #if KB > 75
		lfd	ra2, KB2*8+600(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 75
		lfd	ra3, KB3*8+600(pA0)
   #endif
   #if KB > 75
		lfd	rb1, KB*8+600(pB0)
   #endif
   #if KB > 75
		lfd	rb2, KB2*8+600(pB0)
   #endif
   #if KB > 75
		lfd	rb3, KB3*8+600(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=75 block */
#if KB > 75
   #if KB > 76
		lfd	rB0, 608(pB0)
   #endif
   #if KB > 76
		lfd	rA0, 608(pA0)
   #endif
   #if KB > 76
		lfd	rA1, KB*8+608(pA0)
   #endif
   #if KB > 76
		lfd	rA2, KB2*8+608(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 76
		lfd	rA3, KB3*8+608(pA0)
   #endif
   #if KB > 76
		lfd	rB1, KB*8+608(pB0)
   #endif
   #if KB > 76
		lfd	rB2, KB2*8+608(pB0)
   #endif
   #if KB > 76
		lfd	rB3, KB3*8+608(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=76 block */
#if KB > 76
   #if KB > 77
		lfd	rb0, 616(pB0)
   #endif
   #if KB > 77
		lfd	ra0, 616(pA0)
   #endif
   #if KB > 77
		lfd	ra1, KB*8+616(pA0)
   #endif
   #if KB > 77
		lfd	ra2, KB2*8+616(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 77
		lfd	ra3, KB3*8+616(pA0)
   #endif
   #if KB > 77
		lfd	rb1, KB*8+616(pB0)
   #endif
   #if KB > 77
		lfd	rb2, KB2*8+616(pB0)
   #endif
   #if KB > 77
		lfd	rb3, KB3*8+616(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=77 block */
#if KB > 77
   #if KB > 78
		lfd	rB0, 624(pB0)
   #endif
   #if KB > 78
		lfd	rA0, 624(pA0)
   #endif
   #if KB > 78
		lfd	rA1, KB*8+624(pA0)
   #endif
   #if KB > 78
		lfd	rA2, KB2*8+624(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 78
		lfd	rA3, KB3*8+624(pA0)
   #endif
   #if KB > 78
		lfd	rB1, KB*8+624(pB0)
   #endif
   #if KB > 78
		lfd	rB2, KB2*8+624(pB0)
   #endif
   #if KB > 78
		lfd	rB3, KB3*8+624(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=78 block */
#if KB > 78
   #if KB > 79
		lfd	rb0, 632(pB0)
   #endif
   #if KB > 79
		lfd	ra0, 632(pA0)
   #endif
   #if KB > 79
		lfd	ra1, KB*8+632(pA0)
   #endif
   #if KB > 79
		lfd	ra2, KB2*8+632(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 79
		lfd	ra3, KB3*8+632(pA0)
   #endif
   #if KB > 79
		lfd	rb1, KB*8+632(pB0)
   #endif
   #if KB > 79
		lfd	rb2, KB2*8+632(pB0)
   #endif
   #if KB > 79
		lfd	rb3, KB3*8+632(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=79 block */
#if KB > 79
   #if KB > 80
		lfd	rB0, 640(pB0)
   #endif
   #if KB > 80
		lfd	rA0, 640(pA0)
   #endif
   #if KB > 80
		lfd	rA1, KB*8+640(pA0)
   #endif
   #if KB > 80
		lfd	rA2, KB2*8+640(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 80
		lfd	rA3, KB3*8+640(pA0)
   #endif
   #if KB > 80
		lfd	rB1, KB*8+640(pB0)
   #endif
   #if KB > 80
		lfd	rB2, KB2*8+640(pB0)
   #endif
   #if KB > 80
		lfd	rB3, KB3*8+640(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=80 block */
/* End KLOOP */
/*
 *      Store to C, iterate loop
 */
#if KB != 56 && KB != 36  && KB != 48
	lfd	rB0, 0(pB0)
	lfd	rA0, KB4*8+0(pA0)
	lfd	rA1, KB4*8+KB*8(pA0)
	lfd	rA2, KB4*8+KB2*8(pA0)
	lfd	rA3, KB4*8+KB3*8(pA0)
	lfd	rB1, KB*8(pB0)
	lfd	rB2, KB2*8(pB0)
	lfd	rB3, KB3*8(pB0)
#endif

        stfd    rC00, -CMUL(32)(pC0)
        stfd    rC10, CMUL(8-32)(pC0)
        stfd    rC20, CMUL(16-32)(pC0)
        stfd    rC30, CMUL(24-32)(pC0)

        stfd    rC01, -CMUL(32)(pC1)
        stfd    rC11, CMUL(8-32)(pC1)
        stfd    rC21, CMUL(16-32)(pC1)
        stfd    rC31, CMUL(24-32)(pC1)

        stfd    rC02, -CMUL(32)(pC2)
        stfd    rC12, CMUL(8-32)(pC2)
        stfd    rC22, CMUL(16-32)(pC2)
        stfd    rC32, CMUL(24-32)(pC2)

        stfd    rC03, -CMUL(32)(pC3)
        stfd    rC13, CMUL(8-32)(pC3)
        stfd    rC23, CMUL(16-32)(pC3)
        stfd    rC33, CMUL(24-32)(pC3)
/*
 *      Mov ptrs, while(M)
 */
        addi    pA0, pA0, KB4*8         /* pA0 += 4*lda */
	nop
	nop
	nop
        bdnz+   MLOOP
/*
 *      Last iteration of M-loop unrolled so we can intermix M iterations
 */
/* Begin KLOOP */

#if MB == 0
MPEELED:
#endif
#if KB > 0
   #ifdef BETA0
      #if KB > 1
		lfd	rb0, 8(pB0)
      #endif
      #if KB > 1
		lfd	ra0, 8(pA0)
      #endif
      #if KB > 1
		lfd	ra1, KB*8+8(pA0)
      #endif
      #if KB > 1
		lfd	ra2, KB2*8+8(pA0)
      #endif
	fmul 	rC00, rA0, rB0
	fmul 	rC10, rA1, rB0
	fmul 	rC20, rA2, rB0
	fmul 	rC30, rA3, rB0
      #if KB > 1
		lfd	ra3, KB3*8+8(pA0)
      #endif
      #if KB > 1
		lfd	rb1, KB*8+8(pB0)
      #endif
      #if KB > 1
		lfd	rb2, KB2*8+8(pB0)
      #endif
      #if KB > 1
		lfd	rb3, KB3*8+8(pB0)
      #endif
	fmul 	rC01, rA0, rB1
	fmul 	rC11, rA1, rB1
	fmul 	rC21, rA2, rB1
	fmul 	rC31, rA3, rB1
		dcbt	0, pfA, 0
        	addi    pfA, pfA, 128
		dcbt	0, pfB, 0
        	addi    pfB, pfB, 128
	fmul 	rC02, rA0, rB2
	fmul 	rC12, rA1, rB2
	fmul 	rC22, rA2, rB2
	fmul 	rC32, rA3, rB2
	fmul 	rC03, rA0, rB3
	fmul 	rC13, rA1, rB3
	fmul 	rC23, rA2, rB3
	fmul 	rC33, rA3, rB3
   #elif defined(BETAX)
	lfd	rb3, BOFF(r1)
        lfd     rC00, 0(pC0)
        lfd     rC10, CMUL(8)(pC0)
        lfd     rC20, CMUL(16)(pC0)
        lfd     rC30, CMUL(24)(pC0)
	nop
	nop
	nop
        lfd     rC01, 0(pC1)
        lfd     rC11, CMUL(8)(pC1)
        lfd     rC21, CMUL(16)(pC1)
        lfd     rC31, CMUL(24)(pC1)
	fmul	rC00, rC00, rb3
	fmul	rC10, rC10, rb3
	fmul	rC20, rC20, rb3
	fmul	rC30, rC30, rb3
        lfd     rC02, 0(pC2)
        lfd     rC12, CMUL(8)(pC2)
        lfd     rC22, CMUL(16)(pC2)
        lfd     rC32, CMUL(24)(pC2)
	fmul	rC01, rC01, rb3
	fmul	rC11, rC11, rb3
	fmul	rC21, rC21, rb3
	fmul	rC31, rC31, rb3
        lfd     rC03, 0(pC3)
        lfd     rC13, CMUL(8)(pC3)
        lfd     rC23, CMUL(16)(pC3)
        lfd     rC33, CMUL(24)(pC3)
	fmul	rC02, rC02, rb3
	fmul	rC12, rC12, rb3
	fmul	rC22, rC22, rb3
	fmul	rC32, rC32, rb3
	fmul	rC03, rC03, rb3
	fmul	rC13, rC13, rb3
	fmul	rC23, rC23, rb3
	fmul	rC33, rC33, rb3
      #if KB > 1
		lfd	rb0, 8(pB0)
      #endif
      #if KB > 1
		lfd	ra0, 8(pA0)
      #endif
      #if KB > 1
		lfd	ra1, KB*8+8(pA0)
      #endif
      #if KB > 1
		lfd	ra2, KB2*8+8(pA0)
      #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
      #if KB > 1
		lfd	ra3, KB3*8+8(pA0)
      #endif
      #if KB > 1
		lfd	rb1, KB*8+8(pB0)
      #endif
      #if KB > 1
		lfd	rb2, KB2*8+8(pB0)
      #endif
      #if KB > 1
		lfd	rb3, KB3*8+8(pB0)
      #endif
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
		dcbt	0, pfA, 0
        	addi    pfA, pfA, 128
		dcbt	0, pfB, 0
        	addi    pfB, pfB, 128
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
   #else  /* BETA == 1 */
        lfd     rC00, 0(pC0)
        lfd     rC10, CMUL(8)(pC0)
        lfd     rC20, CMUL(16)(pC0)
        lfd     rC30, CMUL(24)(pC0)
        lfd     rC01, 0(pC1)
        lfd     rC11, CMUL(8)(pC1)
        lfd     rC21, CMUL(16)(pC1)
        lfd     rC31, CMUL(24)(pC1)
        lfd     rC02, 0(pC2)
        lfd     rC12, CMUL(8)(pC2)
        lfd     rC22, CMUL(16)(pC2)
        lfd     rC32, CMUL(24)(pC2)
        	lfd     rC03, 0(pC3)
        	lfd     rC13, CMUL(8)(pC3)
        	lfd     rC23, CMUL(16)(pC3)
        	lfd     rC33, CMUL(24)(pC3)
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
		dcbt	0, pfA, 0
		dcbt	0, pfB, 0
        	addi    pfA, pfA, 128
        	addi    pfB, pfB, 128
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
      #if KB > 1
		lfd	rb0, 8(pB0)
      #endif
      #if KB > 1
		lfd	ra0, 8(pA0)
      #endif
      #if KB > 1
		lfd	ra1, KB*8+8(pA0)
      #endif
      #if KB > 1
		lfd	ra2, KB2*8+8(pA0)
      #endif
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
      #if KB > 1
		lfd	ra3, KB3*8+8(pA0)
      #endif
      #if KB > 1
		lfd	rb1, KB*8+8(pB0)
      #endif
      #if KB > 1
		lfd	rb2, KB2*8+8(pB0)
      #endif
      #if KB > 1
		lfd	rb3, KB3*8+8(pB0)
      #endif
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
   #endif /* done BETA specialization */
#endif  /* end K=1 block */
#if KB > 1
   #if KB > 2
		lfd	rB0, 16(pB0)
   #endif
   #if KB > 2
		lfd	rA0, 16(pA0)
   #endif
   #if KB > 2
		lfd	rA1, KB*8+16(pA0)
   #endif
   #if KB > 2
		lfd	rA2, KB2*8+16(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 2
		lfd	rA3, KB3*8+16(pA0)
   #endif
   #if KB > 2
		lfd	rB1, KB*8+16(pB0)
   #endif
   #if KB > 2
		lfd	rB2, KB2*8+16(pB0)
   #endif
   #if KB > 2
		lfd	rB3, KB3*8+16(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=2 block */
#if KB > 2
   #if KB > 3
		lfd	rb0, 24(pB0)
   #endif
   #if KB > 3
		lfd	ra0, 24(pA0)
   #endif
   #if KB > 3
		lfd	ra1, KB*8+24(pA0)
   #endif
   #if KB > 3
		lfd	ra2, KB2*8+24(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 3
		lfd	ra3, KB3*8+24(pA0)
   #endif
   #if KB > 3
		lfd	rb1, KB*8+24(pB0)
   #endif
   #if KB > 3
		lfd	rb2, KB2*8+24(pB0)
   #endif
   #if KB > 3
		lfd	rb3, KB3*8+24(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
        	addi    pC0, pC0, CMUL(4)*8     /* pC0 += 4 */
        	addi    pC1, pC1, CMUL(4)*8
        	addi    pC2, pC2, CMUL(4)*8
        	addi    pC3, pC3, CMUL(4)*8
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#else
        	addi    pC0, pC0, CMUL(4)*8     /* pC0 += 4 */
        	addi    pC1, pC1, CMUL(4)*8
        	addi    pC2, pC2, CMUL(4)*8
        	addi    pC3, pC3, CMUL(4)*8
#endif  /* end K=3 block */
#if KB > 3
   #if KB > 4
		lfd	rB0, 32(pB0)
   #endif
   #if KB > 4
		lfd	rA0, 32(pA0)
   #endif
   #if KB > 4
		lfd	rA1, KB*8+32(pA0)
   #endif
   #if KB > 4
		lfd	rA2, KB2*8+32(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 4
		lfd	rA3, KB3*8+32(pA0)
   #endif
   #if KB > 4
		lfd	rB1, KB*8+32(pB0)
   #endif
   #if KB > 4
		lfd	rB2, KB2*8+32(pB0)
   #endif
   #if KB > 4
		lfd	rB3, KB3*8+32(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=4 block */
#if KB > 4
   #if KB > 5
		lfd	rb0, 40(pB0)
   #endif
   #if KB > 5
		lfd	ra0, 40(pA0)
   #endif
   #if KB > 5
		lfd	ra1, KB*8+40(pA0)
   #endif
   #if KB > 5
		lfd	ra2, KB2*8+40(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 5
		lfd	ra3, KB3*8+40(pA0)
   #endif
   #if KB > 5
		lfd	rb1, KB*8+40(pB0)
   #endif
   #if KB > 5
		lfd	rb2, KB2*8+40(pB0)
   #endif
   #if KB > 5
		lfd	rb3, KB3*8+40(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=5 block */
#if KB > 5
   #if KB > 6
		lfd	rB0, 48(pB0)
   #endif
   #if KB > 6
		lfd	rA0, 48(pA0)
   #endif
   #if KB > 6
		lfd	rA1, KB*8+48(pA0)
   #endif
   #if KB > 6
		lfd	rA2, KB2*8+48(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 6
		lfd	rA3, KB3*8+48(pA0)
   #endif
   #if KB > 6
		lfd	rB1, KB*8+48(pB0)
   #endif
   #if KB > 6
		lfd	rB2, KB2*8+48(pB0)
   #endif
   #if KB > 6
		lfd	rB3, KB3*8+48(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=6 block */
#if KB > 6
   #if KB > 7
		lfd	rb0, 56(pB0)
   #endif
   #if KB > 7
		lfd	ra0, 56(pA0)
   #endif
   #if KB > 7
		lfd	ra1, KB*8+56(pA0)
   #endif
   #if KB > 7
		lfd	ra2, KB2*8+56(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 7
		lfd	ra3, KB3*8+56(pA0)
   #endif
   #if KB > 7
		lfd	rb1, KB*8+56(pB0)
   #endif
   #if KB > 7
		lfd	rb2, KB2*8+56(pB0)
   #endif
   #if KB > 7
		lfd	rb3, KB3*8+56(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=7 block */
#if KB > 7
   #if KB > 8
		lfd	rB0, 64(pB0)
   #endif
   #if KB > 8
		lfd	rA0, 64(pA0)
   #endif
   #if KB > 8
		lfd	rA1, KB*8+64(pA0)
   #endif
   #if KB > 8
		lfd	rA2, KB2*8+64(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 8
		lfd	rA3, KB3*8+64(pA0)
   #endif
   #if KB > 8
		lfd	rB1, KB*8+64(pB0)
   #endif
   #if KB > 8
		lfd	rB2, KB2*8+64(pB0)
   #endif
   #if KB > 8
		lfd	rB3, KB3*8+64(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=8 block */
#if KB > 8
   #if KB > 9
		lfd	rb0, 72(pB0)
   #endif
   #if KB > 9
		lfd	ra0, 72(pA0)
   #endif
   #if KB > 9
		lfd	ra1, KB*8+72(pA0)
   #endif
   #if KB > 9
		lfd	ra2, KB2*8+72(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 9
		lfd	ra3, KB3*8+72(pA0)
   #endif
   #if KB > 9
		lfd	rb1, KB*8+72(pB0)
   #endif
   #if KB > 9
		lfd	rb2, KB2*8+72(pB0)
   #endif
   #if KB > 9
		lfd	rb3, KB3*8+72(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=9 block */
#if KB > 9
   #if KB > 10
		lfd	rB0, 80(pB0)
   #endif
   #if KB > 10
		lfd	rA0, 80(pA0)
   #endif
   #if KB > 10
		lfd	rA1, KB*8+80(pA0)
   #endif
   #if KB > 10
		lfd	rA2, KB2*8+80(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 10
		lfd	rA3, KB3*8+80(pA0)
   #endif
   #if KB > 10
		lfd	rB1, KB*8+80(pB0)
   #endif
   #if KB > 10
		lfd	rB2, KB2*8+80(pB0)
   #endif
   #if KB > 10
		lfd	rB3, KB3*8+80(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=10 block */
#if KB > 10
   #if KB > 11
		lfd	rb0, 88(pB0)
   #endif
   #if KB > 11
		lfd	ra0, 88(pA0)
   #endif
   #if KB > 11
		lfd	ra1, KB*8+88(pA0)
   #endif
   #if KB > 11
		lfd	ra2, KB2*8+88(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 11
		lfd	ra3, KB3*8+88(pA0)
   #endif
   #if KB > 11
		lfd	rb1, KB*8+88(pB0)
   #endif
   #if KB > 11
		lfd	rb2, KB2*8+88(pB0)
   #endif
   #if KB > 11
		lfd	rb3, KB3*8+88(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=11 block */
#if KB > 11
   #if KB > 12
		lfd	rB0, 96(pB0)
   #endif
   #if KB > 12
		lfd	rA0, 96(pA0)
   #endif
   #if KB > 12
		lfd	rA1, KB*8+96(pA0)
   #endif
   #if KB > 12
		lfd	rA2, KB2*8+96(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 12
		lfd	rA3, KB3*8+96(pA0)
   #endif
   #if KB > 12
		lfd	rB1, KB*8+96(pB0)
   #endif
   #if KB > 12
		lfd	rB2, KB2*8+96(pB0)
   #endif
   #if KB > 12
		lfd	rB3, KB3*8+96(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=12 block */
#if KB > 12
   #if KB > 13
		lfd	rb0, 104(pB0)
   #endif
   #if KB > 13
		lfd	ra0, 104(pA0)
   #endif
   #if KB > 13
		lfd	ra1, KB*8+104(pA0)
   #endif
   #if KB > 13
		lfd	ra2, KB2*8+104(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 13
		lfd	ra3, KB3*8+104(pA0)
   #endif
   #if KB > 13
		lfd	rb1, KB*8+104(pB0)
   #endif
   #if KB > 13
		lfd	rb2, KB2*8+104(pB0)
   #endif
   #if KB > 13
		lfd	rb3, KB3*8+104(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=13 block */
#if KB > 13
   #if KB > 14
		lfd	rB0, 112(pB0)
   #endif
   #if KB > 14
		lfd	rA0, 112(pA0)
   #endif
   #if KB > 14
		lfd	rA1, KB*8+112(pA0)
   #endif
   #if KB > 14
		lfd	rA2, KB2*8+112(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 14
		lfd	rA3, KB3*8+112(pA0)
   #endif
   #if KB > 14
		lfd	rB1, KB*8+112(pB0)
   #endif
   #if KB > 14
		lfd	rB2, KB2*8+112(pB0)
   #endif
   #if KB > 14
		lfd	rB3, KB3*8+112(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=14 block */
#if KB > 14
   #if KB > 15
		lfd	rb0, 120(pB0)
   #endif
   #if KB > 15
		lfd	ra0, 120(pA0)
   #endif
   #if KB > 15
		lfd	ra1, KB*8+120(pA0)
   #endif
   #if KB > 15
		lfd	ra2, KB2*8+120(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 15
		lfd	ra3, KB3*8+120(pA0)
   #endif
   #if KB > 15
		lfd	rb1, KB*8+120(pB0)
   #endif
   #if KB > 15
		lfd	rb2, KB2*8+120(pB0)
   #endif
   #if KB > 15
		lfd	rb3, KB3*8+120(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=15 block */
#if KB > 15
   #if KB > 16
		lfd	rB0, 128(pB0)
   #endif
   #if KB > 16
		lfd	rA0, 128(pA0)
   #endif
   #if KB > 16
		lfd	rA1, KB*8+128(pA0)
   #endif
   #if KB > 16
		lfd	rA2, KB2*8+128(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 16
		lfd	rA3, KB3*8+128(pA0)
   #endif
   #if KB > 16
		lfd	rB1, KB*8+128(pB0)
   #endif
   #if KB > 16
		lfd	rB2, KB2*8+128(pB0)
   #endif
   #if KB > 16
		lfd	rB3, KB3*8+128(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=16 block */
#if KB > 16
   #if KB > 17
		lfd	rb0, 136(pB0)
   #endif
   #if KB > 17
		lfd	ra0, 136(pA0)
   #endif
   #if KB > 17
		lfd	ra1, KB*8+136(pA0)
   #endif
   #if KB > 17
		lfd	ra2, KB2*8+136(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 17
		lfd	ra3, KB3*8+136(pA0)
   #endif
   #if KB > 17
		lfd	rb1, KB*8+136(pB0)
   #endif
   #if KB > 17
		lfd	rb2, KB2*8+136(pB0)
   #endif
   #if KB > 17
		lfd	rb3, KB3*8+136(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=17 block */
#if KB > 17
   #if KB > 18
		lfd	rB0, 144(pB0)
   #endif
   #if KB > 18
		lfd	rA0, 144(pA0)
   #endif
   #if KB > 18
		lfd	rA1, KB*8+144(pA0)
   #endif
   #if KB > 18
		lfd	rA2, KB2*8+144(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 18
		lfd	rA3, KB3*8+144(pA0)
   #endif
   #if KB > 18
		lfd	rB1, KB*8+144(pB0)
   #endif
   #if KB > 18
		lfd	rB2, KB2*8+144(pB0)
   #endif
   #if KB > 18
		lfd	rB3, KB3*8+144(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=18 block */
#if KB > 18
   #if KB > 19
		lfd	rb0, 152(pB0)
   #endif
   #if KB > 19
		lfd	ra0, 152(pA0)
   #endif
   #if KB > 19
		lfd	ra1, KB*8+152(pA0)
   #endif
   #if KB > 19
		lfd	ra2, KB2*8+152(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 19
		lfd	ra3, KB3*8+152(pA0)
   #endif
   #if KB > 19
		lfd	rb1, KB*8+152(pB0)
   #endif
   #if KB > 19
		lfd	rb2, KB2*8+152(pB0)
   #endif
   #if KB > 19
		lfd	rb3, KB3*8+152(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=19 block */
#if KB > 19
   #if KB > 20
		lfd	rB0, 160(pB0)
   #endif
   #if KB > 20
		lfd	rA0, 160(pA0)
   #endif
   #if KB > 20
		lfd	rA1, KB*8+160(pA0)
   #endif
   #if KB > 20
		lfd	rA2, KB2*8+160(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 20
		lfd	rA3, KB3*8+160(pA0)
   #endif
   #if KB > 20
		lfd	rB1, KB*8+160(pB0)
   #endif
   #if KB > 20
		lfd	rB2, KB2*8+160(pB0)
   #endif
   #if KB > 20
		lfd	rB3, KB3*8+160(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=20 block */
#if KB > 20
   #if KB > 21
		lfd	rb0, 168(pB0)
   #endif
   #if KB > 21
		lfd	ra0, 168(pA0)
   #endif
   #if KB > 21
		lfd	ra1, KB*8+168(pA0)
   #endif
   #if KB > 21
		lfd	ra2, KB2*8+168(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 21
		lfd	ra3, KB3*8+168(pA0)
   #endif
   #if KB > 21
		lfd	rb1, KB*8+168(pB0)
   #endif
   #if KB > 21
		lfd	rb2, KB2*8+168(pB0)
   #endif
   #if KB > 21
		lfd	rb3, KB3*8+168(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=21 block */
#if KB > 21
   #if KB > 22
		lfd	rB0, 176(pB0)
   #endif
   #if KB > 22
		lfd	rA0, 176(pA0)
   #endif
   #if KB > 22
		lfd	rA1, KB*8+176(pA0)
   #endif
   #if KB > 22
		lfd	rA2, KB2*8+176(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 22
		lfd	rA3, KB3*8+176(pA0)
   #endif
   #if KB > 22
		lfd	rB1, KB*8+176(pB0)
   #endif
   #if KB > 22
		lfd	rB2, KB2*8+176(pB0)
   #endif
   #if KB > 22
		lfd	rB3, KB3*8+176(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=22 block */
#if KB > 22
   #if KB > 23
		lfd	rb0, 184(pB0)
   #endif
   #if KB > 23
		lfd	ra0, 184(pA0)
   #endif
   #if KB > 23
		lfd	ra1, KB*8+184(pA0)
   #endif
   #if KB > 23
		lfd	ra2, KB2*8+184(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 23
		lfd	ra3, KB3*8+184(pA0)
   #endif
   #if KB > 23
		lfd	rb1, KB*8+184(pB0)
   #endif
   #if KB > 23
		lfd	rb2, KB2*8+184(pB0)
   #endif
   #if KB > 23
		lfd	rb3, KB3*8+184(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=23 block */
#if KB > 23
   #if KB > 24
		lfd	rB0, 192(pB0)
   #endif
   #if KB > 24
		lfd	rA0, 192(pA0)
   #endif
   #if KB > 24
		lfd	rA1, KB*8+192(pA0)
   #endif
   #if KB > 24
		lfd	rA2, KB2*8+192(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 24
		lfd	rA3, KB3*8+192(pA0)
   #endif
   #if KB > 24
		lfd	rB1, KB*8+192(pB0)
   #endif
   #if KB > 24
		lfd	rB2, KB2*8+192(pB0)
   #endif
   #if KB > 24
		lfd	rB3, KB3*8+192(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=24 block */
#if KB > 24
   #if KB > 25
		lfd	rb0, 200(pB0)
   #endif
   #if KB > 25
		lfd	ra0, 200(pA0)
   #endif
   #if KB > 25
		lfd	ra1, KB*8+200(pA0)
   #endif
   #if KB > 25
		lfd	ra2, KB2*8+200(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 25
		lfd	ra3, KB3*8+200(pA0)
   #endif
   #if KB > 25
		lfd	rb1, KB*8+200(pB0)
   #endif
   #if KB > 25
		lfd	rb2, KB2*8+200(pB0)
   #endif
   #if KB > 25
		lfd	rb3, KB3*8+200(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=25 block */
#if KB > 25
   #if KB > 26
		lfd	rB0, 208(pB0)
   #endif
   #if KB > 26
		lfd	rA0, 208(pA0)
   #endif
   #if KB > 26
		lfd	rA1, KB*8+208(pA0)
   #endif
   #if KB > 26
		lfd	rA2, KB2*8+208(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 26
		lfd	rA3, KB3*8+208(pA0)
   #endif
   #if KB > 26
		lfd	rB1, KB*8+208(pB0)
   #endif
   #if KB > 26
		lfd	rB2, KB2*8+208(pB0)
   #endif
   #if KB > 26
		lfd	rB3, KB3*8+208(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=26 block */
#if KB > 26
   #if KB > 27
		lfd	rb0, 216(pB0)
   #endif
   #if KB > 27
		lfd	ra0, 216(pA0)
   #endif
   #if KB > 27
		lfd	ra1, KB*8+216(pA0)
   #endif
   #if KB > 27
		lfd	ra2, KB2*8+216(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 27
		lfd	ra3, KB3*8+216(pA0)
   #endif
   #if KB > 27
		lfd	rb1, KB*8+216(pB0)
   #endif
   #if KB > 27
		lfd	rb2, KB2*8+216(pB0)
   #endif
   #if KB > 27
		lfd	rb3, KB3*8+216(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=27 block */
#if KB > 27
   #if KB > 28
		lfd	rB0, 224(pB0)
   #endif
   #if KB > 28
		lfd	rA0, 224(pA0)
   #endif
   #if KB > 28
		lfd	rA1, KB*8+224(pA0)
   #endif
   #if KB > 28
		lfd	rA2, KB2*8+224(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 28
		lfd	rA3, KB3*8+224(pA0)
   #endif
   #if KB > 28
		lfd	rB1, KB*8+224(pB0)
   #endif
   #if KB > 28
		lfd	rB2, KB2*8+224(pB0)
   #endif
   #if KB > 28
		lfd	rB3, KB3*8+224(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=28 block */
#if KB > 28
   #if KB > 29
		lfd	rb0, 232(pB0)
   #endif
   #if KB > 29
		lfd	ra0, 232(pA0)
   #endif
   #if KB > 29
		lfd	ra1, KB*8+232(pA0)
   #endif
   #if KB > 29
		lfd	ra2, KB2*8+232(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 29
		lfd	ra3, KB3*8+232(pA0)
   #endif
   #if KB > 29
		lfd	rb1, KB*8+232(pB0)
   #endif
   #if KB > 29
		lfd	rb2, KB2*8+232(pB0)
   #endif
   #if KB > 29
		lfd	rb3, KB3*8+232(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=29 block */
#if KB > 29
   #if KB > 30
		lfd	rB0, 240(pB0)
   #endif
   #if KB > 30
		lfd	rA0, 240(pA0)
   #endif
   #if KB > 30
		lfd	rA1, KB*8+240(pA0)
   #endif
   #if KB > 30
		lfd	rA2, KB2*8+240(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 30
		lfd	rA3, KB3*8+240(pA0)
   #endif
   #if KB > 30
		lfd	rB1, KB*8+240(pB0)
   #endif
   #if KB > 30
		lfd	rB2, KB2*8+240(pB0)
   #endif
   #if KB > 30
		lfd	rB3, KB3*8+240(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=30 block */
#if KB > 30
   #if KB > 31
		lfd	rb0, 248(pB0)
   #endif
   #if KB > 31
		lfd	ra0, 248(pA0)
   #endif
   #if KB > 31
		lfd	ra1, KB*8+248(pA0)
   #endif
   #if KB > 31
		lfd	ra2, KB2*8+248(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 31
		lfd	ra3, KB3*8+248(pA0)
   #endif
   #if KB > 31
		lfd	rb1, KB*8+248(pB0)
   #endif
   #if KB > 31
		lfd	rb2, KB2*8+248(pB0)
   #endif
   #if KB > 31
		lfd	rb3, KB3*8+248(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=31 block */
#if KB > 31
   #if KB > 32
		lfd	rB0, 256(pB0)
   #endif
   #if KB > 32
		lfd	rA0, 256(pA0)
   #endif
   #if KB > 32
		lfd	rA1, KB*8+256(pA0)
   #endif
   #if KB > 32
		lfd	rA2, KB2*8+256(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 32
		lfd	rA3, KB3*8+256(pA0)
   #endif
   #if KB > 32
		lfd	rB1, KB*8+256(pB0)
   #endif
   #if KB > 32
		lfd	rB2, KB2*8+256(pB0)
   #endif
   #if KB > 32
		lfd	rB3, KB3*8+256(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=32 block */
#if KB > 32
   #if KB > 33
		lfd	rb0, 264(pB0)
   #endif
   #if KB > 33
		lfd	ra0, 264(pA0)
   #endif
   #if KB > 33
		lfd	ra1, KB*8+264(pA0)
   #endif
   #if KB > 33
		lfd	ra2, KB2*8+264(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 33
		lfd	ra3, KB3*8+264(pA0)
   #endif
   #if KB > 33
		lfd	rb1, KB*8+264(pB0)
   #endif
   #if KB > 33
		lfd	rb2, KB2*8+264(pB0)
   #endif
   #if KB > 33
		lfd	rb3, KB3*8+264(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=33 block */
#if KB > 33
   #if KB > 34
		lfd	rB0, 272(pB0)
   #endif
   #if KB > 34
		lfd	rA0, 272(pA0)
   #endif
   #if KB > 34
		lfd	rA1, KB*8+272(pA0)
   #endif
   #if KB > 34
		lfd	rA2, KB2*8+272(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 34
		lfd	rA3, KB3*8+272(pA0)
   #endif
   #if KB > 34
		lfd	rB1, KB*8+272(pB0)
   #endif
   #if KB > 34
		lfd	rB2, KB2*8+272(pB0)
   #endif
   #if KB > 34
		lfd	rB3, KB3*8+272(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=34 block */
#if KB > 34
   #if KB > 35
		lfd	rb0, 280(pB0)
   #endif
   #if KB > 35
		lfd	ra0, 280(pA0)
   #endif
   #if KB > 35
		lfd	ra1, KB*8+280(pA0)
   #endif
   #if KB > 35
		lfd	ra2, KB2*8+280(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 35
		lfd	ra3, KB3*8+280(pA0)
   #endif
   #if KB > 35
		lfd	rb1, KB*8+280(pB0)
   #endif
   #if KB > 35
		lfd	rb2, KB2*8+280(pB0)
   #endif
   #if KB > 35
		lfd	rb3, KB3*8+280(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=35 block */
#if KB > 35
   #if KB > 36
		lfd	rB0, 288(pB0)
   #endif
   #if KB > 36
		lfd	rA0, 288(pA0)
   #endif
   #if KB > 36
		lfd	rA1, KB*8+288(pA0)
   #endif
   #if KB > 36
		lfd	rA2, KB2*8+288(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 36
		lfd	rA3, KB3*8+288(pA0)
   #endif
   #if KB > 36
		lfd	rB1, KB*8+288(pB0)
   #endif
   #if KB > 36
		lfd	rB2, KB2*8+288(pB0)
   #endif
   #if KB > 36
		lfd	rB3, KB3*8+288(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=36 block */
#if KB > 36
   #if KB > 37
		lfd	rb0, 296(pB0)
   #endif
   #if KB > 37
		lfd	ra0, 296(pA0)
   #endif
   #if KB > 37
		lfd	ra1, KB*8+296(pA0)
   #endif
   #if KB > 37
		lfd	ra2, KB2*8+296(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 37
		lfd	ra3, KB3*8+296(pA0)
   #endif
   #if KB > 37
		lfd	rb1, KB*8+296(pB0)
   #endif
   #if KB > 37
		lfd	rb2, KB2*8+296(pB0)
   #endif
   #if KB > 37
		lfd	rb3, KB3*8+296(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=37 block */
#if KB > 37
   #if KB > 38
		lfd	rB0, 304(pB0)
   #endif
   #if KB > 38
		lfd	rA0, 304(pA0)
   #endif
   #if KB > 38
		lfd	rA1, KB*8+304(pA0)
   #endif
   #if KB > 38
		lfd	rA2, KB2*8+304(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 38
		lfd	rA3, KB3*8+304(pA0)
   #endif
   #if KB > 38
		lfd	rB1, KB*8+304(pB0)
   #endif
   #if KB > 38
		lfd	rB2, KB2*8+304(pB0)
   #endif
   #if KB > 38
		lfd	rB3, KB3*8+304(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=38 block */
#if KB > 38
   #if KB > 39
		lfd	rb0, 312(pB0)
   #endif
   #if KB > 39
		lfd	ra0, 312(pA0)
   #endif
   #if KB > 39
		lfd	ra1, KB*8+312(pA0)
   #endif
   #if KB > 39
		lfd	ra2, KB2*8+312(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 39
		lfd	ra3, KB3*8+312(pA0)
   #endif
   #if KB > 39
		lfd	rb1, KB*8+312(pB0)
   #endif
   #if KB > 39
		lfd	rb2, KB2*8+312(pB0)
   #endif
   #if KB > 39
		lfd	rb3, KB3*8+312(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=39 block */
#if KB > 39
   #if KB > 40
		lfd	rB0, 320(pB0)
   #endif
   #if KB > 40
		lfd	rA0, 320(pA0)
   #endif
   #if KB > 40
		lfd	rA1, KB*8+320(pA0)
   #endif
   #if KB > 40
		lfd	rA2, KB2*8+320(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 40
		lfd	rA3, KB3*8+320(pA0)
   #endif
   #if KB > 40
		lfd	rB1, KB*8+320(pB0)
   #endif
   #if KB > 40
		lfd	rB2, KB2*8+320(pB0)
   #endif
   #if KB > 40
		lfd	rB3, KB3*8+320(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=40 block */
#if KB > 40
   #if KB > 41
		lfd	rb0, 328(pB0)
   #endif
   #if KB > 41
		lfd	ra0, 328(pA0)
   #endif
   #if KB > 41
		lfd	ra1, KB*8+328(pA0)
   #endif
   #if KB > 41
		lfd	ra2, KB2*8+328(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 41
		lfd	ra3, KB3*8+328(pA0)
   #endif
   #if KB > 41
		lfd	rb1, KB*8+328(pB0)
   #endif
   #if KB > 41
		lfd	rb2, KB2*8+328(pB0)
   #endif
   #if KB > 41
		lfd	rb3, KB3*8+328(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=41 block */
#if KB > 41
   #if KB > 42
		lfd	rB0, 336(pB0)
   #endif
   #if KB > 42
		lfd	rA0, 336(pA0)
   #endif
   #if KB > 42
		lfd	rA1, KB*8+336(pA0)
   #endif
   #if KB > 42
		lfd	rA2, KB2*8+336(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 42
		lfd	rA3, KB3*8+336(pA0)
   #endif
   #if KB > 42
		lfd	rB1, KB*8+336(pB0)
   #endif
   #if KB > 42
		lfd	rB2, KB2*8+336(pB0)
   #endif
   #if KB > 42
		lfd	rB3, KB3*8+336(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=42 block */
#if KB > 42
   #if KB > 43
		lfd	rb0, 344(pB0)
   #endif
   #if KB > 43
		lfd	ra0, 344(pA0)
   #endif
   #if KB > 43
		lfd	ra1, KB*8+344(pA0)
   #endif
   #if KB > 43
		lfd	ra2, KB2*8+344(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 43
		lfd	ra3, KB3*8+344(pA0)
   #endif
   #if KB > 43
		lfd	rb1, KB*8+344(pB0)
   #endif
   #if KB > 43
		lfd	rb2, KB2*8+344(pB0)
   #endif
   #if KB > 43
		lfd	rb3, KB3*8+344(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=43 block */
#if KB > 43
   #if KB > 44
		lfd	rB0, 352(pB0)
   #endif
   #if KB > 44
		lfd	rA0, 352(pA0)
   #endif
   #if KB > 44
		lfd	rA1, KB*8+352(pA0)
   #endif
   #if KB > 44
		lfd	rA2, KB2*8+352(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 44
		lfd	rA3, KB3*8+352(pA0)
   #endif
   #if KB > 44
		lfd	rB1, KB*8+352(pB0)
   #endif
   #if KB > 44
		lfd	rB2, KB2*8+352(pB0)
   #endif
   #if KB > 44
		lfd	rB3, KB3*8+352(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=44 block */
#if KB > 44
   #if KB > 45
		lfd	rb0, 360(pB0)
   #endif
   #if KB > 45
		lfd	ra0, 360(pA0)
   #endif
   #if KB > 45
		lfd	ra1, KB*8+360(pA0)
   #endif
   #if KB > 45
		lfd	ra2, KB2*8+360(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 45
		lfd	ra3, KB3*8+360(pA0)
   #endif
   #if KB > 45
		lfd	rb1, KB*8+360(pB0)
   #endif
   #if KB > 45
		lfd	rb2, KB2*8+360(pB0)
   #endif
   #if KB > 45
		lfd	rb3, KB3*8+360(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=45 block */
#if KB > 45
   #if KB > 46
		lfd	rB0, 368(pB0)
   #endif
   #if KB > 46
		lfd	rA0, 368(pA0)
   #endif
   #if KB > 46
		lfd	rA1, KB*8+368(pA0)
   #endif
   #if KB > 46
		lfd	rA2, KB2*8+368(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 46
		lfd	rA3, KB3*8+368(pA0)
   #endif
   #if KB > 46
		lfd	rB1, KB*8+368(pB0)
   #endif
   #if KB > 46
		lfd	rB2, KB2*8+368(pB0)
   #endif
   #if KB > 46
		lfd	rB3, KB3*8+368(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=46 block */
#if KB > 46
   #if KB > 47
		lfd	rb0, 376(pB0)
   #endif
   #if KB > 47
		lfd	ra0, 376(pA0)
   #endif
   #if KB > 47
		lfd	ra1, KB*8+376(pA0)
   #endif
   #if KB > 47
		lfd	ra2, KB2*8+376(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 47
		lfd	ra3, KB3*8+376(pA0)
   #endif
   #if KB > 47
		lfd	rb1, KB*8+376(pB0)
   #endif
   #if KB > 47
		lfd	rb2, KB2*8+376(pB0)
   #endif
   #if KB > 47
		lfd	rb3, KB3*8+376(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=47 block */
#if KB > 47
   #if KB > 48
		lfd	rB0, 384(pB0)
   #endif
   #if KB > 48
		lfd	rA0, 384(pA0)
   #endif
   #if KB > 48
		lfd	rA1, KB*8+384(pA0)
   #endif
   #if KB > 48
		lfd	rA2, KB2*8+384(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 48
		lfd	rA3, KB3*8+384(pA0)
   #endif
   #if KB > 48
		lfd	rB1, KB*8+384(pB0)
   #endif
   #if KB > 48
		lfd	rB2, KB2*8+384(pB0)
   #endif
   #if KB > 48
		lfd	rB3, KB3*8+384(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=48 block */
#if KB > 48
   #if KB > 49
		lfd	rb0, 392(pB0)
   #endif
   #if KB > 49
		lfd	ra0, 392(pA0)
   #endif
   #if KB > 49
		lfd	ra1, KB*8+392(pA0)
   #endif
   #if KB > 49
		lfd	ra2, KB2*8+392(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 49
		lfd	ra3, KB3*8+392(pA0)
   #endif
   #if KB > 49
		lfd	rb1, KB*8+392(pB0)
   #endif
   #if KB > 49
		lfd	rb2, KB2*8+392(pB0)
   #endif
   #if KB > 49
		lfd	rb3, KB3*8+392(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=49 block */
#if KB > 49
   #if KB > 50
		lfd	rB0, 400(pB0)
   #endif
   #if KB > 50
		lfd	rA0, 400(pA0)
   #endif
   #if KB > 50
		lfd	rA1, KB*8+400(pA0)
   #endif
   #if KB > 50
		lfd	rA2, KB2*8+400(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 50
		lfd	rA3, KB3*8+400(pA0)
   #endif
   #if KB > 50
		lfd	rB1, KB*8+400(pB0)
   #endif
   #if KB > 50
		lfd	rB2, KB2*8+400(pB0)
   #endif
   #if KB > 50
		lfd	rB3, KB3*8+400(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=50 block */
#if KB > 50
   #if KB > 51
		lfd	rb0, 408(pB0)
   #endif
   #if KB > 51
		lfd	ra0, 408(pA0)
   #endif
   #if KB > 51
		lfd	ra1, KB*8+408(pA0)
   #endif
   #if KB > 51
		lfd	ra2, KB2*8+408(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 51
		lfd	ra3, KB3*8+408(pA0)
   #endif
   #if KB > 51
		lfd	rb1, KB*8+408(pB0)
   #endif
   #if KB > 51
		lfd	rb2, KB2*8+408(pB0)
   #endif
   #if KB > 51
		lfd	rb3, KB3*8+408(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=51 block */
#if KB > 51
   #if KB > 52
		lfd	rB0, 416(pB0)
   #endif
   #if KB > 52
		lfd	rA0, 416(pA0)
   #endif
   #if KB > 52
		lfd	rA1, KB*8+416(pA0)
   #endif
   #if KB > 52
		lfd	rA2, KB2*8+416(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 52
		lfd	rA3, KB3*8+416(pA0)
   #endif
   #if KB > 52
		lfd	rB1, KB*8+416(pB0)
   #endif
   #if KB > 52
		lfd	rB2, KB2*8+416(pB0)
   #endif
   #if KB > 52
		lfd	rB3, KB3*8+416(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=52 block */
#if KB > 52
   #if KB > 53
		lfd	rb0, 424(pB0)
   #endif
   #if KB > 53
		lfd	ra0, 424(pA0)
   #endif
   #if KB > 53
		lfd	ra1, KB*8+424(pA0)
   #endif
   #if KB > 53
		lfd	ra2, KB2*8+424(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 53
		lfd	ra3, KB3*8+424(pA0)
   #endif
   #if KB > 53
		lfd	rb1, KB*8+424(pB0)
   #endif
   #if KB > 53
		lfd	rb2, KB2*8+424(pB0)
   #endif
   #if KB > 53
		lfd	rb3, KB3*8+424(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=53 block */
#if KB > 53
   #if KB > 54
		lfd	rB0, 432(pB0)
   #endif
   #if KB > 54
		lfd	rA0, 432(pA0)
   #endif
   #if KB > 54
		lfd	rA1, KB*8+432(pA0)
   #endif
   #if KB > 54
		lfd	rA2, KB2*8+432(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 54
		lfd	rA3, KB3*8+432(pA0)
   #endif
   #if KB > 54
		lfd	rB1, KB*8+432(pB0)
   #endif
   #if KB > 54
		lfd	rB2, KB2*8+432(pB0)
   #endif
   #if KB > 54
		lfd	rB3, KB3*8+432(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=54 block */
#if KB > 54
   #if KB > 55
		lfd	rb0, 440(pB0)
   #endif
   #if KB > 55
		lfd	ra0, 440(pA0)
   #endif
   #if KB > 55
		lfd	ra1, KB*8+440(pA0)
   #endif
   #if KB > 55
		lfd	ra2, KB2*8+440(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 55
		lfd	ra3, KB3*8+440(pA0)
   #endif
   #if KB > 55
		lfd	rb1, KB*8+440(pB0)
   #endif
   #if KB > 55
		lfd	rb2, KB2*8+440(pB0)
   #endif
   #if KB > 55
		lfd	rb3, KB3*8+440(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=55 block */
#if KB > 55
   #if KB > 56
		lfd	rB0, 448(pB0)
   #endif
   #if KB > 56
		lfd	rA0, 448(pA0)
   #endif
   #if KB > 56
		lfd	rA1, KB*8+448(pA0)
   #endif
   #if KB > 56
		lfd	rA2, KB2*8+448(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 56
		lfd	rA3, KB3*8+448(pA0)
   #endif
   #if KB > 56
		lfd	rB1, KB*8+448(pB0)
   #endif
   #if KB > 56
		lfd	rB2, KB2*8+448(pB0)
   #endif
   #if KB > 56
		lfd	rB3, KB3*8+448(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=56 block */
#if KB > 56
   #if KB > 57
		lfd	rb0, 456(pB0)
   #endif
   #if KB > 57
		lfd	ra0, 456(pA0)
   #endif
   #if KB > 57
		lfd	ra1, KB*8+456(pA0)
   #endif
   #if KB > 57
		lfd	ra2, KB2*8+456(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 57
		lfd	ra3, KB3*8+456(pA0)
   #endif
   #if KB > 57
		lfd	rb1, KB*8+456(pB0)
   #endif
   #if KB > 57
		lfd	rb2, KB2*8+456(pB0)
   #endif
   #if KB > 57
		lfd	rb3, KB3*8+456(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=57 block */
#if KB > 57
   #if KB > 58
		lfd	rB0, 464(pB0)
   #endif
   #if KB > 58
		lfd	rA0, 464(pA0)
   #endif
   #if KB > 58
		lfd	rA1, KB*8+464(pA0)
   #endif
   #if KB > 58
		lfd	rA2, KB2*8+464(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 58
		lfd	rA3, KB3*8+464(pA0)
   #endif
   #if KB > 58
		lfd	rB1, KB*8+464(pB0)
   #endif
   #if KB > 58
		lfd	rB2, KB2*8+464(pB0)
   #endif
   #if KB > 58
		lfd	rB3, KB3*8+464(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=58 block */
#if KB > 58
   #if KB > 59
		lfd	rb0, 472(pB0)
   #endif
   #if KB > 59
		lfd	ra0, 472(pA0)
   #endif
   #if KB > 59
		lfd	ra1, KB*8+472(pA0)
   #endif
   #if KB > 59
		lfd	ra2, KB2*8+472(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 59
		lfd	ra3, KB3*8+472(pA0)
   #endif
   #if KB > 59
		lfd	rb1, KB*8+472(pB0)
   #endif
   #if KB > 59
		lfd	rb2, KB2*8+472(pB0)
   #endif
   #if KB > 59
		lfd	rb3, KB3*8+472(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=59 block */
#if KB > 59
   #if KB > 60
		lfd	rB0, 480(pB0)
   #endif
   #if KB > 60
		lfd	rA0, 480(pA0)
   #endif
   #if KB > 60
		lfd	rA1, KB*8+480(pA0)
   #endif
   #if KB > 60
		lfd	rA2, KB2*8+480(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 60
		lfd	rA3, KB3*8+480(pA0)
   #endif
   #if KB > 60
		lfd	rB1, KB*8+480(pB0)
   #endif
   #if KB > 60
		lfd	rB2, KB2*8+480(pB0)
   #endif
   #if KB > 60
		lfd	rB3, KB3*8+480(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=60 block */
#if KB > 60
   #if KB > 61
		lfd	rb0, 488(pB0)
   #endif
   #if KB > 61
		lfd	ra0, 488(pA0)
   #endif
   #if KB > 61
		lfd	ra1, KB*8+488(pA0)
   #endif
   #if KB > 61
		lfd	ra2, KB2*8+488(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 61
		lfd	ra3, KB3*8+488(pA0)
   #endif
   #if KB > 61
		lfd	rb1, KB*8+488(pB0)
   #endif
   #if KB > 61
		lfd	rb2, KB2*8+488(pB0)
   #endif
   #if KB > 61
		lfd	rb3, KB3*8+488(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=61 block */
#if KB > 61
   #if KB > 62
		lfd	rB0, 496(pB0)
   #endif
   #if KB > 62
		lfd	rA0, 496(pA0)
   #endif
   #if KB > 62
		lfd	rA1, KB*8+496(pA0)
   #endif
   #if KB > 62
		lfd	rA2, KB2*8+496(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 62
		lfd	rA3, KB3*8+496(pA0)
   #endif
   #if KB > 62
		lfd	rB1, KB*8+496(pB0)
   #endif
   #if KB > 62
		lfd	rB2, KB2*8+496(pB0)
   #endif
   #if KB > 62
		lfd	rB3, KB3*8+496(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=62 block */
#if KB > 62
   #if KB > 63
		lfd	rb0, 504(pB0)
   #endif
   #if KB > 63
		lfd	ra0, 504(pA0)
   #endif
   #if KB > 63
		lfd	ra1, KB*8+504(pA0)
   #endif
   #if KB > 63
		lfd	ra2, KB2*8+504(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 63
		lfd	ra3, KB3*8+504(pA0)
   #endif
   #if KB > 63
		lfd	rb1, KB*8+504(pB0)
   #endif
   #if KB > 63
		lfd	rb2, KB2*8+504(pB0)
   #endif
   #if KB > 63
		lfd	rb3, KB3*8+504(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=63 block */
#if KB > 63
   #if KB > 64
		lfd	rB0, 512(pB0)
   #endif
   #if KB > 64
		lfd	rA0, 512(pA0)
   #endif
   #if KB > 64
		lfd	rA1, KB*8+512(pA0)
   #endif
   #if KB > 64
		lfd	rA2, KB2*8+512(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 64
		lfd	rA3, KB3*8+512(pA0)
   #endif
   #if KB > 64
		lfd	rB1, KB*8+512(pB0)
   #endif
   #if KB > 64
		lfd	rB2, KB2*8+512(pB0)
   #endif
   #if KB > 64
		lfd	rB3, KB3*8+512(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=64 block */
#if KB > 64
   #if KB > 65
		lfd	rb0, 520(pB0)
   #endif
   #if KB > 65
		lfd	ra0, 520(pA0)
   #endif
   #if KB > 65
		lfd	ra1, KB*8+520(pA0)
   #endif
   #if KB > 65
		lfd	ra2, KB2*8+520(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 65
		lfd	ra3, KB3*8+520(pA0)
   #endif
   #if KB > 65
		lfd	rb1, KB*8+520(pB0)
   #endif
   #if KB > 65
		lfd	rb2, KB2*8+520(pB0)
   #endif
   #if KB > 65
		lfd	rb3, KB3*8+520(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=65 block */
#if KB > 65
   #if KB > 66
		lfd	rB0, 528(pB0)
   #endif
   #if KB > 66
		lfd	rA0, 528(pA0)
   #endif
   #if KB > 66
		lfd	rA1, KB*8+528(pA0)
   #endif
   #if KB > 66
		lfd	rA2, KB2*8+528(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 66
		lfd	rA3, KB3*8+528(pA0)
   #endif
   #if KB > 66
		lfd	rB1, KB*8+528(pB0)
   #endif
   #if KB > 66
		lfd	rB2, KB2*8+528(pB0)
   #endif
   #if KB > 66
		lfd	rB3, KB3*8+528(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=66 block */
#if KB > 66
   #if KB > 67
		lfd	rb0, 536(pB0)
   #endif
   #if KB > 67
		lfd	ra0, 536(pA0)
   #endif
   #if KB > 67
		lfd	ra1, KB*8+536(pA0)
   #endif
   #if KB > 67
		lfd	ra2, KB2*8+536(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 67
		lfd	ra3, KB3*8+536(pA0)
   #endif
   #if KB > 67
		lfd	rb1, KB*8+536(pB0)
   #endif
   #if KB > 67
		lfd	rb2, KB2*8+536(pB0)
   #endif
   #if KB > 67
		lfd	rb3, KB3*8+536(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=67 block */
#if KB > 67
   #if KB > 68
		lfd	rB0, 544(pB0)
   #endif
   #if KB > 68
		lfd	rA0, 544(pA0)
   #endif
   #if KB > 68
		lfd	rA1, KB*8+544(pA0)
   #endif
   #if KB > 68
		lfd	rA2, KB2*8+544(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 68
		lfd	rA3, KB3*8+544(pA0)
   #endif
   #if KB > 68
		lfd	rB1, KB*8+544(pB0)
   #endif
   #if KB > 68
		lfd	rB2, KB2*8+544(pB0)
   #endif
   #if KB > 68
		lfd	rB3, KB3*8+544(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=68 block */
#if KB > 68
   #if KB > 69
		lfd	rb0, 552(pB0)
   #endif
   #if KB > 69
		lfd	ra0, 552(pA0)
   #endif
   #if KB > 69
		lfd	ra1, KB*8+552(pA0)
   #endif
   #if KB > 69
		lfd	ra2, KB2*8+552(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 69
		lfd	ra3, KB3*8+552(pA0)
   #endif
   #if KB > 69
		lfd	rb1, KB*8+552(pB0)
   #endif
   #if KB > 69
		lfd	rb2, KB2*8+552(pB0)
   #endif
   #if KB > 69
		lfd	rb3, KB3*8+552(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=69 block */
#if KB > 69
   #if KB > 70
		lfd	rB0, 560(pB0)
   #endif
   #if KB > 70
		lfd	rA0, 560(pA0)
   #endif
   #if KB > 70
		lfd	rA1, KB*8+560(pA0)
   #endif
   #if KB > 70
		lfd	rA2, KB2*8+560(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 70
		lfd	rA3, KB3*8+560(pA0)
   #endif
   #if KB > 70
		lfd	rB1, KB*8+560(pB0)
   #endif
   #if KB > 70
		lfd	rB2, KB2*8+560(pB0)
   #endif
   #if KB > 70
		lfd	rB3, KB3*8+560(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=70 block */
#if KB > 70
   #if KB > 71
		lfd	rb0, 568(pB0)
   #endif
   #if KB > 71
		lfd	ra0, 568(pA0)
   #endif
   #if KB > 71
		lfd	ra1, KB*8+568(pA0)
   #endif
   #if KB > 71
		lfd	ra2, KB2*8+568(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 71
		lfd	ra3, KB3*8+568(pA0)
   #endif
   #if KB > 71
		lfd	rb1, KB*8+568(pB0)
   #endif
   #if KB > 71
		lfd	rb2, KB2*8+568(pB0)
   #endif
   #if KB > 71
		lfd	rb3, KB3*8+568(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=71 block */
#if KB > 71
   #if KB > 72
		lfd	rB0, 576(pB0)
   #endif
   #if KB > 72
		lfd	rA0, 576(pA0)
   #endif
   #if KB > 72
		lfd	rA1, KB*8+576(pA0)
   #endif
   #if KB > 72
		lfd	rA2, KB2*8+576(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 72
		lfd	rA3, KB3*8+576(pA0)
   #endif
   #if KB > 72
		lfd	rB1, KB*8+576(pB0)
   #endif
   #if KB > 72
		lfd	rB2, KB2*8+576(pB0)
   #endif
   #if KB > 72
		lfd	rB3, KB3*8+576(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=72 block */
#if KB > 72
   #if KB > 73
		lfd	rb0, 584(pB0)
   #endif
   #if KB > 73
		lfd	ra0, 584(pA0)
   #endif
   #if KB > 73
		lfd	ra1, KB*8+584(pA0)
   #endif
   #if KB > 73
		lfd	ra2, KB2*8+584(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 73
		lfd	ra3, KB3*8+584(pA0)
   #endif
   #if KB > 73
		lfd	rb1, KB*8+584(pB0)
   #endif
   #if KB > 73
		lfd	rb2, KB2*8+584(pB0)
   #endif
   #if KB > 73
		lfd	rb3, KB3*8+584(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=73 block */
#if KB > 73
   #if KB > 74
		lfd	rB0, 592(pB0)
   #endif
   #if KB > 74
		lfd	rA0, 592(pA0)
   #endif
   #if KB > 74
		lfd	rA1, KB*8+592(pA0)
   #endif
   #if KB > 74
		lfd	rA2, KB2*8+592(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 74
		lfd	rA3, KB3*8+592(pA0)
   #endif
   #if KB > 74
		lfd	rB1, KB*8+592(pB0)
   #endif
   #if KB > 74
		lfd	rB2, KB2*8+592(pB0)
   #endif
   #if KB > 74
		lfd	rB3, KB3*8+592(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=74 block */
#if KB > 74
   #if KB > 75
		lfd	rb0, 600(pB0)
   #endif
   #if KB > 75
		lfd	ra0, 600(pA0)
   #endif
   #if KB > 75
		lfd	ra1, KB*8+600(pA0)
   #endif
   #if KB > 75
		lfd	ra2, KB2*8+600(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 75
		lfd	ra3, KB3*8+600(pA0)
   #endif
   #if KB > 75
		lfd	rb1, KB*8+600(pB0)
   #endif
   #if KB > 75
		lfd	rb2, KB2*8+600(pB0)
   #endif
   #if KB > 75
		lfd	rb3, KB3*8+600(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=75 block */
#if KB > 75
   #if KB > 76
		lfd	rB0, 608(pB0)
   #endif
   #if KB > 76
		lfd	rA0, 608(pA0)
   #endif
   #if KB > 76
		lfd	rA1, KB*8+608(pA0)
   #endif
   #if KB > 76
		lfd	rA2, KB2*8+608(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 76
		lfd	rA3, KB3*8+608(pA0)
   #endif
   #if KB > 76
		lfd	rB1, KB*8+608(pB0)
   #endif
   #if KB > 76
		lfd	rB2, KB2*8+608(pB0)
   #endif
   #if KB > 76
		lfd	rB3, KB3*8+608(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=76 block */
#if KB > 76
   #if KB > 77
		lfd	rb0, 616(pB0)
   #endif
   #if KB > 77
		lfd	ra0, 616(pA0)
   #endif
   #if KB > 77
		lfd	ra1, KB*8+616(pA0)
   #endif
   #if KB > 77
		lfd	ra2, KB2*8+616(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 77
		lfd	ra3, KB3*8+616(pA0)
   #endif
   #if KB > 77
		lfd	rb1, KB*8+616(pB0)
   #endif
   #if KB > 77
		lfd	rb2, KB2*8+616(pB0)
   #endif
   #if KB > 77
		lfd	rb3, KB3*8+616(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=77 block */
#if KB > 77
   #if KB > 78
		lfd	rB0, 624(pB0)
   #endif
   #if KB > 78
		lfd	rA0, 624(pA0)
   #endif
   #if KB > 78
		lfd	rA1, KB*8+624(pA0)
   #endif
   #if KB > 78
		lfd	rA2, KB2*8+624(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 78
		lfd	rA3, KB3*8+624(pA0)
   #endif
   #if KB > 78
		lfd	rB1, KB*8+624(pB0)
   #endif
   #if KB > 78
		lfd	rB2, KB2*8+624(pB0)
   #endif
   #if KB > 78
		lfd	rB3, KB3*8+624(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=78 block */
#if KB > 78
   #if KB > 79
		lfd	rb0, 632(pB0)
   #endif
   #if KB > 79
		lfd	ra0, 632(pA0)
   #endif
   #if KB > 79
		lfd	ra1, KB*8+632(pA0)
   #endif
   #if KB > 79
		lfd	ra2, KB2*8+632(pA0)
   #endif
	fmadd	rC00, rA0, rB0, rC00
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
   #if KB > 79
		lfd	ra3, KB3*8+632(pA0)
   #endif
   #if KB > 79
		lfd	rb1, KB*8+632(pB0)
   #endif
   #if KB > 79
		lfd	rb2, KB2*8+632(pB0)
   #endif
   #if KB > 79
		lfd	rb3, KB3*8+632(pB0)
   #endif
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	fmadd	rC03, rA0, rB3, rC03
	fmadd	rC13, rA1, rB3, rC13
	fmadd	rC23, rA2, rB3, rC23
	fmadd	rC33, rA3, rB3, rC33
#endif  /* end K=79 block */
#if KB > 79
   #if KB > 80
		lfd	rB0, 640(pB0)
   #endif
   #if KB > 80
		lfd	rA0, 640(pA0)
   #endif
   #if KB > 80
		lfd	rA1, KB*8+640(pA0)
   #endif
   #if KB > 80
		lfd	rA2, KB2*8+640(pA0)
   #endif
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
   #if KB > 80
		lfd	rA3, KB3*8+640(pA0)
   #endif
   #if KB > 80
		lfd	rB1, KB*8+640(pB0)
   #endif
   #if KB > 80
		lfd	rB2, KB2*8+640(pB0)
   #endif
   #if KB > 80
		lfd	rB3, KB3*8+640(pB0)
   #endif
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	fmadd	rC03, ra0, rb3, rC03
	fmadd	rC13, ra1, rb3, rC13
	fmadd	rC23, ra2, rb3, rC23
	fmadd	rC33, ra3, rb3, rC33
#endif  /* end K=80 block */
/* End KLOOP */
/*
 *      Store to C, iterate loop
 */

        stfd    rC00, -CMUL(32)(pC0)
        stfd    rC10, CMUL(8-32)(pC0)
        stfd    rC20, CMUL(16-32)(pC0)
        stfd    rC30, CMUL(24-32)(pC0)

        stfd    rC01, -CMUL(32)(pC1)
        stfd    rC11, CMUL(8-32)(pC1)
        stfd    rC21, CMUL(16-32)(pC1)
        stfd    rC31, CMUL(24-32)(pC1)


        stfd    rC02, -CMUL(32)(pC2)
        stfd    rC12, CMUL(8-32)(pC2)
        stfd    rC22, CMUL(16-32)(pC2)
        stfd    rC32, CMUL(24-32)(pC2)

        stfd    rC03, -CMUL(32)(pC3)
        stfd    rC13, CMUL(8-32)(pC3)
        stfd    rC23, CMUL(16-32)(pC3)
        stfd    rC33, CMUL(24-32)(pC3)
/*
 *      Mov ptrs, while(M)
 */
        addi    pA0, pA0, KB4*8         /* pA0 += 4*lda */
	nop
	nop
	nop
/*
 *      Move ptrs, while(N)
 */
        add     pC0, pC0, incCn
        add     pC1, pC1, incCn
        add     pC2, pC2, incCn
        add     pC3, pC3, incCn
        sub     pA0, pA0, incAn         /* pA0 -= M*KB */
        addi    pB0, pB0, KB*4*8        /* pB0 += KB*4 */
        addic.  N, N, -4
        bne+    NLOOP
DONE:
        lfd    f14, NEG(8)(r1)
        lfd    f15, NEG(16)(r1)
        lfd    f16, NEG(24)(r1)
        lfd    f17, NEG(32)(r1)
        lfd    f18, NEG(40)(r1)
        lfd    f19, NEG(48)(r1)
        lfd    f20, NEG(56)(r1)
        lfd    f21, NEG(64)(r1)
        lfd    f22, NEG(72)(r1)
        lfd    f23, NEG(80)(r1)
        lfd    f24, NEG(88)(r1)
        lfd    f25, NEG(96)(r1)
        lfd    f26, NEG(104)(r1)
        lfd    f27, NEG(112)(r1)
        lfd    f28, NEG(120)(r1)
        lfd    f29, NEG(128)(r1)
        lfd    f30, NEG(136)(r1)
        lfd    f31, NEG(144)(r1)
        ld     r14, NEG(152)(r1)
#if defined(ATL_GAS_LINUX_PPC) && !defined(ATL_USE64BITS)
        addi    r1, r1, FSIZE
#endif
        blr
