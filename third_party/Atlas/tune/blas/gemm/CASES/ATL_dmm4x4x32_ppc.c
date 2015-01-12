/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2005 R. Clint Whaley
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
#if !defined(ATL_AS_OSX_PPC) && !defined(ATL_GAS_LINUX_PPC) && \
    !defined(ATL_AS_AIX_PPC)
   #error "This kernel requires OS X, AIX, or Linux PPC assembler!"
#endif

#ifdef BETAX
   #define MulByBeta(elt_, bet_) \
      fmul      elt_, elt_, bet_
#else
   #define MulByBeta(elt_, bet_)
#endif
#ifndef Mjoin
   #define Mjoin(pre, nam) my_join(pre, nam)
   #define my_join(pre, nam) pre ## nam
#endif

#ifdef DCPLX
   #define CMUL(i_) ((i_)*2)
   #define SHF  4
#else
   #define CMUL(i_) i_
   #define SHF  3
#endif

#ifdef ATL_GAS_LINUX_PPC
   #define r0    0
   #define r1    1
   #define r2    2
   #define r3    3
   #define r4    4
   #define r5    5
   #define r6    6
   #define r7    7
   #define r8    8
   #define r9    9
   #define r10  10
   #define r11  11
   #define r12  12
/*   #define r13  13 don't use r13 under linux, as it rules out .so */
   #define r14  14
   #define r15  15
   #define r16  16
   #define r17  17
   #define r18  18
   #define r19  19
   #define r20  20
   #define r21  21
   #define r22  22
   #define r23  23
   #define r24  24
   #define r25  25
   #define r26  26
   #define r27  27
   #define r28  28
   #define r29  29
   #define r30  30
   #define r31  31
   #define f0    0
   #define f1    1
   #define f2    2
   #define f3    3
   #define f4    4
   #define f5    5
   #define f6    6
   #define f7    7
   #define f8    8
   #define f9    9
   #define f10  10
   #define f11  11
   #define f12  12
   #define f13  13
   #define f14  14
   #define f15  15
   #define f16  16
   #define f17  17
   #define f18  18
   #define f19  19
   #define f20  20
   #define f21  21
   #define f22  22
   #define f23  23
   #define f24  24
   #define f25  25
   #define f26  26
   #define f27  27
   #define f28  28
   #define f29  29
   #define f30  30
   #define f31  31
#endif

#ifdef ATL_USE64BITS
   #define slwi         sldi
   #define srwi         srdi
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
   #define BOFF    -160
   #define FSIZE    0
   #define NEG(i_) -i_
#else
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

#ifdef KB0
   #undef KB0
#endif
#define KB0      0
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
	.globl  ATL_USERMM
        .align  3
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

#ifdef ATL_USE64BITS
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

NLOOP:
        addi    pfB, pB0, KB4*8
        mtctr   M
MLOOP:
#ifdef BETA0
        lfd     rC00, BOFF(r1)
        fmr     rC10, rC00
        fmr     rC20, rC00
        fmr     rC30, rC00
        fmr     rC01, rC00
        fmr     rC11, rC00
        fmr     rC21, rC00
        fmr     rC31, rC00
        fmr     rC02, rC00
        fmr     rC12, rC00
        fmr     rC22, rC00
        fmr     rC32, rC00
        fmr     rC03, rC00
        fmr     rC13, rC00
        fmr     rC23, rC00
        fmr     rC33, rC00
#else
   #ifdef BETAX
        lfd     rb3, BOFF(r1)
   #endif
        lfd     rC00, 0(pC0)
        MulByBeta(rC00, rb3)
        lfd     rC10, CMUL(8)(pC0)
        MulByBeta(rC10, rb3)
        lfd     rC20, CMUL(16)(pC0)
        MulByBeta(rC20, rb3)
        lfd     rC30, CMUL(24)(pC0)
        MulByBeta(rC30, rb3)

        lfd     rC01, 0(pC1)
        MulByBeta(rC01, rb3)
        lfd     rC11, CMUL(8)(pC1)
        MulByBeta(rC11, rb3)
        lfd     rC21, CMUL(16)(pC1)
        MulByBeta(rC21, rb3)
        lfd     rC31, CMUL(24)(pC1)
        MulByBeta(rC31, rb3)

        lfd     rC02, 0(pC2)
        MulByBeta(rC02, rb3)
        lfd     rC12, CMUL(8)(pC2)
        MulByBeta(rC12, rb3)
        lfd     rC22, CMUL(16)(pC2)
        MulByBeta(rC22, rb3)
        lfd     rC32, CMUL(24)(pC2)
        MulByBeta(rC32, rb3)

        lfd     rC03, 0(pC3)
        MulByBeta(rC03, rb3)
        lfd     rC13, CMUL(8)(pC3)
        MulByBeta(rC13, rb3)
        lfd     rC23, CMUL(16)(pC3)
        MulByBeta(rC23, rb3)
        lfd     rC33, CMUL(24)(pC3)
        MulByBeta(rC33, rb3)
#endif
/*
 *      Unrolled K loop
 */
        lfd     rA0, 0(pA0)
        lfd     rA1, KB*8(pA0)
        lfd     rA2, KB2*8(pA0)
        lfd     rA3, KB3*8(pA0)
        lfd     rB0, 0(pB0)
        lfd     rB1, KB*8(pB0)
        lfd     rB2, KB2*8(pB0)
        lfd     rB3, KB3*8(pB0)
#if KB > 1
        lfd     ra0, 8(pA0)
        lfd     ra1, 8+KB*8(pA0)
        lfd     ra2, 8+KB2*8(pA0)
        lfd     ra3, 8+KB3*8(pA0)
        lfd     rb0, 8(pB0)
        lfd     rb1, 8+KB*8(pB0)
        lfd     rb2, 8+KB2*8(pB0)
#endif

#if KB > 2
	fmadd	rC00, rA0, rB0, rC00
        lfd     rb3, 8+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
                dcbt    0, pfB, 0
                addi    pfB, pfB, 128
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 16+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 16+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 16+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 16+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 16+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 16+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 16+KB3*8(pA0)
#endif

#if KB > 3
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 16+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 24+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 24+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 24+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 24+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 24+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 24+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 24+KB3*8(pA0)
#endif

#if KB > 4
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 24+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 32+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 32+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 32+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 32+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 32+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 32+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 32+KB3*8(pA0)
#endif

#if KB > 5
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 32+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 40+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 40+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 40+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 40+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 40+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 40+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 40+KB3*8(pA0)
#endif

#if KB > 6
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 40+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 48+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 48+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 48+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 48+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 48+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 48+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 48+KB3*8(pA0)
#endif

#if KB > 7
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 48+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 56+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 56+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 56+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 56+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 56+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 56+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 56+KB3*8(pA0)
#endif

#if KB > 8
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 56+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 64+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 64+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 64+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 64+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 64+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 64+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 64+KB3*8(pA0)
#endif

#if KB > 9
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 64+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 72+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 72+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 72+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 72+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 72+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 72+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 72+KB3*8(pA0)
#endif

#if KB > 10
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 72+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 80+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 80+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 80+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 80+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 80+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 80+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 80+KB3*8(pA0)
#endif

#if KB > 11
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 80+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 88+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 88+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 88+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 88+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 88+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 88+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 88+KB3*8(pA0)
#endif

#if KB > 12
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 88+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 96+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 96+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 96+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 96+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 96+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 96+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 96+KB3*8(pA0)
#endif

#if KB > 13
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 96+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 104+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 104+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 104+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 104+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 104+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 104+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 104+KB3*8(pA0)
#endif

#if KB > 14
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 104+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 112+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 112+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 112+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 112+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 112+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 112+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 112+KB3*8(pA0)
#endif

#if KB > 15
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 112+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 120+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 120+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 120+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 120+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 120+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 120+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 120+KB3*8(pA0)
#endif

#if KB > 16
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 120+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 128+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 128+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 128+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 128+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 128+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 128+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 128+KB3*8(pA0)
#endif

#if KB > 17
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 128+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 136+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 136+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 136+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 136+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 136+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 136+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 136+KB3*8(pA0)
#endif

#if KB > 18
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 136+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 144+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 144+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 144+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 144+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 144+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 144+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 144+KB3*8(pA0)
#endif

#if KB > 19
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 144+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 152+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 152+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 152+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 152+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 152+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 152+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 152+KB3*8(pA0)
#endif

#if KB > 20
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 152+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 160+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 160+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 160+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 160+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 160+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 160+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 160+KB3*8(pA0)
#endif

#if KB > 21
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 160+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 168+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 168+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 168+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 168+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 168+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 168+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 168+KB3*8(pA0)
#endif

#if KB > 22
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 168+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 176+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 176+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 176+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 176+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 176+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 176+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 176+KB3*8(pA0)
#endif

#if KB > 23
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 176+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 184+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 184+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 184+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 184+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 184+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 184+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 184+KB3*8(pA0)
#endif

#if KB > 24
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 184+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 192+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 192+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 192+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 192+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 192+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 192+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 192+KB3*8(pA0)
#endif

#if KB > 25
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 192+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 200+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 200+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 200+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 200+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 200+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 200+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 200+KB3*8(pA0)
#endif

#if KB > 26
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 200+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 208+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 208+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 208+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 208+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 208+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 208+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 208+KB3*8(pA0)
#endif

#if KB > 27
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 208+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 216+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 216+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 216+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 216+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 216+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 216+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 216+KB3*8(pA0)
#endif

#if KB > 28
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 216+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 224+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 224+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 224+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 224+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 224+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 224+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 224+KB3*8(pA0)
#endif

#if KB > 29
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 224+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 232+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 232+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 232+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 232+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 232+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 232+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 232+KB3*8(pA0)
#endif

#if KB > 30
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 232+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 240+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 240+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 240+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 240+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 240+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 240+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 240+KB3*8(pA0)
#endif

#if KB > 31
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 240+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 248+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 248+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 248+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 248+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 248+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 248+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 248+KB3*8(pA0)
#endif

#if KB > 32
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 248+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 256+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 256+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 256+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 256+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 256+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 256+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 256+KB3*8(pA0)
#endif

#if KB > 33
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 256+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 264+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 264+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 264+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 264+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 264+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 264+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 264+KB3*8(pA0)
#endif

#if KB > 34
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 264+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 272+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 272+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 272+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 272+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 272+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 272+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 272+KB3*8(pA0)
#endif

#if KB > 35
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 272+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 280+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 280+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 280+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 280+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 280+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 280+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 280+KB3*8(pA0)
#endif

#if KB > 36
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 280+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 288+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 288+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 288+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 288+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 288+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 288+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 288+KB3*8(pA0)
#endif

#if KB > 37
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 288+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 296+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 296+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 296+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 296+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 296+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 296+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 296+KB3*8(pA0)
#endif

#if KB > 38
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 296+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 304+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 304+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 304+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 304+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 304+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 304+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 304+KB3*8(pA0)
#endif

#if KB > 39
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 304+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 312+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 312+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 312+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 312+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 312+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 312+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 312+KB3*8(pA0)
#endif

#if KB > 40
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 312+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 320+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 320+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 320+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 320+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 320+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 320+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 320+KB3*8(pA0)
#endif

#if KB > 41
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 320+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 328+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 328+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 328+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 328+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 328+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 328+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 328+KB3*8(pA0)
#endif

#if KB > 42
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 328+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 336+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 336+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 336+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 336+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 336+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 336+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 336+KB3*8(pA0)
#endif

#if KB > 43
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 336+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 344+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 344+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 344+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 344+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 344+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 344+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 344+KB3*8(pA0)
#endif

#if KB > 44
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 344+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 352+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 352+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 352+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 352+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 352+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 352+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 352+KB3*8(pA0)
#endif

#if KB > 45
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 352+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 360+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 360+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 360+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 360+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 360+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 360+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 360+KB3*8(pA0)
#endif

#if KB > 46
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 360+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 368+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 368+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 368+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 368+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 368+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 368+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 368+KB3*8(pA0)
#endif

#if KB > 47
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 368+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 376+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 376+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 376+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 376+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 376+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 376+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 376+KB3*8(pA0)
#endif

#if KB > 48
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 376+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 384+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 384+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 384+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 384+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 384+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 384+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 384+KB3*8(pA0)
#endif

#if KB > 49
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 384+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 392+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 392+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 392+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 392+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 392+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 392+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 392+KB3*8(pA0)
#endif

#if KB > 50
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 392+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 400+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 400+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 400+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 400+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 400+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 400+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 400+KB3*8(pA0)
#endif

#if KB > 51
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 400+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 408+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 408+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 408+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 408+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 408+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 408+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 408+KB3*8(pA0)
#endif

#if KB > 52
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 408+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 416+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 416+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 416+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 416+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 416+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 416+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 416+KB3*8(pA0)
#endif

#if KB > 53
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 416+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 424+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 424+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 424+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 424+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 424+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 424+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 424+KB3*8(pA0)
#endif

#if KB > 54
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 424+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 432+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 432+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 432+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 432+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 432+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 432+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 432+KB3*8(pA0)
#endif

#if KB > 55
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 432+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 440+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 440+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 440+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 440+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 440+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 440+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 440+KB3*8(pA0)
#endif

#if KB > 56
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 440+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 448+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 448+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 448+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 448+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 448+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 448+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 448+KB3*8(pA0)
#endif

#if KB > 57
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 448+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 456+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 456+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 456+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 456+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 456+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 456+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 456+KB3*8(pA0)
#endif

#if KB > 58
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 456+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 464+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 464+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 464+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 464+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 464+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 464+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 464+KB3*8(pA0)
#endif

#if KB > 59
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 464+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 472+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 472+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 472+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 472+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 472+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 472+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 472+KB3*8(pA0)
#endif

#if KB > 60
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 472+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 480+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 480+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 480+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 480+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 480+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 480+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 480+KB3*8(pA0)
#endif

#if KB > 61
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 480+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 488+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 488+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 488+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 488+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 488+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 488+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 488+KB3*8(pA0)
#endif

#if KB > 62
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 488+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 496+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 496+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 496+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 496+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 496+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 496+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 496+KB3*8(pA0)
#endif

#if KB > 63
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 496+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 504+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 504+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 504+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 504+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 504+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 504+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 504+KB3*8(pA0)
#endif

#if KB > 64
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 504+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 512+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 512+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 512+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 512+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 512+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 512+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 512+KB3*8(pA0)
#endif

#if KB > 65
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 512+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 520+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 520+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 520+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 520+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 520+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 520+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 520+KB3*8(pA0)
#endif

#if KB > 66
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 520+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 528+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 528+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 528+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 528+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 528+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 528+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 528+KB3*8(pA0)
#endif

#if KB > 67
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 528+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 536+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 536+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 536+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 536+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 536+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 536+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 536+KB3*8(pA0)
#endif

#if KB > 68
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 536+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 544+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 544+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 544+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 544+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 544+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 544+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 544+KB3*8(pA0)
#endif

#if KB > 69
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 544+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 552+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 552+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 552+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 552+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 552+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 552+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 552+KB3*8(pA0)
#endif

#if KB > 70
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 552+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 560+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 560+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 560+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 560+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 560+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 560+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 560+KB3*8(pA0)
#endif

#if KB > 71
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 560+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 568+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 568+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 568+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 568+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 568+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 568+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 568+KB3*8(pA0)
#endif

#if KB > 72
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 568+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 576+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 576+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 576+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 576+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 576+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 576+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 576+KB3*8(pA0)
#endif

#if KB > 73
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 576+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 584+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 584+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 584+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 584+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 584+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 584+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 584+KB3*8(pA0)
#endif

#if KB > 74
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 584+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 592+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 592+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 592+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 592+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 592+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 592+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 592+KB3*8(pA0)
#endif

#if KB > 75
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 592+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 600+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 600+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 600+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 600+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 600+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 600+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 600+KB3*8(pA0)
#endif

#if KB > 76
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 600+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 608+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 608+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 608+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 608+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 608+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 608+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 608+KB3*8(pA0)
#endif

#if KB > 77
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 608+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 616+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 616+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 616+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 616+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 616+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 616+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 616+KB3*8(pA0)
#endif

#if KB > 78
	fmadd	rC00, rA0, rB0, rC00
	lfd	rb3, 616+KB3*8(pB0)
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
	lfd	rB0, 624+KB0*8(pB0)
	fmadd	rC01, rA0, rB1, rC01
	fmadd	rC11, rA1, rB1, rC11
	fmadd	rC21, rA2, rB1, rC21
	fmadd	rC31, rA3, rB1, rC31
	lfd	rB1, 624+KB1*8(pB0)
	fmadd	rC02, rA0, rB2, rC02
	fmadd	rC12, rA1, rB2, rC12
	fmadd	rC22, rA2, rB2, rC22
	fmadd	rC32, rA3, rB2, rC32
	lfd	rB2, 624+KB2*8(pB0)
	fmadd	rC03, rA0, rB3, rC03
	lfd	rA0, 624+KB0*8(pA0)
	fmadd	rC13, rA1, rB3, rC13
	lfd	rA1, 624+KB1*8(pA0)
	fmadd	rC23, rA2, rB3, rC23
	lfd	rA2, 624+KB2*8(pA0)
	fmadd	rC33, rA3, rB3, rC33
	lfd	rA3, 624+KB3*8(pA0)
#endif

#if KB > 79
	fmadd	rC00, ra0, rb0, rC00
	lfd	rB3, 624+KB3*8(pB0)
	fmadd	rC10, ra1, rb0, rC10
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
	lfd	rb0, 632+KB0*8(pB0)
	fmadd	rC01, ra0, rb1, rC01
	fmadd	rC11, ra1, rb1, rC11
	fmadd	rC21, ra2, rb1, rC21
	fmadd	rC31, ra3, rb1, rC31
	lfd	rb1, 632+KB1*8(pB0)
	fmadd	rC02, ra0, rb2, rC02
	fmadd	rC12, ra1, rb2, rC12
	fmadd	rC22, ra2, rb2, rC22
	fmadd	rC32, ra3, rb2, rC32
	lfd	rb2, 632+KB2*8(pB0)
	fmadd	rC03, ra0, rb3, rC03
	lfd	ra0, 632+KB0*8(pA0)
	fmadd	rC13, ra1, rb3, rC13
	lfd	ra1, 632+KB1*8(pA0)
	fmadd	rC23, ra2, rb3, rC23
	lfd	ra2, 632+KB2*8(pA0)
	fmadd	rC33, ra3, rb3, rC33
	lfd	ra3, 632+KB3*8(pA0)
#endif

	fmadd	rC00, rA0, rB0, rC00
#if (KB/2)*2 == KB
	lfd	rb3, (KB-1)*8+KB3*8(pB0)
#else
	lfd	rB3, (KB-1)*8+KB3*8(pB0)
#endif
	fmadd	rC10, rA1, rB0, rC10
	fmadd	rC20, rA2, rB0, rC20
	fmadd	rC30, rA3, rB0, rC30
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

#if KB > 1
	fmadd	rC00, ra0, rb0, rC00
	fmadd	rC10, ra1, rb0, rC10
               dcbt    0, pfA, 0
               addi    pfA, pfA, 128
	fmadd	rC20, ra2, rb0, rC20
	fmadd	rC30, ra3, rb0, rC30
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
#endif

/*
 *      Store to C, iterate loop
 */

        stfd    rC00, 0(pC0)
        stfd    rC10, CMUL(8)(pC0)
        stfd    rC20, CMUL(16)(pC0)
        stfd    rC30, CMUL(24)(pC0)

        stfd    rC01, 0(pC1)
        stfd    rC11, CMUL(8)(pC1)
        stfd    rC21, CMUL(16)(pC1)
        stfd    rC31, CMUL(24)(pC1)

        stfd    rC02, 0(pC2)
        stfd    rC12, CMUL(8)(pC2)
        stfd    rC22, CMUL(16)(pC2)
        stfd    rC32, CMUL(24)(pC2)

        stfd    rC03, 0(pC3)
        stfd    rC13, CMUL(8)(pC3)
        stfd    rC23, CMUL(16)(pC3)
        stfd    rC33, CMUL(24)(pC3)
/*
 *      Mov ptrs, while(M)
 */
        addi    pA0, pA0, KB4*8         /* pA0 += 4*lda */
        addi    pC0, pC0, CMUL(4)*8     /* pC0 += 4 */
        addi    pC1, pC1, CMUL(4)*8
        addi    pC2, pC2, CMUL(4)*8
        addi    pC3, pC3, CMUL(4)*8
        bdnz    MLOOP
/*
 *      Move ptrs, while(N)
 */
        sub     pA0, pA0, incAn         /* pA0 -= M*KB */
        addi    pB0, pB0, KB*4*8        /* pB0 += KB*4 */
        add     pC0, pC0, incCn
        add     pC1, pC1, incCn
        add     pC2, pC2, incCn
        add     pC3, pC3, incCn
        addic.  N, N, -4
        bne     NLOOP
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
