!              Automatically Tuned Linear Algebra Software v3.10.2
!                     (C) Copyright 2002 R. Clint Whaley
!
!  Redistribution and use in source and binary forms, with or without
!  modification, are permitted provided that the following conditions
!  are met:
!    1. Redistributions of source code must retain the above copyright
!       notice, this list of conditions and the following disclaimer.
!    2. Redistributions in binary form must reproduce the above copyright
!       notice, this list of conditions, and the following disclaimer in the
!       documentation and/or other materials provided with the distribution.
!    3. The name of the ATLAS group or the names of its contributers may
!       not be used to endorse or promote products derived from this
!       software without specific written permission.
!
!  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
!  ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
!  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
!  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE ATLAS GROUP OR ITS CONTRIBUTORS
!  BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
!  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
!  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
!  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
!  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
!  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
!  POSSIBILITY OF SUCH DAMAGE.
!

#if !defined(KB)
   #define KB 0
#endif
#if KB != 72
   #error "This kernel requires KB = 72"
#endif
#ifndef ATL_GAS_SPARC
   #error "This kernel requires sparc assembler!"
#endif
#define PFA 168
#define PFB 168
#define PFC 64
#if 1
   #define prefR1(mem) prefetch mem, 0
   #define prefR2(mem) prefetch mem, 0
   #define prefW2(mem) prefetch mem, 2
#else
   #define prefW2(mem)
   #define prefR2(mem)
   #define prefR1(mem)
#endif
#define	M	%i0
#define N	%i1
#define ldab	%i2
#define pA1	%i3
#define pA0	%i4
#define pA2	%i5
#define pA3	%g1
#define pB0	%g2
#define pB1	%g3
#define pB2	%g4
#define pB3	%g5
#define pC0	%g6
#define pC1	%l6
#define pC2	%o0
#define pC3	%o1
#define incCn	%o2
#define Kstart	%o3
#define incBn	%o4
#define incAm	%o5
#define incBm	%l0
#define incAn	%l1
#define pfA	%l2
#define pfB	%l3
#define	II	%l4
#define KK	%l5
!
! fp reg defines
!
#define rA0	%f0
#define ra0	%f1
#define rA1	%f2
#define ra1	%f3
#define rA2	%f4
#define ra2	%f5
#define rA3	%f6
#define ra3	%f7
#define rB0	%f8
#define rb0	%f9
#define rB1	%f10
#define rb1	%f11
#define m0	%f12
#define m1	%f13
#define m2	%f14
#define m3	%f15
#define rC00	%f16
#define rC10	%f17
#define rC20	%f18
#define rC30	%f19
#define rC01	%f20
#define rC11	%f21
#define rC21	%f22
#define rC31	%f23
#define rC02	%f24
#define rC12	%f25
#define rC22	%f26
#define rC32	%f27
#define rC03	%f28
#define rC13	%f29
#define rC23	%f30
#define rC33	%f31
#define FSIZE	64
#ifdef SCPLX
   #define CSH 3
   #define CMUL(arg_) ((arg_)*2)
#else
   #define CSH 2
   #define CMUL(arg_) arg_
#endif
!                         i0,          i1           i2                i3
!void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha,
!                           i4             i5,      [%fp+92]       [%fp+96]
!                const TYPE *A, const int lda, const TYPE *B, const int ldb,
!                      [%fp+100] [%fp+104]     [%fp+108]
!                const TYPE beta, TYPE *C, const int ldc)
#ifdef ATL_USE64BITS
        .register       %g2, #scratch
        .register       %g3, #scratch
        .register       %g6, #scratch
#endif

	.section	".text"
	.align	8
	.global	ATL_USERMM
ATL_USERMM:
	save	%sp, -FSIZE, %sp
	ld	[%fp+92], pB0
	ld	[%fp+104], pC0
	ld	[%fp+108], incCn
	srl	ldab, 1, Kstart
	sub	Kstart, 1, Kstart
	sll	ldab, 2, ldab
	sll	ldab, 2, incBn		! incBn = ldab * 4
	sll	incCn, CSH, incCn	! incCn = ldc * size
!	sll	ldab, 1, incAm
!	add	incAm, ldab, incAm
!	add	incAm, 8, incAm		! incAm = (ldab*3+2)*size
!	mov	8, incBm
!	sub	incBm, ldab, incBm	! incBm = (2-ldab)*size
        sll     ldab, 2, incAm          ! incAm = ldab * 4
        xor     incBm, incBm, incBm
	add	pA0, ldab, pA1
	add	pA1, ldab, pA2
	add	pA2, ldab, pA3
	add	pB0, ldab, pB1
	add	pB1, ldab, pB2
	add	pB2, ldab, pB3
	add	pC0, incCn, pC1
	add	pC1, incCn, pC2
	add	pC2, incCn, pC3
	smul	ldab, M, incAn
	add	pA0, incAn, pfA
	smul	ldab, N, pfB
	add	pfB, pB0, pfB
	sub	%g0, incAn, incAn
	sll	incCn, 2, incCn
	sll	M, CSH, M
	sub	incCn, M, incCn
	srl	M, CSH, M
NLOOP:
	mov	M, II
MLOOP:
!
!	Load C[0..3][0..3] and apply beta if necessary
!       NOTE: later, specialize for case where we can use ldd for $C$
!             if ((ldc/2)*2 == ldc) && ( (pC0 && 0x7) == 0 )
!
#ifdef BETA0
	fzero	rC00
	fzero	rC20
	fzero	rC01
	fzero	rC21
	fzero	rC02
	fzero	rC22
	fzero	rC03
	fzero	rC23
#else
   #ifdef BETAX
   	ld	[%fp+100], ra3
   #endif
   	ld	[pC0], rC00
   	ld	[pC0+CMUL(4)], rC10
   	ld	[pC0+CMUL(8)], rC20
   	ld	[pC0+CMUL(12)], rC30
                                                        prefW2([pC0+PFC])
   	ld	[pC1], rC01
   	ld	[pC1+CMUL(4)], rC11
   	ld	[pC1+CMUL(8)], rC21
   	ld	[pC1+CMUL(12)], rC31
                                                        prefW2([pC1+PFC])
   	ld	[pC2], rC02
   	ld	[pC2+CMUL(4)], rC12
   	ld	[pC2+CMUL(8)], rC22
   	ld	[pC2+CMUL(12)], rC32
                                                        prefW2([pC2+PFC])
   	ld	[pC3], rC03
   	ld	[pC3+CMUL(4)], rC13
   	ld	[pC3+CMUL(8)], rC23
   	ld	[pC3+CMUL(12)], rC33
                                                        prefW2([pC3+PFC])
   #ifdef BETAX
   	fmuls	rC00, ra3, rC00
   	fmuls	rC10, ra3, rC10
   	fmuls	rC20, ra3, rC20
   	fmuls	rC30, ra3, rC30
   	fmuls	rC01, ra3, rC01
   	fmuls	rC11, ra3, rC11
   	fmuls	rC21, ra3, rC21
   	fmuls	rC31, ra3, rC31
   	fmuls	rC02, ra3, rC02
   	fmuls	rC12, ra3, rC12
   	fmuls	rC22, ra3, rC22
   	fmuls	rC32, ra3, rC32
   	fmuls	rC03, ra3, rC03
   	fmuls	rC13, ra3, rC13
   	fmuls	rC23, ra3, rC23
   	fmuls	rC33, ra3, rC33
   #endif
#endif
!
!	Load A & B registers and fill multiply pipe
!
	ldd	[pB0], rB0
	ldd	[pA0], rA0
	ldd	[pA1], rA1
	ldd	[pA2], rA2
	ldd	[pA3], rA3
	ldd	[pB1], rB1
	fmuls	rA0, rB0, m0
	fmuls	rA1, rB0, m1
	fmuls	rA2, rB0, m2
	fmuls	rA3, rB0, m3
KLOOP:
!
!	K=0 iteration
!
	fadds	rC00, m0, rC00
                                                        prefR1([pA0+PFA])
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
                                                        prefR1([pA1+PFA])
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
                                                        prefR1([pA2+PFA])
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
                                                        prefR1([pA3+PFA])
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+0], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+0], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+8], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+8], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+8], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+8], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+8], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+8], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=2 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+8], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+8], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+16], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+16], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+16], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+16], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+16], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+16], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=4 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+16], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+16], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+24], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+24], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+24], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+24], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+24], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+24], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=6 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+24], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+24], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+32], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+32], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+32], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+32], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+32], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+32], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=8 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+32], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+32], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+40], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+40], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+40], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+40], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+40], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+40], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=10 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+40], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+40], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+48], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+48], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+48], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+48], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+48], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+48], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=12 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+48], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+48], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+56], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+56], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+56], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+56], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+56], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+56], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=14 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+56], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+56], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+64], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+64], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+64], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+64], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+64], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+64], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=16 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+64], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+64], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+72], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+72], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+72], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+72], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+72], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+72], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=18 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+72], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+72], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+80], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+80], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+80], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+80], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+80], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+80], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=20 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+80], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+80], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+88], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+88], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+88], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+88], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+88], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+88], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=22 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+88], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+88], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+96], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+96], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+96], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+96], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+96], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+96], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=24 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+96], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+96], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+104], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+104], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+104], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+104], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+104], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+104], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=26 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+104], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+104], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+112], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+112], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+112], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+112], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+112], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+112], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=28 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+112], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+112], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+120], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+120], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+120], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+120], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+120], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+120], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=30 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+120], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+120], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+128], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+128], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+128], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+128], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+128], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+128], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=32 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+128], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+128], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+136], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+136], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+136], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+136], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+136], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+136], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=34 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+136], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+136], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+144], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+144], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+144], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+144], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+144], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+144], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=36 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+144], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+144], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+152], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+152], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+152], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+152], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+152], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+152], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=38 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+152], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+152], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+160], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+160], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+160], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+160], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+160], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+160], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=40 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+160], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+160], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+168], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+168], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+168], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+168], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+168], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+168], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=42 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+168], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+168], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+176], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+176], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+176], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+176], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+176], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+176], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=44 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+176], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+176], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+184], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+184], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+184], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+184], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+184], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+184], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=46 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+184], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+184], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+192], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+192], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+192], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+192], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+192], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+192], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=48 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+192], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+192], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+200], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+200], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+200], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+200], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+200], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+200], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=50 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+200], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+200], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+208], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+208], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+208], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+208], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+208], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+208], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=52 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+208], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+208], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+216], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+216], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+216], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+216], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+216], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+216], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=54 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+216], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+216], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+224], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+224], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+224], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+224], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+224], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+224], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=56 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+224], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+224], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+232], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+232], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+232], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+232], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+232], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+232], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=58 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+232], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+232], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+240], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+240], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+240], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+240], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+240], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+240], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=60 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+240], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+240], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+248], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+248], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+248], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+248], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+248], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+248], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=62 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+248], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+248], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+256], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+256], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+256], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+256], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+256], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+256], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=64 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+256], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+256], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+264], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+264], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+264], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+264], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+264], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+264], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=66 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+264], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+264], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+272], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+272], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+272], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+272], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+272], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+272], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=68 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+272], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+272], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
							ldd	[pB0+280], rB0
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
							ldd	[pA0+280], rA0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
							ldd	[pA1+280], rA1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
							ldd	[pA2+280], rA2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
							ldd	[pA3+280], rA3
	fadds	rC03, m0, rC03
				fmuls	rA0, rB0, m0
							ldd	[pB1+280], rB1
	fadds	rC13, m1, rC13
				fmuls	rA1, rB0, m1
	fadds	rC23, m2, rC23
				fmuls	rA2, rB0, m2
	fadds	rC33, m3, rC33
				fmuls	rA3, rB0, m3
!
!	K=70 iteration
!
	fadds	rC00, m0, rC00
				fmuls	rA0, rB1, m0
	fadds	rC10, m1, rC10
				fmuls	rA1, rB1, m1
	fadds	rC20, m2, rC20
				fmuls	rA2, rB1, m2
	fadds	rC30, m3, rC30
				fmuls	rA3, rB1, m3
	fadds	rC01, m0, rC01
				fmuls	ra0, rb0, m0
	fadds	rC11, m1, rC11
				fmuls	ra1, rb0, m1
	fadds	rC21, m2, rC21
				fmuls	ra2, rb0, m2
	fadds	rC31, m3, rC31
				fmuls	ra3, rb0, m3
							ldd	[pB2+280], rB0
	fadds	rC00, m0, rC00
				fmuls	ra0, rb1, m0
	fadds	rC10, m1, rC10
				fmuls	ra1, rb1, m1
	fadds	rC20, m2, rC20
				fmuls	ra2, rb1, m2
	fadds	rC30, m3, rC30
				fmuls	ra3, rb1, m3
							ldd	[pB3+280], rB1
	fadds	rC01, m0, rC01
				fmuls	rA0, rB0, m0
	fadds	rC11, m1, rC11
				fmuls	rA1, rB0, m1
	fadds	rC21, m2, rC21
				fmuls	rA2, rB0, m2
	fadds	rC31, m3, rC31
				fmuls	rA3, rB0, m3
	fadds	rC02, m0, rC02
				fmuls	rA0, rB1, m0
	fadds	rC12, m1, rC12
				fmuls	rA1, rB1, m1
	fadds	rC22, m2, rC22
				fmuls	rA2, rB1, m2
	fadds	rC32, m3, rC32
				fmuls	rA3, rB1, m3
	fadds	rC03, m0, rC03
				fmuls	ra0, rb0, m0
	fadds	rC13, m1, rC13
				fmuls	ra1, rb0, m1
	fadds	rC23, m2, rC23
				fmuls	ra2, rb0, m2
	fadds	rC33, m3, rC33
				fmuls	ra3, rb0, m3
	fadds	rC02, m0, rC02
				fmuls	ra0, rb1, m0
	fadds	rC12, m1, rC12
				fmuls	ra1, rb1, m1
	fadds	rC22, m2, rC22
				fmuls	ra2, rb1, m2
	fadds	rC32, m3, rC32
				fmuls	ra3, rb1, m3
	fadds	rC03, m0, rC03
                                add     pA0, incAm, pA0
	fadds	rC13, m1, rC13
                                add     pA1, incAm, pA1
	fadds	rC23, m2, rC23
                                add     pA2, incAm, pA2
	fadds	rC33, m3, rC33
                                add     pA3, incAm, pA3
!
!       Write answer back to C
!
	st	rC00, [pC0]
	st	rC10, [pC0+CMUL(4)]
	st	rC20, [pC0+CMUL(8)]
	st	rC30, [pC0+CMUL(12)]
					add	pC0, CMUL(16), pC0
	st	rC01, [pC1]
	st	rC11, [pC1+CMUL(4)]
	st	rC21, [pC1+CMUL(8)]
	st	rC31, [pC1+CMUL(12)]
					add	pC1, CMUL(16), pC1
	st	rC02, [pC2]
	st	rC12, [pC2+CMUL(4)]
	st	rC22, [pC2+CMUL(8)]
	st	rC32, [pC2+CMUL(12)]
					add	pC2, CMUL(16), pC2
	st	rC03, [pC3]
	st	rC13, [pC3+CMUL(4)]
	st	rC23, [pC3+CMUL(8)]
	st	rC33, [pC3+CMUL(12)]
	subcc	II, 4, II
	bnz	MLOOP
					add	pC3, CMUL(16), pC3
	add	pC0, incCn, pC0
	add	pC1, incCn, pC1
	add	pC2, incCn, pC2
	add	pC3, incCn, pC3
	add	pA0, incAn, pA0
	add	pA1, incAn, pA1
	add	pA2, incAn, pA2
	add	pA3, incAn, pA3
	add	pB0, incBn, pB0
	add	pB1, incBn, pB1
	add	pB2, incBn, pB2
!
!       while(N);
!
	subcc	N, 4, N
	bnz	NLOOP
	add	pB3, incBn, pB3
!
!	Epilogue
!
	ret
	restore
