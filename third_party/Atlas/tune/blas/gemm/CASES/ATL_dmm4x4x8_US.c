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
#ifndef ATL_GAS_SPARC
   #error "This kernel requires sparc assembler!"
#endif
#define PFD 168
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
!
! Floating point registers
!
#define rA0     %f0
#define rA1     %f2
#define rA2     %f4
#define rA3     %f6
#define ra0     %f8
#define ra1     %f10
#define ra2     %f12
#define ra3     %f14
#define rB0     %f16
#define rB1     %f18
#define rB2     %f20
#define rB3     %f22
#define m0      %f24
#define m1      %f26
#define m2      %f28
#define m3      %f30
#define rC00    %f32
#define rC10    %f34
#define rC20    %f36
#define rC30    %f38
#define rC01    %f40
#define rC11    %f42
#define rC21    %f44
#define rC31    %f46
#define rC02    %f48
#define rC12    %f50
#define rC22    %f52
#define rC32    %f54
#define rC03    %f56
#define rC13    %f58
#define rC23    %f60
#define rC33    %f62
!
! Integer registers
!
#ifdef ATL_USE64BITS
   #define M       %i0
   #define N       %i1
   #define ldab    %i2
   #define pA1     %i3
   #define pA2     %i5
   #define pA0     %i4
   #define pB0     %l0
   #define pC0     %l1
   #define incAn   %l2
   #define II      %l3
   #define KK      %l4
   #define incCn   %l5
   #define pA3     %l6
   #define pB1     %l7
   #define pB2     %o1
   #define pB3     %o2
   #define pC1     %o3
   #define pC2     %o4
   #define pC3     %o5
   #define pfA     %o7
   #define Kstart  %g1
   #define incAm   %g2
   #define incBm   %g3
   #define incBn   %g4
   #define pfB	   %g5
#else
   #define M       %i0
   #define N       %i1
   #define ldab    %i2
   #define pA1     %i3
   #define pA2     %i4
   #define pA0     %i5
   #define pB0     %l0
   #define pC0     %l1
   #define incAn   %l2
   #define II      %l3
   #define KK      %l4
   #define incCn   %l5
   #define pA3     %l6
   #define pB1     %l7
   #define pB2     %o1
   #define pB3     %o2
   #define pC1     %o3
   #define pC2     %o4
   #define pC3     %o5
   #define pfA     %o7
   #define Kstart  %g1
   #define incAm   %g2
   #define incBm   %g3
   #define incBn   %g4
   #define pfB	%i2  /* aliased with ldab */
#endif

#ifdef DCPLX
   #define CMUL(arg_) ((arg_)*2)
   #define incCm 64
   #define CSH 4
#else
   #define CMUL(arg_) arg_
   #define incCm 32
   #define CSH 3
#endif
!
! Saving registers: g[2,3,4] --> FSIZE = 4*4 + 64 = 80
! 64 bits: g[2,3,4,5] : FSIZE = 4*8+128 = 160
#ifdef ATL_USE64BITS
   #define FSIZE   144
   #define BIAS    2047
   #define BOFF    FSIZE-8
#else
   #define FSIZE   96
   #define BIAS    0
#endif
! 32 bits:
!                         i0,          i1           i2             i3,i4
!void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha,
!                           i5        [%fp+92]      [%fp+96]      [%fp+100]
!                const TYPE *A, const int lda, const TYPE *B, const int ldb,
!                      [%fp+104] [%fp+112]     [%fp+116]
!                const TYPE beta, TYPE *C, const int ldc)
! 64 bits:
!                         i0,          i1           i2                f0
!void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha,
!                           i4             i5      [%fp+176]      [%fp+184]
!                const TYPE *A, const int lda, const TYPE *B, const int ldb,
!                 f16, [%fp+192] [%fp+200]     [%fp+208]
!                const TYPE beta, TYPE *C, const int ldc)
#ifdef ATL_USE64BITS
        .register       %g2, #scratch
        .register       %g3, #scratch
#endif
        .section        ".text"
        .align  8
        .global ATL_USERMM
ATL_USERMM:
        save %sp, -FSIZE, %sp
!
!       Save non-scratch registers, save BETA to local frame if needed
!
#ifdef ATL_USE64BITS
   #ifdef BETAX
        std     %f16, [%sp+BIAS+BOFF]
   #endif
#else
        st      %g2, [%sp+80]
        st      %g3, [%sp+84]
        st      %g4, [%sp+88]
#endif
!
!       Load args and start operations
!
#ifdef ATL_USE64BITS
        ldx     [%fp+BIAS+176], pB0
        ldx     [%fp+BIAS+200], pC0
        ldsw    [%fp+BIAS+212], incCn
#else
        ld      [%fp+96], pB0
        ld      [%fp+112], pC0
        ld      [%fp+116], incCn
#endif
        srl     ldab, 3, Kstart
        sub     Kstart, 1, Kstart
        sll     ldab, 3, ldab
	sll	ldab, 2, incBn      ! incBn = ldab * 4
        sll     incCn, CSH, incCn   ! incCn = ldc*size
        sll     ldab, 1, incAm
        add     incAm, ldab, incAm
        add     incAm, 64, incAm    !   incAm = (ldab*3+8)*size
        mov     64, incBm
        sub     incBm, ldab, incBm  !   incBm = (8-ldab)*size
        add     pA0, ldab, pA1
        add     pA1, ldab, pA2
        add     pA2, ldab, pA3
        add     pB0, ldab, pB1
        add     pB1, ldab, pB2
        add     pB2, ldab, pB3
        add     pC0, incCn, pC1
        add     pC1, incCn, pC2
        add     pC2, incCn, pC3
        smul    ldab, M, incAn
        add     pA0, incAn, pfA
        smul    ldab, N, pfB
	add	pfB, pB0, pfB
        sub     %g0, incAn, incAn
	sll	incCn, 2, incCn
	sll	M, CSH, M
	sub	incCn, M, incCn
	srl	M, CSH, M
NLOOP:
        mov     M, II
MLOOP:
!
!       Load C[0..3][0..3] and apply beta if necessary
!
#ifdef BETA0
!
! NOTE: can zero double fp reg using VIS instruction: fzero %fX
!
							prefW2([pC0+PFC])
        fzero   rC00
        fzero   rC10
        fzero   rC20
        fzero   rC30
							prefW2([pC1+PFC])
        fzero   rC01
        fzero   rC11
        fzero   rC21
        fzero   rC31
							prefW2([pC2+PFC])
        fzero   rC02
        fzero   rC12
        fzero   rC22
        fzero   rC32
							prefW2([pC3+PFC])
        fzero   rC03
        fzero   rC13
        fzero   rC23
        fzero   rC33
#else
   #ifdef BETAX
      #ifdef ATL_USE64BITS
        ldd     [%sp+BIAS+BOFF], ra3
      #else
        ldd     [%fp+104], ra3
      #endif
   #endif
							prefW2([pC0+PFC])
        ldd     [pC0], rC00
        ldd     [pC0+CMUL(8)], rC10
        ldd     [pC0+CMUL(16)], rC20
        ldd     [pC0+CMUL(24)], rC30
							prefW2([pC1+PFC])
        ldd     [pC1], rC01
        ldd     [pC1+CMUL(8)], rC11
        ldd     [pC1+CMUL(16)], rC21
        ldd     [pC1+CMUL(24)], rC31
							prefW2([pC2+PFC])
        ldd     [pC2], rC02
        ldd     [pC2+CMUL(8)], rC12
        ldd     [pC2+CMUL(16)], rC22
        ldd     [pC2+CMUL(24)], rC32
							prefW2([pC3+PFC])
        ldd     [pC3], rC03
        ldd     [pC3+CMUL(8)], rC13
        ldd     [pC3+CMUL(16)], rC23
        ldd     [pC3+CMUL(24)], rC33
   #ifdef BETAX
        fmuld   rC00, ra3, rC00
        fmuld   rC10, ra3, rC10
        fmuld   rC20, ra3, rC20
        fmuld   rC30, ra3, rC30
        fmuld   rC01, ra3, rC01
        fmuld   rC11, ra3, rC11
        fmuld   rC21, ra3, rC21
        fmuld   rC31, ra3, rC31
        fmuld   rC02, ra3, rC02
        fmuld   rC12, ra3, rC12
        fmuld   rC22, ra3, rC22
        fmuld   rC32, ra3, rC32
        fmuld   rC03, ra3, rC03
        fmuld   rC13, ra3, rC13
        fmuld   rC23, ra3, rC23
        fmuld   rC33, ra3, rC33
   #endif
#endif
!
!       Load A & B registers and fill multiply pipeline
!
        ldd     [pB0], rB0
        ldd     [pA0], rA0
        ldd     [pA1], rA1
        ldd     [pA2], rA2
        ldd     [pA3], rA3
        ldd     [pB1], rB1
        ldd     [pB2], rB2
        ldd     [pB3], rB3
        fmuld   rA0, rB0, m0
        ldd     [pA0+8], ra0
        fmuld   rA1, rB0, m1
        ldd     [pA1+8], ra1
        fmuld   rA2, rB0, m2
        ldd     [pA2+8], ra2
        fmuld   rA3, rB0, m3
        ldd     [pB0+8], rB0
        ldd     [pA3+8], ra3
!
!  For K == 1, we never enter the loop at all
!
#if (KB != 8)
   #if KB == 0
        subcc   Kstart, %g0, %g0
        bz      KDRAIN
        nop
   #endif
        mov     Kstart, KK
        .align  4
KLOOP:
!
!	K=0 iteration
!
	faddd	rC00, m0, rC00
							prefR1([pA0+PFD])
				fmuld	rA0, rB1, m0
	faddd	rC10, m1, rC10
							prefR1([pA1+PFD])
				fmuld	rA1, rB1, m1
	faddd	rC20, m2, rC20
							prefR1([pA2+PFD])
				fmuld	rA2, rB1, m2
	faddd	rC30, m3, rC30
				fmuld	rA3, rB1, m3
							ldd	[pB1+8], rB1

	faddd	rC01, m0, rC01
				fmuld	rA0, rB2, m0
	faddd	rC11, m1, rC11
				fmuld	rA1, rB2, m1
	faddd	rC21, m2, rC21
				fmuld	rA2, rB2, m2
	faddd	rC31, m3, rC31
				fmuld	rA3, rB2, m3
							ldd	[pB2+8], rB2

	faddd	rC02, m0, rC02
				fmuld	rA0, rB3, m0
							ldd	[pA0+16], rA0
	faddd	rC12, m1, rC12
				fmuld	rA1, rB3, m1
							ldd	[pA1+16], rA1
	faddd	rC22, m2, rC22
				fmuld	rA2, rB3, m2
							ldd	[pA2+16], rA2
	faddd	rC32, m3, rC32
				fmuld	rA3, rB3, m3
							ldd	[pB3+8], rB3

	faddd	rC03, m0, rC03
				fmuld	ra0, rB0, m0
							ldd	[pA3+16], rA3
	faddd	rC13, m1, rC13
				fmuld	ra1, rB0, m1
	faddd	rC23, m2, rC23
				fmuld	ra2, rB0, m2
	faddd	rC33, m3, rC33
				fmuld	ra3, rB0, m3
							ldd	[pB0+16], rB0
!
!	K=1 iteration
!
	faddd	rC00, m0, rC00
				fmuld	ra0, rB1, m0
	faddd	rC10, m1, rC10
				fmuld	ra1, rB1, m1
	faddd	rC20, m2, rC20
				fmuld	ra2, rB1, m2
	faddd	rC30, m3, rC30
				fmuld	ra3, rB1, m3
							ldd	[pB1+16], rB1

	faddd	rC01, m0, rC01
				fmuld	ra0, rB2, m0
	faddd	rC11, m1, rC11
				fmuld	ra1, rB2, m1
	faddd	rC21, m2, rC21
				fmuld	ra2, rB2, m2
	faddd	rC31, m3, rC31
				fmuld	ra3, rB2, m3
							ldd	[pB2+16], rB2

	faddd	rC02, m0, rC02
				fmuld	ra0, rB3, m0
							ldd	[pA0+24], ra0
	faddd	rC12, m1, rC12
				fmuld	ra1, rB3, m1
							ldd	[pA1+24], ra1
	faddd	rC22, m2, rC22
				fmuld	ra2, rB3, m2
							ldd	[pA2+24], ra2
	faddd	rC32, m3, rC32
				fmuld	ra3, rB3, m3
							ldd	[pB3+16], rB3

	faddd	rC03, m0, rC03
				fmuld	rA0, rB0, m0
							ldd	[pA3+24], ra3
	faddd	rC13, m1, rC13
				fmuld	rA1, rB0, m1
	faddd	rC23, m2, rC23
				fmuld	rA2, rB0, m2
	faddd	rC33, m3, rC33
				fmuld	rA3, rB0, m3
							ldd	[pB0+24], rB0
!
!	K=2 iteration
!
	faddd	rC00, m0, rC00
				fmuld	rA0, rB1, m0
	faddd	rC10, m1, rC10
				fmuld	rA1, rB1, m1
	faddd	rC20, m2, rC20
				fmuld	rA2, rB1, m2
	faddd	rC30, m3, rC30
				fmuld	rA3, rB1, m3
							ldd	[pB1+24], rB1

	faddd	rC01, m0, rC01
				fmuld	rA0, rB2, m0
							prefR1([pA3+PFD])
	faddd	rC11, m1, rC11
				fmuld	rA1, rB2, m1
	faddd	rC21, m2, rC21
				fmuld	rA2, rB2, m2
	faddd	rC31, m3, rC31
				fmuld	rA3, rB2, m3
							ldd	[pB2+24], rB2

	faddd	rC02, m0, rC02
				fmuld	rA0, rB3, m0
							ldd	[pA0+32], rA0
	faddd	rC12, m1, rC12
				fmuld	rA1, rB3, m1
							ldd	[pA1+32], rA1
	faddd	rC22, m2, rC22
				fmuld	rA2, rB3, m2
							ldd	[pA2+32], rA2
	faddd	rC32, m3, rC32
				fmuld	rA3, rB3, m3
							ldd	[pB3+24], rB3

	faddd	rC03, m0, rC03
				fmuld	ra0, rB0, m0
							ldd	[pA3+32], rA3
	faddd	rC13, m1, rC13
				fmuld	ra1, rB0, m1
	faddd	rC23, m2, rC23
				fmuld	ra2, rB0, m2
	faddd	rC33, m3, rC33
				fmuld	ra3, rB0, m3
							ldd	[pB0+32], rB0
!
!	K=3 iteration
!
	faddd	rC00, m0, rC00
				fmuld	ra0, rB1, m0
	faddd	rC10, m1, rC10
				fmuld	ra1, rB1, m1
	faddd	rC20, m2, rC20
				fmuld	ra2, rB1, m2
	faddd	rC30, m3, rC30
				fmuld	ra3, rB1, m3
							ldd	[pB1+32], rB1

	faddd	rC01, m0, rC01
				fmuld	ra0, rB2, m0
	faddd	rC11, m1, rC11
				fmuld	ra1, rB2, m1
	faddd	rC21, m2, rC21
				fmuld	ra2, rB2, m2
	faddd	rC31, m3, rC31
				fmuld	ra3, rB2, m3
							ldd	[pB2+32], rB2

	faddd	rC02, m0, rC02
				fmuld	ra0, rB3, m0
							ldd	[pA0+40], ra0
	faddd	rC12, m1, rC12
				fmuld	ra1, rB3, m1
							ldd	[pA1+40], ra1
	faddd	rC22, m2, rC22
				fmuld	ra2, rB3, m2
							ldd	[pA2+40], ra2
	faddd	rC32, m3, rC32
				fmuld	ra3, rB3, m3
							ldd	[pB3+32], rB3

	faddd	rC03, m0, rC03
				fmuld	rA0, rB0, m0
							ldd	[pA3+40], ra3
	faddd	rC13, m1, rC13
				fmuld	rA1, rB0, m1
	faddd	rC23, m2, rC23
				fmuld	rA2, rB0, m2
	faddd	rC33, m3, rC33
				fmuld	rA3, rB0, m3
							ldd	[pB0+40], rB0
!
!	K=4 iteration
!
	faddd	rC00, m0, rC00
				fmuld	rA0, rB1, m0
	faddd	rC10, m1, rC10
				fmuld	rA1, rB1, m1
	faddd	rC20, m2, rC20
				fmuld	rA2, rB1, m2
	faddd	rC30, m3, rC30
				fmuld	rA3, rB1, m3
							ldd	[pB1+40], rB1

	faddd	rC01, m0, rC01
				fmuld	rA0, rB2, m0
	faddd	rC11, m1, rC11
				fmuld	rA1, rB2, m1
	faddd	rC21, m2, rC21
				fmuld	rA2, rB2, m2
	faddd	rC31, m3, rC31
				fmuld	rA3, rB2, m3
							ldd	[pB2+40], rB2

	faddd	rC02, m0, rC02
				fmuld	rA0, rB3, m0
							ldd	[pA0+48], rA0
	faddd	rC12, m1, rC12
				fmuld	rA1, rB3, m1
							ldd	[pA1+48], rA1
	faddd	rC22, m2, rC22
				fmuld	rA2, rB3, m2
							ldd	[pA2+48], rA2
	faddd	rC32, m3, rC32
				fmuld	rA3, rB3, m3
							ldd	[pB3+40], rB3

	faddd	rC03, m0, rC03
				fmuld	ra0, rB0, m0
							ldd	[pA3+48], rA3
	faddd	rC13, m1, rC13
				fmuld	ra1, rB0, m1
	faddd	rC23, m2, rC23
				fmuld	ra2, rB0, m2
	faddd	rC33, m3, rC33
				fmuld	ra3, rB0, m3
							ldd	[pB0+48], rB0
!
!	K=5 iteration
!
	faddd	rC00, m0, rC00
				fmuld	ra0, rB1, m0
	faddd	rC10, m1, rC10
				fmuld	ra1, rB1, m1
	faddd	rC20, m2, rC20
				fmuld	ra2, rB1, m2
	faddd	rC30, m3, rC30
				fmuld	ra3, rB1, m3
							ldd	[pB1+48], rB1

	faddd	rC01, m0, rC01
				fmuld	ra0, rB2, m0
	faddd	rC11, m1, rC11
				fmuld	ra1, rB2, m1
	faddd	rC21, m2, rC21
				fmuld	ra2, rB2, m2
	faddd	rC31, m3, rC31
				fmuld	ra3, rB2, m3
							ldd	[pB2+48], rB2

	faddd	rC02, m0, rC02
				fmuld	ra0, rB3, m0
							ldd	[pA0+56], ra0
	faddd	rC12, m1, rC12
				fmuld	ra1, rB3, m1
							ldd	[pA1+56], ra1
	faddd	rC22, m2, rC22
				fmuld	ra2, rB3, m2
							ldd	[pA2+56], ra2
	faddd	rC32, m3, rC32
				fmuld	ra3, rB3, m3
							ldd	[pB3+48], rB3

	faddd	rC03, m0, rC03
				fmuld	rA0, rB0, m0
							ldd	[pA3+56], ra3
	faddd	rC13, m1, rC13
				fmuld	rA1, rB0, m1
	faddd	rC23, m2, rC23
				fmuld	rA2, rB0, m2
	faddd	rC33, m3, rC33
				fmuld	rA3, rB0, m3
							ldd	[pB0+56], rB0
!
!	K=6 iteration
!
	faddd	rC00, m0, rC00
				fmuld	rA0, rB1, m0
	faddd	rC10, m1, rC10
				fmuld	rA1, rB1, m1
	faddd	rC20, m2, rC20
				fmuld	rA2, rB1, m2
	faddd	rC30, m3, rC30
				fmuld	rA3, rB1, m3
							ldd	[pB1+56], rB1

	faddd	rC01, m0, rC01
				fmuld	rA0, rB2, m0
	faddd	rC11, m1, rC11
				fmuld	rA1, rB2, m1
	faddd	rC21, m2, rC21
				fmuld	rA2, rB2, m2
	faddd	rC31, m3, rC31
				fmuld	rA3, rB2, m3
							ldd	[pB2+56], rB2

	faddd	rC02, m0, rC02
				fmuld	rA0, rB3, m0
							ldd	[pA0+64], rA0
	faddd	rC12, m1, rC12
				fmuld	rA1, rB3, m1
							ldd	[pA1+64], rA1
	faddd	rC22, m2, rC22
				fmuld	rA2, rB3, m2
							ldd	[pA2+64], rA2
	faddd	rC32, m3, rC32
				fmuld	rA3, rB3, m3
							ldd	[pB3+56], rB3

	faddd	rC03, m0, rC03
				fmuld	ra0, rB0, m0
							ldd	[pA3+64], rA3
	faddd	rC13, m1, rC13
				fmuld	ra1, rB0, m1
	faddd	rC23, m2, rC23
				fmuld	ra2, rB0, m2
	faddd	rC33, m3, rC33
				fmuld	ra3, rB0, m3
							ldd	[pB0+64], rB0
!
!	K=7 iteration
!
	faddd	rC00, m0, rC00
				fmuld	ra0, rB1, m0
							prefR1([pB0+PFD])
	faddd	rC10, m1, rC10
				fmuld	ra1, rB1, m1
							prefR1([pB1+PFD])
	faddd	rC20, m2, rC20
				fmuld	ra2, rB1, m2
	faddd	rC30, m3, rC30
				fmuld	ra3, rB1, m3
							ldd	[pB1+64], rB1

	faddd	rC01, m0, rC01
				fmuld	ra0, rB2, m0
							add	pB1, 64, pB1
	faddd	rC11, m1, rC11
				fmuld	ra1, rB2, m1
							prefR1([pB2+PFD])
	faddd	rC21, m2, rC21
				fmuld	ra2, rB2, m2
							prefR1([pB3+PFD])
	faddd	rC31, m3, rC31
				fmuld	ra3, rB2, m3
							ldd	[pB2+64], rB2

	faddd	rC02, m0, rC02
				fmuld	ra0, rB3, m0
							ldd	[pA0+72], ra0
							add	pA0, 64, pA0
	faddd	rC12, m1, rC12
				fmuld	ra1, rB3, m1
							ldd	[pA1+72], ra1
							add	pA1, 64, pA1
	faddd	rC22, m2, rC22
				fmuld	ra2, rB3, m2
							ldd	[pA2+72], ra2
							add	pA2, 64, pA2
	faddd	rC32, m3, rC32
				fmuld	ra3, rB3, m3
							ldd	[pB3+64], rB3

	faddd	rC03, m0, rC03
				fmuld	rA0, rB0, m0
							ldd	[pA3+72], ra3
							add	pA3, 64, pA3
	faddd	rC13, m1, rC13
				fmuld	rA1, rB0, m1
							add	pB2, 64, pB2
	faddd	rC23, m2, rC23
				fmuld	rA2, rB0, m2
							add	pB3, 64, pB3
	faddd	rC33, m3, rC33
				fmuld	rA3, rB0, m3
							ldd	[pB0+72], rB0
!
!       while(K);
!
        subcc   KK, 1, KK
        bnz     KLOOP
                                                        add     pB0, 64, pB0
#endif
!
! Drain multiply pipe on last iteration of K-loop
!
#if (KB == 0)
KDRAIN:
#endif
!
!	K=0 iteration
!
							prefR2([pfB])
	faddd	rC00, m0, rC00
				fmuld	rA0, rB1, m0
							prefR2([pfB+64])
	faddd	rC10, m1, rC10
				fmuld	rA1, rB1, m1
	faddd	rC20, m2, rC20
				fmuld	rA2, rB1, m2
	faddd	rC30, m3, rC30
				fmuld	rA3, rB1, m3
							ldd	[pB1+8], rB1

	faddd	rC01, m0, rC01
				fmuld	rA0, rB2, m0
!							prefR2([pfB+128])
	faddd	rC11, m1, rC11
				fmuld	rA1, rB2, m1
	faddd	rC21, m2, rC21
				fmuld	rA2, rB2, m2
	faddd	rC31, m3, rC31
				fmuld	rA3, rB2, m3
							ldd	[pB2+8], rB2

	faddd	rC02, m0, rC02
				fmuld	rA0, rB3, m0
							ldd	[pA0+16], rA0
	faddd	rC12, m1, rC12
				fmuld	rA1, rB3, m1
							ldd	[pA1+16], rA1
	faddd	rC22, m2, rC22
				fmuld	rA2, rB3, m2
							ldd	[pA2+16], rA2
	faddd	rC32, m3, rC32
				fmuld	rA3, rB3, m3
							ldd	[pB3+8], rB3

	faddd	rC03, m0, rC03
				fmuld	ra0, rB0, m0
							ldd	[pA3+16], rA3
	faddd	rC13, m1, rC13
				fmuld	ra1, rB0, m1
	faddd	rC23, m2, rC23
				fmuld	ra2, rB0, m2
	faddd	rC33, m3, rC33
				fmuld	ra3, rB0, m3
							ldd	[pB0+16], rB0
!
!	K=1 iteration
!
	faddd	rC00, m0, rC00
				fmuld	ra0, rB1, m0
	faddd	rC10, m1, rC10
				fmuld	ra1, rB1, m1
	faddd	rC20, m2, rC20
				fmuld	ra2, rB1, m2
	faddd	rC30, m3, rC30
				fmuld	ra3, rB1, m3
							ldd	[pB1+16], rB1

	faddd	rC01, m0, rC01
				fmuld	ra0, rB2, m0
	faddd	rC11, m1, rC11
				fmuld	ra1, rB2, m1
	faddd	rC21, m2, rC21
				fmuld	ra2, rB2, m2
	faddd	rC31, m3, rC31
				fmuld	ra3, rB2, m3
							ldd	[pB2+16], rB2

	faddd	rC02, m0, rC02
				fmuld	ra0, rB3, m0
							ldd	[pA0+24], ra0
	faddd	rC12, m1, rC12
				fmuld	ra1, rB3, m1
							ldd	[pA1+24], ra1
	faddd	rC22, m2, rC22
				fmuld	ra2, rB3, m2
							ldd	[pA2+24], ra2
	faddd	rC32, m3, rC32
				fmuld	ra3, rB3, m3
							ldd	[pB3+16], rB3

	faddd	rC03, m0, rC03
				fmuld	rA0, rB0, m0
							ldd	[pA3+24], ra3
	faddd	rC13, m1, rC13
				fmuld	rA1, rB0, m1
	faddd	rC23, m2, rC23
				fmuld	rA2, rB0, m2
	faddd	rC33, m3, rC33
				fmuld	rA3, rB0, m3
							ldd	[pB0+24], rB0
!
!	K=2 iteration
!
	faddd	rC00, m0, rC00
				fmuld	rA0, rB1, m0
	faddd	rC10, m1, rC10
				fmuld	rA1, rB1, m1
	faddd	rC20, m2, rC20
				fmuld	rA2, rB1, m2
	faddd	rC30, m3, rC30
				fmuld	rA3, rB1, m3
							ldd	[pB1+24], rB1

	faddd	rC01, m0, rC01
				fmuld	rA0, rB2, m0
	faddd	rC11, m1, rC11
				fmuld	rA1, rB2, m1
	faddd	rC21, m2, rC21
				fmuld	rA2, rB2, m2
	faddd	rC31, m3, rC31
				fmuld	rA3, rB2, m3
							ldd	[pB2+24], rB2

	faddd	rC02, m0, rC02
				fmuld	rA0, rB3, m0
							ldd	[pA0+32], rA0
	faddd	rC12, m1, rC12
				fmuld	rA1, rB3, m1
							ldd	[pA1+32], rA1
	faddd	rC22, m2, rC22
				fmuld	rA2, rB3, m2
							ldd	[pA2+32], rA2
	faddd	rC32, m3, rC32
				fmuld	rA3, rB3, m3
							ldd	[pB3+24], rB3

	faddd	rC03, m0, rC03
				fmuld	ra0, rB0, m0
							ldd	[pA3+32], rA3
	faddd	rC13, m1, rC13
				fmuld	ra1, rB0, m1
	faddd	rC23, m2, rC23
				fmuld	ra2, rB0, m2
	faddd	rC33, m3, rC33
				fmuld	ra3, rB0, m3
							ldd	[pB0+32], rB0
!
!	K=3 iteration
!
	faddd	rC00, m0, rC00
				fmuld	ra0, rB1, m0
	faddd	rC10, m1, rC10
				fmuld	ra1, rB1, m1
	faddd	rC20, m2, rC20
				fmuld	ra2, rB1, m2
	faddd	rC30, m3, rC30
				fmuld	ra3, rB1, m3
							ldd	[pB1+32], rB1

	faddd	rC01, m0, rC01
				fmuld	ra0, rB2, m0
	faddd	rC11, m1, rC11
				fmuld	ra1, rB2, m1
	faddd	rC21, m2, rC21
				fmuld	ra2, rB2, m2
	faddd	rC31, m3, rC31
				fmuld	ra3, rB2, m3
							ldd	[pB2+32], rB2

	faddd	rC02, m0, rC02
				fmuld	ra0, rB3, m0
							ldd	[pA0+40], ra0
	faddd	rC12, m1, rC12
				fmuld	ra1, rB3, m1
							ldd	[pA1+40], ra1
	faddd	rC22, m2, rC22
				fmuld	ra2, rB3, m2
							ldd	[pA2+40], ra2
	faddd	rC32, m3, rC32
				fmuld	ra3, rB3, m3
							ldd	[pB3+32], rB3

	faddd	rC03, m0, rC03
				fmuld	rA0, rB0, m0
							ldd	[pA3+40], ra3
	faddd	rC13, m1, rC13
				fmuld	rA1, rB0, m1
	faddd	rC23, m2, rC23
				fmuld	rA2, rB0, m2
	faddd	rC33, m3, rC33
				fmuld	rA3, rB0, m3
							ldd	[pB0+40], rB0
!
!	K=4 iteration
!
	faddd	rC00, m0, rC00
				fmuld	rA0, rB1, m0
	faddd	rC10, m1, rC10
				fmuld	rA1, rB1, m1
	faddd	rC20, m2, rC20
				fmuld	rA2, rB1, m2
	faddd	rC30, m3, rC30
				fmuld	rA3, rB1, m3
							ldd	[pB1+40], rB1

	faddd	rC01, m0, rC01
				fmuld	rA0, rB2, m0
	faddd	rC11, m1, rC11
				fmuld	rA1, rB2, m1
	faddd	rC21, m2, rC21
				fmuld	rA2, rB2, m2
	faddd	rC31, m3, rC31
				fmuld	rA3, rB2, m3
							ldd	[pB2+40], rB2

	faddd	rC02, m0, rC02
				fmuld	rA0, rB3, m0
							ldd	[pA0+48], rA0
	faddd	rC12, m1, rC12
				fmuld	rA1, rB3, m1
							ldd	[pA1+48], rA1
	faddd	rC22, m2, rC22
				fmuld	rA2, rB3, m2
							ldd	[pA2+48], rA2
	faddd	rC32, m3, rC32
				fmuld	rA3, rB3, m3
							ldd	[pB3+40], rB3

	faddd	rC03, m0, rC03
				fmuld	ra0, rB0, m0
							ldd	[pA3+48], rA3
	faddd	rC13, m1, rC13
				fmuld	ra1, rB0, m1
	faddd	rC23, m2, rC23
				fmuld	ra2, rB0, m2
	faddd	rC33, m3, rC33
				fmuld	ra3, rB0, m3
							ldd	[pB0+48], rB0
!
!	K=5 iteration
!
	faddd	rC00, m0, rC00
				fmuld	ra0, rB1, m0
	faddd	rC10, m1, rC10
				fmuld	ra1, rB1, m1
	faddd	rC20, m2, rC20
				fmuld	ra2, rB1, m2
	faddd	rC30, m3, rC30
				fmuld	ra3, rB1, m3
							ldd	[pB1+48], rB1

	faddd	rC01, m0, rC01
				fmuld	ra0, rB2, m0
	faddd	rC11, m1, rC11
				fmuld	ra1, rB2, m1
	faddd	rC21, m2, rC21
				fmuld	ra2, rB2, m2
	faddd	rC31, m3, rC31
				fmuld	ra3, rB2, m3
							ldd	[pB2+48], rB2

	faddd	rC02, m0, rC02
				fmuld	ra0, rB3, m0
							ldd	[pA0+56], ra0
	faddd	rC12, m1, rC12
				fmuld	ra1, rB3, m1
							ldd	[pA1+56], ra1
	faddd	rC22, m2, rC22
				fmuld	ra2, rB3, m2
							ldd	[pA2+56], ra2
	faddd	rC32, m3, rC32
				fmuld	ra3, rB3, m3
							ldd	[pB3+48], rB3

	faddd	rC03, m0, rC03
				fmuld	rA0, rB0, m0
							ldd	[pA3+56], ra3
	faddd	rC13, m1, rC13
				fmuld	rA1, rB0, m1
	faddd	rC23, m2, rC23
				fmuld	rA2, rB0, m2
	faddd	rC33, m3, rC33
				fmuld	rA3, rB0, m3
							ldd	[pB0+56], rB0
!
!       Second to last K iteration
!
        faddd   rC00, m0, rC00
                                fmuld   rA0, rB1, m0
                                                        add     pB0, incBm, pB0
        faddd   rC10, m1, rC10
                                fmuld   rA1, rB1, m1
        faddd   rC20, m2, rC20
                                fmuld   rA2, rB1, m2
        faddd   rC30, m3, rC30
                                fmuld   rA3, rB1, m3
                                                        ldd     [pB1+56], rB1

        faddd   rC01, m0, rC01
                                fmuld   rA0, rB2, m0
        faddd   rC11, m1, rC11
                                fmuld   rA1, rB2, m1
        faddd   rC21, m2, rC21
                                fmuld   rA2, rB2, m2
        faddd   rC31, m3, rC31
                                fmuld   rA3, rB2, m3
                                                        ldd     [pB2+56], rB2

        faddd   rC02, m0, rC02
                                fmuld   rA0, rB3, m0
        faddd   rC12, m1, rC12
                                fmuld   rA1, rB3, m1
        faddd   rC22, m2, rC22
                                fmuld   rA2, rB3, m2
        faddd   rC32, m3, rC32
                                fmuld   rA3, rB3, m3
                                                        ldd     [pB3+56], rB3

        faddd   rC03, m0, rC03
                                fmuld   ra0, rB0, m0
        faddd   rC13, m1, rC13
                                fmuld   ra1, rB0, m1
        faddd   rC23, m2, rC23
                                fmuld   ra2, rB0, m2
        faddd   rC33, m3, rC33
                                fmuld   ra3, rB0, m3
!
!       Last K iteration
!
        faddd   rC00, m0, rC00
                                fmuld   ra0, rB1, m0
        faddd   rC10, m1, rC10
                                fmuld   ra1, rB1, m1
        faddd   rC20, m2, rC20
                                fmuld   ra2, rB1, m2
        faddd   rC30, m3, rC30
                                fmuld   ra3, rB1, m3

        faddd   rC01, m0, rC01
                                fmuld   ra0, rB2, m0
                                                        add     pB1, incBm, pB1
        faddd   rC11, m1, rC11
                                fmuld   ra1, rB2, m1
							prefR2([pfA])
        faddd   rC21, m2, rC21
                                fmuld   ra2, rB2, m2
        faddd   rC31, m3, rC31
							prefR2([pfA+64])
                                fmuld   ra3, rB2, m3

        faddd   rC02, m0, rC02
                                fmuld   ra0, rB3, m0
                                                        add     pB2, incBm, pB2
        faddd   rC12, m1, rC12
                                fmuld   ra1, rB3, m1
                                                        add     pA0, incAm, pA0
        faddd   rC22, m2, rC22
                                fmuld   ra2, rB3, m2
                                                        add     pA1, incAm, pA1
        faddd   rC32, m3, rC32
                                fmuld   ra3, rB3, m3
                                                        add     pA2, incAm, pA2

        faddd   rC03, m0, rC03
                                                        add     pB3, incBm, pB3
        faddd   rC13, m1, rC13
                                                        add     pA3, incAm, pA3
        faddd   rC23, m2, rC23
							add	pfB, 128, pfB
        faddd   rC33, m3, rC33
							add	pfA, 128, pfA
!
!       Write result back to C
!
        std     rC00, [pC0]
        std     rC10, [pC0+CMUL(8)]
        std     rC20, [pC0+CMUL(16)]
        std     rC30, [pC0+CMUL(24)]
                                add     pC0, CMUL(32), pC0
        std     rC01, [pC1]
        std     rC11, [pC1+CMUL(8)]
        std     rC21, [pC1+CMUL(16)]
        std     rC31, [pC1+CMUL(24)]
                                add     pC1, CMUL(32), pC1
        std     rC02, [pC2]
        std     rC12, [pC2+CMUL(8)]
        std     rC22, [pC2+CMUL(16)]
        std     rC32, [pC2+CMUL(24)]
                                add     pC2, CMUL(32), pC2
        std     rC03, [pC3]
        std     rC13, [pC3+CMUL(8)]
        std     rC23, [pC3+CMUL(16)]
        std     rC33, [pC3+CMUL(24)]
!
!       while(II);
!
        subcc   II, 4, II
        bnz     MLOOP
                                add     pC3, incCm, pC3

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
        subcc   N, 4, N
        bnz     NLOOP
	add	pB3, incBn, pB3


!
!       Restore non-scratch registers and return
!
#ifdef ATL_USE64BITS
#else
        ld      [%sp+80], %g2
        ld      [%sp+84], %g3
        ld      [%sp+88], %g4
#endif
        ret
        restore
