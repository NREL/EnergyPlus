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

#ifndef KB
   #error "This kernel requires KB be a compile-time constant!"
#endif
#if KB > 72
   #error "This kernel supports max KB of 72"
#endif
#if (KB/3)*3 != KB
   #error "This kernel requires KB is be a multiple of 3!"
#endif
#define rC00 $f0
#define rC10 $f1
#define rC20 $f2
#define rC30 $f3
#define rC01 $f4
#define rC11 $f5
#define rC21 $f6
#define rC31 $f7
#define rC02 $f8
#define rC12 $f9
#define rC22 $f10
#define rC32 $f11
#define rC03 $f12
#define rC13 $f13
#define rC23 $f14
#define rC33 $f15
#define rA0  $f16
#define rA1  $f17
#define rA2  $f18
#define rA3  $f19
#define rB0  $f20
#define rB1  $f21
#define rB2  $f22
#define rB3  $f23
#define ra0  $f24
#define ra1  $f25
#define ra2  $f26
#define ra3  $f27
#define rz0  $f28
#define rz1  $f29
#define rz2  $f30
#define rz3  $f31

#define M       $4
#define N       $5
#define K0      $6
#define pA0     $8
#define incAm   $9
#define pB0     $10
#define incBn   $11
#define pC0     $7
#define pA1     $12
#define pA2     $13
#define pA3     $14
#define pB1     $15
#define pB2     $16
#define pB3     $17
#define K       $18
#define stAm    $19
#define stBn    $20
#define incAn   $21
#define pfA     $22
#define pfB     $23
#define pC1	$24
#define pC2	$25
#define pC3	$30
#define incCn	$2
#ifdef BETAX
   #define FSIZE 160
#else
   #define FSIZE 152
#endif
#define PFDISTA KB*8*8
#if 0
   #define prefA(mem) pref 4, mem
#else
   #define prefA(mem)
#endif
#if 1
   #define prefB(mem) pref 6, mem
#else
   #define prefB(mem)
#endif
#if 1
   #define prefC(mem) pref 5, mem
#else
   #define prefC(mem)
#endif
#ifdef DCPLX
   #define CMUL(i_) ((i_)+(i_))
#else
   #define CMUL(i_) i_
#endif

/*
 * save : 18,19,20,21,22,23
 * $26, $27 reserved.  $0 = 0, $1 used by assembler
 * I think can use $31 if I save it & restore it. r0 = 0
 * Avail: $2, $3, $12-25, $28, $30, $31
 */
/*
void ATL_USERMM
             $4           $5           $6                $f15,             $8
   (const int M, const int N, const int K, const double alpha, const double *A,
               $9               $10           $11               0(%sp)
    const int lda, const double *B, const int ldb, const double beta,
        8($sp)       16($sp)
    double *C, const int ldc)

*/
.text
.align 3
.globl ATL_USERMM
.ent   ATL_USERMM
ATL_USERMM:
        .frame  $sp,FSIZE,$31
        .set    noreorder
        .set    nomacro
        .set    noat
/*
 *      Adjust stack and save registers
 */
        daddiu  $sp, $sp, -FSIZE
        sd      $16, 0($sp)
        sd      $17, 8($sp)
        sd      $18, 16($sp)
        sd      $19, 24($sp)
        sd      $20, 32($sp)
        sd      $21, 40($sp)
        sd      $22, 64($sp)
        sd      $23, 72($sp)
        sd      $30, 80($sp)
#ifdef ATL_USE64BITS
        sdc1    $f24, 88($sp)
        sdc1    $f25, 96($sp)
        sdc1    $f26, 104($sp)
        sdc1    $f27, 112($sp)
        sdc1    $f28, 120($sp)
        sdc1    $f29, 128($sp)
        sdc1    $f30, 136($sp)
        sdc1    $f31, 144($sp)
#else
        sdc1    $f20, 88($sp)
        sdc1    $f22, 96($sp)
        sdc1    $f24, 104($sp)
        sdc1    $f26, 112($sp)
        sdc1    $f28, 120($sp)
        sdc1    $f30, 128($sp)
#endif
#ifdef BETAX
        ldc1    rA0, FSIZE($sp)           /* get BETA from caller's stack */
        sdc1    rA0, 152($sp)             /* save BETA to my stack */
   #define BETOFF 152
#endif
/*
 *      (ldc,lda, ldb, K0) * sizeof; setup column ptrs
 */
#ifdef ATL_USE64BITS
	ld	pC0, FSIZE+8($sp)
	ld	incCn, FSIZE+16($sp)
#else
        lw      pC0, FSIZE+8($sp)
        lw      incCn, FSIZE+16($sp)
#endif
#ifdef DCPLX
	sll	incCn, incCn, 4		/* incCn = ldc*sizeof */
#else
	sll	incCn, incCn, 3		/* incCn = ldc*sizeof */
#endif
        sll     incAm, incAm, 3         /* incAm = lda*sizeof */
        .set    macro
        dmul    incAn, incAm, M         /* incAn = lda*M */
        sll     incBn, incBn, 3         /* incBn = ldb*sizeof */
        dmul    stBn, incBn, N          /* stBn = ldb*N */
        .set    nomacro
        sll     K0, K0, 3
        daddu   pA1, pA0, incAm
        daddu   pA2, pA1, incAm
        daddu   pA3, pA2, incAm
        daddu   pB1, pB0, incBn
        daddu   pB2, pB1, incBn
        daddu   pB3, pB2, incBn
	daddu	pC1, pC0, incCn
	daddu	pC2, pC1, incCn
	daddu	pC3, pC2, incCn
        sll     incAm, incAm, 2         /* incAm = lda*4 */
        sll     incBn, incBn, 2         /* incBn = ldb*4 */
        daddu   stAm, pA0, incAn        /* stAm = pA0 + lda*M */
        daddu   stBn, pB0, stBn         /* stBn = pB0 + ldb*N */
#ifdef DCPLX
 	sll	pfA, M, 4		/* pfA = M*sizeof */
#else
 	sll	pfA, M, 3		/* pfA = M*sizeof */
#endif
        sll	incCn, incCn, 2		/* incCn = ldc*4 */
	dsubu	incCn, incCn, pfA	/* incCn = ldc*4 - M */
        or     pfA, stAm, $0
        or     pfB, stBn, $0
#if 1
	daddu	pfA, pA0, incAn
	dsubu	pfA, pfA, incAm
	dsubu	pfA, pfA, incAm
	dsubu	pfA, pfA, incAm
	dsubu	pfA, pfA, incAm
	dsubu	pfA, pfA, incAm
	dsubu	pfA, pfA, incAm
	dsubu	pfA, pfA, incAm
#else
	sll	pfA, pfA, 6
#endif
	daddiu	K0, K0, -8
NLOOP:
MLOOP:
        ldc1    rB0, 0(pB0)
        ldc1    rA0, 0(pA0)
        ldc1    rA1, 0(pA1)
        ldc1    rA2, 0(pA2)
        ldc1    rA3, 0(pA3)
#if !defined(BETA0) || KB <= 3
        ldc1    rB1, 0(pB1)
        ldc1    rB2, 0(pB2)
   #ifndef BETAX
        ldc1    rB3, 0(pB3)
   #endif
        ldc1    ra0, 8(pA0)
        ldc1    ra1, 8(pA1)
   #if !defined(BETA1) || KB <= 3
        ldc1    ra2, 8(pA2)
        ldc1    ra3, 8(pA3)
   #endif
#endif
#ifdef BETA1
        ldc1    rC00, 0(pC0)
        ldc1    rC10, CMUL(8)(pC0)
        ldc1    rC20, CMUL(16)(pC0)
        ldc1    rC30, CMUL(24)(pC0)
        ldc1    rC01, 0(pC1)
        ldc1    rC11, CMUL(8)(pC1)
        ldc1    rC21, CMUL(16)(pC1)
        ldc1    rC31, CMUL(24)(pC1)
        ldc1    rC02, 0(pC2)
        ldc1    rC12, CMUL(8)(pC2)
   #if KB <= 3
        ldc1    rC22, CMUL(16)(pC2)
        ldc1    rC32, CMUL(24)(pC2)
        ldc1    rC03, 0(pC3)
        ldc1    rC13, CMUL(8)(pC3)
        ldc1    rC23, CMUL(16)(pC3)
        ldc1    rC33, CMUL(24)(pC3)
   #endif
#elif defined(BETAX)
        ldc1    rB3, BETOFF($sp)           /* load BETA */
        ldc1    rC00, 0(pC0)
        ldc1    rC10, CMUL(8)(pC0)
        ldc1    rC20, CMUL(16)(pC0)
        ldc1    rC30, CMUL(24)(pC0)
        ldc1    rC01, 0(pC1)
		mul.d	rC00, rC00, rB3
        ldc1    rC11, CMUL(8)(pC1)
		mul.d	rC10, rC10, rB3
        ldc1    rC21, CMUL(16)(pC1)
		mul.d	rC20, rC20, rB3
        ldc1    rC31, CMUL(24)(pC1)
		mul.d	rC30, rC30, rB3
        ldc1    rC02, 0(pC2)
		mul.d	rC01, rC01, rB3
        ldc1    rC12, CMUL(8)(pC2)
		mul.d	rC11, rC11, rB3
        ldc1    rC22, CMUL(16)(pC2)
		mul.d	rC21, rC21, rB3
        ldc1    rC32, CMUL(24)(pC2)
		mul.d	rC31, rC31, rB3
        ldc1    rC03, 0(pC3)
		mul.d	rC02, rC02, rB3
        ldc1    rC13, CMUL(8)(pC3)
		mul.d	rC12, rC12, rB3
        ldc1    rC23, CMUL(16)(pC3)
		mul.d	rC22, rC22, rB3
        ldc1    rC33, CMUL(24)(pC3)
		mul.d	rC32, rC32, rB3
   #if KB <= 3
		mul.d	rC03, rC03, rB3
		mul.d	rC13, rC13, rB3
		mul.d	rC23, rC23, rB3
		mul.d	rC33, rC33, rB3
                ldc1    rB3, 0(pB3)
   #endif
#endif
#if KB <= 3 && defined(BETA0)
	dmtc1	$0, rC00
	mov.d	rC10, rC00
	mov.d	rC20, rC00
	mov.d	rC30, rC00
	mov.d	rC01, rC00
	mov.d	rC11, rC00
	mov.d	rC21, rC00
	mov.d	rC31, rC00
	mov.d	rC02, rC00
	mov.d	rC12, rC00
	mov.d	rC22, rC00
	mov.d	rC32, rC00
	mov.d	rC03, rC00
	mov.d	rC13, rC00
	mov.d	rC23, rC00
	mov.d	rC33, rC00
#endif
/*	.align 3 */
/* KLOOP: */
#if KB > 3
#ifdef BETAX
	madd.d	rC00, rC00, rA0, rB0
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 16(pA0)
	madd.d	rC20, rC20, rA2, rB0
		mul.d	rC03, rC03, rB3
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 8(pB0)
		mul.d	rC13, rC13, rB3
	madd.d	rC01, rC01, rA0, rB1
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 16(pA1)
		mul.d	rC23, rC23, rB3
	madd.d	rC21, rC21, rA2, rB1
		mul.d	rC33, rC33, rB3
        				ldc1    rB3, 0(pB3)
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 8(pB1)
	madd.d	rC02, rC02, rA0, rB2
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 16(pA2)
	madd.d	rC22, rC22, rA2, rB2
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 8(pB2)
	madd.d	rC03, rC03, rA0, rB3
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 16(pA3)
	madd.d	rC23, rC23, rA2, rB3
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 8(pB3)
#elif defined(BETA0)
	mul.d 	rC00, rA0, rB0
        				ldc1    rB1, 0(pB1)
	mul.d 	rC10, rA1, rB0
        				ldc1    rB2, 0(pB2)
	mul.d 	rC20, rA2, rB0
        				ldc1    rB3, 0(pB3)
	mul.d 	rC30, rA3, rB0
        				ldc1    ra0, 8(pA0)
	mul.d 	rC01, rA0, rB1
					ldc1	rB0, 8(pB0)
	mul.d 	rC11, rA1, rB1
        				ldc1    ra1, 8(pA1)
	mul.d 	rC21, rA2, rB1
        				ldc1    ra2, 8(pA2)
	mul.d 	rC31, rA3, rB1
        				ldc1    ra3, 8(pA3)
	mul.d 	rC02, rA0, rB2
					ldc1	rB1, 8(pB1)
	mul.d 	rC12, rA1, rB2
					ldc1	rz0, 16(pA0)
	mul.d 	rC22, rA2, rB2
					ldc1	rz1, 16(pA1)
	mul.d 	rC32, rA3, rB2
					ldc1	rz2, 16(pA2)
	mul.d 	rC03, rA0, rB3
					ldc1	rB2, 8(pB2)
	mul.d 	rC13, rA1, rB3
					ldc1	rz3, 16(pA3)
	mul.d 	rC23, rA2, rB3
	mul.d 	rC33, rA3, rB3
					ldc1	rB3, 8(pB3)
#else
	madd.d	rC00, rC00, rA0, rB0
        				ldc1    rC22, CMUL(16)(pC2)
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 16(pA0)
	madd.d	rC20, rC20, rA2, rB0
        				ldc1    rC32, CMUL(24)(pC2)
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 8(pB0)
	madd.d	rC01, rC01, rA0, rB1
        				ldc1    rC03, 0(pC3)
	madd.d	rC11, rC11, rA1, rB1
        				ldc1    ra2, 8(pA2)
	madd.d	rC21, rC21, rA2, rB1
        				ldc1    rC13, CMUL(8)(pC3)
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 8(pB1)
	madd.d	rC02, rC02, rA0, rB2
        				ldc1    rC23, CMUL(16)(pC3)
	madd.d	rC12, rC12, rA1, rB2
        				ldc1    ra3, 8(pA3)
	madd.d	rC22, rC22, rA2, rB2
        				ldc1    rC33, CMUL(24)(pC3)
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 8(pB2)
	madd.d	rC03, rC03, rA0, rB3
					ldc1	rz1, 16(pA1)
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz2, 16(pA2)
	madd.d	rC23, rC23, rA2, rB3
					ldc1	rz3, 16(pA3)
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 8(pB3)
#endif
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 24(pA0)
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 16(pB0)
	madd.d	rC01, rC01, ra0, rB1
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 24(pA1)
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 16(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 24(pA2)
	madd.d	rC22, rC22, ra2, rB2
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 16(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 24(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 16(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 32(pA0)
	madd.d	rC20, rC20, rz2, rB0
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 24(pB0)
	madd.d	rC01, rC01, rz0, rB1
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 32(pA1)
	madd.d	rC21, rC21, rz2, rB1
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 24(pB1)
	madd.d	rC02, rC02, rz0, rB2
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 32(pA2)
	madd.d	rC22, rC22, rz2, rB2
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 24(pB2)
	madd.d	rC03, rC03, rz0, rB3
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 32(pA3)
	madd.d	rC23, rC23, rz2, rB3
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 24(pB3)
#endif
#if KB > 6
	madd.d	rC00, rC00, rA0, rB0
					prefC(CMUL(32)(pC0))
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 40(pA0)
	madd.d	rC20, rC20, rA2, rB0
					prefC(CMUL(32)(pC1))
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 32(pB0)
	madd.d	rC01, rC01, rA0, rB1
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 40(pA1)
	madd.d	rC21, rC21, rA2, rB1
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 32(pB1)
	madd.d	rC02, rC02, rA0, rB2
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 40(pA2)
	madd.d	rC22, rC22, rA2, rB2
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 32(pB2)
	madd.d	rC03, rC03, rA0, rB3
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 40(pA3)
	madd.d	rC23, rC23, rA2, rB3
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 32(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 48(pA0)
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 40(pB0)
	madd.d	rC01, rC01, ra0, rB1
					prefC(CMUL(32)(pC2))
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 48(pA1)
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 40(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 48(pA2)
	madd.d	rC22, rC22, ra2, rB2
					prefC(CMUL(32)(pC3))
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 40(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 48(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 40(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 56(pA0)
	madd.d	rC20, rC20, rz2, rB0
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 48(pB0)
	madd.d	rC01, rC01, rz0, rB1
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 56(pA1)
	madd.d	rC21, rC21, rz2, rB1
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 48(pB1)
	madd.d	rC02, rC02, rz0, rB2
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 56(pA2)
	madd.d	rC22, rC22, rz2, rB2
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 48(pB2)
	madd.d	rC03, rC03, rz0, rB3
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 56(pA3)
	madd.d	rC23, rC23, rz2, rB3
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 48(pB3)
#endif
#if KB > 9
	madd.d	rC00, rC00, rA0, rB0
					prefB(KB*8*4(pB0))
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 64(pA0)
	madd.d	rC20, rC20, rA2, rB0
					prefB(KB*8*4(pB1))
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 56(pB0)
	madd.d	rC01, rC01, rA0, rB1
					prefB(KB*8*4(pB2))
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 64(pA1)
	madd.d	rC21, rC21, rA2, rB1
					prefB(KB*8*4(pB3))
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 56(pB1)
	madd.d	rC02, rC02, rA0, rB2
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 64(pA2)
	madd.d	rC22, rC22, rA2, rB2
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 56(pB2)
	madd.d	rC03, rC03, rA0, rB3
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 64(pA3)
	madd.d	rC23, rC23, rA2, rB3
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 56(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 72(pA0)
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 64(pB0)
	madd.d	rC01, rC01, ra0, rB1
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 72(pA1)
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 64(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 72(pA2)
	madd.d	rC22, rC22, ra2, rB2
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 64(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 72(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 64(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 80(pA0)
	madd.d	rC20, rC20, rz2, rB0
					prefB(32+KB*8*4(pB0))
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 72(pB0)
	madd.d	rC01, rC01, rz0, rB1
					prefB(32+KB*8*4(pB1))
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 80(pA1)
	madd.d	rC21, rC21, rz2, rB1
					prefB(32+KB*8*4(pB2))
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 72(pB1)
	madd.d	rC02, rC02, rz0, rB2
					prefB(32+KB*8*4(pB3))
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 80(pA2)
	madd.d	rC22, rC22, rz2, rB2
					prefB(64+KB*8*4(pB0))
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 72(pB2)
	madd.d	rC03, rC03, rz0, rB3
					prefB(64+KB*8*4(pB1))
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 80(pA3)
	madd.d	rC23, rC23, rz2, rB3
					prefB(64+KB*8*4(pB2))
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 72(pB3)
#endif
#if KB > 12
	madd.d	rC00, rC00, rA0, rB0
					prefB(64+KB*8*4(pB3))
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 88(pA0)
	madd.d	rC20, rC20, rA2, rB0
					prefB(96+KB*8*4(pB0))
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 80(pB0)
	madd.d	rC01, rC01, rA0, rB1
					prefB(96+KB*8*4(pB1))
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 88(pA1)
	madd.d	rC21, rC21, rA2, rB1
					prefB(96+KB*8*4(pB2))
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 80(pB1)
	madd.d	rC02, rC02, rA0, rB2
					prefB(96+KB*8*4(pB3))
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 88(pA2)
	madd.d	rC22, rC22, rA2, rB2
					prefB(128+KB*8*4(pB0))
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 80(pB2)
	madd.d	rC03, rC03, rA0, rB3
					prefB(128+KB*8*4(pB1))
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 88(pA3)
	madd.d	rC23, rC23, rA2, rB3
					prefB(128+KB*8*4(pB2))
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 80(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 96(pA0)
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 88(pB0)
	madd.d	rC01, rC01, ra0, rB1
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 96(pA1)
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 88(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 96(pA2)
	madd.d	rC22, rC22, ra2, rB2
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 88(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 96(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 88(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 104(pA0)
	madd.d	rC20, rC20, rz2, rB0
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 96(pB0)
	madd.d	rC01, rC01, rz0, rB1
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 104(pA1)
	madd.d	rC21, rC21, rz2, rB1
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 96(pB1)
	madd.d	rC02, rC02, rz0, rB2
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 104(pA2)
	madd.d	rC22, rC22, rz2, rB2
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 96(pB2)
	madd.d	rC03, rC03, rz0, rB3
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 104(pA3)
	madd.d	rC23, rC23, rz2, rB3
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 96(pB3)
#endif
#if KB > 15
	madd.d	rC00, rC00, rA0, rB0
					prefB(128+KB*8*4(pB3))
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 112(pA0)
	madd.d	rC20, rC20, rA2, rB0
					prefB(160+KB*8*4(pB0))
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 104(pB0)
	madd.d	rC01, rC01, rA0, rB1
					prefB(160+KB*8*4(pB1))
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 112(pA1)
	madd.d	rC21, rC21, rA2, rB1
					prefB(160+KB*8*4(pB2))
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 104(pB1)
	madd.d	rC02, rC02, rA0, rB2
					prefB(160+KB*8*4(pB2))
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 112(pA2)
	madd.d	rC22, rC22, rA2, rB2
					prefB(192+KB*8*4(pB0))
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 104(pB2)
	madd.d	rC03, rC03, rA0, rB3
					prefB(192+KB*8*4(pB1))
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 112(pA3)
	madd.d	rC23, rC23, rA2, rB3
					prefB(192+KB*8*4(pB2))
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 104(pB3)
	madd.d	rC00, rC00, ra0, rB0
					prefB(192+KB*8*4(pB3))
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 120(pA0)
	madd.d	rC20, rC20, ra2, rB0
					prefB(224+KB*8*4(pB0))
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 112(pB0)
	madd.d	rC01, rC01, ra0, rB1
					prefB(224+KB*8*4(pB1))
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 120(pA1)
	madd.d	rC21, rC21, ra2, rB1
					prefB(224+KB*8*4(pB2))
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 112(pB1)
	madd.d	rC02, rC02, ra0, rB2
					prefB(224+KB*8*4(pB3))
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 120(pA2)
	madd.d	rC22, rC22, ra2, rB2
					prefB(256+KB*8*4(pB0))
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 112(pB2)
	madd.d	rC03, rC03, ra0, rB3
					prefB(256+KB*8*4(pB1))
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 120(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 112(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 128(pA0)
	madd.d	rC20, rC20, rz2, rB0
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 120(pB0)
	madd.d	rC01, rC01, rz0, rB1
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 128(pA1)
	madd.d	rC21, rC21, rz2, rB1
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 120(pB1)
	madd.d	rC02, rC02, rz0, rB2
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 128(pA2)
	madd.d	rC22, rC22, rz2, rB2
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 120(pB2)
	madd.d	rC03, rC03, rz0, rB3
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 128(pA3)
	madd.d	rC23, rC23, rz2, rB3
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 120(pB3)
#endif
#if KB > 18
	madd.d	rC00, rC00, rA0, rB0
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 136(pA0)
	madd.d	rC20, rC20, rA2, rB0
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 128(pB0)
	madd.d	rC01, rC01, rA0, rB1
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 136(pA1)
	madd.d	rC21, rC21, rA2, rB1
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 128(pB1)
	madd.d	rC02, rC02, rA0, rB2
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 136(pA2)
	madd.d	rC22, rC22, rA2, rB2
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 128(pB2)
	madd.d	rC03, rC03, rA0, rB3
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 136(pA3)
	madd.d	rC23, rC23, rA2, rB3
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 128(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 144(pA0)
	madd.d	rC20, rC20, ra2, rB0
					prefB(256+KB*8*4(pB2))
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 136(pB0)
	madd.d	rC01, rC01, ra0, rB1
					prefB(256+KB*8*4(pB3))
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 144(pA1)
	madd.d	rC21, rC21, ra2, rB1
					prefB(288+KB*8*4(pB0))
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 136(pB1)
	madd.d	rC02, rC02, ra0, rB2
					prefB(288+KB*8*4(pB1))
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 144(pA2)
	madd.d	rC22, rC22, ra2, rB2
					prefB(288+KB*8*4(pB2))
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 136(pB2)
	madd.d	rC03, rC03, ra0, rB3
					prefB(288+KB*8*4(pB3))
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 144(pA3)
	madd.d	rC23, rC23, ra2, rB3
					prefB(320+KB*8*4(pB0))
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 136(pB3)
	madd.d	rC00, rC00, rz0, rB0
					prefB(320+KB*8*4(pB1))
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 152(pA0)
	madd.d	rC20, rC20, rz2, rB0
					prefB(320+KB*8*4(pB2))
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 144(pB0)
	madd.d	rC01, rC01, rz0, rB1
					prefB(320+KB*8*4(pB3))
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 152(pA1)
	madd.d	rC21, rC21, rz2, rB1
					prefB(352+KB*8*4(pB0))
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 144(pB1)
	madd.d	rC02, rC02, rz0, rB2
					prefB(352+KB*8*4(pB1))
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 152(pA2)
	madd.d	rC22, rC22, rz2, rB2
					prefB(352+KB*8*4(pB2))
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 144(pB2)
	madd.d	rC03, rC03, rz0, rB3
					prefB(352+KB*8*4(pB3))
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 152(pA3)
	madd.d	rC23, rC23, rz2, rB3
					prefA(0(pfA))
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 144(pB3)
#endif
#if KB > 21
	madd.d	rC00, rC00, rA0, rB0
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 160(pA0)
	madd.d	rC20, rC20, rA2, rB0
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 152(pB0)
	madd.d	rC01, rC01, rA0, rB1
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 160(pA1)
	madd.d	rC21, rC21, rA2, rB1
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 152(pB1)
	madd.d	rC02, rC02, rA0, rB2
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 160(pA2)
	madd.d	rC22, rC22, rA2, rB2
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 152(pB2)
	madd.d	rC03, rC03, rA0, rB3
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 160(pA3)
	madd.d	rC23, rC23, rA2, rB3
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 152(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 168(pA0)
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 160(pB0)
	madd.d	rC01, rC01, ra0, rB1
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 168(pA1)
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 160(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 168(pA2)
	madd.d	rC22, rC22, ra2, rB2
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 160(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 168(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 160(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 176(pA0)
	madd.d	rC20, rC20, rz2, rB0
					prefA(32(pfA))
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 168(pB0)
	madd.d	rC01, rC01, rz0, rB1
					prefA(64(pfA))
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 176(pA1)
	madd.d	rC21, rC21, rz2, rB1
					prefA(92(pfA))
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 168(pB1)
	madd.d	rC02, rC02, rz0, rB2
					prefA(128(pfA))
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 176(pA2)
	madd.d	rC22, rC22, rz2, rB2
					prefA(160(pfA))
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 168(pB2)
	madd.d	rC03, rC03, rz0, rB3
					prefA(160(pfA))
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 176(pA3)
	madd.d	rC23, rC23, rz2, rB3
					prefA(192(pfA))
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 168(pB3)
#endif
#if KB > 24
	madd.d	rC00, rC00, rA0, rB0
					daddiu pfA, pfA, 213
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 184(pA0)
	madd.d	rC20, rC20, rA2, rB0
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 176(pB0)
	madd.d	rC01, rC01, rA0, rB1
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 184(pA1)
	madd.d	rC21, rC21, rA2, rB1
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 176(pB1)
	madd.d	rC02, rC02, rA0, rB2
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 184(pA2)
	madd.d	rC22, rC22, rA2, rB2
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 176(pB2)
	madd.d	rC03, rC03, rA0, rB3
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 184(pA3)
	madd.d	rC23, rC23, rA2, rB3
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 176(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 192(pA0)
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 184(pB0)
	madd.d	rC01, rC01, ra0, rB1
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 192(pA1)
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 184(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 192(pA2)
	madd.d	rC22, rC22, ra2, rB2
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 184(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 192(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 184(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 200(pA0)
	madd.d	rC20, rC20, rz2, rB0
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 192(pB0)
	madd.d	rC01, rC01, rz0, rB1
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 200(pA1)
	madd.d	rC21, rC21, rz2, rB1
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 192(pB1)
	madd.d	rC02, rC02, rz0, rB2
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 200(pA2)
	madd.d	rC22, rC22, rz2, rB2
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 192(pB2)
	madd.d	rC03, rC03, rz0, rB3
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 200(pA3)
	madd.d	rC23, rC23, rz2, rB3
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 192(pB3)
#endif
#if KB > 27
	madd.d	rC00, rC00, rA0, rB0
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 208(pA0)
	madd.d	rC20, rC20, rA2, rB0
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 200(pB0)
	madd.d	rC01, rC01, rA0, rB1
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 208(pA1)
	madd.d	rC21, rC21, rA2, rB1
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 200(pB1)
	madd.d	rC02, rC02, rA0, rB2
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 208(pA2)
	madd.d	rC22, rC22, rA2, rB2
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 200(pB2)
	madd.d	rC03, rC03, rA0, rB3
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 208(pA3)
	madd.d	rC23, rC23, rA2, rB3
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 200(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 216(pA0)
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 208(pB0)
	madd.d	rC01, rC01, ra0, rB1
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 216(pA1)
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 208(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 216(pA2)
	madd.d	rC22, rC22, ra2, rB2
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 208(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 216(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 208(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 224(pA0)
	madd.d	rC20, rC20, rz2, rB0
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 216(pB0)
	madd.d	rC01, rC01, rz0, rB1
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 224(pA1)
	madd.d	rC21, rC21, rz2, rB1
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 216(pB1)
	madd.d	rC02, rC02, rz0, rB2
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 224(pA2)
	madd.d	rC22, rC22, rz2, rB2
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 216(pB2)
	madd.d	rC03, rC03, rz0, rB3
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 224(pA3)
	madd.d	rC23, rC23, rz2, rB3
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 216(pB3)
#endif
#if KB > 30
	madd.d	rC00, rC00, rA0, rB0
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 232(pA0)
	madd.d	rC20, rC20, rA2, rB0
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 224(pB0)
	madd.d	rC01, rC01, rA0, rB1
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 232(pA1)
	madd.d	rC21, rC21, rA2, rB1
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 224(pB1)
	madd.d	rC02, rC02, rA0, rB2
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 232(pA2)
	madd.d	rC22, rC22, rA2, rB2
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 224(pB2)
	madd.d	rC03, rC03, rA0, rB3
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 232(pA3)
	madd.d	rC23, rC23, rA2, rB3
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 224(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 240(pA0)
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 232(pB0)
	madd.d	rC01, rC01, ra0, rB1
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 240(pA1)
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 232(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 240(pA2)
	madd.d	rC22, rC22, ra2, rB2
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 232(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 240(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 232(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 248(pA0)
	madd.d	rC20, rC20, rz2, rB0
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 240(pB0)
	madd.d	rC01, rC01, rz0, rB1
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 248(pA1)
	madd.d	rC21, rC21, rz2, rB1
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 240(pB1)
	madd.d	rC02, rC02, rz0, rB2
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 248(pA2)
	madd.d	rC22, rC22, rz2, rB2
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 240(pB2)
	madd.d	rC03, rC03, rz0, rB3
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 248(pA3)
	madd.d	rC23, rC23, rz2, rB3
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 240(pB3)
#endif
#if KB > 33
	madd.d	rC00, rC00, rA0, rB0
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 256(pA0)
	madd.d	rC20, rC20, rA2, rB0
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 248(pB0)
	madd.d	rC01, rC01, rA0, rB1
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 256(pA1)
	madd.d	rC21, rC21, rA2, rB1
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 248(pB1)
	madd.d	rC02, rC02, rA0, rB2
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 256(pA2)
	madd.d	rC22, rC22, rA2, rB2
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 248(pB2)
	madd.d	rC03, rC03, rA0, rB3
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 256(pA3)
	madd.d	rC23, rC23, rA2, rB3
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 248(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 264(pA0)
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 256(pB0)
	madd.d	rC01, rC01, ra0, rB1
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 264(pA1)
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 256(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 264(pA2)
	madd.d	rC22, rC22, ra2, rB2
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 256(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 264(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 256(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 272(pA0)
	madd.d	rC20, rC20, rz2, rB0
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 264(pB0)
	madd.d	rC01, rC01, rz0, rB1
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 272(pA1)
	madd.d	rC21, rC21, rz2, rB1
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 264(pB1)
	madd.d	rC02, rC02, rz0, rB2
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 272(pA2)
	madd.d	rC22, rC22, rz2, rB2
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 264(pB2)
	madd.d	rC03, rC03, rz0, rB3
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 272(pA3)
	madd.d	rC23, rC23, rz2, rB3
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 264(pB3)
#endif
#if KB > 36
	madd.d	rC00, rC00, rA0, rB0
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 280(pA0)
	madd.d	rC20, rC20, rA2, rB0
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 272(pB0)
	madd.d	rC01, rC01, rA0, rB1
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 280(pA1)
	madd.d	rC21, rC21, rA2, rB1
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 272(pB1)
	madd.d	rC02, rC02, rA0, rB2
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 280(pA2)
	madd.d	rC22, rC22, rA2, rB2
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 272(pB2)
	madd.d	rC03, rC03, rA0, rB3
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 280(pA3)
	madd.d	rC23, rC23, rA2, rB3
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 272(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 288(pA0)
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 280(pB0)
	madd.d	rC01, rC01, ra0, rB1
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 288(pA1)
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 280(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 288(pA2)
	madd.d	rC22, rC22, ra2, rB2
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 280(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 288(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 280(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 296(pA0)
	madd.d	rC20, rC20, rz2, rB0
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 288(pB0)
	madd.d	rC01, rC01, rz0, rB1
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 296(pA1)
	madd.d	rC21, rC21, rz2, rB1
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 288(pB1)
	madd.d	rC02, rC02, rz0, rB2
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 296(pA2)
	madd.d	rC22, rC22, rz2, rB2
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 288(pB2)
	madd.d	rC03, rC03, rz0, rB3
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 296(pA3)
	madd.d	rC23, rC23, rz2, rB3
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 288(pB3)
#endif
#if KB > 39
	madd.d	rC00, rC00, rA0, rB0
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 304(pA0)
	madd.d	rC20, rC20, rA2, rB0
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 296(pB0)
	madd.d	rC01, rC01, rA0, rB1
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 304(pA1)
	madd.d	rC21, rC21, rA2, rB1
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 296(pB1)
	madd.d	rC02, rC02, rA0, rB2
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 304(pA2)
	madd.d	rC22, rC22, rA2, rB2
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 296(pB2)
	madd.d	rC03, rC03, rA0, rB3
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 304(pA3)
	madd.d	rC23, rC23, rA2, rB3
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 296(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 312(pA0)
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 304(pB0)
	madd.d	rC01, rC01, ra0, rB1
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 312(pA1)
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 304(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 312(pA2)
	madd.d	rC22, rC22, ra2, rB2
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 304(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 312(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 304(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 320(pA0)
	madd.d	rC20, rC20, rz2, rB0
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 312(pB0)
	madd.d	rC01, rC01, rz0, rB1
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 320(pA1)
	madd.d	rC21, rC21, rz2, rB1
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 312(pB1)
	madd.d	rC02, rC02, rz0, rB2
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 320(pA2)
	madd.d	rC22, rC22, rz2, rB2
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 312(pB2)
	madd.d	rC03, rC03, rz0, rB3
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 320(pA3)
	madd.d	rC23, rC23, rz2, rB3
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 312(pB3)
#endif
#if KB > 42
	madd.d	rC00, rC00, rA0, rB0
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 328(pA0)
	madd.d	rC20, rC20, rA2, rB0
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 320(pB0)
	madd.d	rC01, rC01, rA0, rB1
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 328(pA1)
	madd.d	rC21, rC21, rA2, rB1
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 320(pB1)
	madd.d	rC02, rC02, rA0, rB2
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 328(pA2)
	madd.d	rC22, rC22, rA2, rB2
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 320(pB2)
	madd.d	rC03, rC03, rA0, rB3
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 328(pA3)
	madd.d	rC23, rC23, rA2, rB3
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 320(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 336(pA0)
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 328(pB0)
	madd.d	rC01, rC01, ra0, rB1
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 336(pA1)
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 328(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 336(pA2)
	madd.d	rC22, rC22, ra2, rB2
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 328(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 336(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 328(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 344(pA0)
	madd.d	rC20, rC20, rz2, rB0
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 336(pB0)
	madd.d	rC01, rC01, rz0, rB1
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 344(pA1)
	madd.d	rC21, rC21, rz2, rB1
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 336(pB1)
	madd.d	rC02, rC02, rz0, rB2
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 344(pA2)
	madd.d	rC22, rC22, rz2, rB2
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 336(pB2)
	madd.d	rC03, rC03, rz0, rB3
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 344(pA3)
	madd.d	rC23, rC23, rz2, rB3
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 336(pB3)
#endif
#if KB > 45
	madd.d	rC00, rC00, rA0, rB0
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 352(pA0)
	madd.d	rC20, rC20, rA2, rB0
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 344(pB0)
	madd.d	rC01, rC01, rA0, rB1
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 352(pA1)
	madd.d	rC21, rC21, rA2, rB1
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 344(pB1)
	madd.d	rC02, rC02, rA0, rB2
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 352(pA2)
	madd.d	rC22, rC22, rA2, rB2
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 344(pB2)
	madd.d	rC03, rC03, rA0, rB3
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 352(pA3)
	madd.d	rC23, rC23, rA2, rB3
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 344(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 360(pA0)
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 352(pB0)
	madd.d	rC01, rC01, ra0, rB1
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 360(pA1)
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 352(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 360(pA2)
	madd.d	rC22, rC22, ra2, rB2
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 352(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 360(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 352(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 368(pA0)
	madd.d	rC20, rC20, rz2, rB0
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 360(pB0)
	madd.d	rC01, rC01, rz0, rB1
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 368(pA1)
	madd.d	rC21, rC21, rz2, rB1
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 360(pB1)
	madd.d	rC02, rC02, rz0, rB2
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 368(pA2)
	madd.d	rC22, rC22, rz2, rB2
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 360(pB2)
	madd.d	rC03, rC03, rz0, rB3
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 368(pA3)
	madd.d	rC23, rC23, rz2, rB3
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 360(pB3)
#endif
#if KB > 48
	madd.d	rC00, rC00, rA0, rB0
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 376(pA0)
	madd.d	rC20, rC20, rA2, rB0
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 368(pB0)
	madd.d	rC01, rC01, rA0, rB1
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 376(pA1)
	madd.d	rC21, rC21, rA2, rB1
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 368(pB1)
	madd.d	rC02, rC02, rA0, rB2
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 376(pA2)
	madd.d	rC22, rC22, rA2, rB2
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 368(pB2)
	madd.d	rC03, rC03, rA0, rB3
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 376(pA3)
	madd.d	rC23, rC23, rA2, rB3
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 368(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 384(pA0)
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 376(pB0)
	madd.d	rC01, rC01, ra0, rB1
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 384(pA1)
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 376(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 384(pA2)
	madd.d	rC22, rC22, ra2, rB2
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 376(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 384(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 376(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 392(pA0)
	madd.d	rC20, rC20, rz2, rB0
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 384(pB0)
	madd.d	rC01, rC01, rz0, rB1
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 392(pA1)
	madd.d	rC21, rC21, rz2, rB1
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 384(pB1)
	madd.d	rC02, rC02, rz0, rB2
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 392(pA2)
	madd.d	rC22, rC22, rz2, rB2
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 384(pB2)
	madd.d	rC03, rC03, rz0, rB3
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 392(pA3)
	madd.d	rC23, rC23, rz2, rB3
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 384(pB3)
#endif
#if KB > 51
	madd.d	rC00, rC00, rA0, rB0
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 400(pA0)
	madd.d	rC20, rC20, rA2, rB0
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 392(pB0)
	madd.d	rC01, rC01, rA0, rB1
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 400(pA1)
	madd.d	rC21, rC21, rA2, rB1
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 392(pB1)
	madd.d	rC02, rC02, rA0, rB2
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 400(pA2)
	madd.d	rC22, rC22, rA2, rB2
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 392(pB2)
	madd.d	rC03, rC03, rA0, rB3
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 400(pA3)
	madd.d	rC23, rC23, rA2, rB3
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 392(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 408(pA0)
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 400(pB0)
	madd.d	rC01, rC01, ra0, rB1
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 408(pA1)
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 400(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 408(pA2)
	madd.d	rC22, rC22, ra2, rB2
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 400(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 408(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 400(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 416(pA0)
	madd.d	rC20, rC20, rz2, rB0
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 408(pB0)
	madd.d	rC01, rC01, rz0, rB1
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 416(pA1)
	madd.d	rC21, rC21, rz2, rB1
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 408(pB1)
	madd.d	rC02, rC02, rz0, rB2
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 416(pA2)
	madd.d	rC22, rC22, rz2, rB2
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 408(pB2)
	madd.d	rC03, rC03, rz0, rB3
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 416(pA3)
	madd.d	rC23, rC23, rz2, rB3
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 408(pB3)
#endif
#if KB > 54
	madd.d	rC00, rC00, rA0, rB0
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 424(pA0)
	madd.d	rC20, rC20, rA2, rB0
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 416(pB0)
	madd.d	rC01, rC01, rA0, rB1
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 424(pA1)
	madd.d	rC21, rC21, rA2, rB1
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 416(pB1)
	madd.d	rC02, rC02, rA0, rB2
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 424(pA2)
	madd.d	rC22, rC22, rA2, rB2
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 416(pB2)
	madd.d	rC03, rC03, rA0, rB3
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 424(pA3)
	madd.d	rC23, rC23, rA2, rB3
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 416(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 432(pA0)
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 424(pB0)
	madd.d	rC01, rC01, ra0, rB1
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 432(pA1)
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 424(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 432(pA2)
	madd.d	rC22, rC22, ra2, rB2
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 424(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 432(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 424(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 440(pA0)
	madd.d	rC20, rC20, rz2, rB0
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 432(pB0)
	madd.d	rC01, rC01, rz0, rB1
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 440(pA1)
	madd.d	rC21, rC21, rz2, rB1
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 432(pB1)
	madd.d	rC02, rC02, rz0, rB2
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 440(pA2)
	madd.d	rC22, rC22, rz2, rB2
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 432(pB2)
	madd.d	rC03, rC03, rz0, rB3
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 440(pA3)
	madd.d	rC23, rC23, rz2, rB3
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 432(pB3)
#endif
#if KB > 57
	madd.d	rC00, rC00, rA0, rB0
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 448(pA0)
	madd.d	rC20, rC20, rA2, rB0
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 440(pB0)
	madd.d	rC01, rC01, rA0, rB1
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 448(pA1)
	madd.d	rC21, rC21, rA2, rB1
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 440(pB1)
	madd.d	rC02, rC02, rA0, rB2
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 448(pA2)
	madd.d	rC22, rC22, rA2, rB2
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 440(pB2)
	madd.d	rC03, rC03, rA0, rB3
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 448(pA3)
	madd.d	rC23, rC23, rA2, rB3
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 440(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 456(pA0)
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 448(pB0)
	madd.d	rC01, rC01, ra0, rB1
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 456(pA1)
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 448(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 456(pA2)
	madd.d	rC22, rC22, ra2, rB2
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 448(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 456(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 448(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 464(pA0)
	madd.d	rC20, rC20, rz2, rB0
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 456(pB0)
	madd.d	rC01, rC01, rz0, rB1
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 464(pA1)
	madd.d	rC21, rC21, rz2, rB1
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 456(pB1)
	madd.d	rC02, rC02, rz0, rB2
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 464(pA2)
	madd.d	rC22, rC22, rz2, rB2
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 456(pB2)
	madd.d	rC03, rC03, rz0, rB3
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 464(pA3)
	madd.d	rC23, rC23, rz2, rB3
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 456(pB3)
#endif
#if KB > 60
	madd.d	rC00, rC00, rA0, rB0
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 472(pA0)
	madd.d	rC20, rC20, rA2, rB0
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 464(pB0)
	madd.d	rC01, rC01, rA0, rB1
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 472(pA1)
	madd.d	rC21, rC21, rA2, rB1
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 464(pB1)
	madd.d	rC02, rC02, rA0, rB2
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 472(pA2)
	madd.d	rC22, rC22, rA2, rB2
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 464(pB2)
	madd.d	rC03, rC03, rA0, rB3
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 472(pA3)
	madd.d	rC23, rC23, rA2, rB3
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 464(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 480(pA0)
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 472(pB0)
	madd.d	rC01, rC01, ra0, rB1
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 480(pA1)
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 472(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 480(pA2)
	madd.d	rC22, rC22, ra2, rB2
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 472(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 480(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 472(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 488(pA0)
	madd.d	rC20, rC20, rz2, rB0
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 480(pB0)
	madd.d	rC01, rC01, rz0, rB1
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 488(pA1)
	madd.d	rC21, rC21, rz2, rB1
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 480(pB1)
	madd.d	rC02, rC02, rz0, rB2
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 488(pA2)
	madd.d	rC22, rC22, rz2, rB2
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 480(pB2)
	madd.d	rC03, rC03, rz0, rB3
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 488(pA3)
	madd.d	rC23, rC23, rz2, rB3
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 480(pB3)
#endif
#if KB > 63
	madd.d	rC00, rC00, rA0, rB0
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 496(pA0)
	madd.d	rC20, rC20, rA2, rB0
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 488(pB0)
	madd.d	rC01, rC01, rA0, rB1
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 496(pA1)
	madd.d	rC21, rC21, rA2, rB1
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 488(pB1)
	madd.d	rC02, rC02, rA0, rB2
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 496(pA2)
	madd.d	rC22, rC22, rA2, rB2
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 488(pB2)
	madd.d	rC03, rC03, rA0, rB3
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 496(pA3)
	madd.d	rC23, rC23, rA2, rB3
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 488(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 504(pA0)
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 496(pB0)
	madd.d	rC01, rC01, ra0, rB1
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 504(pA1)
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 496(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 504(pA2)
	madd.d	rC22, rC22, ra2, rB2
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 496(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 504(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 496(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 512(pA0)
	madd.d	rC20, rC20, rz2, rB0
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 504(pB0)
	madd.d	rC01, rC01, rz0, rB1
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 512(pA1)
	madd.d	rC21, rC21, rz2, rB1
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 504(pB1)
	madd.d	rC02, rC02, rz0, rB2
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 512(pA2)
	madd.d	rC22, rC22, rz2, rB2
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 504(pB2)
	madd.d	rC03, rC03, rz0, rB3
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 512(pA3)
	madd.d	rC23, rC23, rz2, rB3
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 504(pB3)
#endif
#if KB > 66
	madd.d	rC00, rC00, rA0, rB0
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 520(pA0)
	madd.d	rC20, rC20, rA2, rB0
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 512(pB0)
	madd.d	rC01, rC01, rA0, rB1
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 520(pA1)
	madd.d	rC21, rC21, rA2, rB1
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 512(pB1)
	madd.d	rC02, rC02, rA0, rB2
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 520(pA2)
	madd.d	rC22, rC22, rA2, rB2
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 512(pB2)
	madd.d	rC03, rC03, rA0, rB3
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 520(pA3)
	madd.d	rC23, rC23, rA2, rB3
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 512(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 528(pA0)
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 520(pB0)
	madd.d	rC01, rC01, ra0, rB1
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 528(pA1)
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 520(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 528(pA2)
	madd.d	rC22, rC22, ra2, rB2
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 520(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 528(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 520(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 536(pA0)
	madd.d	rC20, rC20, rz2, rB0
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 528(pB0)
	madd.d	rC01, rC01, rz0, rB1
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 536(pA1)
	madd.d	rC21, rC21, rz2, rB1
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 528(pB1)
	madd.d	rC02, rC02, rz0, rB2
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 536(pA2)
	madd.d	rC22, rC22, rz2, rB2
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 528(pB2)
	madd.d	rC03, rC03, rz0, rB3
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 536(pA3)
	madd.d	rC23, rC23, rz2, rB3
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 528(pB3)
#endif
#if KB > 69
	madd.d	rC00, rC00, rA0, rB0
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 544(pA0)
	madd.d	rC20, rC20, rA2, rB0
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 536(pB0)
	madd.d	rC01, rC01, rA0, rB1
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 544(pA1)
	madd.d	rC21, rC21, rA2, rB1
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 536(pB1)
	madd.d	rC02, rC02, rA0, rB2
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 544(pA2)
	madd.d	rC22, rC22, rA2, rB2
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 536(pB2)
	madd.d	rC03, rC03, rA0, rB3
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 544(pA3)
	madd.d	rC23, rC23, rA2, rB3
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 536(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 552(pA0)
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 544(pB0)
	madd.d	rC01, rC01, ra0, rB1
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 552(pA1)
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 544(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 552(pA2)
	madd.d	rC22, rC22, ra2, rB2
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 544(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 552(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 544(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 560(pA0)
	madd.d	rC20, rC20, rz2, rB0
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 552(pB0)
	madd.d	rC01, rC01, rz0, rB1
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 560(pA1)
	madd.d	rC21, rC21, rz2, rB1
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 552(pB1)
	madd.d	rC02, rC02, rz0, rB2
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 560(pA2)
	madd.d	rC22, rC22, rz2, rB2
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 552(pB2)
	madd.d	rC03, rC03, rz0, rB3
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 560(pA3)
	madd.d	rC23, rC23, rz2, rB3
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 552(pB3)
#endif
#if KB > 72
	madd.d	rC00, rC00, rA0, rB0
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, 568(pA0)
	madd.d	rC20, rC20, rA2, rB0
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, 560(pB0)
	madd.d	rC01, rC01, rA0, rB1
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, 568(pA1)
	madd.d	rC21, rC21, rA2, rB1
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, 560(pB1)
	madd.d	rC02, rC02, rA0, rB2
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, 568(pA2)
	madd.d	rC22, rC22, rA2, rB2
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, 560(pB2)
	madd.d	rC03, rC03, rA0, rB3
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, 568(pA3)
	madd.d	rC23, rC23, rA2, rB3
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, 560(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
					ldc1	rA0, 576(pA0)
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, 568(pB0)
	madd.d	rC01, rC01, ra0, rB1
	madd.d	rC11, rC11, ra1, rB1
					ldc1	rA1, 576(pA1)
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, 568(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
					ldc1	rA2, 576(pA2)
	madd.d	rC22, rC22, ra2, rB2
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, 568(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
					ldc1	rA3, 576(pA3)
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, 568(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
					ldc1	ra0, 584(pA0)
	madd.d	rC20, rC20, rz2, rB0
	madd.d	rC30, rC30, rz3, rB0
					ldc1	rB0, 576(pB0)
	madd.d	rC01, rC01, rz0, rB1
	madd.d	rC11, rC11, rz1, rB1
					ldc1	ra1, 584(pA1)
	madd.d	rC21, rC21, rz2, rB1
	madd.d	rC31, rC31, rz3, rB1
					ldc1	rB1, 576(pB1)
	madd.d	rC02, rC02, rz0, rB2
	madd.d	rC12, rC12, rz1, rB2
					ldc1	ra2, 584(pA2)
	madd.d	rC22, rC22, rz2, rB2
	madd.d	rC32, rC32, rz3, rB2
					ldc1	rB2, 576(pB2)
	madd.d	rC03, rC03, rz0, rB3
	madd.d	rC13, rC13, rz1, rB3
					ldc1	ra3, 584(pA3)
	madd.d	rC23, rC23, rz2, rB3
	madd.d	rC33, rC33, rz3, rB3
					ldc1	rB3, 576(pB3)
#endif
/*       daddiu  K, K, 8
         bne K, K0, KLOOP
         madd.d  rC33, rC33, ra3, rb3 /*   /* in delay slot! */
/*
 *      Drain ld/use pipe
 */
	madd.d	rC00, rC00, rA0, rB0
					daddiu  pC0, pC0, CMUL(32)
	madd.d	rC10, rC10, rA1, rB0
					ldc1	rz0, KB*8-8(pA0)
	madd.d	rC20, rC20, rA2, rB0
					daddu   pA0, pA0, incAm
	madd.d	rC30, rC30, rA3, rB0
					ldc1	rB0, KB*8-16(pB0)
	madd.d	rC01, rC01, rA0, rB1
					daddiu  pC1, pC1, CMUL(32)
	madd.d	rC11, rC11, rA1, rB1
					ldc1	rz1, KB*8-8(pA1)
	madd.d	rC21, rC21, rA2, rB1
					daddu   pA1, pA1, incAm
	madd.d	rC31, rC31, rA3, rB1
					ldc1	rB1, KB*8-16(pB1)
	madd.d	rC02, rC02, rA0, rB2
					daddiu  pC2, pC2, CMUL(32)
	madd.d	rC12, rC12, rA1, rB2
					ldc1	rz2, KB*8-8(pA2)
	madd.d	rC22, rC22, rA2, rB2
					daddu   pA2, pA2, incAm
	madd.d	rC32, rC32, rA3, rB2
					ldc1	rB2, KB*8-16(pB2)
	madd.d	rC03, rC03, rA0, rB3
					daddiu  pC3, pC3, CMUL(32)
	madd.d	rC13, rC13, rA1, rB3
					ldc1	rz3, KB*8-8(pA3)
	madd.d	rC23, rC23, rA2, rB3
					daddu   pA3, pA3, incAm
	madd.d	rC33, rC33, rA3, rB3
					ldc1	rB3, KB*8-16(pB3)
	madd.d	rC00, rC00, ra0, rB0
	madd.d	rC10, rC10, ra1, rB0
	madd.d	rC20, rC20, ra2, rB0
	madd.d	rC30, rC30, ra3, rB0
					ldc1	rB0, KB*8-8(pB0)
	madd.d	rC01, rC01, ra0, rB1
	madd.d	rC11, rC11, ra1, rB1
	madd.d	rC21, rC21, ra2, rB1
	madd.d	rC31, rC31, ra3, rB1
					ldc1	rB1, KB*8-8(pB1)
	madd.d	rC02, rC02, ra0, rB2
	madd.d	rC12, rC12, ra1, rB2
	madd.d	rC22, rC22, ra2, rB2
	madd.d	rC32, rC32, ra3, rB2
					ldc1	rB2, KB*8-8(pB2)
	madd.d	rC03, rC03, ra0, rB3
	madd.d	rC13, rC13, ra1, rB3
	madd.d	rC23, rC23, ra2, rB3
	madd.d	rC33, rC33, ra3, rB3
					ldc1	rB3, KB*8-8(pB3)
	madd.d	rC00, rC00, rz0, rB0
	madd.d	rC10, rC10, rz1, rB0
	madd.d	rC20, rC20, rz2, rB0
	madd.d	rC30, rC30, rz3, rB0
	madd.d	rC01, rC01, rz0, rB1
        				sdc1    rC00, -CMUL(32)(pC0)
	madd.d	rC11, rC11, rz1, rB1
        				sdc1    rC10, -CMUL(24)(pC0)
	madd.d	rC21, rC21, rz2, rB1
        				sdc1    rC20, -CMUL(16)(pC0)
	madd.d	rC31, rC31, rz3, rB1
        				sdc1    rC30, -CMUL(8)(pC0)
	madd.d	rC02, rC02, rz0, rB2
        				sdc1    rC01, -CMUL(32)(pC1)
	madd.d	rC12, rC12, rz1, rB2
        				sdc1    rC11, -CMUL(24)(pC1)
	madd.d	rC22, rC22, rz2, rB2
        				sdc1    rC21, -CMUL(16)(pC1)
	madd.d	rC32, rC32, rz3, rB2
        				sdc1    rC31, -CMUL(8)(pC1)
	madd.d	rC03, rC03, rz0, rB3
        				sdc1    rC02, -CMUL(32)(pC2)
	madd.d	rC13, rC13, rz1, rB3
        				sdc1    rC12, -CMUL(24)(pC2)
	madd.d	rC23, rC23, rz2, rB3
        				sdc1    rC22, -CMUL(16)(pC2)
	madd.d	rC33, rC33, rz3, rB3
        				sdc1    rC32, -CMUL(8)(pC2)
        sdc1    rC03, -CMUL(32)(pC3)
        sdc1    rC13, -CMUL(24)(pC3)
        sdc1    rC23, -CMUL(16)(pC3)
        bne pA0, stAm, MLOOP
        sdc1    rC33, -CMUL(8)(pC3)

        dsubu   pA0, pA0, incAn
        dsubu   pA1, pA1, incAn
        dsubu   pA2, pA2, incAn
        dsubu   pA3, pA3, incAn
	daddu	pC0, pC0, incCn
	daddu	pC1, pC1, incCn
	daddu	pC2, pC2, incCn
	daddu	pC3, pC3, incCn
        daddu   pB0, pB0, incBn
        daddu   pB1, pB1, incBn
        daddu   pB2, pB2, incBn
        bne pB0, stBn, NLOOP
        daddu   pB3, pB3, incBn         /* delay slot! */
DONE:
/*
 *      Epilogue: restore registers and return
 */
        ld      $16, 0($sp)
        ld      $17, 8($sp)
        ld      $18, 16($sp)
        ld      $19, 24($sp)
        ld      $20, 32($sp)
        ld      $21, 40($sp)
        ld      $22, 64($sp)
        ld      $23, 72($sp)
        ld      $30, 80($sp)
#ifdef ATL_USE64BITS
        ldc1    $f24, 88($sp)
        ldc1    $f25, 96($sp)
        ldc1    $f26, 104($sp)
        ldc1    $f27, 112($sp)
        ldc1    $f28, 120($sp)
        ldc1    $f29, 128($sp)
        ldc1    $f30, 136($sp)
        ldc1    $f31, 144($sp)
#else
        ldc1    $f20, 88($sp)
        ldc1    $f22, 96($sp)
        ldc1    $f24, 104($sp)
        ldc1    $f26, 112($sp)
        ldc1    $f28, 120($sp)
        ldc1    $f30, 128($sp)
#endif
        j       $31
        daddiu  $sp, $sp, FSIZE         /* delay slot of return statement */

/*      end of file MIPS assembler BS */
        .set    macro
        .set    reorder
        .set    at
#ifndef ATL_OS_IRIX
        .size   ATL_USERMM,.-ATL_USERMM
#endif
        .end    ATL_USERMM
