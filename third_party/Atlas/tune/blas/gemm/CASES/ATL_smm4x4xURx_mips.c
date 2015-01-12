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
#if KB > 80
   #error "This kernel supports max KB of 80"
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
#define rE0  $f28
#define re0  $f29
#define rE2  $f30
#define rb0  $f31

#define M       $4
#define N       $5
// #define K0      $6
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
#ifdef SCPLX
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
   (const int M, const int N, const int K, const float  alpha, const float  *A,
               $9               $10           $11               0(%sp)
    const int lda, const float  *B, const int ldb, const float  beta,
        8($sp)       16($sp)
    float  *C, const int ldc)

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
#ifdef SCPLX
	sll	incCn, incCn, 3		/* incCn = ldc*sizeof */
#else
	sll	incCn, incCn, 2		/* incCn = ldc*sizeof */
#endif
        sll     incAm, incAm, 2         /* incAm = lda*sizeof */
        .set    macro
        dmul    incAn, incAm, M         /* incAn = lda*M */
        sll     incBn, incBn, 2         /* incBn = ldb*sizeof */
        dmul    stBn, incBn, N          /* stBn = ldb*N */
        .set    nomacro
//        sll     K0, K0, 3
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
#ifdef SCPLX
 	sll	pfA, M, 3		/* pfA = M*sizeof */
#else
 	sll	pfA, M, 2		/* pfA = M*sizeof */
#endif
        sll	incCn, incCn, 2		/* incCn = ldc*4 */
	dsubu	incCn, incCn, pfA	/* incCn = ldc*4 - M */
        or     pfA, stAm, $0
/*        or     pfB, stBn, $0 */
//	daddiu	K0, K0, -8
NLOOP:
	daddiu	pfB, pB0, 4*4*KB
MLOOP:
	lwc1	rB0, 0(pB0)
	lwc1	rA0, 0(pA0)
#ifdef BETA1
        lwc1    rC00, 0(pC0)
        lwc1    rC10, CMUL(4)(pC0)
        lwc1    rC20, CMUL(8)(pC0)
        lwc1    rC30, CMUL(12)(pC0)
        lwc1    rC01, 0(pC1)
        lwc1    rC11, CMUL(4)(pC1)
        lwc1    rC21, CMUL(8)(pC1)
        lwc1    rC31, CMUL(12)(pC1)
        lwc1    rC02, 0(pC2)
#elif defined(BETAX)
        lwc1    rB3, BETOFF($sp)           /* load BETA */
        lwc1    rC00, 0(pC0)
        lwc1    rC10, CMUL(4)(pC0)
        lwc1    rC20, CMUL(8)(pC0)
        lwc1    rC30, CMUL(12)(pC0)
        lwc1    rC01, 0(pC1)
		mul.s	rC00, rC00, rB3
        lwc1    rC11, CMUL(4)(pC1)
		mul.s	rC10, rC10, rB3
        lwc1    rC21, CMUL(8)(pC1)
		mul.s	rC20, rC20, rB3
        lwc1    rC31, CMUL(12)(pC1)
		mul.s	rC30, rC30, rB3
        lwc1    rC02, 0(pC2)
		mul.s	rC01, rC01, rB3
        lwc1    rC12, CMUL(4)(pC2)
		mul.s	rC11, rC11, rB3
        lwc1    rC22, CMUL(8)(pC2)
		mul.s	rC21, rC21, rB3
        lwc1    rC32, CMUL(12)(pC2)
		mul.s	rC31, rC31, rB3
        lwc1    rC03, 0(pC3)
		mul.s	rC02, rC02, rB3
        lwc1    rC13, CMUL(4)(pC3)
		mul.s	rC12, rC12, rB3
        lwc1    rC23, CMUL(8)(pC3)
		mul.s	rC22, rC22, rB3
        lwc1    rC33, CMUL(12)(pC3)
		mul.s	rC32, rC32, rB3
#endif
/*	.align 3 */

/* KLOOP: */
	lwc1	rA1, 0(pA1)
	lwc1	rA2, 0(pA2)
	lwc1	rA3, 0(pA3)
	lwc1	rB1, 0(pB1)
	lwc1	rB2, 0(pB2)
   #ifndef BETAX
	lwc1	rB3, 0(pB3)
   #endif
   #if KB > 1
	lwc1	ra0, 4(pA0)
   #endif
   #if KB > 1
	lwc1	ra2, 4(pA2)
   #endif
   #if KB > 2
	lwc1	rE0, 8(pA0)
   #endif

#if KB > 0
   #ifdef BETA0
	#if KB > 1
					lwc1	ra1, 4(pA1)
	#endif
	mul.s 	rC00, rA0, rB0
					daddiu  pC0, pC0, CMUL(16)
	mul.s 	rC10, rA1, rB0
	#if KB > 3
					lwc1	re0, 12(pA0)
	#endif
	mul.s 	rC20, rA2, rB0
					daddiu  pC1, pC1, CMUL(16)
	mul.s 	rC30, rA3, rB0
	#if KB > 1
					lwc1	rb0, 4(pB0)
	#endif
	mul.s 	rC01, rA0, rB1
					prefC(-CMUL(16)(pC0))
	mul.s 	rC11, rA1, rB1
					daddiu  pC2, pC2, CMUL(16)
	mul.s 	rC21, rA2, rB1
					daddiu  pC3, pC3, CMUL(16)
	mul.s 	rC31, rA3, rB1
	#if KB > 1
					lwc1	rB1, 4(pB1)
	#endif
	mul.s 	rC02, rA0, rB2
	mul.s 	rC12, rA1, rB2
	#if KB > 2
					lwc1	rE2, 8(pA2)
	#endif
	mul.s 	rC22, rA2, rB2
					prefC(-CMUL(16)(pC1))
	mul.s 	rC32, rA3, rB2
	#if KB > 1
					lwc1	rB2, 4(pB2)
	#endif
	mul.s 	rC03, rA0, rB3
					prefC(-CMUL(16)(pC2))
	mul.s 	rC13, rA1, rB3
	#if KB > 1
					lwc1	ra3, 4(pA3)
	#endif
	mul.s 	rC23, rA2, rB3
					prefC(-CMUL(16)(pC3))
	mul.s 	rC33, rA3, rB3
	#if KB > 1
					lwc1	rB3, 4(pB3)
	#endif
   #elif defined(BETAX)
	#if KB > 1
					lwc1	ra1, 4(pA1)
	#endif
	madd.s	rC00, rC00, rA0, rB0
					daddiu  pC0, pC0, CMUL(16)
	madd.s	rC10, rC10, rA1, rB0
		mul.s	rC03, rC03, rB3
	#if KB > 3
					lwc1	re0, 12(pA0)
	#endif
	madd.s	rC20, rC20, rA2, rB0
					daddiu  pC1, pC1, CMUL(16)
	madd.s	rC30, rC30, rA3, rB0
		mul.s	rC13, rC13, rB3
	#if KB > 1
					lwc1	rb0, 4(pB0)
	#endif
	madd.s	rC01, rC01, rA0, rB1
					daddiu  pC2, pC2, CMUL(16)
	madd.s	rC11, rC11, rA1, rB1
					daddiu  pC3, pC3, CMUL(16)
	madd.s	rC21, rC21, rA2, rB1
	madd.s	rC31, rC31, rA3, rB1
		mul.s	rC23, rC23, rB3
	#if KB > 1
					lwc1	rB1, 4(pB1)
	#endif
		mul.s	rC33, rC33, rB3
					lwc1	rB3, 0(pB3)
	madd.s	rC02, rC02, rA0, rB2
	madd.s	rC12, rC12, rA1, rB2
	madd.s	rC22, rC22, rA2, rB2
	#if KB > 2
					lwc1	rE2,  8(pA2)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 1
					lwc1	rB2, 4(pB2)
	#endif
	madd.s	rC03, rC03, rA0, rB3
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 1
					lwc1	ra3, 4(pA3)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 1
					lwc1	rB3, 4(pB3)
	#endif
   #else  /* BETA = 1 */
	#if KB > 1
					lwc1	ra1, 4(pA1)
	#endif
	madd.s	rC00, rC00, rA0, rB0
        				lwc1    rC12, CMUL(4)(pC2)
	madd.s	rC10, rC10, rA1, rB0
        				lwc1    rC22, CMUL(8)(pC2)
	madd.s	rC20, rC20, rA2, rB0
        				lwc1    rC32, CMUL(12)(pC2)
	madd.s	rC30, rC30, rA3, rB0
        				lwc1    rC03, 0(pC3)
	madd.s	rC01, rC01, rA0, rB1
        				lwc1    rC13, CMUL(4)(pC3)
	madd.s	rC11, rC11, rA1, rB1
        				lwc1    rC23, CMUL(8)(pC3)
	madd.s	rC21, rC21, rA2, rB1
	#if KB > 3
					lwc1	re0, 12(pA0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 1
					lwc1	rb0, 4(pB0)
	#endif
	madd.s	rC02, rC02, rA0, rB2
	#if KB > 1
					lwc1	rB1, 4(pB1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 2
					lwc1	rE2, 8(pA2)
	#endif
	madd.s	rC22, rC22, rA2, rB2
        				lwc1    rC33, CMUL(12)(pC3)
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 1
					lwc1	rB2, 4(pB2)
	#endif
	madd.s	rC03, rC03, rA0, rB3
					daddiu  pC0, pC0, CMUL(16)
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 1
					lwc1	ra3, 4(pA3)
	#endif
	madd.s	rC23, rC23, rA2, rB3
					daddiu  pC1, pC1, CMUL(16)
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 1
					lwc1	rB3, 4(pB3)
	#endif
   #endif /* end BETA specialization */
#endif
#if KB <= 2 && defined(BETA1)
					daddiu  pC2, pC2, CMUL(16)
					daddiu  pC3, pC3, CMUL(16)
#endif
	.align 3

#if KB > 1
	#if KB > 2
					lwc1	rA1, 8(pA1)
	#elif KB == 2
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, ra0, rb0
	#if KB == 2
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 4
					lwc1	rA0, 16(pA0)
	#elif KB == 2
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rb0
	#if KB == 2
					daddu	pA3, pA3, incAm
        #elif defined(BETA1)
					daddiu  pC2, pC2, CMUL(16)
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 2
					lwc1	rB0, 8(pB0)
	#elif KB == 2
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, ra0, rB1
	#if KB == 2
					swc1	rC10, -CMUL(12)(pC0)
        #elif defined(BETA1)
					daddiu  pC3, pC3, CMUL(16)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 2
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 2
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 2
					lwc1	rB1, 8(pB1)
	#elif KB == 2
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, ra0, rB2
	#if KB == 2
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 3
					lwc1	rA2, 12(pA2)
	#elif KB == 2
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 2
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 2
					lwc1	rB2, 8(pB2)
	#elif KB == 2
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, ra0, rB3
	#if KB == 2
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 2
					lwc1	rA3, 8(pA3)
	#elif KB == 2
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 2
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 2
					lwc1	rB3, 8(pB3)
	#endif
#endif
#if KB > 2
	#if KB > 3
					lwc1	ra1, 12(pA1)
	#elif KB == 3
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rE0, rB0
	#if KB == 3
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 5
					lwc1	ra0, 20(pA0)
	#elif KB == 3
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rB0
	#if KB == 3
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 3
					lwc1	rb0, 12(pB0)
	#elif KB == 3
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rE0, rB1
	#if KB == 3
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 3
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 3
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 3
					lwc1	rB1, 12(pB1)
	#elif KB == 3
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rE0, rB2
	#if KB == 3
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 4
					lwc1	ra2, 16(pA2)
	#elif KB == 3
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 3
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 3
					lwc1	rB2, 12(pB2)
	#elif KB == 3
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rE0, rB3
	#if KB == 3
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 3
					lwc1	ra3, 12(pA3)
	#elif KB == 3
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 3
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 3
					lwc1	rB3, 12(pB3)
	#endif
#endif
#if KB > 3
	#if KB > 4
					lwc1	rA1, 16(pA1)
	#elif KB == 4
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, re0, rb0
	#if KB == 4
					daddu	pA1, pA1, incAm
	#elif !defined(BETA0)
		prefC(16(pC0))
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 6
					lwc1	rE0, 24(pA0)
	#elif KB == 4
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rb0
	#if KB == 4
					daddu	pA3, pA3, incAm
	#elif !defined(BETA0)
		prefC(16(pC1))
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 4
					lwc1	rB0, 16(pB0)
	#elif KB == 4
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, re0, rB1
	#if KB == 4
					swc1	rC10, -CMUL(12)(pC0)
	#elif !defined(BETA0)
		prefC(16(pC2))
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 4
					swc1	rC20, -CMUL(8)(pC0)
	#elif !defined(BETA0)
		prefC(16(pC3))
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 4
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 4
					lwc1	rB1, 16(pB1)
	#elif KB == 4
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, re0, rB2
	#if KB == 4
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 5
					lwc1	rE2, 20(pA2)
	#elif KB == 4
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 4
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 4
					lwc1	rB2, 16(pB2)
	#elif KB == 4
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, re0, rB3
	#if KB == 4
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 4
					lwc1	rA3, 16(pA3)
	#elif KB == 4
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 4
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 4
					lwc1	rB3, 16(pB3)
	#endif
#endif
#if KB > 4
	#if KB > 5
					lwc1	ra1, 20(pA1)
	#elif KB == 5
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rA0, rB0
	#if KB == 5
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 7
					lwc1	re0, 28(pA0)
	#elif KB == 5
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rB0
	#if KB == 5
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 5
					lwc1	rb0, 20(pB0)
	#elif KB == 5
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rA0, rB1
	#if KB == 5
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 5
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 5
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 5
					lwc1	rB1, 20(pB1)
	#elif KB == 5
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rA0, rB2
	#if KB == 5
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 6
					lwc1	rA2, 24(pA2)
	#elif KB == 5
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 5
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 5
					lwc1	rB2, 20(pB2)
	#elif KB == 5
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rA0, rB3
	#if KB == 5
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 5
					lwc1	ra3, 20(pA3)
	#elif KB == 5
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 5
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 5
					lwc1	rB3, 20(pB3)
	#endif
#endif
#if KB > 5
	#if KB > 6
					lwc1	rA1, 24(pA1)
	#elif KB == 6
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, ra0, rb0
	#if KB == 6
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 8
					lwc1	rA0, 32(pA0)
	#elif KB == 6
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rb0
	#if KB == 6
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 6
					lwc1	rB0, 24(pB0)
	#elif KB == 6
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, ra0, rB1
	#if KB == 6
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 6
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 6
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 6
					lwc1	rB1, 24(pB1)
	#elif KB == 6
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, ra0, rB2
	#if KB == 6
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 7
					lwc1	ra2, 28(pA2)
	#elif KB == 6
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 6
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 6
					lwc1	rB2, 24(pB2)
	#elif KB == 6
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, ra0, rB3
	#if KB == 6
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 6
					lwc1	rA3, 24(pA3)
	#elif KB == 6
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 6
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 6
					lwc1	rB3, 24(pB3)
	#endif
#endif
#if KB > 6
	#if KB > 7
					lwc1	ra1, 28(pA1)
	#elif KB == 7
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rE0, rB0
	#if KB == 7
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 9
					lwc1	ra0, 36(pA0)
	#elif KB == 7
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rB0
	#if KB == 7
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 7
					lwc1	rb0, 28(pB0)
	#elif KB == 7
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rE0, rB1
	#if KB == 7
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 7
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 7
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 7
					lwc1	rB1, 28(pB1)
	#elif KB == 7
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rE0, rB2
	#if KB == 7
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 8
					lwc1	rE2, 32(pA2)
	#elif KB == 7
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 7
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 7
					lwc1	rB2, 28(pB2)
	#elif KB == 7
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rE0, rB3
	#if KB == 7
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 7
					lwc1	ra3, 28(pA3)
	#elif KB == 7
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 7
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 7
					lwc1	rB3, 28(pB3)
	#endif
#endif
#if KB > 7
	#if KB > 8
					lwc1	rA1, 32(pA1)
	#elif KB == 8
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, re0, rb0
	#if KB == 8
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 10
					lwc1	rE0, 40(pA0)
	#elif KB == 8
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rb0
	#if KB == 8
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 8
					lwc1	rB0, 32(pB0)
	#elif KB == 8
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, re0, rB1
	#if KB == 8
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 8
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 8
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 8
					lwc1	rB1, 32(pB1)
	#elif KB == 8
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, re0, rB2
	#if KB == 8
					swc1	rC11, -CMUL(12)(pC1)
	#endif
					prefB(0(pfB))
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 9
					lwc1	rA2, 36(pA2)
	#elif KB == 8
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 8
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 8
					lwc1	rB2, 32(pB2)
	#elif KB == 8
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, re0, rB3
	#if KB == 8
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 8
					lwc1	rA3, 32(pA3)
	#elif KB == 8
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 8
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 8
					lwc1	rB3, 32(pB3)
	#endif
#endif
#if KB > 8
	#if KB > 9
					lwc1	ra1, 36(pA1)
	#elif KB == 9
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rA0, rB0
	#if KB == 9
					daddu	pA1, pA1, incAm
	#endif
					prefB(32(pfB))
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 11
					lwc1	re0, 44(pA0)
	#elif KB == 9
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rB0
	#if KB == 9
					daddu	pA3, pA3, incAm
	#endif
					daddiu  pfB, pfB, 64
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 9
					lwc1	rb0, 36(pB0)
	#elif KB == 9
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rA0, rB1
	#if KB == 9
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 9
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 9
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 9
					lwc1	rB1, 36(pB1)
	#elif KB == 9
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rA0, rB2
	#if KB == 9
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 10
					lwc1	ra2, 40(pA2)
	#elif KB == 9
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 9
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 9
					lwc1	rB2, 36(pB2)
	#elif KB == 9
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rA0, rB3
	#if KB == 9
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 9
					lwc1	ra3, 36(pA3)
	#elif KB == 9
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 9
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 9
					lwc1	rB3, 36(pB3)
	#endif
#endif
#if KB > 9
	#if KB > 10
					lwc1	rA1, 40(pA1)
	#elif KB == 10
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, ra0, rb0
	#if KB == 10
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 12
					lwc1	rA0, 48(pA0)
	#elif KB == 10
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rb0
	#if KB == 10
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 10
					lwc1	rB0, 40(pB0)
	#elif KB == 10
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, ra0, rB1
	#if KB == 10
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 10
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 10
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 10
					lwc1	rB1, 40(pB1)
	#elif KB == 10
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, ra0, rB2
	#if KB == 10
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 11
					lwc1	rE2, 44(pA2)
	#elif KB == 10
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 10
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 10
					lwc1	rB2, 40(pB2)
	#elif KB == 10
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, ra0, rB3
	#if KB == 10
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 10
					lwc1	rA3, 40(pA3)
	#elif KB == 10
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 10
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 10
					lwc1	rB3, 40(pB3)
	#endif
#endif
#if KB > 10
	#if KB > 11
					lwc1	ra1, 44(pA1)
	#elif KB == 11
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rE0, rB0
	#if KB == 11
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 13
					lwc1	ra0, 52(pA0)
	#elif KB == 11
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rB0
	#if KB == 11
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 11
					lwc1	rb0, 44(pB0)
	#elif KB == 11
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rE0, rB1
	#if KB == 11
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 11
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 11
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 11
					lwc1	rB1, 44(pB1)
	#elif KB == 11
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rE0, rB2
	#if KB == 11
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 12
					lwc1	rA2, 48(pA2)
	#elif KB == 11
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 11
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 11
					lwc1	rB2, 44(pB2)
	#elif KB == 11
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rE0, rB3
	#if KB == 11
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 11
					lwc1	ra3, 44(pA3)
	#elif KB == 11
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 11
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 11
					lwc1	rB3, 44(pB3)
	#endif
#endif
#if KB > 11
	#if KB > 12
					lwc1	rA1, 48(pA1)
	#elif KB == 12
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, re0, rb0
	#if KB == 12
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 14
					lwc1	rE0, 56(pA0)
	#elif KB == 12
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rb0
	#if KB == 12
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 12
					lwc1	rB0, 48(pB0)
	#elif KB == 12
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, re0, rB1
	#if KB == 12
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 12
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 12
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 12
					lwc1	rB1, 48(pB1)
	#elif KB == 12
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, re0, rB2
	#if KB == 12
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 13
					lwc1	ra2, 52(pA2)
	#elif KB == 12
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 12
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 12
					lwc1	rB2, 48(pB2)
	#elif KB == 12
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, re0, rB3
	#if KB == 12
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 12
					lwc1	rA3, 48(pA3)
	#elif KB == 12
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 12
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 12
					lwc1	rB3, 48(pB3)
	#endif
#endif
#if KB > 12
	#if KB > 13
					lwc1	ra1, 52(pA1)
	#elif KB == 13
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rA0, rB0
	#if KB == 13
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 15
					lwc1	re0, 60(pA0)
	#elif KB == 13
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rB0
	#if KB == 13
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 13
					lwc1	rb0, 52(pB0)
	#elif KB == 13
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rA0, rB1
	#if KB == 13
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 13
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 13
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 13
					lwc1	rB1, 52(pB1)
	#elif KB == 13
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rA0, rB2
	#if KB == 13
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 14
					lwc1	rE2, 56(pA2)
	#elif KB == 13
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 13
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 13
					lwc1	rB2, 52(pB2)
	#elif KB == 13
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rA0, rB3
	#if KB == 13
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 13
					lwc1	ra3, 52(pA3)
	#elif KB == 13
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 13
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 13
					lwc1	rB3, 52(pB3)
	#endif
#endif
#if KB > 13
	#if KB > 14
					lwc1	rA1, 56(pA1)
	#elif KB == 14
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, ra0, rb0
	#if KB == 14
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 16
					lwc1	rA0, 64(pA0)
	#elif KB == 14
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rb0
	#if KB == 14
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 14
					lwc1	rB0, 56(pB0)
	#elif KB == 14
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, ra0, rB1
	#if KB == 14
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 14
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 14
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 14
					lwc1	rB1, 56(pB1)
	#elif KB == 14
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, ra0, rB2
	#if KB == 14
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 15
					lwc1	rA2, 60(pA2)
	#elif KB == 14
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 14
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 14
					lwc1	rB2, 56(pB2)
	#elif KB == 14
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, ra0, rB3
	#if KB == 14
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 14
					lwc1	rA3, 56(pA3)
	#elif KB == 14
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 14
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 14
					lwc1	rB3, 56(pB3)
	#endif
#endif
#if KB > 14
	#if KB > 15
					lwc1	ra1, 60(pA1)
	#elif KB == 15
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rE0, rB0
	#if KB == 15
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 17
					lwc1	ra0, 68(pA0)
	#elif KB == 15
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rB0
	#if KB == 15
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 15
					lwc1	rb0, 60(pB0)
	#elif KB == 15
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rE0, rB1
	#if KB == 15
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 15
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 15
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 15
					lwc1	rB1, 60(pB1)
	#elif KB == 15
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rE0, rB2
	#if KB == 15
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 16
					lwc1	ra2, 64(pA2)
	#elif KB == 15
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 15
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 15
					lwc1	rB2, 60(pB2)
	#elif KB == 15
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rE0, rB3
	#if KB == 15
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 15
					lwc1	ra3, 60(pA3)
	#elif KB == 15
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 15
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 15
					lwc1	rB3, 60(pB3)
	#endif
#endif
#if KB > 15
	#if KB > 16
					lwc1	rA1, 64(pA1)
	#elif KB == 16
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, re0, rb0
	#if KB == 16
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 18
					lwc1	rE0, 72(pA0)
	#elif KB == 16
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rb0
	#if KB == 16
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 16
					lwc1	rB0, 64(pB0)
	#elif KB == 16
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, re0, rB1
	#if KB == 16
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 16
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 16
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 16
					lwc1	rB1, 64(pB1)
	#elif KB == 16
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, re0, rB2
	#if KB == 16
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 17
					lwc1	rE2, 68(pA2)
	#elif KB == 16
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 16
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 16
					lwc1	rB2, 64(pB2)
	#elif KB == 16
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, re0, rB3
	#if KB == 16
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 16
					lwc1	rA3, 64(pA3)
	#elif KB == 16
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 16
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 16
					lwc1	rB3, 64(pB3)
	#endif
#endif
#if KB > 16
	#if KB > 17
					lwc1	ra1, 68(pA1)
	#elif KB == 17
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rA0, rB0
	#if KB == 17
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 19
					lwc1	re0, 76(pA0)
	#elif KB == 17
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rB0
	#if KB == 17
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 17
					lwc1	rb0, 68(pB0)
	#elif KB == 17
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rA0, rB1
	#if KB == 17
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 17
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 17
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 17
					lwc1	rB1, 68(pB1)
	#elif KB == 17
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rA0, rB2
	#if KB == 17
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 18
					lwc1	rA2, 72(pA2)
	#elif KB == 17
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 17
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 17
					lwc1	rB2, 68(pB2)
	#elif KB == 17
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rA0, rB3
	#if KB == 17
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 17
					lwc1	ra3, 68(pA3)
	#elif KB == 17
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 17
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 17
					lwc1	rB3, 68(pB3)
	#endif
#endif
#if KB > 17
	#if KB > 18
					lwc1	rA1, 72(pA1)
	#elif KB == 18
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, ra0, rb0
	#if KB == 18
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 20
					lwc1	rA0, 80(pA0)
	#elif KB == 18
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rb0
	#if KB == 18
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 18
					lwc1	rB0, 72(pB0)
	#elif KB == 18
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, ra0, rB1
	#if KB == 18
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 18
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 18
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 18
					lwc1	rB1, 72(pB1)
	#elif KB == 18
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, ra0, rB2
	#if KB == 18
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 19
					lwc1	ra2, 76(pA2)
	#elif KB == 18
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 18
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 18
					lwc1	rB2, 72(pB2)
	#elif KB == 18
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, ra0, rB3
	#if KB == 18
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 18
					lwc1	rA3, 72(pA3)
	#elif KB == 18
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 18
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 18
					lwc1	rB3, 72(pB3)
	#endif
#endif
#if KB > 18
	#if KB > 19
					lwc1	ra1, 76(pA1)
	#elif KB == 19
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rE0, rB0
	#if KB == 19
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 21
					lwc1	ra0, 84(pA0)
	#elif KB == 19
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rB0
	#if KB == 19
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 19
					lwc1	rb0, 76(pB0)
	#elif KB == 19
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rE0, rB1
	#if KB == 19
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 19
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 19
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 19
					lwc1	rB1, 76(pB1)
	#elif KB == 19
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rE0, rB2
	#if KB == 19
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 20
					lwc1	rE2, 80(pA2)
	#elif KB == 19
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 19
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 19
					lwc1	rB2, 76(pB2)
	#elif KB == 19
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rE0, rB3
	#if KB == 19
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 19
					lwc1	ra3, 76(pA3)
	#elif KB == 19
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 19
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 19
					lwc1	rB3, 76(pB3)
	#endif
#endif
#if KB > 19
	#if KB > 20
					lwc1	rA1, 80(pA1)
	#elif KB == 20
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, re0, rb0
	#if KB == 20
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 22
					lwc1	rE0, 88(pA0)
	#elif KB == 20
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rb0
	#if KB == 20
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 20
					lwc1	rB0, 80(pB0)
	#elif KB == 20
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, re0, rB1
	#if KB == 20
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 20
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 20
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 20
					lwc1	rB1, 80(pB1)
	#elif KB == 20
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, re0, rB2
	#if KB == 20
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 21
					lwc1	rA2, 84(pA2)
	#elif KB == 20
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 20
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 20
					lwc1	rB2, 80(pB2)
	#elif KB == 20
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, re0, rB3
	#if KB == 20
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 20
					lwc1	rA3, 80(pA3)
	#elif KB == 20
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 20
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 20
					lwc1	rB3, 80(pB3)
	#endif
#endif
#if KB > 20
	#if KB > 21
					lwc1	ra1, 84(pA1)
	#elif KB == 21
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rA0, rB0
	#if KB == 21
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 23
					lwc1	re0, 92(pA0)
	#elif KB == 21
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rB0
	#if KB == 21
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 21
					lwc1	rb0, 84(pB0)
	#elif KB == 21
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rA0, rB1
	#if KB == 21
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 21
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 21
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 21
					lwc1	rB1, 84(pB1)
	#elif KB == 21
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rA0, rB2
	#if KB == 21
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 22
					lwc1	ra2, 88(pA2)
	#elif KB == 21
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 21
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 21
					lwc1	rB2, 84(pB2)
	#elif KB == 21
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rA0, rB3
	#if KB == 21
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 21
					lwc1	ra3, 84(pA3)
	#elif KB == 21
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 21
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 21
					lwc1	rB3, 84(pB3)
	#endif
#endif
#if KB > 21
	#if KB > 22
					lwc1	rA1, 88(pA1)
	#elif KB == 22
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, ra0, rb0
	#if KB == 22
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 24
					lwc1	rA0, 96(pA0)
	#elif KB == 22
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rb0
	#if KB == 22
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 22
					lwc1	rB0, 88(pB0)
	#elif KB == 22
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, ra0, rB1
	#if KB == 22
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 22
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 22
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 22
					lwc1	rB1, 88(pB1)
	#elif KB == 22
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, ra0, rB2
	#if KB == 22
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 23
					lwc1	rE2, 92(pA2)
	#elif KB == 22
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 22
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 22
					lwc1	rB2, 88(pB2)
	#elif KB == 22
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, ra0, rB3
	#if KB == 22
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 22
					lwc1	rA3, 88(pA3)
	#elif KB == 22
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 22
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 22
					lwc1	rB3, 88(pB3)
	#endif
#endif
#if KB > 22
	#if KB > 23
					lwc1	ra1, 92(pA1)
	#elif KB == 23
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rE0, rB0
	#if KB == 23
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 25
					lwc1	ra0, 100(pA0)
	#elif KB == 23
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rB0
	#if KB == 23
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 23
					lwc1	rb0, 92(pB0)
	#elif KB == 23
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rE0, rB1
	#if KB == 23
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 23
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 23
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 23
					lwc1	rB1, 92(pB1)
	#elif KB == 23
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rE0, rB2
	#if KB == 23
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 24
					lwc1	rA2, 96(pA2)
	#elif KB == 23
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 23
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 23
					lwc1	rB2, 92(pB2)
	#elif KB == 23
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rE0, rB3
	#if KB == 23
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 23
					lwc1	ra3, 92(pA3)
	#elif KB == 23
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 23
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 23
					lwc1	rB3, 92(pB3)
	#endif
#endif
#if KB > 23
	#if KB > 24
					lwc1	rA1, 96(pA1)
	#elif KB == 24
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, re0, rb0
	#if KB == 24
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 26
					lwc1	rE0, 104(pA0)
	#elif KB == 24
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rb0
	#if KB == 24
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 24
					lwc1	rB0, 96(pB0)
	#elif KB == 24
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, re0, rB1
	#if KB == 24
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 24
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 24
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 24
					lwc1	rB1, 96(pB1)
	#elif KB == 24
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, re0, rB2
	#if KB == 24
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 25
					lwc1	ra2, 100(pA2)
	#elif KB == 24
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 24
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 24
					lwc1	rB2, 96(pB2)
	#elif KB == 24
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, re0, rB3
	#if KB == 24
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 24
					lwc1	rA3, 96(pA3)
	#elif KB == 24
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 24
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 24
					lwc1	rB3, 96(pB3)
	#endif
#endif
#if KB > 24
	#if KB > 25
					lwc1	ra1, 100(pA1)
	#elif KB == 25
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rA0, rB0
	#if KB == 25
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 27
					lwc1	re0, 108(pA0)
	#elif KB == 25
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rB0
	#if KB == 25
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 25
					lwc1	rb0, 100(pB0)
	#elif KB == 25
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rA0, rB1
	#if KB == 25
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 25
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 25
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 25
					lwc1	rB1, 100(pB1)
	#elif KB == 25
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rA0, rB2
	#if KB == 25
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 26
					lwc1	rE2, 104(pA2)
	#elif KB == 25
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 25
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 25
					lwc1	rB2, 100(pB2)
	#elif KB == 25
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rA0, rB3
	#if KB == 25
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 25
					lwc1	ra3, 100(pA3)
	#elif KB == 25
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 25
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 25
					lwc1	rB3, 100(pB3)
	#endif
#endif
#if KB > 25
	#if KB > 26
					lwc1	rA1, 104(pA1)
	#elif KB == 26
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, ra0, rb0
	#if KB == 26
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 28
					lwc1	rA0, 112(pA0)
	#elif KB == 26
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rb0
	#if KB == 26
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 26
					lwc1	rB0, 104(pB0)
	#elif KB == 26
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, ra0, rB1
	#if KB == 26
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 26
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 26
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 26
					lwc1	rB1, 104(pB1)
	#elif KB == 26
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, ra0, rB2
	#if KB == 26
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 27
					lwc1	rA2, 108(pA2)
	#elif KB == 26
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 26
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 26
					lwc1	rB2, 104(pB2)
	#elif KB == 26
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, ra0, rB3
	#if KB == 26
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 26
					lwc1	rA3, 104(pA3)
	#elif KB == 26
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 26
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 26
					lwc1	rB3, 104(pB3)
	#endif
#endif
#if KB > 26
	#if KB > 27
					lwc1	ra1, 108(pA1)
	#elif KB == 27
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rE0, rB0
	#if KB == 27
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 29
					lwc1	ra0, 116(pA0)
	#elif KB == 27
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rB0
	#if KB == 27
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 27
					lwc1	rb0, 108(pB0)
	#elif KB == 27
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rE0, rB1
	#if KB == 27
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 27
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 27
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 27
					lwc1	rB1, 108(pB1)
	#elif KB == 27
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rE0, rB2
	#if KB == 27
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 28
					lwc1	ra2, 112(pA2)
	#elif KB == 27
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 27
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 27
					lwc1	rB2, 108(pB2)
	#elif KB == 27
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rE0, rB3
	#if KB == 27
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 27
					lwc1	ra3, 108(pA3)
	#elif KB == 27
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 27
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 27
					lwc1	rB3, 108(pB3)
	#endif
#endif
#if KB > 27
	#if KB > 28
					lwc1	rA1, 112(pA1)
	#elif KB == 28
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, re0, rb0
	#if KB == 28
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 30
					lwc1	rE0, 120(pA0)
	#elif KB == 28
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rb0
	#if KB == 28
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 28
					lwc1	rB0, 112(pB0)
	#elif KB == 28
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, re0, rB1
	#if KB == 28
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 28
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 28
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 28
					lwc1	rB1, 112(pB1)
	#elif KB == 28
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, re0, rB2
	#if KB == 28
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 29
					lwc1	rE2, 116(pA2)
	#elif KB == 28
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 28
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 28
					lwc1	rB2, 112(pB2)
	#elif KB == 28
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, re0, rB3
	#if KB == 28
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 28
					lwc1	rA3, 112(pA3)
	#elif KB == 28
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 28
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 28
					lwc1	rB3, 112(pB3)
	#endif
#endif
#if KB > 28
	#if KB > 29
					lwc1	ra1, 116(pA1)
	#elif KB == 29
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rA0, rB0
	#if KB == 29
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 31
					lwc1	re0, 124(pA0)
	#elif KB == 29
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rB0
	#if KB == 29
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 29
					lwc1	rb0, 116(pB0)
	#elif KB == 29
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rA0, rB1
	#if KB == 29
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 29
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 29
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 29
					lwc1	rB1, 116(pB1)
	#elif KB == 29
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rA0, rB2
	#if KB == 29
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 30
					lwc1	rA2, 120(pA2)
	#elif KB == 29
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 29
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 29
					lwc1	rB2, 116(pB2)
	#elif KB == 29
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rA0, rB3
	#if KB == 29
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 29
					lwc1	ra3, 116(pA3)
	#elif KB == 29
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 29
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 29
					lwc1	rB3, 116(pB3)
	#endif
#endif
#if KB > 29
	#if KB > 30
					lwc1	rA1, 120(pA1)
	#elif KB == 30
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, ra0, rb0
	#if KB == 30
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 32
					lwc1	rA0, 128(pA0)
	#elif KB == 30
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rb0
	#if KB == 30
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 30
					lwc1	rB0, 120(pB0)
	#elif KB == 30
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, ra0, rB1
	#if KB == 30
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 30
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 30
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 30
					lwc1	rB1, 120(pB1)
	#elif KB == 30
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, ra0, rB2
	#if KB == 30
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 31
					lwc1	ra2, 124(pA2)
	#elif KB == 30
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 30
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 30
					lwc1	rB2, 120(pB2)
	#elif KB == 30
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, ra0, rB3
	#if KB == 30
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 30
					lwc1	rA3, 120(pA3)
	#elif KB == 30
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 30
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 30
					lwc1	rB3, 120(pB3)
	#endif
#endif
#if KB > 30
	#if KB > 31
					lwc1	ra1, 124(pA1)
	#elif KB == 31
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rE0, rB0
	#if KB == 31
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 33
					lwc1	ra0, 132(pA0)
	#elif KB == 31
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rB0
	#if KB == 31
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 31
					lwc1	rb0, 124(pB0)
	#elif KB == 31
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rE0, rB1
	#if KB == 31
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 31
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 31
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 31
					lwc1	rB1, 124(pB1)
	#elif KB == 31
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rE0, rB2
	#if KB == 31
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 32
					lwc1	rE2, 128(pA2)
	#elif KB == 31
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 31
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 31
					lwc1	rB2, 124(pB2)
	#elif KB == 31
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rE0, rB3
	#if KB == 31
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 31
					lwc1	ra3, 124(pA3)
	#elif KB == 31
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 31
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 31
					lwc1	rB3, 124(pB3)
	#endif
#endif
#if KB > 31
	#if KB > 32
					lwc1	rA1, 128(pA1)
	#elif KB == 32
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, re0, rb0
	#if KB == 32
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 34
					lwc1	rE0, 136(pA0)
	#elif KB == 32
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rb0
	#if KB == 32
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 32
					lwc1	rB0, 128(pB0)
	#elif KB == 32
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, re0, rB1
	#if KB == 32
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 32
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 32
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 32
					lwc1	rB1, 128(pB1)
	#elif KB == 32
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, re0, rB2
	#if KB == 32
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 33
					lwc1	rA2, 132(pA2)
	#elif KB == 32
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 32
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 32
					lwc1	rB2, 128(pB2)
	#elif KB == 32
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, re0, rB3
	#if KB == 32
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 32
					lwc1	rA3, 128(pA3)
	#elif KB == 32
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 32
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 32
					lwc1	rB3, 128(pB3)
	#endif
#endif
#if KB > 32
	#if KB > 33
					lwc1	ra1, 132(pA1)
	#elif KB == 33
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rA0, rB0
	#if KB == 33
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 35
					lwc1	re0, 140(pA0)
	#elif KB == 33
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rB0
	#if KB == 33
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 33
					lwc1	rb0, 132(pB0)
	#elif KB == 33
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rA0, rB1
	#if KB == 33
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 33
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 33
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 33
					lwc1	rB1, 132(pB1)
	#elif KB == 33
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rA0, rB2
	#if KB == 33
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 34
					lwc1	ra2, 136(pA2)
	#elif KB == 33
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 33
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 33
					lwc1	rB2, 132(pB2)
	#elif KB == 33
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rA0, rB3
	#if KB == 33
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 33
					lwc1	ra3, 132(pA3)
	#elif KB == 33
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 33
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 33
					lwc1	rB3, 132(pB3)
	#endif
#endif
#if KB > 33
	#if KB > 34
					lwc1	rA1, 136(pA1)
	#elif KB == 34
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, ra0, rb0
	#if KB == 34
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 36
					lwc1	rA0, 144(pA0)
	#elif KB == 34
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rb0
	#if KB == 34
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 34
					lwc1	rB0, 136(pB0)
	#elif KB == 34
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, ra0, rB1
	#if KB == 34
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 34
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 34
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 34
					lwc1	rB1, 136(pB1)
	#elif KB == 34
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, ra0, rB2
	#if KB == 34
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 35
					lwc1	rE2, 140(pA2)
	#elif KB == 34
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 34
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 34
					lwc1	rB2, 136(pB2)
	#elif KB == 34
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, ra0, rB3
	#if KB == 34
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 34
					lwc1	rA3, 136(pA3)
	#elif KB == 34
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 34
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 34
					lwc1	rB3, 136(pB3)
	#endif
#endif
#if KB > 34
	#if KB > 35
					lwc1	ra1, 140(pA1)
	#elif KB == 35
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rE0, rB0
	#if KB == 35
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 37
					lwc1	ra0, 148(pA0)
	#elif KB == 35
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rB0
	#if KB == 35
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 35
					lwc1	rb0, 140(pB0)
	#elif KB == 35
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rE0, rB1
	#if KB == 35
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 35
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 35
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 35
					lwc1	rB1, 140(pB1)
	#elif KB == 35
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rE0, rB2
	#if KB == 35
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 36
					lwc1	rA2, 144(pA2)
	#elif KB == 35
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 35
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 35
					lwc1	rB2, 140(pB2)
	#elif KB == 35
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rE0, rB3
	#if KB == 35
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 35
					lwc1	ra3, 140(pA3)
	#elif KB == 35
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 35
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 35
					lwc1	rB3, 140(pB3)
	#endif
#endif
#if KB > 35
	#if KB > 36
					lwc1	rA1, 144(pA1)
	#elif KB == 36
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, re0, rb0
	#if KB == 36
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 38
					lwc1	rE0, 152(pA0)
	#elif KB == 36
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rb0
	#if KB == 36
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 36
					lwc1	rB0, 144(pB0)
	#elif KB == 36
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, re0, rB1
	#if KB == 36
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 36
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 36
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 36
					lwc1	rB1, 144(pB1)
	#elif KB == 36
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, re0, rB2
	#if KB == 36
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 37
					lwc1	ra2, 148(pA2)
	#elif KB == 36
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 36
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 36
					lwc1	rB2, 144(pB2)
	#elif KB == 36
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, re0, rB3
	#if KB == 36
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 36
					lwc1	rA3, 144(pA3)
	#elif KB == 36
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 36
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 36
					lwc1	rB3, 144(pB3)
	#endif
#endif
#if KB > 36
	#if KB > 37
					lwc1	ra1, 148(pA1)
	#elif KB == 37
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rA0, rB0
	#if KB == 37
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 39
					lwc1	re0, 156(pA0)
	#elif KB == 37
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rB0
	#if KB == 37
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 37
					lwc1	rb0, 148(pB0)
	#elif KB == 37
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rA0, rB1
	#if KB == 37
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 37
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 37
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 37
					lwc1	rB1, 148(pB1)
	#elif KB == 37
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rA0, rB2
	#if KB == 37
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 38
					lwc1	rE2, 152(pA2)
	#elif KB == 37
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 37
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 37
					lwc1	rB2, 148(pB2)
	#elif KB == 37
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rA0, rB3
	#if KB == 37
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 37
					lwc1	ra3, 148(pA3)
	#elif KB == 37
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 37
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 37
					lwc1	rB3, 148(pB3)
	#endif
#endif
#if KB > 37
	#if KB > 38
					lwc1	rA1, 152(pA1)
	#elif KB == 38
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, ra0, rb0
	#if KB == 38
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 40
					lwc1	rA0, 160(pA0)
	#elif KB == 38
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rb0
	#if KB == 38
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 38
					lwc1	rB0, 152(pB0)
	#elif KB == 38
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, ra0, rB1
	#if KB == 38
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 38
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 38
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 38
					lwc1	rB1, 152(pB1)
	#elif KB == 38
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, ra0, rB2
	#if KB == 38
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 39
					lwc1	rA2, 156(pA2)
	#elif KB == 38
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 38
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 38
					lwc1	rB2, 152(pB2)
	#elif KB == 38
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, ra0, rB3
	#if KB == 38
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 38
					lwc1	rA3, 152(pA3)
	#elif KB == 38
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 38
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 38
					lwc1	rB3, 152(pB3)
	#endif
#endif
#if KB > 38
	#if KB > 39
					lwc1	ra1, 156(pA1)
	#elif KB == 39
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rE0, rB0
	#if KB == 39
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 41
					lwc1	ra0, 164(pA0)
	#elif KB == 39
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rB0
	#if KB == 39
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 39
					lwc1	rb0, 156(pB0)
	#elif KB == 39
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rE0, rB1
	#if KB == 39
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 39
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 39
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 39
					lwc1	rB1, 156(pB1)
	#elif KB == 39
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rE0, rB2
	#if KB == 39
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 40
					lwc1	ra2, 160(pA2)
	#elif KB == 39
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 39
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 39
					lwc1	rB2, 156(pB2)
	#elif KB == 39
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rE0, rB3
	#if KB == 39
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 39
					lwc1	ra3, 156(pA3)
	#elif KB == 39
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 39
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 39
					lwc1	rB3, 156(pB3)
	#endif
#endif
#if KB > 39
	#if KB > 40
					lwc1	rA1, 160(pA1)
	#elif KB == 40
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, re0, rb0
	#if KB == 40
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 42
					lwc1	rE0, 168(pA0)
	#elif KB == 40
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rb0
	#if KB == 40
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 40
					lwc1	rB0, 160(pB0)
	#elif KB == 40
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, re0, rB1
	#if KB == 40
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 40
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 40
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 40
					lwc1	rB1, 160(pB1)
	#elif KB == 40
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, re0, rB2
	#if KB == 40
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 41
					lwc1	rE2, 164(pA2)
	#elif KB == 40
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 40
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 40
					lwc1	rB2, 160(pB2)
	#elif KB == 40
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, re0, rB3
	#if KB == 40
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 40
					lwc1	rA3, 160(pA3)
	#elif KB == 40
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 40
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 40
					lwc1	rB3, 160(pB3)
	#endif
#endif
#if KB > 40
	#if KB > 41
					lwc1	ra1, 164(pA1)
	#elif KB == 41
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rA0, rB0
	#if KB == 41
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 43
					lwc1	re0, 172(pA0)
	#elif KB == 41
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rB0
	#if KB == 41
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 41
					lwc1	rb0, 164(pB0)
	#elif KB == 41
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rA0, rB1
	#if KB == 41
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 41
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 41
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 41
					lwc1	rB1, 164(pB1)
	#elif KB == 41
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rA0, rB2
	#if KB == 41
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 42
					lwc1	rA2, 168(pA2)
	#elif KB == 41
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 41
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 41
					lwc1	rB2, 164(pB2)
	#elif KB == 41
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rA0, rB3
	#if KB == 41
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 41
					lwc1	ra3, 164(pA3)
	#elif KB == 41
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 41
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 41
					lwc1	rB3, 164(pB3)
	#endif
#endif
#if KB > 41
	#if KB > 42
					lwc1	rA1, 168(pA1)
	#elif KB == 42
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, ra0, rb0
	#if KB == 42
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 44
					lwc1	rA0, 176(pA0)
	#elif KB == 42
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rb0
	#if KB == 42
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 42
					lwc1	rB0, 168(pB0)
	#elif KB == 42
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, ra0, rB1
	#if KB == 42
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 42
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 42
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 42
					lwc1	rB1, 168(pB1)
	#elif KB == 42
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, ra0, rB2
	#if KB == 42
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 43
					lwc1	ra2, 172(pA2)
	#elif KB == 42
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 42
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 42
					lwc1	rB2, 168(pB2)
	#elif KB == 42
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, ra0, rB3
	#if KB == 42
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 42
					lwc1	rA3, 168(pA3)
	#elif KB == 42
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 42
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 42
					lwc1	rB3, 168(pB3)
	#endif
#endif
#if KB > 42
	#if KB > 43
					lwc1	ra1, 172(pA1)
	#elif KB == 43
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rE0, rB0
	#if KB == 43
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 45
					lwc1	ra0, 180(pA0)
	#elif KB == 43
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rB0
	#if KB == 43
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 43
					lwc1	rb0, 172(pB0)
	#elif KB == 43
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rE0, rB1
	#if KB == 43
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 43
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 43
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 43
					lwc1	rB1, 172(pB1)
	#elif KB == 43
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rE0, rB2
	#if KB == 43
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 44
					lwc1	rE2, 176(pA2)
	#elif KB == 43
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 43
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 43
					lwc1	rB2, 172(pB2)
	#elif KB == 43
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rE0, rB3
	#if KB == 43
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 43
					lwc1	ra3, 172(pA3)
	#elif KB == 43
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 43
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 43
					lwc1	rB3, 172(pB3)
	#endif
#endif
#if KB > 43
	#if KB > 44
					lwc1	rA1, 176(pA1)
	#elif KB == 44
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, re0, rb0
	#if KB == 44
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 46
					lwc1	rE0, 184(pA0)
	#elif KB == 44
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rb0
	#if KB == 44
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 44
					lwc1	rB0, 176(pB0)
	#elif KB == 44
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, re0, rB1
	#if KB == 44
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 44
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 44
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 44
					lwc1	rB1, 176(pB1)
	#elif KB == 44
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, re0, rB2
	#if KB == 44
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 45
					lwc1	rA2, 180(pA2)
	#elif KB == 44
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 44
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 44
					lwc1	rB2, 176(pB2)
	#elif KB == 44
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, re0, rB3
	#if KB == 44
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 44
					lwc1	rA3, 176(pA3)
	#elif KB == 44
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 44
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 44
					lwc1	rB3, 176(pB3)
	#endif
#endif
#if KB > 44
	#if KB > 45
					lwc1	ra1, 180(pA1)
	#elif KB == 45
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rA0, rB0
	#if KB == 45
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 47
					lwc1	re0, 188(pA0)
	#elif KB == 45
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rB0
	#if KB == 45
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 45
					lwc1	rb0, 180(pB0)
	#elif KB == 45
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rA0, rB1
	#if KB == 45
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 45
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 45
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 45
					lwc1	rB1, 180(pB1)
	#elif KB == 45
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rA0, rB2
	#if KB == 45
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 46
					lwc1	ra2, 184(pA2)
	#elif KB == 45
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 45
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 45
					lwc1	rB2, 180(pB2)
	#elif KB == 45
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rA0, rB3
	#if KB == 45
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 45
					lwc1	ra3, 180(pA3)
	#elif KB == 45
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 45
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 45
					lwc1	rB3, 180(pB3)
	#endif
#endif
#if KB > 45
	#if KB > 46
					lwc1	rA1, 184(pA1)
	#elif KB == 46
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, ra0, rb0
	#if KB == 46
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 48
					lwc1	rA0, 192(pA0)
	#elif KB == 46
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rb0
	#if KB == 46
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 46
					lwc1	rB0, 184(pB0)
	#elif KB == 46
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, ra0, rB1
	#if KB == 46
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 46
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 46
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 46
					lwc1	rB1, 184(pB1)
	#elif KB == 46
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, ra0, rB2
	#if KB == 46
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 47
					lwc1	rE2, 188(pA2)
	#elif KB == 46
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 46
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 46
					lwc1	rB2, 184(pB2)
	#elif KB == 46
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, ra0, rB3
	#if KB == 46
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 46
					lwc1	rA3, 184(pA3)
	#elif KB == 46
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 46
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 46
					lwc1	rB3, 184(pB3)
	#endif
#endif
#if KB > 46
	#if KB > 47
					lwc1	ra1, 188(pA1)
	#elif KB == 47
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rE0, rB0
	#if KB == 47
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 49
					lwc1	ra0, 196(pA0)
	#elif KB == 47
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rB0
	#if KB == 47
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 47
					lwc1	rb0, 188(pB0)
	#elif KB == 47
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rE0, rB1
	#if KB == 47
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 47
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 47
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 47
					lwc1	rB1, 188(pB1)
	#elif KB == 47
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rE0, rB2
	#if KB == 47
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 48
					lwc1	rA2, 192(pA2)
	#elif KB == 47
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 47
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 47
					lwc1	rB2, 188(pB2)
	#elif KB == 47
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rE0, rB3
	#if KB == 47
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 47
					lwc1	ra3, 188(pA3)
	#elif KB == 47
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 47
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 47
					lwc1	rB3, 188(pB3)
	#endif
#endif
#if KB > 47
	#if KB > 48
					lwc1	rA1, 192(pA1)
	#elif KB == 48
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, re0, rb0
	#if KB == 48
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 50
					lwc1	rE0, 200(pA0)
	#elif KB == 48
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rb0
	#if KB == 48
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 48
					lwc1	rB0, 192(pB0)
	#elif KB == 48
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, re0, rB1
	#if KB == 48
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 48
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 48
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 48
					lwc1	rB1, 192(pB1)
	#elif KB == 48
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, re0, rB2
	#if KB == 48
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 49
					lwc1	ra2, 196(pA2)
	#elif KB == 48
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 48
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 48
					lwc1	rB2, 192(pB2)
	#elif KB == 48
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, re0, rB3
	#if KB == 48
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 48
					lwc1	rA3, 192(pA3)
	#elif KB == 48
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 48
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 48
					lwc1	rB3, 192(pB3)
	#endif
#endif
#if KB > 48
	#if KB > 49
					lwc1	ra1, 196(pA1)
	#elif KB == 49
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rA0, rB0
	#if KB == 49
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 51
					lwc1	re0, 204(pA0)
	#elif KB == 49
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rB0
	#if KB == 49
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 49
					lwc1	rb0, 196(pB0)
	#elif KB == 49
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rA0, rB1
	#if KB == 49
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 49
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 49
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 49
					lwc1	rB1, 196(pB1)
	#elif KB == 49
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rA0, rB2
	#if KB == 49
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 50
					lwc1	rE2, 200(pA2)
	#elif KB == 49
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 49
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 49
					lwc1	rB2, 196(pB2)
	#elif KB == 49
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rA0, rB3
	#if KB == 49
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 49
					lwc1	ra3, 196(pA3)
	#elif KB == 49
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 49
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 49
					lwc1	rB3, 196(pB3)
	#endif
#endif
#if KB > 49
	#if KB > 50
					lwc1	rA1, 200(pA1)
	#elif KB == 50
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, ra0, rb0
	#if KB == 50
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 52
					lwc1	rA0, 208(pA0)
	#elif KB == 50
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rb0
	#if KB == 50
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 50
					lwc1	rB0, 200(pB0)
	#elif KB == 50
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, ra0, rB1
	#if KB == 50
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 50
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 50
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 50
					lwc1	rB1, 200(pB1)
	#elif KB == 50
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, ra0, rB2
	#if KB == 50
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 51
					lwc1	rA2, 204(pA2)
	#elif KB == 50
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 50
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 50
					lwc1	rB2, 200(pB2)
	#elif KB == 50
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, ra0, rB3
	#if KB == 50
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 50
					lwc1	rA3, 200(pA3)
	#elif KB == 50
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 50
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 50
					lwc1	rB3, 200(pB3)
	#endif
#endif
#if KB > 50
	#if KB > 51
					lwc1	ra1, 204(pA1)
	#elif KB == 51
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rE0, rB0
	#if KB == 51
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 53
					lwc1	ra0, 212(pA0)
	#elif KB == 51
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rB0
	#if KB == 51
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 51
					lwc1	rb0, 204(pB0)
	#elif KB == 51
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rE0, rB1
	#if KB == 51
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 51
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 51
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 51
					lwc1	rB1, 204(pB1)
	#elif KB == 51
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rE0, rB2
	#if KB == 51
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 52
					lwc1	ra2, 208(pA2)
	#elif KB == 51
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 51
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 51
					lwc1	rB2, 204(pB2)
	#elif KB == 51
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rE0, rB3
	#if KB == 51
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 51
					lwc1	ra3, 204(pA3)
	#elif KB == 51
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 51
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 51
					lwc1	rB3, 204(pB3)
	#endif
#endif
#if KB > 51
	#if KB > 52
					lwc1	rA1, 208(pA1)
	#elif KB == 52
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, re0, rb0
	#if KB == 52
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 54
					lwc1	rE0, 216(pA0)
	#elif KB == 52
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rb0
	#if KB == 52
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 52
					lwc1	rB0, 208(pB0)
	#elif KB == 52
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, re0, rB1
	#if KB == 52
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 52
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 52
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 52
					lwc1	rB1, 208(pB1)
	#elif KB == 52
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, re0, rB2
	#if KB == 52
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 53
					lwc1	rE2, 212(pA2)
	#elif KB == 52
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 52
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 52
					lwc1	rB2, 208(pB2)
	#elif KB == 52
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, re0, rB3
	#if KB == 52
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 52
					lwc1	rA3, 208(pA3)
	#elif KB == 52
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 52
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 52
					lwc1	rB3, 208(pB3)
	#endif
#endif
#if KB > 52
	#if KB > 53
					lwc1	ra1, 212(pA1)
	#elif KB == 53
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rA0, rB0
	#if KB == 53
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 55
					lwc1	re0, 220(pA0)
	#elif KB == 53
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rB0
	#if KB == 53
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 53
					lwc1	rb0, 212(pB0)
	#elif KB == 53
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rA0, rB1
	#if KB == 53
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 53
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 53
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 53
					lwc1	rB1, 212(pB1)
	#elif KB == 53
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rA0, rB2
	#if KB == 53
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 54
					lwc1	rA2, 216(pA2)
	#elif KB == 53
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 53
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 53
					lwc1	rB2, 212(pB2)
	#elif KB == 53
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rA0, rB3
	#if KB == 53
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 53
					lwc1	ra3, 212(pA3)
	#elif KB == 53
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 53
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 53
					lwc1	rB3, 212(pB3)
	#endif
#endif
#if KB > 53
	#if KB > 54
					lwc1	rA1, 216(pA1)
	#elif KB == 54
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, ra0, rb0
	#if KB == 54
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 56
					lwc1	rA0, 224(pA0)
	#elif KB == 54
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rb0
	#if KB == 54
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 54
					lwc1	rB0, 216(pB0)
	#elif KB == 54
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, ra0, rB1
	#if KB == 54
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 54
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 54
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 54
					lwc1	rB1, 216(pB1)
	#elif KB == 54
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, ra0, rB2
	#if KB == 54
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 55
					lwc1	ra2, 220(pA2)
	#elif KB == 54
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 54
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 54
					lwc1	rB2, 216(pB2)
	#elif KB == 54
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, ra0, rB3
	#if KB == 54
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 54
					lwc1	rA3, 216(pA3)
	#elif KB == 54
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 54
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 54
					lwc1	rB3, 216(pB3)
	#endif
#endif
#if KB > 54
	#if KB > 55
					lwc1	ra1, 220(pA1)
	#elif KB == 55
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rE0, rB0
	#if KB == 55
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 57
					lwc1	ra0, 228(pA0)
	#elif KB == 55
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rB0
	#if KB == 55
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 55
					lwc1	rb0, 220(pB0)
	#elif KB == 55
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rE0, rB1
	#if KB == 55
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 55
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 55
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 55
					lwc1	rB1, 220(pB1)
	#elif KB == 55
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rE0, rB2
	#if KB == 55
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 56
					lwc1	rE2, 224(pA2)
	#elif KB == 55
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 55
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 55
					lwc1	rB2, 220(pB2)
	#elif KB == 55
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rE0, rB3
	#if KB == 55
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 55
					lwc1	ra3, 220(pA3)
	#elif KB == 55
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 55
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 55
					lwc1	rB3, 220(pB3)
	#endif
#endif
#if KB > 55
	#if KB > 56
					lwc1	rA1, 224(pA1)
	#elif KB == 56
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, re0, rb0
	#if KB == 56
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 58
					lwc1	rE0, 232(pA0)
	#elif KB == 56
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rb0
	#if KB == 56
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 56
					lwc1	rB0, 224(pB0)
	#elif KB == 56
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, re0, rB1
	#if KB == 56
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 56
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 56
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 56
					lwc1	rB1, 224(pB1)
	#elif KB == 56
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, re0, rB2
	#if KB == 56
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 57
					lwc1	rA2, 228(pA2)
	#elif KB == 56
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 56
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 56
					lwc1	rB2, 224(pB2)
	#elif KB == 56
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, re0, rB3
	#if KB == 56
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 56
					lwc1	rA3, 224(pA3)
	#elif KB == 56
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 56
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 56
					lwc1	rB3, 224(pB3)
	#endif
#endif
#if KB > 56
	#if KB > 57
					lwc1	ra1, 228(pA1)
	#elif KB == 57
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rA0, rB0
	#if KB == 57
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 59
					lwc1	re0, 236(pA0)
	#elif KB == 57
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rB0
	#if KB == 57
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 57
					lwc1	rb0, 228(pB0)
	#elif KB == 57
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rA0, rB1
	#if KB == 57
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 57
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 57
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 57
					lwc1	rB1, 228(pB1)
	#elif KB == 57
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rA0, rB2
	#if KB == 57
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 58
					lwc1	ra2, 232(pA2)
	#elif KB == 57
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 57
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 57
					lwc1	rB2, 228(pB2)
	#elif KB == 57
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rA0, rB3
	#if KB == 57
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 57
					lwc1	ra3, 228(pA3)
	#elif KB == 57
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 57
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 57
					lwc1	rB3, 228(pB3)
	#endif
#endif
#if KB > 57
	#if KB > 58
					lwc1	rA1, 232(pA1)
	#elif KB == 58
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, ra0, rb0
	#if KB == 58
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 60
					lwc1	rA0, 240(pA0)
	#elif KB == 58
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rb0
	#if KB == 58
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 58
					lwc1	rB0, 232(pB0)
	#elif KB == 58
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, ra0, rB1
	#if KB == 58
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 58
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 58
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 58
					lwc1	rB1, 232(pB1)
	#elif KB == 58
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, ra0, rB2
	#if KB == 58
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 59
					lwc1	rE2, 236(pA2)
	#elif KB == 58
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 58
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 58
					lwc1	rB2, 232(pB2)
	#elif KB == 58
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, ra0, rB3
	#if KB == 58
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 58
					lwc1	rA3, 232(pA3)
	#elif KB == 58
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 58
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 58
					lwc1	rB3, 232(pB3)
	#endif
#endif
#if KB > 58
	#if KB > 59
					lwc1	ra1, 236(pA1)
	#elif KB == 59
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rE0, rB0
	#if KB == 59
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 61
					lwc1	ra0, 244(pA0)
	#elif KB == 59
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rB0
	#if KB == 59
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 59
					lwc1	rb0, 236(pB0)
	#elif KB == 59
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rE0, rB1
	#if KB == 59
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 59
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 59
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 59
					lwc1	rB1, 236(pB1)
	#elif KB == 59
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rE0, rB2
	#if KB == 59
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 60
					lwc1	rA2, 240(pA2)
	#elif KB == 59
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 59
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 59
					lwc1	rB2, 236(pB2)
	#elif KB == 59
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rE0, rB3
	#if KB == 59
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 59
					lwc1	ra3, 236(pA3)
	#elif KB == 59
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 59
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 59
					lwc1	rB3, 236(pB3)
	#endif
#endif
#if KB > 59
	#if KB > 60
					lwc1	rA1, 240(pA1)
	#elif KB == 60
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, re0, rb0
	#if KB == 60
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 62
					lwc1	rE0, 248(pA0)
	#elif KB == 60
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rb0
	#if KB == 60
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 60
					lwc1	rB0, 240(pB0)
	#elif KB == 60
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, re0, rB1
	#if KB == 60
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 60
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 60
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 60
					lwc1	rB1, 240(pB1)
	#elif KB == 60
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, re0, rB2
	#if KB == 60
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 61
					lwc1	ra2, 244(pA2)
	#elif KB == 60
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 60
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 60
					lwc1	rB2, 240(pB2)
	#elif KB == 60
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, re0, rB3
	#if KB == 60
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 60
					lwc1	rA3, 240(pA3)
	#elif KB == 60
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 60
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 60
					lwc1	rB3, 240(pB3)
	#endif
#endif
#if KB > 60
	#if KB > 61
					lwc1	ra1, 244(pA1)
	#elif KB == 61
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rA0, rB0
	#if KB == 61
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 63
					lwc1	re0, 252(pA0)
	#elif KB == 61
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rB0
	#if KB == 61
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 61
					lwc1	rb0, 244(pB0)
	#elif KB == 61
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rA0, rB1
	#if KB == 61
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 61
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 61
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 61
					lwc1	rB1, 244(pB1)
	#elif KB == 61
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rA0, rB2
	#if KB == 61
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 62
					lwc1	rE2, 248(pA2)
	#elif KB == 61
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 61
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 61
					lwc1	rB2, 244(pB2)
	#elif KB == 61
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rA0, rB3
	#if KB == 61
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 61
					lwc1	ra3, 244(pA3)
	#elif KB == 61
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 61
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 61
					lwc1	rB3, 244(pB3)
	#endif
#endif
#if KB > 61
	#if KB > 62
					lwc1	rA1, 248(pA1)
	#elif KB == 62
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, ra0, rb0
	#if KB == 62
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 64
					lwc1	rA0, 256(pA0)
	#elif KB == 62
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rb0
	#if KB == 62
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 62
					lwc1	rB0, 248(pB0)
	#elif KB == 62
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, ra0, rB1
	#if KB == 62
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 62
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 62
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 62
					lwc1	rB1, 248(pB1)
	#elif KB == 62
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, ra0, rB2
	#if KB == 62
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 63
					lwc1	rA2, 252(pA2)
	#elif KB == 62
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 62
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 62
					lwc1	rB2, 248(pB2)
	#elif KB == 62
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, ra0, rB3
	#if KB == 62
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 62
					lwc1	rA3, 248(pA3)
	#elif KB == 62
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 62
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 62
					lwc1	rB3, 248(pB3)
	#endif
#endif
#if KB > 62
	#if KB > 63
					lwc1	ra1, 252(pA1)
	#elif KB == 63
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rE0, rB0
	#if KB == 63
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 65
					lwc1	ra0, 260(pA0)
	#elif KB == 63
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rB0
	#if KB == 63
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 63
					lwc1	rb0, 252(pB0)
	#elif KB == 63
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rE0, rB1
	#if KB == 63
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 63
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 63
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 63
					lwc1	rB1, 252(pB1)
	#elif KB == 63
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rE0, rB2
	#if KB == 63
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 64
					lwc1	ra2, 256(pA2)
	#elif KB == 63
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 63
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 63
					lwc1	rB2, 252(pB2)
	#elif KB == 63
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rE0, rB3
	#if KB == 63
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 63
					lwc1	ra3, 252(pA3)
	#elif KB == 63
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 63
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 63
					lwc1	rB3, 252(pB3)
	#endif
#endif
#if KB > 63
	#if KB > 64
					lwc1	rA1, 256(pA1)
	#elif KB == 64
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, re0, rb0
	#if KB == 64
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 66
					lwc1	rE0, 264(pA0)
	#elif KB == 64
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rb0
	#if KB == 64
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 64
					lwc1	rB0, 256(pB0)
	#elif KB == 64
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, re0, rB1
	#if KB == 64
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 64
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 64
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 64
					lwc1	rB1, 256(pB1)
	#elif KB == 64
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, re0, rB2
	#if KB == 64
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 65
					lwc1	rE2, 260(pA2)
	#elif KB == 64
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 64
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 64
					lwc1	rB2, 256(pB2)
	#elif KB == 64
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, re0, rB3
	#if KB == 64
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 64
					lwc1	rA3, 256(pA3)
	#elif KB == 64
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 64
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 64
					lwc1	rB3, 256(pB3)
	#endif
#endif
#if KB > 64
	#if KB > 65
					lwc1	ra1, 260(pA1)
	#elif KB == 65
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rA0, rB0
	#if KB == 65
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 67
					lwc1	re0, 268(pA0)
	#elif KB == 65
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rB0
	#if KB == 65
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 65
					lwc1	rb0, 260(pB0)
	#elif KB == 65
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rA0, rB1
	#if KB == 65
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 65
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 65
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 65
					lwc1	rB1, 260(pB1)
	#elif KB == 65
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rA0, rB2
	#if KB == 65
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 66
					lwc1	rA2, 264(pA2)
	#elif KB == 65
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 65
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 65
					lwc1	rB2, 260(pB2)
	#elif KB == 65
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rA0, rB3
	#if KB == 65
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 65
					lwc1	ra3, 260(pA3)
	#elif KB == 65
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 65
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 65
					lwc1	rB3, 260(pB3)
	#endif
#endif
#if KB > 65
	#if KB > 66
					lwc1	rA1, 264(pA1)
	#elif KB == 66
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, ra0, rb0
	#if KB == 66
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 68
					lwc1	rA0, 272(pA0)
	#elif KB == 66
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rb0
	#if KB == 66
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 66
					lwc1	rB0, 264(pB0)
	#elif KB == 66
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, ra0, rB1
	#if KB == 66
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 66
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 66
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 66
					lwc1	rB1, 264(pB1)
	#elif KB == 66
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, ra0, rB2
	#if KB == 66
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 67
					lwc1	ra2, 268(pA2)
	#elif KB == 66
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 66
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 66
					lwc1	rB2, 264(pB2)
	#elif KB == 66
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, ra0, rB3
	#if KB == 66
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 66
					lwc1	rA3, 264(pA3)
	#elif KB == 66
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 66
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 66
					lwc1	rB3, 264(pB3)
	#endif
#endif
#if KB > 66
	#if KB > 67
					lwc1	ra1, 268(pA1)
	#elif KB == 67
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rE0, rB0
	#if KB == 67
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 69
					lwc1	ra0, 276(pA0)
	#elif KB == 67
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rB0
	#if KB == 67
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 67
					lwc1	rb0, 268(pB0)
	#elif KB == 67
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rE0, rB1
	#if KB == 67
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 67
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 67
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 67
					lwc1	rB1, 268(pB1)
	#elif KB == 67
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rE0, rB2
	#if KB == 67
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 68
					lwc1	rE2, 272(pA2)
	#elif KB == 67
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 67
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 67
					lwc1	rB2, 268(pB2)
	#elif KB == 67
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rE0, rB3
	#if KB == 67
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 67
					lwc1	ra3, 268(pA3)
	#elif KB == 67
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 67
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 67
					lwc1	rB3, 268(pB3)
	#endif
#endif
#if KB > 67
	#if KB > 68
					lwc1	rA1, 272(pA1)
	#elif KB == 68
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, re0, rb0
	#if KB == 68
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 70
					lwc1	rE0, 280(pA0)
	#elif KB == 68
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rb0
	#if KB == 68
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 68
					lwc1	rB0, 272(pB0)
	#elif KB == 68
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, re0, rB1
	#if KB == 68
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 68
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 68
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 68
					lwc1	rB1, 272(pB1)
	#elif KB == 68
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, re0, rB2
	#if KB == 68
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 69
					lwc1	rA2, 276(pA2)
	#elif KB == 68
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 68
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 68
					lwc1	rB2, 272(pB2)
	#elif KB == 68
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, re0, rB3
	#if KB == 68
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 68
					lwc1	rA3, 272(pA3)
	#elif KB == 68
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 68
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 68
					lwc1	rB3, 272(pB3)
	#endif
#endif
#if KB > 68
	#if KB > 69
					lwc1	ra1, 276(pA1)
	#elif KB == 69
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rA0, rB0
	#if KB == 69
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 71
					lwc1	re0, 284(pA0)
	#elif KB == 69
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rB0
	#if KB == 69
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 69
					lwc1	rb0, 276(pB0)
	#elif KB == 69
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rA0, rB1
	#if KB == 69
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 69
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 69
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 69
					lwc1	rB1, 276(pB1)
	#elif KB == 69
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rA0, rB2
	#if KB == 69
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 70
					lwc1	ra2, 280(pA2)
	#elif KB == 69
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 69
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 69
					lwc1	rB2, 276(pB2)
	#elif KB == 69
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rA0, rB3
	#if KB == 69
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 69
					lwc1	ra3, 276(pA3)
	#elif KB == 69
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 69
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 69
					lwc1	rB3, 276(pB3)
	#endif
#endif
#if KB > 69
	#if KB > 70
					lwc1	rA1, 280(pA1)
	#elif KB == 70
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, ra0, rb0
	#if KB == 70
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 72
					lwc1	rA0, 288(pA0)
	#elif KB == 70
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rb0
	#if KB == 70
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 70
					lwc1	rB0, 280(pB0)
	#elif KB == 70
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, ra0, rB1
	#if KB == 70
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 70
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 70
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 70
					lwc1	rB1, 280(pB1)
	#elif KB == 70
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, ra0, rB2
	#if KB == 70
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 71
					lwc1	rE2, 284(pA2)
	#elif KB == 70
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 70
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 70
					lwc1	rB2, 280(pB2)
	#elif KB == 70
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, ra0, rB3
	#if KB == 70
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 70
					lwc1	rA3, 280(pA3)
	#elif KB == 70
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 70
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 70
					lwc1	rB3, 280(pB3)
	#endif
#endif
#if KB > 70
	#if KB > 71
					lwc1	ra1, 284(pA1)
	#elif KB == 71
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rE0, rB0
	#if KB == 71
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 73
					lwc1	ra0, 292(pA0)
	#elif KB == 71
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rB0
	#if KB == 71
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 71
					lwc1	rb0, 284(pB0)
	#elif KB == 71
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rE0, rB1
	#if KB == 71
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 71
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 71
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 71
					lwc1	rB1, 284(pB1)
	#elif KB == 71
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rE0, rB2
	#if KB == 71
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 72
					lwc1	rA2, 288(pA2)
	#elif KB == 71
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 71
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 71
					lwc1	rB2, 284(pB2)
	#elif KB == 71
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rE0, rB3
	#if KB == 71
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 71
					lwc1	ra3, 284(pA3)
	#elif KB == 71
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 71
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 71
					lwc1	rB3, 284(pB3)
	#endif
#endif
#if KB > 71
	#if KB > 72
					lwc1	rA1, 288(pA1)
	#elif KB == 72
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, re0, rb0
	#if KB == 72
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 74
					lwc1	rE0, 296(pA0)
	#elif KB == 72
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rb0
	#if KB == 72
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 72
					lwc1	rB0, 288(pB0)
	#elif KB == 72
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, re0, rB1
	#if KB == 72
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 72
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 72
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 72
					lwc1	rB1, 288(pB1)
	#elif KB == 72
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, re0, rB2
	#if KB == 72
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 73
					lwc1	ra2, 292(pA2)
	#elif KB == 72
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 72
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 72
					lwc1	rB2, 288(pB2)
	#elif KB == 72
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, re0, rB3
	#if KB == 72
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 72
					lwc1	rA3, 288(pA3)
	#elif KB == 72
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 72
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 72
					lwc1	rB3, 288(pB3)
	#endif
#endif
#if KB > 72
	#if KB > 73
					lwc1	ra1, 292(pA1)
	#elif KB == 73
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rA0, rB0
	#if KB == 73
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 75
					lwc1	re0, 300(pA0)
	#elif KB == 73
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rB0
	#if KB == 73
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 73
					lwc1	rb0, 292(pB0)
	#elif KB == 73
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rA0, rB1
	#if KB == 73
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 73
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 73
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 73
					lwc1	rB1, 292(pB1)
	#elif KB == 73
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rA0, rB2
	#if KB == 73
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 74
					lwc1	rE2, 296(pA2)
	#elif KB == 73
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 73
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 73
					lwc1	rB2, 292(pB2)
	#elif KB == 73
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rA0, rB3
	#if KB == 73
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 73
					lwc1	ra3, 292(pA3)
	#elif KB == 73
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 73
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 73
					lwc1	rB3, 292(pB3)
	#endif
#endif
#if KB > 73
	#if KB > 74
					lwc1	rA1, 296(pA1)
	#elif KB == 74
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, ra0, rb0
	#if KB == 74
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 76
					lwc1	rA0, 304(pA0)
	#elif KB == 74
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rb0
	#if KB == 74
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 74
					lwc1	rB0, 296(pB0)
	#elif KB == 74
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, ra0, rB1
	#if KB == 74
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 74
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 74
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 74
					lwc1	rB1, 296(pB1)
	#elif KB == 74
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, ra0, rB2
	#if KB == 74
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 75
					lwc1	rA2, 300(pA2)
	#elif KB == 74
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 74
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 74
					lwc1	rB2, 296(pB2)
	#elif KB == 74
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, ra0, rB3
	#if KB == 74
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 74
					lwc1	rA3, 296(pA3)
	#elif KB == 74
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 74
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 74
					lwc1	rB3, 296(pB3)
	#endif
#endif
#if KB > 74
	#if KB > 75
					lwc1	ra1, 300(pA1)
	#elif KB == 75
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rE0, rB0
	#if KB == 75
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 77
					lwc1	ra0, 308(pA0)
	#elif KB == 75
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rB0
	#if KB == 75
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 75
					lwc1	rb0, 300(pB0)
	#elif KB == 75
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rE0, rB1
	#if KB == 75
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 75
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 75
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 75
					lwc1	rB1, 300(pB1)
	#elif KB == 75
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rE0, rB2
	#if KB == 75
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 76
					lwc1	ra2, 304(pA2)
	#elif KB == 75
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 75
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 75
					lwc1	rB2, 300(pB2)
	#elif KB == 75
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rE0, rB3
	#if KB == 75
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 75
					lwc1	ra3, 300(pA3)
	#elif KB == 75
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 75
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 75
					lwc1	rB3, 300(pB3)
	#endif
#endif
#if KB > 75
	#if KB > 76
					lwc1	rA1, 304(pA1)
	#elif KB == 76
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, re0, rb0
	#if KB == 76
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 78
					lwc1	rE0, 312(pA0)
	#elif KB == 76
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rb0
	#if KB == 76
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 76
					lwc1	rB0, 304(pB0)
	#elif KB == 76
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, re0, rB1
	#if KB == 76
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 76
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 76
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 76
					lwc1	rB1, 304(pB1)
	#elif KB == 76
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, re0, rB2
	#if KB == 76
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 77
					lwc1	rE2, 308(pA2)
	#elif KB == 76
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 76
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 76
					lwc1	rB2, 304(pB2)
	#elif KB == 76
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, re0, rB3
	#if KB == 76
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 76
					lwc1	rA3, 304(pA3)
	#elif KB == 76
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 76
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 76
					lwc1	rB3, 304(pB3)
	#endif
#endif
#if KB > 76
	#if KB > 77
					lwc1	ra1, 308(pA1)
	#elif KB == 77
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rA0, rB0
	#if KB == 77
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 79
					lwc1	re0, 316(pA0)
	#elif KB == 77
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rB0
	#if KB == 77
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 77
					lwc1	rb0, 308(pB0)
	#elif KB == 77
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rA0, rB1
	#if KB == 77
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 77
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 77
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 77
					lwc1	rB1, 308(pB1)
	#elif KB == 77
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rA0, rB2
	#if KB == 77
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 78
					lwc1	rA2, 312(pA2)
	#elif KB == 77
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 77
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 77
					lwc1	rB2, 308(pB2)
	#elif KB == 77
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rA0, rB3
	#if KB == 77
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 77
					lwc1	ra3, 308(pA3)
	#elif KB == 77
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 77
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 77
					lwc1	rB3, 308(pB3)
	#endif
#endif
#if KB > 77
	#if KB > 78
					lwc1	rA1, 312(pA1)
	#elif KB == 78
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, ra0, rb0
	#if KB == 78
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 80
					lwc1	rA0, 320(pA0)
	#elif KB == 78
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rE2, rb0
	#if KB == 78
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 78
					lwc1	rB0, 312(pB0)
	#elif KB == 78
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, ra0, rB1
	#if KB == 78
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 78
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rE2, rB1
	#if KB == 78
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 78
					lwc1	rB1, 312(pB1)
	#elif KB == 78
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, ra0, rB2
	#if KB == 78
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 79
					lwc1	ra2, 316(pA2)
	#elif KB == 78
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rE2, rB2
	#if KB == 78
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 78
					lwc1	rB2, 312(pB2)
	#elif KB == 78
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, ra0, rB3
	#if KB == 78
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 78
					lwc1	rA3, 312(pA3)
	#elif KB == 78
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rE2, rB3
	#if KB == 78
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 78
					lwc1	rB3, 312(pB3)
	#endif
#endif
#if KB > 78
	#if KB > 79
					lwc1	ra1, 316(pA1)
	#elif KB == 79
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, rE0, rB0
	#if KB == 79
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, rA1, rB0
	#if KB > 81
					lwc1	ra0, 324(pA0)
	#elif KB == 79
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, rA2, rB0
	#if KB == 79
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, rA3, rB0
	#if KB > 79
					lwc1	rb0, 316(pB0)
	#elif KB == 79
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, rE0, rB1
	#if KB == 79
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, rA1, rB1
	#if KB == 79
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, rA2, rB1
	#if KB == 79
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, rA3, rB1
	#if KB > 79
					lwc1	rB1, 316(pB1)
	#elif KB == 79
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, rE0, rB2
	#if KB == 79
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, rA1, rB2
	#if KB > 80
					lwc1	rE2, 320(pA2)
	#elif KB == 79
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, rA2, rB2
	#if KB == 79
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, rA3, rB2
	#if KB > 79
					lwc1	rB2, 316(pB2)
	#elif KB == 79
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, rE0, rB3
	#if KB == 79
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, rA1, rB3
	#if KB > 79
					lwc1	ra3, 316(pA3)
	#elif KB == 79
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, rA2, rB3
	#if KB == 79
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, rA3, rB3
	#if KB > 79
					lwc1	rB3, 316(pB3)
	#endif
#endif
#if KB > 79
	#if KB > 80
					lwc1	rA1, 320(pA1)
	#elif KB == 80
					daddu	pA0, pA0, incAm
	#endif
	madd.s	rC00, rC00, re0, rb0
	#if KB == 80
					daddu	pA1, pA1, incAm
	#endif
	madd.s	rC10, rC10, ra1, rb0
	#if KB > 82
					lwc1	rE0, 328(pA0)
	#elif KB == 80
					daddu	pA2, pA2, incAm
	#endif
	madd.s	rC20, rC20, ra2, rb0
	#if KB == 80
					daddu	pA3, pA3, incAm
	#endif
	madd.s	rC30, rC30, ra3, rb0
	#if KB > 80
					lwc1	rB0, 320(pB0)
	#elif KB == 80
					swc1	rC00, -CMUL(16)(pC0)
	#endif
	madd.s	rC01, rC01, re0, rB1
	#if KB == 80
					swc1	rC10, -CMUL(12)(pC0)
	#endif
	madd.s	rC11, rC11, ra1, rB1
	#if KB == 80
					swc1	rC20, -CMUL(8)(pC0)
	#endif
	madd.s	rC21, rC21, ra2, rB1
	#if KB == 80
					swc1	rC30, -CMUL(4)(pC0)
	#endif
	madd.s	rC31, rC31, ra3, rB1
	#if KB > 80
					lwc1	rB1, 320(pB1)
	#elif KB == 80
					swc1	rC01, -CMUL(16)(pC1)
	#endif
	madd.s	rC02, rC02, re0, rB2
	#if KB == 80
					swc1	rC11, -CMUL(12)(pC1)
	#endif
	madd.s	rC12, rC12, ra1, rB2
	#if KB > 81
					lwc1	rA2, 324(pA2)
	#elif KB == 80
					swc1	rC21, -CMUL(8)(pC1)
	#endif
	madd.s	rC22, rC22, ra2, rB2
	#if KB == 80
					swc1	rC31, -CMUL(4)(pC1)
	#endif
	madd.s	rC32, rC32, ra3, rB2
	#if KB > 80
					lwc1	rB2, 320(pB2)
	#elif KB == 80
					swc1	rC02, -CMUL(16)(pC2)
	#endif
	madd.s	rC03, rC03, re0, rB3
	#if KB == 80
					swc1	rC12, -CMUL(12)(pC2)
	#endif
	madd.s	rC13, rC13, ra1, rB3
	#if KB > 80
					lwc1	rA3, 320(pA3)
	#elif KB == 80
					swc1	rC22, -CMUL(8)(pC2)
	#endif
	madd.s	rC23, rC23, ra2, rB3
	#if KB == 80
					swc1	rC32, -CMUL(4)(pC2)
	#endif
	madd.s	rC33, rC33, ra3, rB3
	#if KB > 80
					lwc1	rB3, 320(pB3)
	#endif
#endif
/* end KLOOP */
/*
 *      Drain ld/use pipe
 */

  #if KB == 1

					daddu   pA0, pA0, incAm
					daddu   pA1, pA1, incAm
					daddu   pA2, pA2, incAm
					daddu   pA3, pA3, incAm
        				swc1    rC00, -CMUL(16)(pC0)
        				swc1    rC10, -CMUL(12)(pC0)
        				swc1    rC20, -CMUL(8)(pC0)
        				swc1    rC30, -CMUL(4)(pC0)
        				swc1    rC01, -CMUL(16)(pC1)
        				swc1    rC11, -CMUL(12)(pC1)
        				swc1    rC21, -CMUL(8)(pC1)
        				swc1    rC31, -CMUL(4)(pC1)
        				swc1    rC02, -CMUL(16)(pC2)
        				swc1    rC12, -CMUL(12)(pC2)
        				swc1    rC22, -CMUL(8)(pC2)
        				swc1    rC32, -CMUL(4)(pC2)
  #endif
        swc1    rC03, -CMUL(16)(pC3)
        swc1    rC13, -CMUL(12)(pC3)
        swc1    rC23, -CMUL(8)(pC3)
        bne pA0, stAm, MLOOP
        swc1    rC33, -CMUL(4)(pC3)

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
