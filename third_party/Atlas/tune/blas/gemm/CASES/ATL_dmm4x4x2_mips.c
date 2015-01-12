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
#define rb0  $f28
#define rb1  $f29
#define rb2  $f30
#define rb3  $f31

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
#ifndef KB
   #define KB 0
#endif
#define ADIST KB*4*8   		/* Prefetch distance for A */
#define BDIST ADIST		/* Pref dist for B */
#define CDIST 32		/* Pref dist for C */
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
	daddiu	K0, K0, -8
NLOOP:
MLOOP:


#ifdef BETA0
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
		mul.d	rC03, rC03, rB3
		mul.d	rC13, rC13, rB3
		mul.d	rC23, rC23, rB3
		mul.d	rC33, rC33, rB3
        ldc1    rB3, 0(pB3)
#else
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
        ldc1    rC22, CMUL(16)(pC2)
        ldc1    rC32, CMUL(24)(pC2)
        ldc1    rC03, 0(pC3)
        ldc1    rC13, CMUL(8)(pC3)
        ldc1    rC23, CMUL(16)(pC3)
        ldc1    rC33, CMUL(24)(pC3)
#endif
        li	K, 8
        ldc1    rB0, 0(pB0)
        ldc1    rA0, 0(pA0)
        ldc1    rA1, 0(pA1)
        ldc1    rA2, 0(pA2)
        ldc1    rA3, 0(pA3)
        ldc1    rB1, 0(pB1)
        ldc1    rB2, 0(pB2)
        ldc1    rB3, 0(pB3)
#if KB == 0
        beq K, K0, DRAIN
#endif
	.align 3
#if KB == 0 || KB > 2
KLOOP:
        				ldxc1    rb0, K(pB0)
        madd.d  rC00, rC00, rA0, rB0
        				ldxc1    ra0, K(pA0)
        madd.d  rC10, rC10, rA1, rB0
        				ldxc1    ra1, K(pA1)
        madd.d  rC20, rC20, rA2, rB0
        				ldxc1    ra2, K(pA2)
        madd.d  rC30, rC30, rA3, rB0
        				ldxc1    ra3, K(pA3)
        madd.d  rC01, rC01, rA0, rB1
        				ldxc1    rb1, K(pB1)
        madd.d  rC11, rC11, rA1, rB1
        				ldxc1    rb2, K(pB2)
        madd.d  rC21, rC21, rA2, rB1
        				ldxc1    rb3, K(pB3)
        madd.d  rC31, rC31, rA3, rB1
        				daddiu  K, K, 8
        madd.d  rC02, rC02, rA0, rB2
					pref	6, ADIST(pA0)
        madd.d  rC12, rC12, rA1, rB2
					pref	6, ADIST(pA1)
        madd.d  rC22, rC22, rA2, rB2
					pref	6, ADIST(pA2)
        madd.d  rC32, rC32, rA3, rB2
					pref	6, ADIST(pA3)
        madd.d  rC03, rC03, rA0, rB3
        				ldxc1    rA0, K(pA0)
        madd.d  rC13, rC13, rA1, rB3
        				ldxc1    rA1, K(pA1)
        madd.d  rC23, rC23, rA2, rB3
        				ldxc1    rA2, K(pA2)
        madd.d  rC33, rC33, rA3, rB3
        				ldxc1    rB0, K(pB0)
        madd.d  rC00, rC00, ra0, rb0
        				ldxc1    rA3, K(pA3)
        madd.d  rC10, rC10, ra1, rb0
        				ldxc1    rB1, K(pB1)
        madd.d  rC20, rC20, ra2, rb0
        				ldxc1    rB2, K(pB2)
        madd.d  rC30, rC30, ra3, rb0
        				ldxc1    rB3, K(pB3)
        madd.d  rC01, rC01, ra0, rb1
        madd.d  rC11, rC11, ra1, rb1
					pref	6, BDIST(pB0)
        madd.d  rC21, rC21, ra2, rb1
        madd.d  rC31, rC31, ra3, rb1
					pref	6, BDIST(pB1)
        madd.d  rC02, rC02, ra0, rb2
        madd.d  rC12, rC12, ra1, rb2
					pref	6, BDIST(pB2)
        madd.d  rC22, rC22, ra2, rb2
        madd.d  rC32, rC32, ra3, rb2
					pref	6, BDIST(pB3)
        madd.d  rC03, rC03, ra0, rb3
        madd.d  rC13, rC13, ra1, rb3
        madd.d  rC23, rC23, ra2, rb3
        daddiu  K, K, 8
        bne K, K0, KLOOP
        madd.d  rC33, rC33, ra3, rb3    /* in delay slot! */
#endif
/*
 *      Drain ld/use pipe
 */
#if KB == 0
.local DRAIN
DRAIN:
#endif
        ldxc1    rb0, K(pB0)
        madd.d  rC00, rC00, rA0, rB0
        ldxc1    ra0, K(pA0)
        madd.d  rC10, rC10, rA1, rB0
        ldxc1    ra1, K(pA1)
        madd.d  rC20, rC20, rA2, rB0
        ldxc1    ra2, K(pA2)
        madd.d  rC30, rC30, rA3, rB0
        ldxc1    ra3, K(pA3)
        madd.d  rC01, rC01, rA0, rB1
        ldxc1    rb1, K(pB1)
        madd.d  rC11, rC11, rA1, rB1
        ldxc1    rb2, K(pB2)
        madd.d  rC21, rC21, rA2, rB1
        ldxc1    rb3, K(pB3)
        madd.d  rC31, rC31, rA3, rB1
        daddu   pA0, pA0, incAm
        madd.d  rC02, rC02, rA0, rB2
        daddu   pA1, pA1, incAm
        madd.d  rC12, rC12, rA1, rB2
        daddu   pA2, pA2, incAm
        madd.d  rC22, rC22, rA2, rB2
        daddu   pA3, pA3, incAm
        madd.d  rC32, rC32, rA3, rB2
        daddiu  pC0, pC0, CMUL(32)
        madd.d  rC03, rC03, rA0, rB3
        daddiu  pC1, pC1, CMUL(32)
        madd.d  rC13, rC13, rA1, rB3
        daddiu  pC2, pC2, CMUL(32)
        madd.d  rC23, rC23, rA2, rB3
        daddiu  pC3, pC3, CMUL(32)
        madd.d  rC33, rC33, rA3, rB3

        madd.d  rC00, rC00, ra0, rb0
					pref	7, CDIST(pC0)
        madd.d  rC10, rC10, ra1, rb0
					pref	7, CDIST(pC1)
        madd.d  rC20, rC20, ra2, rb0
					pref	7, CDIST(pC2)
        madd.d  rC30, rC30, ra3, rb0
					pref	7, CDIST(pC3)
        madd.d  rC01, rC01, ra0, rb1
        				sdc1    rC00, -CMUL(32)(pC0)
        madd.d  rC11, rC11, ra1, rb1
        				sdc1    rC10, -CMUL(24)(pC0)
        madd.d  rC21, rC21, ra2, rb1
        				sdc1    rC20, -CMUL(16)(pC0)
        madd.d  rC31, rC31, ra3, rb1
        				sdc1    rC30, -CMUL(8)(pC0)
        madd.d  rC02, rC02, ra0, rb2
        				sdc1    rC01, -CMUL(32)(pC1)
        madd.d  rC12, rC12, ra1, rb2
        				sdc1    rC11, -CMUL(24)(pC1)
        madd.d  rC22, rC22, ra2, rb2
        				sdc1    rC21, -CMUL(16)(pC1)
        madd.d  rC32, rC32, ra3, rb2
        				sdc1    rC31, -CMUL(8)(pC1)
        madd.d  rC03, rC03, ra0, rb3
        				sdc1    rC02, -CMUL(32)(pC2)
        madd.d  rC13, rC13, ra1, rb3
        				sdc1    rC12, -CMUL(24)(pC2)
        madd.d  rC23, rC23, ra2, rb3
        				sdc1    rC22, -CMUL(16)(pC2)
        madd.d  rC33, rC33, ra3, rb3
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

/*       end of file MIPS assembler BS */
        .set    macro
        .set    reorder
        .set    at
#ifndef ATL_OS_IRIX
        .size   ATL_USERMM,.-ATL_USERMM
#endif
        .end    ATL_USERMM
