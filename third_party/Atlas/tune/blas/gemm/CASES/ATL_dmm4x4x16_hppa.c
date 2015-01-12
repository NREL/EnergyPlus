
#if !defined(ATL_LINUX_PARISC) && !defined(ATL_HPUX_PARISC)
   #error "This kernel requires gas PA-RISC assembler!"
#endif

#if !defined(KB) || KB == 0
   #error "KB must be compile time constant
#endif
#if (KB/16)*16 != KB
   #error "KB must be a multiple of 16!"
#endif
#ifndef MB
   #define MB 0
#endif
#ifndef NB
   #define NB 0
#endif

#if (((NB/4)*4) != NB) || (((MB/4)*4) != MB)
   #error "MB & NB must be a multiple of 4!"
#endif

#ifndef Mjoin
   #define Mjoin(pre, nam) my_join(pre, nam)
   #define my_join(pre, nam) pre ## nam
#endif
#ifdef DCPLX
   #define CMUL(i_) (2*(i_))
#else
   #define CMUL(i_) i_
#endif

/*
 * Integer register usage
 */
#define incCn	%r20
#define incAn	%r21
#define  rM     %r26
#define  rN     %r25
#define  rK     %r24
#define pA0	%r1
#define	pA1	%r3
#define	pA2	%r4
#define	pA3	%r5
#define pfA	%r6
#define	pfB	%r7
#define pB0	%r28
#define pB1	%r31
#define pB2	%r22
#define pB3	%r23
#define rMM     %r10
#define pC0	%r29
#define pC1	%r11
#define pC2	%r12
#define pC3	%r13

/*
 * fp reg usage
 */
#define rC00    %fr31
#define rC10    %fr30
#define rC20    %fr29
#define rC30    %fr28
#define rC01    %fr27
#define rC11    %fr26
#define rC21    %fr25
#define rC31    %fr24
#define rC02    %fr23
#define rC12    %fr22
#define rC22    %fr21
#define rC32    %fr20
#define rC03    %fr19
#define rC13    %fr18
#define rC23    %fr17
#define rC33    %fr16
#define rA0	%fr15
#define rA1	%fr14
#define rA2	%fr13
#define rA3	%fr12
#define ra0     %fr11
#define ra1     %fr10
#define ra2     %fr9
#define ra3     %fr8
#define rB0	%fr7
#define rB1	%fr6
#define rB2	%fr5
#define rB3	%fr4

#define FSIZE 128
	.LEVEL 2.0
#ifdef ATL_HPUX_PARISC
	.SPACE $PRIVATE$
	.SUBSPA $DATA$,QUAD=1,ALIGN=8,ACCESS=31
	.SUBSPA $BSS$,QUAD=1,ALIGN=8,ACCESS=31,ZERO,SORT=82
	.SPACE $TEXT$
	.SUBSPA $LIT$,QUAD=0,ALIGN=8,ACCESS=44
	.SUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY
	.IMPORT $global$,DATA
	.IMPORT $$dyncall,MILLICODE
	.SPACE $TEXT$
	.SUBSPA $CODE$

	.align 4
	.NSUBSPA $CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY
	.EXPORT ATL_USERMM,ENTRY,PRIV_LEV=3,ARGW0=GR,ARGW1=GR,ARGW2=GR,ARGW3=FR
ATL_USERMM
#else
        .text
        .align 4
        .globl ATL_USERMM
ATL_USERMM:
#endif
	.PROC
	.CALLINFO FRAME=FSIZE,NO_CALLS
	.ENTRY
/*
void ATL_USERMM
;         36           40           44                  56               60
(const int M, const int N, const int K, const double alpha, const double *A,
;          64               68             72                 80         84
const int lda, const double *B, const int ldb, const double beta, double *C,
;          88
const int ldc)
*/
	ldw	-60(%r30), pA0
	ldw	-68(%r30), pB0
	ldw	-84(%r30), pC0
	ldw	-88(%r30), incCn
/*
 *      Move frame pointer, and save registers
 */
	ldo	FSIZE(%r30), %r30
	fstd	%fr12, -8(%r30)
	fstd	%fr13, -16(%r30)
	fstd	%fr14, -24(%r30)
	fstd	%fr15, -32(%r30)
	fstd	%fr16, -40(%r30)
	fstd	%fr17, -48(%r30)
	fstd	%fr18, -56(%r30)
	fstd	%fr19, -64(%r30)
	fstd	%fr20, -72(%r30)
	fstd	%fr21, -80(%r30)
	stw	%r3, -84(%r30)
	stw	%r4, -88(%r30)
	stw	%r5, -92(%r30)
;	stw	%r6, -96(%r30)
;	stw	%r7, -100(%r30)
;	stw	%r8, -104(%r30)
;	stw	%r9, -108(%r30)
	stw	%r10, -112(%r30)
	stw	%r11, -116(%r30)
	stw	%r12, -120(%r30)
	stw	%r13, -124(%r30)
	copy	rM, rMM
;
;       incCn = ldc*sizeof
;
#ifdef DCPLX
	depw,z	incCn,27,28,incCn
#else
	depw,z	incCn,28,29,incCn
#endif
;
;       incAn = M*K*sizeof
;
	ldo	-FSIZE-36(%r30), pA1
	ldi	8*KB, incAn
	stw	rM, -4(pA1)
	stw	incAn, 0(pA1)
	fldw	0(pA1), %fr15L
	fldw	-4(pA1), %fr15R
	xmpyu	%fr15R, %fr15L, %fr14
	fstw	%fr14R, 0(pA1)
	ldw	0(pA1), incAn
;	extrs	incAn, 30, 31, pfB
;	add	pA0, incAn, pfA
;	add	pB0, incAn, pfB
;	add	pB0, pfB, pfB
;
;       Init pC[1-3] & set incCn = 4*ldc - MB
;
	add	incCn, pC0, pC1
	add	incCn, pC1, pC2
	add	incCn, pC2, pC3
	depw,z	incCn,29,30, incCn ; incCn = 4*ldc*size
#ifdef DCPLX
	depw,z	rMM, 27,28,pA1     ; pA1 = MB*size
#else
	depw,z	rMM, 28,29,pA1     ; pA1 = MB*size
#endif
	sub	incCn, pA1, incCn  ; incCn = 4*ldc*size - MB*size
;
;       Init pA[1-5] & pB[1-3]
;
	ldo	KB*8(pA0), pA1
	ldo	2*KB*8(pA0), pA2
	ldo	3*KB*8(pA0), pA3
	ldo	KB*8(pB0), pB1
	ldo	2*KB*8(pB0), pB2
	ldo	3*KB*8(pB0), pB3

NLOOP:
	copy	rMM, rM
MLOOP:
#ifdef BETA0
	fcpy,dbl	%fr0, rC00
	fcpy,dbl	%fr0, rC10
	fcpy,dbl	%fr0, rC20
	fcpy,dbl	%fr0, rC30
	fcpy,dbl	%fr0, rC01
	fcpy,dbl	%fr0, rC11
	fcpy,dbl	%fr0, rC21
	fcpy,dbl	%fr0, rC31
	fcpy,dbl	%fr0, rC02
	fcpy,dbl	%fr0, rC12
	fcpy,dbl	%fr0, rC22
	fcpy,dbl	%fr0, rC32
	fcpy,dbl	%fr0, rC03
	fcpy,dbl	%fr0, rC13
	fcpy,dbl	%fr0, rC23
	fcpy,dbl	%fr0, rC33
#else
	fldd	0(pC0), rC00
	fldd	CMUL(8)(pC0), rC10
	fldd	CMUL(16)(pC0), rC20
	fldd	CMUL(24)(pC0), rC30
	fldd	 0(pC1), rC01
	fldd	 CMUL(8)(pC1), rC11
	fldd	 CMUL(16)(pC1), rC21
	fldd	CMUL(24)(pC1), rC31
	fldd	 0(pC2), rC02
	fldd	 CMUL(8)(pC2), rC12
	fldd	 CMUL(16)(pC2), rC22
	fldd	CMUL(24)(pC2), rC32
	fldd	 0(pC3), rC03
	fldd	 CMUL(8)(pC3), rC13
	fldd	 CMUL(16)(pC3), rC23
	fldd	CMUL(24)(pC3), rC33
   #ifdef BETAX
	fldd	-FSIZE-80(%r30), ra0
	fmpy,dbl	ra0, rC00, rC00
	fmpy,dbl	ra0, rC10, rC10
	fmpy,dbl	ra0, rC20, rC20
	fmpy,dbl	ra0, rC30, rC30
	fmpy,dbl	ra0, rC01, rC01
	fmpy,dbl	ra0, rC11, rC11
	fmpy,dbl	ra0, rC21, rC21
	fmpy,dbl	ra0, rC31, rC31
	fmpy,dbl	ra0, rC02, rC02
	fmpy,dbl	ra0, rC12, rC12
	fmpy,dbl	ra0, rC22, rC22
	fmpy,dbl	ra0, rC32, rC32
	fmpy,dbl	ra0, rC03, rC03
	fmpy,dbl	ra0, rC13, rC13
	fmpy,dbl	ra0, rC23, rC23
	fmpy,dbl	ra0, rC33, rC33
   #endif
#endif
	fldd   	0(pB0), rB0
	fldd   	0(pA0), rA0
	fldd   	0(pA1), rA1
	fldd   	0(pA2), rA2
	fldd   	0(pA3), rA3
	fldd   	0(pB1), rB1
	fldd   	0(pB2), rB2
	fldd   	0(pB3), rB3

#if KB > 16
	ldi	KB-16, rK
KLOOP:
	fmpyfadd,dbl	rA0, rB0, rC00, rC00
	fmpyfadd,dbl	rA1, rB0, rC10, rC10
						fldd   	8(pA0), ra0
	fmpyfadd,dbl	rA2, rB0, rC20, rC20
	fmpyfadd,dbl	rA3, rB0, rC30, rC30
						fldd   	8(pB0), rB0

	fmpyfadd,dbl	rA0, rB1, rC01, rC01
	fmpyfadd,dbl	rA1, rB1, rC11, rC11
						fldd   	8(pA1), ra1
	fmpyfadd,dbl	rA2, rB1, rC21, rC21
	fmpyfadd,dbl	rA3, rB1, rC31, rC31
						fldd   	8(pB1), rB1

	fmpyfadd,dbl	rA0, rB2, rC02, rC02
	fmpyfadd,dbl	rA1, rB2, rC12, rC12
						fldd   	8(pA2), ra2
	fmpyfadd,dbl	rA2, rB2, rC22, rC22
	fmpyfadd,dbl	rA3, rB2, rC32, rC32
						fldd   	8(pB2), rB2

	fmpyfadd,dbl	rA0, rB3, rC03, rC03
	fmpyfadd,dbl	rA1, rB3, rC13, rC13
						fldd   	8(pA3), ra3
	fmpyfadd,dbl	rA2, rB3, rC23, rC23
	fmpyfadd,dbl	rA3, rB3, rC33, rC33
						fldd   	8(pB3), rB3

	fmpyfadd,dbl	ra0, rB0, rC00, rC00
	fmpyfadd,dbl	ra1, rB0, rC10, rC10
						fldd   	16(pA0), rA0
	fmpyfadd,dbl	ra2, rB0, rC20, rC20
	fmpyfadd,dbl	ra3, rB0, rC30, rC30
						fldd   	16(pB0), rB0

	fmpyfadd,dbl	ra0, rB1, rC01, rC01
	fmpyfadd,dbl	ra1, rB1, rC11, rC11
						fldd   	16(pA1), rA1
	fmpyfadd,dbl	ra2, rB1, rC21, rC21
	fmpyfadd,dbl	ra3, rB1, rC31, rC31
						fldd   	16(pB1), rB1

	fmpyfadd,dbl	ra0, rB2, rC02, rC02
	fmpyfadd,dbl	ra1, rB2, rC12, rC12
						fldd   	16(pA2), rA2
	fmpyfadd,dbl	ra2, rB2, rC22, rC22
	fmpyfadd,dbl	ra3, rB2, rC32, rC32
						fldd   	16(pB2), rB2

	fmpyfadd,dbl	ra0, rB3, rC03, rC03
	fmpyfadd,dbl	ra1, rB3, rC13, rC13
						fldd   	16(pA3), rA3
	fmpyfadd,dbl	ra2, rB3, rC23, rC23
	fmpyfadd,dbl	ra3, rB3, rC33, rC33
						fldd   	16(pB3), rB3

	fmpyfadd,dbl	rA0, rB0, rC00, rC00
	fmpyfadd,dbl	rA1, rB0, rC10, rC10
						fldd   	24(pA0), ra0
	fmpyfadd,dbl	rA2, rB0, rC20, rC20
	fmpyfadd,dbl	rA3, rB0, rC30, rC30
						fldd   	24(pB0), rB0

	fmpyfadd,dbl	rA0, rB1, rC01, rC01
	fmpyfadd,dbl	rA1, rB1, rC11, rC11
						fldd   	24(pA1), ra1
	fmpyfadd,dbl	rA2, rB1, rC21, rC21
	fmpyfadd,dbl	rA3, rB1, rC31, rC31
						fldd   	24(pB1), rB1

	fmpyfadd,dbl	rA0, rB2, rC02, rC02
	fmpyfadd,dbl	rA1, rB2, rC12, rC12
						fldd   	24(pA2), ra2
	fmpyfadd,dbl	rA2, rB2, rC22, rC22
	fmpyfadd,dbl	rA3, rB2, rC32, rC32
						fldd   	24(pB2), rB2

	fmpyfadd,dbl	rA0, rB3, rC03, rC03
	fmpyfadd,dbl	rA1, rB3, rC13, rC13
						fldd   	24(pA3), ra3
	fmpyfadd,dbl	rA2, rB3, rC23, rC23
	fmpyfadd,dbl	rA3, rB3, rC33, rC33
						fldd   	24(pB3), rB3

	fmpyfadd,dbl	ra0, rB0, rC00, rC00
	fmpyfadd,dbl	ra1, rB0, rC10, rC10
						fldd   	32(pA0), rA0
	fmpyfadd,dbl	ra2, rB0, rC20, rC20
	fmpyfadd,dbl	ra3, rB0, rC30, rC30
						fldd   	32(pB0), rB0

	fmpyfadd,dbl	ra0, rB1, rC01, rC01
	fmpyfadd,dbl	ra1, rB1, rC11, rC11
						fldd   	32(pA1), rA1
	fmpyfadd,dbl	ra2, rB1, rC21, rC21
	fmpyfadd,dbl	ra3, rB1, rC31, rC31
						fldd   	32(pB1), rB1

	fmpyfadd,dbl	ra0, rB2, rC02, rC02
	fmpyfadd,dbl	ra1, rB2, rC12, rC12
						fldd   	32(pA2), rA2
	fmpyfadd,dbl	ra2, rB2, rC22, rC22
	fmpyfadd,dbl	ra3, rB2, rC32, rC32
						fldd   	32(pB2), rB2

	fmpyfadd,dbl	ra0, rB3, rC03, rC03
	fmpyfadd,dbl	ra1, rB3, rC13, rC13
						fldd   	32(pA3), rA3
	fmpyfadd,dbl	ra2, rB3, rC23, rC23
	fmpyfadd,dbl	ra3, rB3, rC33, rC33
						fldd   	32(pB3), rB3

	fmpyfadd,dbl	rA0, rB0, rC00, rC00
	fmpyfadd,dbl	rA1, rB0, rC10, rC10
						fldd   	40(pA0), ra0
	fmpyfadd,dbl	rA2, rB0, rC20, rC20
	fmpyfadd,dbl	rA3, rB0, rC30, rC30
						fldd   	40(pB0), rB0

	fmpyfadd,dbl	rA0, rB1, rC01, rC01
	fmpyfadd,dbl	rA1, rB1, rC11, rC11
						fldd   	40(pA1), ra1
	fmpyfadd,dbl	rA2, rB1, rC21, rC21
	fmpyfadd,dbl	rA3, rB1, rC31, rC31
						fldd   	40(pB1), rB1

	fmpyfadd,dbl	rA0, rB2, rC02, rC02
	fmpyfadd,dbl	rA1, rB2, rC12, rC12
						fldd   	40(pA2), ra2
	fmpyfadd,dbl	rA2, rB2, rC22, rC22
	fmpyfadd,dbl	rA3, rB2, rC32, rC32
						fldd   	40(pB2), rB2

	fmpyfadd,dbl	rA0, rB3, rC03, rC03
	fmpyfadd,dbl	rA1, rB3, rC13, rC13
						fldd   	40(pA3), ra3
	fmpyfadd,dbl	rA2, rB3, rC23, rC23
	fmpyfadd,dbl	rA3, rB3, rC33, rC33
						fldd   	40(pB3), rB3

	fmpyfadd,dbl	ra0, rB0, rC00, rC00
	fmpyfadd,dbl	ra1, rB0, rC10, rC10
						fldd   	48(pA0), rA0
	fmpyfadd,dbl	ra2, rB0, rC20, rC20
	fmpyfadd,dbl	ra3, rB0, rC30, rC30
						fldd   	48(pB0), rB0

	fmpyfadd,dbl	ra0, rB1, rC01, rC01
	fmpyfadd,dbl	ra1, rB1, rC11, rC11
						fldd   	48(pA1), rA1
	fmpyfadd,dbl	ra2, rB1, rC21, rC21
	fmpyfadd,dbl	ra3, rB1, rC31, rC31
						fldd   	48(pB1), rB1

	fmpyfadd,dbl	ra0, rB2, rC02, rC02
	fmpyfadd,dbl	ra1, rB2, rC12, rC12
						fldd   	48(pA2), rA2
	fmpyfadd,dbl	ra2, rB2, rC22, rC22
	fmpyfadd,dbl	ra3, rB2, rC32, rC32
						fldd   	48(pB2), rB2

	fmpyfadd,dbl	ra0, rB3, rC03, rC03
	fmpyfadd,dbl	ra1, rB3, rC13, rC13
						fldd   	48(pA3), rA3
	fmpyfadd,dbl	ra2, rB3, rC23, rC23
	fmpyfadd,dbl	ra3, rB3, rC33, rC33
						fldd   	48(pB3), rB3

	fmpyfadd,dbl	rA0, rB0, rC00, rC00
	fmpyfadd,dbl	rA1, rB0, rC10, rC10
						fldd   	56(pA0), ra0
	fmpyfadd,dbl	rA2, rB0, rC20, rC20
	fmpyfadd,dbl	rA3, rB0, rC30, rC30
						fldd   	56(pB0), rB0

	fmpyfadd,dbl	rA0, rB1, rC01, rC01
	fmpyfadd,dbl	rA1, rB1, rC11, rC11
						fldd   	56(pA1), ra1
	fmpyfadd,dbl	rA2, rB1, rC21, rC21
	fmpyfadd,dbl	rA3, rB1, rC31, rC31
						fldd   	56(pB1), rB1

	fmpyfadd,dbl	rA0, rB2, rC02, rC02
	fmpyfadd,dbl	rA1, rB2, rC12, rC12
						fldd   	56(pA2), ra2
	fmpyfadd,dbl	rA2, rB2, rC22, rC22
	fmpyfadd,dbl	rA3, rB2, rC32, rC32
						fldd   	56(pB2), rB2

	fmpyfadd,dbl	rA0, rB3, rC03, rC03
	fmpyfadd,dbl	rA1, rB3, rC13, rC13
						fldd   	56(pA3), ra3
	fmpyfadd,dbl	rA2, rB3, rC23, rC23
	fmpyfadd,dbl	rA3, rB3, rC33, rC33
						fldd   	56(pB3), rB3

	fmpyfadd,dbl	ra0, rB0, rC00, rC00
	fmpyfadd,dbl	ra1, rB0, rC10, rC10
						fldd   	64(pA0), rA0
	fmpyfadd,dbl	ra2, rB0, rC20, rC20
	fmpyfadd,dbl	ra3, rB0, rC30, rC30
						fldd   	64(pB0), rB0

	fmpyfadd,dbl	ra0, rB1, rC01, rC01
	fmpyfadd,dbl	ra1, rB1, rC11, rC11
						fldd   	64(pA1), rA1
	fmpyfadd,dbl	ra2, rB1, rC21, rC21
	fmpyfadd,dbl	ra3, rB1, rC31, rC31
						fldd   	64(pB1), rB1

	fmpyfadd,dbl	ra0, rB2, rC02, rC02
	fmpyfadd,dbl	ra1, rB2, rC12, rC12
						fldd   	64(pA2), rA2
	fmpyfadd,dbl	ra2, rB2, rC22, rC22
	fmpyfadd,dbl	ra3, rB2, rC32, rC32
						fldd   	64(pB2), rB2

	fmpyfadd,dbl	ra0, rB3, rC03, rC03
	fmpyfadd,dbl	ra1, rB3, rC13, rC13
						fldd   	64(pA3), rA3
	fmpyfadd,dbl	ra2, rB3, rC23, rC23
	fmpyfadd,dbl	ra3, rB3, rC33, rC33
						fldd   	64(pB3), rB3

	fmpyfadd,dbl	rA0, rB0, rC00, rC00
	fmpyfadd,dbl	rA1, rB0, rC10, rC10
						fldd   	72(pA0), ra0
	fmpyfadd,dbl	rA2, rB0, rC20, rC20
	fmpyfadd,dbl	rA3, rB0, rC30, rC30
						fldd   	72(pB0), rB0

	fmpyfadd,dbl	rA0, rB1, rC01, rC01
	fmpyfadd,dbl	rA1, rB1, rC11, rC11
						fldd   	72(pA1), ra1
	fmpyfadd,dbl	rA2, rB1, rC21, rC21
	fmpyfadd,dbl	rA3, rB1, rC31, rC31
						fldd   	72(pB1), rB1

	fmpyfadd,dbl	rA0, rB2, rC02, rC02
	fmpyfadd,dbl	rA1, rB2, rC12, rC12
						fldd   	72(pA2), ra2
	fmpyfadd,dbl	rA2, rB2, rC22, rC22
	fmpyfadd,dbl	rA3, rB2, rC32, rC32
						fldd   	72(pB2), rB2

	fmpyfadd,dbl	rA0, rB3, rC03, rC03
	fmpyfadd,dbl	rA1, rB3, rC13, rC13
						fldd   	72(pA3), ra3
	fmpyfadd,dbl	rA2, rB3, rC23, rC23
	fmpyfadd,dbl	rA3, rB3, rC33, rC33
						fldd   	72(pB3), rB3

	fmpyfadd,dbl	ra0, rB0, rC00, rC00
	fmpyfadd,dbl	ra1, rB0, rC10, rC10
						fldd   	80(pA0), rA0
	fmpyfadd,dbl	ra2, rB0, rC20, rC20
	fmpyfadd,dbl	ra3, rB0, rC30, rC30
						fldd   	80(pB0), rB0

	fmpyfadd,dbl	ra0, rB1, rC01, rC01
	fmpyfadd,dbl	ra1, rB1, rC11, rC11
						fldd   	80(pA1), rA1
	fmpyfadd,dbl	ra2, rB1, rC21, rC21
	fmpyfadd,dbl	ra3, rB1, rC31, rC31
						fldd   	80(pB1), rB1

	fmpyfadd,dbl	ra0, rB2, rC02, rC02
	fmpyfadd,dbl	ra1, rB2, rC12, rC12
						fldd   	80(pA2), rA2
	fmpyfadd,dbl	ra2, rB2, rC22, rC22
	fmpyfadd,dbl	ra3, rB2, rC32, rC32
						fldd   	80(pB2), rB2

	fmpyfadd,dbl	ra0, rB3, rC03, rC03
	fmpyfadd,dbl	ra1, rB3, rC13, rC13
						fldd   	80(pA3), rA3
	fmpyfadd,dbl	ra2, rB3, rC23, rC23
	fmpyfadd,dbl	ra3, rB3, rC33, rC33
						fldd   	80(pB3), rB3

	fmpyfadd,dbl	rA0, rB0, rC00, rC00
	fmpyfadd,dbl	rA1, rB0, rC10, rC10
						fldd   	88(pA0), ra0
	fmpyfadd,dbl	rA2, rB0, rC20, rC20
	fmpyfadd,dbl	rA3, rB0, rC30, rC30
						fldd   	88(pB0), rB0

	fmpyfadd,dbl	rA0, rB1, rC01, rC01
	fmpyfadd,dbl	rA1, rB1, rC11, rC11
						fldd   	88(pA1), ra1
	fmpyfadd,dbl	rA2, rB1, rC21, rC21
	fmpyfadd,dbl	rA3, rB1, rC31, rC31
						fldd   	88(pB1), rB1

	fmpyfadd,dbl	rA0, rB2, rC02, rC02
	fmpyfadd,dbl	rA1, rB2, rC12, rC12
						fldd   	88(pA2), ra2
	fmpyfadd,dbl	rA2, rB2, rC22, rC22
	fmpyfadd,dbl	rA3, rB2, rC32, rC32
						fldd   	88(pB2), rB2

	fmpyfadd,dbl	rA0, rB3, rC03, rC03
	fmpyfadd,dbl	rA1, rB3, rC13, rC13
						fldd   	88(pA3), ra3
	fmpyfadd,dbl	rA2, rB3, rC23, rC23
	fmpyfadd,dbl	rA3, rB3, rC33, rC33
						fldd   	88(pB3), rB3

	fmpyfadd,dbl	ra0, rB0, rC00, rC00
	fmpyfadd,dbl	ra1, rB0, rC10, rC10
						fldd   	96(pA0), rA0
	fmpyfadd,dbl	ra2, rB0, rC20, rC20
	fmpyfadd,dbl	ra3, rB0, rC30, rC30
						fldd   	96(pB0), rB0

	fmpyfadd,dbl	ra0, rB1, rC01, rC01
	fmpyfadd,dbl	ra1, rB1, rC11, rC11
						fldd   	96(pA1), rA1
	fmpyfadd,dbl	ra2, rB1, rC21, rC21
	fmpyfadd,dbl	ra3, rB1, rC31, rC31
						fldd   	96(pB1), rB1

	fmpyfadd,dbl	ra0, rB2, rC02, rC02
	fmpyfadd,dbl	ra1, rB2, rC12, rC12
						fldd   	96(pA2), rA2
	fmpyfadd,dbl	ra2, rB2, rC22, rC22
	fmpyfadd,dbl	ra3, rB2, rC32, rC32
						fldd   	96(pB2), rB2

	fmpyfadd,dbl	ra0, rB3, rC03, rC03
	fmpyfadd,dbl	ra1, rB3, rC13, rC13
						fldd   	96(pA3), rA3
	fmpyfadd,dbl	ra2, rB3, rC23, rC23
	fmpyfadd,dbl	ra3, rB3, rC33, rC33
						fldd   	96(pB3), rB3

	fmpyfadd,dbl	rA0, rB0, rC00, rC00
	fmpyfadd,dbl	rA1, rB0, rC10, rC10
						fldd   	104(pA0), ra0
	fmpyfadd,dbl	rA2, rB0, rC20, rC20
	fmpyfadd,dbl	rA3, rB0, rC30, rC30
						fldd   	104(pB0), rB0

	fmpyfadd,dbl	rA0, rB1, rC01, rC01
	fmpyfadd,dbl	rA1, rB1, rC11, rC11
						fldd   	104(pA1), ra1
	fmpyfadd,dbl	rA2, rB1, rC21, rC21
	fmpyfadd,dbl	rA3, rB1, rC31, rC31
						fldd   	104(pB1), rB1

	fmpyfadd,dbl	rA0, rB2, rC02, rC02
	fmpyfadd,dbl	rA1, rB2, rC12, rC12
						fldd   	104(pA2), ra2
	fmpyfadd,dbl	rA2, rB2, rC22, rC22
	fmpyfadd,dbl	rA3, rB2, rC32, rC32
						fldd   	104(pB2), rB2

	fmpyfadd,dbl	rA0, rB3, rC03, rC03
	fmpyfadd,dbl	rA1, rB3, rC13, rC13
						fldd   	104(pA3), ra3
	fmpyfadd,dbl	rA2, rB3, rC23, rC23
	fmpyfadd,dbl	rA3, rB3, rC33, rC33
						fldd   	104(pB3), rB3

	fmpyfadd,dbl	ra0, rB0, rC00, rC00
	fmpyfadd,dbl	ra1, rB0, rC10, rC10
						fldd   	112(pA0), rA0
	fmpyfadd,dbl	ra2, rB0, rC20, rC20
	fmpyfadd,dbl	ra3, rB0, rC30, rC30
						fldd   	112(pB0), rB0

	fmpyfadd,dbl	ra0, rB1, rC01, rC01
	fmpyfadd,dbl	ra1, rB1, rC11, rC11
						fldd   	112(pA1), rA1
	fmpyfadd,dbl	ra2, rB1, rC21, rC21
	fmpyfadd,dbl	ra3, rB1, rC31, rC31
						fldd   	112(pB1), rB1

	fmpyfadd,dbl	ra0, rB2, rC02, rC02
	fmpyfadd,dbl	ra1, rB2, rC12, rC12
						fldd   	112(pA2), rA2
	fmpyfadd,dbl	ra2, rB2, rC22, rC22
	fmpyfadd,dbl	ra3, rB2, rC32, rC32
						fldd   	112(pB2), rB2

	fmpyfadd,dbl	ra0, rB3, rC03, rC03
	fmpyfadd,dbl	ra1, rB3, rC13, rC13
						fldd   	112(pA3), rA3
	fmpyfadd,dbl	ra2, rB3, rC23, rC23
	fmpyfadd,dbl	ra3, rB3, rC33, rC33
						fldd   	112(pB3), rB3

	fmpyfadd,dbl	rA0, rB0, rC00, rC00
	fmpyfadd,dbl	rA1, rB0, rC10, rC10
						fldd   	120(pA0), ra0
	fmpyfadd,dbl	rA2, rB0, rC20, rC20
	fmpyfadd,dbl	rA3, rB0, rC30, rC30
						fldd   	120(pB0), rB0

	fmpyfadd,dbl	rA0, rB1, rC01, rC01
	fmpyfadd,dbl	rA1, rB1, rC11, rC11
						fldd   	120(pA1), ra1
	fmpyfadd,dbl	rA2, rB1, rC21, rC21
	fmpyfadd,dbl	rA3, rB1, rC31, rC31
						fldd   	120(pB1), rB1

	fmpyfadd,dbl	rA0, rB2, rC02, rC02
	fmpyfadd,dbl	rA1, rB2, rC12, rC12
						fldd   	120(pA2), ra2
	fmpyfadd,dbl	rA2, rB2, rC22, rC22
	fmpyfadd,dbl	rA3, rB2, rC32, rC32
						fldd   	120(pB2), rB2

	fmpyfadd,dbl	rA0, rB3, rC03, rC03
	fmpyfadd,dbl	rA1, rB3, rC13, rC13
						fldd   	120(pA3), ra3
	fmpyfadd,dbl	rA2, rB3, rC23, rC23
	fmpyfadd,dbl	rA3, rB3, rC33, rC33
						fldd   	120(pB3), rB3

	fmpyfadd,dbl	ra0, rB0, rC00, rC00
	fmpyfadd,dbl	ra1, rB0, rC10, rC10
						fldd,mb	128(pA0), rA0
	fmpyfadd,dbl	ra2, rB0, rC20, rC20
	fmpyfadd,dbl	ra3, rB0, rC30, rC30
						fldd,mb	128(pB0), rB0

	fmpyfadd,dbl	ra0, rB1, rC01, rC01
	fmpyfadd,dbl	ra1, rB1, rC11, rC11
						fldd,mb	128(pA1), rA1
	fmpyfadd,dbl	ra2, rB1, rC21, rC21
	fmpyfadd,dbl	ra3, rB1, rC31, rC31
						fldd,mb	128(pB1), rB1

	fmpyfadd,dbl	ra0, rB2, rC02, rC02
	fmpyfadd,dbl	ra1, rB2, rC12, rC12
						fldd,mb	128(pA2), rA2
	fmpyfadd,dbl	ra2, rB2, rC22, rC22
	fmpyfadd,dbl	ra3, rB2, rC32, rC32
						fldd,mb	128(pB2), rB2

	fmpyfadd,dbl	ra0, rB3, rC03, rC03
	fmpyfadd,dbl	ra1, rB3, rC13, rC13
						fldd,mb	128(pA3), rA3
	fmpyfadd,dbl	ra2, rB3, rC23, rC23
	fmpyfadd,dbl	ra3, rB3, rC33, rC33
;
;       while (--k);
;
	addib,<>	-16, rK, KLOOP
						fldd,mb	128(pB3), rB3
#endif
;
;       Drain pipe
;

	fmpyfadd,dbl	rA0, rB0, rC00, rC00
	fmpyfadd,dbl	rA1, rB0, rC10, rC10
						fldd   	8(pA0), ra0
	fmpyfadd,dbl	rA2, rB0, rC20, rC20
	fmpyfadd,dbl	rA3, rB0, rC30, rC30
						fldd   	8(pB0), rB0

	fmpyfadd,dbl	rA0, rB1, rC01, rC01
	fmpyfadd,dbl	rA1, rB1, rC11, rC11
						fldd   	8(pA1), ra1
	fmpyfadd,dbl	rA2, rB1, rC21, rC21
	fmpyfadd,dbl	rA3, rB1, rC31, rC31
						fldd   	8(pB1), rB1

	fmpyfadd,dbl	rA0, rB2, rC02, rC02
	fmpyfadd,dbl	rA1, rB2, rC12, rC12
						fldd   	8(pA2), ra2
	fmpyfadd,dbl	rA2, rB2, rC22, rC22
	fmpyfadd,dbl	rA3, rB2, rC32, rC32
						fldd   	8(pB2), rB2

	fmpyfadd,dbl	rA0, rB3, rC03, rC03
	fmpyfadd,dbl	rA1, rB3, rC13, rC13
						fldd   	8(pA3), ra3
	fmpyfadd,dbl	rA2, rB3, rC23, rC23
	fmpyfadd,dbl	rA3, rB3, rC33, rC33
						fldd   	8(pB3), rB3

	fmpyfadd,dbl	ra0, rB0, rC00, rC00
	fmpyfadd,dbl	ra1, rB0, rC10, rC10
						fldd   	16(pA0), rA0
	fmpyfadd,dbl	ra2, rB0, rC20, rC20
	fmpyfadd,dbl	ra3, rB0, rC30, rC30
						fldd   	16(pB0), rB0

	fmpyfadd,dbl	ra0, rB1, rC01, rC01
	fmpyfadd,dbl	ra1, rB1, rC11, rC11
						fldd   	16(pA1), rA1
	fmpyfadd,dbl	ra2, rB1, rC21, rC21
	fmpyfadd,dbl	ra3, rB1, rC31, rC31
						fldd   	16(pB1), rB1

	fmpyfadd,dbl	ra0, rB2, rC02, rC02
	fmpyfadd,dbl	ra1, rB2, rC12, rC12
						fldd   	16(pA2), rA2
	fmpyfadd,dbl	ra2, rB2, rC22, rC22
	fmpyfadd,dbl	ra3, rB2, rC32, rC32
						fldd   	16(pB2), rB2

	fmpyfadd,dbl	ra0, rB3, rC03, rC03
	fmpyfadd,dbl	ra1, rB3, rC13, rC13
						fldd   	16(pA3), rA3
	fmpyfadd,dbl	ra2, rB3, rC23, rC23
	fmpyfadd,dbl	ra3, rB3, rC33, rC33
						fldd   	16(pB3), rB3

	fmpyfadd,dbl	rA0, rB0, rC00, rC00
	fmpyfadd,dbl	rA1, rB0, rC10, rC10
						fldd   	24(pA0), ra0
	fmpyfadd,dbl	rA2, rB0, rC20, rC20
	fmpyfadd,dbl	rA3, rB0, rC30, rC30
						fldd   	24(pB0), rB0

	fmpyfadd,dbl	rA0, rB1, rC01, rC01
	fmpyfadd,dbl	rA1, rB1, rC11, rC11
						fldd   	24(pA1), ra1
	fmpyfadd,dbl	rA2, rB1, rC21, rC21
	fmpyfadd,dbl	rA3, rB1, rC31, rC31
						fldd   	24(pB1), rB1

	fmpyfadd,dbl	rA0, rB2, rC02, rC02
	fmpyfadd,dbl	rA1, rB2, rC12, rC12
						fldd   	24(pA2), ra2
	fmpyfadd,dbl	rA2, rB2, rC22, rC22
	fmpyfadd,dbl	rA3, rB2, rC32, rC32
						fldd   	24(pB2), rB2

	fmpyfadd,dbl	rA0, rB3, rC03, rC03
	fmpyfadd,dbl	rA1, rB3, rC13, rC13
						fldd   	24(pA3), ra3
	fmpyfadd,dbl	rA2, rB3, rC23, rC23
	fmpyfadd,dbl	rA3, rB3, rC33, rC33
						fldd   	24(pB3), rB3

	fmpyfadd,dbl	ra0, rB0, rC00, rC00
	fmpyfadd,dbl	ra1, rB0, rC10, rC10
						fldd   	32(pA0), rA0
	fmpyfadd,dbl	ra2, rB0, rC20, rC20
	fmpyfadd,dbl	ra3, rB0, rC30, rC30
						fldd   	32(pB0), rB0

	fmpyfadd,dbl	ra0, rB1, rC01, rC01
	fmpyfadd,dbl	ra1, rB1, rC11, rC11
						fldd   	32(pA1), rA1
	fmpyfadd,dbl	ra2, rB1, rC21, rC21
	fmpyfadd,dbl	ra3, rB1, rC31, rC31
						fldd   	32(pB1), rB1

	fmpyfadd,dbl	ra0, rB2, rC02, rC02
	fmpyfadd,dbl	ra1, rB2, rC12, rC12
						fldd   	32(pA2), rA2
	fmpyfadd,dbl	ra2, rB2, rC22, rC22
	fmpyfadd,dbl	ra3, rB2, rC32, rC32
						fldd   	32(pB2), rB2

	fmpyfadd,dbl	ra0, rB3, rC03, rC03
	fmpyfadd,dbl	ra1, rB3, rC13, rC13
						fldd   	32(pA3), rA3
	fmpyfadd,dbl	ra2, rB3, rC23, rC23
	fmpyfadd,dbl	ra3, rB3, rC33, rC33
						fldd   	32(pB3), rB3

	fmpyfadd,dbl	rA0, rB0, rC00, rC00
	fmpyfadd,dbl	rA1, rB0, rC10, rC10
						fldd   	40(pA0), ra0
	fmpyfadd,dbl	rA2, rB0, rC20, rC20
	fmpyfadd,dbl	rA3, rB0, rC30, rC30
						fldd   	40(pB0), rB0

	fmpyfadd,dbl	rA0, rB1, rC01, rC01
	fmpyfadd,dbl	rA1, rB1, rC11, rC11
						fldd   	40(pA1), ra1
	fmpyfadd,dbl	rA2, rB1, rC21, rC21
	fmpyfadd,dbl	rA3, rB1, rC31, rC31
						fldd   	40(pB1), rB1

	fmpyfadd,dbl	rA0, rB2, rC02, rC02
	fmpyfadd,dbl	rA1, rB2, rC12, rC12
						fldd   	40(pA2), ra2
	fmpyfadd,dbl	rA2, rB2, rC22, rC22
	fmpyfadd,dbl	rA3, rB2, rC32, rC32
						fldd   	40(pB2), rB2

	fmpyfadd,dbl	rA0, rB3, rC03, rC03
	fmpyfadd,dbl	rA1, rB3, rC13, rC13
						fldd   	40(pA3), ra3
	fmpyfadd,dbl	rA2, rB3, rC23, rC23
	fmpyfadd,dbl	rA3, rB3, rC33, rC33
						fldd   	40(pB3), rB3

	fmpyfadd,dbl	ra0, rB0, rC00, rC00
	fmpyfadd,dbl	ra1, rB0, rC10, rC10
						fldd   	48(pA0), rA0
	fmpyfadd,dbl	ra2, rB0, rC20, rC20
	fmpyfadd,dbl	ra3, rB0, rC30, rC30
						fldd   	48(pB0), rB0

	fmpyfadd,dbl	ra0, rB1, rC01, rC01
	fmpyfadd,dbl	ra1, rB1, rC11, rC11
						fldd   	48(pA1), rA1
	fmpyfadd,dbl	ra2, rB1, rC21, rC21
	fmpyfadd,dbl	ra3, rB1, rC31, rC31
						fldd   	48(pB1), rB1

	fmpyfadd,dbl	ra0, rB2, rC02, rC02
	fmpyfadd,dbl	ra1, rB2, rC12, rC12
						fldd   	48(pA2), rA2
	fmpyfadd,dbl	ra2, rB2, rC22, rC22
	fmpyfadd,dbl	ra3, rB2, rC32, rC32
						fldd   	48(pB2), rB2

	fmpyfadd,dbl	ra0, rB3, rC03, rC03
	fmpyfadd,dbl	ra1, rB3, rC13, rC13
						fldd   	48(pA3), rA3
	fmpyfadd,dbl	ra2, rB3, rC23, rC23
	fmpyfadd,dbl	ra3, rB3, rC33, rC33
						fldd   	48(pB3), rB3

	fmpyfadd,dbl	rA0, rB0, rC00, rC00
	fmpyfadd,dbl	rA1, rB0, rC10, rC10
						fldd   	56(pA0), ra0
	fmpyfadd,dbl	rA2, rB0, rC20, rC20
	fmpyfadd,dbl	rA3, rB0, rC30, rC30
						fldd   	56(pB0), rB0

	fmpyfadd,dbl	rA0, rB1, rC01, rC01
	fmpyfadd,dbl	rA1, rB1, rC11, rC11
						fldd   	56(pA1), ra1
	fmpyfadd,dbl	rA2, rB1, rC21, rC21
	fmpyfadd,dbl	rA3, rB1, rC31, rC31
						fldd   	56(pB1), rB1

	fmpyfadd,dbl	rA0, rB2, rC02, rC02
	fmpyfadd,dbl	rA1, rB2, rC12, rC12
						fldd   	56(pA2), ra2
	fmpyfadd,dbl	rA2, rB2, rC22, rC22
	fmpyfadd,dbl	rA3, rB2, rC32, rC32
						fldd   	56(pB2), rB2

	fmpyfadd,dbl	rA0, rB3, rC03, rC03
	fmpyfadd,dbl	rA1, rB3, rC13, rC13
						fldd   	56(pA3), ra3
	fmpyfadd,dbl	rA2, rB3, rC23, rC23
	fmpyfadd,dbl	rA3, rB3, rC33, rC33
						fldd   	56(pB3), rB3

	fmpyfadd,dbl	ra0, rB0, rC00, rC00
	fmpyfadd,dbl	ra1, rB0, rC10, rC10
						fldd   	64(pA0), rA0
	fmpyfadd,dbl	ra2, rB0, rC20, rC20
	fmpyfadd,dbl	ra3, rB0, rC30, rC30
						fldd   	64(pB0), rB0

	fmpyfadd,dbl	ra0, rB1, rC01, rC01
	fmpyfadd,dbl	ra1, rB1, rC11, rC11
						fldd   	64(pA1), rA1
	fmpyfadd,dbl	ra2, rB1, rC21, rC21
	fmpyfadd,dbl	ra3, rB1, rC31, rC31
						fldd   	64(pB1), rB1

	fmpyfadd,dbl	ra0, rB2, rC02, rC02
	fmpyfadd,dbl	ra1, rB2, rC12, rC12
						fldd   	64(pA2), rA2
	fmpyfadd,dbl	ra2, rB2, rC22, rC22
	fmpyfadd,dbl	ra3, rB2, rC32, rC32
						fldd   	64(pB2), rB2

	fmpyfadd,dbl	ra0, rB3, rC03, rC03
	fmpyfadd,dbl	ra1, rB3, rC13, rC13
						fldd   	64(pA3), rA3
	fmpyfadd,dbl	ra2, rB3, rC23, rC23
	fmpyfadd,dbl	ra3, rB3, rC33, rC33
						fldd   	64(pB3), rB3

	fmpyfadd,dbl	rA0, rB0, rC00, rC00
	fmpyfadd,dbl	rA1, rB0, rC10, rC10
						fldd   	72(pA0), ra0
	fmpyfadd,dbl	rA2, rB0, rC20, rC20
	fmpyfadd,dbl	rA3, rB0, rC30, rC30
						fldd   	72(pB0), rB0

	fmpyfadd,dbl	rA0, rB1, rC01, rC01
	fmpyfadd,dbl	rA1, rB1, rC11, rC11
						fldd   	72(pA1), ra1
	fmpyfadd,dbl	rA2, rB1, rC21, rC21
	fmpyfadd,dbl	rA3, rB1, rC31, rC31
						fldd   	72(pB1), rB1

	fmpyfadd,dbl	rA0, rB2, rC02, rC02
	fmpyfadd,dbl	rA1, rB2, rC12, rC12
						fldd   	72(pA2), ra2
	fmpyfadd,dbl	rA2, rB2, rC22, rC22
	fmpyfadd,dbl	rA3, rB2, rC32, rC32
						fldd   	72(pB2), rB2

	fmpyfadd,dbl	rA0, rB3, rC03, rC03
	fmpyfadd,dbl	rA1, rB3, rC13, rC13
						fldd   	72(pA3), ra3
	fmpyfadd,dbl	rA2, rB3, rC23, rC23
	fmpyfadd,dbl	rA3, rB3, rC33, rC33
						fldd   	72(pB3), rB3

	fmpyfadd,dbl	ra0, rB0, rC00, rC00
	fmpyfadd,dbl	ra1, rB0, rC10, rC10
						fldd   	80(pA0), rA0
	fmpyfadd,dbl	ra2, rB0, rC20, rC20
	fmpyfadd,dbl	ra3, rB0, rC30, rC30
						fldd   	80(pB0), rB0

	fmpyfadd,dbl	ra0, rB1, rC01, rC01
	fmpyfadd,dbl	ra1, rB1, rC11, rC11
						fldd   	80(pA1), rA1
	fmpyfadd,dbl	ra2, rB1, rC21, rC21
	fmpyfadd,dbl	ra3, rB1, rC31, rC31
						fldd   	80(pB1), rB1

	fmpyfadd,dbl	ra0, rB2, rC02, rC02
	fmpyfadd,dbl	ra1, rB2, rC12, rC12
						fldd   	80(pA2), rA2
	fmpyfadd,dbl	ra2, rB2, rC22, rC22
	fmpyfadd,dbl	ra3, rB2, rC32, rC32
						fldd   	80(pB2), rB2

	fmpyfadd,dbl	ra0, rB3, rC03, rC03
	fmpyfadd,dbl	ra1, rB3, rC13, rC13
						fldd   	80(pA3), rA3
	fmpyfadd,dbl	ra2, rB3, rC23, rC23
	fmpyfadd,dbl	ra3, rB3, rC33, rC33
						fldd   	80(pB3), rB3

	fmpyfadd,dbl	rA0, rB0, rC00, rC00
	fmpyfadd,dbl	rA1, rB0, rC10, rC10
						fldd   	88(pA0), ra0
	fmpyfadd,dbl	rA2, rB0, rC20, rC20
	fmpyfadd,dbl	rA3, rB0, rC30, rC30
						fldd   	88(pB0), rB0

	fmpyfadd,dbl	rA0, rB1, rC01, rC01
	fmpyfadd,dbl	rA1, rB1, rC11, rC11
						fldd   	88(pA1), ra1
	fmpyfadd,dbl	rA2, rB1, rC21, rC21
	fmpyfadd,dbl	rA3, rB1, rC31, rC31
						fldd   	88(pB1), rB1

	fmpyfadd,dbl	rA0, rB2, rC02, rC02
	fmpyfadd,dbl	rA1, rB2, rC12, rC12
						fldd   	88(pA2), ra2
	fmpyfadd,dbl	rA2, rB2, rC22, rC22
	fmpyfadd,dbl	rA3, rB2, rC32, rC32
						fldd   	88(pB2), rB2

	fmpyfadd,dbl	rA0, rB3, rC03, rC03
	fmpyfadd,dbl	rA1, rB3, rC13, rC13
						fldd   	88(pA3), ra3
	fmpyfadd,dbl	rA2, rB3, rC23, rC23
	fmpyfadd,dbl	rA3, rB3, rC33, rC33
						fldd   	88(pB3), rB3

	fmpyfadd,dbl	ra0, rB0, rC00, rC00
	fmpyfadd,dbl	ra1, rB0, rC10, rC10
						fldd   	96(pA0), rA0
	fmpyfadd,dbl	ra2, rB0, rC20, rC20
	fmpyfadd,dbl	ra3, rB0, rC30, rC30
						fldd   	96(pB0), rB0

	fmpyfadd,dbl	ra0, rB1, rC01, rC01
	fmpyfadd,dbl	ra1, rB1, rC11, rC11
						fldd   	96(pA1), rA1
	fmpyfadd,dbl	ra2, rB1, rC21, rC21
	fmpyfadd,dbl	ra3, rB1, rC31, rC31
						fldd   	96(pB1), rB1

	fmpyfadd,dbl	ra0, rB2, rC02, rC02
	fmpyfadd,dbl	ra1, rB2, rC12, rC12
						fldd   	96(pA2), rA2
	fmpyfadd,dbl	ra2, rB2, rC22, rC22
	fmpyfadd,dbl	ra3, rB2, rC32, rC32
						fldd   	96(pB2), rB2

	fmpyfadd,dbl	ra0, rB3, rC03, rC03
	fmpyfadd,dbl	ra1, rB3, rC13, rC13
						fldd   	96(pA3), rA3
	fmpyfadd,dbl	ra2, rB3, rC23, rC23
	fmpyfadd,dbl	ra3, rB3, rC33, rC33
						fldd   	96(pB3), rB3

	fmpyfadd,dbl	rA0, rB0, rC00, rC00
	fmpyfadd,dbl	rA1, rB0, rC10, rC10
						fldd   	104(pA0), ra0
	fmpyfadd,dbl	rA2, rB0, rC20, rC20
	fmpyfadd,dbl	rA3, rB0, rC30, rC30
						fldd   	104(pB0), rB0

	fmpyfadd,dbl	rA0, rB1, rC01, rC01
	fmpyfadd,dbl	rA1, rB1, rC11, rC11
						fldd   	104(pA1), ra1
	fmpyfadd,dbl	rA2, rB1, rC21, rC21
	fmpyfadd,dbl	rA3, rB1, rC31, rC31
						fldd   	104(pB1), rB1

	fmpyfadd,dbl	rA0, rB2, rC02, rC02
	fmpyfadd,dbl	rA1, rB2, rC12, rC12
						fldd   	104(pA2), ra2
	fmpyfadd,dbl	rA2, rB2, rC22, rC22
	fmpyfadd,dbl	rA3, rB2, rC32, rC32
						fldd   	104(pB2), rB2

	fmpyfadd,dbl	rA0, rB3, rC03, rC03
	fmpyfadd,dbl	rA1, rB3, rC13, rC13
						fldd   	104(pA3), ra3
	fmpyfadd,dbl	rA2, rB3, rC23, rC23
	fmpyfadd,dbl	rA3, rB3, rC33, rC33
						fldd   	104(pB3), rB3

	fmpyfadd,dbl	ra0, rB0, rC00, rC00
	fmpyfadd,dbl	ra1, rB0, rC10, rC10
						fldd   	112(pA0), rA0
	fmpyfadd,dbl	ra2, rB0, rC20, rC20
	fmpyfadd,dbl	ra3, rB0, rC30, rC30
						fldd   	112(pB0), rB0

	fmpyfadd,dbl	ra0, rB1, rC01, rC01
	fmpyfadd,dbl	ra1, rB1, rC11, rC11
						fldd   	112(pA1), rA1
	fmpyfadd,dbl	ra2, rB1, rC21, rC21
	fmpyfadd,dbl	ra3, rB1, rC31, rC31
						fldd   	112(pB1), rB1

	fmpyfadd,dbl	ra0, rB2, rC02, rC02
	fmpyfadd,dbl	ra1, rB2, rC12, rC12
						fldd   	112(pA2), rA2
	fmpyfadd,dbl	ra2, rB2, rC22, rC22
	fmpyfadd,dbl	ra3, rB2, rC32, rC32
						fldd   	112(pB2), rB2

	fmpyfadd,dbl	ra0, rB3, rC03, rC03
	fmpyfadd,dbl	ra1, rB3, rC13, rC13
						fldd   	112(pA3), rA3
	fmpyfadd,dbl	ra2, rB3, rC23, rC23
	fmpyfadd,dbl	ra3, rB3, rC33, rC33
						fldd   	112(pB3), rB3

	fmpyfadd,dbl	rA0, rB0, rC00, rC00
	fmpyfadd,dbl	rA1, rB0, rC10, rC10
						fldd   	120(pA0), ra0
	fmpyfadd,dbl	rA2, rB0, rC20, rC20
	fmpyfadd,dbl	rA3, rB0, rC30, rC30
						fldd   	120(pB0), rB0

	fmpyfadd,dbl	rA0, rB1, rC01, rC01
	fmpyfadd,dbl	rA1, rB1, rC11, rC11
						fldd   	120(pA1), ra1
	fmpyfadd,dbl	rA2, rB1, rC21, rC21
	fmpyfadd,dbl	rA3, rB1, rC31, rC31
						fldd   	120(pB1), rB1

	fmpyfadd,dbl	rA0, rB2, rC02, rC02
	fmpyfadd,dbl	rA1, rB2, rC12, rC12
						fldd   	120(pA2), ra2
	fmpyfadd,dbl	rA2, rB2, rC22, rC22
	fmpyfadd,dbl	rA3, rB2, rC32, rC32
						fldd   	120(pB2), rB2

	fmpyfadd,dbl	rA0, rB3, rC03, rC03
	fmpyfadd,dbl	rA1, rB3, rC13, rC13
						fldd   	120(pA3), ra3
	fmpyfadd,dbl	rA2, rB3, rC23, rC23
	fmpyfadd,dbl	rA3, rB3, rC33, rC33
						fldd   	120(pB3), rB3

	fmpyfadd,dbl	ra0, rB0, rC00, rC00
	fmpyfadd,dbl	ra1, rB0, rC10, rC10
						ldo	8*(3*KB+16)(pA0), pA0
						ldo	8*(3*KB+16)(pA1), pA1
	fmpyfadd,dbl	ra2, rB0, rC20, rC20
	fmpyfadd,dbl	ra3, rB0, rC30, rC30
						ldo	8*(3*KB+16)(pA2), pA2
						ldo	8*(3*KB+16)(pA3), pA3

	fmpyfadd,dbl	ra0, rB1, rC01, rC01
	fmpyfadd,dbl	ra1, rB1, rC11, rC11
						ldo	-8*(KB-16)(pB0), pB0
						ldo	-8*(KB-16)(pB1), pB1
	fmpyfadd,dbl	ra2, rB1, rC21, rC21
	fmpyfadd,dbl	ra3, rB1, rC31, rC31

	fmpyfadd,dbl	ra0, rB2, rC02, rC02
	fmpyfadd,dbl	ra1, rB2, rC12, rC12
						ldo	-8*(KB-16)(pB2), pB2
	fmpyfadd,dbl	ra2, rB2, rC22, rC22
	fmpyfadd,dbl	ra3, rB2, rC32, rC32

	fmpyfadd,dbl	ra0, rB3, rC03, rC03
	fmpyfadd,dbl	ra1, rB3, rC13, rC13
						ldo	-8*(KB-16)(pB3), pB3
	fmpyfadd,dbl	ra2, rB3, rC23, rC23
	fmpyfadd,dbl	ra3, rB3, rC33, rC33
;
;       end drain KLOOP
;
;
;       Write to C
;
	fstd	rC00,  0(pC0)
	fstd	rC10,  CMUL(8)(pC0)
	fstd	rC20,  CMUL(16)(pC0)
	fstd	rC30, CMUL(24)(pC0)
					ldo	CMUL(32)(pC0), pC0
	fstd	rC01,  0(pC1)
	fstd	rC11,  CMUL(8)(pC1)
	fstd	rC21,  CMUL(16)(pC1)
	fstd	rC31, CMUL(24)(pC1)
					ldo	CMUL(32)(pC1), pC1
	fstd	rC02,  0(pC2)
	fstd	rC12,  CMUL(8)(pC2)
	fstd	rC22,  CMUL(16)(pC2)
	fstd	rC32, CMUL(24)(pC2)
					ldo 	CMUL(32)(pC2), pC2
	fstd	rC03,  0(pC3)
	fstd	rC13,  CMUL(8)(pC3)
	fstd	rC23,  CMUL(16)(pC3)
	fstd	rC33, CMUL(24)(pC3)
;
;       while (M);
;
;	ldo	-6(rM), rM
;	cmpib,<> 0, rM, MLOOP
	addib,<>	-4, rM, MLOOP
					ldo	CMUL(32)(pC3), pC3
	ldo	8*4*KB(pB0), pB0
	ldo	8*4*KB(pB1), pB1
	ldo	8*4*KB(pB2), pB2
	ldo	8*4*KB(pB3), pB3
	sub	pA0, incAn, pA0
	sub	pA1, incAn, pA1
	sub	pA2, incAn, pA2
	sub	pA3, incAn, pA3
	add	pC0, incCn, pC0
	add	pC1, incCn, pC1
	add	pC2, incCn, pC2
;
;       while (N);
;
	addib,<>	-4, rN, NLOOP
	add	pC3, incCn, pC3

/*
 *      Restore regs and return
 */
	fldd	-8(%r30), %fr12
	fldd	-16(%r30), %fr13
	fldd	-24(%r30), %fr14
	fldd	-32(%r30), %fr15
	fldd	-40(%r30), %fr16
	fldd	-48(%r30), %fr17
	fldd	-56(%r30), %fr18
	fldd	-64(%r30), %fr19
	fldd	-72(%r30), %fr20
	fldd	-80(%r30), %fr21
	ldw	-84(%r30), %r3
	ldw	-88(%r30), %r4
	ldw	-92(%r30), %r5
;	ldw	-96(%r30), %r6
;	ldw	-100(%r30), %r7
;	ldw	-104(%r30), %r8
;	ldw	-108(%r30), %r9
	ldw	-112(%r30), %r10
	ldw	-116(%r30), %r11
	ldw	-120(%r30), %r12
	ldw	-124(%r30), %r13
	bve (%r2)
	ldo	-FSIZE(%r30), %r30
	.EXIT
	.PROCEND
