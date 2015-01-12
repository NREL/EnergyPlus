
#if !defined(ATL_LINUX_PARISC) && !defined(ATL_HPUX_PARISC)
   #error "This kernel requires gas PA-RISC assembler!"
#endif

#ifndef Mjoin
   #define Mjoin(pre, nam) my_join(pre, nam)
   #define my_join(pre, nam) pre ## nam
#endif
#ifdef SCPLX
   #define CMUL(i_) (2*(i_))
#else
   #define CMUL(i_) i_
#endif

/*
 * Integer register usage
 */
#define incCn	%r20
#define incAm	%r20
#define incAn	%r21
#define incBm	%r22
#define incBn	%r23
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
#define pB2	%r8
#define pB3	%r9
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

#define rA0a	Mjoin(rA0,L)
#define rA0b	Mjoin(rA0,R)
#define rA1a	Mjoin(rA1,L)
#define rA1b	Mjoin(rA1,R)
#define rA2a	Mjoin(rA2,L)
#define rA2b	Mjoin(rA2,R)
#define rA3a	Mjoin(rA3,L)
#define rA3b	Mjoin(rA3,R)
#define ra0a	Mjoin(ra0,L)
#define ra0b	Mjoin(ra0,R)
#define ra1a	Mjoin(ra1,L)
#define ra1b	Mjoin(ra1,R)
#define ra2a	Mjoin(ra2,L)
#define ra2b	Mjoin(ra2,R)
#define ra3a	Mjoin(ra3,L)
#define ra3b	Mjoin(ra3,R)
#define rB0a	Mjoin(rB0,L)
#define rB0b	Mjoin(rB0,R)
#define rB1a	Mjoin(rB1,L)
#define rB1b	Mjoin(rB1,R)
#define rB2a	Mjoin(rB2,L)
#define rB2b	Mjoin(rB2,R)
#define rB3a	Mjoin(rB3,L)
#define rB3b	Mjoin(rB3,R)
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
;         36           40           44                 48              52
(const int M, const int N, const int K, const float alpha, const float *A,
;          56              60             64                68        72
const int lda, const float *B, const int ldb, const float beta, float *C,
;          76
const int ldc)
*/
	ldw	-52(%r30), pA0
	ldw	-60(%r30), pB0
	ldw	-72(%r30), pC0
	ldw	-76(%r30), incCn
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
	stw	%r6, -96(%r30)
	stw	%r7, -100(%r30)
	stw	%r8, -104(%r30)
	stw	%r9, -108(%r30)
	stw	%r10, -112(%r30)
	stw	%r11, -116(%r30)
	stw	%r12, -120(%r30)
	stw	%r13, -124(%r30)
	copy	rM, rMM
;
;       incCn = ldc*sizeof
;
#ifdef SCPLX
	depw,z	incCn,28,29,incCn
#else
	depw,z	incCn,29,30,incCn
#endif
;
;       incAn = M*K*sizeof
;
	ldo	-FSIZE-36(%r30), pA1
	ldi	4*KB, incAn
	stw	rM, -4(pA1)
	stw	incAn, 0(pA1)
	fldw	0(pA1), rA0a
	fldw	-4(pA1), rA0b
	xmpyu	rA0a, rA0b, rA1
	fstw	rA1b, 0(pA1)
	ldw	0(pA1), incAn
	add	pA0, incAn, pfA
	add	pB0, incAn, pfB
;
;       Init pC[1-3] & set incCn = 4*ldc - MB
;
	add	incCn, pC0, pC1
	add	incCn, pC1, pC2
	add	incCn, pC2, pC3
	depw,z	incCn,29,30, incCn ; incCn = 4*ldc*size
#ifdef SCPLX
	depw,z	rMM, 28,29,pA1     ; pA1 = MB*size
#else
	depw,z	rMM, 29,30,pA1     ; pA1 = MB*size
#endif
	sub	incCn, pA1, incCn  ; incCn = 4*ldc*size - MB*size
;
;       Init pA[1-5] & pB[1-3]
;
	ldo	KB*4(pA0), pA1
	ldo	2*KB*4(pA0), pA2
	ldo	3*KB*4(pA0), pA3
	ldo	KB*4(pB0), pB1
	ldo	2*KB*4(pB0), pB2
	ldo	3*KB*4(pB0), pB3

NLOOP:
	copy	rMM, rM
MLOOP:
#ifdef BETA0
	fcpy,sgl	%fr0, rC00
	fcpy,sgl	%fr0, rC10
	fcpy,sgl	%fr0, rC20
	fcpy,sgl	%fr0, rC30
	fcpy,sgl	%fr0, rC01
	fcpy,sgl	%fr0, rC11
	fcpy,sgl	%fr0, rC21
	fcpy,sgl	%fr0, rC31
	fcpy,sgl	%fr0, rC02
	fcpy,sgl	%fr0, rC12
	fcpy,sgl	%fr0, rC22
	fcpy,sgl	%fr0, rC32
	fcpy,sgl	%fr0, rC03
	fcpy,sgl	%fr0, rC13
	fcpy,sgl	%fr0, rC23
	fcpy,sgl	%fr0, rC33
#else
	fldw	0(pC0), rC00
	fldw	CMUL(4)(pC0), rC10
	fldw	CMUL(8)(pC0), rC20
	fldw	CMUL(12)(pC0), rC30
	fldw	 0(pC1), rC01
	fldw	 CMUL(4)(pC1), rC11
	fldw	 CMUL(8)(pC1), rC21
	fldw	CMUL(12)(pC1), rC31
	fldw	 0(pC2), rC02
	fldw	 CMUL(4)(pC2), rC12
	fldw	 CMUL(8)(pC2), rC22
	fldw	CMUL(12)(pC2), rC32
	fldw	 0(pC3), rC03
	fldw	 CMUL(4)(pC3), rC13
	fldw	 CMUL(8)(pC3), rC23
	fldw	CMUL(12)(pC3), rC33
   #ifdef BETAX
	fldw	-FSIZE-68(%r30), ra0
	fmpy,sgl	ra0, rC00, rC00
	fmpy,sgl	ra0, rC10, rC10
	fmpy,sgl	ra0, rC20, rC20
	fmpy,sgl	ra0, rC30, rC30
	fmpy,sgl	ra0, rC01, rC01
	fmpy,sgl	ra0, rC11, rC11
	fmpy,sgl	ra0, rC21, rC21
	fmpy,sgl	ra0, rC31, rC31
	fmpy,sgl	ra0, rC02, rC02
	fmpy,sgl	ra0, rC12, rC12
	fmpy,sgl	ra0, rC22, rC22
	fmpy,sgl	ra0, rC32, rC32
	fmpy,sgl	ra0, rC03, rC03
	fmpy,sgl	ra0, rC13, rC13
	fmpy,sgl	ra0, rC23, rC23
	fmpy,sgl	ra0, rC33, rC33
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
	fmpyfadd,sgl	rA0a, rB0a, rC00, rC00
	fmpyfadd,sgl	rA1a, rB0a, rC10, rC10
	fmpyfadd,sgl	rA2a, rB0a, rC20, rC20
	fmpyfadd,sgl	rA3a, rB0a, rC30, rC30
						fldd   	8(pA0), ra0

	fmpyfadd,sgl	rA0a, rB1a, rC01, rC01
	fmpyfadd,sgl	rA1a, rB1a, rC11, rC11
	fmpyfadd,sgl	rA2a, rB1a, rC21, rC21
	fmpyfadd,sgl	rA3a, rB1a, rC31, rC31
						fldd   	8(pA1), ra1

	fmpyfadd,sgl	rA0a, rB2a, rC02, rC02
	fmpyfadd,sgl	rA1a, rB2a, rC12, rC12
	fmpyfadd,sgl	rA2a, rB2a, rC22, rC22
	fmpyfadd,sgl	rA3a, rB2a, rC32, rC32
						fldd   	8(pA2), ra2

	fmpyfadd,sgl	rA0a, rB3a, rC03, rC03
	fmpyfadd,sgl	rA1a, rB3a, rC13, rC13
	fmpyfadd,sgl	rA2a, rB3a, rC23, rC23
	fmpyfadd,sgl	rA3a, rB3a, rC33, rC33
						fldd   	8(pA3), ra3

	fmpyfadd,sgl	rA0b, rB0b, rC00, rC00
	fmpyfadd,sgl	rA1b, rB0b, rC10, rC10
	fmpyfadd,sgl	rA2b, rB0b, rC20, rC20
	fmpyfadd,sgl	rA3b, rB0b, rC30, rC30
						fldd   	8(pB0), rB0

	fmpyfadd,sgl	rA0b, rB1b, rC01, rC01
	fmpyfadd,sgl	rA1b, rB1b, rC11, rC11
	fmpyfadd,sgl	rA2b, rB1b, rC21, rC21
	fmpyfadd,sgl	rA3b, rB1b, rC31, rC31
						fldd   	8(pB1), rB1

	fmpyfadd,sgl	rA0b, rB2b, rC02, rC02
	fmpyfadd,sgl	rA1b, rB2b, rC12, rC12
	fmpyfadd,sgl	rA2b, rB2b, rC22, rC22
	fmpyfadd,sgl	rA3b, rB2b, rC32, rC32
						fldd   	8(pB2), rB2

	fmpyfadd,sgl	rA0b, rB3b, rC03, rC03
	fmpyfadd,sgl	rA1b, rB3b, rC13, rC13
	fmpyfadd,sgl	rA2b, rB3b, rC23, rC23
	fmpyfadd,sgl	rA3b, rB3b, rC33, rC33
						fldd   	8(pB3), rB3

	fmpyfadd,sgl	ra0a, rB0a, rC00, rC00
	fmpyfadd,sgl	ra1a, rB0a, rC10, rC10
	fmpyfadd,sgl	ra2a, rB0a, rC20, rC20
	fmpyfadd,sgl	ra3a, rB0a, rC30, rC30
						fldd    16(pA0), rA0

	fmpyfadd,sgl	ra0a, rB1a, rC01, rC01
	fmpyfadd,sgl	ra1a, rB1a, rC11, rC11
	fmpyfadd,sgl	ra2a, rB1a, rC21, rC21
	fmpyfadd,sgl	ra3a, rB1a, rC31, rC31
						fldd    16(pA1), rA1

	fmpyfadd,sgl	ra0a, rB2a, rC02, rC02
	fmpyfadd,sgl	ra1a, rB2a, rC12, rC12
	fmpyfadd,sgl	ra2a, rB2a, rC22, rC22
	fmpyfadd,sgl	ra3a, rB2a, rC32, rC32
						fldd    16(pA2), rA2

	fmpyfadd,sgl	ra0a, rB3a, rC03, rC03
	fmpyfadd,sgl	ra1a, rB3a, rC13, rC13
	fmpyfadd,sgl	ra2a, rB3a, rC23, rC23
	fmpyfadd,sgl	ra3a, rB3a, rC33, rC33
						fldd    16(pA3), rA3

	fmpyfadd,sgl	ra0b, rB0b, rC00, rC00
	fmpyfadd,sgl	ra1b, rB0b, rC10, rC10
	fmpyfadd,sgl	ra2b, rB0b, rC20, rC20
	fmpyfadd,sgl	ra3b, rB0b, rC30, rC30
						fldd   	16(pB0), rB0

	fmpyfadd,sgl	ra0b, rB1b, rC01, rC01
	fmpyfadd,sgl	ra1b, rB1b, rC11, rC11
	fmpyfadd,sgl	ra2b, rB1b, rC21, rC21
	fmpyfadd,sgl	ra3b, rB1b, rC31, rC31
						fldd      16(pB1), rB1

	fmpyfadd,sgl	ra0b, rB2b, rC02, rC02
	fmpyfadd,sgl	ra1b, rB2b, rC12, rC12
	fmpyfadd,sgl	ra2b, rB2b, rC22, rC22
	fmpyfadd,sgl	ra3b, rB2b, rC32, rC32
						fldd      16(pB2), rB2

	fmpyfadd,sgl	ra0b, rB3b, rC03, rC03
	fmpyfadd,sgl	ra1b, rB3b, rC13, rC13
	fmpyfadd,sgl	ra2b, rB3b, rC23, rC23
	fmpyfadd,sgl	ra3b, rB3b, rC33, rC33
						fldd    16(pB3), rB3
	fmpyfadd,sgl	rA0a, rB0a, rC00, rC00
	fmpyfadd,sgl	rA1a, rB0a, rC10, rC10
	fmpyfadd,sgl	rA2a, rB0a, rC20, rC20
	fmpyfadd,sgl	rA3a, rB0a, rC30, rC30
						fldd   	24(pA0), ra0

	fmpyfadd,sgl	rA0a, rB1a, rC01, rC01
	fmpyfadd,sgl	rA1a, rB1a, rC11, rC11
	fmpyfadd,sgl	rA2a, rB1a, rC21, rC21
	fmpyfadd,sgl	rA3a, rB1a, rC31, rC31
						fldd   	24(pA1), ra1

	fmpyfadd,sgl	rA0a, rB2a, rC02, rC02
	fmpyfadd,sgl	rA1a, rB2a, rC12, rC12
	fmpyfadd,sgl	rA2a, rB2a, rC22, rC22
	fmpyfadd,sgl	rA3a, rB2a, rC32, rC32
						fldd   	24(pA2), ra2

	fmpyfadd,sgl	rA0a, rB3a, rC03, rC03
	fmpyfadd,sgl	rA1a, rB3a, rC13, rC13
	fmpyfadd,sgl	rA2a, rB3a, rC23, rC23
	fmpyfadd,sgl	rA3a, rB3a, rC33, rC33
						fldd   	24(pA3), ra3

	fmpyfadd,sgl	rA0b, rB0b, rC00, rC00
	fmpyfadd,sgl	rA1b, rB0b, rC10, rC10
	fmpyfadd,sgl	rA2b, rB0b, rC20, rC20
	fmpyfadd,sgl	rA3b, rB0b, rC30, rC30
						fldd   	24(pB0), rB0

	fmpyfadd,sgl	rA0b, rB1b, rC01, rC01
	fmpyfadd,sgl	rA1b, rB1b, rC11, rC11
	fmpyfadd,sgl	rA2b, rB1b, rC21, rC21
	fmpyfadd,sgl	rA3b, rB1b, rC31, rC31
						fldd   	24(pB1), rB1

	fmpyfadd,sgl	rA0b, rB2b, rC02, rC02
	fmpyfadd,sgl	rA1b, rB2b, rC12, rC12
	fmpyfadd,sgl	rA2b, rB2b, rC22, rC22
	fmpyfadd,sgl	rA3b, rB2b, rC32, rC32
						fldd   	24(pB2), rB2

	fmpyfadd,sgl	rA0b, rB3b, rC03, rC03
	fmpyfadd,sgl	rA1b, rB3b, rC13, rC13
	fmpyfadd,sgl	rA2b, rB3b, rC23, rC23
	fmpyfadd,sgl	rA3b, rB3b, rC33, rC33
						fldd   	24(pB3), rB3

	fmpyfadd,sgl	ra0a, rB0a, rC00, rC00
	fmpyfadd,sgl	ra1a, rB0a, rC10, rC10
	fmpyfadd,sgl	ra2a, rB0a, rC20, rC20
	fmpyfadd,sgl	ra3a, rB0a, rC30, rC30
						fldd    32(pA0), rA0

	fmpyfadd,sgl	ra0a, rB1a, rC01, rC01
	fmpyfadd,sgl	ra1a, rB1a, rC11, rC11
	fmpyfadd,sgl	ra2a, rB1a, rC21, rC21
	fmpyfadd,sgl	ra3a, rB1a, rC31, rC31
						fldd    32(pA1), rA1

	fmpyfadd,sgl	ra0a, rB2a, rC02, rC02
	fmpyfadd,sgl	ra1a, rB2a, rC12, rC12
	fmpyfadd,sgl	ra2a, rB2a, rC22, rC22
	fmpyfadd,sgl	ra3a, rB2a, rC32, rC32
						fldd    32(pA2), rA2

	fmpyfadd,sgl	ra0a, rB3a, rC03, rC03
	fmpyfadd,sgl	ra1a, rB3a, rC13, rC13
	fmpyfadd,sgl	ra2a, rB3a, rC23, rC23
	fmpyfadd,sgl	ra3a, rB3a, rC33, rC33
						fldd    32(pA3), rA3

	fmpyfadd,sgl	ra0b, rB0b, rC00, rC00
	fmpyfadd,sgl	ra1b, rB0b, rC10, rC10
	fmpyfadd,sgl	ra2b, rB0b, rC20, rC20
	fmpyfadd,sgl	ra3b, rB0b, rC30, rC30
						fldd   	32(pB0), rB0

	fmpyfadd,sgl	ra0b, rB1b, rC01, rC01
	fmpyfadd,sgl	ra1b, rB1b, rC11, rC11
	fmpyfadd,sgl	ra2b, rB1b, rC21, rC21
	fmpyfadd,sgl	ra3b, rB1b, rC31, rC31
						fldd      32(pB1), rB1

	fmpyfadd,sgl	ra0b, rB2b, rC02, rC02
	fmpyfadd,sgl	ra1b, rB2b, rC12, rC12
	fmpyfadd,sgl	ra2b, rB2b, rC22, rC22
	fmpyfadd,sgl	ra3b, rB2b, rC32, rC32
						fldd      32(pB2), rB2

	fmpyfadd,sgl	ra0b, rB3b, rC03, rC03
	fmpyfadd,sgl	ra1b, rB3b, rC13, rC13
	fmpyfadd,sgl	ra2b, rB3b, rC23, rC23
	fmpyfadd,sgl	ra3b, rB3b, rC33, rC33
						fldd    32(pB3), rB3

	fmpyfadd,sgl	rA0a, rB0a, rC00, rC00
	fmpyfadd,sgl	rA1a, rB0a, rC10, rC10
	fmpyfadd,sgl	rA2a, rB0a, rC20, rC20
	fmpyfadd,sgl	rA3a, rB0a, rC30, rC30
						fldd   	40(pA0), ra0

	fmpyfadd,sgl	rA0a, rB1a, rC01, rC01
	fmpyfadd,sgl	rA1a, rB1a, rC11, rC11
	fmpyfadd,sgl	rA2a, rB1a, rC21, rC21
	fmpyfadd,sgl	rA3a, rB1a, rC31, rC31
						fldd   	40(pA1), ra1

	fmpyfadd,sgl	rA0a, rB2a, rC02, rC02
	fmpyfadd,sgl	rA1a, rB2a, rC12, rC12
	fmpyfadd,sgl	rA2a, rB2a, rC22, rC22
	fmpyfadd,sgl	rA3a, rB2a, rC32, rC32
						fldd   	40(pA2), ra2

	fmpyfadd,sgl	rA0a, rB3a, rC03, rC03
	fmpyfadd,sgl	rA1a, rB3a, rC13, rC13
	fmpyfadd,sgl	rA2a, rB3a, rC23, rC23
	fmpyfadd,sgl	rA3a, rB3a, rC33, rC33
						fldd   	40(pA3), ra3

	fmpyfadd,sgl	rA0b, rB0b, rC00, rC00
	fmpyfadd,sgl	rA1b, rB0b, rC10, rC10
	fmpyfadd,sgl	rA2b, rB0b, rC20, rC20
	fmpyfadd,sgl	rA3b, rB0b, rC30, rC30
						fldd   	40(pB0), rB0

	fmpyfadd,sgl	rA0b, rB1b, rC01, rC01
	fmpyfadd,sgl	rA1b, rB1b, rC11, rC11
	fmpyfadd,sgl	rA2b, rB1b, rC21, rC21
	fmpyfadd,sgl	rA3b, rB1b, rC31, rC31
						fldd   	40(pB1), rB1

	fmpyfadd,sgl	rA0b, rB2b, rC02, rC02
	fmpyfadd,sgl	rA1b, rB2b, rC12, rC12
	fmpyfadd,sgl	rA2b, rB2b, rC22, rC22
	fmpyfadd,sgl	rA3b, rB2b, rC32, rC32
						fldd   	40(pB2), rB2

	fmpyfadd,sgl	rA0b, rB3b, rC03, rC03
	fmpyfadd,sgl	rA1b, rB3b, rC13, rC13
	fmpyfadd,sgl	rA2b, rB3b, rC23, rC23
	fmpyfadd,sgl	rA3b, rB3b, rC33, rC33
						fldd   	40(pB3), rB3

	fmpyfadd,sgl	ra0a, rB0a, rC00, rC00
	fmpyfadd,sgl	ra1a, rB0a, rC10, rC10
	fmpyfadd,sgl	ra2a, rB0a, rC20, rC20
	fmpyfadd,sgl	ra3a, rB0a, rC30, rC30
						fldd    48(pA0), rA0

	fmpyfadd,sgl	ra0a, rB1a, rC01, rC01
	fmpyfadd,sgl	ra1a, rB1a, rC11, rC11
	fmpyfadd,sgl	ra2a, rB1a, rC21, rC21
	fmpyfadd,sgl	ra3a, rB1a, rC31, rC31
						fldd    48(pA1), rA1

	fmpyfadd,sgl	ra0a, rB2a, rC02, rC02
	fmpyfadd,sgl	ra1a, rB2a, rC12, rC12
	fmpyfadd,sgl	ra2a, rB2a, rC22, rC22
	fmpyfadd,sgl	ra3a, rB2a, rC32, rC32
						fldd    48(pA2), rA2

	fmpyfadd,sgl	ra0a, rB3a, rC03, rC03
	fmpyfadd,sgl	ra1a, rB3a, rC13, rC13
	fmpyfadd,sgl	ra2a, rB3a, rC23, rC23
	fmpyfadd,sgl	ra3a, rB3a, rC33, rC33
						fldd    48(pA3), rA3

	fmpyfadd,sgl	ra0b, rB0b, rC00, rC00
	fmpyfadd,sgl	ra1b, rB0b, rC10, rC10
	fmpyfadd,sgl	ra2b, rB0b, rC20, rC20
	fmpyfadd,sgl	ra3b, rB0b, rC30, rC30
						fldd   	48(pB0), rB0

	fmpyfadd,sgl	ra0b, rB1b, rC01, rC01
	fmpyfadd,sgl	ra1b, rB1b, rC11, rC11
	fmpyfadd,sgl	ra2b, rB1b, rC21, rC21
	fmpyfadd,sgl	ra3b, rB1b, rC31, rC31
						fldd      48(pB1), rB1

	fmpyfadd,sgl	ra0b, rB2b, rC02, rC02
	fmpyfadd,sgl	ra1b, rB2b, rC12, rC12
	fmpyfadd,sgl	ra2b, rB2b, rC22, rC22
	fmpyfadd,sgl	ra3b, rB2b, rC32, rC32
						fldd      48(pB2), rB2

	fmpyfadd,sgl	ra0b, rB3b, rC03, rC03
	fmpyfadd,sgl	ra1b, rB3b, rC13, rC13
	fmpyfadd,sgl	ra2b, rB3b, rC23, rC23
	fmpyfadd,sgl	ra3b, rB3b, rC33, rC33
						fldd    48(pB3), rB3
	fmpyfadd,sgl	rA0a, rB0a, rC00, rC00
	fmpyfadd,sgl	rA1a, rB0a, rC10, rC10
	fmpyfadd,sgl	rA2a, rB0a, rC20, rC20
	fmpyfadd,sgl	rA3a, rB0a, rC30, rC30
						fldd   	56(pA0), ra0

	fmpyfadd,sgl	rA0a, rB1a, rC01, rC01
	fmpyfadd,sgl	rA1a, rB1a, rC11, rC11
	fmpyfadd,sgl	rA2a, rB1a, rC21, rC21
	fmpyfadd,sgl	rA3a, rB1a, rC31, rC31
						fldd   	56(pA1), ra1

	fmpyfadd,sgl	rA0a, rB2a, rC02, rC02
	fmpyfadd,sgl	rA1a, rB2a, rC12, rC12
	fmpyfadd,sgl	rA2a, rB2a, rC22, rC22
	fmpyfadd,sgl	rA3a, rB2a, rC32, rC32
						fldd   	56(pA2), ra2

	fmpyfadd,sgl	rA0a, rB3a, rC03, rC03
	fmpyfadd,sgl	rA1a, rB3a, rC13, rC13
	fmpyfadd,sgl	rA2a, rB3a, rC23, rC23
	fmpyfadd,sgl	rA3a, rB3a, rC33, rC33
						fldd   	56(pA3), ra3

	fmpyfadd,sgl	rA0b, rB0b, rC00, rC00
	fmpyfadd,sgl	rA1b, rB0b, rC10, rC10
	fmpyfadd,sgl	rA2b, rB0b, rC20, rC20
	fmpyfadd,sgl	rA3b, rB0b, rC30, rC30
						fldd   	56(pB0), rB0

	fmpyfadd,sgl	rA0b, rB1b, rC01, rC01
	fmpyfadd,sgl	rA1b, rB1b, rC11, rC11
	fmpyfadd,sgl	rA2b, rB1b, rC21, rC21
	fmpyfadd,sgl	rA3b, rB1b, rC31, rC31
						fldd   	56(pB1), rB1

	fmpyfadd,sgl	rA0b, rB2b, rC02, rC02
	fmpyfadd,sgl	rA1b, rB2b, rC12, rC12
	fmpyfadd,sgl	rA2b, rB2b, rC22, rC22
	fmpyfadd,sgl	rA3b, rB2b, rC32, rC32
						fldd   	56(pB2), rB2

	fmpyfadd,sgl	rA0b, rB3b, rC03, rC03
	fmpyfadd,sgl	rA1b, rB3b, rC13, rC13
	fmpyfadd,sgl	rA2b, rB3b, rC23, rC23
	fmpyfadd,sgl	rA3b, rB3b, rC33, rC33
						fldd   	56(pB3), rB3

	fmpyfadd,sgl	ra0a, rB0a, rC00, rC00
	fmpyfadd,sgl	ra1a, rB0a, rC10, rC10
	fmpyfadd,sgl	ra2a, rB0a, rC20, rC20
	fmpyfadd,sgl	ra3a, rB0a, rC30, rC30
						fldd,mb 64(pA0), rA0

	fmpyfadd,sgl	ra0a, rB1a, rC01, rC01
	fmpyfadd,sgl	ra1a, rB1a, rC11, rC11
	fmpyfadd,sgl	ra2a, rB1a, rC21, rC21
	fmpyfadd,sgl	ra3a, rB1a, rC31, rC31
						fldd,mb 64(pA1), rA1

	fmpyfadd,sgl	ra0a, rB2a, rC02, rC02
	fmpyfadd,sgl	ra1a, rB2a, rC12, rC12
	fmpyfadd,sgl	ra2a, rB2a, rC22, rC22
	fmpyfadd,sgl	ra3a, rB2a, rC32, rC32
						fldd,mb 64(pA2), rA2

	fmpyfadd,sgl	ra0a, rB3a, rC03, rC03
	fmpyfadd,sgl	ra1a, rB3a, rC13, rC13
	fmpyfadd,sgl	ra2a, rB3a, rC23, rC23
	fmpyfadd,sgl	ra3a, rB3a, rC33, rC33
						fldd,mb 64(pA3), rA3

	fmpyfadd,sgl	ra0b, rB0b, rC00, rC00
	fmpyfadd,sgl	ra1b, rB0b, rC10, rC10
	fmpyfadd,sgl	ra2b, rB0b, rC20, rC20
	fmpyfadd,sgl	ra3b, rB0b, rC30, rC30
						fldd,mb	64(pB0), rB0

	fmpyfadd,sgl	ra0b, rB1b, rC01, rC01
	fmpyfadd,sgl	ra1b, rB1b, rC11, rC11
	fmpyfadd,sgl	ra2b, rB1b, rC21, rC21
	fmpyfadd,sgl	ra3b, rB1b, rC31, rC31
						fldd,mb   64(pB1), rB1

	fmpyfadd,sgl	ra0b, rB2b, rC02, rC02
	fmpyfadd,sgl	ra1b, rB2b, rC12, rC12
	fmpyfadd,sgl	ra2b, rB2b, rC22, rC22
	fmpyfadd,sgl	ra3b, rB2b, rC32, rC32
						fldd,mb   64(pB2), rB2

	fmpyfadd,sgl	ra0b, rB3b, rC03, rC03
	fmpyfadd,sgl	ra1b, rB3b, rC13, rC13
	fmpyfadd,sgl	ra2b, rB3b, rC23, rC23
	fmpyfadd,sgl	ra3b, rB3b, rC33, rC33
;
;       while (--k);
;
	addib,<>	-16, rK, KLOOP
						fldd,mb 64(pB3), rB3
;	nop
#endif
;
;       Drain pipe
;

	fmpyfadd,sgl	rA0a, rB0a, rC00, rC00
	fmpyfadd,sgl	rA1a, rB0a, rC10, rC10
	fmpyfadd,sgl	rA2a, rB0a, rC20, rC20
	fmpyfadd,sgl	rA3a, rB0a, rC30, rC30
						fldd   	8(pA0), ra0

	fmpyfadd,sgl	rA0a, rB1a, rC01, rC01
	fmpyfadd,sgl	rA1a, rB1a, rC11, rC11
	fmpyfadd,sgl	rA2a, rB1a, rC21, rC21
	fmpyfadd,sgl	rA3a, rB1a, rC31, rC31
						fldd   	8(pA1), ra1

	fmpyfadd,sgl	rA0a, rB2a, rC02, rC02
	fmpyfadd,sgl	rA1a, rB2a, rC12, rC12
	fmpyfadd,sgl	rA2a, rB2a, rC22, rC22
	fmpyfadd,sgl	rA3a, rB2a, rC32, rC32
						fldd   	8(pA2), ra2

	fmpyfadd,sgl	rA0a, rB3a, rC03, rC03
	fmpyfadd,sgl	rA1a, rB3a, rC13, rC13
	fmpyfadd,sgl	rA2a, rB3a, rC23, rC23
	fmpyfadd,sgl	rA3a, rB3a, rC33, rC33
						fldd   	8(pA3), ra3

	fmpyfadd,sgl	rA0b, rB0b, rC00, rC00
	fmpyfadd,sgl	rA1b, rB0b, rC10, rC10
	fmpyfadd,sgl	rA2b, rB0b, rC20, rC20
	fmpyfadd,sgl	rA3b, rB0b, rC30, rC30
						fldd   	8(pB0), rB0

	fmpyfadd,sgl	rA0b, rB1b, rC01, rC01
	fmpyfadd,sgl	rA1b, rB1b, rC11, rC11
	fmpyfadd,sgl	rA2b, rB1b, rC21, rC21
	fmpyfadd,sgl	rA3b, rB1b, rC31, rC31
						fldd   	8(pB1), rB1

	fmpyfadd,sgl	rA0b, rB2b, rC02, rC02
	fmpyfadd,sgl	rA1b, rB2b, rC12, rC12
	fmpyfadd,sgl	rA2b, rB2b, rC22, rC22
	fmpyfadd,sgl	rA3b, rB2b, rC32, rC32
						fldd   	8(pB2), rB2

	fmpyfadd,sgl	rA0b, rB3b, rC03, rC03
	fmpyfadd,sgl	rA1b, rB3b, rC13, rC13
	fmpyfadd,sgl	rA2b, rB3b, rC23, rC23
	fmpyfadd,sgl	rA3b, rB3b, rC33, rC33
						fldd   	8(pB3), rB3

	fmpyfadd,sgl	ra0a, rB0a, rC00, rC00
	fmpyfadd,sgl	ra1a, rB0a, rC10, rC10
	fmpyfadd,sgl	ra2a, rB0a, rC20, rC20
	fmpyfadd,sgl	ra3a, rB0a, rC30, rC30
						fldd    16(pA0), rA0

	fmpyfadd,sgl	ra0a, rB1a, rC01, rC01
	fmpyfadd,sgl	ra1a, rB1a, rC11, rC11
	fmpyfadd,sgl	ra2a, rB1a, rC21, rC21
	fmpyfadd,sgl	ra3a, rB1a, rC31, rC31
						fldd    16(pA1), rA1

	fmpyfadd,sgl	ra0a, rB2a, rC02, rC02
	fmpyfadd,sgl	ra1a, rB2a, rC12, rC12
	fmpyfadd,sgl	ra2a, rB2a, rC22, rC22
	fmpyfadd,sgl	ra3a, rB2a, rC32, rC32
						fldd    16(pA2), rA2

	fmpyfadd,sgl	ra0a, rB3a, rC03, rC03
	fmpyfadd,sgl	ra1a, rB3a, rC13, rC13
	fmpyfadd,sgl	ra2a, rB3a, rC23, rC23
	fmpyfadd,sgl	ra3a, rB3a, rC33, rC33
						fldd    16(pA3), rA3

	fmpyfadd,sgl	ra0b, rB0b, rC00, rC00
	fmpyfadd,sgl	ra1b, rB0b, rC10, rC10
	fmpyfadd,sgl	ra2b, rB0b, rC20, rC20
	fmpyfadd,sgl	ra3b, rB0b, rC30, rC30
						fldd   	16(pB0), rB0

	fmpyfadd,sgl	ra0b, rB1b, rC01, rC01
	fmpyfadd,sgl	ra1b, rB1b, rC11, rC11
	fmpyfadd,sgl	ra2b, rB1b, rC21, rC21
	fmpyfadd,sgl	ra3b, rB1b, rC31, rC31
						fldd      16(pB1), rB1

	fmpyfadd,sgl	ra0b, rB2b, rC02, rC02
	fmpyfadd,sgl	ra1b, rB2b, rC12, rC12
	fmpyfadd,sgl	ra2b, rB2b, rC22, rC22
	fmpyfadd,sgl	ra3b, rB2b, rC32, rC32
						fldd      16(pB2), rB2

	fmpyfadd,sgl	ra0b, rB3b, rC03, rC03
	fmpyfadd,sgl	ra1b, rB3b, rC13, rC13
	fmpyfadd,sgl	ra2b, rB3b, rC23, rC23
	fmpyfadd,sgl	ra3b, rB3b, rC33, rC33
						fldd    16(pB3), rB3
	fmpyfadd,sgl	rA0a, rB0a, rC00, rC00
	fmpyfadd,sgl	rA1a, rB0a, rC10, rC10
	fmpyfadd,sgl	rA2a, rB0a, rC20, rC20
	fmpyfadd,sgl	rA3a, rB0a, rC30, rC30
						fldd   	24(pA0), ra0

	fmpyfadd,sgl	rA0a, rB1a, rC01, rC01
	fmpyfadd,sgl	rA1a, rB1a, rC11, rC11
	fmpyfadd,sgl	rA2a, rB1a, rC21, rC21
	fmpyfadd,sgl	rA3a, rB1a, rC31, rC31
						fldd   	24(pA1), ra1

	fmpyfadd,sgl	rA0a, rB2a, rC02, rC02
	fmpyfadd,sgl	rA1a, rB2a, rC12, rC12
	fmpyfadd,sgl	rA2a, rB2a, rC22, rC22
	fmpyfadd,sgl	rA3a, rB2a, rC32, rC32
						fldd   	24(pA2), ra2

	fmpyfadd,sgl	rA0a, rB3a, rC03, rC03
	fmpyfadd,sgl	rA1a, rB3a, rC13, rC13
	fmpyfadd,sgl	rA2a, rB3a, rC23, rC23
	fmpyfadd,sgl	rA3a, rB3a, rC33, rC33
						fldd   	24(pA3), ra3

	fmpyfadd,sgl	rA0b, rB0b, rC00, rC00
	fmpyfadd,sgl	rA1b, rB0b, rC10, rC10
	fmpyfadd,sgl	rA2b, rB0b, rC20, rC20
	fmpyfadd,sgl	rA3b, rB0b, rC30, rC30
						fldd   	24(pB0), rB0

	fmpyfadd,sgl	rA0b, rB1b, rC01, rC01
	fmpyfadd,sgl	rA1b, rB1b, rC11, rC11
	fmpyfadd,sgl	rA2b, rB1b, rC21, rC21
	fmpyfadd,sgl	rA3b, rB1b, rC31, rC31
						fldd   	24(pB1), rB1

	fmpyfadd,sgl	rA0b, rB2b, rC02, rC02
	fmpyfadd,sgl	rA1b, rB2b, rC12, rC12
	fmpyfadd,sgl	rA2b, rB2b, rC22, rC22
	fmpyfadd,sgl	rA3b, rB2b, rC32, rC32
						fldd   	24(pB2), rB2

	fmpyfadd,sgl	rA0b, rB3b, rC03, rC03
	fmpyfadd,sgl	rA1b, rB3b, rC13, rC13
	fmpyfadd,sgl	rA2b, rB3b, rC23, rC23
	fmpyfadd,sgl	rA3b, rB3b, rC33, rC33
						fldd   	24(pB3), rB3

	fmpyfadd,sgl	ra0a, rB0a, rC00, rC00
	fmpyfadd,sgl	ra1a, rB0a, rC10, rC10
	fmpyfadd,sgl	ra2a, rB0a, rC20, rC20
	fmpyfadd,sgl	ra3a, rB0a, rC30, rC30
						fldd    32(pA0), rA0

	fmpyfadd,sgl	ra0a, rB1a, rC01, rC01
	fmpyfadd,sgl	ra1a, rB1a, rC11, rC11
	fmpyfadd,sgl	ra2a, rB1a, rC21, rC21
	fmpyfadd,sgl	ra3a, rB1a, rC31, rC31
						fldd    32(pA1), rA1

	fmpyfadd,sgl	ra0a, rB2a, rC02, rC02
	fmpyfadd,sgl	ra1a, rB2a, rC12, rC12
	fmpyfadd,sgl	ra2a, rB2a, rC22, rC22
	fmpyfadd,sgl	ra3a, rB2a, rC32, rC32
						fldd    32(pA2), rA2

	fmpyfadd,sgl	ra0a, rB3a, rC03, rC03
	fmpyfadd,sgl	ra1a, rB3a, rC13, rC13
	fmpyfadd,sgl	ra2a, rB3a, rC23, rC23
	fmpyfadd,sgl	ra3a, rB3a, rC33, rC33
						fldd    32(pA3), rA3

	fmpyfadd,sgl	ra0b, rB0b, rC00, rC00
	fmpyfadd,sgl	ra1b, rB0b, rC10, rC10
	fmpyfadd,sgl	ra2b, rB0b, rC20, rC20
	fmpyfadd,sgl	ra3b, rB0b, rC30, rC30
						fldd   	32(pB0), rB0

	fmpyfadd,sgl	ra0b, rB1b, rC01, rC01
	fmpyfadd,sgl	ra1b, rB1b, rC11, rC11
	fmpyfadd,sgl	ra2b, rB1b, rC21, rC21
	fmpyfadd,sgl	ra3b, rB1b, rC31, rC31
						fldd      32(pB1), rB1

	fmpyfadd,sgl	ra0b, rB2b, rC02, rC02
	fmpyfadd,sgl	ra1b, rB2b, rC12, rC12
	fmpyfadd,sgl	ra2b, rB2b, rC22, rC22
	fmpyfadd,sgl	ra3b, rB2b, rC32, rC32
						fldd      32(pB2), rB2

	fmpyfadd,sgl	ra0b, rB3b, rC03, rC03
	fmpyfadd,sgl	ra1b, rB3b, rC13, rC13
	fmpyfadd,sgl	ra2b, rB3b, rC23, rC23
	fmpyfadd,sgl	ra3b, rB3b, rC33, rC33
						fldd    32(pB3), rB3

	fmpyfadd,sgl	rA0a, rB0a, rC00, rC00
	fmpyfadd,sgl	rA1a, rB0a, rC10, rC10
	fmpyfadd,sgl	rA2a, rB0a, rC20, rC20
	fmpyfadd,sgl	rA3a, rB0a, rC30, rC30
						fldd   	40(pA0), ra0

	fmpyfadd,sgl	rA0a, rB1a, rC01, rC01
	fmpyfadd,sgl	rA1a, rB1a, rC11, rC11
	fmpyfadd,sgl	rA2a, rB1a, rC21, rC21
	fmpyfadd,sgl	rA3a, rB1a, rC31, rC31
						fldd   	40(pA1), ra1

	fmpyfadd,sgl	rA0a, rB2a, rC02, rC02
	fmpyfadd,sgl	rA1a, rB2a, rC12, rC12
	fmpyfadd,sgl	rA2a, rB2a, rC22, rC22
	fmpyfadd,sgl	rA3a, rB2a, rC32, rC32
						fldd   	40(pA2), ra2

	fmpyfadd,sgl	rA0a, rB3a, rC03, rC03
	fmpyfadd,sgl	rA1a, rB3a, rC13, rC13
	fmpyfadd,sgl	rA2a, rB3a, rC23, rC23
	fmpyfadd,sgl	rA3a, rB3a, rC33, rC33
						fldd   	40(pA3), ra3

	fmpyfadd,sgl	rA0b, rB0b, rC00, rC00
	fmpyfadd,sgl	rA1b, rB0b, rC10, rC10
	fmpyfadd,sgl	rA2b, rB0b, rC20, rC20
	fmpyfadd,sgl	rA3b, rB0b, rC30, rC30
						fldd   	40(pB0), rB0

	fmpyfadd,sgl	rA0b, rB1b, rC01, rC01
	fmpyfadd,sgl	rA1b, rB1b, rC11, rC11
	fmpyfadd,sgl	rA2b, rB1b, rC21, rC21
	fmpyfadd,sgl	rA3b, rB1b, rC31, rC31
						fldd   	40(pB1), rB1

	fmpyfadd,sgl	rA0b, rB2b, rC02, rC02
	fmpyfadd,sgl	rA1b, rB2b, rC12, rC12
	fmpyfadd,sgl	rA2b, rB2b, rC22, rC22
	fmpyfadd,sgl	rA3b, rB2b, rC32, rC32
						fldd   	40(pB2), rB2

	fmpyfadd,sgl	rA0b, rB3b, rC03, rC03
	fmpyfadd,sgl	rA1b, rB3b, rC13, rC13
	fmpyfadd,sgl	rA2b, rB3b, rC23, rC23
	fmpyfadd,sgl	rA3b, rB3b, rC33, rC33
						fldd   	40(pB3), rB3

	fmpyfadd,sgl	ra0a, rB0a, rC00, rC00
	fmpyfadd,sgl	ra1a, rB0a, rC10, rC10
	fmpyfadd,sgl	ra2a, rB0a, rC20, rC20
	fmpyfadd,sgl	ra3a, rB0a, rC30, rC30
						fldd    48(pA0), rA0

	fmpyfadd,sgl	ra0a, rB1a, rC01, rC01
	fmpyfadd,sgl	ra1a, rB1a, rC11, rC11
	fmpyfadd,sgl	ra2a, rB1a, rC21, rC21
	fmpyfadd,sgl	ra3a, rB1a, rC31, rC31
						fldd    48(pA1), rA1

	fmpyfadd,sgl	ra0a, rB2a, rC02, rC02
	fmpyfadd,sgl	ra1a, rB2a, rC12, rC12
	fmpyfadd,sgl	ra2a, rB2a, rC22, rC22
	fmpyfadd,sgl	ra3a, rB2a, rC32, rC32
						fldd    48(pA2), rA2

	fmpyfadd,sgl	ra0a, rB3a, rC03, rC03
	fmpyfadd,sgl	ra1a, rB3a, rC13, rC13
	fmpyfadd,sgl	ra2a, rB3a, rC23, rC23
	fmpyfadd,sgl	ra3a, rB3a, rC33, rC33
						fldd    48(pA3), rA3

	fmpyfadd,sgl	ra0b, rB0b, rC00, rC00
	fmpyfadd,sgl	ra1b, rB0b, rC10, rC10
	fmpyfadd,sgl	ra2b, rB0b, rC20, rC20
	fmpyfadd,sgl	ra3b, rB0b, rC30, rC30
						fldd   	48(pB0), rB0

	fmpyfadd,sgl	ra0b, rB1b, rC01, rC01
	fmpyfadd,sgl	ra1b, rB1b, rC11, rC11
	fmpyfadd,sgl	ra2b, rB1b, rC21, rC21
	fmpyfadd,sgl	ra3b, rB1b, rC31, rC31
						fldd      48(pB1), rB1

	fmpyfadd,sgl	ra0b, rB2b, rC02, rC02
	fmpyfadd,sgl	ra1b, rB2b, rC12, rC12
	fmpyfadd,sgl	ra2b, rB2b, rC22, rC22
	fmpyfadd,sgl	ra3b, rB2b, rC32, rC32
						fldd      48(pB2), rB2

	fmpyfadd,sgl	ra0b, rB3b, rC03, rC03
	fmpyfadd,sgl	ra1b, rB3b, rC13, rC13
	fmpyfadd,sgl	ra2b, rB3b, rC23, rC23
	fmpyfadd,sgl	ra3b, rB3b, rC33, rC33
						fldd    48(pB3), rB3
	fmpyfadd,sgl	rA0a, rB0a, rC00, rC00
	fmpyfadd,sgl	rA1a, rB0a, rC10, rC10
	fmpyfadd,sgl	rA2a, rB0a, rC20, rC20
	fmpyfadd,sgl	rA3a, rB0a, rC30, rC30
						fldd   	56(pA0), ra0

	fmpyfadd,sgl	rA0a, rB1a, rC01, rC01
	fmpyfadd,sgl	rA1a, rB1a, rC11, rC11
	fmpyfadd,sgl	rA2a, rB1a, rC21, rC21
	fmpyfadd,sgl	rA3a, rB1a, rC31, rC31
						fldd   	56(pA1), ra1

	fmpyfadd,sgl	rA0a, rB2a, rC02, rC02
	fmpyfadd,sgl	rA1a, rB2a, rC12, rC12
	fmpyfadd,sgl	rA2a, rB2a, rC22, rC22
	fmpyfadd,sgl	rA3a, rB2a, rC32, rC32
						fldd   	56(pA2), ra2

	fmpyfadd,sgl	rA0a, rB3a, rC03, rC03
	fmpyfadd,sgl	rA1a, rB3a, rC13, rC13
	fmpyfadd,sgl	rA2a, rB3a, rC23, rC23
	fmpyfadd,sgl	rA3a, rB3a, rC33, rC33
						fldd   	56(pA3), ra3

	fmpyfadd,sgl	rA0b, rB0b, rC00, rC00
	fmpyfadd,sgl	rA1b, rB0b, rC10, rC10
	fmpyfadd,sgl	rA2b, rB0b, rC20, rC20
	fmpyfadd,sgl	rA3b, rB0b, rC30, rC30
						fldd   	56(pB0), rB0

	fmpyfadd,sgl	rA0b, rB1b, rC01, rC01
	fmpyfadd,sgl	rA1b, rB1b, rC11, rC11
	fmpyfadd,sgl	rA2b, rB1b, rC21, rC21
	fmpyfadd,sgl	rA3b, rB1b, rC31, rC31
						fldd   	56(pB1), rB1

	fmpyfadd,sgl	rA0b, rB2b, rC02, rC02
	fmpyfadd,sgl	rA1b, rB2b, rC12, rC12
	fmpyfadd,sgl	rA2b, rB2b, rC22, rC22
	fmpyfadd,sgl	rA3b, rB2b, rC32, rC32
						fldd   	56(pB2), rB2

	fmpyfadd,sgl	rA0b, rB3b, rC03, rC03
	fmpyfadd,sgl	rA1b, rB3b, rC13, rC13
	fmpyfadd,sgl	rA2b, rB3b, rC23, rC23
	fmpyfadd,sgl	rA3b, rB3b, rC33, rC33
						fldd   	56(pB3), rB3

	fmpyfadd,sgl	ra0a, rB0a, rC00, rC00
	fmpyfadd,sgl	ra1a, rB0a, rC10, rC10
						ldo	4*3*KB+64(pA0), pA0
	fmpyfadd,sgl	ra2a, rB0a, rC20, rC20
	fmpyfadd,sgl	ra3a, rB0a, rC30, rC30

	fmpyfadd,sgl	ra0a, rB1a, rC01, rC01
	fmpyfadd,sgl	ra1a, rB1a, rC11, rC11
						ldo	4*3*KB+64(pA1), pA1
	fmpyfadd,sgl	ra2a, rB1a, rC21, rC21
	fmpyfadd,sgl	ra3a, rB1a, rC31, rC31
							ldw,ma	64(pfA), %r0

	fmpyfadd,sgl	ra0a, rB2a, rC02, rC02
	fmpyfadd,sgl	ra1a, rB2a, rC12, rC12
						ldo	4*3*KB+64(pA2), pA2
	fmpyfadd,sgl	ra2a, rB2a, rC22, rC22
	fmpyfadd,sgl	ra3a, rB2a, rC32, rC32

	fmpyfadd,sgl	ra0a, rB3a, rC03, rC03
	fmpyfadd,sgl	ra1a, rB3a, rC13, rC13
						ldo	4*3*KB+64(pA3), pA3
	fmpyfadd,sgl	ra2a, rB3a, rC23, rC23
	fmpyfadd,sgl	ra3a, rB3a, rC33, rC33

	fmpyfadd,sgl	ra0b, rB0b, rC00, rC00
	fmpyfadd,sgl	ra1b, rB0b, rC10, rC10
						ldo	-4*(KB-16)(pB0), pB0
	fmpyfadd,sgl	ra2b, rB0b, rC20, rC20
	fmpyfadd,sgl	ra3b, rB0b, rC30, rC30
							ldw,ma	64(pfB), %r0

	fmpyfadd,sgl	ra0b, rB1b, rC01, rC01
	fmpyfadd,sgl	ra1b, rB1b, rC11, rC11
							ldd   	64(pC0), %r0
	fmpyfadd,sgl	ra2b, rB1b, rC21, rC21
	fmpyfadd,sgl	ra3b, rB1b, rC31, rC31
							ldd   	64(pC1), %r0

	fmpyfadd,sgl	ra0b, rB2b, rC02, rC02
	fmpyfadd,sgl	ra1b, rB2b, rC12, rC12
						ldo	-4*(KB-16)(pB1), pB1
	fmpyfadd,sgl	ra2b, rB2b, rC22, rC22
	fmpyfadd,sgl	ra3b, rB2b, rC32, rC32
							ldd   	64(pC2), %r0

	fmpyfadd,sgl	ra0b, rB3b, rC03, rC03
	fmpyfadd,sgl	ra1b, rB3b, rC13, rC13
						ldo	-4*(KB-16)(pB2), pB2
	fmpyfadd,sgl	ra2b, rB3b, rC23, rC23
	fmpyfadd,sgl	ra3b, rB3b, rC33, rC33
						ldo	-4*(KB-16)(pB3), pB3
							ldd   	64(pC3), %r0
;
;       end drain KLOOP
;
;
;       Write to C
;
	fstw	rC00,  0(pC0)
	fstw	rC10,  CMUL(4)(pC0)
	fstw	rC20,  CMUL(8)(pC0)
	fstw	rC30, CMUL(12)(pC0)
					ldo	CMUL(16)(pC0), pC0
	fstw	rC01,  0(pC1)
	fstw	rC11,  CMUL(4)(pC1)
	fstw	rC21,  CMUL(8)(pC1)
	fstw	rC31, CMUL(12)(pC1)
					ldo	CMUL(16)(pC1), pC1
	fstw	rC02,  0(pC2)
	fstw	rC12,  CMUL(4)(pC2)
	fstw	rC22,  CMUL(8)(pC2)
	fstw	rC32, CMUL(12)(pC2)
					ldo 	CMUL(16)(pC2), pC2
	fstw	rC03,  0(pC3)
	fstw	rC13,  CMUL(4)(pC3)
	fstw	rC23,  CMUL(8)(pC3)
	fstw	rC33, CMUL(12)(pC3)
;
;       while (M);
;
;	ldo	-6(rM), rM
;	cmpib,<> 0, rM, MLOOP
	addib,<>	-4, rM, MLOOP
					ldo	CMUL(16)(pC3), pC3
	ldo	4*4*KB(pB0), pB0
	ldo	4*4*KB(pB1), pB1
	ldo	4*4*KB(pB2), pB2
	ldo	4*4*KB(pB3), pB3
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
	ldw	-96(%r30), %r6
	ldw	-100(%r30), %r7
	ldw	-104(%r30), %r8
	ldw	-108(%r30), %r9
	ldw	-112(%r30), %r10
	ldw	-116(%r30), %r11
	ldw	-120(%r30), %r12
	ldw	-124(%r30), %r13
	bve (%r2)
	ldo	-FSIZE(%r30), %r30
	.EXIT
	.PROCEND
