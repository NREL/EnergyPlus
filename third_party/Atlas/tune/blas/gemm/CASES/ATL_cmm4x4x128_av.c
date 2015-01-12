
/*
 * This is a stupid adaptation of ATL_cmm4x4x128_av.c to complex.  I was
 * forced to add it, because the newest gcc does terrible with intrinsics
 * for some reason (8Gflop rather than 12).  In order to get something to
 * work, I simply wrote a code to do all access of C using the scalar FPU,
 * meaning that this kernel will do an extra store of vector C to memory
 * for load by the scalar FPU.
 */
#include "atlas_asm.h"

#if !defined(ATL_AS_OSX_PPC) && !defined(ATL_GAS_LINUX_PPC)
   #error "This kernel requires OS X or Linux PPC assembler!"
#endif
#if !defined(KB) || KB == 0
   #error "This kernel requires KB be a compile-time constant!"
#endif
#if KB >= 92
   #error "This kernel requires KB < 92!"
#endif
#ifndef MB
   #define MB 0
#endif
#ifdef ATL_GAS_LINUX_PPC
   #define ATL_ReadVRSAVE(r_) mfvrsave r_
   #define ATL_WriteVRSAVE(r_) mtvrsave r_
#else
   #define ATL_ReadVRSAVE(r_) mfspr r_, VRsave
   #define ATL_WriteVRSAVE(r_) mtspr VRsave, r_
#endif

#ifdef SCPLX
   #define CMUL(i_) ((i_)*2)
   #define SHF  3
   #define incCm 32
#else
   #define CMUL(i_) i_
   #define SHF  2
   #define incCm 16
#endif

#ifdef ATL_USE64BITS
   #define slwi         sldi
   #define srwi         srdi
   #define cmpwi        cmpdi
#else
   #define std  stw
   #define ld   lwz
#endif

#ifdef ATL_AS_OSX_PPC
   #define M       r3
   #define N       r4
   #define pA0     r7
   #define pB0     r9
   #define pC0     r6
   #define ldc     r8
   #define ldc2    r10
   #define ldc3    r11
   #define pfA     r12
   #define incAn   r0
   #define incCn   r5
#elif defined(ATL_USE64BITS)
   #define M       r3
   #define N       r4
   #define pA0     r7
   #define pB0     r9
   #define pC0     r10
   #define ldc     r5
   #define ldc2    r6
   #define ldc3    r8
   #define pfA     r11
   #define incAn   r0
   #define incCn   r12
#else
   #define M       r3
   #define N       r4
   #define pA0     r6
   #define pB0     r8
   #define pC0     r10
   #define ldc     r5
   #define ldc2    r7
   #define ldc3    r9
   #define pfA     r11
   #define incAn   r0
   #define incCn   r12
#endif
#define pfB     r14
#define k0      r15
#define k1      r16
#define k2      r17
#define k3      r18
#define pBETA   r19
#define pC1     r20
#define pC2     r21
#define pC3     r22

#if defined(ATL_USE64BITS)
   #define FSIZE   352
   #define FST     48
#else
   #define FSIZE   336
   #define FST     32
#endif
#define BOFF    FSIZE-16

#define vA0     v0
#define vA1     v1
#define vA2     v2
#define vA3     v3
#define vB0     v4
#define vB1     v5
#define vB2     v6
#define vB3     v7
#define va0     v8
#define va1     v9
#define va2     v10
#define va3     v11
#define vb0     v12
#define vb1     v13
#define vb2     v14
#define vb3     v15
#define vC00    v16
#define vC10    v17
#define vC20    v18
#define vC30    v19
#define vC01    v20
#define vC11    v21
#define vC21    v22
#define vC31    v23
#define vC02    v24
#define vC12    v25
#define vC22    v26
#define vC32    v27
#define vC03    v28
#define vC13    v29
#define vC23    v30
#define vC33    v31

#define rbeta f2
#define rC0   f4
#define rC1   f5
#define rC2   f6
#define rC3   f7
#define rc0   f0
#define rc1   f3
#define rc2   f8
#define rc3   f9
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
32 bit ABIs: (linux in parenthesis)
                         r3           r4           r5             r6,f1
void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha,
                (r6)       r7  (r7)       r8  (r8)       r9  (r9)      r10
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
.text
#ifdef ATL_AS_OSX_PPC
	.globl  Mjoin(_,ATL_USERMM)
Mjoin(_,ATL_USERMM):
#else
   #if defined(ATL_USE64BITS) && _CALL_ELF != 2
/*
 *      Official Program Descripter section, seg fault w/o it on Linux/PPC64
 */
        .section        ".opd","aw"
        .align 2
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
/*      Save regs */
#if defined(ATL_USE64BITS)
        stdu    r1, -FSIZE(r1)
#else
        stwu    r1, -FSIZE(r1)
#endif
        std     r14, FST(r1)
        std     r15, FST+8(r1)
        std     r16, FST+16(r1)
        std     r17, FST+24(r1)

        ATL_ReadVRSAVE(r14)
	nop
	nop
	nop
        std     r18, FST+32(r1)
        std     r19, FST+40(r1)
        std     r20, FST+48(r1)
        std     r21, FST+56(r1)

        std     r22, FST+64(r1)
        std     r23, FST+72(r1)
        std     r24, FST+80(r1)
        std     r25, FST+88(r1)
        std     r26, FST+96(r1)
        std     r14, FST+104(r1)
        li      r14, FST+112
        stvx    v20, r1, r14
        addi    r14, r14, 16
        stvx    v21, r1, r14
        addi    r14, r14, 16
        stvx    v22, r1, r14
        addi    r14, r14, 16
        stvx    v23, r1, r14
        addi    r14, r14, 16
        stvx    v24, r1, r14
        addi    r14, r14, 16
        stvx    v25, r1, r14
        addi    r14, r14, 16
        stvx    v26, r1, r14
        addi    r14, r14, 16
        stvx    v27, r1, r14
        addi    r14, r14, 16
        stvx    v28, r1, r14
        addi    r14, r14, 16
        stvx    v29, r1, r14
        addi    r14, r14, 16
        stvx    v30, r1, r14
        addi    r14, r14, 16
        stvx    v31, r1, r14
        addi    r14, r14, 16
        vxor    v0, v0, v0      /* zero v0 */
        mtvscr  v0              /* force IEEE compliance */
	addi	pBETA, r1, BOFF
        eqv     r0, r0, r0      /* all 1s */
        ATL_WriteVRSAVE(r0)     /* signal we use all vector regs */
#if defined (ATL_USE64BITS)
        ld      pC0, FSIZE+120(r1)
        ld      ldc, FSIZE+128(r1)
#elif defined(ATL_AS_OSX_PPC)
        lwz     pC0, FSIZE+60(r1)
        lwz     ldc,  FSIZE+64(r1)
#else
        lwz     ldc,  FSIZE+8(r1)
#endif
        slwi    ldc, ldc, SHF           /* ldc = ldc*sizeof */
        slwi    pfA, M, SHF             /* pfA = M*sizeof() */
        slwi    incCn, ldc, 2
        sub     incCn, incCn, pfA       /* incCn = ldc*4 - M */
        mulli   incAn, M, KB*4          /* incAn = M*KB*sizeof() */
        add     pfA, pA0, incAn         /* pfA = A + M*KB */
        srwi    M, M, 2                 /* M /= 4 */
	addi	M, M, -1
// pA0 = pA0 - incAn + KB4*4 = pA0 -(incAn - KB*4)
	mr	k1, incAn
	addi	incAn, k1, -KB4*4
	addi	incCn, incCn, incCm
        add     pC1, pC0, ldc
        add     pC2, pC1, ldc
        add     pC3, pC2, ldc
#if MB == 0
        cmpwi   cr5, M, 0
#endif
//	.align 5
NLOOPU:
        addi    pfB, pB0, KB4*4
        mtctr   M
#if MB == 0
        beq-    cr5, MPEELEDU
#endif
        xor     k0, k0, k0
        li      k1, KB*4
        li      k2, 2*KB*4
        li      k3, 3*KB*4
        lvx     vB0, 0, pB0
        lvx     vA0, 0, pA0
        lvx     vA1, pA0, k1
        lvx     vA2, pA0, k2
        lvx     vA3, pA0, k3
        lvx     vB1, pB0, k1
        lvx     vB2, pB0, k2
        lvx     vB3, pB0, k3
        vxor    vC33, vC33, vC33
#if KB > 4
        addi    k0, k0, 16
        addi    k1, k1, 16
        addi    k2, k2, 16
        addi    k3, k3, 16
        lvx     vb0, pB0, k0
        lvx     va0, pA0, k0
        lvx     va1, pA0, k1
        lvx     va2, pA0, k2
        lvx     va3, pA0, k3
        lvx     vb1, pB0, k1
        lvx     vb2, pB0, k2
        lvx     vb3, pB0, k3
#endif
#if MB == 0 || MB > 4
MLOOPU:

/* Begin KLOOPU */

#if KB > 0
      #if KB > 8
		addi	k0, k0, 16
      #endif
	vmaddfp	vC00, vA0, vB0, vC33
      #if KB > 8
		addi	k1, k1, 16
      #endif
	vmaddfp	vC10, vA1, vB0, vC33
      #if KB > 8
		addi	k2, k2, 16
      #endif
	vmaddfp	vC20, vA2, vB0, vC33
      #if KB > 8
		addi	k3, k3, 16
      #endif
	vmaddfp	vC30, vA3, vB0, vC33
      #if KB > 8
		lvx vB0, pB0, k0
      #endif
	vmaddfp	vC01, vA0, vB1, vC33
	vmaddfp	vC11, vA1, vB1, vC33
                        dcbt    0, pfA, 0
	vmaddfp	vC21, vA2, vB1, vC33
                        addi    pfA, pfA, 64
	vmaddfp	vC31, vA3, vB1, vC33
      #if KB > 8
		lvx vB1, pB0, k1
      #endif
	vmaddfp	vC02, vA0, vB2, vC33
	vmaddfp	vC12, vA1, vB2, vC33
	vmaddfp	vC22, vA2, vB2, vC33
	vmaddfp	vC32, vA3, vB2, vC33
      #if KB > 8
		lvx vB2, pB0, k2
      #endif
	vmaddfp	vC03, vA0, vB3, vC33
      #if KB > 8
		lvx vA0, pA0, k0
      #endif
	vmaddfp	vC13, vA1, vB3, vC33
      #if KB > 8
		lvx vA1, pA0, k1
      #endif
	vmaddfp	vC23, vA2, vB3, vC33
      #if KB > 8
		lvx vA2, pA0, k2
      #endif
	vmaddfp	vC33, vA3, vB3, vC33
      #if KB > 8
		lvx vA3, pA0, k3
      #endif
      #if KB > 8
		lvx vB3, pB0, k3
      #endif
#endif  /* end K=0 block */
#if KB > 4
   #if KB > 12
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 12
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 12
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 12
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 12
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 12
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 12
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 12
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 12
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 12
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 12
		lvx va3, pA0, k3
   #endif
   #if KB > 12
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=4 block */
#if KB > 8
   #if KB > 16
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 16
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 16
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 16
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 16
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 16
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 16
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 16
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 16
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 16
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 16
		lvx vA3, pA0, k3
   #endif
   #if KB > 16
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=8 block */
#if KB > 12
   #if KB > 20
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 20
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 20
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 20
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 20
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 20
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 20
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 20
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 20
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 20
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 20
		lvx va3, pA0, k3
   #endif
   #if KB > 20
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=12 block */
#if KB > 16
   #if KB > 24
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 24
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 24
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 24
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 24
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 24
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 24
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 24
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 24
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 24
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 24
		lvx vA3, pA0, k3
   #endif
   #if KB > 24
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=16 block */
#if KB > 20
   #if KB > 28
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 28
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 28
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 28
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 28
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 28
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 28
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 28
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 28
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 28
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 28
		lvx va3, pA0, k3
   #endif
   #if KB > 28
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=20 block */
#if KB > 24
   #if KB > 32
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 32
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 32
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 32
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 32
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 32
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 32
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 32
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 32
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 32
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 32
		lvx vA3, pA0, k3
   #endif
   #if KB > 32
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=24 block */
#if KB > 28
   #if KB > 36
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 36
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 36
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 36
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 36
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 36
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
                        dcbt    0, pfB, 0
	vmaddfp	vC22, va2, vb2, vC22
                        addi    pfB, pfB, 64
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 36
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 36
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 36
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 36
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 36
		lvx va3, pA0, k3
   #endif
   #if KB > 36
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=28 block */
#if KB > 32
   #if KB > 40
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 40
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 40
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 40
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 40
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 40
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 40
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 40
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 40
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 40
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 40
		lvx vA3, pA0, k3
   #endif
   #if KB > 40
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=32 block */
#if KB > 36
   #if KB > 44
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 44
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 44
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 44
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 44
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 44
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 44
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 44
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 44
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 44
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 44
		lvx va3, pA0, k3
   #endif
   #if KB > 44
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=36 block */
#if KB > 40
   #if KB > 48
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 48
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 48
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 48
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 48
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 48
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 48
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 48
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 48
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 48
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 48
		lvx vA3, pA0, k3
   #endif
   #if KB > 48
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=40 block */
#if KB > 44
   #if KB > 52
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 52
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 52
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 52
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 52
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 52
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 52
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 52
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 52
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 52
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 52
		lvx va3, pA0, k3
   #endif
   #if KB > 52
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=44 block */
#if KB > 48
   #if KB > 56
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 56
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 56
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 56
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 56
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 56
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 56
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 56
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 56
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 56
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 56
		lvx vA3, pA0, k3
   #endif
   #if KB > 56
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=48 block */
#if KB > 52
   #if KB > 60
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 60
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 60
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 60
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 60
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 60
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 60
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 60
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 60
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 60
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 60
		lvx va3, pA0, k3
   #endif
   #if KB > 60
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=52 block */
#if KB > 56
   #if KB > 64
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 64
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 64
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 64
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 64
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 64
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 64
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 64
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 64
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 64
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 64
		lvx vA3, pA0, k3
   #endif
   #if KB > 64
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=56 block */
#if KB > 60
   #if KB > 68
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 68
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 68
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 68
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 68
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 68
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 68
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 68
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 68
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 68
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 68
		lvx va3, pA0, k3
   #endif
   #if KB > 68
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=60 block */
#if KB > 64
   #if KB > 72
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 72
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 72
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 72
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 72
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 72
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 72
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 72
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 72
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 72
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 72
		lvx vA3, pA0, k3
   #endif
   #if KB > 72
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=64 block */
#if KB > 68
   #if KB > 76
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 76
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 76
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 76
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 76
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 76
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 76
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 76
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 76
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 76
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 76
		lvx va3, pA0, k3
   #endif
   #if KB > 76
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=68 block */
#if KB > 72
   #if KB > 80
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 80
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 80
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 80
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 80
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
   #ifdef BETAX
	dcbtst	0, pBETA
   #endif
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 80
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 80
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 80
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 80
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 80
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 80
		lvx vA3, pA0, k3
   #endif
   #if KB > 80
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=72 block */
#if KB > 76
   #if KB > 84
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 84
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 84
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 84
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 84
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 84
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 84
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 84
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 84
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 84
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 84
		lvx va3, pA0, k3
   #endif
   #if KB > 84
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=76 block */
#if KB > 80
   #if KB > 88
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 88
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 88
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 88
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 88
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 88
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 88
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 88
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 88
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 88
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 88
		lvx vA3, pA0, k3
   #endif
   #if KB > 88
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=80 block */
#if KB > 84
   #if KB > 92
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 92
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 92
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 92
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 92
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 92
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 92
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 92
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 92
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 92
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 92
		lvx va3, pA0, k3
   #endif
   #if KB > 92
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=84 block */
#if KB > 88  /* HERE HERE */
   #if KB > 96
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 96
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 96
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 96
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 96
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 96
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 96
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 96
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 96
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 96
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 96
		lvx vA3, pA0, k3
   #endif
   #if KB > 96
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=88 block */
#if KB > 92
   #if KB > 100
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 100
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 100
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 100
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 100
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 100
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 100
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 100
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 100
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 100
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 100
		lvx va3, pA0, k3
   #endif
   #if KB > 100
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=92 block */
#if KB > 96
   #if KB > 104
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 104
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 104
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 104
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 104
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 104
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 104
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 104
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 104
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 104
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 104
		lvx vA3, pA0, k3
   #endif
   #if KB > 104
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=96 block */
#if KB > 100
   #if KB > 108
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 108
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 108
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 108
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 108
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 108
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 108
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 108
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 108
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 108
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 108
		lvx va3, pA0, k3
   #endif
   #if KB > 108
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=100 block */
#if KB > 104
   #if KB > 112
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 112
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 112
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 112
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 112
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 112
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 112
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 112
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 112
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 112
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 112
		lvx vA3, pA0, k3
   #endif
   #if KB > 112
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=104 block */
#if KB > 108
   #if KB > 116
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 116
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 116
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 116
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 116
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 116
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 116
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 116
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 116
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 116
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 116
		lvx va3, pA0, k3
   #endif
   #if KB > 116
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=108 block */
#if KB > 112
   #if KB > 120
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 120
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 120
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 120
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 120
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 120
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 120
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 120
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 120
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 120
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 120
		lvx vA3, pA0, k3
   #endif
   #if KB > 120
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=112 block */
#if KB > 116
   #if KB > 124
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 124
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 124
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 124
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 124
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 124
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 124
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 124
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 124
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 124
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 124
		lvx va3, pA0, k3
   #endif
   #if KB > 124
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=116 block */
#if KB > 120
   #if KB > 128
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 128
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 128
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 128
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 128
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 128
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 128
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 128
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 128
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 128
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 128
		lvx vA3, pA0, k3
   #endif
   #if KB > 128
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=120 block */
#if KB > 124
   #if KB > 132
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 132
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 132
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 132
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 132
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 132
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 132
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 132
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 132
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 132
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 132
		lvx va3, pA0, k3
   #endif
   #if KB > 132
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=124 block */
/* End KLOOPU */
        vspltisb vb0, 8         /* vb0={8,8,8,8,8,8,8,8,8, 8, 8, 8, 8, 8, 8} */
        addi    pA0, pA0, KB4*4         /* pA0 += 4*lda */
       	   xor     k0, k0, k0
        vxor    vb1, vb1, vb1   /* vb1={0...0} */
       	   li      k1, KB*4
        vsldoi  vb0, vb1, vb0, 8  /* vb0={0,0,0,0,0,0,0,0,8,8,8,8,8,8,8,8} */
       	   li      k2, 2*KB*4
        lvsl    vb1, 0, r1    /*vb1={0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}*/
       	   li      k3, 3*KB*4
        vaddubm vb1, vb0, vb1 /*vb1={0,1,2,3,4,5,6,7,16,17,18,19,20,21,22,23}*/
           lvx     vB0, 0, pB0
        vaddubm vb1, vb0, vb1 /*vb1={0,1,2,3,4,5,6,7,24,25,26,27,28,29,30,31}*/
           lvx     vA0, 0, pA0

/*
 *      Reduce C vectors to scalars
 */
                                        /* rC00 = {c0a, c0b, c0c, c0d} */
                                        /* rC10 = {c1a, c1b, c1c, c1d} */
                                        /* rC20 = {c2a, c2b, c2c, c2d} */
                                        /* rC30 = {c3a, c3b, c3c, c3d} */
	vmrglw  vb2, vC00, vC10		// vb2  = {c0c, c1c, c0d, c1d}
           lvx     vA1, pA0, k1
	vmrghw  vC00, vC00, vC10	// vC00 = {c0a, c1a, c0b, c1b}
           lvx     vA2, pA0, k2
	vaddfp  vC00, vC00, vb2         // vC00 = {c0ac, c1ac, c0bd, c1bd}
           lvx     vA3, pA0, k3
	vmrglw  vb3, vC20, vC30		// vb3  = {c2c, c3c, c2d, c3d}
           lvx     vB1, pB0, k1
	vmrghw  vC20, vC20, vC30	// vC20 = {c2a, c3a, c2b, c3b}
           lvx     vB2, pB0, k2
	vaddfp  vC20, vC20, vb3         // vC20 = {c2ac, c3ac, c2bd, c3bd}
           lvx     vB3, pB0, k3
        vperm   vb2, vC00, vC20,vb1     // vb2  = {c0ac, c1ac, c2bd, c3bd}
        #if KB > 4
           addi k0, k0, 16
           addi k1, k1, 16
           addi k2, k2, 16
           addi k3, k3, 16
        #endif
        vsldoi  vC00, vC00, vC20,8      // vC00 = {c0bd, c1bd, c2ac, c3ac}
	vaddfp vC00, vC00, vb2          // vC00 = {c0acbd,c1acbd,c2acbd,c3acbd}

	vmrglw  vb2, vC01, vC11		// vb2  = {c0c, c1c, c0d, c1d}
        #if KB > 4
           lvx     va0, pA0, k0
        #endif
	vmrghw  vC01, vC01, vC11	// vC01 = {c0a, c1a, c0b, c1b}
	vaddfp  vC01, vC01, vb2         // vC01 = {c0ac, c1ac, c0bd, c1bd}
	vmrglw  vb3, vC21, vC31		// vb3  = {c2c, c3c, c2d, c3d}
	vmrghw  vC21, vC21, vC31	// vC21 = {c2a, c3a, c2b, c3b}
	vaddfp  vC21, vC21, vb3         // vC21 = {c2ac, c3ac, c2bd, c3bd}
        vperm   vb2, vC01, vC21,vb1     // vb2  = {c0ac, c1ac, c2bd, c3bd}
        vsldoi  vC01, vC01, vC21,8      // vC01 = {c0bd, c1bd, c2ac, c3ac}
	vaddfp vC01, vC01, vb2          // vC01 = {c0acbd,c1acbd,c0acbd,c1acbd}
        #if KB > 4
           lvx     vb0, pB0, k0
        #endif

	vmrglw  vb2, vC02, vC12		// vb2  = {c0c, c1c, c0d, c1d}
	vmrghw  vC02, vC02, vC12	// vC02 = {c0a, c1a, c0b, c1b}
	vaddfp  vC02, vC02, vb2         // vC02 = {c0ac, c1ac, c0bd, c1bd}
	vmrglw  vb3, vC22, vC32		// vb3  = {c2c, c3c, c2d, c3d}
	vmrghw  vC22, vC22, vC32	// vC22 = {c2a, c3a, c2b, c3b}
	vaddfp  vC22, vC22, vb3         // vC22 = {c2ac, c3ac, c2bd, c3bd}
        vperm   vb2, vC02, vC22,vb1     // vb2  = {c0ac, c1ac, c2bd, c3bd}
        vsldoi  vC02, vC02, vC22,8      // vC02 = {c0bd, c1bd, c2ac, c3ac}
	vaddfp vC02, vC02, vb2          // vC02 = {c0acbd,c1acbd,c0acbd,c1acbd}

	vmrglw  vb2, vC03, vC13		// vb2  = {c0c, c1c, c0d, c1d}
	vmrghw  vC03, vC03, vC13	// vC03 = {c0a, c1a, c0b, c1b}
	vaddfp  vC03, vC03, vb2         // vC03 = {c0ac, c1ac, c0bd, c1bd}
	vmrglw  vb3, vC23, vC33		// vb3  = {c2c, c3c, c2d, c3d}
	vmrghw  vC23, vC23, vC33	// vC23 = {c2a, c3a, c2b, c3b}
           vxor    vC33, vC33, vC33
	vaddfp  vC23, vC23, vb3         // vC23 = {c2ac, c3ac, c2bd, c3bd}
        vperm   vb2, vC03, vC23,vb1     // vb2  = {c0ac, c1ac, c2bd, c3bd}
        vsldoi  vC03, vC03, vC23,8      // vC03 = {c0bd, c1bd, c2ac, c3ac}
        #if KB > 4
           lvx     va1, pA0, k1
        #endif
	vaddfp vC03, vC03, vb2          // vC03 = {c0acbd,c1acbd,c0acbd,c1acbd}
/*
 *      Store it back out, and add it to C if necessary
 */
        stvx vC00, 0, pBETA
        lfs rC0, 0(pBETA)
        lfs rC1, 4(pBETA)
        lfs rC2, 8(pBETA)
        lfs rC3, 12(pBETA)
        #ifndef BETA0
           lfs rc0, 0(pC0)
           lfs rc1, 8(pC0)
           lfs rc2, 16(pC0)
           lfs rc3, 24(pC0)
           #ifdef BETA1
              fadd rC0, rC0, rc0
              fadd rC1, rC1, rc1
              fadd rC2, rC2, rc2
              fadd rC3, rC3, rc3
           #else
              fmadd rC0, rc0, rbeta, rC0
              fmadd rC1, rc1, rbeta, rC1
              fmadd rC2, rc2, rbeta, rC2
              fmadd rC3, rc3, rbeta, rC3
           #endif
        #endif
        stfs rC0, 0(pC0)
        stfs rC1, 8(pC0)
        stfs rC2, 16(pC0)
        stfs rC3, 24(pC0)

        stvx vC01, 0, pBETA
        lfs rC0, 0(pBETA)
        lfs rC1, 4(pBETA)
        lfs rC2, 8(pBETA)
        lfs rC3, 12(pBETA)
        #ifndef BETA0
           lfs rc0, 0(pC1)
           lfs rc1, 8(pC1)
           lfs rc2, 16(pC1)
           lfs rc3, 24(pC1)
           #ifdef BETA1
              fadd rC0, rC0, rc0
              fadd rC1, rC1, rc1
              fadd rC2, rC2, rc2
              fadd rC3, rC3, rc3
           #else
              fmadd rC0, rc0, rbeta, rC0
              fmadd rC1, rc1, rbeta, rC1
              fmadd rC2, rc2, rbeta, rC2
              fmadd rC3, rc3, rbeta, rC3
           #endif
        #endif
        stfs rC0, 0(pC1)
        stfs rC1, 8(pC1)
        stfs rC2, 16(pC1)
        stfs rC3, 24(pC1)

        stvx vC02, 0, pBETA
        lfs rC0, 0(pBETA)
        lfs rC1, 4(pBETA)
        lfs rC2, 8(pBETA)
        lfs rC3, 12(pBETA)
        #ifndef BETA0
           lfs rc0, 0(pC2)
           lfs rc1, 8(pC2)
           lfs rc2, 16(pC2)
           lfs rc3, 24(pC2)
           #ifdef BETA1
              fadd rC0, rC0, rc0
              fadd rC1, rC1, rc1
              fadd rC2, rC2, rc2
              fadd rC3, rC3, rc3
           #else
              fmadd rC0, rc0, rbeta, rC0
              fmadd rC1, rc1, rbeta, rC1
              fmadd rC2, rc2, rbeta, rC2
              fmadd rC3, rc3, rbeta, rC3
           #endif
        #endif
        stfs rC0, 0(pC2)
        stfs rC1, 8(pC2)
        stfs rC2, 16(pC2)
        stfs rC3, 24(pC2)

        #if KB > 4
           lvx     va2, pA0, k2
           nop
nop
nop
        #endif
        stvx vC03, 0, pBETA
        lfs rC0, 0(pBETA)
        lfs rC1, 4(pBETA)
        lfs rC2, 8(pBETA)
        lfs rC3, 12(pBETA)
        #ifndef BETA0
           lfs rc0, 0(pC3)
           lfs rc1, 8(pC3)
           lfs rc2, 16(pC3)
           lfs rc3, 24(pC3)
           #ifdef BETA1
              fadd rC0, rC0, rc0
              fadd rC1, rC1, rc1
              fadd rC2, rC2, rc2
              fadd rC3, rC3, rc3
           #else
              fmadd rC0, rc0, rbeta, rC0
              fmadd rC1, rc1, rbeta, rC1
              fmadd rC2, rc2, rbeta, rC2
              fmadd rC3, rc3, rbeta, rC3
           #endif
        #endif
        stfs rC0, 0(pC3)
        stfs rC1, 8(pC3)
        stfs rC2, 16(pC3)
        stfs rC3, 24(pC3)
/*
 *      Mov ptrs, while(M)
 */
        #if KB > 4
           lvx     va3, pA0, k3
           lvx     vb1, pB0, k1
           lvx     vb2, pB0, k2
           lvx     vb3, pB0, k3
        #endif
	addi	pC0, pC0, incCm
	addi	pC1, pC1, incCm
	addi	pC2, pC2, incCm
	addi	pC3, pC3, incCm
        bdnz+   MLOOPU
#endif
/*
 *      Last iteration of M-loop unrolled so we can intermix M iterations
 */

#if MB == 0
MPEELEDU:
#endif
#MLOOPU:
        xor     k0, k0, k0
        li      k1, KB*4
        li      k2, 2*KB*4
        li      k3, 3*KB*4
        lvx     vB0, 0, pB0
        lvx     vA0, 0, pA0
        lvx     vA1, pA0, k1
        lvx     vA2, pA0, k2
        lvx     vA3, pA0, k3
        lvx     vB1, pB0, k1
        lvx     vB2, pB0, k2
        lvx     vB3, pB0, k3
        vxor    vC33, vC33, vC33
#if KB > 4
        addi    k0, k0, 16
        addi    k1, k1, 16
        addi    k2, k2, 16
        addi    k3, k3, 16
        lvx     vb0, pB0, k0
        lvx     va0, pA0, k0
        lvx     va1, pA0, k1
        lvx     va2, pA0, k2
        lvx     va3, pA0, k3
        lvx     vb1, pB0, k1
        lvx     vb2, pB0, k2
        lvx     vb3, pB0, k3
#endif

/* Begin KLOOPU */

#if KB > 0
      #if KB > 8
		addi	k0, k0, 16
      #endif
	vmaddfp	vC00, vA0, vB0, vC33
      #if KB > 8
		addi	k1, k1, 16
      #endif
	vmaddfp	vC10, vA1, vB0, vC33
      #if KB > 8
		addi	k2, k2, 16
      #endif
	vmaddfp	vC20, vA2, vB0, vC33
      #if KB > 8
		addi	k3, k3, 16
      #endif
	vmaddfp	vC30, vA3, vB0, vC33
      #if KB > 8
		lvx vB0, pB0, k0
      #endif
	vmaddfp	vC01, vA0, vB1, vC33
	vmaddfp	vC11, vA1, vB1, vC33
                        dcbt    0, pfA, 0
	vmaddfp	vC21, vA2, vB1, vC33
                        addi    pfA, pfA, 64
	vmaddfp	vC31, vA3, vB1, vC33
      #if KB > 8
		lvx vB1, pB0, k1
      #endif
	vmaddfp	vC02, vA0, vB2, vC33
	vmaddfp	vC12, vA1, vB2, vC33
	vmaddfp	vC22, vA2, vB2, vC33
	vmaddfp	vC32, vA3, vB2, vC33
      #if KB > 8
		lvx vB2, pB0, k2
      #endif
	vmaddfp	vC03, vA0, vB3, vC33
      #if KB > 8
		lvx vA0, pA0, k0
      #endif
	vmaddfp	vC13, vA1, vB3, vC33
      #if KB > 8
		lvx vA1, pA0, k1
      #endif
	vmaddfp	vC23, vA2, vB3, vC33
      #if KB > 8
		lvx vA2, pA0, k2
      #endif
	vmaddfp	vC33, vA3, vB3, vC33
      #if KB > 8
		lvx vA3, pA0, k3
      #endif
      #if KB > 8
		lvx vB3, pB0, k3
      #endif
#endif  /* end K=0 block */
#if KB > 4
   #if KB > 12
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 12
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 12
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 12
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 12
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 12
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 12
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 12
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 12
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 12
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 12
		lvx va3, pA0, k3
   #endif
   #if KB > 12
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=4 block */
#if KB > 8
   #if KB > 16
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 16
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 16
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 16
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 16
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 16
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 16
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 16
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 16
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 16
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 16
		lvx vA3, pA0, k3
   #endif
   #if KB > 16
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=8 block */
#if KB > 12
   #if KB > 20
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 20
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 20
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 20
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 20
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 20
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 20
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 20
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 20
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 20
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 20
		lvx va3, pA0, k3
   #endif
   #if KB > 20
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=12 block */
#if KB > 16
   #if KB > 24
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 24
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 24
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 24
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 24
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 24
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 24
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 24
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 24
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 24
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 24
		lvx vA3, pA0, k3
   #endif
   #if KB > 24
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=16 block */
#if KB > 20
   #if KB > 28
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 28
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 28
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 28
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 28
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 28
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 28
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 28
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 28
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 28
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 28
		lvx va3, pA0, k3
   #endif
   #if KB > 28
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=20 block */
#if KB > 24
   #if KB > 32
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 32
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 32
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 32
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 32
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 32
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 32
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 32
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 32
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 32
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 32
		lvx vA3, pA0, k3
   #endif
   #if KB > 32
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=24 block */
#if KB > 28
   #if KB > 36
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 36
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 36
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 36
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 36
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 36
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
                        dcbt    0, pfB, 0
	vmaddfp	vC22, va2, vb2, vC22
                        addi    pfB, pfB, 64
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 36
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 36
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 36
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 36
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 36
		lvx va3, pA0, k3
   #endif
   #if KB > 36
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=28 block */
#if KB > 32
   #if KB > 40
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 40
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 40
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 40
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 40
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 40
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 40
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 40
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 40
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 40
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 40
		lvx vA3, pA0, k3
   #endif
   #if KB > 40
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=32 block */
#if KB > 36
   #if KB > 44
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 44
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 44
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 44
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 44
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 44
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 44
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 44
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 44
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 44
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 44
		lvx va3, pA0, k3
   #endif
   #if KB > 44
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=36 block */
#if KB > 40
   #if KB > 48
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 48
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 48
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 48
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 48
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 48
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 48
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 48
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 48
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 48
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 48
		lvx vA3, pA0, k3
   #endif
   #if KB > 48
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=40 block */
#if KB > 44
   #if KB > 52
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 52
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 52
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 52
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 52
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 52
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 52
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 52
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 52
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 52
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 52
		lvx va3, pA0, k3
   #endif
   #if KB > 52
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=44 block */
#if KB > 48
   #if KB > 56
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 56
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 56
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 56
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 56
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 56
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 56
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 56
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 56
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 56
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 56
		lvx vA3, pA0, k3
   #endif
   #if KB > 56
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=48 block */
#if KB > 52
   #if KB > 60
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 60
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 60
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 60
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 60
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 60
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 60
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 60
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 60
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 60
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 60
		lvx va3, pA0, k3
   #endif
   #if KB > 60
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=52 block */
#if KB > 56
   #if KB > 64
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 64
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 64
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 64
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 64
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 64
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 64
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 64
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 64
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 64
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 64
		lvx vA3, pA0, k3
   #endif
   #if KB > 64
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=56 block */
#if KB > 60
   #if KB > 68
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 68
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 68
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 68
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 68
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 68
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 68
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 68
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 68
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 68
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 68
		lvx va3, pA0, k3
   #endif
   #if KB > 68
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=60 block */
#if KB > 64
   #if KB > 72
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 72
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 72
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 72
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 72
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 72
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 72
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 72
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 72
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 72
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 72
		lvx vA3, pA0, k3
   #endif
   #if KB > 72
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=64 block */
#if KB > 68
   #if KB > 76
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 76
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 76
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 76
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 76
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 76
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 76
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 76
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 76
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 76
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 76
		lvx va3, pA0, k3
   #endif
   #if KB > 76
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=68 block */
#if KB > 72
   #if KB > 80
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 80
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 80
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 80
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 80
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 80
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 80
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 80
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 80
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 80
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 80
		lvx vA3, pA0, k3
   #endif
   #if KB > 80
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=72 block */
#if KB > 76
   #if KB > 84
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 84
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 84
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 84
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 84
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 84
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 84
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 84
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 84
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 84
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 84
		lvx va3, pA0, k3
   #endif
   #if KB > 84
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=76 block */
#if KB > 80
   #if KB > 88
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 88
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 88
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 88
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 88
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 88
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 88
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 88
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 88
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 88
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 88
		lvx vA3, pA0, k3
   #endif
   #if KB > 88
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=80 block */
#if KB > 84
   #if KB > 92
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 92
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 92
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 92
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 92
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 92
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 92
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 92
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 92
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 92
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 92
		lvx va3, pA0, k3
   #endif
   #if KB > 92
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=84 block */
#if KB > 88  /* HERE HERE */
   #if KB > 96
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 96
		addi	k1, k1, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 96
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 96
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 96
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 96
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 96
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 96
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 96
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 96
		lvx vA3, pA0, k3
   #endif
   #if KB > 96
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=88 block */
#if KB > 92
   #if KB > 100
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 100
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 100
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 100
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 100
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 100
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 100
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 100
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 100
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 100
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 100
		lvx va3, pA0, k3
   #endif
   #if KB > 100
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=92 block */
#if KB > 96
   #if KB > 104
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 104
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 104
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 104
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 104
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 104
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 104
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 104
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 104
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 104
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 104
		lvx vA3, pA0, k3
   #endif
   #if KB > 104
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=96 block */
#if KB > 100
   #if KB > 108
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 108
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 108
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 108
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 108
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 108
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 108
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 108
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 108
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 108
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 108
		lvx va3, pA0, k3
   #endif
   #if KB > 108
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=100 block */
#if KB > 104
   #if KB > 112
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 112
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 112
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 112
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 112
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 112
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 112
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 112
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 112
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 112
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 112
		lvx vA3, pA0, k3
   #endif
   #if KB > 112
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=104 block */
#if KB > 108
   #if KB > 116
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 116
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 116
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 116
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 116
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 116
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 116
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 116
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 116
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 116
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 116
		lvx va3, pA0, k3
   #endif
   #if KB > 116
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=108 block */
#if KB > 112
   #if KB > 120
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 120
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 120
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 120
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 120
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 120
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 120
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 120
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 120
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 120
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 120
		lvx vA3, pA0, k3
   #endif
   #if KB > 120
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=112 block */
#if KB > 116
   #if KB > 124
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 124
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 124
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 124
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 124
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 124
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 124
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 124
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 124
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 124
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 124
		lvx va3, pA0, k3
   #endif
   #if KB > 124
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=116 block */
#if KB > 120
   #if KB > 128
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, vA0, vB0, vC00
   #if KB > 128
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, vA1, vB0, vC10
   #if KB > 128
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, vA2, vB0, vC20
   #if KB > 128
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, vA3, vB0, vC30
   #if KB > 128
		lvx vB0, pB0, k0
   #endif
	vmaddfp	vC01, vA0, vB1, vC01
	vmaddfp	vC11, vA1, vB1, vC11
	vmaddfp	vC21, vA2, vB1, vC21
	vmaddfp	vC31, vA3, vB1, vC31
   #if KB > 128
		lvx vB1, pB0, k1
   #endif
	vmaddfp	vC02, vA0, vB2, vC02
	vmaddfp	vC12, vA1, vB2, vC12
	vmaddfp	vC22, vA2, vB2, vC22
	vmaddfp	vC32, vA3, vB2, vC32
   #if KB > 128
		lvx vB2, pB0, k2
   #endif
	vmaddfp	vC03, vA0, vB3, vC03
   #if KB > 128
		lvx vA0, pA0, k0
   #endif
	vmaddfp	vC13, vA1, vB3, vC13
   #if KB > 128
		lvx vA1, pA0, k1
   #endif
	vmaddfp	vC23, vA2, vB3, vC23
   #if KB > 128
		lvx vA2, pA0, k2
   #endif
	vmaddfp	vC33, vA3, vB3, vC33
   #if KB > 128
		lvx vA3, pA0, k3
   #endif
   #if KB > 128
		lvx vB3, pB0, k3
   #endif
#endif  /* end K=120 block */
#if KB > 124
   #if KB > 132
		addi	k0, k0, 16
   #endif
	vmaddfp	vC00, va0, vb0, vC00
   #if KB > 132
		addi	k1, k1, 16
   #endif
	vmaddfp	vC10, va1, vb0, vC10
   #if KB > 132
		addi	k2, k2, 16
   #endif
	vmaddfp	vC20, va2, vb0, vC20
   #if KB > 132
		addi	k3, k3, 16
   #endif
	vmaddfp	vC30, va3, vb0, vC30
   #if KB > 132
		lvx vb0, pB0, k0
   #endif
	vmaddfp	vC01, va0, vb1, vC01
	vmaddfp	vC11, va1, vb1, vC11
	vmaddfp	vC21, va2, vb1, vC21
	vmaddfp	vC31, va3, vb1, vC31
   #if KB > 132
		lvx vb1, pB0, k1
   #endif
	vmaddfp	vC02, va0, vb2, vC02
	vmaddfp	vC12, va1, vb2, vC12
	vmaddfp	vC22, va2, vb2, vC22
	vmaddfp	vC32, va3, vb2, vC32
   #if KB > 132
		lvx vb2, pB0, k2
   #endif
	vmaddfp	vC03, va0, vb3, vC03
   #if KB > 132
		lvx va0, pA0, k0
   #endif
	vmaddfp	vC13, va1, vb3, vC13
   #if KB > 132
		lvx va1, pA0, k1
   #endif
	vmaddfp	vC23, va2, vb3, vC23
   #if KB > 132
		lvx va2, pA0, k2
   #endif
	vmaddfp	vC33, va3, vb3, vC33
   #if KB > 132
		lvx va3, pA0, k3
   #endif
   #if KB > 132
		lvx vb3, pB0, k3
   #endif
#endif  /* end K=124 block */
/* End KLOOPU */
/*
 *      Reduce vectors to scalars stored vC0x
 */
        vspltisb vb0, 8         /* vb0={8,8,8,8,8,8,8,8,8, 8, 8, 8, 8, 8, 8} */
        vxor    vb1, vb1, vb1   /* vb1={0...0} */
        vsldoi  vb0, vb1, vb0, 8  /* vb0={0,0,0,0,0,0,0,0,8,8,8,8,8,8,8,8} */
        lvsl    vb1, 0, r1    /*vb1={0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15}*/
        vaddubm vb1, vb0, vb1 /*vb1={0,1,2,3,4,5,6,7,16,17,18,19,20,21,22,23}*/
        vaddubm vb1, vb0, vb1 /*vb1={0,1,2,3,4,5,6,7,24,25,26,27,28,29,30,31}*/
                                        /* rC00 = {c0a, c0b, c0c, c0d} */
                                        /* rC10 = {c1a, c1b, c1c, c1d} */
                                        /* rC20 = {c2a, c2b, c2c, c2d} */
                                        /* rC30 = {c3a, c3b, c3c, c3d} */
	vmrglw  vb2, vC00, vC10		// vb2  = {c0c, c1c, c0d, c1d}
	vmrghw  vC00, vC00, vC10	// vC00 = {c0a, c1a, c0b, c1b}
	vaddfp  vC00, vC00, vb2         // vC00 = {c0ac, c1ac, c0bd, c1bd}
	vmrglw  vb3, vC20, vC30		// vb3  = {c2c, c3c, c2d, c3d}
	vmrghw  vC20, vC20, vC30	// vC20 = {c2a, c3a, c2b, c3b}
	vaddfp  vC20, vC20, vb3         // vC20 = {c2ac, c3ac, c2bd, c3bd}
        vperm   vb2, vC00, vC20,vb1     // vb2  = {c0ac, c1ac, c2bd, c3bd}
        vsldoi  vC00, vC00, vC20,8      // vC00 = {c0bd, c1bd, c2ac, c3ac}
	vaddfp vC00, vC00, vb2          // vC00 = {c0acbd,c1acbd,c2acbd,c3acbd}

	vmrglw  vb2, vC01, vC11		// vb2  = {c0c, c1c, c0d, c1d}
	vmrghw  vC01, vC01, vC11	// vC01 = {c0a, c1a, c0b, c1b}
	vaddfp  vC01, vC01, vb2         // vC01 = {c0ac, c1ac, c0bd, c1bd}
	vmrglw  vb3, vC21, vC31		// vb3  = {c2c, c3c, c2d, c3d}
	vmrghw  vC21, vC21, vC31	// vC21 = {c2a, c3a, c2b, c3b}
	vaddfp  vC21, vC21, vb3         // vC21 = {c2ac, c3ac, c2bd, c3bd}
        vperm   vb2, vC01, vC21,vb1     // vb2  = {c0ac, c1ac, c2bd, c3bd}
        vsldoi  vC01, vC01, vC21,8      // vC01 = {c0bd, c1bd, c2ac, c3ac}
	vaddfp vC01, vC01, vb2          // vC01 = {c0acbd,c1acbd,c0acbd,c1acbd}

	vmrglw  vb2, vC02, vC12		// vb2  = {c0c, c1c, c0d, c1d}
	vmrghw  vC02, vC02, vC12	// vC02 = {c0a, c1a, c0b, c1b}
	vaddfp  vC02, vC02, vb2         // vC02 = {c0ac, c1ac, c0bd, c1bd}
	vmrglw  vb3, vC22, vC32		// vb3  = {c2c, c3c, c2d, c3d}
	vmrghw  vC22, vC22, vC32	// vC22 = {c2a, c3a, c2b, c3b}
	vaddfp  vC22, vC22, vb3         // vC22 = {c2ac, c3ac, c2bd, c3bd}
        vperm   vb2, vC02, vC22,vb1     // vb2  = {c0ac, c1ac, c2bd, c3bd}
        vsldoi  vC02, vC02, vC22,8      // vC02 = {c0bd, c1bd, c2ac, c3ac}
	vaddfp vC02, vC02, vb2          // vC02 = {c0acbd,c1acbd,c0acbd,c1acbd}

	vmrglw  vb2, vC03, vC13		// vb2  = {c0c, c1c, c0d, c1d}
	vmrghw  vC03, vC03, vC13	// vC03 = {c0a, c1a, c0b, c1b}
	vaddfp  vC03, vC03, vb2         // vC03 = {c0ac, c1ac, c0bd, c1bd}
	vmrglw  vb3, vC23, vC33		// vb3  = {c2c, c3c, c2d, c3d}
	vmrghw  vC23, vC23, vC33	// vC23 = {c2a, c3a, c2b, c3b}
	vaddfp  vC23, vC23, vb3         // vC23 = {c2ac, c3ac, c2bd, c3bd}
        vperm   vb2, vC03, vC23,vb1     // vb2  = {c0ac, c1ac, c2bd, c3bd}
        vsldoi  vC03, vC03, vC23,8      // vC03 = {c0bd, c1bd, c2ac, c3ac}
	vaddfp vC03, vC03, vb2          // vC03 = {c0acbd,c1acbd,c0acbd,c1acbd}
/*
 *      Store it back out, and add it to C if necessary
 */
        stvx vC00, 0, pBETA
        lfs rC0, 0(pBETA)
        lfs rC1, 4(pBETA)
        lfs rC2, 8(pBETA)
        lfs rC3, 12(pBETA)
        #ifndef BETA0
           lfs rc0, 0(pC0)
           lfs rc1, 8(pC0)
           lfs rc2, 16(pC0)
           lfs rc3, 24(pC0)
           #ifdef BETA1
              fadd rC0, rC0, rc0
              fadd rC1, rC1, rc1
              fadd rC2, rC2, rc2
              fadd rC3, rC3, rc3
           #else
              fmadd rC0, rc0, rbeta, rC0
              fmadd rC1, rc1, rbeta, rC1
              fmadd rC2, rc2, rbeta, rC2
              fmadd rC3, rc3, rbeta, rC3
           #endif
        #endif
        stfs rC0, 0(pC0)
        stfs rC1, 8(pC0)
        stfs rC2, 16(pC0)
        stfs rC3, 24(pC0)

        stvx vC01, 0, pBETA
        lfs rC0, 0(pBETA)
        lfs rC1, 4(pBETA)
        lfs rC2, 8(pBETA)
        lfs rC3, 12(pBETA)
        #ifndef BETA0
           lfs rc0, 0(pC1)
           lfs rc1, 8(pC1)
           lfs rc2, 16(pC1)
           lfs rc3, 24(pC1)
           #ifdef BETA1
              fadd rC0, rC0, rc0
              fadd rC1, rC1, rc1
              fadd rC2, rC2, rc2
              fadd rC3, rC3, rc3
           #else
              fmadd rC0, rc0, rbeta, rC0
              fmadd rC1, rc1, rbeta, rC1
              fmadd rC2, rc2, rbeta, rC2
              fmadd rC3, rc3, rbeta, rC3
           #endif
        #endif
        stfs rC0, 0(pC1)
        stfs rC1, 8(pC1)
        stfs rC2, 16(pC1)
        stfs rC3, 24(pC1)

        stvx vC02, 0, pBETA
        lfs rC0, 0(pBETA)
        lfs rC1, 4(pBETA)
        lfs rC2, 8(pBETA)
        lfs rC3, 12(pBETA)
        #ifndef BETA0
           lfs rc0, 0(pC2)
           lfs rc1, 8(pC2)
           lfs rc2, 16(pC2)
           lfs rc3, 24(pC2)
           #ifdef BETA1
              fadd rC0, rC0, rc0
              fadd rC1, rC1, rc1
              fadd rC2, rC2, rc2
              fadd rC3, rC3, rc3
           #else
              fmadd rC0, rc0, rbeta, rC0
              fmadd rC1, rc1, rbeta, rC1
              fmadd rC2, rc2, rbeta, rC2
              fmadd rC3, rc3, rbeta, rC3
           #endif
        #endif
        stfs rC0, 0(pC2)
        stfs rC1, 8(pC2)
        stfs rC2, 16(pC2)
        stfs rC3, 24(pC2)

        stvx vC03, 0, pBETA
        lfs rC0, 0(pBETA)
        lfs rC1, 4(pBETA)
        lfs rC2, 8(pBETA)
        lfs rC3, 12(pBETA)
        #ifndef BETA0
           lfs rc0, 0(pC3)
           lfs rc1, 8(pC3)
           lfs rc2, 16(pC3)
           lfs rc3, 24(pC3)
           #ifdef BETA1
              fadd rC0, rC0, rc0
              fadd rC1, rC1, rc1
              fadd rC2, rC2, rc2
              fadd rC3, rC3, rc3
           #else
              fmadd rC0, rc0, rbeta, rC0
              fmadd rC1, rc1, rbeta, rC1
              fmadd rC2, rc2, rbeta, rC2
              fmadd rC3, rc3, rbeta, rC3
           #endif
        #endif
        stfs rC0, 0(pC3)
        stfs rC1, 8(pC3)
        stfs rC2, 16(pC3)
        stfs rC3, 24(pC3)
/*
 *      Mov ptrs, while(M)
 */
	add	pC0, pC0, incCn
	add	pC1, pC1, incCn
	add	pC2, pC2, incCn
	add	pC3, pC3, incCn
/*
 *      Move ptrs, while(N)
 */
        sub     pA0, pA0, incAn         /* pA0 -= M*KB */
        addi    pB0, pB0, KB*4*4        /* pB0 += KB*4 */
        addic.  N, N, -4
        bne+    NLOOPU
/* DONEU: */
        ld      r14, FST(r1)
        ld      r15, FST+8(r1)
        ld      r16, FST+16(r1)
        ld      r17, FST+24(r1)
        ld      r18, FST+32(r1)
        ld      r19, FST+40(r1)
        ld      r20, FST+48(r1)
        ld      r21, FST+56(r1)
        ld      r22, FST+64(r1)
        ld      r23, FST+72(r1)
        ld      r24, FST+80(r1)
        ld      r25, FST+88(r1)
        ld      r26, FST+96(r1)
        ld      r3, FST+104(r1)
        ATL_WriteVRSAVE(r3)
        li      r3, FST+112
        lvx     v20, r1, r3
        addi    r3, r3, 16
        lvx     v21, r1, r3
        addi    r3, r3, 16
        lvx     v22, r1, r3
        addi    r3, r3, 16
        lvx     v23, r1, r3
        addi    r3, r3, 16
        lvx     v24, r1, r3
        addi    r3, r3, 16
        lvx     v25, r1, r3
        addi    r3, r3, 16
        lvx     v26, r1, r3
        addi    r3, r3, 16
        lvx     v27, r1, r3
        addi    r3, r3, 16
        lvx     v28, r1, r3
        addi    r3, r3, 16
        lvx     v29, r1, r3
        addi    r3, r3, 16
        lvx     v30, r1, r3
        addi    r3, r3, 16
        lvx     v31, r1, r3
        ld      r3, FST(r1)
        addi    r1, r1, FSIZE
        blr
