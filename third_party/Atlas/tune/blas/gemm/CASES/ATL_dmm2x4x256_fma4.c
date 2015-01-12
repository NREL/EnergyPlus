#include "atlas_asm.h"
#ifndef ATL_AVXFMA4
   #error "This kernel requires AMD's 4-operand FMAC (FM4)"
#endif
#ifdef DCPLX
   #define CMUL(arg_) 2*arg_
#else
   #define CMUL(arg_) arg_
#endif
/*
 *Integer register usage shown by these defines
 */
#define pA0     %rcx
#define lda     %rbx
#define ldb3    %rbp
#define pfA     %rdi
#define pB0     %rax
#define ldb     %rsi
#define pfB     %rdx
#define incAn   %r8
#define incCn   %r9
#define pC0     %r10
#define MM      %r11
#define NN      %r12
#define MM0     %r13
#define ldc     %r14
#define ldc3    %r15

#define rA0 	%xmm0
#define rA1 	%xmm1
#define ra0 	%xmm2
#define ra1 	%xmm3
#define rB0 	%xmm4
#define rB1 	%xmm5
#define rB2 	%xmm6
#define rB3 	%xmm7
#define rC00 	%xmm8
#define rC10 	%xmm9
#define rC01 	%xmm10
#define rC11 	%xmm11
#define rC02 	%xmm12
#define rC12 	%xmm13
#define rC03 	%xmm14
#define rC13 	%xmm15

/*
 * Save some inst space by using short version of instructions
 */
#define movapd movaps
#define movupd movups
#define movlpd movlps
#define movhpd movhps

/*
                      %rdi/4       %rsi/8       %rdx/12          %xmm0/16
 void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha,
                       %rcx/24         %r8/28         %r9/32           8/36
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                        %xmm1/40    16/48          24/52
                 const TYPE beta, TYPE *C, const int ldc)
*/
#define MOVCPD movupd
        .text
.global ATL_asmdecor(ATL_USERMM)
ALIGN16
ATL_asmdecor(ATL_USERMM):
/*
 *      Save callee-saved iregs
 */
        movq    %rbp, -8(%rsp)
        movq    %rbx, -16(%rsp)
        movq    %r12, -24(%rsp)
        movq    %r13, -32(%rsp)
        movq    %r14, -40(%rsp)
                                        prefetcht0 (pA0)
        movq    %r15, -48(%rsp)

/*
 *      Setup input parameters
 */
   #ifdef BETAX
      #define BETAOFF -72
        unpcklpd        %xmm1, %xmm1
        movapd  %xmm1, BETAOFF(%rsp)
   #endif
        movq    %rdi, MM0
        movq    %rsi, NN
        movq    %r8, lda
                                        prefetcht0      (pA0,lda)
        movq    %r9, pB0
                                        prefetcht0      (pB0)
        movslq  8(%rsp), ldb
                                        prefetcht0      (pB0,ldb)
        movq    16(%rsp), pC0
        movslq  24(%rsp), incCn
	movq	incCn, ldc
                                        prefetcht0      (pB0,lda,2)
/*
 *      incCn = (4*ldc-M)*sizeof
 */
	shl	$2, incCn
        sub     MM0, incCn
#ifdef DCPLX
        shl     $4, incCn
        shl	$4, ldc
#else
        shl     $3, incCn
        shl	$3, ldc
#endif
/*
 *      pA0 += 128; pB0 += 128
 */
        sub     $-128, pA0
        sub     $-128, pB0
                                        prefetcht0      -64(pB0)
/*
 *      ldb = ldb*sizeof;  ldb3 = ldb*3;   ldc3 = ldc*3
 */
        shl     $3, ldb
                                                prefetcht0      (pB0)
        lea     (ldb,ldb,2), ldb3
        lea     (ldc,ldc,2), ldc3
/*
 *      lda = lda*sizeof
 */
        shl     $3, lda
                                                prefetcht0      64(pB0)
/*
 *      pfA = A + lda*M ; incAn = lda*M
 */
        movq    lda, pfA
                                                prefetcht0      128(pB0)
        imulq   MM0, pfA
        movq    pfA, incAn
        lea     -128(pA0, pfA), pfA
        movq    MM0, MM
        lea     -128(pB0,ldb,2), pfB
ALIGN16
   MNLOOP:
/*
 *       K=0,1, with rCxx starting at zero
 */
         movapd -128(pB0), rB0
         xorpd  rC13, rC13
         movapd -128(pA0), rA0
         vfmaddpd rC13, rA0, rB0, rC00
         movapd -128(pB0,ldb), rB1
         vfmaddpd rC13, rA0, rB1, rC01
         movapd -128(pB0,ldb,2), rB2
         vfmaddpd rC13, rA0, rB2, rC02
         movapd -128(pB0,ldb3), rB3
         vfmaddpd rC13, rA0, rB3, rC03
         movapd -128(pA0,lda), rA1
         vfmaddpd rC13, rA1, rB0, rC10
         #if KB > 2
            movapd -112(pA0), ra0
         #endif
         vfmaddpd rC13, rA1, rB1, rC11
         #if KB > 2
            movapd -112(pA0,lda), ra1
         #endif
         vfmaddpd rC13, rA1, rB2, rC12
         #if KB > 2
            movapd -112(pB0), rB0
         #endif
         vfmaddpd rC13, rA1, rB3, rC13
         #if KB > 2
            movapd -112(pB0,ldb), rB1
         #endif
/*
 *       K=2,3
 */
        #if KB > 2
           vfmaddpd rC00, ra0, rB0, rC00
           movapd -112(pB0,ldb,2), rB2
           vfmaddpd rC10, ra1, rB0, rC10
           movapd -112(pB0,ldb3), rB3
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 4
              movapd -96(pA0), rA0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 4
              movapd -96(pA0,lda), rA1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB > 4
              movapd -96(pB0), rB0
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 4
              movapd -96(pB0,ldb), rB1
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB > 4
              movapd -96(pB0,ldb,2), rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 4
              movapd -96(pB0,ldb3), rB3
           #endif
        #endif
        #if KB <= 4
            movlpd (pC0),rB0
            movhpd CMUL(8)(pC0),rB0
            movlpd (pC0,ldc),rB1
            movhpd CMUL(8)(pC0,ldc),rB1
            movlpd (pC0,ldc,2),rB2
            movhpd CMUL(8)(pC0,ldc,2),rB2
            movlpd (pC0,ldc3),rB3
            movhpd CMUL(8)(pC0,ldc3),rB3
        #endif
        #if KB > 4
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 6
              movapd -80(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 6
              movapd -80(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 6
              movapd -80(pB0), rB0
           #elif KB == 6 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 6
              movapd -80(pB0,ldb), rB1
           #elif KB == 6 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 6
              prefetcht0 (pfB)
              add $64, pfB
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 6
              movapd -80(pB0,ldb,2), rB2
           #elif KB == 6 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 6 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 6
              movapd -80(pB0,ldb3), rB3
           #elif KB == 6 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 6
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 8
              movapd -64(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 8
              movapd -64(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 8
              movapd -64(pB0), rB0
           #elif KB == 8 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 8
              movapd -64(pB0,ldb), rB1
           #elif KB == 8 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 8
              prefetcht0 (pfB)
              add $64, pfB
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 8
              movapd -64(pB0,ldb,2), rB2
           #elif KB == 8 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 8 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 8
              movapd -64(pB0,ldb3), rB3
           #elif KB == 8 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 8
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 10
              movapd -48(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 10
              movapd -48(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 10
              movapd -48(pB0), rB0
           #elif KB == 10 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 10
              movapd -48(pB0,ldb), rB1
           #elif KB == 10 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 10 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 10
              movapd -48(pB0,ldb,2), rB2
           #elif KB == 10 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 10 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 10
              movapd -48(pB0,ldb3), rB3
           #elif KB == 10 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 10
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 12
              movapd -32(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 12
              movapd -32(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 12
              movapd -32(pB0), rB0
           #elif KB == 12 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 12
              movapd -32(pB0,ldb), rB1
           #elif KB == 12 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 12 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 12
              movapd -32(pB0,ldb,2), rB2
           #elif KB == 12 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 12 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 12
              movapd -32(pB0,ldb3), rB3
           #elif KB == 12 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 12
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 14
              movapd -16(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 14
              movapd -16(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 14
              movapd -16(pB0), rB0
           #elif KB == 14 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 14
              movapd -16(pB0,ldb), rB1
           #elif KB == 14 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 14 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 14
              movapd -16(pB0,ldb,2), rB2
           #elif KB == 14 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 14 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 14
              movapd -16(pB0,ldb3), rB3
           #elif KB == 14 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 14
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 16
              movapd 0(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 16
              movapd 0(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 16
              movapd 0(pB0), rB0
           #elif KB == 16 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 16
              movapd 0(pB0,ldb), rB1
           #elif KB == 16 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 16 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 16
              movapd 0(pB0,ldb,2), rB2
           #elif KB == 16 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 16 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 16
              movapd 0(pB0,ldb3), rB3
           #elif KB == 16 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 16
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 18
              movapd 16(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 18
              movapd 16(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 18
              movapd 16(pB0), rB0
           #elif KB == 18 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 18
              movapd 16(pB0,ldb), rB1
           #elif KB == 18 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 18 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 18
              movapd 16(pB0,ldb,2), rB2
           #elif KB == 18 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 18 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 18
              movapd 16(pB0,ldb3), rB3
           #elif KB == 18 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 18
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 20
              movapd 32(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 20
              movapd 32(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 20
              movapd 32(pB0), rB0
           #elif KB == 20 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 20
              movapd 32(pB0,ldb), rB1
           #elif KB == 20 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 20 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 20
              movapd 32(pB0,ldb,2), rB2
           #elif KB == 20 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 20 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 20
              movapd 32(pB0,ldb3), rB3
           #elif KB == 20 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 20
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 22
              movapd 48(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 22
              movapd 48(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 22
              movapd 48(pB0), rB0
           #elif KB == 22 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 22
              movapd 48(pB0,ldb), rB1
           #elif KB == 22 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 22 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 22
              movapd 48(pB0,ldb,2), rB2
           #elif KB == 22 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 22 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 22
              movapd 48(pB0,ldb3), rB3
           #elif KB == 22 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 22
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 24
              movapd 64(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 24
              movapd 64(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 24
              movapd 64(pB0), rB0
           #elif KB == 24 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 24
              movapd 64(pB0,ldb), rB1
           #elif KB == 24 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 24 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 24
              movapd 64(pB0,ldb,2), rB2
           #elif KB == 24 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 24 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 24
              movapd 64(pB0,ldb3), rB3
           #elif KB == 24 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 24
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 26
              movapd 80(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 26
              movapd 80(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 26
              movapd 80(pB0), rB0
           #elif KB == 26 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 26
              movapd 80(pB0,ldb), rB1
           #elif KB == 26 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 26 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 26
              movapd 80(pB0,ldb,2), rB2
           #elif KB == 26 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 26 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 26
              movapd 80(pB0,ldb3), rB3
           #elif KB == 26 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 26
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 28
              movapd 96(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 28
              movapd 96(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 28
              movapd 96(pB0), rB0
           #elif KB == 28 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 28
              movapd 96(pB0,ldb), rB1
           #elif KB == 28 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 28 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 28
              movapd 96(pB0,ldb,2), rB2
           #elif KB == 28 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 28 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 28
              movapd 96(pB0,ldb3), rB3
           #elif KB == 28 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 28
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 30
              movapd 112(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 30
              movapd 112(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 30
              movapd 112(pB0), rB0
           #elif KB == 30 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 30
              movapd 112(pB0,ldb), rB1
           #elif KB == 30 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 30 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 30
              movapd 112(pB0,ldb,2), rB2
           #elif KB == 30 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 30 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 30
              movapd 112(pB0,ldb3), rB3
           #elif KB == 30 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 30
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 32
              movapd 128(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 32
              movapd 128(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 32
              movapd 128(pB0), rB0
           #elif KB == 32 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 32
              movapd 128(pB0,ldb), rB1
           #elif KB == 32 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 32 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 32
              movapd 128(pB0,ldb,2), rB2
           #elif KB == 32 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 32 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 32
              movapd 128(pB0,ldb3), rB3
           #elif KB == 32 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 32
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 34
              movapd 144(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 34
              movapd 144(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 34
              movapd 144(pB0), rB0
           #elif KB == 34 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 34
              movapd 144(pB0,ldb), rB1
           #elif KB == 34 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 34 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 34
              movapd 144(pB0,ldb,2), rB2
           #elif KB == 34 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 34 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 34
              movapd 144(pB0,ldb3), rB3
           #elif KB == 34 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 34
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 36
              movapd 160(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 36
              movapd 160(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 36
              movapd 160(pB0), rB0
           #elif KB == 36 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 36
              movapd 160(pB0,ldb), rB1
           #elif KB == 36 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 36 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 36
              movapd 160(pB0,ldb,2), rB2
           #elif KB == 36 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 36 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 36
              movapd 160(pB0,ldb3), rB3
           #elif KB == 36 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 36
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 38
              movapd 176(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 38
              movapd 176(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 38
              movapd 176(pB0), rB0
           #elif KB == 38 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 38
              movapd 176(pB0,ldb), rB1
           #elif KB == 38 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 38 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 38
              movapd 176(pB0,ldb,2), rB2
           #elif KB == 38 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 38 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 38
              movapd 176(pB0,ldb3), rB3
           #elif KB == 38 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 38
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 40
              movapd 192(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 40
              movapd 192(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 40
              movapd 192(pB0), rB0
           #elif KB == 40 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 40
              movapd 192(pB0,ldb), rB1
           #elif KB == 40 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 40 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 40
              movapd 192(pB0,ldb,2), rB2
           #elif KB == 40 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 40 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 40
              movapd 192(pB0,ldb3), rB3
           #elif KB == 40 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 40
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 42
              movapd 208(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 42
              movapd 208(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 42
              movapd 208(pB0), rB0
           #elif KB == 42 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 42
              movapd 208(pB0,ldb), rB1
           #elif KB == 42 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 42 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 42
              movapd 208(pB0,ldb,2), rB2
           #elif KB == 42 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 42 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 42
              movapd 208(pB0,ldb3), rB3
           #elif KB == 42 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 42
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 44
              movapd 224(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 44
              movapd 224(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 44
              movapd 224(pB0), rB0
           #elif KB == 44 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 44
              movapd 224(pB0,ldb), rB1
           #elif KB == 44 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 44 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 44
              movapd 224(pB0,ldb,2), rB2
           #elif KB == 44 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 44 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 44
              movapd 224(pB0,ldb3), rB3
           #elif KB == 44 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 44
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 46
              movapd 240(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 46
              movapd 240(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 46
              movapd 240(pB0), rB0
           #elif KB == 46 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 46
              movapd 240(pB0,ldb), rB1
           #elif KB == 46 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 46 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 46
              movapd 240(pB0,ldb,2), rB2
           #elif KB == 46 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 46 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 46
              movapd 240(pB0,ldb3), rB3
           #elif KB == 46 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 46
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 48
              movapd 256(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 48
              movapd 256(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 48
              movapd 256(pB0), rB0
           #elif KB == 48 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 48
              movapd 256(pB0,ldb), rB1
           #elif KB == 48 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 48 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 48
              movapd 256(pB0,ldb,2), rB2
           #elif KB == 48 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 48 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 48
              movapd 256(pB0,ldb3), rB3
           #elif KB == 48 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 48
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 50
              movapd 272(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 50
              movapd 272(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 50
              movapd 272(pB0), rB0
           #elif KB == 50 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 50
              movapd 272(pB0,ldb), rB1
           #elif KB == 50 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 50 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 50
              movapd 272(pB0,ldb,2), rB2
           #elif KB == 50 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 50 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 50
              movapd 272(pB0,ldb3), rB3
           #elif KB == 50 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 50
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 52
              movapd 288(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 52
              movapd 288(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 52
              movapd 288(pB0), rB0
           #elif KB == 52 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 52
              movapd 288(pB0,ldb), rB1
           #elif KB == 52 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 52 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 52
              movapd 288(pB0,ldb,2), rB2
           #elif KB == 52 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 52 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 52
              movapd 288(pB0,ldb3), rB3
           #elif KB == 52 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 52
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 54
              movapd 304(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 54
              movapd 304(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 54
              movapd 304(pB0), rB0
           #elif KB == 54 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 54
              movapd 304(pB0,ldb), rB1
           #elif KB == 54 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 54 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 54
              movapd 304(pB0,ldb,2), rB2
           #elif KB == 54 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 54 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 54
              movapd 304(pB0,ldb3), rB3
           #elif KB == 54 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 54
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 56
              movapd 320(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 56
              movapd 320(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 56
              movapd 320(pB0), rB0
           #elif KB == 56 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 56
              movapd 320(pB0,ldb), rB1
           #elif KB == 56 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 56 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 56
              movapd 320(pB0,ldb,2), rB2
           #elif KB == 56 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 56 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 56
              movapd 320(pB0,ldb3), rB3
           #elif KB == 56 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 56
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 58
              movapd 336(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 58
              movapd 336(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 58
              movapd 336(pB0), rB0
           #elif KB == 58 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 58
              movapd 336(pB0,ldb), rB1
           #elif KB == 58 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 58 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 58
              movapd 336(pB0,ldb,2), rB2
           #elif KB == 58 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 58 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 58
              movapd 336(pB0,ldb3), rB3
           #elif KB == 58 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 58
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 60
              movapd 352(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 60
              movapd 352(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 60
              movapd 352(pB0), rB0
           #elif KB == 60 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 60
              movapd 352(pB0,ldb), rB1
           #elif KB == 60 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 60 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 60
              movapd 352(pB0,ldb,2), rB2
           #elif KB == 60 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 60 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 60
              movapd 352(pB0,ldb3), rB3
           #elif KB == 60 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 60
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 62
              movapd 368(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 62
              movapd 368(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 62
              movapd 368(pB0), rB0
           #elif KB == 62 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 62
              movapd 368(pB0,ldb), rB1
           #elif KB == 62 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 62 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 62
              movapd 368(pB0,ldb,2), rB2
           #elif KB == 62 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 62 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 62
              movapd 368(pB0,ldb3), rB3
           #elif KB == 62 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 62
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 64
              movapd 384(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 64
              movapd 384(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 64
              movapd 384(pB0), rB0
           #elif KB == 64 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 64
              movapd 384(pB0,ldb), rB1
           #elif KB == 64 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 64 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 64
              movapd 384(pB0,ldb,2), rB2
           #elif KB == 64 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 64 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 64
              movapd 384(pB0,ldb3), rB3
           #elif KB == 64 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 64
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 66
              movapd 400(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 66
              movapd 400(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 66
              movapd 400(pB0), rB0
           #elif KB == 66 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 66
              movapd 400(pB0,ldb), rB1
           #elif KB == 66 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 66 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 66
              movapd 400(pB0,ldb,2), rB2
           #elif KB == 66 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 66 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 66
              movapd 400(pB0,ldb3), rB3
           #elif KB == 66 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 66
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 68
              movapd 416(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 68
              movapd 416(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 68
              movapd 416(pB0), rB0
           #elif KB == 68 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 68
              movapd 416(pB0,ldb), rB1
           #elif KB == 68 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 68 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 68
              movapd 416(pB0,ldb,2), rB2
           #elif KB == 68 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 68 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 68
              movapd 416(pB0,ldb3), rB3
           #elif KB == 68 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 68
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 70
              movapd 432(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 70
              movapd 432(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 70
              movapd 432(pB0), rB0
           #elif KB == 70 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 70
              movapd 432(pB0,ldb), rB1
           #elif KB == 70 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 70 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 70
              movapd 432(pB0,ldb,2), rB2
           #elif KB == 70 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 70 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 70
              movapd 432(pB0,ldb3), rB3
           #elif KB == 70 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 70
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 72
              movapd 448(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 72
              movapd 448(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 72
              movapd 448(pB0), rB0
           #elif KB == 72 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 72
              movapd 448(pB0,ldb), rB1
           #elif KB == 72 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 72 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 72
              movapd 448(pB0,ldb,2), rB2
           #elif KB == 72 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 72 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 72
              movapd 448(pB0,ldb3), rB3
           #elif KB == 72 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 72
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 74
              movapd 464(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 74
              movapd 464(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 74
              movapd 464(pB0), rB0
           #elif KB == 74 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 74
              movapd 464(pB0,ldb), rB1
           #elif KB == 74 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 74 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 74
              movapd 464(pB0,ldb,2), rB2
           #elif KB == 74 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 74 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 74
              movapd 464(pB0,ldb3), rB3
           #elif KB == 74 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 74
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 76
              movapd 480(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 76
              movapd 480(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 76
              movapd 480(pB0), rB0
           #elif KB == 76 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 76
              movapd 480(pB0,ldb), rB1
           #elif KB == 76 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 76 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 76
              movapd 480(pB0,ldb,2), rB2
           #elif KB == 76 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 76 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 76
              movapd 480(pB0,ldb3), rB3
           #elif KB == 76 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 76
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 78
              movapd 496(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 78
              movapd 496(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 78
              movapd 496(pB0), rB0
           #elif KB == 78 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 78
              movapd 496(pB0,ldb), rB1
           #elif KB == 78 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 78 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 78
              movapd 496(pB0,ldb,2), rB2
           #elif KB == 78 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 78 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 78
              movapd 496(pB0,ldb3), rB3
           #elif KB == 78 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 78
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 80
              movapd 512(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 80
              movapd 512(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 80
              movapd 512(pB0), rB0
           #elif KB == 80 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 80
              movapd 512(pB0,ldb), rB1
           #elif KB == 80 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 80 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 80
              movapd 512(pB0,ldb,2), rB2
           #elif KB == 80 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 80 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 80
              movapd 512(pB0,ldb3), rB3
           #elif KB == 80 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 80
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 82
              movapd 528(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 82
              movapd 528(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 82
              movapd 528(pB0), rB0
           #elif KB == 82 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 82
              movapd 528(pB0,ldb), rB1
           #elif KB == 82 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 82 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 82
              movapd 528(pB0,ldb,2), rB2
           #elif KB == 82 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 82 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 82
              movapd 528(pB0,ldb3), rB3
           #elif KB == 82 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 82
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 84
              movapd 544(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 84
              movapd 544(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 84
              movapd 544(pB0), rB0
           #elif KB == 84 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 84
              movapd 544(pB0,ldb), rB1
           #elif KB == 84 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 84 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 84
              movapd 544(pB0,ldb,2), rB2
           #elif KB == 84 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 84 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 84
              movapd 544(pB0,ldb3), rB3
           #elif KB == 84 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 84
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 86
              movapd 560(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 86
              movapd 560(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 86
              movapd 560(pB0), rB0
           #elif KB == 86 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 86
              movapd 560(pB0,ldb), rB1
           #elif KB == 86 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 86 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 86
              movapd 560(pB0,ldb,2), rB2
           #elif KB == 86 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 86 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 86
              movapd 560(pB0,ldb3), rB3
           #elif KB == 86 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 86
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 88
              movapd 576(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 88
              movapd 576(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 88
              movapd 576(pB0), rB0
           #elif KB == 88 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 88
              movapd 576(pB0,ldb), rB1
           #elif KB == 88 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 88 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 88
              movapd 576(pB0,ldb,2), rB2
           #elif KB == 88 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 88 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 88
              movapd 576(pB0,ldb3), rB3
           #elif KB == 88 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 88
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 90
              movapd 592(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 90
              movapd 592(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 90
              movapd 592(pB0), rB0
           #elif KB == 90 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 90
              movapd 592(pB0,ldb), rB1
           #elif KB == 90 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 90 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 90
              movapd 592(pB0,ldb,2), rB2
           #elif KB == 90 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 90 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 90
              movapd 592(pB0,ldb3), rB3
           #elif KB == 90 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 90
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 92
              movapd 608(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 92
              movapd 608(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 92
              movapd 608(pB0), rB0
           #elif KB == 92 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 92
              movapd 608(pB0,ldb), rB1
           #elif KB == 92 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 92 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 92
              movapd 608(pB0,ldb,2), rB2
           #elif KB == 92 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 92 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 92
              movapd 608(pB0,ldb3), rB3
           #elif KB == 92 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 92
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 94
              movapd 624(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 94
              movapd 624(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 94
              movapd 624(pB0), rB0
           #elif KB == 94 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 94
              movapd 624(pB0,ldb), rB1
           #elif KB == 94 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 94 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 94
              movapd 624(pB0,ldb,2), rB2
           #elif KB == 94 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 94 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 94
              movapd 624(pB0,ldb3), rB3
           #elif KB == 94 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 94
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 96
              movapd 640(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 96
              movapd 640(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 96
              movapd 640(pB0), rB0
           #elif KB == 96 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 96
              movapd 640(pB0,ldb), rB1
           #elif KB == 96 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 96 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 96
              movapd 640(pB0,ldb,2), rB2
           #elif KB == 96 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 96 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 96
              movapd 640(pB0,ldb3), rB3
           #elif KB == 96 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 96
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 98
              movapd 656(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 98
              movapd 656(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 98
              movapd 656(pB0), rB0
           #elif KB == 98 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 98
              movapd 656(pB0,ldb), rB1
           #elif KB == 98 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 98 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 98
              movapd 656(pB0,ldb,2), rB2
           #elif KB == 98 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 98 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 98
              movapd 656(pB0,ldb3), rB3
           #elif KB == 98 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 98
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 100
              movapd 672(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 100
              movapd 672(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 100
              movapd 672(pB0), rB0
           #elif KB == 100 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 100
              movapd 672(pB0,ldb), rB1
           #elif KB == 100 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 100 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 100
              movapd 672(pB0,ldb,2), rB2
           #elif KB == 100 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 100 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 100
              movapd 672(pB0,ldb3), rB3
           #elif KB == 100 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 100
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 102
              movapd 688(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 102
              movapd 688(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 102
              movapd 688(pB0), rB0
           #elif KB == 102 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 102
              movapd 688(pB0,ldb), rB1
           #elif KB == 102 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 102 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 102
              movapd 688(pB0,ldb,2), rB2
           #elif KB == 102 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 102 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 102
              movapd 688(pB0,ldb3), rB3
           #elif KB == 102 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 102
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 104
              movapd 704(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 104
              movapd 704(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 104
              movapd 704(pB0), rB0
           #elif KB == 104 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 104
              movapd 704(pB0,ldb), rB1
           #elif KB == 104 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 104 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 104
              movapd 704(pB0,ldb,2), rB2
           #elif KB == 104 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 104 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 104
              movapd 704(pB0,ldb3), rB3
           #elif KB == 104 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 104
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 106
              movapd 720(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 106
              movapd 720(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 106
              movapd 720(pB0), rB0
           #elif KB == 106 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 106
              movapd 720(pB0,ldb), rB1
           #elif KB == 106 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 106 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 106
              movapd 720(pB0,ldb,2), rB2
           #elif KB == 106 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 106 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 106
              movapd 720(pB0,ldb3), rB3
           #elif KB == 106 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 106
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 108
              movapd 736(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 108
              movapd 736(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 108
              movapd 736(pB0), rB0
           #elif KB == 108 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 108
              movapd 736(pB0,ldb), rB1
           #elif KB == 108 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 108 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 108
              movapd 736(pB0,ldb,2), rB2
           #elif KB == 108 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 108 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 108
              movapd 736(pB0,ldb3), rB3
           #elif KB == 108 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 108
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 110
              movapd 752(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 110
              movapd 752(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 110
              movapd 752(pB0), rB0
           #elif KB == 110 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 110
              movapd 752(pB0,ldb), rB1
           #elif KB == 110 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 110 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 110
              movapd 752(pB0,ldb,2), rB2
           #elif KB == 110 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 110 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 110
              movapd 752(pB0,ldb3), rB3
           #elif KB == 110 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 110
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 112
              movapd 768(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 112
              movapd 768(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 112
              movapd 768(pB0), rB0
           #elif KB == 112 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 112
              movapd 768(pB0,ldb), rB1
           #elif KB == 112 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 112 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 112
              movapd 768(pB0,ldb,2), rB2
           #elif KB == 112 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 112 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 112
              movapd 768(pB0,ldb3), rB3
           #elif KB == 112 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 112
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 114
              movapd 784(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 114
              movapd 784(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 114
              movapd 784(pB0), rB0
           #elif KB == 114 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 114
              movapd 784(pB0,ldb), rB1
           #elif KB == 114 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 114 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 114
              movapd 784(pB0,ldb,2), rB2
           #elif KB == 114 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 114 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 114
              movapd 784(pB0,ldb3), rB3
           #elif KB == 114 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 114
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 116
              movapd 800(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 116
              movapd 800(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 116
              movapd 800(pB0), rB0
           #elif KB == 116 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 116
              movapd 800(pB0,ldb), rB1
           #elif KB == 116 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 116 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 116
              movapd 800(pB0,ldb,2), rB2
           #elif KB == 116 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 116 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 116
              movapd 800(pB0,ldb3), rB3
           #elif KB == 116 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 116
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 118
              movapd 816(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 118
              movapd 816(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 118
              movapd 816(pB0), rB0
           #elif KB == 118 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 118
              movapd 816(pB0,ldb), rB1
           #elif KB == 118 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 118 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 118
              movapd 816(pB0,ldb,2), rB2
           #elif KB == 118 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 118 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 118
              movapd 816(pB0,ldb3), rB3
           #elif KB == 118 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 118
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 120
              movapd 832(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 120
              movapd 832(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 120
              movapd 832(pB0), rB0
           #elif KB == 120 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 120
              movapd 832(pB0,ldb), rB1
           #elif KB == 120 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 120 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 120
              movapd 832(pB0,ldb,2), rB2
           #elif KB == 120 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 120 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 120
              movapd 832(pB0,ldb3), rB3
           #elif KB == 120 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 120
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 122
              movapd 848(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 122
              movapd 848(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 122
              movapd 848(pB0), rB0
           #elif KB == 122 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 122
              movapd 848(pB0,ldb), rB1
           #elif KB == 122 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 122 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 122
              movapd 848(pB0,ldb,2), rB2
           #elif KB == 122 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 122 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 122
              movapd 848(pB0,ldb3), rB3
           #elif KB == 122 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 122
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 124
              movapd 864(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 124
              movapd 864(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 124
              movapd 864(pB0), rB0
           #elif KB == 124 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 124
              movapd 864(pB0,ldb), rB1
           #elif KB == 124 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 124 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 124
              movapd 864(pB0,ldb,2), rB2
           #elif KB == 124 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 124 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 124
              movapd 864(pB0,ldb3), rB3
           #elif KB == 124 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 124
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 126
              movapd 880(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 126
              movapd 880(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 126
              movapd 880(pB0), rB0
           #elif KB == 126 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 126
              movapd 880(pB0,ldb), rB1
           #elif KB == 126 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 126 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 126
              movapd 880(pB0,ldb,2), rB2
           #elif KB == 126 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 126 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 126
              movapd 880(pB0,ldb3), rB3
           #elif KB == 126 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 126
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 128
              movapd 896(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 128
              movapd 896(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 128
              movapd 896(pB0), rB0
           #elif KB == 128 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 128
              movapd 896(pB0,ldb), rB1
           #elif KB == 128 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 128 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 128
              movapd 896(pB0,ldb,2), rB2
           #elif KB == 128 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 128 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 128
              movapd 896(pB0,ldb3), rB3
           #elif KB == 128 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 128
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 130
              movapd 912(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 130
              movapd 912(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 130
              movapd 912(pB0), rB0
           #elif KB == 130 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 130
              movapd 912(pB0,ldb), rB1
           #elif KB == 130 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 130 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 130
              movapd 912(pB0,ldb,2), rB2
           #elif KB == 130 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 130 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 130
              movapd 912(pB0,ldb3), rB3
           #elif KB == 130 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 130
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 132
              movapd 928(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 132
              movapd 928(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 132
              movapd 928(pB0), rB0
           #elif KB == 132 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 132
              movapd 928(pB0,ldb), rB1
           #elif KB == 132 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 132 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 132
              movapd 928(pB0,ldb,2), rB2
           #elif KB == 132 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 132 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 132
              movapd 928(pB0,ldb3), rB3
           #elif KB == 132 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 132
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 134
              movapd 944(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 134
              movapd 944(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 134
              movapd 944(pB0), rB0
           #elif KB == 134 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 134
              movapd 944(pB0,ldb), rB1
           #elif KB == 134 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 134 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 134
              movapd 944(pB0,ldb,2), rB2
           #elif KB == 134 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 134 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 134
              movapd 944(pB0,ldb3), rB3
           #elif KB == 134 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 134
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 136
              movapd 960(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 136
              movapd 960(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 136
              movapd 960(pB0), rB0
           #elif KB == 136 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 136
              movapd 960(pB0,ldb), rB1
           #elif KB == 136 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 136 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 136
              movapd 960(pB0,ldb,2), rB2
           #elif KB == 136 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 136 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 136
              movapd 960(pB0,ldb3), rB3
           #elif KB == 136 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 136
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 138
              movapd 976(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 138
              movapd 976(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 138
              movapd 976(pB0), rB0
           #elif KB == 138 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 138
              movapd 976(pB0,ldb), rB1
           #elif KB == 138 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 138 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 138
              movapd 976(pB0,ldb,2), rB2
           #elif KB == 138 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 138 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 138
              movapd 976(pB0,ldb3), rB3
           #elif KB == 138 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 138
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 140
              movapd 992(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 140
              movapd 992(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 140
              movapd 992(pB0), rB0
           #elif KB == 140 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 140
              movapd 992(pB0,ldb), rB1
           #elif KB == 140 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 140 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 140
              movapd 992(pB0,ldb,2), rB2
           #elif KB == 140 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 140 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 140
              movapd 992(pB0,ldb3), rB3
           #elif KB == 140 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 140
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 142
              movapd 1008(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 142
              movapd 1008(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 142
              movapd 1008(pB0), rB0
           #elif KB == 142 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 142
              movapd 1008(pB0,ldb), rB1
           #elif KB == 142 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 142 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 142
              movapd 1008(pB0,ldb,2), rB2
           #elif KB == 142 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 142 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 142
              movapd 1008(pB0,ldb3), rB3
           #elif KB == 142 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 142
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 144
              movapd 1024(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 144
              movapd 1024(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 144
              movapd 1024(pB0), rB0
           #elif KB == 144 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 144
              movapd 1024(pB0,ldb), rB1
           #elif KB == 144 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 144 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 144
              movapd 1024(pB0,ldb,2), rB2
           #elif KB == 144 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 144 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 144
              movapd 1024(pB0,ldb3), rB3
           #elif KB == 144 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 144
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 146
              movapd 1040(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 146
              movapd 1040(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 146
              movapd 1040(pB0), rB0
           #elif KB == 146 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 146
              movapd 1040(pB0,ldb), rB1
           #elif KB == 146 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 146 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 146
              movapd 1040(pB0,ldb,2), rB2
           #elif KB == 146 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 146 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 146
              movapd 1040(pB0,ldb3), rB3
           #elif KB == 146 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 146
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 148
              movapd 1056(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 148
              movapd 1056(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 148
              movapd 1056(pB0), rB0
           #elif KB == 148 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 148
              movapd 1056(pB0,ldb), rB1
           #elif KB == 148 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 148 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 148
              movapd 1056(pB0,ldb,2), rB2
           #elif KB == 148 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 148 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 148
              movapd 1056(pB0,ldb3), rB3
           #elif KB == 148 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 148
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 150
              movapd 1072(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 150
              movapd 1072(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 150
              movapd 1072(pB0), rB0
           #elif KB == 150 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 150
              movapd 1072(pB0,ldb), rB1
           #elif KB == 150 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 150 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 150
              movapd 1072(pB0,ldb,2), rB2
           #elif KB == 150 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 150 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 150
              movapd 1072(pB0,ldb3), rB3
           #elif KB == 150 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 150
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 152
              movapd 1088(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 152
              movapd 1088(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 152
              movapd 1088(pB0), rB0
           #elif KB == 152 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 152
              movapd 1088(pB0,ldb), rB1
           #elif KB == 152 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 152 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 152
              movapd 1088(pB0,ldb,2), rB2
           #elif KB == 152 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 152 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 152
              movapd 1088(pB0,ldb3), rB3
           #elif KB == 152 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 152
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 154
              movapd 1104(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 154
              movapd 1104(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 154
              movapd 1104(pB0), rB0
           #elif KB == 154 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 154
              movapd 1104(pB0,ldb), rB1
           #elif KB == 154 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 154 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 154
              movapd 1104(pB0,ldb,2), rB2
           #elif KB == 154 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 154 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 154
              movapd 1104(pB0,ldb3), rB3
           #elif KB == 154 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 154
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 156
              movapd 1120(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 156
              movapd 1120(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 156
              movapd 1120(pB0), rB0
           #elif KB == 156 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 156
              movapd 1120(pB0,ldb), rB1
           #elif KB == 156 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 156 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 156
              movapd 1120(pB0,ldb,2), rB2
           #elif KB == 156 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 156 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 156
              movapd 1120(pB0,ldb3), rB3
           #elif KB == 156 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 156
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 158
              movapd 1136(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 158
              movapd 1136(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 158
              movapd 1136(pB0), rB0
           #elif KB == 158 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 158
              movapd 1136(pB0,ldb), rB1
           #elif KB == 158 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 158 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 158
              movapd 1136(pB0,ldb,2), rB2
           #elif KB == 158 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 158 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 158
              movapd 1136(pB0,ldb3), rB3
           #elif KB == 158 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 158
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 160
              movapd 1152(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 160
              movapd 1152(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 160
              movapd 1152(pB0), rB0
           #elif KB == 160 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 160
              movapd 1152(pB0,ldb), rB1
           #elif KB == 160 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 160 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 160
              movapd 1152(pB0,ldb,2), rB2
           #elif KB == 160 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 160 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 160
              movapd 1152(pB0,ldb3), rB3
           #elif KB == 160 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 160
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 162
              movapd 1168(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 162
              movapd 1168(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 162
              movapd 1168(pB0), rB0
           #elif KB == 162 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 162
              movapd 1168(pB0,ldb), rB1
           #elif KB == 162 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 162 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 162
              movapd 1168(pB0,ldb,2), rB2
           #elif KB == 162 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 162 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 162
              movapd 1168(pB0,ldb3), rB3
           #elif KB == 162 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 162
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 164
              movapd 1184(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 164
              movapd 1184(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 164
              movapd 1184(pB0), rB0
           #elif KB == 164 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 164
              movapd 1184(pB0,ldb), rB1
           #elif KB == 164 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 164 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 164
              movapd 1184(pB0,ldb,2), rB2
           #elif KB == 164 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 164 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 164
              movapd 1184(pB0,ldb3), rB3
           #elif KB == 164 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 164
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 166
              movapd 1200(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 166
              movapd 1200(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 166
              movapd 1200(pB0), rB0
           #elif KB == 166 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 166
              movapd 1200(pB0,ldb), rB1
           #elif KB == 166 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 166 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 166
              movapd 1200(pB0,ldb,2), rB2
           #elif KB == 166 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 166 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 166
              movapd 1200(pB0,ldb3), rB3
           #elif KB == 166 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 166
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 168
              movapd 1216(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 168
              movapd 1216(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 168
              movapd 1216(pB0), rB0
           #elif KB == 168 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 168
              movapd 1216(pB0,ldb), rB1
           #elif KB == 168 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 168 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 168
              movapd 1216(pB0,ldb,2), rB2
           #elif KB == 168 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 168 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 168
              movapd 1216(pB0,ldb3), rB3
           #elif KB == 168 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 168
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 170
              movapd 1232(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 170
              movapd 1232(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 170
              movapd 1232(pB0), rB0
           #elif KB == 170 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 170
              movapd 1232(pB0,ldb), rB1
           #elif KB == 170 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 170 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 170
              movapd 1232(pB0,ldb,2), rB2
           #elif KB == 170 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 170 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 170
              movapd 1232(pB0,ldb3), rB3
           #elif KB == 170 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 170
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 172
              movapd 1248(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 172
              movapd 1248(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 172
              movapd 1248(pB0), rB0
           #elif KB == 172 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 172
              movapd 1248(pB0,ldb), rB1
           #elif KB == 172 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 172 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 172
              movapd 1248(pB0,ldb,2), rB2
           #elif KB == 172 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 172 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 172
              movapd 1248(pB0,ldb3), rB3
           #elif KB == 172 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 172
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 174
              movapd 1264(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 174
              movapd 1264(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 174
              movapd 1264(pB0), rB0
           #elif KB == 174 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 174
              movapd 1264(pB0,ldb), rB1
           #elif KB == 174 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 174 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 174
              movapd 1264(pB0,ldb,2), rB2
           #elif KB == 174 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 174 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 174
              movapd 1264(pB0,ldb3), rB3
           #elif KB == 174 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 174
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 176
              movapd 1280(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 176
              movapd 1280(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 176
              movapd 1280(pB0), rB0
           #elif KB == 176 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 176
              movapd 1280(pB0,ldb), rB1
           #elif KB == 176 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 176 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 176
              movapd 1280(pB0,ldb,2), rB2
           #elif KB == 176 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 176 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 176
              movapd 1280(pB0,ldb3), rB3
           #elif KB == 176 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 176
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 178
              movapd 1296(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 178
              movapd 1296(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 178
              movapd 1296(pB0), rB0
           #elif KB == 178 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 178
              movapd 1296(pB0,ldb), rB1
           #elif KB == 178 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 178 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 178
              movapd 1296(pB0,ldb,2), rB2
           #elif KB == 178 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 178 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 178
              movapd 1296(pB0,ldb3), rB3
           #elif KB == 178 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 178
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 180
              movapd 1312(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 180
              movapd 1312(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 180
              movapd 1312(pB0), rB0
           #elif KB == 180 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 180
              movapd 1312(pB0,ldb), rB1
           #elif KB == 180 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 180 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 180
              movapd 1312(pB0,ldb,2), rB2
           #elif KB == 180 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 180 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 180
              movapd 1312(pB0,ldb3), rB3
           #elif KB == 180 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 180
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 182
              movapd 1328(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 182
              movapd 1328(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 182
              movapd 1328(pB0), rB0
           #elif KB == 182 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 182
              movapd 1328(pB0,ldb), rB1
           #elif KB == 182 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 182 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 182
              movapd 1328(pB0,ldb,2), rB2
           #elif KB == 182 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 182 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 182
              movapd 1328(pB0,ldb3), rB3
           #elif KB == 182 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 182
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 184
              movapd 1344(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 184
              movapd 1344(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 184
              movapd 1344(pB0), rB0
           #elif KB == 184 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 184
              movapd 1344(pB0,ldb), rB1
           #elif KB == 184 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 184 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 184
              movapd 1344(pB0,ldb,2), rB2
           #elif KB == 184 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 184 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 184
              movapd 1344(pB0,ldb3), rB3
           #elif KB == 184 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 184
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 186
              movapd 1360(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 186
              movapd 1360(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 186
              movapd 1360(pB0), rB0
           #elif KB == 186 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 186
              movapd 1360(pB0,ldb), rB1
           #elif KB == 186 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 186 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 186
              movapd 1360(pB0,ldb,2), rB2
           #elif KB == 186 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 186 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 186
              movapd 1360(pB0,ldb3), rB3
           #elif KB == 186 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 186
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 188
              movapd 1376(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 188
              movapd 1376(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 188
              movapd 1376(pB0), rB0
           #elif KB == 188 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 188
              movapd 1376(pB0,ldb), rB1
           #elif KB == 188 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 188 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 188
              movapd 1376(pB0,ldb,2), rB2
           #elif KB == 188 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 188 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 188
              movapd 1376(pB0,ldb3), rB3
           #elif KB == 188 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 188
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 190
              movapd 1392(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 190
              movapd 1392(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 190
              movapd 1392(pB0), rB0
           #elif KB == 190 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 190
              movapd 1392(pB0,ldb), rB1
           #elif KB == 190 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 190 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 190
              movapd 1392(pB0,ldb,2), rB2
           #elif KB == 190 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 190 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 190
              movapd 1392(pB0,ldb3), rB3
           #elif KB == 190 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 190
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 192
              movapd 1408(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 192
              movapd 1408(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 192
              movapd 1408(pB0), rB0
           #elif KB == 192 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 192
              movapd 1408(pB0,ldb), rB1
           #elif KB == 192 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 192 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 192
              movapd 1408(pB0,ldb,2), rB2
           #elif KB == 192 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 192 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 192
              movapd 1408(pB0,ldb3), rB3
           #elif KB == 192 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 192
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 194
              movapd 1424(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 194
              movapd 1424(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 194
              movapd 1424(pB0), rB0
           #elif KB == 194 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 194
              movapd 1424(pB0,ldb), rB1
           #elif KB == 194 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 194 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 194
              movapd 1424(pB0,ldb,2), rB2
           #elif KB == 194 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 194 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 194
              movapd 1424(pB0,ldb3), rB3
           #elif KB == 194 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 194
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 196
              movapd 1440(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 196
              movapd 1440(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 196
              movapd 1440(pB0), rB0
           #elif KB == 196 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 196
              movapd 1440(pB0,ldb), rB1
           #elif KB == 196 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 196 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 196
              movapd 1440(pB0,ldb,2), rB2
           #elif KB == 196 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 196 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 196
              movapd 1440(pB0,ldb3), rB3
           #elif KB == 196 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 196
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 198
              movapd 1456(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 198
              movapd 1456(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 198
              movapd 1456(pB0), rB0
           #elif KB == 198 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 198
              movapd 1456(pB0,ldb), rB1
           #elif KB == 198 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 198 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 198
              movapd 1456(pB0,ldb,2), rB2
           #elif KB == 198 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 198 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 198
              movapd 1456(pB0,ldb3), rB3
           #elif KB == 198 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 198
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 200
              movapd 1472(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 200
              movapd 1472(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 200
              movapd 1472(pB0), rB0
           #elif KB == 200 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 200
              movapd 1472(pB0,ldb), rB1
           #elif KB == 200 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 200 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 200
              movapd 1472(pB0,ldb,2), rB2
           #elif KB == 200 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 200 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 200
              movapd 1472(pB0,ldb3), rB3
           #elif KB == 200 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 200
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 202
              movapd 1488(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 202
              movapd 1488(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 202
              movapd 1488(pB0), rB0
           #elif KB == 202 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 202
              movapd 1488(pB0,ldb), rB1
           #elif KB == 202 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 202 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 202
              movapd 1488(pB0,ldb,2), rB2
           #elif KB == 202 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 202 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 202
              movapd 1488(pB0,ldb3), rB3
           #elif KB == 202 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 202
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 204
              movapd 1504(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 204
              movapd 1504(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 204
              movapd 1504(pB0), rB0
           #elif KB == 204 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 204
              movapd 1504(pB0,ldb), rB1
           #elif KB == 204 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 204 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 204
              movapd 1504(pB0,ldb,2), rB2
           #elif KB == 204 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 204 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 204
              movapd 1504(pB0,ldb3), rB3
           #elif KB == 204 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 204
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 206
              movapd 1520(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 206
              movapd 1520(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 206
              movapd 1520(pB0), rB0
           #elif KB == 206 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 206
              movapd 1520(pB0,ldb), rB1
           #elif KB == 206 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 206 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 206
              movapd 1520(pB0,ldb,2), rB2
           #elif KB == 206 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 206 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 206
              movapd 1520(pB0,ldb3), rB3
           #elif KB == 206 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 206
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 208
              movapd 1536(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 208
              movapd 1536(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 208
              movapd 1536(pB0), rB0
           #elif KB == 208 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 208
              movapd 1536(pB0,ldb), rB1
           #elif KB == 208 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 208 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 208
              movapd 1536(pB0,ldb,2), rB2
           #elif KB == 208 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 208 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 208
              movapd 1536(pB0,ldb3), rB3
           #elif KB == 208 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 208
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 210
              movapd 1552(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 210
              movapd 1552(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 210
              movapd 1552(pB0), rB0
           #elif KB == 210 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 210
              movapd 1552(pB0,ldb), rB1
           #elif KB == 210 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 210 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 210
              movapd 1552(pB0,ldb,2), rB2
           #elif KB == 210 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 210 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 210
              movapd 1552(pB0,ldb3), rB3
           #elif KB == 210 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 210
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 212
              movapd 1568(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 212
              movapd 1568(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 212
              movapd 1568(pB0), rB0
           #elif KB == 212 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 212
              movapd 1568(pB0,ldb), rB1
           #elif KB == 212 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 212 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 212
              movapd 1568(pB0,ldb,2), rB2
           #elif KB == 212 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 212 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 212
              movapd 1568(pB0,ldb3), rB3
           #elif KB == 212 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 212
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 214
              movapd 1584(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 214
              movapd 1584(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 214
              movapd 1584(pB0), rB0
           #elif KB == 214 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 214
              movapd 1584(pB0,ldb), rB1
           #elif KB == 214 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 214 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 214
              movapd 1584(pB0,ldb,2), rB2
           #elif KB == 214 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 214 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 214
              movapd 1584(pB0,ldb3), rB3
           #elif KB == 214 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 214
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 216
              movapd 1600(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 216
              movapd 1600(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 216
              movapd 1600(pB0), rB0
           #elif KB == 216 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 216
              movapd 1600(pB0,ldb), rB1
           #elif KB == 216 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 216 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 216
              movapd 1600(pB0,ldb,2), rB2
           #elif KB == 216 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 216 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 216
              movapd 1600(pB0,ldb3), rB3
           #elif KB == 216 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 216
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 218
              movapd 1616(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 218
              movapd 1616(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 218
              movapd 1616(pB0), rB0
           #elif KB == 218 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 218
              movapd 1616(pB0,ldb), rB1
           #elif KB == 218 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 218 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 218
              movapd 1616(pB0,ldb,2), rB2
           #elif KB == 218 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 218 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 218
              movapd 1616(pB0,ldb3), rB3
           #elif KB == 218 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 218
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 220
              movapd 1632(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 220
              movapd 1632(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 220
              movapd 1632(pB0), rB0
           #elif KB == 220 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 220
              movapd 1632(pB0,ldb), rB1
           #elif KB == 220 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 220 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 220
              movapd 1632(pB0,ldb,2), rB2
           #elif KB == 220 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 220 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 220
              movapd 1632(pB0,ldb3), rB3
           #elif KB == 220 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 220
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 222
              movapd 1648(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 222
              movapd 1648(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 222
              movapd 1648(pB0), rB0
           #elif KB == 222 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 222
              movapd 1648(pB0,ldb), rB1
           #elif KB == 222 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 222 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 222
              movapd 1648(pB0,ldb,2), rB2
           #elif KB == 222 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 222 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 222
              movapd 1648(pB0,ldb3), rB3
           #elif KB == 222 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 222
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 224
              movapd 1664(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 224
              movapd 1664(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 224
              movapd 1664(pB0), rB0
           #elif KB == 224 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 224
              movapd 1664(pB0,ldb), rB1
           #elif KB == 224 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 224 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 224
              movapd 1664(pB0,ldb,2), rB2
           #elif KB == 224 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 224 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 224
              movapd 1664(pB0,ldb3), rB3
           #elif KB == 224 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 224
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 226
              movapd 1680(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 226
              movapd 1680(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 226
              movapd 1680(pB0), rB0
           #elif KB == 226 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 226
              movapd 1680(pB0,ldb), rB1
           #elif KB == 226 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 226 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 226
              movapd 1680(pB0,ldb,2), rB2
           #elif KB == 226 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 226 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 226
              movapd 1680(pB0,ldb3), rB3
           #elif KB == 226 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 226
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 228
              movapd 1696(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 228
              movapd 1696(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 228
              movapd 1696(pB0), rB0
           #elif KB == 228 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 228
              movapd 1696(pB0,ldb), rB1
           #elif KB == 228 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 228 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 228
              movapd 1696(pB0,ldb,2), rB2
           #elif KB == 228 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 228 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 228
              movapd 1696(pB0,ldb3), rB3
           #elif KB == 228 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 228
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 230
              movapd 1712(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 230
              movapd 1712(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 230
              movapd 1712(pB0), rB0
           #elif KB == 230 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 230
              movapd 1712(pB0,ldb), rB1
           #elif KB == 230 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 230 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 230
              movapd 1712(pB0,ldb,2), rB2
           #elif KB == 230 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 230 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 230
              movapd 1712(pB0,ldb3), rB3
           #elif KB == 230 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 230
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 232
              movapd 1728(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 232
              movapd 1728(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 232
              movapd 1728(pB0), rB0
           #elif KB == 232 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 232
              movapd 1728(pB0,ldb), rB1
           #elif KB == 232 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 232 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 232
              movapd 1728(pB0,ldb,2), rB2
           #elif KB == 232 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 232 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 232
              movapd 1728(pB0,ldb3), rB3
           #elif KB == 232 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 232
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 234
              movapd 1744(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 234
              movapd 1744(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 234
              movapd 1744(pB0), rB0
           #elif KB == 234 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 234
              movapd 1744(pB0,ldb), rB1
           #elif KB == 234 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 234 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 234
              movapd 1744(pB0,ldb,2), rB2
           #elif KB == 234 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 234 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 234
              movapd 1744(pB0,ldb3), rB3
           #elif KB == 234 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 234
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 236
              movapd 1760(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 236
              movapd 1760(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 236
              movapd 1760(pB0), rB0
           #elif KB == 236 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 236
              movapd 1760(pB0,ldb), rB1
           #elif KB == 236 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 236 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 236
              movapd 1760(pB0,ldb,2), rB2
           #elif KB == 236 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 236 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 236
              movapd 1760(pB0,ldb3), rB3
           #elif KB == 236 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 236
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 238
              movapd 1776(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 238
              movapd 1776(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 238
              movapd 1776(pB0), rB0
           #elif KB == 238 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 238
              movapd 1776(pB0,ldb), rB1
           #elif KB == 238 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 238 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 238
              movapd 1776(pB0,ldb,2), rB2
           #elif KB == 238 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 238 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 238
              movapd 1776(pB0,ldb3), rB3
           #elif KB == 238 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 238
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 240
              movapd 1792(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 240
              movapd 1792(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 240
              movapd 1792(pB0), rB0
           #elif KB == 240 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 240
              movapd 1792(pB0,ldb), rB1
           #elif KB == 240 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 240 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 240
              movapd 1792(pB0,ldb,2), rB2
           #elif KB == 240 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 240 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 240
              movapd 1792(pB0,ldb3), rB3
           #elif KB == 240 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 240
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 242
              movapd 1808(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 242
              movapd 1808(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 242
              movapd 1808(pB0), rB0
           #elif KB == 242 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 242
              movapd 1808(pB0,ldb), rB1
           #elif KB == 242 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 242 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 242
              movapd 1808(pB0,ldb,2), rB2
           #elif KB == 242 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 242 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 242
              movapd 1808(pB0,ldb3), rB3
           #elif KB == 242 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 242
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 244
              movapd 1824(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 244
              movapd 1824(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 244
              movapd 1824(pB0), rB0
           #elif KB == 244 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 244
              movapd 1824(pB0,ldb), rB1
           #elif KB == 244 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 244 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 244
              movapd 1824(pB0,ldb,2), rB2
           #elif KB == 244 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 244 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 244
              movapd 1824(pB0,ldb3), rB3
           #elif KB == 244 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 244
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 246
              movapd 1840(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 246
              movapd 1840(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 246
              movapd 1840(pB0), rB0
           #elif KB == 246 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 246
              movapd 1840(pB0,ldb), rB1
           #elif KB == 246 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 246 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 246
              movapd 1840(pB0,ldb,2), rB2
           #elif KB == 246 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 246 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 246
              movapd 1840(pB0,ldb3), rB3
           #elif KB == 246 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 246
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 248
              movapd 1856(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 248
              movapd 1856(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 248
              movapd 1856(pB0), rB0
           #elif KB == 248 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 248
              movapd 1856(pB0,ldb), rB1
           #elif KB == 248 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 248 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 248
              movapd 1856(pB0,ldb,2), rB2
           #elif KB == 248 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 248 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 248
              movapd 1856(pB0,ldb3), rB3
           #elif KB == 248 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 248
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 250
              movapd 1872(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 250
              movapd 1872(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 250
              movapd 1872(pB0), rB0
           #elif KB == 250 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 250
              movapd 1872(pB0,ldb), rB1
           #elif KB == 250 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 250 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 250
              movapd 1872(pB0,ldb,2), rB2
           #elif KB == 250 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 250 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 250
              movapd 1872(pB0,ldb3), rB3
           #elif KB == 250 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 250
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 252
              movapd 1888(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 252
              movapd 1888(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 252
              movapd 1888(pB0), rB0
           #elif KB == 252 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 252
              movapd 1888(pB0,ldb), rB1
           #elif KB == 252 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 252 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 252
              movapd 1888(pB0,ldb,2), rB2
           #elif KB == 252 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 252 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 252
              movapd 1888(pB0,ldb3), rB3
           #elif KB == 252 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 252
           vfmaddpd rC00, rA0, rB0, rC00
           #if KB > 254
              movapd 1904(pA0), ra0
           #endif
           vfmaddpd rC10, rA1, rB0, rC10
           #if KB > 254
              movapd 1904(pA0,lda), ra1
           #endif
           vfmaddpd rC01, rA0, rB1, rC01
           #if KB > 254
              movapd 1904(pB0), rB0
           #elif KB == 254 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, rA1, rB1, rC11
           #if KB > 254
              movapd 1904(pB0,ldb), rB1
           #elif KB == 254 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, rA0, rB2, rC02
           #if KB == 254 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, rA1, rB2, rC12
           #if KB > 254
              movapd 1904(pB0,ldb,2), rB2
           #elif KB == 254 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, rA0, rB3, rC03
           #if KB == 254 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, rA1, rB3, rC13
           #if KB > 254
              movapd 1904(pB0,ldb3), rB3
           #elif KB == 254 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif
        #if KB > 254
           vfmaddpd rC00, ra0, rB0, rC00
           #if KB > 256
              movapd 1920(pA0), rA0
           #endif
           vfmaddpd rC10, ra1, rB0, rC10
           #if KB > 256
              movapd 1920(pA0,lda), rA1
           #endif
           vfmaddpd rC01, ra0, rB1, rC01
           #if KB > 256
              movapd 1920(pB0), rB0
           #elif KB == 256 && !defined(BETA0)
               movlpd (pC0),rB0
               movhpd CMUL(8)(pC0),rB0
           #endif
           vfmaddpd rC11, ra1, rB1, rC11
           #if KB > 256
              movapd 1920(pB0,ldb), rB1
           #elif KB == 256 && !defined(BETA0)
               movlpd (pC0,ldc),rB1
           #endif
           vfmaddpd rC02, ra0, rB2, rC02
           #if KB == 256 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc),rB1
           #endif
           vfmaddpd rC12, ra1, rB2, rC12
           #if KB > 256
              movapd 1920(pB0,ldb,2), rB2
           #elif KB == 256 && !defined(BETA0)
               movlpd (pC0,ldc,2),rB2
           #endif
           vfmaddpd rC03, ra0, rB3, rC03
           #if KB == 256 && !defined(BETA0)
               movhpd CMUL(8)(pC0,ldc,2),rB2
           #endif
           vfmaddpd rC13, ra1, rB3, rC13
           #if KB > 256
              movapd 1920(pB0,ldb3), rB3
           #elif KB == 256 && !defined(BETA0)
               movlpd (pC0,ldc3),rB3
               movhpd CMUL(8)(pC0,ldc3),rB3
           #endif
        #endif

/*
 *       K-loop finished, sum up vectors
 */
         prefetcht1 (pfA)
         haddpd rC10, rC00
         add $64, pfA
         haddpd rC11, rC01
         haddpd rC12, rC02
         haddpd rC13, rC03
         #ifndef BETA0
            #ifdef BETAX
               movapd BETAOFF(%rsp), rA0
               vfmaddpd rC00, rB0, rA0, rC00
               vfmaddpd rC01, rB1, rA0, rC01
               vfmaddpd rC02, rB2, rA0, rC02
               vfmaddpd rC03, rB3, rA0, rC03
            #else
               addpd rB0, rC00
               addpd rB1, rC01
               addpd rB2, rC02
               addpd rB3, rC03
            #endif
         #endif
         #ifndef DCPLX
            MOVCPD rC00, (pC0)
            MOVCPD rC01, (pC0,ldc)
            MOVCPD rC02, (pC0,ldc,2)
            MOVCPD rC03, (pC0,ldc3)
         #else
            movlpd rC00, (pC0)
            movhpd rC00, 16(pC0)
            movlpd rC01, (pC0,ldc)
            movhpd rC01, 16(pC0,ldc)
            movlpd rC02, (pC0,ldc,2)
            movhpd rC02, 16(pC0,ldc,2)
            movlpd rC03, (pC0,ldc3)
            movhpd rC03, 16(pC0,ldc3)
         #endif

         lea (pA0,lda,2), pA0
         add $2*CMUL(8), pC0

      sub $2, MM
      jnz MNLOOP

      mov MM0, MM
      sub incAn, pA0
      add incCn, pC0
      lea (pB0, ldb, 4), pB0
   sub $4, NN
   jnz MNLOOP

/* DONE: */
        movq    -8(%rsp), %rbp
        movq    -16(%rsp), %rbx
        movq    -24(%rsp), %r12
        movq    -32(%rsp), %r13
        movq    -40(%rsp), %r14
        movq    -48(%rsp), %r15
        ret
