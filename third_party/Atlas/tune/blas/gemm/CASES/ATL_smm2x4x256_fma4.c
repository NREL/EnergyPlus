#include "atlas_asm.h"
#ifndef ATL_AVXFMA4
   #error "This kernel requires AMD's 4-operand FMAC (FM4)"
#endif
#ifdef SCPLX
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
        movlhps %xmm1, %xmm1
        unpcklps %xmm1, %xmm1
        movaps  %xmm1, BETAOFF(%rsp)
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
#ifdef SCPLX
        shl     $3, incCn
        shl	$3, ldc
#else
        shl     $2, incCn
        shl	$2, ldc
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
        shl     $2, ldb
                                                prefetcht0      (pB0)
        lea     (ldb,ldb,2), ldb3
        lea     (ldc,ldc,2), ldc3
/*
 *      lda = lda*sizeof
 */
        shl     $2, lda
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
 *       K=0-3, with rCxx starting at zero
 */
         movaps -128(pB0), rB0
         xorpd  rC13, rC13
         movaps -128(pA0), rA0
         vfmaddps rC13, rA0, rB0, rC00
         movaps -128(pB0,ldb), rB1
         vfmaddps rC13, rA0, rB1, rC01
         movaps -128(pB0,ldb,2), rB2
         vfmaddps rC13, rA0, rB2, rC02
         movaps -128(pB0,ldb3), rB3
         vfmaddps rC13, rA0, rB3, rC03
         movaps -128(pA0,lda), rA1
         vfmaddps rC13, rA1, rB0, rC10
         #if KB > 4
            movaps -112(pA0), ra0
         #endif
         vfmaddps rC13, rA1, rB1, rC11
         #if KB > 4
            movaps -112(pA0,lda), ra1
         #endif
         vfmaddps rC13, rA1, rB2, rC12
         #if KB > 4
            movaps -112(pB0), rB0
         #endif
         vfmaddps rC13, rA1, rB3, rC13
         #if KB > 4
            movaps -112(pB0,ldb), rB1
         #endif
/*
 *       K=4-7
 */
        #if KB > 4
           vfmaddps rC00, ra0, rB0, rC00
           movaps -112(pB0,ldb,2), rB2
           vfmaddps rC10, ra1, rB0, rC10
           movaps -112(pB0,ldb3), rB3
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 8
              movaps -96(pA0), rA0
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 8
              movaps -96(pA0,lda), rA1
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB > 8
              movaps -96(pB0), rB0
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 8
              movaps -96(pB0,ldb), rB1
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB > 8
              movaps -96(pB0,ldb,2), rB2
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 8
              movaps -96(pB0,ldb3), rB3
           #endif
        #endif
        #if KB <= 8
        #endif
        #if KB > 8
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 12
              movaps -80(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 12
              movaps -80(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 12
              movaps -80(pB0), rB0
           #elif KB == 12 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 12
              movaps -80(pB0,ldb), rB1
           #elif KB == 12 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 12
              prefetcht0 (pfB)
              add $64, pfB
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 12
              movaps -80(pB0,ldb,2), rB2
           #elif KB == 12 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 12 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 12
              movaps -80(pB0,ldb3), rB3
           #elif KB == 12 && defined(BETA0)
           #endif
        #endif
        #if KB > 12
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 16
              movaps -64(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 16
              movaps -64(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 16
              movaps -64(pB0), rB0
           #elif KB == 16 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 16
              movaps -64(pB0,ldb), rB1
           #elif KB == 16 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 16
              prefetcht0 (pfB)
              add $64, pfB
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 16
              movaps -64(pB0,ldb,2), rB2
           #elif KB == 16 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 16 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 16
              movaps -64(pB0,ldb3), rB3
           #elif KB == 16 && defined(BETA1)
           #endif
        #endif
        #if KB > 16
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 20
              movaps -48(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 20
              movaps -48(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 20
              movaps -48(pB0), rB0
           #elif KB == 20 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 20
              movaps -48(pB0,ldb), rB1
           #elif KB == 20 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 20 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 20
              movaps -48(pB0,ldb,2), rB2
           #elif KB == 20 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 20 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 20
              movaps -48(pB0,ldb3), rB3
           #elif KB == 20 && defined(BETA0)
           #endif
        #endif
        #if KB > 20
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 24
              movaps -32(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 24
              movaps -32(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 24
              movaps -32(pB0), rB0
           #elif KB == 24 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 24
              movaps -32(pB0,ldb), rB1
           #elif KB == 24 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 24 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 24
              movaps -32(pB0,ldb,2), rB2
           #elif KB == 24 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 24 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 24
              movaps -32(pB0,ldb3), rB3
           #elif KB == 24 && defined(BETA1)
           #endif
        #endif
        #if KB > 24
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 28
              movaps -16(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 28
              movaps -16(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 28
              movaps -16(pB0), rB0
           #elif KB == 28 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 28
              movaps -16(pB0,ldb), rB1
           #elif KB == 28 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 28 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 28
              movaps -16(pB0,ldb,2), rB2
           #elif KB == 28 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 28 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 28
              movaps -16(pB0,ldb3), rB3
           #elif KB == 28 && defined(BETA0)
           #endif
        #endif
        #if KB > 28
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 32
              movaps 0(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 32
              movaps 0(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 32
              movaps 0(pB0), rB0
           #elif KB == 32 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 32
              movaps 0(pB0,ldb), rB1
           #elif KB == 32 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 32 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 32
              movaps 0(pB0,ldb,2), rB2
           #elif KB == 32 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 32 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 32
              movaps 0(pB0,ldb3), rB3
           #elif KB == 32 && defined(BETA1)
           #endif
        #endif
        #if KB > 32
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 36
              movaps 16(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 36
              movaps 16(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 36
              movaps 16(pB0), rB0
           #elif KB == 36 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 36
              movaps 16(pB0,ldb), rB1
           #elif KB == 36 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 36 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 36
              movaps 16(pB0,ldb,2), rB2
           #elif KB == 36 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 36 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 36
              movaps 16(pB0,ldb3), rB3
           #elif KB == 36 && defined(BETA0)
           #endif
        #endif
        #if KB > 36
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 40
              movaps 32(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 40
              movaps 32(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 40
              movaps 32(pB0), rB0
           #elif KB == 40 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 40
              movaps 32(pB0,ldb), rB1
           #elif KB == 40 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 40 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 40
              movaps 32(pB0,ldb,2), rB2
           #elif KB == 40 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 40 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 40
              movaps 32(pB0,ldb3), rB3
           #elif KB == 40 && defined(BETA1)
           #endif
        #endif
        #if KB > 40
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 44
              movaps 48(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 44
              movaps 48(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 44
              movaps 48(pB0), rB0
           #elif KB == 44 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 44
              movaps 48(pB0,ldb), rB1
           #elif KB == 44 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 44 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 44
              movaps 48(pB0,ldb,2), rB2
           #elif KB == 44 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 44 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 44
              movaps 48(pB0,ldb3), rB3
           #elif KB == 44 && defined(BETA0)
           #endif
        #endif
        #if KB > 44
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 48
              movaps 64(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 48
              movaps 64(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 48
              movaps 64(pB0), rB0
           #elif KB == 48 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 48
              movaps 64(pB0,ldb), rB1
           #elif KB == 48 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 48 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 48
              movaps 64(pB0,ldb,2), rB2
           #elif KB == 48 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 48 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 48
              movaps 64(pB0,ldb3), rB3
           #elif KB == 48 && defined(BETA1)
           #endif
        #endif
        #if KB > 48
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 52
              movaps 80(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 52
              movaps 80(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 52
              movaps 80(pB0), rB0
           #elif KB == 52 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 52
              movaps 80(pB0,ldb), rB1
           #elif KB == 52 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 52 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 52
              movaps 80(pB0,ldb,2), rB2
           #elif KB == 52 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 52 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 52
              movaps 80(pB0,ldb3), rB3
           #elif KB == 52 && defined(BETA0)
           #endif
        #endif
        #if KB > 52
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 56
              movaps 96(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 56
              movaps 96(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 56
              movaps 96(pB0), rB0
           #elif KB == 56 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 56
              movaps 96(pB0,ldb), rB1
           #elif KB == 56 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 56 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 56
              movaps 96(pB0,ldb,2), rB2
           #elif KB == 56 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 56 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 56
              movaps 96(pB0,ldb3), rB3
           #elif KB == 56 && defined(BETA1)
           #endif
        #endif
        #if KB > 56
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 60
              movaps 112(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 60
              movaps 112(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 60
              movaps 112(pB0), rB0
           #elif KB == 60 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 60
              movaps 112(pB0,ldb), rB1
           #elif KB == 60 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 60 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 60
              movaps 112(pB0,ldb,2), rB2
           #elif KB == 60 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 60 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 60
              movaps 112(pB0,ldb3), rB3
           #elif KB == 60 && defined(BETA0)
           #endif
        #endif
        #if KB > 60
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 64
              movaps 128(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 64
              movaps 128(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 64
              movaps 128(pB0), rB0
           #elif KB == 64 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 64
              movaps 128(pB0,ldb), rB1
           #elif KB == 64 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 64 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 64
              movaps 128(pB0,ldb,2), rB2
           #elif KB == 64 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 64 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 64
              movaps 128(pB0,ldb3), rB3
           #elif KB == 64 && defined(BETA1)
           #endif
        #endif
        #if KB > 64
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 68
              movaps 144(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 68
              movaps 144(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 68
              movaps 144(pB0), rB0
           #elif KB == 68 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 68
              movaps 144(pB0,ldb), rB1
           #elif KB == 68 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 68 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 68
              movaps 144(pB0,ldb,2), rB2
           #elif KB == 68 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 68 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 68
              movaps 144(pB0,ldb3), rB3
           #elif KB == 68 && defined(BETA0)
           #endif
        #endif
        #if KB > 68
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 72
              movaps 160(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 72
              movaps 160(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 72
              movaps 160(pB0), rB0
           #elif KB == 72 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 72
              movaps 160(pB0,ldb), rB1
           #elif KB == 72 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 72 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 72
              movaps 160(pB0,ldb,2), rB2
           #elif KB == 72 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 72 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 72
              movaps 160(pB0,ldb3), rB3
           #elif KB == 72 && defined(BETA1)
           #endif
        #endif
        #if KB > 72
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 76
              movaps 176(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 76
              movaps 176(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 76
              movaps 176(pB0), rB0
           #elif KB == 76 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 76
              movaps 176(pB0,ldb), rB1
           #elif KB == 76 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 76 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 76
              movaps 176(pB0,ldb,2), rB2
           #elif KB == 76 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 76 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 76
              movaps 176(pB0,ldb3), rB3
           #elif KB == 76 && defined(BETA0)
           #endif
        #endif
        #if KB > 76
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 80
              movaps 192(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 80
              movaps 192(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 80
              movaps 192(pB0), rB0
           #elif KB == 80 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 80
              movaps 192(pB0,ldb), rB1
           #elif KB == 80 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 80 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 80
              movaps 192(pB0,ldb,2), rB2
           #elif KB == 80 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 80 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 80
              movaps 192(pB0,ldb3), rB3
           #elif KB == 80 && defined(BETA1)
           #endif
        #endif
        #if KB > 80
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 84
              movaps 208(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 84
              movaps 208(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 84
              movaps 208(pB0), rB0
           #elif KB == 84 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 84
              movaps 208(pB0,ldb), rB1
           #elif KB == 84 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 84 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 84
              movaps 208(pB0,ldb,2), rB2
           #elif KB == 84 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 84 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 84
              movaps 208(pB0,ldb3), rB3
           #elif KB == 84 && defined(BETA0)
           #endif
        #endif
        #if KB > 84
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 88
              movaps 224(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 88
              movaps 224(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 88
              movaps 224(pB0), rB0
           #elif KB == 88 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 88
              movaps 224(pB0,ldb), rB1
           #elif KB == 88 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 88 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 88
              movaps 224(pB0,ldb,2), rB2
           #elif KB == 88 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 88 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 88
              movaps 224(pB0,ldb3), rB3
           #elif KB == 88 && defined(BETA1)
           #endif
        #endif
        #if KB > 88
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 92
              movaps 240(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 92
              movaps 240(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 92
              movaps 240(pB0), rB0
           #elif KB == 92 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 92
              movaps 240(pB0,ldb), rB1
           #elif KB == 92 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 92 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 92
              movaps 240(pB0,ldb,2), rB2
           #elif KB == 92 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 92 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 92
              movaps 240(pB0,ldb3), rB3
           #elif KB == 92 && defined(BETA0)
           #endif
        #endif
        #if KB > 92
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 96
              movaps 256(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 96
              movaps 256(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 96
              movaps 256(pB0), rB0
           #elif KB == 96 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 96
              movaps 256(pB0,ldb), rB1
           #elif KB == 96 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 96 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 96
              movaps 256(pB0,ldb,2), rB2
           #elif KB == 96 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 96 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 96
              movaps 256(pB0,ldb3), rB3
           #elif KB == 96 && defined(BETA1)
           #endif
        #endif
        #if KB > 96
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 100
              movaps 272(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 100
              movaps 272(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 100
              movaps 272(pB0), rB0
           #elif KB == 100 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 100
              movaps 272(pB0,ldb), rB1
           #elif KB == 100 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 100 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 100
              movaps 272(pB0,ldb,2), rB2
           #elif KB == 100 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 100 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 100
              movaps 272(pB0,ldb3), rB3
           #elif KB == 100 && defined(BETA0)
           #endif
        #endif
        #if KB > 100
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 104
              movaps 288(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 104
              movaps 288(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 104
              movaps 288(pB0), rB0
           #elif KB == 104 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 104
              movaps 288(pB0,ldb), rB1
           #elif KB == 104 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 104 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 104
              movaps 288(pB0,ldb,2), rB2
           #elif KB == 104 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 104 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 104
              movaps 288(pB0,ldb3), rB3
           #elif KB == 104 && defined(BETA1)
           #endif
        #endif
        #if KB > 104
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 108
              movaps 304(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 108
              movaps 304(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 108
              movaps 304(pB0), rB0
           #elif KB == 108 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 108
              movaps 304(pB0,ldb), rB1
           #elif KB == 108 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 108 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 108
              movaps 304(pB0,ldb,2), rB2
           #elif KB == 108 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 108 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 108
              movaps 304(pB0,ldb3), rB3
           #elif KB == 108 && defined(BETA0)
           #endif
        #endif
        #if KB > 108
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 112
              movaps 320(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 112
              movaps 320(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 112
              movaps 320(pB0), rB0
           #elif KB == 112 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 112
              movaps 320(pB0,ldb), rB1
           #elif KB == 112 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 112 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 112
              movaps 320(pB0,ldb,2), rB2
           #elif KB == 112 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 112 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 112
              movaps 320(pB0,ldb3), rB3
           #elif KB == 112 && defined(BETA1)
           #endif
        #endif
        #if KB > 112
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 116
              movaps 336(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 116
              movaps 336(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 116
              movaps 336(pB0), rB0
           #elif KB == 116 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 116
              movaps 336(pB0,ldb), rB1
           #elif KB == 116 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 116 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 116
              movaps 336(pB0,ldb,2), rB2
           #elif KB == 116 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 116 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 116
              movaps 336(pB0,ldb3), rB3
           #elif KB == 116 && defined(BETA0)
           #endif
        #endif
        #if KB > 116
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 120
              movaps 352(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 120
              movaps 352(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 120
              movaps 352(pB0), rB0
           #elif KB == 120 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 120
              movaps 352(pB0,ldb), rB1
           #elif KB == 120 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 120 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 120
              movaps 352(pB0,ldb,2), rB2
           #elif KB == 120 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 120 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 120
              movaps 352(pB0,ldb3), rB3
           #elif KB == 120 && defined(BETA1)
           #endif
        #endif
        #if KB > 120
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 124
              movaps 368(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 124
              movaps 368(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 124
              movaps 368(pB0), rB0
           #elif KB == 124 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 124
              movaps 368(pB0,ldb), rB1
           #elif KB == 124 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 124 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 124
              movaps 368(pB0,ldb,2), rB2
           #elif KB == 124 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 124 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 124
              movaps 368(pB0,ldb3), rB3
           #elif KB == 124 && defined(BETA0)
           #endif
        #endif
        #if KB > 124
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 128
              movaps 384(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 128
              movaps 384(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 128
              movaps 384(pB0), rB0
           #elif KB == 128 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 128
              movaps 384(pB0,ldb), rB1
           #elif KB == 128 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 128 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 128
              movaps 384(pB0,ldb,2), rB2
           #elif KB == 128 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 128 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 128
              movaps 384(pB0,ldb3), rB3
           #elif KB == 128 && defined(BETA1)
           #endif
        #endif
        #if KB > 128
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 132
              movaps 400(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 132
              movaps 400(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 132
              movaps 400(pB0), rB0
           #elif KB == 132 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 132
              movaps 400(pB0,ldb), rB1
           #elif KB == 132 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 132 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 132
              movaps 400(pB0,ldb,2), rB2
           #elif KB == 132 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 132 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 132
              movaps 400(pB0,ldb3), rB3
           #elif KB == 132 && defined(BETA0)
           #endif
        #endif
        #if KB > 132
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 136
              movaps 416(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 136
              movaps 416(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 136
              movaps 416(pB0), rB0
           #elif KB == 136 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 136
              movaps 416(pB0,ldb), rB1
           #elif KB == 136 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 136 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 136
              movaps 416(pB0,ldb,2), rB2
           #elif KB == 136 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 136 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 136
              movaps 416(pB0,ldb3), rB3
           #elif KB == 136 && defined(BETA1)
           #endif
        #endif
        #if KB > 136
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 140
              movaps 432(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 140
              movaps 432(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 140
              movaps 432(pB0), rB0
           #elif KB == 140 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 140
              movaps 432(pB0,ldb), rB1
           #elif KB == 140 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 140 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 140
              movaps 432(pB0,ldb,2), rB2
           #elif KB == 140 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 140 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 140
              movaps 432(pB0,ldb3), rB3
           #elif KB == 140 && defined(BETA0)
           #endif
        #endif
        #if KB > 140
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 144
              movaps 448(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 144
              movaps 448(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 144
              movaps 448(pB0), rB0
           #elif KB == 144 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 144
              movaps 448(pB0,ldb), rB1
           #elif KB == 144 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 144 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 144
              movaps 448(pB0,ldb,2), rB2
           #elif KB == 144 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 144 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 144
              movaps 448(pB0,ldb3), rB3
           #elif KB == 144 && defined(BETA1)
           #endif
        #endif
        #if KB > 144
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 148
              movaps 464(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 148
              movaps 464(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 148
              movaps 464(pB0), rB0
           #elif KB == 148 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 148
              movaps 464(pB0,ldb), rB1
           #elif KB == 148 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 148 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 148
              movaps 464(pB0,ldb,2), rB2
           #elif KB == 148 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 148 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 148
              movaps 464(pB0,ldb3), rB3
           #elif KB == 148 && defined(BETA0)
           #endif
        #endif
        #if KB > 148
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 152
              movaps 480(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 152
              movaps 480(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 152
              movaps 480(pB0), rB0
           #elif KB == 152 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 152
              movaps 480(pB0,ldb), rB1
           #elif KB == 152 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 152 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 152
              movaps 480(pB0,ldb,2), rB2
           #elif KB == 152 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 152 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 152
              movaps 480(pB0,ldb3), rB3
           #elif KB == 152 && defined(BETA1)
           #endif
        #endif
        #if KB > 152
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 156
              movaps 496(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 156
              movaps 496(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 156
              movaps 496(pB0), rB0
           #elif KB == 156 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 156
              movaps 496(pB0,ldb), rB1
           #elif KB == 156 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 156 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 156
              movaps 496(pB0,ldb,2), rB2
           #elif KB == 156 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 156 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 156
              movaps 496(pB0,ldb3), rB3
           #elif KB == 156 && defined(BETA0)
           #endif
        #endif
        #if KB > 156
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 160
              movaps 512(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 160
              movaps 512(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 160
              movaps 512(pB0), rB0
           #elif KB == 160 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 160
              movaps 512(pB0,ldb), rB1
           #elif KB == 160 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 160 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 160
              movaps 512(pB0,ldb,2), rB2
           #elif KB == 160 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 160 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 160
              movaps 512(pB0,ldb3), rB3
           #elif KB == 160 && defined(BETA1)
           #endif
        #endif
        #if KB > 160
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 164
              movaps 528(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 164
              movaps 528(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 164
              movaps 528(pB0), rB0
           #elif KB == 164 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 164
              movaps 528(pB0,ldb), rB1
           #elif KB == 164 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 164 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 164
              movaps 528(pB0,ldb,2), rB2
           #elif KB == 164 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 164 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 164
              movaps 528(pB0,ldb3), rB3
           #elif KB == 164 && defined(BETA0)
           #endif
        #endif
        #if KB > 164
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 168
              movaps 544(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 168
              movaps 544(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 168
              movaps 544(pB0), rB0
           #elif KB == 168 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 168
              movaps 544(pB0,ldb), rB1
           #elif KB == 168 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 168 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 168
              movaps 544(pB0,ldb,2), rB2
           #elif KB == 168 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 168 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 168
              movaps 544(pB0,ldb3), rB3
           #elif KB == 168 && defined(BETA1)
           #endif
        #endif
        #if KB > 168
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 172
              movaps 560(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 172
              movaps 560(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 172
              movaps 560(pB0), rB0
           #elif KB == 172 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 172
              movaps 560(pB0,ldb), rB1
           #elif KB == 172 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 172 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 172
              movaps 560(pB0,ldb,2), rB2
           #elif KB == 172 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 172 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 172
              movaps 560(pB0,ldb3), rB3
           #elif KB == 172 && defined(BETA0)
           #endif
        #endif
        #if KB > 172
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 176
              movaps 576(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 176
              movaps 576(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 176
              movaps 576(pB0), rB0
           #elif KB == 176 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 176
              movaps 576(pB0,ldb), rB1
           #elif KB == 176 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 176 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 176
              movaps 576(pB0,ldb,2), rB2
           #elif KB == 176 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 176 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 176
              movaps 576(pB0,ldb3), rB3
           #elif KB == 176 && defined(BETA1)
           #endif
        #endif
        #if KB > 176
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 180
              movaps 592(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 180
              movaps 592(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 180
              movaps 592(pB0), rB0
           #elif KB == 180 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 180
              movaps 592(pB0,ldb), rB1
           #elif KB == 180 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 180 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 180
              movaps 592(pB0,ldb,2), rB2
           #elif KB == 180 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 180 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 180
              movaps 592(pB0,ldb3), rB3
           #elif KB == 180 && defined(BETA0)
           #endif
        #endif
        #if KB > 180
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 184
              movaps 608(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 184
              movaps 608(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 184
              movaps 608(pB0), rB0
           #elif KB == 184 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 184
              movaps 608(pB0,ldb), rB1
           #elif KB == 184 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 184 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 184
              movaps 608(pB0,ldb,2), rB2
           #elif KB == 184 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 184 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 184
              movaps 608(pB0,ldb3), rB3
           #elif KB == 184 && defined(BETA1)
           #endif
        #endif
        #if KB > 184
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 188
              movaps 624(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 188
              movaps 624(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 188
              movaps 624(pB0), rB0
           #elif KB == 188 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 188
              movaps 624(pB0,ldb), rB1
           #elif KB == 188 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 188 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 188
              movaps 624(pB0,ldb,2), rB2
           #elif KB == 188 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 188 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 188
              movaps 624(pB0,ldb3), rB3
           #elif KB == 188 && defined(BETA0)
           #endif
        #endif
        #if KB > 188
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 192
              movaps 640(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 192
              movaps 640(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 192
              movaps 640(pB0), rB0
           #elif KB == 192 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 192
              movaps 640(pB0,ldb), rB1
           #elif KB == 192 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 192 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 192
              movaps 640(pB0,ldb,2), rB2
           #elif KB == 192 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 192 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 192
              movaps 640(pB0,ldb3), rB3
           #elif KB == 192 && defined(BETA1)
           #endif
        #endif
        #if KB > 192
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 196
              movaps 656(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 196
              movaps 656(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 196
              movaps 656(pB0), rB0
           #elif KB == 196 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 196
              movaps 656(pB0,ldb), rB1
           #elif KB == 196 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 196 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 196
              movaps 656(pB0,ldb,2), rB2
           #elif KB == 196 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 196 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 196
              movaps 656(pB0,ldb3), rB3
           #elif KB == 196 && defined(BETA0)
           #endif
        #endif
        #if KB > 196
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 200
              movaps 672(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 200
              movaps 672(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 200
              movaps 672(pB0), rB0
           #elif KB == 200 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 200
              movaps 672(pB0,ldb), rB1
           #elif KB == 200 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 200 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 200
              movaps 672(pB0,ldb,2), rB2
           #elif KB == 200 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 200 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 200
              movaps 672(pB0,ldb3), rB3
           #elif KB == 200 && defined(BETA1)
           #endif
        #endif
        #if KB > 200
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 204
              movaps 688(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 204
              movaps 688(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 204
              movaps 688(pB0), rB0
           #elif KB == 204 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 204
              movaps 688(pB0,ldb), rB1
           #elif KB == 204 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 204 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 204
              movaps 688(pB0,ldb,2), rB2
           #elif KB == 204 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 204 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 204
              movaps 688(pB0,ldb3), rB3
           #elif KB == 204 && defined(BETA0)
           #endif
        #endif
        #if KB > 204
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 208
              movaps 704(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 208
              movaps 704(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 208
              movaps 704(pB0), rB0
           #elif KB == 208 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 208
              movaps 704(pB0,ldb), rB1
           #elif KB == 208 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 208 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 208
              movaps 704(pB0,ldb,2), rB2
           #elif KB == 208 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 208 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 208
              movaps 704(pB0,ldb3), rB3
           #elif KB == 208 && defined(BETA1)
           #endif
        #endif
        #if KB > 208
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 212
              movaps 720(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 212
              movaps 720(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 212
              movaps 720(pB0), rB0
           #elif KB == 212 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 212
              movaps 720(pB0,ldb), rB1
           #elif KB == 212 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 212 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 212
              movaps 720(pB0,ldb,2), rB2
           #elif KB == 212 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 212 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 212
              movaps 720(pB0,ldb3), rB3
           #elif KB == 212 && defined(BETA0)
           #endif
        #endif
        #if KB > 212
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 216
              movaps 736(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 216
              movaps 736(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 216
              movaps 736(pB0), rB0
           #elif KB == 216 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 216
              movaps 736(pB0,ldb), rB1
           #elif KB == 216 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 216 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 216
              movaps 736(pB0,ldb,2), rB2
           #elif KB == 216 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 216 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 216
              movaps 736(pB0,ldb3), rB3
           #elif KB == 216 && defined(BETA1)
           #endif
        #endif
        #if KB > 216
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 220
              movaps 752(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 220
              movaps 752(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 220
              movaps 752(pB0), rB0
           #elif KB == 220 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 220
              movaps 752(pB0,ldb), rB1
           #elif KB == 220 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 220 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 220
              movaps 752(pB0,ldb,2), rB2
           #elif KB == 220 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 220 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 220
              movaps 752(pB0,ldb3), rB3
           #elif KB == 220 && defined(BETA0)
           #endif
        #endif
        #if KB > 220
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 224
              movaps 768(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 224
              movaps 768(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 224
              movaps 768(pB0), rB0
           #elif KB == 224 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 224
              movaps 768(pB0,ldb), rB1
           #elif KB == 224 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 224 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 224
              movaps 768(pB0,ldb,2), rB2
           #elif KB == 224 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 224 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 224
              movaps 768(pB0,ldb3), rB3
           #elif KB == 224 && defined(BETA1)
           #endif
        #endif
        #if KB > 224
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 228
              movaps 784(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 228
              movaps 784(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 228
              movaps 784(pB0), rB0
           #elif KB == 228 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 228
              movaps 784(pB0,ldb), rB1
           #elif KB == 228 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 228 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 228
              movaps 784(pB0,ldb,2), rB2
           #elif KB == 228 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 228 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 228
              movaps 784(pB0,ldb3), rB3
           #elif KB == 228 && defined(BETA0)
           #endif
        #endif
        #if KB > 228
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 232
              movaps 800(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 232
              movaps 800(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 232
              movaps 800(pB0), rB0
           #elif KB == 232 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 232
              movaps 800(pB0,ldb), rB1
           #elif KB == 232 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 232 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 232
              movaps 800(pB0,ldb,2), rB2
           #elif KB == 232 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 232 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 232
              movaps 800(pB0,ldb3), rB3
           #elif KB == 232 && defined(BETA1)
           #endif
        #endif
        #if KB > 232
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 236
              movaps 816(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 236
              movaps 816(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 236
              movaps 816(pB0), rB0
           #elif KB == 236 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 236
              movaps 816(pB0,ldb), rB1
           #elif KB == 236 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 236 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 236
              movaps 816(pB0,ldb,2), rB2
           #elif KB == 236 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 236 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 236
              movaps 816(pB0,ldb3), rB3
           #elif KB == 236 && defined(BETA0)
           #endif
        #endif
        #if KB > 236
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 240
              movaps 832(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 240
              movaps 832(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 240
              movaps 832(pB0), rB0
           #elif KB == 240 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 240
              movaps 832(pB0,ldb), rB1
           #elif KB == 240 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 240 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 240
              movaps 832(pB0,ldb,2), rB2
           #elif KB == 240 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 240 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 240
              movaps 832(pB0,ldb3), rB3
           #elif KB == 240 && defined(BETA1)
           #endif
        #endif
        #if KB > 240
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 244
              movaps 848(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 244
              movaps 848(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 244
              movaps 848(pB0), rB0
           #elif KB == 244 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 244
              movaps 848(pB0,ldb), rB1
           #elif KB == 244 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 244 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 244
              movaps 848(pB0,ldb,2), rB2
           #elif KB == 244 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 244 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 244
              movaps 848(pB0,ldb3), rB3
           #elif KB == 244 && defined(BETA0)
           #endif
        #endif
        #if KB > 244
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 248
              movaps 864(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 248
              movaps 864(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 248
              movaps 864(pB0), rB0
           #elif KB == 248 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 248
              movaps 864(pB0,ldb), rB1
           #elif KB == 248 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 248 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 248
              movaps 864(pB0,ldb,2), rB2
           #elif KB == 248 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 248 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 248
              movaps 864(pB0,ldb3), rB3
           #elif KB == 248 && defined(BETA1)
           #endif
        #endif
        #if KB > 248
           vfmaddps rC00, rA0, rB0, rC00
           #if KB > 252
              movaps 880(pA0), ra0
           #endif
           vfmaddps rC10, rA1, rB0, rC10
           #if KB > 252
              movaps 880(pA0,lda), ra1
           #endif
           vfmaddps rC01, rA0, rB1, rC01
           #if KB > 252
              movaps 880(pB0), rB0
           #elif KB == 252 && defined(BETA1)
           #endif
           vfmaddps rC11, rA1, rB1, rC11
           #if KB > 252
              movaps 880(pB0,ldb), rB1
           #elif KB == 252 && defined(BETA1)
           #endif
           vfmaddps rC02, rA0, rB2, rC02
           #if KB == 252 && defined(BETA0)
           #endif
           vfmaddps rC12, rA1, rB2, rC12
           #if KB > 252
              movaps 880(pB0,ldb,2), rB2
           #elif KB == 252 && defined(BETA1)
           #endif
           vfmaddps rC03, rA0, rB3, rC03
           #if KB == 252 && defined(BETA1)
           #endif
           vfmaddps rC13, rA1, rB3, rC13
           #if KB > 252
              movaps 880(pB0,ldb3), rB3
           #elif KB == 252 && defined(BETA0)
           #endif
        #endif
        #if KB > 252
           vfmaddps rC00, ra0, rB0, rC00
           #if KB > 256
              movaps 896(pA0), rA0
           #endif
           vfmaddps rC10, ra1, rB0, rC10
           #if KB > 256
              movaps 896(pA0,lda), rA1
           #endif
           vfmaddps rC01, ra0, rB1, rC01
           #if KB > 256
              movaps 896(pB0), rB0
           #elif KB == 256 && defined(BETA1)
           #endif
           vfmaddps rC11, ra1, rB1, rC11
           #if KB > 256
              movaps 896(pB0,ldb), rB1
           #elif KB == 256 && defined(BETA1)
           #endif
           vfmaddps rC02, ra0, rB2, rC02
           #if KB == 256 && defined(BETA1)
           #endif
           vfmaddps rC12, ra1, rB2, rC12
           #if KB > 256
              movaps 896(pB0,ldb,2), rB2
           #elif KB == 256 && defined(BETA1)
           #endif
           vfmaddps rC03, ra0, rB3, rC03
           #if KB == 256 && defined(BETA1)
           #endif
           vfmaddps rC13, ra1, rB3, rC13
           #if KB > 256
              movaps 896(pB0,ldb3), rB3
           #elif KB == 256 && defined(BETA1)
           #endif
        #endif
/*
 *       Add in original C if necessary
 */
         #ifdef BETA1
            addss (pC0), rC00
            addss CMUL(4)(pC0), rC10
            addss (pC0,ldc), rC01
            addss CMUL(4)(pC0,ldc), rC11
            addss (pC0,ldc,2), rC02
            addss CMUL(4)(pC0,ldc,2), rC12
            addss (pC0,ldc3), rC03
            addss CMUL(4)(pC0,ldc3), rC13
         #elif defined(BETAX)
            movaps BETAOFF(%rsp), rA0
            xorps ra0, ra0
            vfmaddss ra0, (pC0), rA0, rB0
            vfmaddss ra0, CMUL(4)(pC0), rA0, rB1
            unpcklps rB1, rB0                   /* rB0={0,0,c10,c00} */
            vfmaddss ra0, (pC0,ldc), rA0, rB2
            vfmaddss ra0, CMUL(4)(pC0,ldc), rA0, rB3
            unpcklps rB3, rB2                   /* rB2={0,0,c11,c01} */
            movlhps  rB2, rB0                   /* rB0={c11,c01,c10,c00} */
            vfmaddss ra0, (pC0,ldc,2), rA0, rB1
            vfmaddss ra0, CMUL(4)(pC0,ldc,2), rA0, rB3
            unpcklps rB3, rB1                   /* rB1={0,0,c12,c02} */
            vfmaddss ra0, (pC0,ldc3), rA0, rA1
            vfmaddss ra0, CMUL(4)(pC0,ldc3), rA0, ra1
            unpcklps ra1, rA1                   /* rA1={0,0,c13,c03} */
            movlhps  rA1, rB1                   /* rB1={c13,c03,c12,c02} */
         #endif
/*
 *       K-loop finished, sum up vectors
 */
         prefetcht1 (pfA)
         haddps rC10, rC00    /* rC00 = {c10cd, c10ab, c00cd, c00ab} */
         add $64, pfA
         haddps rC11, rC01    /* rC01 = {c11cd, c11ab, c01cd, c01ab} */
         haddps rC12, rC02    /* rC02 = {c12cd, c12ab, c02cd, c02ab} */
         haddps rC13, rC03    /* rC03 = {c13cd, c13ab, c03cd, c03ab} */
         haddps rC01, rC00    /* rC00 = {c11a-d, c01a-d, c10a-d, c00a-d} */
         haddps rC03, rC02    /* rC02 = {c13a-d, c03a-d, c12a-d, c02a-d} */
         #ifdef BETAX
            addps rB0, rC00
            addps rB1, rC02
         #endif

                                        /* rC00 = {c11, c01, c10, c00} */
                                        /* rC02 = {c13, c03, c12, c02} */
         movss rC00, (pC0)
         pshufd $0xB1, rC00, rC01       /* rC01 = {C01, c11, c00, c10} */
         movss rC02, (pC0,ldc,2)
         pshufd $0xB1, rC02, rC03       /* rC03 = {C03, c13, c02, c12} */
         movss rC01, CMUL(4)(pC0)
         movhlps rC00, rC00             /* rC00 = {c11, c01, c11, c01} */
         movss rC03, CMUL(4)(pC0,ldc,2)
         movhlps rC02, rC02             /* rC02 = {c13, c03, c13, c03} */
         movss rC00, (pC0,ldc)
         movhlps rC01, rC01             /* rC01 = {c01, c11, c01, c11} */
         movss rC02, (pC0,ldc3)
         movhlps rC03, rC03             /* rC03 = {c03, c13, c03, c13} */
         movss rC01, CMUL(4)(pC0,ldc)
         movss rC03, CMUL(4)(pC0,ldc3)

         lea (pA0,lda,2), pA0
         add $2*CMUL(4), pC0

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
