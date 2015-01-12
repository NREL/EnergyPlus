/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2006 R. Clint Whaley
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

#include "atlas_asm.h"
#ifndef ATL_SSE3
   #error "This routine requires SSE3!"
#endif
/*
 * This routine optimized for Core2Duo, which has a relatively weak frontend,
 * so we have to be very careful about alignment, and things seem to work
 * better if we keep a register block within 128 bytes
 */
#if !defined(MB)
   #define MB 0
#endif
#if !defined(NB)
   #define NB 0
#endif
#if !defined(KB)
   #define KB 0
#endif
#if KB == 0
   #error "KB must be compile time constant!"
#endif
#if KB/4*4 != KB
   #error "KB must be a multiple of 4!"
#endif
#if KB > 128
   #error "KB must be <= 128!"
#endif
#if KB != 0 && (MB/10)*10 != MB
   #error "MB must be a multiple of 10!"
#endif
/*
 * Floating point (SSE) register usage
 */
#define rA0     %xmm0
#define rB0     %xmm1
#define rC0     %xmm2
#define rC1     %xmm3
#define rC2     %xmm4
#define rC3     %xmm5
#define rC4     %xmm6
#define rC5     %xmm7
#define rC6     %xmm8
#define rC7     %xmm9
#define rC8     %xmm10
#define rC9     %xmm11
#define rCa     %xmm12
#define rCb     %xmm13
#define rCc     %xmm14
#define rBETA   %xmm15
/*
 * Integer register usage
 */
#define pB0     %rax
#define pA2     %rcx
#define pA7     %rbx
#define nlda    %rbp
#define lda     %rdi
#define pfA     %rsi
#define ldb     %rdx
#define II      %r8
#define JJ      %r9
#define M0      %r10
#define pC0     %r11
#define incAn   %r12
#define incCn   %r13
#define incAm   %r14

/*
 * Prefetch defines
 */
#if 1
   #define pref2(mem) prefetcht1        mem
   #define prefB(mem) prefetcht1        mem
   #define prefC(mem) prefetcht0        mem
#else
   #define pref2(mem)
   #define prefB(mem)
   #define prefC(mem)
#endif

#if MB == 0 || defined(ATL_OS_SunOS)  /* retarded gcc on SunOS has no divis */
   #define PFAINC 64
#else
   #define PFAINC ((MB*4+MB/4-1)/(MB/4))
#endif

#ifdef SCPLX
   #define CMUL(arg_) 2*arg_
#else
   #define CMUL(arg_) arg_
#endif

/*
                      %rdi         %rsi         %rdx             %xmm0
 void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha,
                       %rcx            %r8            %r9              8
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                        %xmm1       16             24
                 const TYPE beta, TYPE *C, const int ldc)
*/
.text
.global ATL_asmdecor(ATL_USERMM)
ALIGN128
ATL_asmdecor(ATL_USERMM):
/*
 *      Save callee-saved iregs
 */
        movq    %rbp, -8(%rsp)
        movq    %rbx, -16(%rsp)
        movq    %r12, -24(%rsp)
        movq    %r13, -32(%rsp)
        movq    %r14, -40(%rsp)
/*        movq    %r15, -48(%rsp) */
/*
 *      Load parameters
 */
        movq    %r9, pB0
        movq    %rdi, M0
        movq    %rsi, JJ
        movq    %r8, lda
        movslq  8(%rsp), ldb
        movq    16(%rsp), pC0
        movslq  24(%rsp), incCn
#ifdef BETAX
        pshufd  $0x00, %xmm1, rBETA     # rBETA = {beta,beta,beta,beta}
#endif
/*
 *      ldx *= sizeof;
 */
        shl     $2, lda
        shl     $2, ldb
        movq    lda, nlda
        neg     nlda
/*
 *      incAm = 10*lda - (increment done in K-loop)
 */
#if KB <= 64 || 1                           /* did no += 256 increment */
        lea     (lda,lda,8),incAm           /* incAm = lda*9 */
        lea     (incAm, lda), incAm         /* incAm = lda*10 */
#elif KB <= 128                             /* did one += 256 increment */
        lea     -128(lda,lda,8),incAm       /* incAm = lda*9 - 128 */
        lea     -128(incAm, lda), incAm     /* incAm = lda*10 - 256 */
#endif
/*
 *      pA2 = pA + 2*lda + 128;  pA7 = pA+7*lda + 128;  pB0 += 128
 */
        lea     (lda,lda,2), pA7        /* pA7 = 3*lda */
        sub     $-128, pA2              /* pA2 = pA2 + 128 */
        lea     (pA7,lda,4), pA7        /* pA7 = 7*lda */
        add     pA2, pA7                /* pA7 = pA0 + 7*lda + 128 */
        sub     $-128, pB0              /* pB0 = pB0 + 128 */
        lea     (pA2, lda,2), pA2       /* pA2 = pA0 + 2*lda + 128 */
/*
 *      incAn = lda*M*sizeof
 */
        movq    M0, incAn
        imulq   lda, incAn              /* incAn = lda*M */
        lea     (pA2,incAn), pfA        /* pfA = pA0+2*lda + M*lda + 128 */
        lea     -128(pfA,nlda,2), pfA   /* pfA = pA0 + M*lda */
/*
 *      incCn = (ldc-M)*sizeof
 */
        sub     M0, incCn
#ifdef SCPLX
        shl     $3, incCn
#else
        shl     $2, incCn
#endif
NLOOP:
        movq    M0, II
        prefB(-128(pB0,ldb))
#if KB > 32
        prefB((pB0,ldb))
#endif
#if KB > 64
        prefB(128(pB0,ldb))
#endif
#if KB > 96
        prefB(256(pB0,ldb))
#endif
ALIGN16
MLOOP:
        prefC((pC0))
#define MY_ALIGN
/*
 * Start the KLOOP
 */
	movaps	-128(pB0), rB0
					pref2((pfA))
					add $PFAINC, pfA
	movaps	-128(pA2,nlda,2), rC0
	mulps	rB0,rC0
	movaps	-128(pA2,nlda), rC1
	mulps	rB0,rC1
	movaps	-128(pA2), rC2
	mulps	rB0,rC2
	movaps	-128(pA2,lda), rC3
	mulps	rB0,rC3
	movaps	-128(pA2,lda,2), rC4
	mulps	rB0,rC4
	movaps	-128(pA7,nlda,2), rC5
	mulps	rB0,rC5
	movaps	-128(pA7,nlda), rC6
	mulps	rB0,rC6
	movaps	-128(pA7), rC7
	mulps	rB0,rC7
	movaps	-128(pA7,lda), rC8
	mulps	rB0,rC8
	movaps	-128(pA7,lda,2), rC9
	mulps	rB0,rC9
#ifdef BETA0
        nop
        nop
        nop
#elif defined(BETA1)
#endif
#if KB > 4
	movaps	16-128(pB0), rB0
	movaps	16-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	16-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	16-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	16-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	16-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	16-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	16-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	16-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	16-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	16-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 8
	MY_ALIGN
	movaps	32-128(pB0), rB0
	movaps	32-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	32-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	32-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	32-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	32-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	32-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	32-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	32-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	32-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	32-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 12
	MY_ALIGN
	movaps	48-128(pB0), rB0
	movaps	48-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	48-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	48-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	48-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	48-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	48-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	48-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	48-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	48-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	48-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 16
	MY_ALIGN
	movaps	64-128(pB0), rB0
	movaps	64-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	64-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	64-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	64-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	64-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	64-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	64-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	64-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	64-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	64-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 20
	MY_ALIGN
	movaps	80-128(pB0), rB0
	movaps	80-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	80-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	80-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	80-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	80-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	80-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	80-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	80-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	80-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	80-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 24
	MY_ALIGN
	movaps	96-128(pB0), rB0
	movaps	96-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	96-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	96-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	96-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	96-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	96-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	96-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	96-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	96-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	96-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 28
	MY_ALIGN
	movaps	112-128(pB0), rB0
	movaps	112-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	112-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	112-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	112-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	112-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	112-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	112-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	112-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	112-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	112-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif
#ifndef BETA0                           /* cplx = XX c1 XX c0 */
        movups  (pC0), rCa              /* rCa  = c3 c2 c1 c0 */
#endif

#if KB > 32
	MY_ALIGN
	movaps	128-128(pB0), rB0
	movaps	128-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	128-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	128-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	128-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	128-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	128-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	128-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	128-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	128-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	128-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#ifndef BETA0
   #ifdef SCPLX
        movups  16(pC0), rCc            /* rCc  = XX c3 XX c2 */
   #else
        movups  16(pC0), rCb            /* rCb  = c7 c6 c5 c4 */
   #endif
#endif
#if KB > 36
	MY_ALIGN
	movaps	144-128(pB0), rB0
	movaps	144-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	144-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	144-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	144-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	144-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	144-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	144-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	144-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	144-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	144-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif
#ifndef BETA0
   #ifdef SCPLX
        movups  32(pC0), rCb            /* rCb  = XX c5 XX c4 */
   #else
        movups  32(pC0), rCc            /* rCc  = XX XX c9 c8 */
   #endif
#endif
#if defined(SCPLX) && !defined(BETA0)
        shufps  $0x88, rCc, rCa   	/* rCa = c3 c2 c1 c0 */
#endif

#if KB > 40
	MY_ALIGN
	movaps	160-128(pB0), rB0
	movaps	160-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	160-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	160-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	160-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	160-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	160-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	160-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	160-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	160-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	160-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 44
	MY_ALIGN
	movaps	176-128(pB0), rB0
	movaps	176-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	176-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	176-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	176-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	176-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	176-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	176-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	176-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	176-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	176-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 48
	MY_ALIGN
	movaps	192-128(pB0), rB0
	movaps	192-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	192-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	192-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	192-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	192-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	192-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	192-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	192-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	192-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	192-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 52
	MY_ALIGN
	movaps	208-128(pB0), rB0
	movaps	208-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	208-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	208-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	208-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	208-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	208-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	208-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	208-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	208-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	208-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 56
	MY_ALIGN
	movaps	224-128(pB0), rB0
	movaps	224-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	224-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	224-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	224-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	224-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	224-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	224-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	224-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	224-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	224-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 60
	MY_ALIGN
	movaps	240-128(pB0), rB0
	movaps	240-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	240-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	240-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	240-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	240-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	240-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	240-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	240-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	240-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	240-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 64
	MY_ALIGN
	movaps	256-128(pB0), rB0
	movaps	256-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	256-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	256-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	256-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	256-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	256-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	256-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	256-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	256-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	256-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 68
	MY_ALIGN
	movaps	272-128(pB0), rB0
	movaps	272-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	272-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	272-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	272-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	272-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	272-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	272-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	272-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	272-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	272-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 72
	MY_ALIGN
	movaps	288-128(pB0), rB0
	movaps	288-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	288-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	288-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	288-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	288-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	288-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	288-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	288-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	288-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	288-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 76
	MY_ALIGN
	movaps	304-128(pB0), rB0
	movaps	304-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	304-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	304-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	304-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	304-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	304-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	304-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	304-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	304-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	304-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 80
	MY_ALIGN
	movaps	320-128(pB0), rB0
	movaps	320-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	320-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	320-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	320-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	320-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	320-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	320-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	320-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	320-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	320-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 84
	MY_ALIGN
	movaps	336-128(pB0), rB0
	movaps	336-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	336-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	336-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	336-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	336-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	336-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	336-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	336-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	336-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	336-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 88
	MY_ALIGN
	movaps	352-128(pB0), rB0
	movaps	352-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	352-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	352-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	352-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	352-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	352-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	352-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	352-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	352-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	352-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 92
	MY_ALIGN
	movaps	368-128(pB0), rB0
	movaps	368-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	368-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	368-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	368-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	368-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	368-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	368-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	368-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	368-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	368-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 96
	MY_ALIGN
	movaps	384-128(pB0), rB0
	movaps	384-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	384-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	384-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	384-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	384-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	384-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	384-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	384-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	384-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	384-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 100
	MY_ALIGN
	movaps	400-128(pB0), rB0
	movaps	400-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	400-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	400-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	400-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	400-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	400-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	400-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	400-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	400-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	400-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 104
	MY_ALIGN
	movaps	416-128(pB0), rB0
	movaps	416-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	416-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	416-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	416-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	416-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	416-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	416-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	416-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	416-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	416-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 108
	MY_ALIGN
	movaps	432-128(pB0), rB0
	movaps	432-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	432-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	432-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	432-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	432-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	432-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	432-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	432-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	432-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	432-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 112
	MY_ALIGN
	movaps	448-128(pB0), rB0
	movaps	448-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	448-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	448-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	448-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	448-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	448-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	448-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	448-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	448-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	448-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 116
	MY_ALIGN
	movaps	464-128(pB0), rB0
	movaps	464-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	464-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	464-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	464-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	464-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	464-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	464-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	464-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	464-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	464-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 120
	MY_ALIGN
	movaps	480-128(pB0), rB0
	movaps	480-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	480-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	480-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	480-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	480-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	480-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	480-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	480-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	480-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	480-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

#if KB > 124
	MY_ALIGN
	movaps	496-128(pB0), rB0
	movaps	496-128(pA2,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC0
	movaps	496-128(pA2,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC1
	movaps	496-128(pA2), rA0
	mulps	rB0,rA0
	addps	rA0,rC2
	movaps	496-128(pA2,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC3
	movaps	496-128(pA2,lda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC4
	movaps	496-128(pA7,nlda,2), rA0
	mulps	rB0,rA0
	addps	rA0,rC5
	movaps	496-128(pA7,nlda), rA0
	mulps	rB0,rA0
	addps	rA0,rC6
	movaps	496-128(pA7), rA0
	mulps	rB0,rA0
	addps	rA0,rC7
	movaps	496-128(pA7,lda), rA0
	mulps	rB0,rA0
	addps	rA0,rC8
	mulps	496-128(pA7,lda,2), rB0
	addps	rB0,rC9
#endif

/*
 * End KLOOP
 */
                                        /* rCa = XX c1 XX c0 */
                                        /* rCc = XX c3 XX c2 */
                                        /* rCb = XX c5 XX c4 */
#if defined(SCPLX) && !defined(BETA0)
        movups  48(pC0), rCc            /* rCc = XX c7 XX c6 */
#endif
        haddps  rC1, rC0                /* rC0 = c1cd   c1ab   c0cd   c0ab */
#ifdef BETAX
        mulps   rBETA, rCa
#endif
#if defined(SCPLX) && !defined(BETA0)
        shufps  $0x88, rCc, rCb   	/* rCb = c7 c6 c5 c4 */
#endif
#if defined(SCPLX) && !defined(BETA0)
        movups  64(pC0), rCc            /* rCc = XX c9 XX c8 */
#endif
        haddps  rC3, rC2                /* rC2 = c3cd   c3ab   c2cd   c2ab */
#if defined(SCPLX) && !defined(BETA0)
        shufps  $0x88, rCc, rCc   	/* rCc = c9 c8 c9 c8 */
#endif

#ifdef BETAX
        mulps   rBETA, rCb
#endif
#ifdef BETAX
        mulps   rBETA, rCc
#endif
        haddps  rC5, rC4                /* rC4 = c5cd   c5ab   c4cd   c4ab */
        haddps  rC7, rC6                /* rC6 = c7cd   c7ab   c6cd   c6ab */
        haddps  rC9, rC8                /* rC8 = c9cd   c9ab   c8cd   c8ab */
        haddps  rC2, rC0                /* rC0 = c3abcd c2abcd c1abcd c0abcd */
   #ifndef BETA0
        addps   rCa, rC0
   #endif
        haddps  rC6, rC4                /* rC4 = c7abcd c6abcd c5abcd c4abcd */
   #ifndef BETA0
        addps   rCb, rC4
   #endif
        haddps  rC8, rC8                /* rC8 = c9abcd c8abcd c9abcd c8abcd */
   #ifndef BETA0
        addps   rCc, rC8
   #endif
#ifdef SCPLX
/*      pshufd  $0b0111 0001 */
        pshufd  $0x71, rC0, rCa         /* rCa = c1 c3 c0 c1 */
        movss   rC0, (pC0)
        pshufd  $0x71, rC4, rCb         /* rCb = c5 c7 c4 c5 */
        movss   rC4, 4*8(pC0)
        pshufd  $0x71, rC8, rCc         /* rCc = c9 c9 c8 c9 */
        movss   rC8, 8*8(pC0)
        movhlps rC0, rC0                /* rC0 = c3 c2 c3 c2 */
        movss   rCa, 1*8(pC0)
        movhlps rC4, rC4                /* rC4 = c7 c6 c7 c6 */
        movss   rCb, 5*8(pC0)
        movhlps rCa, rCa                /* rCa = c1 c3 c1 c3 */
        movss   rCc, 9*8(pC0)
        movhlps rCb, rCb                /* rCb = c5 c7 c5 c7 */
        movss   rC0, 2*8(pC0)
        movss   rC4, 6*8(pC0)
        movss   rCa, 3*8(pC0)
        movss   rCb, 7*8(pC0)
#else
        movups  rC0, (pC0)
        movups  rC4, 16(pC0)
	pshufd	$0xE5, rC8, rC9
/*        pshufd  $0b11100101, rC8, rC9 */
        movss   rC8, 32(pC0)
        movss   rC9, 36(pC0)
#endif
        add     incAm, pA2
        add     incAm, pA7
        addq    $10*CMUL(4), pC0
        subq    $10, II
        jnz     MLOOP

        sub     incAn, pA2
        sub     incAn, pA7
        add     incCn, pC0
        add     ldb, pB0
        sub     $1, JJ
        jnz     NLOOP
/*
 *      Restore regs & return (DONE)
 */
        movq    -8(%rsp), %rbp
        movq    -16(%rsp), %rbx
        movq    -24(%rsp), %r12
        movq    -32(%rsp), %r13
        movq    -40(%rsp), %r14
/*        movq    -48(%rsp), %r15 */
        ret
