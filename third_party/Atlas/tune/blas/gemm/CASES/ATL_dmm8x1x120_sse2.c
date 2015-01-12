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
 * This routine designed for Core2, which seems to have relatively few
 * reservation stations on both the VPUs and ld/st, so we make it so
 * we do at most 8 computations per block, and reduce outstanding ld/st
 * over similar x86-64 code for AMD archs
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
/*
 *Register usage
 */
#define pA0     %rcx
#define lda     %rbx
#define lda3    %rbp
#define lda5    %rdx
#define lda7    %rdi
#define pB0     %rax
#define pC0     %rsi
#define ldb     %r8
#define ldc     %r9
#define pfA     %r10
#define MM      %r11
#define NN      %r12
#define incAn   %r13
#define incCn   %r14
#define MM0     %r15

#define rA0     %xmm0
#define rB0     %xmm1
#define rC00    %xmm2
#define rC01    %xmm3
#define rC02    %xmm4
#define rC03    %xmm5
#define rC04    %xmm6
#define rC05    %xmm7
#define rC06    %xmm8
#define rC07    %xmm9
#define rC0     %xmm10
#define rC2     %xmm11
#define rC4     %xmm12
#define rC6     %xmm13
#define rb0     %xmm14
#define BETA    %xmm15

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
   #define PFAINC ((MB*8+MB/8-1)/(MB/8))
#endif

#ifdef DCPLX
   #define CMUL(arg_) 2*arg_
#else
   #define CMUL(arg_) arg_
#endif

/*
                      %rdi/4       %rsi/8       %rdx/12          %xmm0/16
 void ATL_USERMM(const int M, const int N, const int K, const TYPE alpha,
                       %rcx/24         %r8/28         %r9/32           8/36
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                        %xmm1/40    16/48          24/52
                 const TYPE beta, TYPE *C, const int ldc)
*/
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
        movq    %r15, -48(%rsp)
#ifdef BETAX
        pshufd  $0x44, %xmm1, BETA
/*        pshufd  $0b01000100, %xmm1, BETA */
#endif
/*
 *      Setup input parameters
 */
        movq    %rdi, MM0
        movq    %rsi, NN
        movq    %r9, pB0
        movq    %r8, lda
        movslq  8(%rsp), ldb
        movq    16(%rsp), pC0
        movslq  24(%rsp), incCn
/*
 *      ldx *= sizeof; lda3 = 3*lda, lda5=5*lda, lda7=7*lda
 */
#ifndef DCPLX
        movq    incCn, ldc
#endif
        shl     $3, lda
        shl     $3, ldb
        lea     (lda,lda,2), lda3
        lea     (lda,lda,4), lda5
        lea     (lda3,lda,4), lda7
/*
 *      pA3 += 128, pB0 += 128
 */
        sub     $-128, pA0
        sub     $-128, pB0
/*
 *      incAn = lda*M*sizeof
 */
        movq    lda, incAn
        imulq   MM0, incAn
        lea     -128(pA0,incAn), pfA
/*
 *      incCn = (ldc - M)*sizeof
 */
        sub     MM0, incCn
#ifdef DCPLX
        shl     $4, incCn
#else
        shl     $3, incCn
        test    $1, ldc
        jnz     UNLOOP
        test    $15, pC0
        jnz     UNLOOP
NLOOP:
        movq    MM0, MM
        prefB(-128(pB0,ldb))
#if KB > 16
        prefB((pB0,ldb))
#endif
#if KB > 32
        prefB(128(pB0,ldb))
#endif
#if KB > 48
        prefB(256(pB0,ldb))
#endif
#if KB > 64
        prefB(384(pB0,ldb))
#endif
MLOOP:
                prefC((pC0))
/*KLOOP: */
	movapd	-128(pB0), rB0
                                        pref2((pfA))
                                        add     $PFAINC, pfA
	movapd	-128(pA0), rC00
	mulpd	rB0,rC00
	movapd	-128(pA0,lda), rC01
	mulpd	rB0,rC01
	movapd	-128(pA0,lda,2), rC02
	mulpd	rB0,rC02
	movapd	-128(pA0,lda3), rC03
	mulpd	rB0,rC03
	movapd	-128(pA0,lda,4), rC04
	mulpd	rB0,rC04
	movapd	-128(pA0,lda5), rC05
	mulpd	rB0,rC05
	movapd	-128(pA0,lda3,2), rC06
	mulpd	rB0,rC06
	movapd	-128(pA0,lda7), rC07
	mulpd	rB0,rC07
#if KB > 2
	movapd	16-128(pB0), rB0
	movapd	16-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	16-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	16-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	16-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	16-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	16-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	16-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	16-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 4
	movapd	32-128(pB0), rB0
	movapd	32-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	32-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	32-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	32-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	32-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	32-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	32-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	32-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 6
	movapd	48-128(pB0), rB0
	movapd	48-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	48-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	48-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	48-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	48-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	48-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	48-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	48-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 8
	movapd	64-128(pB0), rB0
	movapd	64-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	64-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	64-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	64-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	64-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	64-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	64-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	64-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 10
	movapd	80-128(pB0), rB0
	movapd	80-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	80-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	80-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	80-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	80-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	80-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	80-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	80-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 12
	movapd	96-128(pB0), rB0
	movapd	96-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	96-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	96-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	96-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	96-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	96-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	96-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	96-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 14
	movapd	112-128(pB0), rB0
	movapd	112-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	112-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	112-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	112-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	112-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	112-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	112-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	112-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 16
	movapd	128-128(pB0), rB0
	movapd	128-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	128-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	128-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	128-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	128-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	128-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	128-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	128-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 18
	movapd	144-128(pB0), rB0
	movapd	144-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	144-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	144-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	144-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	144-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	144-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	144-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	144-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 20
	movapd	160-128(pB0), rB0
	movapd	160-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	160-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	160-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	160-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	160-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	160-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	160-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	160-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 22
	movapd	176-128(pB0), rB0
	movapd	176-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	176-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	176-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	176-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	176-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	176-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	176-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	176-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 24
	movapd	192-128(pB0), rB0
	movapd	192-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	192-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	192-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	192-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	192-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	192-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	192-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	192-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 26
	movapd	208-128(pB0), rB0
	movapd	208-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	208-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	208-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	208-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	208-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	208-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	208-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	208-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 28
	movapd	224-128(pB0), rB0
	movapd	224-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	224-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	224-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	224-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	224-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	224-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	224-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	224-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 30
	movapd	240-128(pB0), rB0
	movapd	240-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	240-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	240-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	240-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	240-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	240-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	240-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	240-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 32
	movapd	256-128(pB0), rB0
	movapd	256-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	256-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	256-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	256-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	256-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	256-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	256-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	256-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 34
	movapd	272-128(pB0), rB0
	movapd	272-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	272-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	272-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	272-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	272-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	272-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	272-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	272-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 36
	movapd	288-128(pB0), rB0
	movapd	288-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	288-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	288-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	288-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	288-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	288-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	288-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	288-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 38
	movapd	304-128(pB0), rB0
	movapd	304-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	304-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	304-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	304-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	304-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	304-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	304-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	304-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 40
	movapd	320-128(pB0), rB0
	movapd	320-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	320-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	320-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	320-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	320-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	320-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	320-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	320-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 42
	movapd	336-128(pB0), rB0
	movapd	336-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	336-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	336-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	336-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	336-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	336-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	336-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	336-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 44
	movapd	352-128(pB0), rB0
	movapd	352-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	352-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	352-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	352-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	352-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	352-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	352-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	352-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 46
	movapd	368-128(pB0), rB0
	movapd	368-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	368-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	368-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	368-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	368-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	368-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	368-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	368-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 48
	movapd	384-128(pB0), rB0
	movapd	384-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	384-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	384-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	384-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	384-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	384-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	384-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	384-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 50
	movapd	400-128(pB0), rB0
	movapd	400-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	400-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	400-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	400-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	400-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	400-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	400-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	400-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 52
	movapd	416-128(pB0), rB0
	movapd	416-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	416-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	416-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	416-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	416-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	416-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	416-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	416-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 54
	movapd	432-128(pB0), rB0
	movapd	432-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	432-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	432-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	432-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	432-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	432-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	432-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	432-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 56
	movapd	448-128(pB0), rB0
	movapd	448-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	448-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	448-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	448-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	448-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	448-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	448-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	448-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 58
	movapd	464-128(pB0), rB0
	movapd	464-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	464-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	464-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	464-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	464-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	464-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	464-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	464-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 60
	movapd	480-128(pB0), rB0
	movapd	480-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	480-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	480-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	480-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	480-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	480-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	480-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	480-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 62
	movapd	496-128(pB0), rB0
	movapd	496-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	496-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	496-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	496-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	496-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	496-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	496-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	496-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 64
	movapd	512-128(pB0), rB0
	movapd	512-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	512-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	512-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	512-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	512-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	512-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	512-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	512-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 66
	movapd	528-128(pB0), rB0
	movapd	528-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	528-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	528-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	528-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	528-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	528-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	528-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	528-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 68
	movapd	544-128(pB0), rB0
	movapd	544-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	544-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	544-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	544-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	544-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	544-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	544-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	544-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 70
	movapd	560-128(pB0), rB0
	movapd	560-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	560-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	560-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	560-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	560-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	560-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	560-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	560-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 72
	movapd	576-128(pB0), rB0
	movapd	576-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	576-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	576-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	576-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	576-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	576-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	576-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	576-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 74
	movapd	592-128(pB0), rB0
	movapd	592-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	592-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	592-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	592-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	592-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	592-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	592-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	592-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 76
	movapd	608-128(pB0), rB0
	movapd	608-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	608-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	608-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	608-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	608-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	608-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	608-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	608-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 78
	movapd	624-128(pB0), rB0
	movapd	624-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	624-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	624-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	624-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	624-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	624-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	624-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	624-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 80
	movapd	640-128(pB0), rB0
	movapd	640-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	640-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	640-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	640-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	640-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	640-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	640-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	640-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 82
	movapd	656-128(pB0), rB0
	movapd	656-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	656-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	656-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	656-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	656-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	656-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	656-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	656-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 84
	movapd	672-128(pB0), rB0
	movapd	672-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	672-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	672-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	672-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	672-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	672-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	672-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	672-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 86
	movapd	688-128(pB0), rB0
	movapd	688-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	688-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	688-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	688-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	688-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	688-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	688-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	688-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 88
	movapd	704-128(pB0), rB0
	movapd	704-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	704-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	704-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	704-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	704-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	704-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	704-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	704-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 90
	movapd	720-128(pB0), rB0
	movapd	720-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	720-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	720-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	720-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	720-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	720-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	720-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	720-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 92
	movapd	736-128(pB0), rB0
	movapd	736-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	736-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	736-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	736-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	736-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	736-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	736-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	736-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 94
	movapd	752-128(pB0), rB0
	movapd	752-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	752-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	752-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	752-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	752-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	752-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	752-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	752-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 96
	movapd	768-128(pB0), rB0
	movapd	768-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	768-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	768-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	768-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	768-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	768-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	768-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	768-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 98
	movapd	784-128(pB0), rB0
	movapd	784-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	784-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	784-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	784-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	784-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	784-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	784-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	784-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 100
	movapd	800-128(pB0), rB0
	movapd	800-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	800-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	800-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	800-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	800-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	800-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	800-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	800-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 102
	movapd	816-128(pB0), rB0
	movapd	816-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	816-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	816-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	816-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	816-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	816-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	816-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	816-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 104
	movapd	832-128(pB0), rB0
	movapd	832-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	832-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	832-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	832-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	832-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	832-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	832-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	832-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 106
	movapd	848-128(pB0), rB0
	movapd	848-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	848-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	848-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	848-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	848-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	848-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	848-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	848-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 108
	movapd	864-128(pB0), rB0
	movapd	864-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	864-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	864-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	864-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	864-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	864-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	864-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	864-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 110
	movapd	880-128(pB0), rB0
	movapd	880-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	880-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	880-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	880-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	880-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	880-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	880-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	880-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 112
	movapd	896-128(pB0), rB0
	movapd	896-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	896-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	896-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	896-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	896-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	896-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	896-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	896-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 114
	movapd	912-128(pB0), rB0
	movapd	912-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	912-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	912-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	912-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	912-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	912-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	912-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	912-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 116
	movapd	928-128(pB0), rB0
	movapd	928-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	928-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	928-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	928-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	928-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	928-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	928-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	928-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 118
	movapd	944-128(pB0), rB0
	movapd	944-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	944-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	944-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	944-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	944-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	944-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	944-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	944-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 120
	movapd	960-128(pB0), rB0
	movapd	960-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	960-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	960-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	960-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	960-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	960-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	960-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	960-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 122
	movapd	976-128(pB0), rB0
	movapd	976-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	976-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	976-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	976-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	976-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	976-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	976-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	976-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 124
	movapd	992-128(pB0), rB0
	movapd	992-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	992-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	992-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	992-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	992-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	992-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	992-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	992-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 126
	movapd	1008-128(pB0), rB0
	movapd	1008-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1008-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1008-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1008-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1008-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1008-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1008-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1008-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 128
	movapd	1024-128(pB0), rB0
	movapd	1024-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1024-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1024-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1024-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1024-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1024-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1024-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1024-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 130
	movapd	1040-128(pB0), rB0
	movapd	1040-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1040-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1040-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1040-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1040-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1040-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1040-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1040-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 132
	movapd	1056-128(pB0), rB0
	movapd	1056-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1056-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1056-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1056-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1056-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1056-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1056-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1056-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 134
	movapd	1072-128(pB0), rB0
	movapd	1072-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1072-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1072-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1072-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1072-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1072-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1072-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1072-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 136
	movapd	1088-128(pB0), rB0
	movapd	1088-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1088-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1088-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1088-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1088-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1088-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1088-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1088-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 138
	movapd	1104-128(pB0), rB0
	movapd	1104-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1104-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1104-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1104-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1104-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1104-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1104-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1104-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 140
	movapd	1120-128(pB0), rB0
	movapd	1120-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1120-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1120-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1120-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1120-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1120-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1120-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1120-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 142
	movapd	1136-128(pB0), rB0
	movapd	1136-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1136-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1136-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1136-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1136-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1136-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1136-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1136-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 144
	movapd	1152-128(pB0), rB0
	movapd	1152-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1152-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1152-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1152-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1152-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1152-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1152-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1152-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 146
	movapd	1168-128(pB0), rB0
	movapd	1168-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1168-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1168-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1168-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1168-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1168-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1168-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1168-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 148
	movapd	1184-128(pB0), rB0
	movapd	1184-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1184-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1184-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1184-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1184-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1184-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1184-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1184-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 150
	movapd	1200-128(pB0), rB0
	movapd	1200-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1200-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1200-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1200-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1200-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1200-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1200-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1200-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 152
	movapd	1216-128(pB0), rB0
	movapd	1216-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1216-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1216-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1216-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1216-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1216-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1216-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1216-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 154
	movapd	1232-128(pB0), rB0
	movapd	1232-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1232-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1232-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1232-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1232-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1232-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1232-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1232-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 156
	movapd	1248-128(pB0), rB0
	movapd	1248-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1248-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1248-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1248-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1248-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1248-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1248-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1248-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 158
	movapd	1264-128(pB0), rB0
	movapd	1264-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1264-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1264-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1264-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1264-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1264-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1264-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1264-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 160
	movapd	1280-128(pB0), rB0
	movapd	1280-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1280-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1280-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1280-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1280-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1280-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1280-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1280-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 162
	movapd	1296-128(pB0), rB0
	movapd	1296-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1296-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1296-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1296-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1296-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1296-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1296-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1296-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 164
	movapd	1312-128(pB0), rB0
	movapd	1312-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1312-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1312-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1312-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1312-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1312-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1312-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1312-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 166
	movapd	1328-128(pB0), rB0
	movapd	1328-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1328-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1328-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1328-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1328-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1328-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1328-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1328-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 168
	movapd	1344-128(pB0), rB0
	movapd	1344-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1344-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1344-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1344-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1344-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1344-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1344-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1344-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 170
	movapd	1360-128(pB0), rB0
	movapd	1360-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1360-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1360-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1360-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1360-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1360-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1360-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1360-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 172
	movapd	1376-128(pB0), rB0
	movapd	1376-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1376-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1376-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1376-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1376-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1376-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1376-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1376-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 174
	movapd	1392-128(pB0), rB0
	movapd	1392-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1392-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1392-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1392-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1392-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1392-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1392-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1392-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 176
	movapd	1408-128(pB0), rB0
	movapd	1408-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1408-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1408-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1408-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1408-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1408-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1408-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1408-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 178
	movapd	1424-128(pB0), rB0
	movapd	1424-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1424-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1424-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1424-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1424-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1424-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1424-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1424-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 180
	movapd	1440-128(pB0), rB0
	movapd	1440-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1440-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1440-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1440-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1440-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1440-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1440-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1440-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 182
	movapd	1456-128(pB0), rB0
	movapd	1456-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1456-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1456-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1456-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1456-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1456-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1456-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1456-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 184
	movapd	1472-128(pB0), rB0
	movapd	1472-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1472-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1472-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1472-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1472-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1472-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1472-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1472-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 186
	movapd	1488-128(pB0), rB0
	movapd	1488-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1488-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1488-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1488-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1488-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1488-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1488-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1488-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 188
	movapd	1504-128(pB0), rB0
	movapd	1504-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1504-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1504-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1504-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1504-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1504-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1504-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1504-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 190
	movapd	1520-128(pB0), rB0
	movapd	1520-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1520-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1520-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1520-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1520-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1520-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1520-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1520-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 192
	movapd	1536-128(pB0), rB0
	movapd	1536-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1536-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1536-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1536-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1536-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1536-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1536-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1536-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 194
	movapd	1552-128(pB0), rB0
	movapd	1552-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1552-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1552-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1552-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1552-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1552-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1552-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1552-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 196
	movapd	1568-128(pB0), rB0
	movapd	1568-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1568-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1568-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1568-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1568-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1568-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1568-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1568-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 198
	movapd	1584-128(pB0), rB0
	movapd	1584-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1584-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1584-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1584-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1584-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1584-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1584-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1584-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 200
	movapd	1600-128(pB0), rB0
	movapd	1600-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1600-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1600-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1600-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1600-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1600-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1600-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1600-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 202
	movapd	1616-128(pB0), rB0
	movapd	1616-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1616-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1616-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1616-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1616-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1616-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1616-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1616-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 204
	movapd	1632-128(pB0), rB0
	movapd	1632-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1632-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1632-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1632-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1632-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1632-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1632-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1632-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 206
	movapd	1648-128(pB0), rB0
	movapd	1648-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1648-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1648-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1648-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1648-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1648-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1648-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1648-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 208
	movapd	1664-128(pB0), rB0
	movapd	1664-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1664-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1664-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1664-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1664-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1664-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1664-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1664-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 210
	movapd	1680-128(pB0), rB0
	movapd	1680-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1680-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1680-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1680-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1680-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1680-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1680-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1680-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 212
	movapd	1696-128(pB0), rB0
	movapd	1696-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1696-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1696-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1696-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1696-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1696-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1696-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1696-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 214
	movapd	1712-128(pB0), rB0
	movapd	1712-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1712-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1712-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1712-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1712-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1712-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1712-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1712-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 216
	movapd	1728-128(pB0), rB0
	movapd	1728-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1728-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1728-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1728-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1728-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1728-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1728-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1728-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 218
	movapd	1744-128(pB0), rB0
	movapd	1744-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1744-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1744-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1744-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1744-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1744-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1744-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1744-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 220
	movapd	1760-128(pB0), rB0
	movapd	1760-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1760-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1760-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1760-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1760-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1760-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1760-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1760-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 222
	movapd	1776-128(pB0), rB0
	movapd	1776-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1776-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1776-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1776-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1776-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1776-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1776-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1776-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 224
	movapd	1792-128(pB0), rB0
	movapd	1792-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1792-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1792-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1792-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1792-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1792-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1792-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1792-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 226
	movapd	1808-128(pB0), rB0
	movapd	1808-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1808-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1808-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1808-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1808-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1808-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1808-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1808-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 228
	movapd	1824-128(pB0), rB0
	movapd	1824-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1824-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1824-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1824-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1824-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1824-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1824-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1824-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 230
	movapd	1840-128(pB0), rB0
	movapd	1840-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1840-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1840-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1840-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1840-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1840-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1840-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1840-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 232
	movapd	1856-128(pB0), rB0
	movapd	1856-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1856-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1856-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1856-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1856-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1856-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1856-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1856-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 234
	movapd	1872-128(pB0), rB0
	movapd	1872-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1872-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1872-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1872-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1872-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1872-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1872-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1872-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 236
	movapd	1888-128(pB0), rB0
	movapd	1888-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1888-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1888-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1888-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1888-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1888-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1888-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1888-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 238
	movapd	1904-128(pB0), rB0
	movapd	1904-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1904-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1904-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1904-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1904-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1904-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1904-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1904-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 240
	movapd	1920-128(pB0), rB0
	movapd	1920-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1920-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1920-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1920-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1920-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1920-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1920-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1920-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 242
	movapd	1936-128(pB0), rB0
	movapd	1936-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1936-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1936-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1936-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1936-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1936-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1936-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1936-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 244
	movapd	1952-128(pB0), rB0
	movapd	1952-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1952-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1952-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1952-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1952-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1952-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1952-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1952-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 246
	movapd	1968-128(pB0), rB0
	movapd	1968-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1968-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1968-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1968-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1968-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1968-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1968-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1968-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 248
	movapd	1984-128(pB0), rB0
	movapd	1984-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1984-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1984-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1984-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1984-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1984-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1984-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1984-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 250
	movapd	2000-128(pB0), rB0
	movapd	2000-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2000-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2000-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2000-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2000-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2000-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2000-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2000-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 252
	movapd	2016-128(pB0), rB0
	movapd	2016-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2016-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2016-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2016-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2016-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2016-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2016-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2016-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 254
	movapd	2032-128(pB0), rB0
	movapd	2032-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2032-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2032-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2032-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2032-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2032-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2032-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2032-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 256
	movapd	2048-128(pB0), rB0
	movapd	2048-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2048-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2048-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2048-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2048-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2048-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2048-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2048-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 258
	movapd	2064-128(pB0), rB0
	movapd	2064-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2064-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2064-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2064-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2064-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2064-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2064-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2064-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 260
	movapd	2080-128(pB0), rB0
	movapd	2080-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2080-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2080-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2080-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2080-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2080-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2080-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2080-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 262
	movapd	2096-128(pB0), rB0
	movapd	2096-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2096-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2096-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2096-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2096-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2096-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2096-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2096-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 264
	movapd	2112-128(pB0), rB0
	movapd	2112-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2112-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2112-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2112-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2112-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2112-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2112-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2112-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 266
	movapd	2128-128(pB0), rB0
	movapd	2128-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2128-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2128-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2128-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2128-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2128-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2128-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2128-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 268
	movapd	2144-128(pB0), rB0
	movapd	2144-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2144-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2144-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2144-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2144-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2144-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2144-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2144-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 270
	movapd	2160-128(pB0), rB0
	movapd	2160-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2160-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2160-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2160-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2160-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2160-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2160-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2160-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 272
	movapd	2176-128(pB0), rB0
	movapd	2176-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2176-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2176-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2176-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2176-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2176-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2176-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2176-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 274
	movapd	2192-128(pB0), rB0
	movapd	2192-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2192-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2192-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2192-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2192-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2192-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2192-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2192-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 276
	movapd	2208-128(pB0), rB0
	movapd	2208-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2208-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2208-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2208-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2208-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2208-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2208-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2208-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 278
	movapd	2224-128(pB0), rB0
	movapd	2224-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2224-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2224-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2224-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2224-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2224-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2224-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2224-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 280
	movapd	2240-128(pB0), rB0
	movapd	2240-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2240-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2240-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2240-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2240-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2240-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2240-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2240-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 282
	movapd	2256-128(pB0), rB0
	movapd	2256-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2256-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2256-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2256-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2256-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2256-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2256-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2256-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 284
	movapd	2272-128(pB0), rB0
	movapd	2272-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2272-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2272-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2272-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2272-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2272-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2272-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2272-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 286
	movapd	2288-128(pB0), rB0
	movapd	2288-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2288-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2288-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2288-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2288-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2288-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2288-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2288-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 288
	movapd	2304-128(pB0), rB0
	movapd	2304-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2304-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2304-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2304-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2304-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2304-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2304-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2304-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 290
	movapd	2320-128(pB0), rB0
	movapd	2320-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2320-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2320-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2320-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2320-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2320-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2320-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2320-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 292
	movapd	2336-128(pB0), rB0
	movapd	2336-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2336-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2336-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2336-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2336-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2336-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2336-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2336-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 294
	movapd	2352-128(pB0), rB0
	movapd	2352-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2352-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2352-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2352-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2352-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2352-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2352-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2352-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 296
	movapd	2368-128(pB0), rB0
	movapd	2368-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2368-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2368-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2368-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2368-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2368-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2368-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2368-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 298
	movapd	2384-128(pB0), rB0
	movapd	2384-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2384-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2384-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2384-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2384-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2384-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2384-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2384-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 300
	movapd	2400-128(pB0), rB0
	movapd	2400-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2400-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2400-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2400-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2400-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2400-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2400-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2400-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 302
	movapd	2416-128(pB0), rB0
	movapd	2416-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2416-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2416-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2416-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2416-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2416-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2416-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2416-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 304
	movapd	2432-128(pB0), rB0
	movapd	2432-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2432-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2432-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2432-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2432-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2432-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2432-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2432-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 306
	movapd	2448-128(pB0), rB0
	movapd	2448-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2448-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2448-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2448-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2448-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2448-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2448-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2448-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 308
	movapd	2464-128(pB0), rB0
	movapd	2464-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2464-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2464-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2464-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2464-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2464-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2464-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2464-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 310
	movapd	2480-128(pB0), rB0
	movapd	2480-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2480-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2480-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2480-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2480-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2480-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2480-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2480-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 312
	movapd	2496-128(pB0), rB0
	movapd	2496-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2496-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2496-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2496-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2496-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2496-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2496-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2496-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 314
	movapd	2512-128(pB0), rB0
	movapd	2512-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2512-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2512-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2512-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2512-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2512-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2512-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2512-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 316
	movapd	2528-128(pB0), rB0
	movapd	2528-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2528-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2528-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2528-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2528-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2528-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2528-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2528-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 318
	movapd	2544-128(pB0), rB0
	movapd	2544-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2544-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2544-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2544-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2544-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2544-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2544-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2544-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 320
	movapd	2560-128(pB0), rB0
	movapd	2560-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2560-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2560-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2560-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2560-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2560-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2560-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2560-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 322
	movapd	2576-128(pB0), rB0
	movapd	2576-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2576-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2576-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2576-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2576-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2576-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2576-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2576-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 324
	movapd	2592-128(pB0), rB0
	movapd	2592-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2592-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2592-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2592-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2592-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2592-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2592-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2592-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 326
	movapd	2608-128(pB0), rB0
	movapd	2608-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2608-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2608-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2608-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2608-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2608-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2608-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2608-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 328
	movapd	2624-128(pB0), rB0
	movapd	2624-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2624-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2624-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2624-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2624-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2624-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2624-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2624-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 330
	movapd	2640-128(pB0), rB0
	movapd	2640-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2640-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2640-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2640-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2640-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2640-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2640-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2640-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 332
	movapd	2656-128(pB0), rB0
	movapd	2656-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2656-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2656-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2656-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2656-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2656-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2656-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2656-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 334
	movapd	2672-128(pB0), rB0
	movapd	2672-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2672-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2672-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2672-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2672-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2672-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2672-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2672-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 336
	movapd	2688-128(pB0), rB0
	movapd	2688-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2688-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2688-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2688-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2688-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2688-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2688-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2688-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 338
	movapd	2704-128(pB0), rB0
	movapd	2704-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2704-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2704-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2704-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2704-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2704-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2704-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2704-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 340
	movapd	2720-128(pB0), rB0
	movapd	2720-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2720-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2720-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2720-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2720-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2720-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2720-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2720-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 342
	movapd	2736-128(pB0), rB0
	movapd	2736-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2736-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2736-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2736-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2736-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2736-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2736-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2736-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 344
	movapd	2752-128(pB0), rB0
	movapd	2752-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2752-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2752-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2752-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2752-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2752-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2752-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2752-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 346
	movapd	2768-128(pB0), rB0
	movapd	2768-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2768-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2768-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2768-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2768-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2768-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2768-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2768-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 348
	movapd	2784-128(pB0), rB0
	movapd	2784-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2784-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2784-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2784-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2784-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2784-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2784-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2784-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 350
	movapd	2800-128(pB0), rB0
	movapd	2800-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2800-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2800-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2800-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2800-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2800-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2800-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2800-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 352
	movapd	2816-128(pB0), rB0
	movapd	2816-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2816-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2816-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2816-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2816-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2816-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2816-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2816-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 354
	movapd	2832-128(pB0), rB0
	movapd	2832-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2832-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2832-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2832-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2832-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2832-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2832-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2832-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 356
	movapd	2848-128(pB0), rB0
	movapd	2848-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2848-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2848-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2848-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2848-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2848-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2848-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2848-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 358
	movapd	2864-128(pB0), rB0
	movapd	2864-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2864-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2864-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2864-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2864-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2864-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2864-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2864-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 360
	movapd	2880-128(pB0), rB0
	movapd	2880-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2880-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2880-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2880-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2880-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2880-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2880-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2880-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 362
	movapd	2896-128(pB0), rB0
	movapd	2896-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2896-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2896-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2896-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2896-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2896-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2896-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2896-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 364
	movapd	2912-128(pB0), rB0
	movapd	2912-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2912-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2912-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2912-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2912-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2912-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2912-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2912-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 366
	movapd	2928-128(pB0), rB0
	movapd	2928-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2928-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2928-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2928-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2928-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2928-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2928-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2928-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 368
	movapd	2944-128(pB0), rB0
	movapd	2944-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2944-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2944-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2944-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2944-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2944-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2944-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2944-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 370
	movapd	2960-128(pB0), rB0
	movapd	2960-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2960-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2960-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2960-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2960-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2960-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2960-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2960-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 372
	movapd	2976-128(pB0), rB0
	movapd	2976-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2976-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2976-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2976-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2976-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2976-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2976-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2976-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 374
	movapd	2992-128(pB0), rB0
	movapd	2992-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2992-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2992-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2992-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2992-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2992-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2992-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2992-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 376
	movapd	3008-128(pB0), rB0
	movapd	3008-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3008-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3008-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3008-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3008-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3008-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3008-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3008-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 378
	movapd	3024-128(pB0), rB0
	movapd	3024-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3024-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3024-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3024-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3024-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3024-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3024-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3024-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 380
	movapd	3040-128(pB0), rB0
	movapd	3040-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3040-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3040-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3040-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3040-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3040-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3040-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3040-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 382
	movapd	3056-128(pB0), rB0
	movapd	3056-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3056-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3056-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3056-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3056-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3056-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3056-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3056-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 384
	movapd	3072-128(pB0), rB0
	movapd	3072-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3072-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3072-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3072-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3072-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3072-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3072-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3072-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 386
	movapd	3088-128(pB0), rB0
	movapd	3088-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3088-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3088-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3088-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3088-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3088-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3088-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3088-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 388
	movapd	3104-128(pB0), rB0
	movapd	3104-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3104-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3104-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3104-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3104-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3104-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3104-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3104-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 390
	movapd	3120-128(pB0), rB0
	movapd	3120-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3120-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3120-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3120-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3120-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3120-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3120-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3120-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 392
	movapd	3136-128(pB0), rB0
	movapd	3136-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3136-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3136-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3136-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3136-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3136-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3136-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3136-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 394
	movapd	3152-128(pB0), rB0
	movapd	3152-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3152-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3152-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3152-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3152-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3152-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3152-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3152-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 396
	movapd	3168-128(pB0), rB0
	movapd	3168-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3168-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3168-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3168-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3168-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3168-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3168-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3168-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 398
	movapd	3184-128(pB0), rB0
	movapd	3184-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3184-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3184-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3184-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3184-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3184-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3184-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3184-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 400
	movapd	3200-128(pB0), rB0
	movapd	3200-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3200-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3200-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3200-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3200-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3200-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3200-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3200-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 402
	movapd	3216-128(pB0), rB0
	movapd	3216-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3216-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3216-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3216-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3216-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3216-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3216-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3216-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 404
	movapd	3232-128(pB0), rB0
	movapd	3232-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3232-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3232-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3232-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3232-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3232-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3232-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3232-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 406
	movapd	3248-128(pB0), rB0
	movapd	3248-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3248-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3248-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3248-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3248-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3248-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3248-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3248-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 408
	movapd	3264-128(pB0), rB0
	movapd	3264-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3264-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3264-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3264-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3264-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3264-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3264-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3264-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 410
	movapd	3280-128(pB0), rB0
	movapd	3280-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3280-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3280-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3280-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3280-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3280-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3280-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3280-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 412
	movapd	3296-128(pB0), rB0
	movapd	3296-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3296-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3296-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3296-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3296-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3296-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3296-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3296-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 414
	movapd	3312-128(pB0), rB0
	movapd	3312-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3312-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3312-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3312-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3312-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3312-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3312-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3312-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 416
	movapd	3328-128(pB0), rB0
	movapd	3328-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3328-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3328-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3328-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3328-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3328-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3328-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3328-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 418
	movapd	3344-128(pB0), rB0
	movapd	3344-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3344-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3344-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3344-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3344-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3344-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3344-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3344-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 420
	movapd	3360-128(pB0), rB0
	movapd	3360-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3360-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3360-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3360-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3360-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3360-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3360-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3360-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 422
	movapd	3376-128(pB0), rB0
	movapd	3376-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3376-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3376-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3376-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3376-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3376-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3376-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3376-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 424
	movapd	3392-128(pB0), rB0
	movapd	3392-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3392-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3392-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3392-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3392-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3392-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3392-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3392-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 426
	movapd	3408-128(pB0), rB0
	movapd	3408-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3408-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3408-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3408-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3408-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3408-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3408-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3408-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 428
	movapd	3424-128(pB0), rB0
	movapd	3424-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3424-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3424-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3424-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3424-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3424-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3424-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3424-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 430
	movapd	3440-128(pB0), rB0
	movapd	3440-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3440-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3440-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3440-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3440-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3440-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3440-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3440-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 432
	movapd	3456-128(pB0), rB0
	movapd	3456-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3456-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3456-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3456-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3456-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3456-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3456-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3456-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 434
	movapd	3472-128(pB0), rB0
	movapd	3472-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3472-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3472-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3472-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3472-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3472-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3472-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3472-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 436
	movapd	3488-128(pB0), rB0
	movapd	3488-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3488-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3488-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3488-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3488-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3488-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3488-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3488-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 438
	movapd	3504-128(pB0), rB0
	movapd	3504-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3504-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3504-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3504-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3504-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3504-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3504-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3504-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 440
	movapd	3520-128(pB0), rB0
	movapd	3520-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3520-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3520-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3520-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3520-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3520-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3520-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3520-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 442
	movapd	3536-128(pB0), rB0
	movapd	3536-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3536-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3536-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3536-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3536-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3536-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3536-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3536-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 444
	movapd	3552-128(pB0), rB0
	movapd	3552-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3552-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3552-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3552-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3552-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3552-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3552-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3552-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 446
	movapd	3568-128(pB0), rB0
	movapd	3568-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3568-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3568-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3568-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3568-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3568-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3568-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3568-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 448
	movapd	3584-128(pB0), rB0
	movapd	3584-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3584-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3584-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3584-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3584-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3584-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3584-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3584-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 450
	movapd	3600-128(pB0), rB0
	movapd	3600-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3600-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3600-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3600-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3600-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3600-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3600-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3600-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 452
	movapd	3616-128(pB0), rB0
	movapd	3616-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3616-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3616-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3616-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3616-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3616-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3616-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3616-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 454
	movapd	3632-128(pB0), rB0
	movapd	3632-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3632-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3632-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3632-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3632-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3632-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3632-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3632-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 456
	movapd	3648-128(pB0), rB0
	movapd	3648-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3648-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3648-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3648-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3648-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3648-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3648-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3648-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 458
	movapd	3664-128(pB0), rB0
	movapd	3664-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3664-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3664-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3664-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3664-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3664-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3664-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3664-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 460
	movapd	3680-128(pB0), rB0
	movapd	3680-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3680-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3680-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3680-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3680-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3680-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3680-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3680-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 462
	movapd	3696-128(pB0), rB0
	movapd	3696-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3696-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3696-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3696-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3696-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3696-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3696-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3696-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 464
	movapd	3712-128(pB0), rB0
	movapd	3712-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3712-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3712-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3712-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3712-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3712-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3712-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3712-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 466
	movapd	3728-128(pB0), rB0
	movapd	3728-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3728-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3728-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3728-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3728-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3728-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3728-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3728-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 468
	movapd	3744-128(pB0), rB0
	movapd	3744-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3744-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3744-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3744-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3744-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3744-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3744-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3744-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 470
	movapd	3760-128(pB0), rB0
	movapd	3760-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3760-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3760-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3760-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3760-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3760-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3760-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3760-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 472
	movapd	3776-128(pB0), rB0
	movapd	3776-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3776-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3776-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3776-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3776-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3776-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3776-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3776-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 474
	movapd	3792-128(pB0), rB0
	movapd	3792-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3792-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3792-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3792-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3792-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3792-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3792-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3792-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 476
	movapd	3808-128(pB0), rB0
	movapd	3808-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3808-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3808-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3808-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3808-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3808-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3808-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3808-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 478
	movapd	3824-128(pB0), rB0
	movapd	3824-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3824-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3824-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3824-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3824-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3824-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3824-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3824-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 480
	movapd	3840-128(pB0), rB0
	movapd	3840-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3840-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3840-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3840-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3840-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3840-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3840-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3840-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 482
	movapd	3856-128(pB0), rB0
	movapd	3856-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3856-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3856-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3856-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3856-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3856-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3856-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3856-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 484
	movapd	3872-128(pB0), rB0
	movapd	3872-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3872-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3872-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3872-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3872-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3872-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3872-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3872-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 486
	movapd	3888-128(pB0), rB0
	movapd	3888-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3888-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3888-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3888-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3888-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3888-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3888-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3888-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 488
	movapd	3904-128(pB0), rB0
	movapd	3904-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3904-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3904-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3904-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3904-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3904-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3904-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3904-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 490
	movapd	3920-128(pB0), rB0
	movapd	3920-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3920-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3920-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3920-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3920-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3920-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3920-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3920-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 492
	movapd	3936-128(pB0), rB0
	movapd	3936-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3936-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3936-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3936-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3936-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3936-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3936-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3936-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 494
	movapd	3952-128(pB0), rB0
	movapd	3952-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3952-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3952-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3952-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3952-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3952-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3952-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3952-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 496
	movapd	3968-128(pB0), rB0
	movapd	3968-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3968-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3968-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3968-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3968-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3968-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3968-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3968-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 498
	movapd	3984-128(pB0), rB0
	movapd	3984-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3984-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3984-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3984-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3984-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3984-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3984-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3984-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 500
	movapd	4000-128(pB0), rB0
	movapd	4000-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	4000-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	4000-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	4000-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	4000-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	4000-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	4000-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	4000-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 502
	movapd	4016-128(pB0), rB0
	movapd	4016-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	4016-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	4016-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	4016-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	4016-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	4016-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	4016-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	4016-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 504
	movapd	4032-128(pB0), rB0
	movapd	4032-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	4032-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	4032-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	4032-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	4032-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	4032-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	4032-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	4032-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 506
	movapd	4048-128(pB0), rB0
	movapd	4048-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	4048-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	4048-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	4048-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	4048-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	4048-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	4048-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	4048-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 508
	movapd	4064-128(pB0), rB0
	movapd	4064-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	4064-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	4064-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	4064-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	4064-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	4064-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	4064-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	4064-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 510
	movapd	4080-128(pB0), rB0
	movapd	4080-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	4080-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	4080-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	4080-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	4080-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	4080-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	4080-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	4080-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

/*       jne KLOOP */
/*
 *      pC[0-3] = rC[0-3]
 */

        #ifndef BETA0
           movapd       (pC0), rA0
           #ifdef BETAX
              mulpd     BETA, rA0
           #endif
        #endif
        haddpd  rC01,rC00
                                lea     (pA0,lda,8), pA0
        #ifdef BETAN1
                subpd   rA0, rC00
        #elif !defined(BETA0)
                addpd   rA0, rC00
        #endif
        movapd  rC00, (pC0)

        #ifndef BETA0
           movapd       16(pC0), rA0
           #ifdef BETAX
              mulpd     BETA, rA0
           #endif
        #endif
        haddpd  rC03,rC02
        #ifdef BETAN1
                subpd   rA0, rC02
        #elif !defined(BETA0)
                addpd   rA0, rC02
        #endif
        movapd  rC02, 16(pC0)

        #ifndef BETA0
           movapd       32(pC0), rA0
           #ifdef BETAX
              mulpd     BETA, rA0
           #endif
        #endif
        haddpd  rC05,rC04
        #ifdef BETAN1
                subpd   rA0, rC04
        #elif !defined(BETA0)
                addpd   rA0, rC04
        #endif
        movapd  rC04, 32(pC0)

        #ifndef BETA0
           movapd       48(pC0), rA0
           #ifdef BETAX
              mulpd     BETA, rA0
           #endif
        #endif
        haddpd  rC07,rC06
        #ifdef BETAN1
                subpd   rA0, rC06
        #elif !defined(BETA0)
                addpd   rA0, rC06
        #endif
        movapd  rC06, 48(pC0)

        add     $8*8, pC0
        sub     $8, MM
        jnz     MLOOP

        sub     incAn, pA0
        add     incCn, pC0
        add     ldb, pB0
        sub     $1, NN

        jnz     NLOOP

/* DONE: */
        movq    -8(%rsp), %rbp
        movq    -16(%rsp), %rbx
        movq    -24(%rsp), %r12
        movq    -32(%rsp), %r13
        movq    -40(%rsp), %r14
        movq    -48(%rsp), %r15
        ret
#endif
/*
 *This set of loops is used always for complex data, and anytime all columns
 *of $C$ aren't on 16-byte boundary (allowing use of high performance movapd)
 */
UNLOOP:
        movq    MM0, MM
        prefB(-128(pB0,ldb))
#if KB > 16
        prefB((pB0,ldb))
#endif
#if KB > 32
        prefB(128(pB0,ldb))
#endif
#if KB > 48
        prefB(256(pB0,ldb))
#endif
#if KB > 64
        prefB(384(pB0,ldb))
#endif
UMLOOP:
/*
 *      For unaligned code, load C at top so we don't exhaust reservation stat
 *      of write buffer with all the movlpd/movhpd pairs at end of loop
 */
        prefC(CMUL(64)(pC0))
        #ifndef BETA0
           movlpd    (pC0), rC0
           movhpd    CMUL(8)(pC0), rC0
           #ifdef BETAX
              mulpd     BETA, rC0
           #endif
           movlpd    CMUL(16)(pC0), rC2
           movhpd    CMUL(24)(pC0), rC2
           #ifdef BETAX
              mulpd     BETA, rC2
           #endif
           movlpd    CMUL(32)(pC0), rC4
           movhpd    CMUL(40)(pC0), rC4
           #ifdef BETAX
              mulpd     BETA, rC4
           #endif
           movlpd    CMUL(48)(pC0), rC6
           movhpd    CMUL(56)(pC0), rC6
           #ifdef BETAX
              mulpd     BETA, rC6
           #endif
        #endif
/*KLOOP: */
	movapd	-128(pB0), rB0
                                        pref2((pfA))
                                        add     $PFAINC, pfA
	movapd	-128(pA0), rC00
	mulpd	rB0,rC00
	movapd	-128(pA0,lda), rC01
	mulpd	rB0,rC01
	movapd	-128(pA0,lda,2), rC02
	mulpd	rB0,rC02
	movapd	-128(pA0,lda3), rC03
	mulpd	rB0,rC03
	movapd	-128(pA0,lda,4), rC04
	mulpd	rB0,rC04
	movapd	-128(pA0,lda5), rC05
	mulpd	rB0,rC05
	movapd	-128(pA0,lda3,2), rC06
	mulpd	rB0,rC06
	movapd	-128(pA0,lda7), rC07
	mulpd	rB0,rC07
#if KB > 2
	movapd	16-128(pB0), rB0
	movapd	16-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	16-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	16-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	16-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	16-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	16-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	16-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	16-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 4
	movapd	32-128(pB0), rB0
	movapd	32-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	32-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	32-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	32-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	32-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	32-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	32-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	32-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 6
	movapd	48-128(pB0), rB0
	movapd	48-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	48-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	48-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	48-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	48-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	48-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	48-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	48-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 8
	movapd	64-128(pB0), rB0
	movapd	64-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	64-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	64-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	64-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	64-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	64-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	64-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	64-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 10
	movapd	80-128(pB0), rB0
	movapd	80-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	80-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	80-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	80-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	80-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	80-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	80-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	80-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 12
	movapd	96-128(pB0), rB0
	movapd	96-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	96-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	96-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	96-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	96-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	96-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	96-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	96-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 14
	movapd	112-128(pB0), rB0
	movapd	112-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	112-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	112-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	112-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	112-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	112-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	112-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	112-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 16
	movapd	128-128(pB0), rB0
	movapd	128-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	128-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	128-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	128-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	128-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	128-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	128-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	128-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 18
	movapd	144-128(pB0), rB0
	movapd	144-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	144-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	144-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	144-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	144-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	144-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	144-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	144-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 20
	movapd	160-128(pB0), rB0
	movapd	160-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	160-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	160-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	160-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	160-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	160-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	160-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	160-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 22
	movapd	176-128(pB0), rB0
	movapd	176-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	176-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	176-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	176-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	176-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	176-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	176-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	176-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 24
	movapd	192-128(pB0), rB0
	movapd	192-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	192-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	192-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	192-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	192-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	192-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	192-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	192-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 26
	movapd	208-128(pB0), rB0
	movapd	208-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	208-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	208-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	208-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	208-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	208-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	208-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	208-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 28
	movapd	224-128(pB0), rB0
	movapd	224-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	224-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	224-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	224-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	224-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	224-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	224-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	224-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 30
	movapd	240-128(pB0), rB0
	movapd	240-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	240-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	240-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	240-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	240-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	240-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	240-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	240-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 32
	movapd	256-128(pB0), rB0
	movapd	256-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	256-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	256-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	256-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	256-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	256-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	256-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	256-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 34
	movapd	272-128(pB0), rB0
	movapd	272-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	272-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	272-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	272-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	272-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	272-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	272-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	272-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 36
	movapd	288-128(pB0), rB0
	movapd	288-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	288-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	288-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	288-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	288-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	288-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	288-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	288-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 38
	movapd	304-128(pB0), rB0
	movapd	304-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	304-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	304-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	304-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	304-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	304-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	304-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	304-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 40
	movapd	320-128(pB0), rB0
	movapd	320-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	320-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	320-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	320-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	320-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	320-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	320-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	320-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 42
	movapd	336-128(pB0), rB0
	movapd	336-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	336-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	336-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	336-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	336-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	336-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	336-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	336-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 44
	movapd	352-128(pB0), rB0
	movapd	352-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	352-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	352-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	352-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	352-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	352-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	352-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	352-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 46
	movapd	368-128(pB0), rB0
	movapd	368-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	368-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	368-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	368-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	368-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	368-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	368-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	368-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 48
	movapd	384-128(pB0), rB0
	movapd	384-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	384-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	384-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	384-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	384-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	384-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	384-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	384-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 50
	movapd	400-128(pB0), rB0
	movapd	400-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	400-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	400-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	400-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	400-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	400-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	400-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	400-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 52
	movapd	416-128(pB0), rB0
	movapd	416-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	416-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	416-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	416-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	416-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	416-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	416-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	416-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 54
	movapd	432-128(pB0), rB0
	movapd	432-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	432-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	432-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	432-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	432-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	432-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	432-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	432-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 56
	movapd	448-128(pB0), rB0
	movapd	448-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	448-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	448-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	448-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	448-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	448-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	448-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	448-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 58
	movapd	464-128(pB0), rB0
	movapd	464-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	464-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	464-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	464-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	464-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	464-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	464-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	464-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 60
	movapd	480-128(pB0), rB0
	movapd	480-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	480-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	480-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	480-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	480-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	480-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	480-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	480-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 62
	movapd	496-128(pB0), rB0
	movapd	496-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	496-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	496-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	496-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	496-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	496-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	496-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	496-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 64
	movapd	512-128(pB0), rB0
	movapd	512-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	512-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	512-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	512-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	512-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	512-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	512-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	512-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 66
	movapd	528-128(pB0), rB0
	movapd	528-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	528-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	528-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	528-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	528-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	528-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	528-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	528-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 68
	movapd	544-128(pB0), rB0
	movapd	544-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	544-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	544-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	544-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	544-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	544-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	544-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	544-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 70
	movapd	560-128(pB0), rB0
	movapd	560-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	560-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	560-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	560-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	560-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	560-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	560-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	560-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 72
	movapd	576-128(pB0), rB0
	movapd	576-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	576-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	576-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	576-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	576-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	576-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	576-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	576-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 74
	movapd	592-128(pB0), rB0
	movapd	592-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	592-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	592-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	592-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	592-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	592-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	592-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	592-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 76
	movapd	608-128(pB0), rB0
	movapd	608-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	608-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	608-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	608-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	608-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	608-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	608-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	608-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 78
	movapd	624-128(pB0), rB0
	movapd	624-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	624-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	624-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	624-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	624-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	624-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	624-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	624-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 80
	movapd	640-128(pB0), rB0
	movapd	640-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	640-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	640-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	640-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	640-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	640-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	640-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	640-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 82
	movapd	656-128(pB0), rB0
	movapd	656-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	656-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	656-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	656-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	656-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	656-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	656-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	656-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 84
	movapd	672-128(pB0), rB0
	movapd	672-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	672-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	672-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	672-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	672-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	672-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	672-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	672-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 86
	movapd	688-128(pB0), rB0
	movapd	688-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	688-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	688-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	688-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	688-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	688-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	688-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	688-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 88
	movapd	704-128(pB0), rB0
	movapd	704-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	704-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	704-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	704-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	704-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	704-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	704-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	704-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 90
	movapd	720-128(pB0), rB0
	movapd	720-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	720-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	720-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	720-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	720-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	720-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	720-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	720-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 92
	movapd	736-128(pB0), rB0
	movapd	736-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	736-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	736-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	736-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	736-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	736-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	736-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	736-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 94
	movapd	752-128(pB0), rB0
	movapd	752-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	752-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	752-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	752-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	752-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	752-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	752-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	752-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 96
	movapd	768-128(pB0), rB0
	movapd	768-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	768-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	768-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	768-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	768-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	768-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	768-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	768-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 98
	movapd	784-128(pB0), rB0
	movapd	784-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	784-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	784-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	784-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	784-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	784-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	784-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	784-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 100
	movapd	800-128(pB0), rB0
	movapd	800-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	800-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	800-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	800-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	800-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	800-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	800-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	800-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 102
	movapd	816-128(pB0), rB0
	movapd	816-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	816-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	816-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	816-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	816-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	816-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	816-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	816-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 104
	movapd	832-128(pB0), rB0
	movapd	832-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	832-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	832-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	832-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	832-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	832-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	832-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	832-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 106
	movapd	848-128(pB0), rB0
	movapd	848-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	848-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	848-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	848-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	848-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	848-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	848-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	848-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 108
	movapd	864-128(pB0), rB0
	movapd	864-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	864-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	864-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	864-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	864-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	864-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	864-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	864-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 110
	movapd	880-128(pB0), rB0
	movapd	880-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	880-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	880-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	880-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	880-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	880-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	880-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	880-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 112
	movapd	896-128(pB0), rB0
	movapd	896-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	896-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	896-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	896-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	896-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	896-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	896-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	896-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 114
	movapd	912-128(pB0), rB0
	movapd	912-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	912-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	912-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	912-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	912-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	912-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	912-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	912-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 116
	movapd	928-128(pB0), rB0
	movapd	928-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	928-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	928-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	928-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	928-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	928-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	928-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	928-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 118
	movapd	944-128(pB0), rB0
	movapd	944-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	944-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	944-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	944-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	944-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	944-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	944-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	944-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 120
	movapd	960-128(pB0), rB0
	movapd	960-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	960-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	960-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	960-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	960-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	960-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	960-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	960-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 122
	movapd	976-128(pB0), rB0
	movapd	976-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	976-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	976-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	976-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	976-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	976-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	976-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	976-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 124
	movapd	992-128(pB0), rB0
	movapd	992-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	992-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	992-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	992-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	992-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	992-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	992-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	992-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 126
	movapd	1008-128(pB0), rB0
	movapd	1008-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1008-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1008-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1008-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1008-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1008-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1008-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1008-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 128
	movapd	1024-128(pB0), rB0
	movapd	1024-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1024-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1024-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1024-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1024-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1024-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1024-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1024-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 130
	movapd	1040-128(pB0), rB0
	movapd	1040-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1040-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1040-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1040-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1040-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1040-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1040-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1040-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 132
	movapd	1056-128(pB0), rB0
	movapd	1056-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1056-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1056-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1056-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1056-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1056-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1056-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1056-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 134
	movapd	1072-128(pB0), rB0
	movapd	1072-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1072-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1072-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1072-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1072-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1072-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1072-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1072-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 136
	movapd	1088-128(pB0), rB0
	movapd	1088-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1088-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1088-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1088-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1088-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1088-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1088-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1088-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 138
	movapd	1104-128(pB0), rB0
	movapd	1104-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1104-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1104-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1104-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1104-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1104-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1104-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1104-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 140
	movapd	1120-128(pB0), rB0
	movapd	1120-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1120-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1120-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1120-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1120-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1120-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1120-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1120-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 142
	movapd	1136-128(pB0), rB0
	movapd	1136-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1136-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1136-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1136-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1136-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1136-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1136-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1136-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 144
	movapd	1152-128(pB0), rB0
	movapd	1152-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1152-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1152-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1152-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1152-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1152-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1152-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1152-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 146
	movapd	1168-128(pB0), rB0
	movapd	1168-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1168-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1168-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1168-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1168-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1168-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1168-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1168-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 148
	movapd	1184-128(pB0), rB0
	movapd	1184-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1184-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1184-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1184-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1184-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1184-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1184-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1184-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 150
	movapd	1200-128(pB0), rB0
	movapd	1200-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1200-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1200-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1200-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1200-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1200-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1200-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1200-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 152
	movapd	1216-128(pB0), rB0
	movapd	1216-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1216-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1216-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1216-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1216-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1216-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1216-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1216-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 154
	movapd	1232-128(pB0), rB0
	movapd	1232-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1232-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1232-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1232-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1232-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1232-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1232-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1232-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 156
	movapd	1248-128(pB0), rB0
	movapd	1248-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1248-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1248-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1248-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1248-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1248-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1248-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1248-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 158
	movapd	1264-128(pB0), rB0
	movapd	1264-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1264-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1264-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1264-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1264-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1264-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1264-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1264-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 160
	movapd	1280-128(pB0), rB0
	movapd	1280-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1280-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1280-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1280-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1280-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1280-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1280-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1280-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 162
	movapd	1296-128(pB0), rB0
	movapd	1296-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1296-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1296-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1296-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1296-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1296-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1296-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1296-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 164
	movapd	1312-128(pB0), rB0
	movapd	1312-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1312-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1312-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1312-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1312-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1312-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1312-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1312-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 166
	movapd	1328-128(pB0), rB0
	movapd	1328-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1328-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1328-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1328-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1328-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1328-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1328-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1328-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 168
	movapd	1344-128(pB0), rB0
	movapd	1344-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1344-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1344-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1344-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1344-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1344-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1344-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1344-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 170
	movapd	1360-128(pB0), rB0
	movapd	1360-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1360-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1360-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1360-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1360-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1360-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1360-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1360-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 172
	movapd	1376-128(pB0), rB0
	movapd	1376-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1376-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1376-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1376-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1376-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1376-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1376-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1376-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 174
	movapd	1392-128(pB0), rB0
	movapd	1392-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1392-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1392-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1392-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1392-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1392-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1392-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1392-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 176
	movapd	1408-128(pB0), rB0
	movapd	1408-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1408-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1408-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1408-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1408-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1408-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1408-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1408-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 178
	movapd	1424-128(pB0), rB0
	movapd	1424-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1424-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1424-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1424-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1424-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1424-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1424-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1424-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 180
	movapd	1440-128(pB0), rB0
	movapd	1440-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1440-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1440-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1440-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1440-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1440-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1440-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1440-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 182
	movapd	1456-128(pB0), rB0
	movapd	1456-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1456-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1456-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1456-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1456-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1456-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1456-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1456-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 184
	movapd	1472-128(pB0), rB0
	movapd	1472-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1472-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1472-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1472-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1472-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1472-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1472-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1472-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 186
	movapd	1488-128(pB0), rB0
	movapd	1488-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1488-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1488-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1488-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1488-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1488-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1488-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1488-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 188
	movapd	1504-128(pB0), rB0
	movapd	1504-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1504-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1504-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1504-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1504-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1504-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1504-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1504-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 190
	movapd	1520-128(pB0), rB0
	movapd	1520-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1520-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1520-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1520-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1520-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1520-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1520-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1520-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 192
	movapd	1536-128(pB0), rB0
	movapd	1536-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1536-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1536-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1536-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1536-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1536-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1536-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1536-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 194
	movapd	1552-128(pB0), rB0
	movapd	1552-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1552-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1552-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1552-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1552-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1552-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1552-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1552-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 196
	movapd	1568-128(pB0), rB0
	movapd	1568-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1568-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1568-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1568-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1568-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1568-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1568-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1568-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 198
	movapd	1584-128(pB0), rB0
	movapd	1584-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1584-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1584-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1584-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1584-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1584-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1584-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1584-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 200
	movapd	1600-128(pB0), rB0
	movapd	1600-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1600-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1600-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1600-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1600-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1600-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1600-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1600-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 202
	movapd	1616-128(pB0), rB0
	movapd	1616-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1616-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1616-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1616-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1616-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1616-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1616-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1616-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 204
	movapd	1632-128(pB0), rB0
	movapd	1632-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1632-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1632-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1632-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1632-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1632-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1632-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1632-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 206
	movapd	1648-128(pB0), rB0
	movapd	1648-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1648-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1648-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1648-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1648-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1648-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1648-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1648-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 208
	movapd	1664-128(pB0), rB0
	movapd	1664-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1664-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1664-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1664-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1664-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1664-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1664-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1664-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 210
	movapd	1680-128(pB0), rB0
	movapd	1680-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1680-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1680-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1680-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1680-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1680-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1680-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1680-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 212
	movapd	1696-128(pB0), rB0
	movapd	1696-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1696-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1696-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1696-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1696-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1696-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1696-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1696-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 214
	movapd	1712-128(pB0), rB0
	movapd	1712-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1712-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1712-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1712-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1712-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1712-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1712-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1712-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 216
	movapd	1728-128(pB0), rB0
	movapd	1728-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1728-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1728-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1728-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1728-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1728-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1728-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1728-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 218
	movapd	1744-128(pB0), rB0
	movapd	1744-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1744-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1744-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1744-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1744-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1744-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1744-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1744-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 220
	movapd	1760-128(pB0), rB0
	movapd	1760-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1760-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1760-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1760-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1760-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1760-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1760-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1760-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 222
	movapd	1776-128(pB0), rB0
	movapd	1776-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1776-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1776-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1776-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1776-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1776-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1776-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1776-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 224
	movapd	1792-128(pB0), rB0
	movapd	1792-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1792-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1792-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1792-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1792-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1792-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1792-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1792-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 226
	movapd	1808-128(pB0), rB0
	movapd	1808-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1808-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1808-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1808-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1808-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1808-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1808-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1808-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 228
	movapd	1824-128(pB0), rB0
	movapd	1824-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1824-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1824-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1824-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1824-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1824-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1824-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1824-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 230
	movapd	1840-128(pB0), rB0
	movapd	1840-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1840-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1840-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1840-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1840-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1840-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1840-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1840-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 232
	movapd	1856-128(pB0), rB0
	movapd	1856-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1856-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1856-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1856-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1856-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1856-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1856-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1856-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 234
	movapd	1872-128(pB0), rB0
	movapd	1872-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1872-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1872-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1872-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1872-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1872-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1872-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1872-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 236
	movapd	1888-128(pB0), rB0
	movapd	1888-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1888-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1888-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1888-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1888-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1888-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1888-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1888-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 238
	movapd	1904-128(pB0), rB0
	movapd	1904-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1904-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1904-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1904-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1904-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1904-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1904-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1904-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 240
	movapd	1920-128(pB0), rB0
	movapd	1920-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1920-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1920-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1920-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1920-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1920-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1920-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1920-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 242
	movapd	1936-128(pB0), rB0
	movapd	1936-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1936-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1936-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1936-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1936-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1936-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1936-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1936-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 244
	movapd	1952-128(pB0), rB0
	movapd	1952-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1952-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1952-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1952-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1952-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1952-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1952-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1952-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 246
	movapd	1968-128(pB0), rB0
	movapd	1968-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1968-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1968-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1968-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1968-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1968-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1968-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1968-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 248
	movapd	1984-128(pB0), rB0
	movapd	1984-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	1984-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	1984-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	1984-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	1984-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	1984-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	1984-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	1984-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 250
	movapd	2000-128(pB0), rB0
	movapd	2000-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2000-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2000-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2000-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2000-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2000-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2000-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2000-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 252
	movapd	2016-128(pB0), rB0
	movapd	2016-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2016-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2016-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2016-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2016-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2016-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2016-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2016-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 254
	movapd	2032-128(pB0), rB0
	movapd	2032-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2032-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2032-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2032-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2032-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2032-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2032-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2032-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 256
	movapd	2048-128(pB0), rB0
	movapd	2048-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2048-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2048-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2048-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2048-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2048-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2048-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2048-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 258
	movapd	2064-128(pB0), rB0
	movapd	2064-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2064-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2064-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2064-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2064-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2064-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2064-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2064-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 260
	movapd	2080-128(pB0), rB0
	movapd	2080-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2080-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2080-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2080-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2080-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2080-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2080-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2080-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 262
	movapd	2096-128(pB0), rB0
	movapd	2096-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2096-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2096-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2096-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2096-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2096-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2096-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2096-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 264
	movapd	2112-128(pB0), rB0
	movapd	2112-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2112-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2112-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2112-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2112-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2112-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2112-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2112-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 266
	movapd	2128-128(pB0), rB0
	movapd	2128-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2128-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2128-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2128-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2128-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2128-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2128-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2128-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 268
	movapd	2144-128(pB0), rB0
	movapd	2144-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2144-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2144-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2144-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2144-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2144-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2144-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2144-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 270
	movapd	2160-128(pB0), rB0
	movapd	2160-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2160-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2160-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2160-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2160-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2160-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2160-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2160-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 272
	movapd	2176-128(pB0), rB0
	movapd	2176-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2176-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2176-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2176-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2176-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2176-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2176-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2176-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 274
	movapd	2192-128(pB0), rB0
	movapd	2192-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2192-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2192-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2192-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2192-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2192-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2192-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2192-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 276
	movapd	2208-128(pB0), rB0
	movapd	2208-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2208-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2208-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2208-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2208-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2208-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2208-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2208-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 278
	movapd	2224-128(pB0), rB0
	movapd	2224-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2224-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2224-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2224-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2224-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2224-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2224-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2224-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 280
	movapd	2240-128(pB0), rB0
	movapd	2240-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2240-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2240-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2240-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2240-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2240-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2240-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2240-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 282
	movapd	2256-128(pB0), rB0
	movapd	2256-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2256-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2256-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2256-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2256-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2256-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2256-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2256-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 284
	movapd	2272-128(pB0), rB0
	movapd	2272-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2272-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2272-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2272-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2272-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2272-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2272-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2272-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 286
	movapd	2288-128(pB0), rB0
	movapd	2288-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2288-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2288-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2288-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2288-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2288-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2288-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2288-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 288
	movapd	2304-128(pB0), rB0
	movapd	2304-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2304-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2304-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2304-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2304-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2304-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2304-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2304-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 290
	movapd	2320-128(pB0), rB0
	movapd	2320-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2320-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2320-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2320-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2320-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2320-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2320-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2320-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 292
	movapd	2336-128(pB0), rB0
	movapd	2336-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2336-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2336-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2336-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2336-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2336-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2336-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2336-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 294
	movapd	2352-128(pB0), rB0
	movapd	2352-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2352-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2352-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2352-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2352-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2352-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2352-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2352-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 296
	movapd	2368-128(pB0), rB0
	movapd	2368-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2368-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2368-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2368-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2368-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2368-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2368-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2368-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 298
	movapd	2384-128(pB0), rB0
	movapd	2384-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2384-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2384-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2384-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2384-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2384-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2384-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2384-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 300
	movapd	2400-128(pB0), rB0
	movapd	2400-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2400-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2400-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2400-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2400-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2400-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2400-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2400-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 302
	movapd	2416-128(pB0), rB0
	movapd	2416-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2416-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2416-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2416-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2416-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2416-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2416-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2416-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 304
	movapd	2432-128(pB0), rB0
	movapd	2432-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2432-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2432-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2432-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2432-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2432-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2432-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2432-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 306
	movapd	2448-128(pB0), rB0
	movapd	2448-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2448-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2448-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2448-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2448-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2448-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2448-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2448-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 308
	movapd	2464-128(pB0), rB0
	movapd	2464-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2464-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2464-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2464-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2464-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2464-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2464-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2464-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 310
	movapd	2480-128(pB0), rB0
	movapd	2480-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2480-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2480-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2480-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2480-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2480-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2480-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2480-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 312
	movapd	2496-128(pB0), rB0
	movapd	2496-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2496-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2496-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2496-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2496-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2496-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2496-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2496-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 314
	movapd	2512-128(pB0), rB0
	movapd	2512-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2512-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2512-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2512-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2512-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2512-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2512-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2512-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 316
	movapd	2528-128(pB0), rB0
	movapd	2528-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2528-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2528-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2528-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2528-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2528-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2528-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2528-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 318
	movapd	2544-128(pB0), rB0
	movapd	2544-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2544-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2544-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2544-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2544-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2544-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2544-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2544-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 320
	movapd	2560-128(pB0), rB0
	movapd	2560-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2560-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2560-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2560-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2560-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2560-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2560-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2560-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 322
	movapd	2576-128(pB0), rB0
	movapd	2576-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2576-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2576-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2576-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2576-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2576-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2576-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2576-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 324
	movapd	2592-128(pB0), rB0
	movapd	2592-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2592-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2592-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2592-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2592-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2592-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2592-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2592-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 326
	movapd	2608-128(pB0), rB0
	movapd	2608-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2608-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2608-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2608-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2608-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2608-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2608-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2608-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 328
	movapd	2624-128(pB0), rB0
	movapd	2624-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2624-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2624-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2624-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2624-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2624-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2624-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2624-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 330
	movapd	2640-128(pB0), rB0
	movapd	2640-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2640-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2640-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2640-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2640-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2640-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2640-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2640-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 332
	movapd	2656-128(pB0), rB0
	movapd	2656-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2656-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2656-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2656-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2656-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2656-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2656-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2656-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 334
	movapd	2672-128(pB0), rB0
	movapd	2672-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2672-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2672-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2672-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2672-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2672-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2672-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2672-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 336
	movapd	2688-128(pB0), rB0
	movapd	2688-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2688-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2688-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2688-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2688-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2688-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2688-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2688-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 338
	movapd	2704-128(pB0), rB0
	movapd	2704-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2704-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2704-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2704-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2704-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2704-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2704-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2704-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 340
	movapd	2720-128(pB0), rB0
	movapd	2720-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2720-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2720-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2720-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2720-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2720-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2720-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2720-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 342
	movapd	2736-128(pB0), rB0
	movapd	2736-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2736-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2736-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2736-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2736-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2736-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2736-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2736-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 344
	movapd	2752-128(pB0), rB0
	movapd	2752-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2752-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2752-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2752-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2752-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2752-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2752-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2752-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 346
	movapd	2768-128(pB0), rB0
	movapd	2768-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2768-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2768-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2768-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2768-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2768-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2768-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2768-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 348
	movapd	2784-128(pB0), rB0
	movapd	2784-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2784-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2784-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2784-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2784-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2784-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2784-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2784-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 350
	movapd	2800-128(pB0), rB0
	movapd	2800-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2800-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2800-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2800-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2800-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2800-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2800-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2800-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 352
	movapd	2816-128(pB0), rB0
	movapd	2816-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2816-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2816-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2816-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2816-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2816-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2816-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2816-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 354
	movapd	2832-128(pB0), rB0
	movapd	2832-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2832-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2832-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2832-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2832-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2832-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2832-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2832-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 356
	movapd	2848-128(pB0), rB0
	movapd	2848-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2848-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2848-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2848-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2848-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2848-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2848-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2848-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 358
	movapd	2864-128(pB0), rB0
	movapd	2864-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2864-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2864-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2864-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2864-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2864-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2864-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2864-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 360
	movapd	2880-128(pB0), rB0
	movapd	2880-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2880-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2880-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2880-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2880-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2880-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2880-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2880-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 362
	movapd	2896-128(pB0), rB0
	movapd	2896-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2896-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2896-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2896-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2896-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2896-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2896-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2896-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 364
	movapd	2912-128(pB0), rB0
	movapd	2912-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2912-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2912-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2912-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2912-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2912-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2912-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2912-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 366
	movapd	2928-128(pB0), rB0
	movapd	2928-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2928-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2928-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2928-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2928-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2928-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2928-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2928-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 368
	movapd	2944-128(pB0), rB0
	movapd	2944-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2944-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2944-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2944-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2944-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2944-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2944-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2944-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 370
	movapd	2960-128(pB0), rB0
	movapd	2960-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2960-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2960-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2960-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2960-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2960-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2960-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2960-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 372
	movapd	2976-128(pB0), rB0
	movapd	2976-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2976-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2976-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2976-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2976-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2976-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2976-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2976-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 374
	movapd	2992-128(pB0), rB0
	movapd	2992-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	2992-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	2992-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	2992-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	2992-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	2992-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	2992-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	2992-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 376
	movapd	3008-128(pB0), rB0
	movapd	3008-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3008-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3008-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3008-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3008-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3008-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3008-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3008-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 378
	movapd	3024-128(pB0), rB0
	movapd	3024-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3024-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3024-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3024-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3024-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3024-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3024-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3024-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 380
	movapd	3040-128(pB0), rB0
	movapd	3040-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3040-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3040-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3040-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3040-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3040-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3040-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3040-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 382
	movapd	3056-128(pB0), rB0
	movapd	3056-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3056-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3056-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3056-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3056-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3056-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3056-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3056-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 384
	movapd	3072-128(pB0), rB0
	movapd	3072-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3072-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3072-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3072-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3072-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3072-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3072-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3072-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 386
	movapd	3088-128(pB0), rB0
	movapd	3088-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3088-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3088-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3088-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3088-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3088-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3088-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3088-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 388
	movapd	3104-128(pB0), rB0
	movapd	3104-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3104-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3104-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3104-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3104-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3104-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3104-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3104-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 390
	movapd	3120-128(pB0), rB0
	movapd	3120-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3120-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3120-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3120-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3120-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3120-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3120-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3120-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 392
	movapd	3136-128(pB0), rB0
	movapd	3136-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3136-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3136-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3136-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3136-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3136-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3136-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3136-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 394
	movapd	3152-128(pB0), rB0
	movapd	3152-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3152-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3152-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3152-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3152-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3152-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3152-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3152-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 396
	movapd	3168-128(pB0), rB0
	movapd	3168-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3168-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3168-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3168-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3168-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3168-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3168-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3168-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 398
	movapd	3184-128(pB0), rB0
	movapd	3184-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3184-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3184-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3184-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3184-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3184-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3184-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3184-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 400
	movapd	3200-128(pB0), rB0
	movapd	3200-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3200-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3200-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3200-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3200-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3200-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3200-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3200-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 402
	movapd	3216-128(pB0), rB0
	movapd	3216-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3216-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3216-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3216-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3216-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3216-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3216-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3216-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 404
	movapd	3232-128(pB0), rB0
	movapd	3232-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3232-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3232-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3232-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3232-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3232-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3232-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3232-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 406
	movapd	3248-128(pB0), rB0
	movapd	3248-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3248-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3248-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3248-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3248-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3248-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3248-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3248-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 408
	movapd	3264-128(pB0), rB0
	movapd	3264-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3264-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3264-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3264-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3264-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3264-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3264-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3264-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 410
	movapd	3280-128(pB0), rB0
	movapd	3280-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3280-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3280-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3280-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3280-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3280-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3280-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3280-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 412
	movapd	3296-128(pB0), rB0
	movapd	3296-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3296-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3296-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3296-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3296-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3296-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3296-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3296-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 414
	movapd	3312-128(pB0), rB0
	movapd	3312-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3312-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3312-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3312-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3312-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3312-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3312-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3312-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 416
	movapd	3328-128(pB0), rB0
	movapd	3328-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3328-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3328-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3328-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3328-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3328-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3328-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3328-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 418
	movapd	3344-128(pB0), rB0
	movapd	3344-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3344-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3344-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3344-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3344-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3344-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3344-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3344-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 420
	movapd	3360-128(pB0), rB0
	movapd	3360-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3360-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3360-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3360-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3360-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3360-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3360-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3360-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 422
	movapd	3376-128(pB0), rB0
	movapd	3376-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3376-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3376-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3376-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3376-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3376-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3376-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3376-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 424
	movapd	3392-128(pB0), rB0
	movapd	3392-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3392-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3392-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3392-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3392-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3392-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3392-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3392-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 426
	movapd	3408-128(pB0), rB0
	movapd	3408-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3408-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3408-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3408-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3408-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3408-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3408-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3408-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 428
	movapd	3424-128(pB0), rB0
	movapd	3424-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3424-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3424-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3424-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3424-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3424-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3424-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3424-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 430
	movapd	3440-128(pB0), rB0
	movapd	3440-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3440-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3440-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3440-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3440-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3440-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3440-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3440-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 432
	movapd	3456-128(pB0), rB0
	movapd	3456-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3456-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3456-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3456-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3456-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3456-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3456-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3456-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 434
	movapd	3472-128(pB0), rB0
	movapd	3472-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3472-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3472-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3472-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3472-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3472-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3472-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3472-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 436
	movapd	3488-128(pB0), rB0
	movapd	3488-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3488-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3488-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3488-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3488-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3488-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3488-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3488-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 438
	movapd	3504-128(pB0), rB0
	movapd	3504-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3504-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3504-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3504-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3504-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3504-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3504-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3504-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 440
	movapd	3520-128(pB0), rB0
	movapd	3520-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3520-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3520-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3520-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3520-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3520-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3520-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3520-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 442
	movapd	3536-128(pB0), rB0
	movapd	3536-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3536-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3536-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3536-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3536-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3536-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3536-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3536-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 444
	movapd	3552-128(pB0), rB0
	movapd	3552-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3552-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3552-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3552-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3552-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3552-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3552-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3552-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 446
	movapd	3568-128(pB0), rB0
	movapd	3568-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3568-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3568-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3568-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3568-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3568-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3568-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3568-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 448
	movapd	3584-128(pB0), rB0
	movapd	3584-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3584-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3584-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3584-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3584-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3584-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3584-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3584-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 450
	movapd	3600-128(pB0), rB0
	movapd	3600-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3600-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3600-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3600-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3600-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3600-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3600-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3600-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 452
	movapd	3616-128(pB0), rB0
	movapd	3616-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3616-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3616-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3616-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3616-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3616-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3616-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3616-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 454
	movapd	3632-128(pB0), rB0
	movapd	3632-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3632-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3632-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3632-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3632-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3632-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3632-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3632-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 456
	movapd	3648-128(pB0), rB0
	movapd	3648-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3648-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3648-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3648-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3648-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3648-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3648-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3648-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 458
	movapd	3664-128(pB0), rB0
	movapd	3664-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3664-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3664-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3664-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3664-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3664-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3664-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3664-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 460
	movapd	3680-128(pB0), rB0
	movapd	3680-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3680-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3680-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3680-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3680-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3680-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3680-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3680-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 462
	movapd	3696-128(pB0), rB0
	movapd	3696-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3696-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3696-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3696-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3696-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3696-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3696-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3696-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 464
	movapd	3712-128(pB0), rB0
	movapd	3712-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3712-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3712-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3712-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3712-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3712-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3712-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3712-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 466
	movapd	3728-128(pB0), rB0
	movapd	3728-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3728-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3728-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3728-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3728-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3728-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3728-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3728-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 468
	movapd	3744-128(pB0), rB0
	movapd	3744-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3744-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3744-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3744-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3744-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3744-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3744-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3744-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 470
	movapd	3760-128(pB0), rB0
	movapd	3760-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3760-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3760-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3760-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3760-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3760-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3760-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3760-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 472
	movapd	3776-128(pB0), rB0
	movapd	3776-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3776-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3776-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3776-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3776-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3776-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3776-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3776-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 474
	movapd	3792-128(pB0), rB0
	movapd	3792-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3792-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3792-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3792-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3792-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3792-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3792-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3792-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 476
	movapd	3808-128(pB0), rB0
	movapd	3808-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3808-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3808-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3808-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3808-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3808-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3808-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3808-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 478
	movapd	3824-128(pB0), rB0
	movapd	3824-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3824-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3824-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3824-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3824-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3824-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3824-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3824-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 480
	movapd	3840-128(pB0), rB0
	movapd	3840-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3840-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3840-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3840-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3840-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3840-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3840-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3840-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 482
	movapd	3856-128(pB0), rB0
	movapd	3856-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3856-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3856-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3856-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3856-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3856-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3856-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3856-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 484
	movapd	3872-128(pB0), rB0
	movapd	3872-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3872-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3872-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3872-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3872-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3872-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3872-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3872-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 486
	movapd	3888-128(pB0), rB0
	movapd	3888-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3888-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3888-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3888-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3888-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3888-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3888-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3888-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 488
	movapd	3904-128(pB0), rB0
	movapd	3904-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3904-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3904-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3904-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3904-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3904-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3904-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3904-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 490
	movapd	3920-128(pB0), rB0
	movapd	3920-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3920-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3920-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3920-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3920-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3920-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3920-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3920-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 492
	movapd	3936-128(pB0), rB0
	movapd	3936-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3936-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3936-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3936-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3936-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3936-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3936-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3936-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 494
	movapd	3952-128(pB0), rB0
	movapd	3952-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3952-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3952-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3952-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3952-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3952-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3952-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3952-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 496
	movapd	3968-128(pB0), rB0
	movapd	3968-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3968-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3968-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3968-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3968-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3968-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3968-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3968-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 498
	movapd	3984-128(pB0), rB0
	movapd	3984-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	3984-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	3984-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	3984-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	3984-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	3984-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	3984-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	3984-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 500
	movapd	4000-128(pB0), rB0
	movapd	4000-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	4000-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	4000-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	4000-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	4000-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	4000-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	4000-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	4000-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 502
	movapd	4016-128(pB0), rB0
	movapd	4016-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	4016-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	4016-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	4016-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	4016-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	4016-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	4016-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	4016-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 504
	movapd	4032-128(pB0), rB0
	movapd	4032-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	4032-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	4032-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	4032-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	4032-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	4032-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	4032-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	4032-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 506
	movapd	4048-128(pB0), rB0
	movapd	4048-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	4048-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	4048-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	4048-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	4048-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	4048-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	4048-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	4048-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 508
	movapd	4064-128(pB0), rB0
	movapd	4064-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	4064-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	4064-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	4064-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	4064-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	4064-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	4064-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	4064-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

#if KB > 510
	movapd	4080-128(pB0), rB0
	movapd	4080-128(pA0), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC00
	movapd	4080-128(pA0,lda), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC01
	movapd	4080-128(pA0,lda,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC02
	movapd	4080-128(pA0,lda3), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC03
	movapd	4080-128(pA0,lda,4), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC04
	movapd	4080-128(pA0,lda5), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC05
	movapd	4080-128(pA0,lda3,2), rA0
	mulpd	rB0,rA0
	addpd	rA0,rC06
	mulpd	4080-128(pA0,lda7), rB0
	addpd	rB0,rC07
#endif

/*       jne KLOOP */
/*
 *      pC[0-8] = rC[0-8]
 */

        haddpd  rC01,rC00
                                lea     (pA0,lda,8), pA0
        #ifdef BETAN1
                subpd   rC0, rC00
        #elif !defined(BETA0)
                addpd   rC0, rC00
        #endif
        movlpd  rC00, (pC0)
        movhpd  rC00, CMUL(8)(pC0)

        haddpd  rC03,rC02
        #ifdef BETAN1
                subpd   rC2, rC02
        #elif !defined(BETA0)
                addpd   rC2, rC02
        #endif
        movlpd  rC02, CMUL(16)(pC0)
        movhpd  rC02, CMUL(24)(pC0)

        haddpd  rC05,rC04
        #ifdef BETAN1
                subpd   rC4, rC04
        #elif !defined(BETA0)
                addpd   rC4, rC04
        #endif
        movlpd  rC04, CMUL(32)(pC0)
        movhpd  rC04, CMUL(40)(pC0)

        haddpd  rC07,rC06
        #ifdef BETAN1
                subpd   rC6, rC06
        #elif !defined(BETA0)
                addpd   rC6, rC06
        #endif

        movlpd  rC06, CMUL(48)(pC0)
        movhpd  rC06, CMUL(56)(pC0)

        add     $8*CMUL(8), pC0
        sub     $8, MM
        jnz     UMLOOP

        sub     incAn, pA0
        add     incCn, pC0
        add     ldb, pB0
        sub     $1, NN

        jnz     UNLOOP

/* UDONE: */
        movq    -8(%rsp), %rbp
        movq    -16(%rsp), %rbx
        movq    -24(%rsp), %r12
        movq    -32(%rsp), %r13
        movq    -40(%rsp), %r14
        movq    -48(%rsp), %r15
        ret
