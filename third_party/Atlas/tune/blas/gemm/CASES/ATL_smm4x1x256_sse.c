/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2008 R. Clint Whaley
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


#include "atlas_asm.h"   /* usually do this in include file */
#ifndef ATL_SSE1
   #error "This routine requires SSE1!"
#endif

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
#if KB > 256
   #error "KB must be <= 256!"
#endif
#if (MB/4)*4 != MB
   #error "MB must be a multiple of 4!"
#endif

#if 1
   #define pref2(mem) prefetcht1        mem
   #define prefB(mem) prefetcht1        mem
   #define prefC(mem) prefetcht0        mem
#else
   #define pref2(mem)
   #define prefB(mem)
   #define prefC(mem)
#endif

#define PFAINC 16

#define rA0     %xmm0
#define rB0     %xmm1
#define rC0     %xmm2
#define rC1     %xmm3
#define rC2     %xmm4
#define rC3     %xmm5
#define rc0     %xmm6
#define BETA    %xmm7

#ifdef ATL_GAS_x8632
   #define pC0          %esi
   #define pA0          %ecx
   #define ldab3        %eax
   #define pB0          %edi
   #define ldab         %edx
   #define pfA          %ebp
   #define stM          %ebx

   #define subL subl
   #define addL addl
   #define movL movl
#elif defined(ATL_GAS_x8664)
   #define pC0          %rdx
   #define pA0          %rcx
   #define ldab3        %rax
   #define pB0          %rbx
   #define ldab         %rsi
   #define pfA          %rbp
   #define stM          %rdi
   #define stN          %r8
   #define AORIG        %r9
   #define MORIG        %r10
   #define INCCn        %r11

   #define subL subq
   #define addL addq
   #define movL movq
#else
   #error "This kernel requires a gas x86 assembler!"
#endif

/*
                      %rdi/4       %rsi/8       %rdx/12          %xmm0/16
 void ATL_AUSERMM(const int M, const int N, const int K, const TYPE alpha,
                       %rcx/20         %r8/24         %r9/28             32
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                        %xmm1/36    16/40          24/44
                 const TYPE beta, TYPE *C, const int ldc)
*/
        .text
.global ATL_asmdecor(ATL_USERMM)
ALIGN16
ATL_asmdecor(ATL_USERMM):
#ifdef ATL_GAS_x8632
      #define FSIZE     32
        subl    $FSIZE, %esp
        movl    %ebp,    (%esp)
        movl    %ebx,   4(%esp)
        movl    %esi,   8(%esp)
        movl    %edi,  12(%esp)
/*
 *      Load (and save where necessary) parameters
 */
   #ifdef BETAX
        movss   FSIZE+36(%esp), BETA
        shufps  $0x00, BETA, BETA
   #endif
      #define stN 16(%esp)
        movl    FSIZE+8(%esp), %eax     /* get N */
        movl    %eax, stN               /* store to stack */
      #define  AORIG     20(%esp)
        movl    FSIZE+20(%esp), pA0
      #define MORIG 24(%esp)
        movl    FSIZE+4(%esp), stM      /* get M */
        movl    stM, MORIG              /* store to stack */
      #define INCCn     28(%esp)
        movl    FSIZE+44(%esp), %eax    /* get ldc */
        subl    stM, %eax               /* incCn = (ldc-M) */
      #ifdef SCPLX
        shl     $3, %eax                /* incCn = (ldc-M)*sizeof */
      #else
        shl     $2, %eax                /* incCn = (ldc-M)*sizeof */
      #endif
        movl    %eax, INCCn             /* store to stack */
        movl    FSIZE+20(%esp), pA0
        movl    FSIZE+28(%esp), pB0
        movl    FSIZE+40(%esp), pC0
#else                                   /* x86-64 prologue */
/*
 *      Save callee-saved registers
 */
        movq    %rbp,  -8(%rsp)
        movq    %rbx, -16(%rsp)
/*        movq    %r12, -24(%rsp) */
/*
 *      Get parameters in correct registers
 */
        movq    %rsi, stN
        movq    %r9, pB0
        shufps  $0x00, %xmm1, %xmm1
        movaps  %xmm1, BETA
        movq    16(%rsp), pC0
        movslq  24(%rsp), INCCn         /* incCn = ldc (32-to-64 extension) */
        sub     stM, INCCn              /* incCn = ldc-M */
   #ifdef SCPLX
        shl     $3, INCCn               /* incCn = (ldc-M)*sizeof */
   #else
        shl     $2, INCCn               /* incCn = (ldc-M)*sizeof */
   #endif
        movq    stM, MORIG
#endif
        mov     stM, pfA
        imul    $KB*4, pfA              /* pfA = M * K * sizeof */
        add     pA0, pfA                /* pfA = A + M*K*sizeof */
        mov     $KB*4, ldab             /* ld[a,b] = KB*sizeof */
        lea     (ldab,ldab,2), ldab3    /* ldab3 = 3*ldab */
        sub     $-128, pA0
        sub     $-128, pB0
        movL    pA0, AORIG
ALIGN32
NLOOP:
MLOOP:
/* BEGIN KLOOP */

#if KB > 3
	movaps	-128(pB0), rC3
	movaps	-128(pA0), rC0
	mulps	rC3, rC0
	movaps	-128(pA0,ldab), rC1
	mulps	rC3, rC1
	movaps	-128(pA0,ldab,2), rC2
	mulps	rC3, rC2
	mulps	-128(pA0,ldab3), rC3
#endif
#if KB > 7
	movaps	-128+16(pB0), rB0
	movaps	-128+16(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+16(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+16(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+16(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 11
	movaps	-128+32(pB0), rB0
	movaps	-128+32(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+32(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+32(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+32(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 15
	movaps	-128+48(pB0), rB0
	movaps	-128+48(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+48(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+48(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+48(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 19
	movaps	-128+64(pB0), rB0
	movaps	-128+64(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+64(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+64(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+64(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 23
	movaps	-128+80(pB0), rB0
	movaps	-128+80(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+80(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+80(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+80(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 27
	movaps	-128+96(pB0), rB0
	movaps	-128+96(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+96(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+96(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+96(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 31
	movaps	-128+112(pB0), rB0
	movaps	-128+112(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+112(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+112(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+112(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 35
	movaps	-128+128(pB0), rB0
	movaps	-128+128(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+128(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+128(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+128(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 39
	movaps	-128+144(pB0), rB0
	movaps	-128+144(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+144(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+144(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+144(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 43
	movaps	-128+160(pB0), rB0
	movaps	-128+160(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+160(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+160(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+160(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 47
	movaps	-128+176(pB0), rB0
	movaps	-128+176(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+176(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+176(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+176(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 51
	movaps	-128+192(pB0), rB0
	movaps	-128+192(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+192(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+192(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+192(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 55
	movaps	-128+208(pB0), rB0
	movaps	-128+208(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+208(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+208(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+208(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 59
	movaps	-128+224(pB0), rB0
	movaps	-128+224(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+224(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+224(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+224(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 63
	movaps	-128+240(pB0), rB0
	movaps	-128+240(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+240(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+240(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+240(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 67
	movaps	-128+256(pB0), rB0
	movaps	-128+256(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+256(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+256(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+256(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 71
	movaps	-128+272(pB0), rB0
	movaps	-128+272(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+272(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+272(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+272(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 75
	movaps	-128+288(pB0), rB0
	movaps	-128+288(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+288(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+288(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+288(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 79
	movaps	-128+304(pB0), rB0
	movaps	-128+304(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+304(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+304(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+304(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 83
	movaps	-128+320(pB0), rB0
	movaps	-128+320(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+320(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+320(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+320(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 87
	movaps	-128+336(pB0), rB0
	movaps	-128+336(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+336(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+336(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+336(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 91
	movaps	-128+352(pB0), rB0
	movaps	-128+352(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+352(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+352(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+352(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 95
	movaps	-128+368(pB0), rB0
	movaps	-128+368(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+368(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+368(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+368(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 99
	movaps	-128+384(pB0), rB0
	movaps	-128+384(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+384(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+384(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+384(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 103
	movaps	-128+400(pB0), rB0
	movaps	-128+400(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+400(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+400(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+400(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 107
	movaps	-128+416(pB0), rB0
	movaps	-128+416(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+416(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+416(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+416(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 111
	movaps	-128+432(pB0), rB0
	movaps	-128+432(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+432(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+432(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+432(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 115
	movaps	-128+448(pB0), rB0
	movaps	-128+448(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+448(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+448(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+448(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 119
	movaps	-128+464(pB0), rB0
	movaps	-128+464(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+464(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+464(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+464(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 123
	movaps	-128+480(pB0), rB0
	movaps	-128+480(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+480(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+480(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+480(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 127
	movaps	-128+496(pB0), rB0
	movaps	-128+496(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+496(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+496(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+496(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 131
	movaps	-128+512(pB0), rB0
	movaps	-128+512(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+512(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+512(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+512(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 135
	movaps	-128+528(pB0), rB0
	movaps	-128+528(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+528(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+528(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+528(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 139
	movaps	-128+544(pB0), rB0
	movaps	-128+544(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+544(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+544(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+544(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 143
	movaps	-128+560(pB0), rB0
	movaps	-128+560(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+560(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+560(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+560(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 147
	movaps	-128+576(pB0), rB0
	movaps	-128+576(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+576(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+576(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+576(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 151
	movaps	-128+592(pB0), rB0
	movaps	-128+592(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+592(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+592(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+592(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 155
	movaps	-128+608(pB0), rB0
	movaps	-128+608(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+608(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+608(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+608(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 159
	movaps	-128+624(pB0), rB0
	movaps	-128+624(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+624(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+624(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+624(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 163
	movaps	-128+640(pB0), rB0
	movaps	-128+640(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+640(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+640(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+640(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 167
	movaps	-128+656(pB0), rB0
	movaps	-128+656(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+656(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+656(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+656(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 171
	movaps	-128+672(pB0), rB0
	movaps	-128+672(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+672(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+672(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+672(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 175
	movaps	-128+688(pB0), rB0
	movaps	-128+688(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+688(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+688(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+688(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 179
	movaps	-128+704(pB0), rB0
	movaps	-128+704(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+704(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+704(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+704(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 183
	movaps	-128+720(pB0), rB0
	movaps	-128+720(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+720(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+720(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+720(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 187
	movaps	-128+736(pB0), rB0
	movaps	-128+736(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+736(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+736(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+736(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 191
	movaps	-128+752(pB0), rB0
	movaps	-128+752(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+752(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+752(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+752(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 195
	movaps	-128+768(pB0), rB0
	movaps	-128+768(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+768(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+768(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+768(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 199
	movaps	-128+784(pB0), rB0
	movaps	-128+784(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+784(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+784(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+784(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 203
	movaps	-128+800(pB0), rB0
	movaps	-128+800(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+800(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+800(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+800(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 207
	movaps	-128+816(pB0), rB0
	movaps	-128+816(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+816(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+816(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+816(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 211
	movaps	-128+832(pB0), rB0
	movaps	-128+832(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+832(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+832(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+832(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 215
	movaps	-128+848(pB0), rB0
	movaps	-128+848(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+848(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+848(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+848(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 219
	movaps	-128+864(pB0), rB0
	movaps	-128+864(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+864(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+864(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+864(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 223
	movaps	-128+880(pB0), rB0
	movaps	-128+880(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+880(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+880(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+880(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 227
	movaps	-128+896(pB0), rB0
	movaps	-128+896(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+896(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+896(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+896(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 231
	movaps	-128+912(pB0), rB0
	movaps	-128+912(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+912(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+912(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+912(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 235
	movaps	-128+928(pB0), rB0
	movaps	-128+928(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+928(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+928(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+928(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 239
	movaps	-128+944(pB0), rB0
	movaps	-128+944(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+944(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+944(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+944(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 243
	movaps	-128+960(pB0), rB0
	movaps	-128+960(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+960(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+960(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+960(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 247
	movaps	-128+976(pB0), rB0
	movaps	-128+976(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+976(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+976(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+976(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 251
	movaps	-128+992(pB0), rB0
	movaps	-128+992(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+992(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+992(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+992(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
#if KB > 255
	movaps	-128+1008(pB0), rB0
	movaps	-128+1008(pA0), rA0
	mulps	rB0, rA0
	addps	rA0, rC0
	movaps	-128+1008(pA0,ldab), rA0
	mulps	rB0, rA0
	addps	rA0, rC1
	movaps	-128+1008(pA0,ldab,2), rA0
	mulps	rB0, rA0
	addps	rA0, rC2
	mulps	-128+1008(pA0,ldab3), rB0
	addps	rB0, rC3
#endif
/* End KLOOP */
/*
 *      Get these bastard things summed up correctly
 *      Note this summation is Camm's, as his sequence was faster
 *      than the piece of crap I came up with
 */
        movaps          rC0, rA0        /* rA0 = c0d    c0c    c0b    c0a */
        unpcklps        rC1, rC0        /* rC0 = c1b    c0b    c1a    c0d */
        movaps          rC2, rB0        /* rB0 = c2d    c2c    c2b    c2a */
        unpckhps        rC1, rA0        /* rA0 = c1d    c0d    c1c    c0c */
                                   pref2((pfA))
        unpcklps        rC3, rC2        /* rC2 = c3b    c2b    c3a    c2a */
        addps           rA0, rC0        /* rC0 = c1bd   c0bd   c1ac   c0ac */
        unpckhps        rC3, rB0        /* rB0 = c3d    c2d    c3c    c2c */
        movaps          rC0, rA0        /* rA0 = c1bd   c0bd   c1ac   c0ac */
        addps           rB0, rC2        /* rC2 = c3bd   c2bd   c3ac   c2ac */
                        prefC(16(pC0))
        shufps          $0x44,rC2,rC0   /* rC0 = c3ac   c2ac   c1ac   c0ac */
                                    add     $PFAINC, pfA
        shufps          $0xEE,rC2,rA0   /* rA0 = c3bd   c2bd   c1bd   c0bd */
        addps           rA0, rC0        /* rC0 = c3abcd c2abcd c1abcd c0abcd */
#ifndef BETA0
        movups          (pC0), rc0
   #ifdef SCPLX                         /* rc0 = XX   c1  XX  c0 */
        movss           24(pC0), rB0    /* rB0 = XX   XX  XX  c3 */
        movlhps         rB0, rB0        /* rB0 = XX   c3  XX  c3 */
        movlps          16(pC0), rB0    /* rB0 = XX   c3  XX  c2 */
        shufps          $0x88, rB0, rc0
   #endif
   #ifdef BETAX
        mulps           BETA, rc0
   #endif
   #ifdef BETAN1
        subps           rc0, rC0
   #else
        addps           rc0, rC0
   #endif
#endif
/*
 *      Write results back to C
 */
   #ifdef SCPLX
                                        /* rC0 = c3 c2 c1 c0 */
#ifdef ATL_SSE2
        pshufd  $0xB1, rC0, rC1         /* rC1 = c2 c3 c0 c1 */
#else
        movaps  rC0, rC1
        shufps  $0xB1, rC1, rC1         /* rC1 = c2 c3 c0 c1 */
#endif
        movhlps rC0, rC2                /* rC2 =  X  X c3 c2 */
        movhlps rC1, rC3                /* rC3 =  X  X c2 c3 */
        movss   rC0, (pC0)
        movss   rC1, 8(pC0)
        movss   rC2, 16(pC0)
        movss   rC3, 24(pC0)
   #else
        movups  rC0, (pC0)
   #endif
/*
 *      Increment ptrs and continue MLOOP
 */
   #ifdef SCPLX
        add     $32, pC0
   #else
        add     $16, pC0
   #endif
        lea     (pA0, ldab, 4), pA0
        sub     $4, stM
        jnz     MLOOP
/*
 *      incrementer pointers and continue NLOOP
 */
        movL    MORIG, stM
        addL    INCCn, pC0
        movL    AORIG, pA0
        add     ldab, pB0
        subL    $1, stN
        jnz     NLOOP
/*
 *      Epilogue: restore callee-saved iregs and return
 */
DONE:
#ifdef ATL_GAS_x8632
        movl    (%esp), %ebp
        movl    4(%esp), %ebx
        movl    8(%esp), %esi
        movl    12(%esp), %edi
        addl    $FSIZE, %esp
#else
        movq     -8(%rsp), %rbp
        movq    -16(%rsp), %rbx
/*        movq    -24(%rsp), %r12 */
#endif
        ret

