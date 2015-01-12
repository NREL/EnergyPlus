/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2012, 2011 R. Clint Whaley
 *
 * Code contributers : R. Clint Whaley, Tom Wallace
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
#ifndef ATL_GAS_ARM
   #error "This routine requires GAS/ARM assembly"
#endif
#if defined(KB) && (KB/2)*2 != KB
   #error "KB must be a multiple of 2"
#endif
#if defined(MB) && (MB/4)*4 != MB
   #error "MB must be a multiple of 4"
#endif
#if defined(NB) && (NB/4)*4 != NB
   #error "NB must be a multiple of 4"
#endif

/*
 * This routine is a simple 4x4 register blocked routine, with two iterations
 * of software pipelining on the load/use of A and B for the arm.
 * Prefetch (PLD) seems to reduce performance.
 * Gets around 91% (95%) of peak for beta=1 (0) in-cache, but drops to less
 * than 70% out-of-cache, so memory is main problem.
 */
#define M 	r0
#define N 	r1
#define K 	r2
#define pA0 	r3
#define lda	r4
#define pB0	r5
#define pA00    r6
#define K0	r7
#define pfA	r8
#define pC0	r9
#define ldc	r10
#define M0	r11
#define zero    r12
#define PTR     r14
/*      SP      r13 */
/*      LR      r14 */
/*      PC      r15 */
#define ldb     lda

#define rC00	d4
#define rC10	d5
#define rC20	d6
#define rC30	d7
#define rC01	d8
#define rC11	d9
#define rC21	d10
#define rC31	d11
#define rC02	d12
#define rC12	d13
#define rC22	d14
#define rC32	d15
#define rC03	d20
#define rC13	d21
#define rC23	d22
#define rC33	d23

#define rA0     d24
#define ra0 	d25
#define rA1     d26
#define ra1 	d27
#define rA2     d28
#define ra2 	d29
#define rA3     d30
#define ra3 	d31
#define rB0     d0
#define rb0     d1
#define rB1     d2
#define rb1     d3
#define rB2     d16
#define rb2     d17
#define rB3     d18
#define rb3     d19

/*                       r0           r1           r2                 0
void ATL_USERMM (const int M, const int N, const int K, const TYPE alpha,
                             8             12             16             20
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                             24       32             40
                 const TYPE beta, TYPE *C, const int ldc)
*/
#define FSIZE 64+36
#include "atlas_asm.h"
.code 32
.fpu vfpv3
#ifdef ATL_ARM_HARDFP
.eabi_attribute 28, 1
#endif
.text
.align 2
.globl ATL_asmdecor(ATL_USERMM)
.type  ATL_asmdecor(ATL_USERMM), %function
ATL_asmdecor(ATL_USERMM):
   fstmDBd SP!, {d8-d15}
   stmDB SP!, {r4-r11,r14}
/*
 * Load needed vals to registers
 */
#ifdef ATL_ARM_HARDFP
   add M0, SP, #FSIZE
   ldmIA M0, {lda,pB0,pA00,pC0,ldc} /* lda,B,ldb,C,ldc */
   vmov M0, zero, d1  /* put beta in M0, zero */
   push {M0,zero}     /* put beta on stack */
#else
   add M0, SP, #8+FSIZE
   ldmIA M0, {pA0,lda,pB0,pA00,K0,pfA,pC0,ldc}  /* K0/pfA hold beta, unused */
#endif
   mov lda, lda, LSL #3   /* lda *= sizeof */
   #ifdef DCPLX
      mov ldc, ldc, LSL #4   /* ldc *= sizeof */
   #else
      mov ldc, ldc, LSL #3   /* ldc *= sizeof */
   #endif
   #if 0  /* prefetch doesn't help */
      mul pfA, lda, M           /* pfA = M*lda */
      add pfA, pA0, pfA, LSR #1         /* pfA = pA + M*lda/2 */
   #endif
   mov M0, M
   mov K0, K
   mov zero, #0
   mov pA00, pA0

   NLOOP:
      MLOOP:
         #if 0  /* prefetch does not seem to help */
            pld [pfA]
            add pfA, pfA, #64
         #endif
         #ifdef BETA0
            fmdhr rC00, zero
            fmdlr rC00, zero
            fcpyd rC10, rC00
            fcpyd rC20, rC00
            fcpyd rC30, rC00
            fcpyd rC01, rC00
            fcpyd rC11, rC00
            fcpyd rC21, rC00
            fcpyd rC31, rC00
            fcpyd rC02, rC00
            fcpyd rC12, rC00
            fcpyd rC22, rC00
            fcpyd rC32, rC00
            fcpyd rC03, rC00
            fcpyd rC13, rC00
            fcpyd rC23, rC00
            fcpyd rC33, rC00
         #else
            add PTR, pC0, ldc
            #ifdef BETAX
               #ifdef ATL_ARM_HARDFP
                  fldd rb0, [SP, #0]
               #else
                  fldd rb0, [SP, #(FSIZE+24)]
               #endif
            #endif
            #ifdef DCPLX
               fldd rC00, [pC0]
               fldd rC10, [pC0, #16]
               fldd rC20, [pC0, #32]
               fldd rC30, [pC0, #48]
               fldd rC01, [PTR]
               fldd rC11, [PTR, #16]
               fldd rC21, [PTR, #32]
               fldd rC31, [PTR, #48]
               add PTR, PTR, ldc
               fldd rC02, [PTR]
               fldd rC12, [PTR, #16]
               fldd rC22, [PTR, #32]
               fldd rC32, [PTR, #48]
               add PTR, PTR, ldc
               fldd rC03, [PTR]
               fldd rC13, [PTR, #16]
               fldd rC23, [PTR, #32]
               fldd rC33, [PTR, #48]
            #else
               fldmIAd pC0, {rC00,rC10,rC20,rC30}
               fldmIAd PTR, {rC01,rC11,rC21,rC31}
               add PTR, PTR, ldc
               fldmIAd PTR, {rC02,rC12,rC22,rC32}
               add PTR, PTR, ldc
               fldmIAd PTR, {rC03,rC13,rC23,rC33}
            #endif
            #ifdef BETAX
               fmuld rC00, rC00, rb0
               fmuld rC10, rC10, rb0
               fmuld rC20, rC20, rb0
               fmuld rC30, rC30, rb0
               fmuld rC01, rC01, rb0
               fmuld rC11, rC11, rb0
               fmuld rC21, rC21, rb0
               fmuld rC31, rC31, rb0
               fmuld rC02, rC02, rb0
               fmuld rC12, rC12, rb0
               fmuld rC22, rC22, rb0
               fmuld rC32, rC32, rb0
               fmuld rC03, rC03, rb0
               fmuld rC13, rC13, rb0
               fmuld rC23, rC23, rb0
               fmuld rC33, rC33, rb0
            #endif
         #endif
         fldmiad pB0, {rB0, rb0}
         add PTR, pA0, lda
         fldmiad pA0, {rA0, ra0}
         fldmiad PTR, {rA1, ra1}
         add PTR, PTR, lda
         fldmiad PTR, {rA2, ra2}
         add PTR, PTR, lda
         fldd rA3, [PTR]
         add PTR, pB0, ldb
         fldmiad PTR, {rB1, rb1}
         add PTR, PTR, ldb
         fldmiad PTR, {rB2, rb2}
         add PTR, PTR, ldb
         fldd rB3, [PTR]
         add PTR, pA0, lda, LSL #1
	 KLOOP:
            add PTR, PTR, lda
            fmacd rC00, rA0, rB0
            fldd ra3, [PTR, #8]
            add PTR, pB0, ldb, LSL #1
            fmacd rC10, rA1, rB0
            add PTR, PTR, ldb
            fldd rb3, [PTR, #8]
            fmacd rC20, rA2, rB0
            add pB0, #16
            fmacd rC30, rA3, rB0
            subs K, K, #2  	        /* K -= 2, set cond codes */

            fmacd rC01, rA0, rB1
            add pA0, #16
            fmacd rC11, rA1, rB1
            flddne rB0, [pB0]
            fmacd rC21, rA2, rB1
            fmacd rC31, rA3, rB1
            add PTR, pB0, ldb

            fmacd rC02, rA0, rB2
            flddne rB1, [PTR]
            fmacd rC12, rA1, rB2
            add PTR, PTR, ldb
            fmacd rC22, rA2, rB2
            fmacd rC32, rA3, rB2
            flddne rB2, [PTR]

            add PTR, pA0, lda
            fmacd rC03, rA0, rB3
            flddne rA0, [pA0]
            fmacd rC13, rA1, rB3
            flddne rA1, [PTR]
            fmacd rC23, rA2, rB3
            add PTR, PTR, lda
            fmacd rC33, rA3, rB3
            flddne rA2, [PTR]


            fmacd rC00, ra0, rb0
            add PTR, PTR, lda
            fmacd rC10, ra1, rb0
            flddne rA3, [PTR]
            fmacd rC20, ra2, rb0
            add PTR, pB0, ldb, LSL #1
            add PTR, PTR, ldb
            fmacd rC30, ra3, rb0
            flddne rB3, [PTR]

            fmacd rC01, ra0, rb1
            flddne rb0, [pB0,#8]
            fmacd rC11, ra1, rb1
            add PTR, pB0, ldb
            fmacd rC21, ra2, rb1
            fmacd rC31, ra3, rb1
            flddne rb1, [PTR,#8]

            fmacd rC02, ra0, rb2
            add PTR, PTR, ldb
            fmacd rC12, ra1, rb2
            fmacd rC22, ra2, rb2
            fmacd rC32, ra3, rb2
            flddne rb2, [PTR,#8]

            add PTR, pA0, lda
            fmacd rC03, ra0, rb3
            flddne ra0, [pA0, #8]
            fmacd rC13, ra1, rb3
            flddne ra1, [PTR, #8]
            fmacd rC23, ra2, rb3
            add PTR, PTR, lda
            fmacd rC33, ra3, rb3
            flddne ra2, [PTR, #8]
	 bne KLOOP
         add PTR, pC0, ldc
         #ifdef DCPLX
            fstd rC00, [pC0]
            fstd rC10, [pC0, #16]
            subs M, M, #4  	        /* M -= 4; set cond codes */
            fstd rC20, [pC0, #32]
            fstd rC30, [pC0, #48]
            fstd rC01, [PTR]
            fstd rC11, [PTR, #16]
            fstd rC21, [PTR, #32]
            fstd rC31, [PTR, #48]
            add PTR, PTR, ldc
            fstd rC02, [PTR]
            fstd rC12, [PTR, #16]
            fstd rC22, [PTR, #32]
            fstd rC32, [PTR, #48]
            add PTR, PTR, ldc
            fstd rC03, [PTR]
            fstd rC13, [PTR, #16]
            add pC0, pC0, #64
            fstd rC23, [PTR, #32]
            fstd rC33, [PTR, #48]
         #else
            fstmIAd pC0!, {rC00,rC10,rC20,rC30}
            subs M, M, #4  	        /* M -= 4; set cond codes */
            fstmIAd PTR, {rC01,rC11,rC21,rC31}
            add PTR, PTR, ldc
            fstmIAd PTR, {rC02,rC12,rC22,rC32}
            add PTR, PTR, ldc
            fstmIAd PTR, {rC03,rC13,rC23,rC33}
         #endif
         sub pB0, pB0, K0, LSL #3      /* rewind pB0 ptr for reuse */
         add pA0, pA0, lda, LSL #2     /* pA0 += 4*lda */
         sub pA0, pA0, K0, LSL #3      /* rewind K-loop increment */
	 mov K, K0
      bne MLOOP
      subs N, N, #4                     /* N -= 4; set cond codes */
      add pB0, pB0, ldb, LSL #2         /* pB0 += ldb*4 */
      mov pA0, pA00
      add pC0, pC0, ldc, LSL #2         /* pC0 += 4*ldc */
      #ifdef DCPLX
         sub pC0, pC0, M0, LSL #4          /* pC0 -= M already inc in Mloop */
      #else
         sub pC0, pC0, M0, LSL #3          /* pC0 -= M already inc in Mloop */
      #endif
      mov M, M0
   bne NLOOP

   #ifdef ATL_ARM_HARDFP
      pop {r0,r1}  /* clear beta off stack */
   #endif
   ldmIA SP!, {r4-r11,r14}
   fldmIAd SP!, {d8-d15}
   bx      lr
.size ATL_asmdecor(ATL_USERMM),.-ATL_asmdecor(ATL_USERMM)

