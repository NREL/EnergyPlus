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
 * of software pipelining on the load/use of A and B for the ARM.
 * This routine is a straight translation of ATL_smm4x4x2pf_arm.c, and on
 * my 1Ghz, it only gets around 1.28Gflops, instead of 2 even in-cache.
 * OOC, it is just a little under 1.1Gflop, so not sure what is wrong.
 * Prefetch (PLD) seems to reduce performance.
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

#define rC00	s4
#define rC10	s5
#define rC20	s6
#define rC30	s7
#define rC01	s8
#define rC11	s9
#define rC21	s10
#define rC31	s11
#define rC02	s12
#define rC12	s13
#define rC22	s14
#define rC32	s15
#define rC03	s20
#define rC13	s21
#define rC23	s22
#define rC33	s23

#define rA0     s24
#define ra0 	s25
#define rA1     s26
#define ra1 	s27
#define rA2     s28
#define ra2 	s29
#define rA3     s30
#define ra3 	s31
#define rB0     s0
#define rb0     s1
#define rB1     s2
#define rb1     s3
#define rB2     s16
#define rb2     s17
#define rB3     s18
#define rb3     s19

/*                       r0           r1           r2                r3
void ATL_USERMM (const int M, const int N, const int K, const TYPE alpha,
                             0              4              8             12
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                             16       20             24
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
   fstmDBs SP!, {s16-s31}
   stmDB SP!, {r4-r11,r14}
/*
 * Load needed vals to registers
 */
#ifdef ATL_ARM_HARDFP
   add M0, SP, #FSIZE
   ldmIA M0, {lda,pB0,pA00,pC0,ldc} /* lda,B,ldb,C,ldc */
   vmov.32 M0, d0[1] /* put beta in M0 */
   push {M0}         /* put beta on stack */
#else
   add M0, SP, #FSIZE
   ldmIA M0, {pA0,lda,pB0,pA00,pfA,pC0,ldc}
#endif
   mov lda, lda, LSL #2   /* lda *= sizeof */
   #ifdef SCPLX
      mov ldc, ldc, LSL #3   /* ldc *= sizeof */
   #else
      mov ldc, ldc, LSL #2   /* ldc *= sizeof */
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
            fmsr rC00, zero
            fcpys rC10, rC00
            fcpys rC20, rC00
            fcpys rC30, rC00
            fcpys rC01, rC00
            fcpys rC11, rC00
            fcpys rC21, rC00
            fcpys rC31, rC00
            fcpys rC02, rC00
            fcpys rC12, rC00
            fcpys rC22, rC00
            fcpys rC32, rC00
            fcpys rC03, rC00
            fcpys rC13, rC00
            fcpys rC23, rC00
            fcpys rC33, rC00
         #else
            add PTR, pC0, ldc
            #ifdef BETAX
               #ifdef ATL_ARM_HARDFP
                  flds rb0, [SP, #0]
               #else
                  flds rb0, [SP, #(FSIZE+16)]
               #endif
            #endif
            #ifdef SCPLX
               flds rC00, [pC0]
               flds rC10, [pC0, #8]
               flds rC20, [pC0, #16]
               flds rC30, [pC0, #24]
               flds rC01, [PTR]
               flds rC11, [PTR, #8]
               flds rC21, [PTR, #16]
               flds rC31, [PTR, #24]
               add PTR, PTR, ldc
               flds rC02, [PTR]
               flds rC12, [PTR, #8]
               flds rC22, [PTR, #16]
               flds rC32, [PTR, #24]
               add PTR, PTR, ldc
               flds rC03, [PTR]
               flds rC13, [PTR, #8]
               flds rC23, [PTR, #16]
               flds rC33, [PTR, #24]
            #else
               fldmIAs pC0, {rC00,rC10,rC20,rC30}
               fldmIAs PTR, {rC01,rC11,rC21,rC31}
               add PTR, PTR, ldc
               fldmIAs PTR, {rC02,rC12,rC22,rC32}
               add PTR, PTR, ldc
               fldmIAs PTR, {rC03,rC13,rC23,rC33}
            #endif
            #ifdef BETAX
               fmuls rC00, rC00, rb0
               fmuls rC10, rC10, rb0
               fmuls rC20, rC20, rb0
               fmuls rC30, rC30, rb0
               fmuls rC01, rC01, rb0
               fmuls rC11, rC11, rb0
               fmuls rC21, rC21, rb0
               fmuls rC31, rC31, rb0
               fmuls rC02, rC02, rb0
               fmuls rC12, rC12, rb0
               fmuls rC22, rC22, rb0
               fmuls rC32, rC32, rb0
               fmuls rC03, rC03, rb0
               fmuls rC13, rC13, rb0
               fmuls rC23, rC23, rb0
               fmuls rC33, rC33, rb0
            #endif
         #endif
         fldmias pB0, {rB0, rb0}
         add PTR, pA0, lda
         fldmias pA0, {rA0, ra0}
         fldmias PTR, {rA1, ra1}
         add PTR, PTR, lda
         fldmias PTR, {rA2, ra2}
         add PTR, PTR, lda
         flds rA3, [PTR]
         add PTR, pB0, ldb
         fldmias PTR, {rB1, rb1}
         add PTR, PTR, ldb
         fldmias PTR, {rB2, rb2}
         add PTR, PTR, ldb
         flds rB3, [PTR]
         add PTR, pA0, lda, LSL #1
	 KLOOP:
            add PTR, PTR, lda
            fmacs rC00, rA0, rB0
            flds ra3, [PTR, #4]
            add PTR, pB0, ldb, LSL #1
            fmacs rC10, rA1, rB0
            add PTR, PTR, ldb
            flds rb3, [PTR, #4]
            fmacs rC20, rA2, rB0
            add pB0, #8
            fmacs rC30, rA3, rB0
            subs K, K, #2  	        /* K -= 2, set cond codes */

            fmacs rC01, rA0, rB1
            add pA0, #8
            fmacs rC11, rA1, rB1
            fldsne rB0, [pB0]
            fmacs rC21, rA2, rB1
            fmacs rC31, rA3, rB1
            add PTR, pB0, ldb

            fmacs rC02, rA0, rB2
            fldsne rB1, [PTR]
            fmacs rC12, rA1, rB2
            add PTR, PTR, ldb
            fmacs rC22, rA2, rB2
            fmacs rC32, rA3, rB2
            fldsne rB2, [PTR]

            add PTR, pA0, lda
            fmacs rC03, rA0, rB3
            fldsne rA0, [pA0]
            fmacs rC13, rA1, rB3
            fldsne rA1, [PTR]
            fmacs rC23, rA2, rB3
            add PTR, PTR, lda
            fmacs rC33, rA3, rB3
            fldsne rA2, [PTR]


            fmacs rC00, ra0, rb0
            add PTR, PTR, lda
            fmacs rC10, ra1, rb0
            fldsne rA3, [PTR]
            fmacs rC20, ra2, rb0
            add PTR, pB0, ldb, LSL #1
            add PTR, PTR, ldb
            fmacs rC30, ra3, rb0
            fldsne rB3, [PTR]

            fmacs rC01, ra0, rb1
            fldsne rb0, [pB0,#4]
            fmacs rC11, ra1, rb1
            add PTR, pB0, ldb
            fmacs rC21, ra2, rb1
            fmacs rC31, ra3, rb1
            fldsne rb1, [PTR,#4]

            fmacs rC02, ra0, rb2
            add PTR, PTR, ldb
            fmacs rC12, ra1, rb2
            fmacs rC22, ra2, rb2
            fmacs rC32, ra3, rb2
            fldsne rb2, [PTR,#4]

            add PTR, pA0, lda
            fmacs rC03, ra0, rb3
            fldsne ra0, [pA0, #4]
            fmacs rC13, ra1, rb3
            fldsne ra1, [PTR, #4]
            fmacs rC23, ra2, rb3
            add PTR, PTR, lda
            fmacs rC33, ra3, rb3
            fldsne ra2, [PTR, #4]
	 bne KLOOP
         add PTR, pC0, ldc
         #ifdef SCPLX
            fsts rC00, [pC0]
            fsts rC10, [pC0, #8]
            subs M, M, #4  	        /* M -= 4; set cond codes */
            fsts rC20, [pC0, #16]
            fsts rC30, [pC0, #24]
            fsts rC01, [PTR]
            fsts rC11, [PTR, #8]
            fsts rC21, [PTR, #16]
            fsts rC31, [PTR, #24]
            add PTR, PTR, ldc
            fsts rC02, [PTR]
            fsts rC12, [PTR, #8]
            fsts rC22, [PTR, #16]
            fsts rC32, [PTR, #24]
            add PTR, PTR, ldc
            fsts rC03, [PTR]
            fsts rC13, [PTR, #8]
            add pC0, pC0, #32
            fsts rC23, [PTR, #16]
            fsts rC33, [PTR, #24]
         #else
            fstmIAs pC0!, {rC00,rC10,rC20,rC30}
            subs M, M, #4  	        /* M -= 4; set cond codes */
            fstmIAs PTR, {rC01,rC11,rC21,rC31}
            add PTR, PTR, ldc
            fstmIAs PTR, {rC02,rC12,rC22,rC32}
            add PTR, PTR, ldc
            fstmIAs PTR, {rC03,rC13,rC23,rC33}
         #endif
         sub pB0, pB0, K0, LSL #2      /* rewind pB0 ptr for reuse */
         add pA0, pA0, lda, LSL #2     /* pA0 += 4*lda */
         sub pA0, pA0, K0, LSL #2      /* rewind K-loop increment */
	 mov K, K0
      bne MLOOP
      subs N, N, #4                     /* N -= 4; set cond codes */
      add pB0, pB0, ldb, LSL #2         /* pB0 += ldb*4 */
      mov pA0, pA00
      add pC0, pC0, ldc, LSL #2         /* pC0 += 4*ldc */
      #ifdef SCPLX
         sub pC0, pC0, M0, LSL #3          /* pC0 -= M already inc in Mloop */
      #else
         sub pC0, pC0, M0, LSL #2          /* pC0 -= M already inc in Mloop */
      #endif
      mov M, M0
   bne NLOOP

   #ifdef ATL_ARM_HARDFP
      pop {r0} /* clear beta off stack */
   #endif
   ldmIA SP!, {r4-r11,r14}
   fldmIAs SP!, {s16-s31}
   bx      lr
.size ATL_asmdecor(ATL_USERMM),.-ATL_asmdecor(ATL_USERMM)

