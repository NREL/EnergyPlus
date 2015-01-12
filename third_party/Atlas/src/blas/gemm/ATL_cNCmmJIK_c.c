/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 1999 R. Clint Whaley
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

#include "atlas_misc.h"
#ifdef SCPLX
   #include "atlas_cNCmm.h"
#elif defined(DCPLX)
   #include "atlas_zNCmm.h"
#endif
#include "atlas_lvl3.h"

#ifndef MB
   #define MB NB
#endif
#ifndef KB
   #define KB NB
#endif


#define NBnam Mjoin(Mjoin(Mjoin(Mjoin(MB,x),NB),x),KB)
#define NCmm0 Mjoin(Mjoin(PATL,JIK),NBnam)
#define NCmm00 Mjoin(PATL,JIK)
void Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_aX_bX)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_bX)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_aX_b1)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_b1)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_aX_b0)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_b0)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),NN),0x0x0_aX_bX)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),NN),0x0x0_aX_bX)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);

void Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_bX)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_b1)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_b1)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_b0)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_b0)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),NT),0x0x0_aX_bX)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),NT),0x0x0_aX_bX)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);

void Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_bX)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_b1)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_b1)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_b0)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_b0)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),TN),0x0x0_aX_bX)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),TN),0x0x0_aX_bX)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);

void Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_b1)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b1)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_b0)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b0)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),TT),0x0x0_aX_bX)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),TT),0x0x0_aX_bX)
   (const int M, const int N, const int K, const TYPE alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);


#ifndef ATL_MaxMMalpha
   #define ATL_MaxMMalpha 3
#endif
#ifndef MB
   #define MB NB
#endif
#ifndef KB
   #define KB NB
#endif

typedef void (*MMPTR)
   (const int M, const int N, const int K, const TYPE alpha,
    const TYPE *A, const int lda, const TYPE *B, const int ldb,
    const TYPE beta, TYPE *C, const int ldc);
int Mjoin(PATL,NCmmJIK_c)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda0, const TYPE *B, const int ldb0,
    const SCALAR beta, TYPE *C, const int ldc0)
/*
 * JIK loop-ordered matmul with no matrix copy
 */
{
   const size_t lda=lda0, ldb=ldb0, ldc=ldc0;
   size_t incAk, incAm, incAn, incBk, incBm, incBn;
   const int Mb = M / MB, Nb = N / NB, Kb = K / KB;
   const int mr = M - Mb*MB, nr = N - Nb*NB, kr = K - Kb*KB;
   const size_t incCm = (MB<<1), incCn = (ldc*NB - M + mr)<<1;
   int i, j, k;
   const TYPE *a=A, *b=B;
   const TYPE ralpha = *alpha, rbeta = *beta;
   const TYPE nrbeta = (rbeta == ATL_rzero) ? ATL_rzero : -rbeta;
   const int AlphaIsReal = (alpha[1] == ATL_rzero);
   const int BetaIsReal = (beta[1] == ATL_rzero);
   const int AlphaIsOne = (AlphaIsReal && ralpha == ATL_rone);
   const int BetaIsOne = (BetaIsReal && rbeta == ATL_rone);
   const int BetaIsZero = (BetaIsReal && rbeta == ATL_rzero);
   TYPE ar0, ai0, ai1;
   TYPE *c=C;
   TYPE btmp;
   TYPE czero[2] = {ATL_rzero, ATL_rzero};
   void *vp;
   TYPE *cp;
   void (*geadd)(const int M, const int N, const SCALAR scalar, const TYPE *A,
                  const int lda, const SCALAR beta, TYPE *C, const int ldc);
   MMPTR r0mm_bX, i0mm_bX, r0mm_b1, i0mm_b1, r1mm, i1mm;
   MMPTR mmcu, mm_fixedKcu;

   if (TA == AtlasNoTrans)
   {
      if (TB == AtlasNoTrans)
      {
         ar0 = ai0 = ai1 = ATL_rone;
         i0mm_bX = r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_b0);
         i1mm = i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_b1);
         r0mm_b1 = r1mm = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_bX);
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),NN),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),NN),0x0x0_aX_bX);
      }
      else if (TB == AtlasConjTrans)
      {
         ai1 = ar0 = ATL_rnone; ai0 = ATL_rone;
         r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_b0);
         r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
         r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_bX);
         i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_b0);
         i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_b1);
         i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),NT),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),NT),0x0x0_aX_bX);
      }
      else
      {
         ar0 = ai0 = ai1 = ATL_rone;
         i0mm_bX = r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_b0);
         i1mm = i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_b1);
         r0mm_b1 = r1mm = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_bX);
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),NT),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),NT),0x0x0_aX_bX);
      }
   }
   else if (TA == AtlasConjTrans)
   {
      if (TB == AtlasNoTrans)
      {
         ai0 = ar0 = ATL_rnone; ai1 = ATL_rone;
         r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_b0);
         r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
         r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_bX);
         i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_b0);
         i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
         i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_b1);
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),TN),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),TN),0x0x0_aX_bX);
      }
      else if (TB == AtlasConjTrans)
      {
         ar0 = ATL_rone; ai1 = ai0 = ATL_rnone;
         r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b0);
         r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
         r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
         i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_b0);
         i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
         i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),TT),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),TT),0x0x0_aX_bX);
      }
      else  /* TA == AtlasConjTrans, TB == AtlasTrans */
      {
         ai0 = ar0 = ATL_rnone; ai1 = ATL_rone;
         r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_b0);
         r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
         r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
         i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_b0);
         i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
         i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b1);
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),TT),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),TT),0x0x0_aX_bX);
      }
   }
   else
   {
      if (TB == AtlasNoTrans)
      {
         ar0 = ai0 = ai1 = ATL_rone;
         i0mm_bX = r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_b0);
         i1mm = i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_b1);
         r0mm_b1 = r1mm = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_bX);
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),TN),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),TN),0x0x0_aX_bX);
      }
      else if (TB == AtlasConjTrans)
      {
         ai1 = ar0 = ATL_rnone; ai0 = ATL_rone;
         r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_b0);
         r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
         r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
         i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b0);
         i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b1);
         i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),TT),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),TT),0x0x0_aX_bX);
      }
      else
      {
         ar0 = ai0 = ai1 = ATL_rone;
         i0mm_bX = r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b0);
         i1mm = i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b1);
         r0mm_b1 = r1mm = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),TT),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),TT),0x0x0_aX_bX);
      }
   }

   if (TA == AtlasNoTrans)
   {
      incAk = lda * (KB<<1);
      incAm = (MB<<1) - Kb * incAk;
      incAn = -Mb * (MB<<1);
   }
   else
   {
      incAk = KB<<1;
      incAm = (lda*MB - Kb*KB)<<1;
      incAn = -lda*(MB<<1)*Mb;
   }
   if (TB == AtlasNoTrans)
   {
      incBk = KB<<1;
      incBm = -(KB<<1)*Kb;
      incBn = ldb*(NB<<1);
   }
   else
   {
      incBk = (KB<<1)*ldb;
      incBm = -Kb * incBk;
      incBn = NB<<1;
   }

   if (AlphaIsOne)
   {
      if (BetaIsOne) geadd = Mjoin(Mjoin(Mjoin(PATL,geadd),_a1),_b1);
      else if (BetaIsZero) geadd = Mjoin(Mjoin(Mjoin(PATL,geadd),_a1),_b0);
      else if (BetaIsReal) geadd = Mjoin(Mjoin(Mjoin(PATL,geadd),_a1),_bXi0);
      else geadd = Mjoin(Mjoin(Mjoin(PATL,geadd),_a1),_bX);
   }
   else if (AlphaIsReal)
   {
      if (BetaIsOne) geadd = Mjoin(Mjoin(Mjoin(PATL,geadd),_aXi0),_b1);
      else if (BetaIsZero) geadd = Mjoin(Mjoin(Mjoin(PATL,geadd),_aXi0),_b0);
      else if (BetaIsReal) geadd=Mjoin(Mjoin(Mjoin(PATL,geadd),_aXi0),_bXi0);
      else geadd = Mjoin(Mjoin(Mjoin(PATL,geadd),_aXi0),_bX);
   }
   else if (BetaIsOne) geadd = Mjoin(Mjoin(Mjoin(PATL,geadd),_aX),_b1);
   else if (BetaIsZero) geadd = Mjoin(Mjoin(Mjoin(PATL,geadd),_aX),_b0);
   else if (BetaIsReal) geadd = Mjoin(Mjoin(Mjoin(PATL,geadd),_aX),_bXi0);
   else geadd = Mjoin(Mjoin(Mjoin(PATL,geadd),_aX),_bX);
   vp = malloc(ATL_Cachelen + ATL_MulBySize(MB * NB));
   ATL_assert(vp);
   cp = ATL_AlignPtr(vp);
   if (mr || nr || kr) Mjoin(PATL,gezero)(MB, NB, cp, MB);

   for (j=Nb; j; j--, a += incAn, b += incBn, c += incCn)
   {
      for (i=Mb; i; i--, a += incAm, b += incBm, c += incCm)
      {
         if (Kb)
         {
            r0mm_bX(MB, NB, KB, ar0, a+1, lda, b+1, ldb, ATL_rzero, cp, MB);
            i0mm_bX(MB, NB, KB, ai0, a+1, lda, b, ldb, ATL_rzero, cp+1, MB);
            r1mm(MB, NB, KB, ATL_rone, a, lda, b, ldb, ATL_rnone, cp, MB);
            i1mm(MB, NB, KB, ai1, a, lda, b+1, ldb, ATL_rone, cp+1, MB);
            a += incAk;  b += incBk;
            for (k=Kb-1; k; k--, a += incAk, b += incBk)
            {
               r0mm_b1(MB, NB, KB, ar0, a+1, lda, b+1, ldb, ATL_rnone, cp, MB);
               i0mm_b1(MB, NB, KB, ai0, a+1, lda, b, ldb, ATL_rone, cp+1, MB);
               r1mm(MB, NB, KB, ATL_rone, a, lda, b, ldb, ATL_rnone, cp, MB);
               i1mm(MB, NB, KB, ai1, a, lda, b+1, ldb, ATL_rone, cp+1, MB);
            }
            if (kr)
            {
               mmcu(MB, NB, kr, ar0, a+1, lda, b+1, ldb, ATL_rnone, cp, MB);
               mmcu(MB, NB, kr, ai0, a+1, lda, b, ldb, ATL_rone, cp+1, MB);
               mmcu(MB, NB, kr, ATL_rone, a, lda, b, ldb, ATL_rnone, cp, MB);
               mmcu(MB, NB, kr, ai1, a, lda, b+1, ldb, ATL_rone, cp+1, MB);
            }
         }
         else if (kr)
         {
            Mjoin(PATL,zero)(MB*NB, cp, 1); /* kill NaN/INF from before */
            mmcu(MB, NB, kr, ar0, a+1, lda, b+1, ldb, ATL_rzero, cp, MB);
            mmcu(MB, NB, kr, ai0, a+1, lda, b, ldb, ATL_rzero, cp+1, MB);
            mmcu(MB, NB, kr, ATL_rone, a, lda, b, ldb, ATL_rnone, cp, MB);
            mmcu(MB, NB, kr, ai1, a, lda, b+1, ldb, ATL_rone, cp+1, MB);
         }
         geadd(MB, NB, alpha, cp, MB, beta, c, ldc);
      }
   }
   if (mr && N != nr)
      Mjoin(PATL,NCmmIJK)(TA, TB, mr, N-nr, K, alpha, A+Mb*(incAm+Kb*incAk),
                          lda, B, ldb, beta, C+Mb*(MB<<1), ldc);
   if (nr)
   {
      for (i=Mb; i; i--, a += incAm, b += incBm, c += incCm)
      {
            Mjoin(PATL,zero)(MB*nr, cp, 1); /* kill NaN/INF from before */
         if (Kb)
         {
            mm_fixedKcu(MB, nr, KB, ar0, a+1, lda, b+1, ldb, ATL_rzero,
                        cp, MB);
            mm_fixedKcu(MB, nr, KB, ai0, a+1, lda, b, ldb, ATL_rzero,
                        cp+1, MB);
            mm_fixedKcu(MB, nr, KB, ATL_rone, a, lda, b, ldb, ATL_rnone,
                        cp, MB);
            mm_fixedKcu(MB, nr, KB, ai1, a, lda, b+1, ldb, ATL_rone,
                        cp+1, MB);
            a += incAk;  b += incBk;
            for (k=Kb-1; k; k--, a += incAk, b += incBk)
            {
               mm_fixedKcu(MB, nr, KB, ar0, a+1, lda, b+1, ldb, ATL_rnone,
                           cp, MB);
               mm_fixedKcu(MB, nr, KB, ai0, a+1, lda, b, ldb, ATL_rone,
                           cp+1, MB);
               mm_fixedKcu(MB, nr, KB, ATL_rone, a, lda, b, ldb, ATL_rnone,
                           cp, MB);
               mm_fixedKcu(MB, nr, KB, ai1, a, lda, b+1, ldb, ATL_rone,
                           cp+1, MB);
            }
            if (kr)
            {
               mmcu(MB, nr, kr, ar0, a+1, lda, b+1, ldb, ATL_rnone, cp, MB);
               mmcu(MB, nr, kr, ai0, a+1, lda, b, ldb, ATL_rone, cp+1, MB);
               mmcu(MB, nr, kr, ATL_rone, a, lda, b, ldb, ATL_rnone, cp, MB);
               mmcu(MB, nr, kr, ai1, a, lda, b+1, ldb, ATL_rone, cp+1, MB);
            }
         }
         else if (kr)
         {
            mmcu(MB, nr, kr, ar0, a+1, lda, b+1, ldb, ATL_rzero, cp, MB);
            mmcu(MB, nr, kr, ai0, a+1, lda, b, ldb, ATL_rzero, cp+1, MB);
            mmcu(MB, nr, kr, ATL_rone, a, lda, b, ldb, ATL_rnone, cp, MB);
            mmcu(MB, nr, kr, ai1, a, lda, b+1, ldb, ATL_rone, cp+1, MB);
         }
         geadd(MB, nr, alpha, cp, MB, beta, c, ldc);
      }
      if (mr)  /* cleanup small mr x nr block of C */
      {
         c = C + ((Mb*MB + ldc*Nb*NB)<<1);
         a = A + Mb*(incAm+Kb*incAk);
         b = B + Nb*( incBn+(Mb*(incBm+Kb*incBk)) );
         Mjoin(PATL,zero)(MB*nr, cp, 1); /* kill NaN/INF from before */
         if (Kb)
         {
            mm_fixedKcu(mr, nr, KB, ar0, a+1, lda, b+1, ldb, ATL_rzero,
                        cp, MB);
            mm_fixedKcu(mr, nr, KB, ai0, a+1, lda, b, ldb, ATL_rzero,
                        cp+1, MB);
            mm_fixedKcu(mr, nr, KB, ATL_rone, a, lda, b, ldb, ATL_rnone,
                        cp, MB);
            mm_fixedKcu(mr, nr, KB, ai1, a, lda, b+1, ldb, ATL_rone,
                        cp+1, MB);
            a += incAk;  b += incBk;
            for (k=Kb-1; k; k--, a += incAk, b += incBk)
            {
               mm_fixedKcu(mr, nr, KB, ar0, a+1, lda, b+1, ldb, ATL_rnone,
                           cp, MB);
               mm_fixedKcu(mr, nr, KB, ai0, a+1, lda, b, ldb, ATL_rone,
                           cp+1, MB);
               mm_fixedKcu(mr, nr, KB, ATL_rone, a, lda, b, ldb, ATL_rnone,
                           cp, MB);
               mm_fixedKcu(mr, nr, KB, ai1, a, lda, b+1, ldb, ATL_rone,
                           cp+1, MB);
            }
            if (kr)
            {
               mmcu(mr, nr, kr, ar0, a+1, lda, b+1, ldb, ATL_rnone, cp, MB);
               mmcu(mr, nr, kr, ai0, a+1, lda, b, ldb, ATL_rone, cp+1, MB);
               mmcu(mr, nr, kr, ATL_rone, a, lda, b, ldb, ATL_rnone, cp, MB);
               mmcu(mr, nr, kr, ai1, a, lda, b+1, ldb, ATL_rone, cp+1, MB);
            }
         }
         else if (kr)
         {
            mmcu(mr, nr, kr, ar0, a+1, lda, b+1, ldb, ATL_rzero, cp, MB);
            mmcu(mr, nr, kr, ai0, a+1, lda, b, ldb, ATL_rzero, cp+1, MB);
            mmcu(mr, nr, kr, ATL_rone, a, lda, b, ldb, ATL_rnone, cp, MB);
            mmcu(mr, nr, kr, ai1, a, lda, b+1, ldb, ATL_rone, cp+1, MB);
         }
         geadd(mr, nr, alpha, cp, MB, beta, c, ldc);
      }
   }
   free(vp);
   return(0);
}
