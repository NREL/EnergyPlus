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
int Mjoin(PATL,NCmmIJK)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda0, const TYPE *B, const int ldb0,
    const SCALAR beta, TYPE *C, const int ldc0)
/*
 * IJK loop-ordered matmul with no matrix copy
 */
{
   const size_t lda=lda0, ldb=ldb0, ldc=ldc0;
   size_t incAk, incAm, incAn, incBk, incBm, incBn;
   const int Mb = M / MB, Nb = N / NB, Kb = K / KB;
   const int mr = M - Mb*MB, nr = N - Nb*NB, kr = K - Kb*KB;
   const size_t incCn = ldc*(NB<<1), incCm = (MB<<1) - Nb * incCn;
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
   MMPTR r0mm_bX, i0mm_bX, r0mm_b1, i0mm_b1, r1mm, i1mm;
   MMPTR mmcu, mm_fixedKcu;

/*
 * See if we need to call Separate routine in order to handle alpha & beta
 */
   i = 0;
   if (!AlphaIsReal || !BetaIsReal) i = 1;
   else
   {
      if (Kb > ATL_MaxMMalpha) i = 1;
      else if (!AlphaIsOne || TA == AtlasConjTrans || TB == AtlasConjTrans)
      {
         btmp = Mabs(*beta);
         if (btmp < ATL_rone) btmp = ATL_rone;
         if (Mabs(*alpha) < btmp || ATL_rone < btmp) i = 1;
      }
   }
   if (i)
      return(Mjoin(PATL,NCmmIJK_c)(TA, TB, M, N, K, alpha, A, lda, B, ldb,
                                   beta, C, ldc));

   if (TA == AtlasNoTrans)
   {
      if (TB == AtlasNoTrans)
      {
         ar0 = ai0 = ai1 = ralpha;
         if (AlphaIsOne)
         {
            r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_bX);
            i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_b1);
            if (BetaIsOne)
            {
               r1mm = r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_bX);
               i1mm = i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_b1);
            }
            else if (BetaIsZero)
            {
               i0mm_bX = r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_b0);
               i1mm = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_b1);
               r1mm = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_bX);
            }
            else
            {
               i0mm_bX=r1mm=r0mm_bX=Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_bX);
               i1mm = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_b1);
            }
         }
         else
         {
            i0mm_b1 = r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_aX_bX);
            if (BetaIsOne)
            {
               r1mm = r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_aX_bX);
               i1mm = i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_aX_bX);
            }
            else if (BetaIsZero)
            {
               i0mm_bX = r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_aX_b0);
               i1mm = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_aX_bX);
               r1mm = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_aX_bX);
            }
            else
            {
               i0mm_bX=r1mm=r0mm_bX=Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_aX_bX);
               i1mm = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_aX_bX);
            }
         }
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),NN),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),NN),0x0x0_aX_bX);
      }
      else if (TB == AtlasConjTrans)
      {
         ai1 = ar0 = -ralpha; ai0 = ralpha;
         if (AlphaIsOne)
         {
            r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
            i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_b1);
            if (BetaIsOne)
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_b1);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
            }
            else if (BetaIsZero)
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_b0);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_b0);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
            }
            else
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_bX);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
            }
         }
         else
         {
            r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
            i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
            if (BetaIsOne)
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
            }
            else if (BetaIsZero)
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_b0);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_b0);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
            }
            else
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
            }
         }
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),NT),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),NT),0x0x0_aX_bX);
      }
      else
      {
         ar0 = ai0 = ai1 = ralpha;
         if (AlphaIsOne)
         {
            r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_bX);
            i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_b1);
            if (BetaIsOne)
            {
               r1mm = r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_bX);
               i1mm = i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_b1);
            }
            else if (BetaIsZero)
            {
               i0mm_bX = r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_b0);
               i1mm = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_b1);
               r1mm = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_bX);
            }
            else
            {
               i0mm_bX=r1mm=r0mm_bX=Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_bX);
               i1mm = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_b1);
            }
         }
         else
         {
            r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
            i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
            if (BetaIsOne)
            {
               r1mm = r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
               i1mm = i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
            }
            else if (BetaIsZero)
            {
               i0mm_bX = r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_b0);
               i1mm = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
               r1mm = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
            }
            else
            {
               i0mm_bX=r1mm=r0mm_bX=Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
               i1mm = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
            }
         }
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),NT),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),NT),0x0x0_aX_bX);
      }
   }
   else if (TA == AtlasConjTrans)
   {
      if (TB == AtlasNoTrans)
      {
         ai0 = ar0 = -ralpha; ai1 = ralpha;
         if (AlphaIsOne)
         {
            r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
            i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
            if (BetaIsOne)
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_b1);
            }
            else if (BetaIsZero)
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_b0);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_b0);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_b1);
            }
            else
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_b1);
            }
         }
         else
         {
            r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
            i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
            if (BetaIsOne)
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
            }
            else if (BetaIsZero)
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_b0);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_b0);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
            }
            else
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
            }
         }
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),TN),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),TN),0x0x0_aX_bX);
      }
      else if (TB == AtlasConjTrans)
      {
         ar0 = ralpha; ai1 = ai0 = -ralpha;
         if (AlphaIsOne)
         {
            r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
            i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            if (BetaIsOne)
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            }
            else if (BetaIsZero)
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b0);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_b0);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            }
            else
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            }
         }
         else
         {
            r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            if (BetaIsOne)
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            }
            else if (BetaIsZero)
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_b0);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_b0);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            }
            else
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            }
         }
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),TT),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),TT),0x0x0_aX_bX);
      }
      else  /* TA == AtlasConjTrans, TB == AtlasTrans */
      {
         ai0 = ar0 = -ralpha; ai1 = ralpha;
         if (AlphaIsOne)
         {
            r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            if (BetaIsOne)
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b1);
            }
            else if (BetaIsZero)
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_b0);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_b0);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b1);
            }
            else
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b1);
            }
         }
         else
         {
            r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            if (BetaIsOne)
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            }
            else if (BetaIsZero)
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_b0);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_b0);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            }
            else
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            }
         }
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),TT),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),TT),0x0x0_aX_bX);
      }
   }
   else
   {
      if (TB == AtlasNoTrans)
      {
         ar0 = ai0 = ai1 = ralpha;
         if (AlphaIsOne)
         {
            r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_bX);
            i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_b1);
            if (BetaIsOne)
            {
               r1mm = r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_bX);
               i1mm = i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_b1);
            }
            else if (BetaIsZero)
            {
               i0mm_bX = r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_b0);
               i1mm = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_b1);
               r1mm = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_bX);
            }
            else
            {
               i0mm_bX=r1mm=r0mm_bX=Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_bX);
               i1mm = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_b1);
            }
         }
         else
         {
            r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
            i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
            if (BetaIsOne)
            {
               r1mm = r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
               i1mm = i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
            }
            else if (BetaIsZero)
            {
               i0mm_bX = r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_b0);
               i1mm = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
               r1mm = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
            }
            else
            {
               i0mm_bX=r1mm=r0mm_bX=Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
               i1mm = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
            }
         }
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),TN),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),TN),0x0x0_aX_bX);
      }
      else if (TB == AtlasConjTrans)
      {
         ai1 = ar0 = -ralpha; ai0 = ralpha;
         if (AlphaIsOne)
         {
            r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b1);
            if (BetaIsOne)
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b1);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            }
            else if (BetaIsZero)
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_b0);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b0);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            }
            else
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            }
         }
         else
         {
            r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            if (BetaIsOne)
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            }
            else if (BetaIsZero)
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_b0);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_b0);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            }
            else
            {
               r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               r1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               i1mm    = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            }
         }
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),TT),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),TT),0x0x0_aX_bX);
      }
      else
      {
         ar0 = ai0 = ai1 = ralpha;
         if (AlphaIsOne)
         {
            r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
            i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b1);
            if (BetaIsOne)
            {
               r1mm = r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
               i1mm = i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b1);
            }
            else if (BetaIsZero)
            {
               i0mm_bX = r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b0);
               i1mm = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b1);
               r1mm = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
            }
            else
            {
               i0mm_bX=r1mm=r0mm_bX=Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
               i1mm = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b1);
            }
         }
         else
         {
            r0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            i0mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            if (BetaIsOne)
            {
               r1mm = r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               i1mm = i0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            }
            else if (BetaIsZero)
            {
               i0mm_bX = r0mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_b0);
               i1mm = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               r1mm = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            }
            else
            {
               i0mm_bX=r1mm=r0mm_bX=Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
               i1mm = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            }
         }
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),TT),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),TT),0x0x0_aX_bX);
      }
   }

   if (TA == AtlasNoTrans)
   {
      incAk = lda * (KB<<1);
      incAn = -Kb * incAk;
      incAm = MB<<1;
   }
   else
   {
      incAk = KB<<1;
      incAn = -Kb * incAk;
      incAm = (MB<<1) * lda;
   }
   if (TB == AtlasNoTrans)
   {
      incBk = KB<<1;
      incBn = (ldb*NB - K + kr)<<1;
      incBm = -((Nb * ldb * NB)<<1);
   }
   else
   {
      incBk = (KB<<1)*ldb;
      incBn = (NB<<1) - Kb*incBk;
      incBm = -Nb*(NB<<1);
   }

   for (i=Mb; i; i--, a += incAm, b += incBm, c += incCm)
   {
      for (j=Nb; j; j--, a += incAn, b += incBn, c += incCn)
      {
         if (Kb)
         {
            r0mm_bX(MB, NB, KB, ar0, a+1, lda, b+1, ldb, nrbeta, c, ldc);
            i0mm_bX(MB, NB, KB, ai0, a+1, lda, b, ldb, rbeta, c+1, ldc);
            r1mm(MB, NB, KB, ralpha, a, lda, b, ldb, ATL_rnone, c, ldc);
            i1mm(MB, NB, KB, ai1, a, lda, b+1, ldb, ATL_rone, c+1, ldc);
            a += incAk;  b += incBk;
            for (k=Kb-1; k; k--, a += incAk, b += incBk)
            {
               r0mm_b1(MB, NB, KB, ar0, a+1, lda, b+1, ldb, ATL_rnone, c, ldc);
               i0mm_b1(MB, NB, KB, ai0, a+1, lda, b, ldb, ATL_rone, c+1, ldc);
               r1mm(MB, NB, KB, ralpha, a, lda, b, ldb, ATL_rnone, c, ldc);
               i1mm(MB, NB, KB, ai1, a, lda, b+1, ldb, ATL_rone, c+1, ldc);
            }
            if (kr)
            {
               mmcu(MB, NB, kr, ar0, a+1, lda, b+1, ldb, ATL_rnone, c, ldc);
               mmcu(MB, NB, kr, ai0, a+1, lda, b, ldb, ATL_rone, c+1, ldc);
               mmcu(MB, NB, kr, ralpha, a, lda, b, ldb, ATL_rnone, c, ldc);
               mmcu(MB, NB, kr, ai1, a, lda, b+1, ldb, ATL_rone, c+1, ldc);
            }
         }
         else if (kr)
         {
            if (BetaIsZero) Mjoin(PATL,gezero)(MB, NB, c, ldc);
            mmcu(MB, NB, kr, ar0, a+1, lda, b+1, ldb, nrbeta, c, ldc);
            mmcu(MB, NB, kr, ai0, a+1, lda, b, ldb, rbeta, c+1, ldc);
            mmcu(MB, NB, kr, ralpha, a, lda, b, ldb, ATL_rnone, c, ldc);
            mmcu(MB, NB, kr, ai1, a, lda, b+1, ldb, ATL_rone, c+1, ldc);
         }
      }
   }
   if (mr)  /* M-loop remainder */
   {
      for (j=Nb; j; j--, a += incAn, b += incBn, c += incCn)
      {
         if (BetaIsZero) Mjoin(PATL,gezero)(mr, NB, c, ldc);
         if (Kb)
         {
            mm_fixedKcu(mr, NB, KB, ar0, a+1, lda, b+1, ldb, nrbeta,
                        c, ldc);
            mm_fixedKcu(mr, NB, KB, ai0, a+1, lda, b, ldb, rbeta,
                        c+1, ldc);
            mm_fixedKcu(mr, NB, KB, ralpha, a, lda, b, ldb, ATL_rnone,
                        c, ldc);
            mm_fixedKcu(mr, NB, KB, ai1, a, lda, b+1, ldb, ATL_rone,
                        c+1, ldc);
            a += incAk;  b += incBk;
            for (k=Kb-1; k; k--, a += incAk, b += incBk)
            {
               mm_fixedKcu(mr, NB, KB, ar0, a+1, lda, b+1, ldb, ATL_rnone,
                           c, ldc);
               mm_fixedKcu(mr, NB, KB, ai0, a+1, lda, b, ldb, ATL_rone,
                           c+1, ldc);
               mm_fixedKcu(mr, NB, KB, ralpha, a, lda, b, ldb, ATL_rnone,
                           c, ldc);
               mm_fixedKcu(mr, NB, KB, ai1, a, lda, b+1, ldb, ATL_rone,
                           c+1, ldc);
            }
            if (kr)
            {
               mmcu(mr, NB, kr, ar0, a+1, lda, b+1, ldb, ATL_rnone, c, ldc);
               mmcu(mr, NB, kr, ai0, a+1, lda, b, ldb, ATL_rone, c+1, ldc);
               mmcu(mr, NB, kr, ralpha, a, lda, b, ldb, ATL_rnone, c, ldc);
               mmcu(mr, NB, kr, ai1, a, lda, b+1, ldb, ATL_rone, c+1, ldc);
            }
         }
         else if (kr)
         {
            mmcu(mr, NB, kr, ar0, a+1, lda, b+1, ldb, nrbeta, c, ldc);
            mmcu(mr, NB, kr, ai0, a+1, lda, b, ldb, rbeta, c+1, ldc);
            mmcu(mr, NB, kr, ralpha, a, lda, b, ldb, ATL_rnone, c, ldc);
            mmcu(mr, NB, kr, ai1, a, lda, b+1, ldb, ATL_rone, c+1, ldc);
         }
      }
   }
   if (nr) Mjoin(PATL,NCmmJIK)(TA, TB, M, nr, K, alpha, A, lda,
                               B+Nb*(incBn+Kb*incBk), ldb,
                               beta, C+Nb*(NB<<1)*ldc, ldc);
   return(0);
}
