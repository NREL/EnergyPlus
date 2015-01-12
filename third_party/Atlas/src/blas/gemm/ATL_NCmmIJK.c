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
#include Mstr(Mjoin(Mjoin(atlas_,PRE),NCmm.h))
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
void Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_b1)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_b0)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_aX_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_aX_b0)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),NN),0x0x0_aX_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),NN),0x0x0_aX_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);

void Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_b1)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_b0)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_b0)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),NT),0x0x0_aX_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),NT),0x0x0_aX_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);

void Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_b1)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_b0)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_b0)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),TN),0x0x0_aX_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),TN),0x0x0_aX_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);

void Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b1)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b0)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_b0)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),TT),0x0x0_aX_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);
void Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),TT),0x0x0_aX_bX)
   (const int M, const int N, const int K, const SCALAR alpha, const TYPE *A,
    const int lda, const TYPE *B, const int ldb,
    const SCALAR beta, TYPE *C, const int ldc);

#ifndef ATL_MaxMMalpha
   #define ATL_MaxMMalpha 3
#endif
#ifndef MB
   #define MB NB
#endif
#ifndef KB
   #define KB NB
#endif

int Mjoin(PATL,NCmmIJK)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int lda0, const TYPE *B, const int ldb0,
    const SCALAR beta, TYPE *C, const int ldc0)
/*
 * IJK loop-ordered matmul with no matrix copy
 */
{
   size_t incAk, incAm, incAn, incBk, incBm, incBn;
   const size_t lda=lda0, ldb=ldb0, ldc=ldc0;
   const int Mb = M / MB, Nb = N / NB, Kb = K / KB;
   const int mr = M - Mb*MB, nr = N - Nb*NB, kr = K - Kb*KB;
   const size_t incCn = ldc*NB, incCm = MB - Nb * incCn;
   const int BetaIsZero = (beta == ATL_rzero);
   int i, j, k;
   const TYPE *a=A, *b=B;
   TYPE *c=C;
   TYPE btmp;
   void (*mm_bX)(const int M, const int N, const int K, const SCALAR alpha,
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                 const SCALAR beta, TYPE *C, const int ldc);
   void (*mm_b1)(const int M, const int N, const int K, const SCALAR alpha,
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                 const SCALAR beta, TYPE *C, const int ldc);
   void (*mmcu) (const int M, const int N, const int K, const SCALAR alpha,
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                 const SCALAR beta, TYPE *C, const int ldc);
   void (*mm_fixedKcu)(const int M, const int N, const int K,
                       const SCALAR alpha, const TYPE *A, const int lda,
                       const TYPE *B, const int ldb, const
                       SCALAR beta, TYPE *C, const int ldc);

   if (TA == AtlasNoTrans)
   {
      if (TB == AtlasNoTrans)
      {
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),NN),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),NN),0x0x0_aX_bX);
      }
      else
      {
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),NT),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),NT),0x0x0_aX_bX);
      }
      incAk = lda * KB;
      incAn = -Kb * incAk;
      incAm = MB;
   }
   else
   {
      if (TB == AtlasNoTrans)
      {
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),TN),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),TN),0x0x0_aX_bX);
      }
      else
      {
         mm_fixedKcu=Mjoin(Mjoin(Mjoin(NCmm00,Mjoin(0x0x,KB)),TT),0x0x0_aX_bX);
         mmcu = Mjoin(Mjoin(Mjoin(NCmm00,0x0x0),TT),0x0x0_aX_bX);
      }
      incAk = KB;
      incAn = -Kb * incAk;
      incAm = MB * lda;
   }
   if (TB == AtlasNoTrans)
   {
      incBk = KB;
      incBn = ldb*NB - K + kr;
      incBm = -Nb * ldb * NB;
   }
   else
   {
      incBk = KB*ldb;
      incBn = NB - Kb*incBk;
      incBm = -Nb*NB;
   }

   if (alpha == ATL_rone)
   {
      if (TA == AtlasNoTrans)
      {
         if (TB == AtlasNoTrans)
         {
            mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_b1);
            if (beta == ATL_rone) mm_bX = mm_b1;
            else if (beta == ATL_rzero)
               mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_b0);
            else mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_a1_bX);
         }
         else
         {
            mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_b1);
            if (beta == ATL_rone) mm_bX = mm_b1;
            else if (beta == ATL_rzero)
               mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_b0);
            else mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_a1_bX);
         }
      }
      else
      {
         if (TB == AtlasNoTrans)
         {
            mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_b1);
            if (beta == ATL_rone) mm_bX = mm_b1;
            else if (beta == ATL_rzero)
               mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_b0);
            else mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_a1_bX);
         }
         else
         {
            mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b1);
            if (beta == ATL_rone) mm_bX = mm_b1;
            else if (beta == ATL_rzero)
               mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_b0);
            else mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_a1_bX);
         }
      }
   }
   else  /* non-one alpha */
   {
      btmp = Mabs(beta);
      if (btmp < ATL_rone) btmp = 1.0;
/*
 *    If needed, call version that uses temp C to handle alpha & beta safely
 */
      if (Kb >= ATL_MaxMMalpha || Mabs(alpha) < btmp)
         return(Mjoin(PATL,NCmmIJK_c)(TA, TB, M, N, K, alpha, A, lda, B, ldb,
                                      beta, C, ldc));
      if (TA == AtlasNoTrans)
      {
         if (TB == AtlasNoTrans)
         {
            mm_bX = mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_aX_bX);
            if (beta == ATL_rzero)
               mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NN),0x0x0),_aX_b0);
         }
         else
         {
            mm_bX = mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_bX);
            if (beta == ATL_rzero)
               mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,NT),0x0x0),_aX_b0);
         }
      }
      else
      {
         if (TB == AtlasNoTrans)
         {
            mm_bX = mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_bX);
            if (beta == ATL_rzero)
               mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TN),0x0x0),_aX_b0);
         }
         else
         {
            mm_bX = mm_b1 = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_bX);
            if (beta == ATL_rzero)
               mm_bX = Mjoin(Mjoin(Mjoin(NCmm0,TT),0x0x0),_aX_b0);
         }
      }
   }

   for (i=Mb; i; i--, a += incAm, b += incBm, c += incCm)
   {
      for (j=Nb; j; j--, a += incAn, b += incBn, c += incCn)
      {
         if (Kb)
         {
            mm_bX(MB, NB, KB, alpha, a, lda, b, ldb, beta, c, ldc);
            a += incAk;  b += incBk;
            for (k=Kb-1; k; k--, a += incAk, b += incBk)
               mm_b1(MB, NB, KB, alpha, a, lda, b, ldb, ATL_rone, c, ldc);
            if (kr)
               mmcu(MB, NB, kr, alpha, a, lda, b, ldb, ATL_rone, c, ldc);
         }
         else if (kr)
         {
            if (BetaIsZero) Mjoin(PATL,gezero)(MB, NB, c, ldc);
            mmcu(MB, NB, kr, alpha, a, lda, b, ldb, beta, c, ldc);
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
            mm_fixedKcu(mr, NB, KB, alpha, a, lda, b, ldb, beta,
                        c, ldc);
            a += incAk;  b += incBk;
            for (k=Kb-1; k; k--, a += incAk, b += incBk)
               mm_fixedKcu(mr, NB, KB, alpha, a, lda, b, ldb, ATL_rone,
                           c, ldc);
            if (kr)
               mmcu(mr, NB, kr, alpha, a, lda, b, ldb, ATL_rone, c, ldc);
         }
         else if (kr)
            mmcu(mr, NB, kr, alpha, a, lda, b, ldb, beta, c, ldc);
      }
   }
   if (nr)
      ATL_assert(Mjoin(PATL,NCmmJIK)(TA, TB, M, nr, K, alpha, A, lda,
                                     B+Nb*(incBn+Kb*incBk), ldb,
                                     beta, C+Nb*NB*ldc, ldc) == 0);
   return(0);
}
