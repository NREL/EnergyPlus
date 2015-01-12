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
#include "atlas_lvl3.h"
#include <stdlib.h>

#define KBmm Mjoin(PATL,pKBmm)
#define IBNBmm Mjoin(PATL,IBNBmm)
#define MBJBmm Mjoin(PATL,MBJBmm)
#define IBJBmm Mjoin(PATL,IBJBmm)

void Mjoin(PATL,mmIJK2)
   (int K, int nMb, int nNb, int nKb, int ib, int jb, int kb,
    const SCALAR alpha, const TYPE *A, const int lda, TYPE *pA0, const int incA,
    MAT2BLK A2blk, TYPE *pB0, const SCALAR beta, TYPE *C, int ldc,
    MATSCAL gescal, NBMM0 NBmm0)
{
   const int incK = ATL_MulByNB(K)<<1;
   const int incCn = ATL_MulByNB(ldc)<<1, incCm = (MB<<1) - nNb*incCn;
   const int ZEROC = ((gescal == NULL) && SCALAR_IS_ZERO(beta));
   int i, j, k;
   const TYPE *pB=pB0;
   const TYPE rbeta = ( (gescal) ? ATL_rone : *beta );
   TYPE *pA=pA0;

   for (i=nMb; i; i--)
   {
      if (A)
      {
         A2blk(K, NB, A, lda, pA, alpha);  /* get 1 row panel of A */
         A += incA;
      }
      for (j=nNb; j; j--)
      {
         if (gescal) gescal(MB, NB, beta, C, ldc);
         if (nKb)
         {
            NBmm0(MB, NB, KB, ATL_rone, pA, KB, pB, KB, rbeta, C, ldc);
            pA += NBNB2;
            pB += NBNB2;
            if (nKb != 1)
            {
               for (k=nKb-1; k; k--, pA += NBNB2, pB += NBNB2)
                  NBmm_b1(MB, NB, KB, ATL_rone, pA, KB, pB, KB,
                          ATL_rone, C, ldc);
            }
            if (kb)
            {
               KBmm(MB, NB, kb, ATL_rone, pA, kb, pB, kb, ATL_rone, C, ldc);
               pB += ATL_MulByNB(kb)<<1;
            }
         }
         else
         {
            if (ZEROC) Mjoin(PATL,gezero)(MB, NB, C, ldc);
            if (kb)
            {
               KBmm(MB, NB, kb, ATL_rone, pA, kb, pB, kb, rbeta, C, ldc);
               pB += ATL_MulByNB(kb)<<1;
            }
         }
         pA = pA0;
         C += incCn;
      }
      if (jb)
      {
         if (gescal) gescal(MB, jb, beta, C, ldc);
         MBJBmm(jb, K, pA, pB, rbeta, C, ldc);
      }
      pB = pB0;
      if (!A)
      {
         pA0 += incK;
         pA = pA0;
      }
      C += incCm;
   }
   if (ib)
   {
      if (A) A2blk(K, ib, A, lda, pA, alpha);   /* get last row panel of A */
      for(j=nNb; j; j--) /* full column panels of B */
      {
         if (gescal) gescal(ib, NB, beta, C, ldc);
         IBNBmm(ib, K, pA, pB, rbeta, C, ldc);
         pB += incK;
         C += incCn;
      }
      if (jb)
      {
         if (gescal) gescal(ib, jb, beta, C, ldc);
         IBJBmm(ib, jb, K, pA, pB, rbeta, C, ldc);
      }
   }
}

int Mjoin(PATL,mmIJK)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                      const int M, const int N0, const int K,
                      const SCALAR alpha, const TYPE *A, const int lda0,
                      const TYPE *B, const int ldb0, const SCALAR beta,
                      TYPE *C, const int ldc0)
{
   int N = N0;
   int nMb, nNb, nKb, ib, jb, kb, jb2, h, i, j, k, n;
   const size_t incK = ATL_MulByNB(K), lda=lda0, ldb=ldb0, ldc=ldc0;
   size_t incA, incB, incC;
   void *vA=NULL;
   TYPE *pA, *pB;
   MAT2BLK A2blk, B2blk;
   MATSCAL gescal;
   NBMM0 NBmm0;

   nMb = ATL_DivByNB(M);
   nNb = ATL_DivByNB(N);
   nKb = ATL_DivByNB(K);
   ib = M - ATL_MulByNB(nMb);
   jb = N - ATL_MulByNB(nNb);
   kb = K - ATL_MulByNB(nKb);

   if (beta[1] == ATL_rzero)
   {
      gescal = NULL;
      if (*beta == ATL_rone) NBmm0 = Mjoin(PATL,CNBmm_b1);
      else if (*beta == ATL_rzero) NBmm0 = Mjoin(PATL,CNBmm_b0);
      else NBmm0 = Mjoin(PATL,CNBmm_bX);
   }
   else
   {
      gescal = Mjoin(PATL,gescal_bX);
      NBmm0 = Mjoin(PATL,CNBmm_b1);
   }

   i = ATL_Cachelen + ATL_MulBySize(N*K + incK);
   if (i <= ATL_MaxMalloc) vA = malloc(i);
   if (!vA)
   {
      if (TA == AtlasNoTrans && TB == AtlasNoTrans) return(1);
      if (jb) n = nNb + 1;
      else n = nNb;
      for (j=2; !vA; j++)
      {
         k = n / j;
         if (k < 1) break;
         if (k*j < n) k++;
         h = ATL_Cachelen + ATL_MulBySize((k+1)*incK);
         if (h <= ATL_MaxMalloc) vA = malloc(h);
      }
      if (!vA) return(-1);
      n = ATL_MulByNB(k);
      jb2 = 0;
   }
   else
   {
      jb2 = jb;
      k = nNb;
      n = N;
   }
   pA = ATL_AlignPtr(vA);
   if (TB == AtlasNoTrans)
   {
      incB = ldb*n<<1;
      if (alpha[1] == ATL_rzero)
      {
         if (*alpha == ATL_rone) B2blk = Mjoin(PATL,col2blk2_a1);
         else B2blk = Mjoin(PATL,col2blk2_aXi0);
      }
      else B2blk = Mjoin(PATL,col2blk2_aX);
   }
   else if (TB == AtlasConjTrans)
   {
      incB = n<<1;
      if (alpha[1] == ATL_rzero)
      {
         if (*alpha == ATL_rone) B2blk = Mjoin(PATL,row2blkC2_a1);
         else B2blk = Mjoin(PATL,row2blkC2_aXi0);
      }
      else B2blk = Mjoin(PATL,row2blkC2_aX);
   }
   else
   {
      incB = n<<1;
      if (alpha[1] == ATL_rzero)
      {
         if (*alpha == ATL_rone) B2blk = Mjoin(PATL,row2blkT2_a1);
         else B2blk = Mjoin(PATL,row2blkT2_aXi0);
      }
      else B2blk = Mjoin(PATL,row2blkT2_aX);
   }
   if (TA == AtlasNoTrans)
   {
      incA = NB<<1;
      A2blk = Mjoin(PATL,row2blkT_a1);
   }
   else if (TA == AtlasConjTrans)
   {
      incA = ATL_MulByNB(lda)<<1;
      A2blk = Mjoin(PATL,col2blkConj_a1);
   }
   else
   {
      incA = ATL_MulByNB(lda)<<1;
      A2blk = Mjoin(PATL,col2blk_a1);
   }
   incC = ldc*n<<1;
   pB = pA + (incK<<1);

   do
   {
      if (TB == AtlasNoTrans) B2blk(K, n, B, ldb, pB, alpha);
      else B2blk(n, K, B, ldb, pB, alpha);
      Mjoin(PATL,mmIJK2)(K, nMb, k, nKb, ib, jb2, kb, alpha, A, lda, pA,
                         incA, A2blk, pB, beta, C, ldc, gescal, NBmm0);
      N -= n;
      nNb -= k;
      if (N < n)
      {
         jb2 = jb;
         n = N;
         k = nNb;
      }
      C += incC;
      B += incB;
   }
   while (N);

   free(vA);
   return(0);
}
