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
#define NBJBmm Mjoin(PATL,MBJBmm)
#define IBJBmm Mjoin(PATL,IBJBmm)

void Mjoin(PATL,mmJIK2)
             (int K, int nMb, int nNb, int nKb, int ib, int jb, int kb,
              const SCALAR alpha, const TYPE *pA0, const TYPE *B, int ldb,
              TYPE *pB0, int incB, MAT2BLK B2blk, const SCALAR beta,
              TYPE *C, int ldc, MATSCAL gescal, NBMM0 NBmm0)
{
   const int incK = ATL_MulByNB(K)SHIFT, incC = ATL_MulByNB(ldc-nMb) SHIFT;
   const int ZEROC = ((gescal == NULL) && SCALAR_IS_ZERO(beta));
   int i, j = nNb;
   const TYPE *pA=pA0;
   const TYPE rbeta = ( (gescal) ? ATL_rone : *beta );
   TYPE *pB=pB0, *stB=pB0+(ATL_MulByNBNB(nKb)SHIFT);

   if (nNb)
   {
      do  /* Loop over full column panels of B */
      {
         if (B)
         {
            B2blk(K, NB, B, ldb, pB, alpha);
            B += incB;
         }
         if (nMb)
         {
            i = nMb;
            do /* loop over full row panels of A */
            {
               if (gescal) gescal(NB, NB, beta, C, ldc);
               if (nKb) /* loop over full blocks in panels */
               {
                  NBmm0(MB, NB, KB, ATL_rone, pA, KB, pB, KB, rbeta, C, ldc);
                  pA += NBNB2;
                  pB += NBNB2;
                  if (nKb != 1)
                  {
                     do
                     {
                        NBmm_b1(MB, NB, KB, ATL_rone, pA, KB, pB, KB, ATL_rone,
                                C, ldc);
                        pA += NBNB2;
                        pB += NBNB2;
                     }
                     while (pB != stB);
                  }
                  if (kb)
                  {
                     KBmm(MB, NB, kb, ATL_rone, pA, kb, pB, kb, ATL_rone,
                          C, ldc);
                     pA += ATL_MulByNB(kb)<<1;
                  }
               }
               else if (kb)
               {
                  if (ZEROC) Mjoin(PATL,gezero)(MB, NB, C, ldc);
                  KBmm(MB, NB, kb, ATL_rone, pA, kb, pB, kb, rbeta, C, ldc);
                  pA += ATL_MulByNB(kb)<<1;
               }
               pB = pB0;
               C += NB2;
            }
            while (--i);
         }
         if (ib)
         {
            if (gescal) gescal(ib, NB, beta, C, ldc);
            IBNBmm(ib, K, pA, pB, rbeta, C, ldc);
         }
         if (!B)
         {
            pB0 += incK;
            pB = pB0;
            stB += incK;
         }
         C += incC;
         pA = pA0;
      }
      while (--j);
   }
   if (jb)
   {
      if (B) B2blk(K, jb, B, ldb, pB, alpha);
      for (i=nMb; i; i--)
      {
         if (gescal) gescal(NB, jb, beta, C, ldc);
         NBJBmm(jb, K, pA, pB, rbeta, C, ldc);
         pA += incK;
         C += NB2;
      }
      if (ib)
      {
         if (gescal) gescal(ib, jb, beta, C, ldc);
         IBJBmm(ib, jb, K, pA, pB, rbeta, C, ldc);
      }
   }
}

int Mjoin(PATL,mmJIK)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                      const int M0, const int N, const int K,
                      const SCALAR alpha, const TYPE *A, const int lda0,
                      const TYPE *B, const int ldb0, const SCALAR beta,
                      TYPE *C, const int ldc0)
/*
 * Outer three loops for matmul with outer loop over columns of B
 */
{
   int M = M0;
   int nMb, nNb, nKb, ib, jb, kb, ib2, h, i, j, k, m, n;
   const size_t lda=lda0, ldb=ldb0, ldc=ldc0;
   size_t incA, incB, incC;
   int AlphaIsOne;
   const size_t incK = ATL_MulByNB((size_t)K);
   void *vB=NULL, *vC=NULL;
   TYPE *pA, *pB, *pC;
   const TYPE one[2] = {1.0,0.0}, zero[2] = {0.0,0.0};
   MAT2BLK A2blk, B2blk;
   MATSCAL gescal;
   NBMM0 NBmm0;

   nMb = ATL_DivByNB(M);
   nNb = ATL_DivByNB(N);
   nKb = ATL_DivByNB(K);
   ib = M - ATL_MulByNB(nMb);
   jb = N - ATL_MulByNB(nNb);
   kb = K - ATL_MulByNB(nKb);

   pC = C;
   if (beta[1] == ATL_rzero)
   {
      gescal = NULL;
      if (*beta == ATL_rone) NBmm0 = Mjoin(PATL,CNBmm_b1);
      else if (*beta == ATL_rzero) NBmm0 = Mjoin(PATL,CNBmm_b0);
      else NBmm0 = Mjoin(PATL,CNBmm_bX);
   }
   else
   {
      NBmm0 = Mjoin(PATL,CNBmm_b1);
      gescal = Mjoin(PATL,gescal_bX);
   }
/*
 * Special case for when what we are really doing is
 *    C <- beta*C + alpha * A * A'   or   C <- beta*C + alpha * A' * A
 */
   if ( A == B && M == N && TA != TB && (SCALAR_IS_ONE(alpha) || M <= NB)
        && TA != AtlasConjTrans && TB != AtlasConjTrans && lda == ldb )
   {
      AlphaIsOne = SCALAR_IS_ONE(alpha);
      i = ATL_MulBySize(M * K);
      if (!AlphaIsOne && pC == C && !SCALAR_IS_ZERO(beta))
         i += ATL_MulBySize(M*N);
      if (i <= ATL_MaxMalloc) vB = malloc(i + ATL_Cachelen);
      if (vB)
      {
         pA = ATL_AlignPtr(vB);
         if (TA == AtlasNoTrans)
            Mjoin(PATL,row2blkT2_a1)(M, K, A, lda, pA, alpha);
         else Mjoin(PATL,col2blk2_a1)(K, M, A, lda, pA, alpha);
/*
 *       Can't write directly to C if alpha is not one
 */
         if (!AlphaIsOne)
         {
            if (SCALAR_IS_ZERO(beta)) h = ldc;
            else if (pC == C)
            {
               pC = pA + (((size_t)M) * K SHIFT);
               h = M;
            }
            else h = NB;
            Mjoin(PATL,mmJIK2)(K, nMb, nNb, nKb, ib, jb, kb, one, pA, NULL,
                               ldb, pA, 0, NULL, zero, pC, h,
                               Mjoin(PATL,gescal_b0), Mjoin(PATL,CNBmm_b0));

            if (alpha[1] == ATL_rzero)
               Mjoin(PATL,gescal_bXi0)(M, N, alpha, pC, h);
            else Mjoin(PATL,gescal_bX)(M, N, alpha, pC, h);

            if (C != pC)
            {
               if (beta[1] == ATL_rzero)
               {
                  if (*beta == ATL_rone)
                     Mjoin(PATL,putblk_b1)(M, N, pC, C, ldc, beta);
                  else if (*beta == ATL_rnone)
                     Mjoin(PATL,putblk_bn1)(M, N, pC, C, ldc, beta);
                  else if (*beta == ATL_rzero)
                     Mjoin(PATL,putblk_b0)(M, N, pC, C, ldc, beta);
                  else Mjoin(PATL,putblk_bXi0)(M, N, pC, C, ldc, beta);
               }
               else Mjoin(PATL,putblk_bX)(M, N, pC, C, ldc, beta);
            }
         }
         else Mjoin(PATL,mmJIK2)(K, nMb, nNb, nKb, ib, jb, kb, alpha, pA, NULL,
                                 ldb, pA, 0, NULL, beta, C, ldc, gescal, NBmm0);
         free(vB);
         if (vC) free(vC);
         return(0);
      }
   }
   i = ATL_Cachelen + ATL_MulBySize(M*K + incK);
   if (i <= ATL_MaxMalloc) vB = malloc(i);
   if (!vB)
   {
      if (TA != AtlasNoTrans && TB != AtlasNoTrans) return(1);
      if (ib) n = nMb + 1;
      else n = nMb;
      for (j=2; !vB; j++)
      {
         k = n / j;
         if (k < 1) break;
         if (k*j < n) k++;
         h = ATL_Cachelen + ATL_MulBySize((k+1)*incK);
         if (h <= ATL_MaxMalloc) vB = malloc(h);
      }
      if (!vB) return(-1);
      n = k;
      m = ATL_MulByNB(n);
      ib2 = 0;
   }
   else
   {
      n = nMb;
      m = M;
      ib2 = ib;
   }
   pB = ATL_AlignPtr(vB);
   if (TA == AtlasNoTrans)
   {
      incA = m SHIFT;
      if (alpha[1] == ATL_rzero)
      {
         if (*alpha == ATL_rone) A2blk = Mjoin(PATL,row2blkT2_a1);
         else A2blk = Mjoin(PATL,row2blkT2_aXi0);
      }
      else A2blk = Mjoin(PATL,row2blkT2_aX);
   }
   else if (TA == AtlasConjTrans)
   {
      incA = lda*m SHIFT;
      if (alpha[1] == ATL_rzero)
      {
         if (*alpha == ATL_rone) A2blk = Mjoin(PATL,col2blkConj2_a1);
         else A2blk = Mjoin(PATL,col2blkConj2_aXi0);
      }
      else A2blk = Mjoin(PATL,col2blkConj2_aX);
   }
   else
   {
      incA = lda*m SHIFT;
      if (alpha[1] == ATL_rzero)
      {
         if (*alpha == ATL_rone) A2blk = Mjoin(PATL,col2blk2_a1);
         else A2blk = Mjoin(PATL,col2blk2_aXi0);
      }
      else A2blk = Mjoin(PATL,col2blk2_aX);
   }
   if (TB == AtlasNoTrans)
   {
      incB = ATL_MulByNB(ldb) SHIFT;
      B2blk = Mjoin(PATL,col2blk_a1);
   }
   else if (TB == AtlasConjTrans)
   {
      incB = NB2;
      B2blk = Mjoin(PATL,row2blkC_a1);
   }
   else
   {
      incB = NB2;
      B2blk = Mjoin(PATL,row2blkT_a1);
   }
   incC = m SHIFT;

   pA = pB + (incK SHIFT);
   do
   {
      if (TA == AtlasNoTrans) A2blk(m, K, A, lda, pA, alpha);
      else A2blk(K, m, A, lda, pA, alpha);
      Mjoin(PATL,mmJIK2)(K, n, nNb, nKb, ib2, jb, kb, alpha, pA, B, ldb, pB,
                         incB, B2blk, beta, C, ldc, gescal, NBmm0);
      M -= m;
      nMb -= n;
      if (M <= m)
      {
         ib2 = ib;
         m = M;
         n = nMb;
      }
      C += incC;
      A += incA;
   }
   while (M);
   free(vB);
   if (vC) free(vC);
   return(0);
}
