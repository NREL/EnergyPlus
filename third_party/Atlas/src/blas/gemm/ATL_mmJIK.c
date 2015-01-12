/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 1997 R. Clint Whaley
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
              TYPE *C, int ldc, TYPE *pC, PUTBLK putblk, NBMM0 NBmm0)
{
   const int incK = ATL_MulByNB(K), incC = ATL_MulByNB(ldc-nMb);
   const int ZEROC = ((putblk == NULL) && (beta == ATL_rzero));
   int i, j = nNb, ldpc;
   const TYPE *pA=pA0;
   const TYPE cubeta = ( (putblk) ? ATL_rzero : beta );
   TYPE *pB=pB0, *stB=pB0+ATL_MulByNBNB(nKb);

   if (putblk)
   {
      ldpc = NB;
      if (!nKb && kb) Mjoin(PATL,gezero)(MB, NB, pC, MB);
   }
   else ldpc = ldc;

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
               if (nKb) /* loop over full blocks in panels */
               {  /* 1st block does scaling */
                  NBmm0(MB, NB, KB, ATL_rone, pA, KB, pB, KB, beta, pC, ldpc);
                  pA += NBNB;
                  pB += NBNB;
                  if (nKb != 1)
                  {
                     do
                     {
                        NBmm(MB, NB, KB, ATL_rone, pA, KB, pB, KB, ATL_rone,
                             pC, ldpc);
                        pA += NBNB;
                        pB += NBNB;
                     }
                     while (pB != stB);
                  }
                  if (kb)
                  {
                     KBmm(MB, NB, kb, ATL_rone, pA, kb, pB, kb, ATL_rone,
                          pC, ldpc);
                     pA += ATL_MulByNB(kb);
                  }
               }
               else
               {
                  if (ZEROC) Mjoin(PATL,gezero)(MB, NB, pC, ldpc);
                  if (kb)
                  {
                     KBmm(MB, NB, kb, ATL_rone, pA, kb, pB, kb, cubeta,
                          pC, ldpc);
                     pA += ATL_MulByNB(kb);
                  }
               }
               pB = pB0;
               if (putblk) putblk(NB, NB, pC, C, ldc, beta);
               else pC += NB;
               C += NB;
            }
            while (--i);
         }
         if (ib)
         {
            if (putblk)
            {
               IBNBmm(ib, K, pA, pB, ATL_rzero, pC, ib);
               putblk(ib, NB, pC, C, ldc, beta);
            }
            else IBNBmm(ib, K, pA, pB, beta, C, ldc);
         }
         if (!B)
         {
            pB0 += incK;
            pB = pB0;
            stB += incK;
         }
         C += incC;
         if (!putblk) pC = C;
         pA = pA0;
      }
      while (--j);
   }
   if (jb)
   {
      if (B) B2blk(K, jb, B, ldb, pB, alpha);
      for (i=nMb; i; i--)
      {
         NBJBmm(jb, K, pA, pB, cubeta, pC, ldpc);
         if (putblk) putblk(NB, jb, pC, C, ldc, beta);
         else pC += NB;
         pA += incK;
         C += NB;
      }
      if (ib)
      {
         if (putblk)
         {
            IBJBmm(ib, jb, K, pA, pB, ATL_rzero, pC, ib);
            putblk(ib, jb, pC, C, ldc, beta);
         }
         else IBJBmm(ib, jb, K, pA, pB, beta, C, ldc);
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
   const size_t lda = lda0, ldb = ldb0, ldc = ldc0;
   const size_t incK = ATL_MulByNB((size_t)K);
   size_t incA, incB, incC;
   void *vB=NULL, *vC=NULL;
   TYPE *pA, *pB, *pC;
   MAT2BLK A2blk, B2blk;
   PUTBLK putblk;
   NBMM0 NBmm0;

   nMb = ATL_DivByNB(M);
   nNb = ATL_DivByNB(N);
   nKb = ATL_DivByNB(K);
   ib = M - ATL_MulByNB(nMb);
   jb = N - ATL_MulByNB(nNb);
   kb = K - ATL_MulByNB(nKb);

/*
 * If K sufficiently large, write to temporary C as safety measure;  otherwise
 * write directly to C
 */
   if (nKb < 12)
   {
      putblk = NULL;
      pC = C;
      if ( SCALAR_IS_ONE(beta) ) NBmm0 = NBmm_b1;
      else if ( SCALAR_IS_ZERO(beta) ) NBmm0 = NBmm_b0;
      else NBmm0 = NBmm_bX;
   }
   else
   {
      NBmm0 = NBmm_b0;
      vC = malloc(ATL_MulBySize(NBNB)+ATL_Cachelen);
      if (!vC) return(-1);
      pC = ATL_AlignPtr(vC);
      if ( SCALAR_IS_ONE(beta) ) putblk = Mjoin(PATL,putblk_b1);
      else if ( SCALAR_IS_ZERO(beta) ) putblk = Mjoin(PATL,putblk_b0);
      else if ( SCALAR_IS_NONE(beta) ) putblk = Mjoin(PATL,putblk_bn1);
      else putblk = Mjoin(PATL,putblk_bX);
   }
/*
 * Special case for when we don't need to copy one or more of our matrices
 */
   if (K == NB && TA == AtlasTrans && lda == NB && ATL_DataIsMinAligned(A))
   {
      pA = (TYPE *) A;
      if (TB == AtlasNoTrans && ldb == NB && SCALAR_IS_ONE(alpha) &&
          ATL_DataIsMinAligned(B))
      {
         i = NBNB;
         pB = (TYPE *) B;
         B = NULL;
         B2blk = NULL;
         incB = 0;
      }
      else
      {
         i = NBNB + incK;
         vB = malloc(ATL_MulBySize(incK) + ATL_Cachelen);
         if (!vB)
         {
            if (vC) free(vC);
            return(-1);
         }
         pB = ATL_AlignPtr(vB);
         if (TB == AtlasNoTrans)
         {
            incB = ATL_MulByNB(ldb);
            if ( SCALAR_IS_ONE(alpha) ) B2blk = Mjoin(PATL,col2blk_a1);
            else B2blk = Mjoin(PATL,col2blk_aX);
         }
         else
         {
            incB = NB;
            if ( SCALAR_IS_ONE(alpha) ) B2blk = Mjoin(PATL,row2blkT_a1);
            else B2blk = Mjoin(PATL,row2blkT_aX);
         }
      }
      Mjoin(PATL,mmJIK2)(K, nMb, nNb, nKb, ib, jb, kb, alpha, pA, B, ldb, pB,
                          incB, B2blk, beta, C, ldc, pC, putblk, NBmm0);
      if (vB) free(vB);
      if (vC) free(vC);
      return(0);
   }
/*
 * Special case for when what we are really doing is
 *    C <- beta*C + alpha * A * A'   or   C <- beta*C + alpha * A' * A
 */
   if ( A == B && M == N && TA != TB && lda == ldb &&
        (SCALAR_IS_ONE(alpha) || M <= NB) )
   {
      i = ATL_MulBySize(M * K);
      if (!SCALAR_IS_ONE(alpha) && pC == C && !SCALAR_IS_ZERO(beta))
         i += ATL_MulBySize(M*N);
      if (i <= ATL_MaxMalloc) vB = malloc(i + ATL_Cachelen);
      if (vB)
      {
         pA = ATL_AlignPtr(vB);
         if (TA == AtlasNoTrans)
            Mjoin(PATL,row2blkT2_a1)(M, K, A, lda, pA, alpha);
         else Mjoin(PATL,col2blk_a1)(K, M, A, lda, pA, alpha);
/*
 *       Can't write directly to C if alpha is not one
 */
         if (!SCALAR_IS_ONE(alpha))
         {
            if (SCALAR_IS_ZERO(beta)) h = ldc;
            else if (pC == C)
            {
               pC = pA + ((size_t)M) * K;
               h = M;
            }
            else h = NB;
            Mjoin(PATL,mmJIK2)(K, nMb, nNb, nKb, ib, jb, kb, 1.0, pA, NULL,
                                 ldb, pA, 0, NULL, 0.0, pC, h, pC, NULL,
                                 NBmm_b0);

            Mjoin(PATL,gescal_bX)(M, N, alpha, pC, h);

            if (C != pC)
            {
               if (SCALAR_IS_ONE(beta))
                  Mjoin(PATL,putblk_b1)(M, N, pC, C, ldc, beta);
               else if (SCALAR_IS_NONE(beta))
                  Mjoin(PATL,putblk_bn1)(M, N, pC, C, ldc, beta);
               else if (SCALAR_IS_ZERO(beta))
                  Mjoin(PATL,putblk_b0)(M, N, pC, C, ldc, beta);
               else
                  Mjoin(PATL,putblk_bX)(M, N, pC, C, ldc, beta);
            }
         }
         else
            Mjoin(PATL,mmJIK2)(K, nMb, nNb, nKb, ib, jb, kb, alpha, pA, NULL,
                                 ldb, pA, 0, NULL, beta, C, ldc, pC,
                                 putblk, NBmm0);
         free(vB);
         if (vC) free(vC);
         return(0);
      }
   }
   i = ATL_Cachelen + ATL_MulBySize(M*K + incK);
   if (i <= ATL_MaxMalloc) vB = malloc(i);
   if (!vB)
   {
      if (TA != AtlasNoTrans && TB != AtlasNoTrans)
      {
         if (vC) free(vC);
         return(1);
      }
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
      if (!vB)
      {
         if (vC) free(vC);
         return(-1);
      }
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
      incA = m;
      if ( SCALAR_IS_ONE(alpha) ) A2blk = Mjoin(PATL,row2blkT2_a1);
      else A2blk = Mjoin(PATL,row2blkT2_aX);
   }
   else
   {
      incA = lda*m;
      if ( SCALAR_IS_ONE(alpha) ) A2blk = Mjoin(PATL,col2blk2_a1);
      else A2blk = Mjoin(PATL,col2blk2_aX);
   }
   if (TB == AtlasNoTrans)
   {
      incB = ATL_MulByNB(ldb);
      B2blk = Mjoin(PATL,col2blk_a1);
   }
   else
   {
      incB = NB;
      B2blk = Mjoin(PATL,row2blkT_a1);
   }
   incC = m;

   pA = pB + incK;
   do
   {
      if (TA == AtlasNoTrans) A2blk(m, K, A, lda, pA, alpha);
      else A2blk(K, m, A, lda, pA, alpha);
      Mjoin(PATL,mmJIK2)(K, n, nNb, nKb, ib2, jb, kb, alpha, pA, B, ldb, pB,
                          incB, B2blk, beta, C, ldc, pC, putblk, NBmm0);
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
      if (!putblk) pC = C;
   }
   while (M);
   free(vB);
   if (vC) free(vC);
   return(0);
}
