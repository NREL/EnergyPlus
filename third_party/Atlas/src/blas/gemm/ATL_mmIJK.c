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

#define KBmm Mjoin(PATL,pKBmm)
#define IBNBmm Mjoin(PATL,IBNBmm)
#define NBJBmm Mjoin(PATL,MBJBmm)
#define IBJBmm Mjoin(PATL,IBJBmm)
#define col2blk Mjoin(PATL,col2blk_a1)

void Mjoin(PATL,mmIJK2)(int K, int nMb, int nNb, int nKb, int ib, int jb,
                        int kb, const SCALAR alpha, const TYPE *A, int lda,
                        TYPE *pA0, int incA, MAT2BLK A2blk, const TYPE *pB0,
                        const SCALAR beta, TYPE *C, int ldc, TYPE *pC,
                        PUTBLK putblk, NBMM0 NBmm0)
/*
 * Outer three loops for matmul with outer loop over rows of A
 */
{
   int i, j, ldpc;
   const int ZEROC = ((putblk == NULL) && SCALAR_IS_ZERO(beta));
   const int incK = ATL_MulByNB(K), incC = ATL_MulByNB(ldc);
   TYPE *pA=pA0, *stA=pA0+ATL_MulByNBNB(nKb);
   const TYPE *pB=pB0;
   const TYPE cubeta = ( (putblk) ? ATL_rzero : beta );
   TYPE *c;

   if (putblk)
   {
      ldpc = NB;
      if (!nKb && kb) Mjoin(PATL,gezero)(MB, NB, pC, MB);
   }
   else ldpc = ldc;
   for (i=nMb; i; i--)    /* loop over full row panels of A */
   {
      if (A)
      {
         A2blk(K, NB, A, lda, pA, alpha);  /* get 1 row panel of A */
         A += incA;
      }
      if (!putblk) pC = C;
      c = C;
      C += NB;
      for (j=nNb; j; j--)  /* full column panels of B */
      {
         if (nKb)
         {
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
               while (pA != stA);
            }
            if (kb)
            {
               KBmm(MB, NB, kb, ATL_rone, pA, kb, pB, kb, ATL_rone, pC, ldpc);
               pB += kb*NB;
            }
         }
         else
         {
            if (ZEROC) Mjoin(PATL,gezero)(MB, NB, pC, ldpc);
            if (kb)
            {
               KBmm(MB, NB, kb, ATL_rone, pA, kb, pB, kb, cubeta, pC, ldpc);
               pB += kb*NB;
            }
         }
         pA = pA0;
         if (putblk) putblk(NB, NB, pC, c, ldc, beta);
         else pC += incC;
         c += incC;
      }
      if (jb)
      {
         NBJBmm(jb, K, pA, pB, cubeta, pC, ldpc);
         if (putblk) putblk(NB, jb, pC, c, ldc, beta);
      }
      pB = pB0;
      if (!A)
      {
         pA0 += incK;
         pA = pA0;
         stA += incK;
      }
   }
   if (ib)
   {
      c = C;
      if (A) A2blk(K, ib, A, lda, pA, alpha);  /* get last row panel of A */
      for (j=nNb; j; j--)  /* full column panels of B */
      {
         if (putblk)
         {
            IBNBmm(ib, K, pA, pB, ATL_rzero, pC, ib);
            putblk(ib, NB, pC, c, ldc, beta);
         }
         else IBNBmm(ib, K, pA, pB, beta, c, ldc);
         pB += incK;
         c += incC;
      }
      if (jb)
      {
         if (putblk)
         {
            IBJBmm(ib, jb, K, pA, pB, ATL_rzero, pC, ib);
            putblk(ib, jb, pC, c, ldc, beta);
         }
         else IBJBmm(ib, jb, K, pA, pB, beta, c, ldc);
      }
   }
}

int Mjoin(PATL,mmIJK)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                      const int M, const int N0, const int K,
                      const SCALAR alpha, const TYPE *A, const int lda0,
                      const TYPE *B, const int ldb0, const SCALAR beta,
                      TYPE *C, const int ldc0)
{
   size_t incA, incB, incC;
   const size_t lda=lda0, ldb=ldb0, ldc=ldc0;
   const size_t incK = ATL_MulByNB((size_t)K);
   int N = N0;
   int nMb, nNb, nKb, ib, jb, kb, jb2, h, i, j, k, n;
   void *vA=NULL, *vC=NULL;
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
      vC = malloc(ATL_Cachelen + ATL_MulBySize(NBNB));
      if (!vC) return(-1);
      pC = ATL_AlignPtr(vC);
      if ( SCALAR_IS_ONE(beta) ) putblk = Mjoin(PATL,putblk_b1);
      else if ( SCALAR_IS_ZERO(beta) ) putblk = Mjoin(PATL,putblk_b0);
      else if ( SCALAR_IS_NONE(beta) ) putblk = Mjoin(PATL,putblk_bn1);
      else putblk = Mjoin(PATL,putblk_bX);
   }
/*
 * Special case if we don't need to copy one or more input matrix
 */
   if (K == NB && TB == AtlasNoTrans && ldb == NB && ATL_DataIsMinAligned(B))
   {
      if (lda == NB && TA == AtlasTrans && SCALAR_IS_ONE(alpha) &&
          ATL_DataIsMinAligned(A))
      {
         i = NBNB;
         pA = (TYPE *) A;
         A = NULL;
         A2blk = NULL;
         incA = 0;
      }
      else
      {
         vA = malloc(ATL_Cachelen + ATL_MulBySize(incK));
         if (!vA)
         {
            free(vC);
            return(-1);
         }
         pA = ATL_AlignPtr(vA);
         if (TA == AtlasNoTrans)
         {
            incA = NB;
            if ( SCALAR_IS_ONE(alpha) ) A2blk = Mjoin(PATL,row2blkT_a1);
            else A2blk = Mjoin(PATL,row2blkT_aX);
         }
         else
         {
            incA = ATL_MulByNB(lda);
            if ( SCALAR_IS_ONE(alpha) ) A2blk = Mjoin(PATL,col2blk_a1);
            else A2blk = Mjoin(PATL,col2blk_aX);
         }
      }
      Mjoin(PATL,mmIJK2)(K, nMb, nNb, nKb, ib, jb, kb, alpha, A, lda, pA,
                          incA, A2blk, B, beta, C, ldc, pC, putblk, NBmm0);
      if (vA) free(vA);
      if (vC) free(vC);
      return(0);
   }
   i = ATL_Cachelen + ATL_MulBySize(N*K + incK);
   if (i <= ATL_MaxMalloc) vA = malloc(i);
   if (!vA)
   {
      if (TA == AtlasNoTrans && TB == AtlasNoTrans)
      {
         if (vC) free(vC);
         return(1);
      }
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
      if (!vA)
      {
         if (vC) free(vC);
         return(-1);
      }
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
      incB = ldb*n;
      if ( SCALAR_IS_ONE(alpha) ) B2blk = Mjoin(PATL,col2blk2_a1);
      else B2blk = Mjoin(PATL,col2blk2_aX);
   }
   else
   {
      incB = n;
      if ( SCALAR_IS_ONE(alpha) ) B2blk = Mjoin(PATL,row2blkT2_a1);
      else B2blk = Mjoin(PATL,row2blkT2_aX);
   }
   if (TA == AtlasNoTrans)
   {
      incA = NB;
      A2blk = Mjoin(PATL,row2blkT_a1);
   }
   else
   {
      incA = ATL_MulByNB(lda);
      A2blk = Mjoin(PATL,col2blk_a1);
   }
   incC = ldc*n;
   pB = pA + incK;

   do
   {
      if (TB == AtlasNoTrans) B2blk(K, n, B, ldb, pB, alpha);
      else B2blk(n, K, B, ldb, pB, alpha);
      Mjoin(PATL,mmIJK2)(K, nMb, k, nKb, ib, jb2, kb, alpha, A, lda, pA,
                          incA, A2blk, pB, beta, C, ldc, pC, putblk, NBmm0);
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
      if (!putblk) pC = C;
   }
   while (N);

   if (vC) free(vC);
   free(vA);
   return(0);
}
