/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2003 R. Clint Whaley
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
#include "atlas_pkblas.h"
static void ATL_gNBmm_bX(const int M, const int N, const int K, SCALAR alpha,
                         const TYPE *A, const int lda, const TYPE *B,
                         const int ldb, const SCALAR beta, TYPE *C,
                         const int ldc)
{
   if (M == MB && N == NB && K == KB)
      NBmm_bX(M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   else if (M != MB)
   {
      if (N == NB && K == KB)
         Mjoin(PATL,pMBmm_bX)(M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
      else Mjoin(PATL,pKBmm)(M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   }
   else if (N != NB)  /* ib is full */
   {
      if (K == KB)
         Mjoin(PATL,pNBmm_bX)(M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
      else Mjoin(PATL,pKBmm)(M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   }
   else  /* ib and jb are full */
      Mjoin(PATL,pKBmm_bX)(M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
}

static void ATL_gNBmm(const int M, const int N, const int K, SCALAR alpha,
                      const TYPE *A, const int lda, const TYPE *B,
                      const int ldb, const SCALAR beta, TYPE *C,
                      const int ldc)
/*
 * BETA is known to be 0 or 1
 */
{
   if (M == MB && N == NB && K == KB)
   {
      if (beta == ATL_rone)
         NBmm_b1(M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
      else
         NBmm_b0(M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   }
   else if (M != MB)
   {
      if (N == NB && K == KB)
      {
         if (beta == ATL_rone)
            Mjoin(PATL,pMBmm_b1)(M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
         else
            Mjoin(PATL,pMBmm_b0)(M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
      }
      else Mjoin(PATL,pKBmm)(M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   }
   else if (N != NB)  /* ib is full */
   {
      if (K == KB)
      {
         if (beta == ATL_rone)
            Mjoin(PATL,pNBmm_b1)(M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
         else
            Mjoin(PATL,pNBmm_b0)(M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
      }
      else Mjoin(PATL,pKBmm)(M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   }
   else  /* ib and jb are full */
   {
      if (beta == ATL_rone)
         Mjoin(PATL,pKBmm_b1)(M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
      else
         Mjoin(PATL,pKBmm_b0)(M, N, K, alpha, A, lda, B, ldb, beta, C, ldc);
   }
}

#define ATL_pmmJIKF      Mjoin(PATL,pmmJIKF)
#define ATL_pmmJIK       Mjoin(PATL,pmmJIK)
#define ATL_prow2blkTF   Mjoin(PATL,prow2blkTF)
#define ATL_pcol2blkF    Mjoin(PATL,pcol2blkF)
#define ATL_pcol2blk     Mjoin(PATL,pcol2blk_a1)
#define ATL_pcol2blk_aX  Mjoin(PATL,pcol2blk_aX)
#define ATL_prow2blkT    Mjoin(PATL,prow2blkT_a1)
#define ATL_prow2blkT_aX Mjoin(PATL,prow2blkT_aX)
#define ATL_pputblk      Mjoin(PATL,pputblk)
#define KBmm            Mjoin(PATL,pKBmm)
int Mjoin(PATL,prk_kmm)(const enum ATLAS_UPLO UC,
                        const enum PACK_UPLO UA, const enum ATLAS_TRANS TA,
                        const int N, const int K, const SCALAR alpha,
                        const TYPE *A, const int lda, const SCALAR beta,
                        const int CP, TYPE *C, const int ldc)
/*
 * If CP == 0, then C is a general rectangular matrix.
 * If CP != 0, then C is packed matrix.
 * In both cases, UC tells what portion of the matrix to update.
 * Performs a syrk/sprk by calling the gemm kernel directly
 */
{
   const enum PACK_UPLO UC2 = ((CP) ? (enum PACK_UPLO)UC : PackGen);
   const int nKb = ATL_DivByNB(K), kb = K - ATL_MulByNB(nKb);
   const int KK = K - kb;
   const int incK = ATL_MulByNB(K)SHIFT;
   const int ldainc = (UA == AtlasUpper) ? 1 : ((UA == AtlasLower) ? -1 : 0);
   const int ldcinc = (UC2 == AtlasUpper) ? 1 : ((UC2 == AtlasLower) ? -1 : 0);
   const int BUFC = (CP || K >= (NB*12) || (alpha != ATL_rone));
   int ldcc;
   int i, j, k, ib, jb, iend;
   void *vC=NULL;
   TYPE *pC, *pA, *pB, *c;
   TYPE cbeta;
   NBMM0 pNBmm, pNBmm0;

/*
 * Grab enough space for NBxNB C workspace and all of A, and copy all of A.
 * If this is too much, we will reduce K in outer routine until malloc succeeds
 */
   i = ATL_Cachelen + ATL_MulBySize(NBNB+N*K);
   if (i <= ATL_pkMaxMalloc || K <= NB) vC = malloc(i);
   if (!vC) return(-1);
   pC = ATL_AlignPtr(vC);
   pB = pC + NBNB;
   if (TA == AtlasNoTrans)
      ATL_prow2blkTF(N, K, ATL_rone, A, lda, ldainc, pB);
   else ATL_pcol2blkF(K, N, ATL_rone, A, lda, ldainc, pB);
/*
 * Loop over column panels of $C$
 */
   for (j=0; j < N; j += NB)
   {
      jb = N-j;
      jb = Mmin(jb, NB);
/*
 *    Loop over row panels of $C$
 */
      iend   = (UC == AtlasUpper) ? j+1 : N;
      for (i=((UC == AtlasUpper) ? 0 : j); i < iend; i += NB)
      {
         pA = pC + NBNB + i*(K SHIFT);
         ib = N-i;
         ib = Mmin(ib, NB);
         if (ib == NB && jb == NB)
         {
            pNBmm = NBmm;
            if (BUFC || i == j)
            {
               c = pC;
               ldcc = ib;
               pNBmm0 = NBmm_b0;
               cbeta = ATL_rzero;
            }
            else
            {
               c = C+i+j*ldc;
               ldcc = ldc;
               if (beta == ATL_rone) pNBmm0 = NBmm;
               else if (beta == ATL_rzero) pNBmm0 = NBmm_b0;
               else pNBmm0 = NBmm_bX;
               cbeta = beta;
            }
         }
         else /* if (ib != NB || jb != NB) */
         {
            pNBmm0 = pNBmm = ATL_gNBmm;
            if (BUFC || i == j)
            {
               c = pC;
               ldcc = ib;
               cbeta = ATL_rzero;
               Mjoin(PATL,gezero)(ib, jb, c, ldcc);
            }
            else
            {
               c = C+i+j*ldc;
               ldcc = ldc;
               cbeta = beta;
               if (beta == ATL_rzero)
                  Mjoin(PATL,gezero)(ib, jb, c, ldcc);
               else if (beta != ATL_rone) pNBmm0 = ATL_gNBmm_bX;
            }
         }
         if (nKb)
         {
            pNBmm0(ib, jb, NB, ATL_rone, pA, NB, pB, NB, cbeta, c, ldcc);
            for (k=NB; k != KK; k += NB)
               pNBmm(ib, jb, NB, ATL_rone, pA+ib*(k SHIFT), NB, pB+jb*(k SHIFT),
                     NB, ATL_rone, c, ldcc);
            if (kb)
               ATL_gNBmm(ib, jb, kb, ATL_rone, pA+ib*(KK SHIFT), kb,
                         pB+jb*(KK SHIFT), kb, ATL_rone, c, ldcc);
         }
         else if (kb)
         {
            if (cbeta == ATL_rzero && ib == NB && jb == NB)
               Mjoin(PATL,gezero)(ib, jb, c, ldcc);
            if (c == pC || cbeta == ATL_rzero || cbeta == ATL_rone)
               ATL_gNBmm(ib,jb,kb, ATL_rone, pA, kb, pB, kb, cbeta,
                         c, ldcc);
            else
               ATL_gNBmm_bX(ib,jb,kb, ATL_rone, pA, kb, pB, kb, cbeta,
                            c, ldcc);
         }
         if (c == pC)
         {
            if (i != j)
               Mjoin(PATL,pputblk_aX)(ib, jb, pC, C+MindexP(UC2,i,j,ldc),
                                      Mpld(UC2,j,ldc), ldcinc, alpha, beta);
            else
               Mjoin(PATL,pputblk_diag)(ib, jb, pC, UC, C+MindexP(UC2,i,j,ldc),
                                        Mpld(UC2,j,ldc), ldcinc, alpha, beta);
         }
      }
      pB += incK;
   }
   free(vC);
   return(0);
}
