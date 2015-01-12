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

int ATL_pmmJIK(const enum PACK_UPLO UA, const enum ATLAS_TRANS TA,
               const enum PACK_UPLO UB, const enum ATLAS_TRANS TB,
               const int M, const int N, const int K, const SCALAR alpha,
               const TYPE *A, const int lda, const TYPE *B, const int ldb,
               const SCALAR beta, const enum PACK_UPLO UC,
               TYPE *C, const int ldc)
/*
 * Special packed matmul, calls dense gemm kernel using at most
 * K*NB + 2*NB*NB space.  $B$ is copied only once, but $A$ is copied
 * ceil(N/NB) times.  However, $A$ should start in-cache for kernel call.
 */

{
   const int nKb = ATL_DivByNB(K), kb = K - ATL_MulByNB(nKb);
   const int incK = ATL_MulByNB(K);
   const int ldainc = (UA == AtlasUpper) ? 1 : ((UA == AtlasLower) ? -1 : 0);
   const int ldbinc = (UB == AtlasUpper) ? 1 : ((UB == AtlasLower) ? -1 : 0);
   const int ldcinc = (UC == AtlasUpper) ? 1 : ((UC == AtlasLower) ? -1 : 0);
   int ib, jb, i, j, k;
   void *vC;
   TYPE *pC, *pA, *pB;
   NBMM0 pNBmm, pNBmm0;

   vC = malloc(ATL_Cachelen + ATL_MulBySize(NBNB+NBNB+incK));
   if (!vC) return(-1);
   pC = ATL_AlignPtr(vC);
   pA = pC + NBNB;
   pB = pA + NBNB;

/*
 * Loop over column panels of $B$
 */
   for (j=0; j < N; j += NB)
   {
      jb = N - j;
      jb = Mmin(jb, NB);
/*
 *    Copy column-panel of B to block-major storage
 */
      if (alpha == 1.0)
      {
         if (TB == AtlasNoTrans)
            ATL_pcol2blk(K, jb, alpha, B+MindexP(UB,0,j,ldb), Mpld(UB,j,ldb),
                         ldbinc, pB);
         else /* TB == AtlasTrans */
            ATL_prow2blkT(jb, K, alpha, B+MindexP(UB,j,0,ldb), ldb, ldbinc, pB);
      }
      else if (TB == AtlasNoTrans)
         ATL_pcol2blk_aX(K, jb, alpha, B+MindexP(UB,0,j,ldb), Mpld(UB,j,ldb),
                         ldbinc, pB);
      else /* TB == AtlasTrans */
         ATL_prow2blkT_aX(jb, K, alpha, B+MindexP(UB,j,0,ldb), ldb, ldbinc, pB);
/*
 *    Loop over row-panels of A
 */
      for (i=0; i < M; i += MB)
      {
         ib = M - i;
         ib = Mmin(ib, MB);
         if (jb != NB || ib != MB)
         {
            pNBmm0 = pNBmm = ATL_gNBmm;
            if (ib != NB && jb != NB) Mjoin(PATL,gezero)(MB, NB, pC, MB);
         }
         else
         {
            pNBmm = NBmm;
            pNBmm0 = NBmm_b0;
         }
/*
 *       Handle full blocks of K
 */
         if (nKb)
         {
            if (TA == AtlasNoTrans)
               ATL_prow2blkT(ib, NB, 1.0, A+MindexP(UA,i,0,lda),
                             lda, ldainc, pA);
            else
               ATL_pcol2blk(NB, ib, 1.0, A+MindexP(UA,0,i,lda),
                            Mpld(UA,i,lda), ldainc, pA);
            pNBmm0(ib, jb, NB, ATL_rone, pA, NB, pB, NB, ATL_rzero, pC, ib);
            for (k=1; k != nKb; k++)
            {
               if (TA == AtlasNoTrans)
                  ATL_prow2blkT(ib, NB, 1.0, A+MindexP(UA,i,ATL_MulByNB(k),lda),
                                Mpld(UA,ATL_MulByNB(k),lda), ldainc, pA);
               else
                  ATL_pcol2blk(NB, ib, 1.0, A+MindexP(UA,ATL_MulByNB(k),i,lda),
                               Mpld(UA,i,lda), ldainc, pA);
               pNBmm(ib, jb, NB, ATL_rone, pA, NB, pB+jb*NB*k, NB,
                     ATL_rone, pC, ib);
            }
            if (kb)
            {
               if (TA == AtlasNoTrans)
                  ATL_prow2blkT(ib, kb, 1.0,
                                A+MindexP(UA,i,ATL_MulByNB(nKb),lda),
                                Mpld(UA,ATL_MulByNB(nKb),lda), ldainc, pA);
               else
                  ATL_pcol2blk(kb, ib, 1.0,
                               A+MindexP(UA,ATL_MulByNB(nKb),i,lda),
                               Mpld(UA,i,lda), ldainc, pA);
               ATL_gNBmm(ib, jb, kb, ATL_rone, pA, kb, pB+jb*NB*nKb, kb,
                         ATL_rone, pC, ib);
            }
         }
         else if (kb)
         {
            Mjoin(PATL,gezero)(ib, jb, pC, ib);
            if (TA == AtlasNoTrans)
               ATL_prow2blkT(ib, kb, 1.0, A+MindexP(UA,i,0,lda),
                             lda, ldainc, pA);
            else
               ATL_pcol2blk(kb, ib, 1.0, A+MindexP(UA,0,i,lda),
                            Mpld(UA,i,lda), ldainc, pA);
            ATL_gNBmm(ib, jb, kb, ATL_rone, pA, kb, pB, kb, ATL_rzero, pC, ib);
         }
         ATL_pputblk(ib, jb, pC, C+MindexP(UC,i,j,ldc), Mpld(UC,j,ldc),
                     ldcinc, beta);
      }
   }
   free(vC);
   return(0);
}
int ATL_pmmJIKF(const enum PACK_UPLO UA, const enum ATLAS_TRANS TA,
                const enum PACK_UPLO UB, const enum ATLAS_TRANS TB,
                const int M, const int N, const int K, const SCALAR alpha,
                const TYPE *A, const int lda, const TYPE *B, const int ldb,
                const SCALAR beta, const enum PACK_UPLO UC,
                TYPE *C, const int ldc)
/*
 * Special packed matmul, calls dense gemm kernel using at most
 * M*K + K*NB + NB*NB space.  If this exceeds ATL_pkMaxMalloc or fails,
 * operates using at most 2*K*NB + NB*NB.  If this fails, returns non-zero.
 * If full space is malloced, both matrices are copied exactly once.  If
 * the smaller space is used, $A$ will be copied ceil(N/NB) times.
 */
{
   const int nKb = ATL_DivByNB(K), kb = K - ATL_MulByNB(nKb);
   const int incK = ATL_MulByNB(K);
   const int ldainc = (UA == AtlasUpper) ? 1 : ((UA == AtlasLower) ? -1 : 0);
   const int ldbinc = (UB == AtlasUpper) ? 1 : ((UB == AtlasLower) ? -1 : 0);
   const int ldcinc = (UC == AtlasUpper) ? 1 : ((UC == AtlasLower) ? -1 : 0);
   int ib, jb, i, j, k;
   void *vC=NULL;
   TYPE *pC, *pA, *pB, *pA0;
   NBMM0 pNBmm, pNBmm0;
   void (*A2blk)(const int M, const int N, const TYPE alpha, const TYPE *A,
                 int lda, const int ldainc, TYPE *V);

   i = ATL_Cachelen + ATL_MulBySize(NBNB+ATL_MulByNB(K)+M*K);
   if (i <= ATL_pkMaxMalloc) vC = malloc(i);
   if (!vC)
   {
      vC = malloc(ATL_Cachelen + ATL_MulBySize(NBNB+ATL_MulByNB(K+K)));
      if (TA == AtlasNoTrans) A2blk = ATL_prow2blkT;
      else A2blk = ATL_pcol2blk;
   }
   else A2blk = NULL;

   if (!vC) return(-1);
   pC = ATL_AlignPtr(vC);
   pB = pC + NBNB;
   pA = pB + ATL_MulByNB(K);

/*
 * If we've got the space, copy all of A up front
 */
   if (!A2blk)
   {
      if (TA == AtlasNoTrans)
         ATL_prow2blkTF(M, K, ATL_rone, A, lda, ldainc, pA);
      else ATL_pcol2blkF(K, M, ATL_rone, A, lda, ldainc, pA);
      pA -= ATL_MulByNB(K);
   }
   pA0 = pA;
/*
 * Loop over column panels of $B$
 */
   for (j=0; j < N; j += NB)
   {
      jb = N - j;
      jb = Mmin(jb, NB);
/*
 *    Copy column-panel of B to block-major storage
 */
      if (alpha == 1.0)
      {
         if (TB == AtlasNoTrans)
            ATL_pcol2blk(K, jb, alpha, B+MindexP(UB,0,j,ldb), Mpld(UB,j,ldb),
                         ldbinc, pB);
         else /* TB == AtlasTrans */
            ATL_prow2blkT(jb, K, alpha, B+MindexP(UB,j,0,ldb), ldb, ldbinc, pB);
      }
      else if (TB == AtlasNoTrans)
         ATL_pcol2blk_aX(K, jb, alpha, B+MindexP(UB,0,j,ldb), Mpld(UB,j,ldb),
                         ldbinc, pB);
      else /* TB == AtlasTrans */
         ATL_prow2blkT_aX(jb, K, alpha, B+MindexP(UB,j,0,ldb), ldb, ldbinc, pB);
/*
 *    Loop over row-panels of A
 */
      for (i=0; i < M; i += MB)
      {
         ib = M - i;
         ib = Mmin(ib, MB);
         if (A2blk)
         {
            if (TA == AtlasNoTrans)
               ATL_prow2blkT(ib, K, ATL_rone, A+MindexP(UA,i,0,lda), lda,
                             ldainc, pA);
            else /* TA == AtlasTrans */
               ATL_pcol2blk(K, ib, ATL_rone, A+MindexP(UA,0,i,lda),
                            Mpld(UA,i,lda), ldainc, pA);
         }
         else pA += ATL_MulByNB(K);
         if (jb != NB || ib != MB)
         {
            pNBmm0 = pNBmm = ATL_gNBmm;
            if (ib != NB && jb != NB) Mjoin(PATL,gezero)(MB, NB, pC, MB);
         }
         else
         {
            pNBmm = NBmm;
            pNBmm0 = NBmm_b0;
         }
/*
 *       Handle full blocks of K
 */
         if (nKb)
         {
            pNBmm0(ib, jb, NB, ATL_rone, pA, NB, pB, NB, ATL_rzero, pC, ib);
            for (k=1; k != nKb; k++)
            {
               pNBmm(ib, jb, NB, ATL_rone, pA+ib*NB*k, NB, pB+jb*NB*k, NB,
                     ATL_rone, pC, ib);
            }
            if (kb)
               ATL_gNBmm(ib, jb, kb, ATL_rone, pA+ib*NB*nKb, kb,
                         pB+jb*NB*nKb, kb, ATL_rone, pC, ib);
         }
         else if (kb)
         {
            Mjoin(PATL,gezero)(ib, jb, pC, ib);
            ATL_gNBmm(ib, jb, kb, ATL_rone, pA, kb, pB, kb, ATL_rzero, pC, ib);
         }
         ATL_pputblk(ib, jb, pC, C+MindexP(UC,i,j,ldc), Mpld(UC,j,ldc),
                     ldcinc, beta);
      }
      pA = pA0;
   }
   free(vC);
   return(0);
}

