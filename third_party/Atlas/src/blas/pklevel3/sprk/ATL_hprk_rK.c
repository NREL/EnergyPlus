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

static void ATL_rk_recUN
   (const enum PACK_UPLO UA, const enum PACK_TRANS TA,
    const enum ATLAS_UPLO UC, const int CP,
    const int N, const int K, const SCALAR alpha,
    const TYPE *A, int lda, const SCALAR beta, TYPE *C, const int ldc)
/*
 * For upper notrans matrix, use recursion to reduce N until enough memory
 * can be allocated
 */
{
   int Nright, Nleft;
   const enum PACK_UPLO UC2 = (CP ? (enum PACK_UPLO)UC : PackGen);
   if (Mjoin(PATL,phk_kmm)(UC, UA, TA, N, K, alpha, A, lda, beta, CP, C, ldc))
   {
      Nleft = N >> 1;
      #ifdef NB
         if (Nleft > NB) Nleft = ATL_MulByNB(ATL_DivByNB(Nleft));
      #endif
      Nright = N - Nleft;
      ATL_rk_recUN(UA, TA, UC, CP, Nleft, K, alpha, A, lda, beta, C, ldc);
      Mjoin(PATL,gpmm)(PackGen, TA, PackGen, AtlasConjTrans, CP ? PackUpper:PackGen,
                       Nleft, Nright, K, alpha, A, 0, 0, lda,
                       A+(Nleft SHIFT), 0, 0, lda, beta,
                       C+MindexP(UC2,0,Nleft,ldc), 0, 0, Mpld(UC2,Nleft,ldc));
      ATL_rk_recUN(UA, TA, UC, CP, Nright, K, alpha, A+(Nleft SHIFT), lda,
                  beta, C+MindexP(UC2,Nleft,Nleft,ldc), Mpld(UC2,Nleft,ldc));
   }
}

static void ATL_rk_recUT
   (const enum PACK_UPLO UA, const enum PACK_TRANS TA,
    const enum ATLAS_UPLO UC, const int CP,
    const int N, const int K, const SCALAR alpha,
    const TYPE *A, int lda, const SCALAR beta, TYPE *C, const int ldc)
/*
 * For upper trans matrix, use recursion to reduce N until enough memory
 * can be allocated
 */
{
   int Nright, Nleft;
   const enum PACK_UPLO UC2 = (CP ? (enum PACK_UPLO)UC : PackGen);
   if (Mjoin(PATL,phk_kmm)(UC, UA, TA, N, K, alpha, A, lda, beta, CP, C, ldc))
   {
      Nleft = N >> 1;
      #ifdef NB
         if (Nleft > NB) Nleft = ATL_MulByNB(ATL_DivByNB(Nleft));
      #endif
      Nright = N - Nleft;
      ATL_rk_recUT(UA, TA, UC, CP, Nleft, K, alpha, A, lda, beta, C, ldc);
      Mjoin(PATL,gpmm)(PackGen, TA, PackGen, AtlasNoTrans, CP?PackUpper:PackGen,
                       Nleft, Nright, K, alpha, A, 0, 0, lda,
                       A+Nleft*(lda SHIFT), 0, 0, lda, beta,
                       C+MindexP(UC2,0,Nleft,ldc), 0, 0, Mpld(UC2,Nleft,ldc));
      ATL_rk_recUT(UA, TA, UC, CP, Nright, K, alpha, A+(Nleft SHIFT)*lda, lda,
                  beta, C+MindexP(UC2,Nleft,Nleft,ldc), Mpld(UC2,Nleft,ldc));
   }
}

static void ATL_rk_recLN
   (const enum PACK_UPLO UA, const enum PACK_TRANS TA,
    const enum ATLAS_UPLO UC, const int CP,
    const int N, const int K, const SCALAR alpha,
    const TYPE *A, int lda, const SCALAR beta, TYPE *C, const int ldc)
/*
 * For lower notrans matrix, use recursion to reduce N until enough memory
 * can be allocated
 */
{
   int Nright, Nleft;
   const enum PACK_UPLO UC2 = (CP ? (enum PACK_UPLO)UC : PackGen);
   if (Mjoin(PATL,phk_kmm)(UC, UA, TA, N, K, alpha, A, lda, beta, CP, C, ldc))
   {
      Nleft = N >> 1;
      #ifdef NB
         if (Nleft > NB) Nleft = ATL_MulByNB(ATL_DivByNB(Nleft));
      #endif
      Nright = N - Nleft;
      ATL_rk_recLN(UA, TA, UC, CP, Nleft, K, alpha, A, lda, beta, C, ldc);
      Mjoin(PATL,gpmm)(PackGen, TA, PackGen, AtlasConjTrans, CP ? PackLower:PackGen,
                       Nright, Nleft, K, alpha, A+(Nleft SHIFT), 0, 0, lda,
                       A, 0, 0, lda, beta,
                       C+MindexP(UC2,Nleft,0,ldc), 0, 0, ldc);
      ATL_rk_recLN(UA, TA, UC, CP, Nright, K, alpha, A+(Nleft SHIFT), lda,
                   beta, C+MindexP(UC2,Nleft,Nleft,ldc), Mpld(UC2,Nleft,ldc));
   }
}
static void ATL_rk_recLT
   (const enum PACK_UPLO UA, const enum PACK_TRANS TA,
    const enum ATLAS_UPLO UC, const int CP,
    const int N, const int K, const SCALAR alpha,
    const TYPE *A, int lda, const SCALAR beta, TYPE *C, const int ldc)
/*
 * For lower notrans matrix, use recursion to reduce N until enough memory
 * can be allocated
 */
{
   int Nright, Nleft;
   const enum PACK_UPLO UC2 = (CP ? (enum PACK_UPLO)UC : PackGen);
   if (Mjoin(PATL,phk_kmm)(UC, UA, TA, N, K, alpha, A, lda, beta, CP, C, ldc))
   {
      Nleft = N >> 1;
      #ifdef NB
         if (Nleft > NB) Nleft = ATL_MulByNB(ATL_DivByNB(Nleft));
      #endif
      Nright = N - Nleft;
      ATL_rk_recLT(UA, TA, UC, CP, Nleft, K, alpha, A, lda, beta, C, ldc);
      Mjoin(PATL,gpmm)(PackGen, TA, PackGen, AtlasNoTrans, CP?PackLower:PackGen,
                       Nright, Nleft, K, alpha, A+lda*(Nleft SHIFT), 0, 0, lda,
                       A, 0, 0, lda, beta,
                       C+MindexP(UC2,Nleft,0,ldc), 0, 0, ldc);
      ATL_rk_recLT(UA, TA, UC, CP, Nright, K, alpha, A+lda*(Nleft SHIFT), lda,
                   beta, C+MindexP(UC2,Nleft,Nleft,ldc), Mpld(UC2,Nleft,ldc));
   }
}

void Mjoin(PATL,hprk_rK)
   (const enum PACK_UPLO UA, const enum PACK_TRANS TA,
    const enum ATLAS_UPLO UC, const int CP,
    const int N, const int K, int R, const SCALAR alpha,
    const TYPE *A, int lda, const SCALAR beta0, TYPE *C, const int ldc)
/*
 * This routine does the packed symmetric rank-K update by doing ceil(K/R)
 * rank-R updates of C.  This primarily done for CacheEdge, but is also
 * useful to auto-reduce R until enough workspace may be allocated.
 */
{
   const enum PACK_UPLO UC2 = ((CP) ? (enum PACK_UPLO)UC : PackGen);
   int k=0, kb, ierr;
   const int ldainc = (UA == AtlasUpper) ? 1 : ((UA == AtlasLower) ? -1 : 0);
   const int ldcinc = (UC2 == AtlasUpper) ? 1 : ((UC2 == AtlasLower) ? -1 : 0);
   #ifdef TREAL
      TYPE beta = beta0;
   #else
      TYPE beta[2];
      *beta = *beta0;
      beta[1] = beta0[1];
   #endif

   if (R < NB) R = NB<<4;
   if ((K - R) < 2*NB) R = K;
   do
   {
      kb = K - k;
      if (kb - R < 2*NB) R = kb;
      kb = Mmin(R, kb);
/*
 *    If we can't do the rank-R update, reduce R until we can, or R = 1
 */
      ierr = Mjoin(PATL,phk_kmm)(UC, UA, TA, N, kb, alpha,
                                 A, lda, beta, CP, C, ldc);
      if (ierr && R <= NB*8)
      {
         if (UC == AtlasUpper)
         {
            if (TA == AtlasNoTrans)
               ATL_rk_recUN(UA, TA, UC, CP, N, kb, alpha, A, lda, beta, C, ldc);
            else
               ATL_rk_recUT(UA, TA, UC, CP, N, kb, alpha, A, lda, beta, C, ldc);
         }
         else
         {
            if (TA == AtlasNoTrans)
               ATL_rk_recLN(UA, TA, UC, CP, N, kb, alpha, A, lda, beta, C, ldc);
            else
               ATL_rk_recLT(UA, TA, UC, CP, N, kb, alpha, A, lda, beta, C, ldc);
         }
         ierr = 0;
      }
      if (ierr)
      {
         R = Mmin(NB*8, R>>1);
         ATL_assert(R);
      }
/*
 *    Subsequent updates use beta = 1
 */
      else
      {
         #ifdef TREAL
            beta = ATL_rone;
         #else
            *beta = ATL_rone;
            beta[1] = ATL_rzero;
         #endif
         if (TA == AtlasNoTrans)
         {
            A += MindexP(UA, 0, R, lda);
            lda = Mpld(UA, R, lda);
         }
         else A += MindexP(UA, R, 0, lda);
         k += R;
      }
   }
   while(k < K);
}
