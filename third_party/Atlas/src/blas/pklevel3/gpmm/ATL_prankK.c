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

void Mjoin(PATL,prankK)
   (const enum PACK_UPLO UA, const enum ATLAS_TRANS TA,
    const enum PACK_UPLO UB, const enum ATLAS_TRANS TB,
    const int M, const int N, const int K, int R, const SCALAR alpha,
    const TYPE *A, int lda, const TYPE *B, int ldb,
    const SCALAR beta0, const enum PACK_UPLO UC, TYPE *C, int ldc)
/*
 * This routine performs a packed matrix multiply by doing ceil(K/R)
 * rank-R updates of C.  This is primarily done for CacheEdge, but
 * is also useful to auto-reduce R until enough workspace may be allocated.
 */
{
   int k=0, kb;
   const int ldainc = (UA == AtlasUpper) ? 1 : ((UA == AtlasLower) ? -1 : 0);
   const int ldbinc = (UB == AtlasUpper) ? 1 : ((UB == AtlasLower) ? -1 : 0);
   const int ldcinc = (UC == AtlasUpper) ? 1 : ((UC == AtlasLower) ? -1 : 0);
   #ifdef TCPLX
      TYPE beta[2];
   #else
      TYPE beta = beta0;
   #endif
   int (*ATL_mm)(const enum PACK_UPLO UA, const enum ATLAS_TRANS TA,
                 const enum PACK_UPLO UB, const enum ATLAS_TRANS TB,
                 const int M, const int N, const int K, const SCALAR alpha,
                 const TYPE *A, const int lda, const TYPE *B, const int ldb,
                 const SCALAR beta, const enum PACK_UPLO UC,
                 TYPE *C, const int ldc);

   #ifdef TCPLX
      beta[0] = *beta0;
      beta[1] = beta0[1];
   #endif
   ATL_mm = Mjoin(PATL,pmmJIKF);
   R = Mmin(R, K);
   do
   {
      kb = K - k;
      kb = Mmin(R, kb);
/*
 *    If we can't do the rank-R update, reduce R and/or use a lower-memory
 *    algorithm until we can
 */
      if (ATL_mm(UA, TA, UB, TB, M, N, kb, alpha, A, lda, B, ldb, beta,
                 UC, C, ldc))
      {
         kb = (R>>1);
         kb = ATL_DivByNB(kb);
         if (kb) R = ATL_MulByNB(kb);
         else
         {
            if (ATL_mm != Mjoin(PATL,pmmJIK)) ATL_mm = Mjoin(PATL,pmmJIK);
            else
            {
               ATL_assert(R > 1);
               R = (R>>1);
            }
         }
      }
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
         if (TB == AtlasNoTrans) B += MindexP(UB, R, 0, ldb);
         else
         {
            B += MindexP(UB, 0, R, ldb);
            ldb = Mpld(UB, R, ldb);
         }
         k += R;
      }
   }
   while(k < K);
}
