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
#include "atlas_cacheedge.h"
void Mjoin(PATL,sprk)
   (const enum PACK_UPLO UA, const enum PACK_TRANS TA,
    const enum ATLAS_UPLO UC, const int CP,
    const int N, const int K, const SCALAR alpha,
    const TYPE *A, const int IA, const int JA, const int lda,
    const SCALAR beta, TYPE *C, const int IC, const int JC, const int ldc)
{
   const enum PACK_UPLO UC2 = ((CP) ? (enum PACK_UPLO)UC : PackGen);
   int j;
   #ifdef CacheEdge
      static const int CE_K = ((ATL_DivBySize(CacheEdge SHIFT)-(NBNB SHIFT)) /
                                           (NB*(NB+NB)))*NB;
   #else
      #define CE_K K
   #endif

   if ((!N) || ((SCALAR_IS_ZERO(alpha) || (!K)) && (SCALAR_IS_ONE(beta))))
      return;
   if (!K || SCALAR_IS_ZERO(alpha))
   {
      if (UC == CblasLower)
      {
         for (j=0; j != N; j++)
            Mjoin(PATL,scal)(N-j, beta, C+MindexP(UC2,IC+j,JC+j,ldc), 1);
      }
      else /* UC == CblasUpper */
      {
         for (j=0; j != N; j++)
            Mjoin(PATL,scal)(j+1, beta, C+MindexP(UC2,IC,JC+j,ldc), 1);
      }
      return;
   }
   Mjoin(PATL,sprk_rK)(UA, TA, UC, CP, N, K, CE_K, alpha, A, lda, beta, C, ldc);
}
