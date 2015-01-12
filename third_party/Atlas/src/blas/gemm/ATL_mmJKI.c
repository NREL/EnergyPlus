/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2006 R. Clint Whaley
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

void Mjoin(PATL,mm_axpy)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB, const int M,
    const int N, const int K, const SCALAR alpha, const TYPE *A, const int lda,
    const TYPE *B, const int ldb, const SCALAR beta, TYPE *C, const int ldc)
/*
 * TA == AtlasNoTrans
 * GEMM implemented by calling axpy, with any M partitioning already done
 */
{
   int i, j, k, incBk, incBn;
   size_t incAn = lda*K;

   if (TB == AtlasNoTrans)
   {
      incBk = 1;
      incBn = ldb - K;
   }
   else
   {
      incBk = ldb;
      incBn = 1 - ldb*K;
   }

   if (beta == ATL_rone)
   {
      if (alpha == ATL_rone)
      {
         for(j=0; j < N; j++)
         {
            for (k=0; k < K; k++, B += incBk, A += lda)
               Mjoin(PATL,axpy)(M, *B, A, 1, C, 1);
            C += ldc;
            B += incBn;
            A -= incAn;
         }
      }
      else
      {
         for(j=0; j < N; j++)
         {
            Mjoin(PATL,axpby)(M, alpha*(*B), A, 1, beta, C, 1);
            B += incBk;
            A += lda;
            for (k=1; k < K; k++, B += incBk, A += lda)
               Mjoin(PATL,axpy)(M, alpha*(*B), A, 1, C, 1);
            C += ldc;
            B += incBn;
            A -= incAn;
         }
      }
   }
   else /* BETA != 1.0 */
   {
      if (alpha == ATL_rone)
      {
         for(j=0; j < N; j++)
         {
            Mjoin(PATL,axpby)(M, *B, A, 1, beta, C, 1);
            B += incBk;
            A += lda;
            for (k=1; k < K; k++, B += incBk, A += lda)
               Mjoin(PATL,axpy)(M, *B, A, 1, C, 1);
            C += ldc;
            B += incBn;
            A -= incAn;
         }
      }
      else
      {
         for(j=0; j < N; j++)
         {
            Mjoin(PATL,axpby)(M, alpha*(*B), A, 1, beta, C, 1);
            B += incBk;
            A += lda;
            for (k=1; k < K; k++, B += incBk, A += lda)
               Mjoin(PATL,axpy)(M, alpha*(*B), A, 1, C, 1);
            C += ldc;
            B += incBn;
            A -= incAn;
         }
      }
   }
}

int Mjoin(PATL,mmJKI)(const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB,
                      const int M, const int N, const int K,
                      const SCALAR alpha, const TYPE *A, const int lda,
                      const TYPE *B, const int ldb, const SCALAR beta,
                      TYPE *C, const int ldc)
/*
 * This gemm is for small K, so we build gemm out of AXPY (outer product)
 * rather than dot (inner product).
 */
{
   int Mp, mp, m, k, ldaa=lda;
   void *vA=NULL;
   TYPE *pA;
   SCALAR alp=alpha;

/*
 * Compute M partition necessary to promote reuse in the L1 cache.  Check
 * NB^2 in addition to L1elts, to catch machines where L1 is not used by FPU.
 * If this gives a small Mp, use CacheEdge instead (reuse in L2 instead of L1).
 */
   Mp = NB*NB;
   if (ATL_L1elts > Mp) Mp = ATL_L1elts;
   Mp /= (K+2);
   if (Mp < 256)
   {
      #if !defined(CacheEdge) || CacheEdge == 0
         Mp = M;
      #else
         Mp = (CacheEdge) / ((K+2)*ATL_sizeof);
         if (Mp < 256)
            Mp = M;
      #endif
   }
   if (Mp > M)
      Mp = M;
/*
 * Change Mp if remainder is very small
 */
   else
   {
      Mp -= 16;      /* small safety margin on filling cache */
      mp = M / Mp;
      m = M - mp*Mp;
      if (m && m < 32)
         Mp += (m+mp-1)/mp;
   }
/*
 * If A not in NoTrans format, need to copy so it can use axpy wt stride=1.
 * NOTE: this routine should not be called when you can't afford this copy
 */
   if (TA != AtlasNoTrans)
   {
      vA = malloc(ATL_Cachelen + Mp*ATL_MulBySize(K));
      if (!vA) return(-1);
      pA = ATL_AlignPtr(vA);
      alp = ATL_rone;
      ldaa = Mp;
      pA += Mp;
   }
   else
      pA = (TYPE *) A;
   for (m=0; m < M; m += Mp)
   {
      mp = M - m;
      if (mp > Mp)
         mp = Mp;
/*
 *    If the thing is in Trans format, copy to NoTrans
 */
      if (vA)
      {
         pA -= Mp;
         for (k=0; k < K; k++)
            Mjoin(PATL,cpsc)(mp, alpha, A+k, lda, pA+k*ldaa, 1);
         A += mp*lda;
      }
      Mjoin(PATL,mm_axpy)(AtlasNoTrans, TB, mp, N, K, alp, pA, ldaa, B, ldb,
                          beta, C, ldc);
      pA += mp;
      C += mp;
   }
   if (vA) free(vA);
   return(0);
}

