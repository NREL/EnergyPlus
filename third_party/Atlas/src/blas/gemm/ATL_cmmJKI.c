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

#define ATL_cplxmul(out_, in1_, in2_) \
{ \
   r1 = in1_[0]; i1 = in1_[1]; \
   r2 = in2_[0]; i2 = in2_[1]; \
   out_[0] = r1*r2 - i1*i2; \
   out_[1] = r1*i2 + i1*r2; \
}

void Mjoin(PATL,mm_axpy)
   (const enum ATLAS_TRANS TA, const enum ATLAS_TRANS TB, const int M,
    const int N, const int K, const SCALAR alpha, const TYPE *A, const int lda0,
    const TYPE *B, const int ldb0, const SCALAR beta, TYPE *C, const int ldc0)
/*
 * TA == AtlasNoTrans
 * GEMM implemented by calling axpy, with any M partitioning already done
 */
{
   int i, j, k, incBk, incBn;
   const size_t lda=((size_t)lda0)+lda0, ldc=((size_t)ldc0)+ldc0, incAn = lda*K;
   const int ALONE=SCALAR_IS_ONE(alpha), BEONE=SCALAR_IS_ONE(beta);
   TYPE alph[2], BC[2];
   register TYPE r1, r2, i1, i2;

   if (TB == AtlasNoTrans)
   {
      incBk = 2;
      incBn = ldb0 - K;
   }
   else
   {
      incBk = ldb0+ldb0;
      incBn = 1 - ldb0*K;
   }
   incBn += incBn;

   if (TB == AtlasConjTrans)
   {
      if (BEONE)
      {
         if (ALONE)
         {
            for(j=0; j < N; j++)
            {
               for (k=0; k < K; k++, B += incBk, A += lda)
               {
                  BC[0] = B[0]; BC[1] = -B[1];
                  Mjoin(PATL,axpy)(M, BC, A, 1, C, 1);
               }
               C += ldc;
               B += incBn;
               A -= incAn;
            }
         }
         else
         {
            for(j=0; j < N; j++)
            {
               BC[0] = B[0]; BC[1] = -B[1];
               ATL_cplxmul(alph, alpha, BC);
               Mjoin(PATL,axpby)(M, alph, A, 1, beta, C, 1);
               B += incBk;
               A += lda;
               for (k=1; k < K; k++, B += incBk, A += lda)
               {
                  BC[0] = B[0]; BC[1] = -B[1];
                  ATL_cplxmul(alph, alpha, BC);
                  Mjoin(PATL,axpy)(M, alph, A, 1, C, 1);
               }
               C += ldc;
               B += incBn;
               A -= incAn;
            }
         }
      }
      else /* BETA != 1.0 */
      {
         if (ALONE)
         {
            for(j=0; j < N; j++)
            {
               BC[0] = B[0]; BC[1] = -B[1];
               Mjoin(PATL,axpby)(M, BC, A, 1, beta, C, 1);
               B += incBk;
               A += lda;
               for (k=1; k < K; k++, B += incBk, A += lda)
               {
                  BC[0] = B[0]; BC[1] = -B[1];
                  Mjoin(PATL,axpy)(M, BC, A, 1, C, 1);
               }
               C += ldc;
               B += incBn;
               A -= incAn;
            }
         }
         else
         {
            for(j=0; j < N; j++)
            {
               BC[0] = B[0]; BC[1] = -B[1];
               ATL_cplxmul(alph, alpha, BC);
               Mjoin(PATL,axpby)(M, alph, A, 1, beta, C, 1);
               B += incBk;
               A += lda;
               for (k=1; k < K; k++, B += incBk, A += lda)
               {
                  BC[0] = B[0]; BC[1] = -B[1];
                  ATL_cplxmul(alph, alpha, BC);
                  Mjoin(PATL,axpy)(M, alph, A, 1, C, 1);
               }
               C += ldc;
               B += incBn;
               A -= incAn;
            }
         }
      }
   }
   else /* B is not conjugated */
   {
      if (BEONE)
      {
         if (ALONE)
         {
            for(j=0; j < N; j++)
            {
               for (k=0; k < K; k++, B += incBk, A += lda)
                  Mjoin(PATL,axpy)(M, B, A, 1, C, 1);
               C += ldc;
               B += incBn;
               A -= incAn;
            }
         }
         else
         {
            for(j=0; j < N; j++)
            {
               ATL_cplxmul(alph, alpha, B);
               Mjoin(PATL,axpby)(M, alph, A, 1, beta, C, 1);
               B += incBk;
               A += lda;
               for (k=1; k < K; k++, B += incBk, A += lda)
               {
                  ATL_cplxmul(alph, alpha, B);
                  Mjoin(PATL,axpy)(M, alph, A, 1, C, 1);
               }
               C += ldc;
               B += incBn;
               A -= incAn;
            }
         }
      }
      else /* BETA != 1.0 */
      {
         if (ALONE)
         {
            for(j=0; j < N; j++)
            {
               Mjoin(PATL,axpby)(M, B, A, 1, beta, C, 1);
               B += incBk;
               A += lda;
               for (k=1; k < K; k++, B += incBk, A += lda)
                  Mjoin(PATL,axpy)(M, B, A, 1, C, 1);
               C += ldc;
               B += incBn;
               A -= incAn;
            }
         }
         else
         {
            for(j=0; j < N; j++)
            {
               ATL_cplxmul(alph, alpha, B);
               Mjoin(PATL,axpby)(M, alph, A, 1, beta, C, 1);
               B += incBk;
               A += lda;
               for (k=1; k < K; k++, B += incBk, A += lda)
               {
                  ATL_cplxmul(alph, alpha, B);
                  Mjoin(PATL,axpy)(M, alph, A, 1, C, 1);
               }
               C += ldc;
               B += incBn;
               A -= incAn;
            }
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
   const TYPE CONE[2]={ATL_rone, ATL_rzero}, CNONE[2]={ATL_rnone, ATL_rzero};
   const SCALAR alp=alpha;

/*
 * Compute M partition necessary to promote reuse in the L1 cache.  Check
 * NB^2 in addition to L1elts, to catch machines where L1 is not used by FPU.
 * If this gives a small Mp, use CacheEdge instead (reuse in L2 instead of L1).
 */
   Mp = NB*NB;
   m = ATL_L1elts >> 1;
   Mp = (m > Mp) ? m : Mp;
   Mp /= ((K+2)<<1);
   if (Mp < 128)
   {
      #if !defined(CacheEdge) || CacheEdge == 0
         Mp = M;
      #else
         Mp = (CacheEdge) / ((K+2)*ATL_sizeof);
         if (Mp < 128)
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
      alp = CONE;
      ldaa = Mp;
      pA += Mp+Mp;
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
         pA -= (Mp+Mp);
         if (TA == AtlasConjTrans)
         {
            for (k=0; k < K; k++)
            {
               Mjoin(PATL,copy)(mp, A+k+k, lda, pA+((k*ldaa)<<1), 1);
               Mjoin(PATLU,scal)(mp, ATL_rnone, pA+1+((k*ldaa)<<1), 2);
               if (!SCALAR_IS_ONE(alpha))
                  Mjoin(PATL,scal)(mp, alpha, pA+((k*ldaa)<<1), 1);
            }
         }
         else
         {
            for (k=0; k < K; k++)
               Mjoin(PATL,cpsc)(mp, alpha, A+k+k, lda, pA+((k*ldaa)<<1), 1);
         }
         A += mp*(lda+lda);
      }
      Mjoin(PATL,mm_axpy)(AtlasNoTrans, TB, mp, N, K, alp, pA, ldaa, B, ldb,
                          beta, C, ldc);
      pA += mp+mp;
      C += mp+mp;
   }
   if (vA) free(vA);
   return(0);
}
