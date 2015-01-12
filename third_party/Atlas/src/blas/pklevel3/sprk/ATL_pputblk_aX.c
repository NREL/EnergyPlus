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
void Mjoin(PATL,pputblk_diag)
   (const int M, const int N, const TYPE *V, const enum ATLAS_UPLO UC,
    TYPE *C, int ldc, int ldcinc, const SCALAR alpha, const SCALAR beta)
/*
 * Copies only the Upper or Lower portion of V to C
 */
{
   int i, j;

   if (UC == AtlasUpper)
   {
      if (SCALAR_IS_ZERO(beta))
      {
         if (SCALAR_IS_ONE(alpha))
         {
            for (j=0; j < N; j++)
            {
               for (i=0; i <= j; i++) C[i] = V[i];
               C += ldc;
               V += M;
               ldc += ldcinc;
            }
         }
         else if (SCALAR_IS_NONE(alpha))
         {
            for (j=0; j < N; j++)
            {
               for (i=0; i <= j; i++) C[i] = -V[i];
               C += ldc;
               V += M;
               ldc += ldcinc;
            }
         }
         else
         {
            for (j=0; j < N; j++)
            {
               for (i=0; i <= j; i++) C[i] = alpha * V[i];
               C += ldc;
               V += M;
               ldc += ldcinc;
            }
         }
      }
      else if (SCALAR_IS_ONE(beta))
      {
         if (SCALAR_IS_ONE(alpha))
         {
            for (j=0; j < N; j++)
            {
               for (i=0; i <= j; i++) C[i] += V[i];
               C += ldc;
               V += M;
               ldc += ldcinc;
            }
         }
         else if (SCALAR_IS_NONE(alpha))
         {
            for (j=0; j < N; j++)
            {
               for (i=0; i <= j; i++) C[i] -= V[i];
               C += ldc;
               V += M;
               ldc += ldcinc;
            }
         }
         else
         {
            for (j=0; j < N; j++)
            {
               for (i=0; i <= j; i++) C[i] += alpha * V[i];
               C += ldc;
               V += M;
               ldc += ldcinc;
            }
         }
      }
      else
      {
         if (SCALAR_IS_ONE(alpha))
         {
            for (j=0; j < N; j++)
            {
               for (i=0; i <= j; i++) C[i] = beta*C[i] + V[i];
               C += ldc;
               V += M;
               ldc += ldcinc;
            }
         }
         else if (SCALAR_IS_NONE(alpha))
         {
            for (j=0; j < N; j++)
            {
               for (i=0; i <= j; i++) C[i] = beta*C[i] - V[i];
               C += ldc;
               V += M;
               ldc += ldcinc;
            }
         }
         else
         {
            for (j=0; j < N; j++)
            {
               for (i=0; i <= j; i++) C[i] = beta*C[i] + alpha * V[i];
               C += ldc;
               V += M;
               ldc += ldcinc;
            }
         }
      }
   }
   else
   {
      if (SCALAR_IS_ZERO(beta))
      {
         if (SCALAR_IS_NONE(alpha))
         {
            for (j=0; j < N; j++)
            {
               ldc += ldcinc;
               for (i=j; i < M; i++) C[i] = -V[i];
               C += ldc;
               V += M;
            }
         }
         else if (SCALAR_IS_ONE(alpha))
         {
            for (j=0; j < N; j++)
            {
               ldc += ldcinc;
               for (i=j; i < M; i++) C[i] = V[i];
               C += ldc;
               V += M;
            }
         }
         else
         {
            for (j=0; j < N; j++)
            {
               ldc += ldcinc;
               for (i=j; i < M; i++) C[i] = alpha * V[i];
               C += ldc;
               V += M;
            }
         }
      }
      else if (SCALAR_IS_ONE(beta))
      {
         if (SCALAR_IS_NONE(alpha))
         {
            for (j=0; j < N; j++)
            {
               ldc += ldcinc;
               for (i=j; i < M; i++) C[i] -= V[i];
               C += ldc;
               V += M;
            }
         }
         else if (SCALAR_IS_ONE(alpha))
         {
            for (j=0; j < N; j++)
            {
               ldc += ldcinc;
               for (i=j; i < M; i++) C[i] += V[i];
               C += ldc;
               V += M;
            }
         }
         else
         {
            for (j=0; j < N; j++)
            {
               ldc += ldcinc;
               for (i=j; i < M; i++) C[i] += alpha * V[i];
               C += ldc;
               V += M;
            }
         }
      }
      else
      {
         if (SCALAR_IS_NONE(alpha))
         {
            for (j=0; j < N; j++)
            {
               ldc += ldcinc;
               for (i=j; i < M; i++) C[i] = beta*C[i] - V[i];
               C += ldc;
               V += M;
            }
         }
         else if (SCALAR_IS_ONE(alpha))
         {
            for (j=0; j < N; j++)
            {
               ldc += ldcinc;
               for (i=j; i < M; i++) C[i] = beta*C[i] + V[i];
               C += ldc;
               V += M;
            }
         }
         else
         {
            for (j=0; j < N; j++)
            {
               ldc += ldcinc;
               for (i=j; i < M; i++) C[i] = beta*C[i] + alpha * V[i];
               C += ldc;
               V += M;
            }
         }
      }
   }
}
void Mjoin(PATL,pputblk_aX)(const int M, const int N, const TYPE *V,
                            TYPE *C, int ldc, int ldcinc,
                            const SCALAR alpha, const SCALAR beta)
/*
 * Given a MxN block-major block V, copy to a packed C
 *    ldcinc =  0 : General rectangular
 *    ldcinc =  1 : Upper
 *    ldcinc = -1 : Lower
 */
{
   int i, j;

   if (!ldcinc && 0)
   {
      Mjoin(PATL,putblk_bX)(M, N, (TYPE*)V, C, ldc, beta);
      return;
   }
   ldc -= M;
   if (ldcinc == -1) ldc--;
   if (beta == 0.0)
   {
      if (alpha == 1.0)
      {
         for (j=N; j; j--)
         {
            for (i=M; i; i--) *C++ = *V++;
            C += ldc;
            ldc += ldcinc;
         }
      }
      else
      {
         for (j=N; j; j--)
         {
            for (i=M; i; i--) *C++ = alpha * *V++;
            C += ldc;
            ldc += ldcinc;
         }
      }
   }
   else if (beta == 1.0)
   {
      if (alpha == 1.0)
      {
         for (j=N; j; j--)
         {
            for (i=M; i; i--) *C++ += *V++;
            C += ldc;
            ldc += ldcinc;
         }
      }
      else
      {
         for (j=N; j; j--)
         {
            for (i=M; i; i--) *C++ += alpha * *V++;
            C += ldc;
            ldc += ldcinc;
         }
      }
   }
   else
   {
      if (alpha == 1.0)
      {
         for (j=N; j; j--)
         {
            for (i=M; i; i--, C++) *C = *C * beta + *V++;
            C += ldc;
            ldc += ldcinc;
         }
      }
      else
      {
         for (j=N; j; j--)
         {
            for (i=M; i; i--, C++) *C = *C * beta + alpha * *V++;
            C += ldc;
            ldc += ldcinc;
         }
      }
   }
}
