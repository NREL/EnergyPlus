/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 1999 R. Clint Whaley
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

void Mjoin(PATL,geadd)
   (const int M, const int N, const SCALAR alpha, const TYPE *A, const int lda,
    const SCALAR beta, TYPE *C, const int ldc)
/*
 * C <- alpha*A + beta*C
 */
{
#ifdef TREAL
   if (beta == ATL_rzero) Mjoin(PATL,gemove)(M, N, alpha, A, lda, C, ldc);
   else if (alpha == ATL_rzero) Mjoin(PATL,gescal)(M, N, beta, C, ldc);
   else if (alpha == ATL_rone)
   {
      if (beta == ATL_rone)
         Mjoin(PATL,geadd_a1_b1)(M, N, alpha, A, lda, beta, C, ldc);
      else if (beta == ATL_rzero)
         Mjoin(PATL,geadd_a1_b0)(M, N, alpha, A, lda, beta, C, ldc);
      else Mjoin(PATL,geadd_a1_bX)(M, N, alpha, A, lda, beta, C, ldc);
   }
   else
   {
      if (beta == ATL_rone)
         Mjoin(PATL,geadd_aX_b1)(M, N, alpha, A, lda, beta, C, ldc);
      else if (beta == ATL_rzero)
         Mjoin(PATL,geadd_aX_b0)(M, N, alpha, A, lda, beta, C, ldc);
      else Mjoin(PATL,geadd_aX_bX)(M, N, alpha, A, lda, beta, C, ldc);
   }
#else
   const int AlphaIsReal = (alpha[1] == ATL_rzero);
   const int AlphaIsOne = (AlphaIsReal && *alpha == ATL_rone);
   const int AlphaIsZero = (AlphaIsReal && *alpha == ATL_rzero);
   const int BetaIsReal = (beta[1] == ATL_rzero);
   const int BetaIsOne = (BetaIsReal && *beta == ATL_rone);
   const int BetaIsZero = (BetaIsReal && *beta == ATL_rzero);

   if (BetaIsZero) Mjoin(PATL,gemove)(M, N, alpha, A, lda, C, ldc);
   else if (AlphaIsZero) Mjoin(PATL,gescal)(M, N, beta, C, ldc);
   else if (AlphaIsOne)
   {
      if (BetaIsOne)
         Mjoin(PATL,geadd_a1_b1)(M, N, alpha, A, lda, beta, C, ldc);
      else if (BetaIsZero)
         Mjoin(PATL,geadd_a1_b0)(M, N, alpha, A, lda, beta, C, ldc);
      else if (BetaIsReal)
         Mjoin(PATL,geadd_a1_bXi0)(M, N, alpha, A, lda, beta, C, ldc);
      else Mjoin(PATL,geadd_a1_bX)(M, N, alpha, A, lda, beta, C, ldc);
   }
   else if (AlphaIsReal)
   {
      if (BetaIsOne)
         Mjoin(PATL,geadd_aXi0_b1)(M, N, alpha, A, lda, beta, C, ldc);
      else if (BetaIsZero)
         Mjoin(PATL,geadd_aXi0_b0)(M, N, alpha, A, lda, beta, C, ldc);
      else if (BetaIsReal)
         Mjoin(PATL,geadd_aXi0_bXi0)(M, N, alpha, A, lda, beta, C, ldc);
      else Mjoin(PATL,geadd_aXi0_bX)(M, N, alpha, A, lda, beta, C, ldc);
   }
   else
   {
      if (BetaIsOne)
         Mjoin(PATL,geadd_aX_b1)(M, N, alpha, A, lda, beta, C, ldc);
      else if (BetaIsZero)
         Mjoin(PATL,geadd_aX_b0)(M, N, alpha, A, lda, beta, C, ldc);
      else if (BetaIsReal)
         Mjoin(PATL,geadd_aX_bXi0)(M, N, alpha, A, lda, beta, C, ldc);
      else Mjoin(PATL,geadd_aX_bX)(M, N, alpha, A, lda, beta, C, ldc);
   }
#endif
}
