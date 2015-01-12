/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2010 R. Clint Whaley
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
#include "atlas_lvl2.h"
#include "atlas_lvl3.h"
#include Mstr(Mjoin(Mjoin(atlas_,PRE),sysinfo.h))
#include "atlas_cacheedge.h"
/*
 * If I don't believe CacheEdge setting (or not set), set L2 size to 4*L1
 */
#ifdef CacheEdge
   #if CacheEdge > 4194304 || CacheEdge == 0
      #define MY_CE (4*ATL_MulBySize(ATL_L1elts))
   #else
      #define MY_CE CacheEdge
   #endif
#else
   #define MY_CE (4*ATL_MulBySize(ATL_L1elts))
#endif

void Mjoin(PATL,gemv)
   (const enum ATLAS_TRANS TA, ATL_CINT M, ATL_CINT N, const SCALAR alpha,
    const TYPE *A, ATL_CINT lda, const TYPE *X, ATL_CINT incX,
    const SCALAR beta, TYPE *Y, ATL_CINT incY)
/*
 * ATL_gemv is a wrapper that chooses which context-tuned GEMV to call.
 * Supported contexts are tuned for in-L1 performance (_L1), tuned for
 * in-L2 performance (_L2), and tuned for out-of-cache (no suffix).
 * Right now, we do this based sheerly on operand size, since this matches
 * most common (we think!) LAPACK usage, like in factorizations.  If we
 * had access to the hardware counters, a better way to would be to access
 * say 10/100 elements of A, and call one of these contexts based on how
 * many cache misses were generated
 */
{
   const size_t opsize = (M*N+M+N)*sizeof(TYPE)SHIFT, Abeg, Aend, Obeg, Oend;
   int OL;

   if (TA == AtlasNoTrans)
   {
      if (opsize > MY_CE)
        Mjoin(PATL,gemvN)(M, N, alpha, A, lda, X, incX, beta, Y, incY);
      else if (opsize > ATL_MulBySize(ATL_L1elts))
        Mjoin(PATL,gemvN_L2)(M, N, alpha, A, lda, X, incX, beta, Y, incY);
      else
        Mjoin(PATL,gemvN_L1)(M, N, alpha, A, lda, X, incX, beta, Y, incY);
   }
   else
   {
   #ifdef TCPLX
      if (TA == AtlasTrans)
      {
   #endif
      if (opsize > MY_CE)
         Mjoin(PATL,gemvT)(M, N, alpha, A, lda, X, incX, beta, Y, incY);
      else if (opsize > ATL_MulBySize(ATL_L1elts))
         Mjoin(PATL,gemvT_L2)(M, N, alpha, A, lda, X, incX, beta, Y, incY);
      else
         Mjoin(PATL,gemvT_L1)(M, N, alpha, A, lda, X, incX, beta, Y, incY);
   #ifdef TCPLX
      }
      else if (TA == AtlasConjTrans)
      {
         if (opsize > MY_CE)
            Mjoin(PATL,gemvCT)(M, N, alpha, A, lda, X, incX, beta, Y, incY);
         else if (opsize > ATL_MulBySize(ATL_L1elts))
            Mjoin(PATL,gemvCT_L2)(M, N, alpha, A, lda, X, incX, beta, Y, incY);
         else
            Mjoin(PATL,gemvCT_L1)(M, N, alpha, A, lda, X, incX, beta, Y, incY);
      }
      else /* if (TA == AtlasConj) */
      {
         if (opsize > MY_CE)
            Mjoin(PATL,gemvCN)(M, N, alpha, A, lda, X, incX, beta, Y, incY);
         else if (opsize > ATL_MulBySize(ATL_L1elts))
            Mjoin(PATL,gemvCN_L2)(M, N, alpha, A, lda, X, incX, beta, Y, incY);
         else
            Mjoin(PATL,gemvCN_L1)(M, N, alpha, A, lda, X, incX, beta, Y, incY);
      }
   #endif
   }
}
