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

#ifdef Conj_
   #define AXPBY axpbyConj
void Mjoin(PATL,axpbyConj)
/*
 * y <- beta*y + alpha * conj(x)
 */
#else
   #define AXPBY axpby
void Mjoin(PATL,axpby)
/*
 * y <- beta*y + alpha * x
 */
#endif
   (const int N, const SCALAR alpha, const TYPE *X, const int incX,
    const SCALAR beta, TYPE *Y, const int incY)
{
#ifdef TREAL
   if (alpha == ATL_rzero)
   {
      if (beta != ATL_rzero)
         Mjoin(PATL,scal)(N, beta, Y, incY);
      else
         Mjoin(PATL,zero)(N, Y, incY);
   }
   else if (beta == ATL_rzero) Mjoin(PATL,cpsc)(N, alpha, X, incX, Y, incY);
   else if (beta == ATL_rone) Mjoin(PATL,axpy)(N, alpha, X, incX, Y, incY);
   else if (alpha == ATL_rone)
      Mjoin(PATL,axpby_a1_bX)(N, alpha, X, incX, beta, Y, incY);
   else Mjoin(PATL,axpby_aX_bX)(N, alpha, X, incX, beta, Y, incY);
#else
   const int AlphaIsReal = (alpha[1] == ATL_rzero);
   const int BetaIsReal = (beta[1] == ATL_rzero);
   const int AlphaIsOne = (AlphaIsReal && *alpha == ATL_rone);
   const int AlphaIsZero = (AlphaIsReal && *alpha == ATL_rzero);
   const int BetaIsOne = (BetaIsReal && *beta == ATL_rone);
   const int BetaIsZero = (BetaIsReal && *beta == ATL_rzero);

   if (AlphaIsZero)
   {
      if (!BetaIsZero)
         Mjoin(PATL,scal)(N, beta, Y, incY);
      else
         Mjoin(PATL,zero)(N, Y, incY);
   }
   #ifdef Conj_
      else if (BetaIsZero) Mjoin(PATL,moveConj)(N, alpha, X, incX, Y, incY);
      else if (BetaIsOne) Mjoin(PATL,axpyConj)(N, alpha, X, incX, Y, incY);
   #else
      else if (BetaIsZero) Mjoin(PATL,cpsc)(N, alpha, X, incX, Y, incY);
      else if (BetaIsOne) Mjoin(PATL,axpy)(N, alpha, X, incX, Y, incY);
   #endif
   else if (AlphaIsOne)
   {
      if (BetaIsReal)
         Mjoin(Mjoin(PATL,AXPBY),_a1_bXi0)(N, alpha, X, incX, beta, Y, incY);
      else Mjoin(Mjoin(PATL,AXPBY),_a1_bX)(N, alpha, X, incX, beta, Y, incY);
   }
   else if (AlphaIsReal)
   {
      if (BetaIsReal)
         Mjoin(Mjoin(PATL,AXPBY),_aXi0_bXi0)(N, alpha, X, incX, beta, Y, incY);
      else Mjoin(Mjoin(PATL,AXPBY),_aXi0_bX)(N, alpha, X, incX, beta, Y, incY);
   }
   else if (BetaIsReal)
      Mjoin(Mjoin(PATL,AXPBY),_aX_bXi0)(N, alpha, X, incX, beta, Y, incY);
   else Mjoin(Mjoin(PATL,AXPBY),_aX_bX)(N, alpha, X, incX, beta, Y, incY);
#endif
}
