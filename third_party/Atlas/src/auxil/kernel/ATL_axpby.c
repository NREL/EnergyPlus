/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2009, 1999 R. Clint Whaley
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
void Mjoin(Mjoin(Mjoin(PATL,axpbyConj),NM),BNM)
/*
 * y = beta * y + alpha * conj(x)
 */
#else
void Mjoin(Mjoin(Mjoin(PATL,axpby),NM),BNM)
/*
 * y = beta*y + alpha*x
 */
#endif
   (ATL_CINT N, const SCALAR alpha, const TYPE *X, ATL_CINT incX,
    const SCALAR beta, TYPE *Y, ATL_CINT incY)
{
   #ifdef ALPHA0
      #if defined(BETA0)
         Mjoin(PATL,zero)(N, Y, incY);
      #elif defined(BETA1)
         #ifndef Conj_
            return;
         #else
            Mjoin(PATLU,scal)(N<<1, ATL_rnone, Y, incY<<1);
         #endif
      #else
         #ifndef Conj_
            Mjoin(PATL,scal)(N, beta, Y, incY);
         #else
            Mjoin(PATL,scalConj)(N, beta, Y, incY);
         #endif
      #endif
   #elif defined(BETA0)
      #ifdef ALPHA1
         #ifndef Conj_
            Mjoin(PATL,copy)(N, X, incX, Y, incY);
         #else
            Mjoin(PATL,copyConj)(N, X, incX, Y, incY);
         #endif
      #else
         #ifndef Conj_
            Mjoin(PATL,cpsc)(N, alpha, X, incX, Y, incY);
         #else
            Mjoin(PATL,moveConj)(N, alpha, X, incX, Y, incY);
         #endif
      #endif
   #elif defined(BETA1)
      #ifndef Conj_
         Mjoin(PATL,axpy)(N, alpha, X, incX, Y, incY);
      #else
         Mjoin(PATL,axpyConj)(N, alpha, X, incX, Y, incY);
      #endif
   #elif defined(TREAL)
      int i;
      if (incX == 1 && incY == 1)
         for (i=0; i != N; i++)
            Y[i] = ATL_MulByBETA(Y[i]) + ATL_MulByALPHA(X[i]);
      else
         for (i=N; i; i--, X += incX, Y += incY)
            *Y = ATL_MulByBETA(*Y) + ATL_MulByALPHA(*X);
   #else
      ATL_CINT incx = incX<<1, incy = incY<<1;
      ATL_INT i;
      const register TYPE ralpha = *alpha, ialpha = alpha[1];
      const register TYPE rbeta = *beta, ibeta = beta[1];
      register TYPE rx, ix, ry, iy, t0;
      for (i=N; i; i--, X += incx, Y += incy)
      {
         rx = *X;
         #ifndef Conj_
            ix = X[1];
         #else
            ix = -X[1];
         #endif
         ry = *Y;
         iy = Y[1];

         #ifndef ALPHA1
            #ifdef ALPHAXI0
               rx *= ralpha;
               ix *= ralpha;
            #else
               t0 = rx;
               rx = rx * ralpha - ix * ialpha;
               ix = t0 * ialpha + ix * ralpha;
            #endif
         #endif
         #ifdef BETAXI0
            ry *= rbeta;
            iy *= rbeta;
         #else
            t0 = ry;
            ry = ry * rbeta - iy * ibeta;
            iy = t0 * ibeta + iy * rbeta;
         #endif

         *Y = ry + rx;
         Y[1] = iy + ix;
      }
   #endif
}
