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
void Mjoin(PATL,moveConj)
/*
 * y <- alpha * conj(x)
 */
#else
void Mjoin(PATL,move)
/*
 * y <- alpha * x
 */
#endif
   (const int N, const SCALAR alpha, const TYPE *X, const int incX,
    TYPE *Y, const int incY)
{
   #ifdef TREAL
      int i;
      if (alpha == ATL_rzero) Mjoin(PATL,zero)(N, Y, incY);
      else if (alpha == ATL_rone) Mjoin(PATL,copy)(N, X, incX, Y, incY);
      else if (incX == 1 && incY == 1)
         for (i=0; i != N; i++) Y[i] = alpha * X[i];
      else for (i=N; i; i--, Y += incY, X += incX) *Y = alpha * *X;
   #else
      int i;
      const int M = N<<1, incx = incX<<1, incy = incY<<1;
      const register TYPE ralpha = *alpha, ialpha = alpha[1];
      #ifdef Conj_
         const register TYPE conjal=-ralpha;
      #else
         #define conjal ralpha
      #endif
      register TYPE rx, ix;
      if (ialpha == ATL_rzero)
      {
         #ifndef Conj_
            if (incX == 1 && incY == 1)
               Mjoin(PATLU,move)(M, ralpha, X, 1, Y, 1);
            else
         #endif
         {
            for (i=N; i; i--, Y += incy, X += incx)
            {
               *Y = ralpha * *X;
               Y[1] = conjal * X[1];
            }
         }
      }
      else
      {
         for (i=N; i; i--, Y += incy, X += incx)
         {
            rx = *X;
            ix = X[1];
            #ifdef Conj_
               *Y = ralpha * rx + ialpha * ix;
               Y[1] = ialpha * rx - ralpha * ix;
            #else
               *Y = ralpha * rx - ialpha * ix;
               Y[1] = ralpha * ix + ialpha * rx;
            #endif
         }
      }
   #endif
}
