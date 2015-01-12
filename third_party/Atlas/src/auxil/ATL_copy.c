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
void Mjoin(PATL,copyConj)
/*
 * Y <- conj(X)
 */
#else
void Mjoin(PATL,copy)
/*
 * Y <- X
 */
#endif
   (const int N, const TYPE *X, const int incX, TYPE *Y, const int incY)
{
   #ifdef TREAL
      #define M N
   #else
      const int M = N<<1, incx = incX<<1, incy = incY<<1;
   #endif
   int i;

   #ifndef Conj_
      if (incY == 1 && incX == 1) for (i=0; i != M; i++) Y[i] = X[i];
      else
   #endif
   #ifdef TREAL
         for (i=N; i; i--, X += incX, Y += incY) *Y = *X;
   #else
      {
         for (i=N; i; i--, X += incx, Y += incy)
         {
            *Y = *X;
            #ifdef Conj_
               Y[1] = -X[1];
            #else
               Y[1] = X[1];
            #endif
         }
      }
   #endif
}
