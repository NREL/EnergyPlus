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
#include "atlas_level1.h"

#ifdef TREAL

void Mjoin(PATL,rot)(const int N, TYPE *X, const int incX,
                     TYPE *Y, const int incY, const TYPE c, const TYPE s)
{
   int i;
   TYPE tmp;

   if (c != ATL_rone || s != ATL_rzero)
   {
      if (incX == 1 && incY == 1)
      {
         for (i=0; i != N; i++)
         {
            tmp = c * X[i] + s * Y[i];
            Y[i] = c * Y[i] - s * X[i];
            X[i] = tmp;
         }
      }
      else
      {
         for (i=N; i; i--, Y += incY, X += incX)
         {
            tmp = c * *X + s * *Y;
            *Y = c * *Y - s * *X;
            *X = tmp;
         }
      }
   }
}

#else /* complex routine */
void Mjoin(Mjoin(PATL,UPR),rot)
   (const int N, TYPE *X, const int incx, TYPE *Y, const int incy,
    const TYPE c0, const TYPE s0)
{
   const register TYPE c = c0, s = s0;
   register TYPE rx, ix, ry, iy;
   const int incX = incx<<1, incY = incy<<1;
   int i;

   if (N > 0)
   {
      if (incx == 1 && incy == 1)
      {
         for (i=N; i; i--, X += 2, Y += 2)  /* maybe compiler unrolls */
         {
            rx = *X;  ix = X[1];
            ry = *Y;  iy = Y[1];
            *X   = c * rx + s * ry;
            X[1] = c * ix + s * iy;
            *Y   = c * ry - s * rx;
            Y[1] = c * iy - s * ix;
         }
      }
      else
      {
         for (i=N; i; i--, X += incX, Y += incY)  /* maybe compiler unrolls */
         {
            rx = *X;  ix = X[1];
            ry = *Y;  iy = Y[1];
            *X   = c * rx + s * ry;
            X[1] = c * ix + s * iy;
            *Y   = c * ry - s * rx;
            Y[1] = c * iy - s * ix;
         }
      }
   }
}
#endif
