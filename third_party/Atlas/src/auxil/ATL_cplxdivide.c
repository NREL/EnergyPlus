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

void Mjoin(PATL,cplxdivide)
   (ATL_CINT N, TYPE *beta, TYPE *X, ATL_CINT incx, TYPE *Y, ATL_CINT incy)
/*
 * Y(:) = X(:)/beta, wt division done with safe complex arithmetic.
 * It is OK for Y & X to be the same pointer
 * This code is straight adaptation of LAPACK's DLADIV, which comes from
 * the algorithm developed by Robert L. Smith (Art of Comp Prog, Vol.2 p.195)
 */
{
   ATL_CINT incY=incy+incy, incX = incx+incx;
   ATL_INT i;
   const register TYPE rb = beta[0], ib = beta[1];
   register TYPE rx, ix, e, f;

   if (Mabs(ib) < Mabs(rb))
   {
      e = ib / rb;
      f = rb + ib*e;
      for (i=N; i; i--, X += incX, Y += incY)
      {
         rx = *X; ix = X[1];
         Y[0] = (rx + ix*e) / f;
         Y[1] = (ix - rx*e) / f;
      }
   }
   else
   {
      e = rb / ib;
      f = ib + rb*e;
      for (i=N; i; i--, X += incX, Y += incY)
      {
         rx = *X; ix = X[1];
         Y[0] = (ix + rx*e) / f;
         Y[1] = (ix*e - rx) / f;
      }
   }
}
