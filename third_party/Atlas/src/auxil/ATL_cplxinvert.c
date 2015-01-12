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

void Mjoin(PATL,cplxinvert)(ATL_CINT N, TYPE *X, ATL_CINT incx,
                            TYPE *Y, ATL_CINT incy)
/*
 * Y(:) = 1 / X(:)
 * Invert N complex scalars held in X, and write answer to Y.
 * X & Y can be same space
 */
{
   int i;
   const TYPE one=1.0, none=(-1.0);
   ATL_CINT incX=incx+incx, incY=incy+incy;
   register TYPE rtmp, itmp, t0;

   for (i=N; i; i--, X += incX, Y += incY)
   {
      rtmp = *X;
      itmp = X[1];
      if (Mabs(itmp) <= Mabs(rtmp))
      {
         t0 = itmp / rtmp;
         *Y = rtmp = one / (rtmp + itmp*t0);
         Y[1] = -rtmp * t0;
      }
      else
      {
         t0 = rtmp / itmp;
         Y[1] = rtmp = none / (itmp + rtmp*t0);
         *Y = -t0 * rtmp;
      }
   }
}
