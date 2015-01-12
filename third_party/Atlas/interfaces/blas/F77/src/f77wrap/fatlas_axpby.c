/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2001 R. Clint Whaley
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
#include "atlas_f77blas.h"
#include "atlas_f77wrap.h"
void F77axpby(const F77_INTEGER *N, const TYPE *alpha, const TYPE *X,
              const F77_INTEGER *incX, const TYPE *beta,
              TYPE *Y, const F77_INTEGER *incY)
{
   int incx=(*incX), incy=(*incY);

   if (*N > 0)
   {
      if (incx >= 0 && incy >= 0) goto L1;
      else if (incy < 0)
      {
         if (incx < 0) { incx = -incx; incy = -incy; }
         else Y -= ((*N-1)SHIFT)*incy;
      }
      else if (incx < 0) X -= ((*N-1)SHIFT)*incx;
L1:
      #ifdef TREAL
         Mjoin(PATL,axpby)(*N, *alpha, X, incx, *beta, Y, incy);
      #else
         Mjoin(PATL,axpby)(*N, alpha, X, incx, beta, Y, incy);
      #endif
   }
}
