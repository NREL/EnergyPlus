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
void Mjoin(PATL,scalConj)
/*
 * X <- alpha * conj(X)
 */
#else
void Mjoin(PATL,scal)
/*
 * X <- alpha * X
 */
#endif
   (const int N, const SCALAR alpha, TYPE *X, const int incX)
{
   int i;
   #ifdef TREAL
      if (alpha != ATL_rzero)
      {
         if (incX == 1) for (i=0; i != N; i++) X[i] *= alpha;
         else for (i=N; i; i--, X += incX) *X *= alpha;
      }
      else Mjoin(PATL,zero)(N, X, incX);
   #else
      const register TYPE ralpha = *alpha, ialpha = alpha[1];
      #ifdef Conj_
         const register TYPE conjal = -ralpha;
      #else
         #define conjal ralpha
      #endif
      register TYPE rtmp, itmp;
      int incx = incX<<1;

      if (ialpha == ATL_rzero)
      {
         if (ralpha != ATL_rzero)
         {
      #ifndef Conj_
            if (incX == 1) Mjoin(PATLU,scal)(N<<1, ralpha, X, incX);
            else
      #endif
            {
               for (i=N; i; i--, X += incx)
               {
                  *X *= ralpha;
                  X[1] *= conjal;
               }
            }
         }
         else Mjoin(PATL,zero)(N, X, incX);
      }
      else
      {
         if (incX == 1)
         {
            for (i=N; i; i--, X += 2)
            {
               rtmp = X[0];
               itmp = X[1];
               #ifdef Conj_
                  X[0] = rtmp * ralpha + itmp * ialpha;
                  X[1] = rtmp * ialpha - itmp * ralpha;
               #else
                  X[0] = rtmp * ralpha - itmp * ialpha;
                  X[1] = rtmp * ialpha + itmp * ralpha;
               #endif
            }
         }
         else
         {
            for (i=N; i; i--, X += incx)
            {
               rtmp = X[0];
               itmp = X[1];
               #ifdef Conj_
                  X[0] = rtmp * ralpha + itmp * ialpha;
                  X[1] = rtmp * ialpha - itmp * ralpha;
               #else
                  X[0] = rtmp * ralpha - itmp * ialpha;
                  X[1] = rtmp * ialpha + itmp * ralpha;
               #endif
            }
         }
      }
   #endif
}
