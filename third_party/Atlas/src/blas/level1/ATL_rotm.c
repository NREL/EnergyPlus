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

void Mjoin(PATL,rotm)(const int N, TYPE *X, const int incX,
                      TYPE *Y, const int incY, const TYPE *P)
{
   int i;
   const TYPE flag = *P;
   TYPE h11, h21, h12, h22, w, z;

   if (N <= 0 || flag == -2.0) return;
   if (flag == ATL_rnone)
   {
      h11=P[1]; h21=P[2]; h12=P[3]; h22=P[4];
      if (incX == 1 && incY == 1)
      {
         for (i=N; i; i--)
         {
            w = *X;
            z = *Y;
            *X++ = w * h11 + z * h12;
            *Y++ = w * h21 + z * h22;
         }
      }
      else
      {
         for (i=N; i; i--)
         {
            w = *X;
            z = *Y;
            *X = w * h11 + z * h12;
            *Y = w * h21 + z * h22;
            X += incX;
            Y += incY;
         }
      }
   }
   else if (flag == ATL_rzero)
   {
      h21=P[2];
      h12=P[3];
      if (incX == 1 && incY == 1)
      {
         for (i=N; i; i--)
         {
            w = *X;
            z = *Y;
            *X++ = w + z * h12;
            *Y++ = w * h21 + z;
         }
      }
      else
      {
         for (i=N; i; i--)
         {
            w = *X;
            z = *Y;
            *X = w + z * h12;
            *Y = w * h21 + z;
            X += incX;
            Y += incY;
         }
      }
   }
   else if (flag == ATL_rone)
   {
      h11=P[1];
      h22=P[4];
      if (incX == 1 && incY == 1)
      {
         for (i=N; i; i--)
         {
            w = *X;
            z = *Y;
            *X++ = w * h11 + z;
            *Y++ = z * h22 - w;
         }
      }
      else
      {
         for (i=N; i; i--)
         {
            w = *X;
            z = *Y;
            *X = w * h11 + z;
            *Y = z * h22 - w;
            X += incX;
            Y += incY;
         }
      }
   }
}
