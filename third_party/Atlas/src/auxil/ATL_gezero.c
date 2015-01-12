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

void Mjoin(PATL,gezero)(const int M0, const int N, TYPE *C, const int ldc0)
/*
 * C(:,:) = 0, assign matrix C to zero
 */
{
   #ifdef TREAL
      #define M M0
      #define ldc ldc0
   #else
      const int M = M0<<1, ldc = ldc0<<1;
   #endif
   const int m = M >> 5 << 5;
   TYPE *c, *stC = C + m;
   register int j;
   int k;

   for (j=0; j != N; j++)
   {
      c = C;
      if (c != stC)
      {
         do
         {
            *c    = c[ 1] = c[ 2] = c[ 3] = c[ 4] = c[ 5] = c[ 6] = c[ 7] =
            c[ 8] = c[ 9] = c[10] = c[11] = c[12] = c[13] = c[14] = c[15] =
            c[16] = c[17] = c[18] = c[19] = c[20] = c[21] = c[22] = c[23] =
            c[24] = c[25] = c[26] = c[27] = c[28] = c[29] = c[30] = c[31] =
                    ATL_rzero;
            c += 32;
         }
         while (c != stC);
      }

      k = M - m;
      if (k)
      {
         if (k >> 4) /* K >= 16 */
         {
            *c = c[ 1] = c[ 2] = c[ 3] = c[ 4] = c[ 5] = c[ 6] = c[ 7] = c[ 8] =
            c[ 9] = c[10] = c[11] = c[12] = c[13] = c[14] = c[15] = ATL_rzero;
            k -= 16;
            c += 16;
         }
         if (k >> 3) /* K >= 8 */
         {
            *c    = c[ 1] = c[ 2] = c[ 3] = c[ 4] = c[ 5] = c[ 6] = c[ 7] =
                    ATL_rzero;
            k -= 8;
            c += 8;
         }
         switch(k)
         {
         case 1:
            *c = ATL_rzero;
            break;
         case 2:
            *c    = c[ 1] = ATL_rzero;
            break;
         case 3:
            *c    = c[ 1] = c[ 2] = ATL_rzero;
            break;
         case 4:
            *c    = c[ 1] = c[ 2] = c[ 3] = ATL_rzero;
            break;
         case 5:
            *c    = c[ 1] = c[ 2] = c[ 3] = c[ 4] = ATL_rzero;
            break;
         case 6:
            *c    = c[ 1] = c[ 2] = c[ 3] = c[ 4] = c[ 5] = ATL_rzero;
            break;
         case 7:
            *c    = c[ 1] = c[ 2] = c[ 3] = c[ 4] = c[ 5] = c[ 6] = ATL_rzero;
            break;
         default:;
         }
      }
      C += ldc;
      stC += ldc;
   }
}
