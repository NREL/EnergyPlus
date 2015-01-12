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
#include "atlas_lvl2.h"

void ATL_UGERK
   (ATL_CINT M, ATL_CINT N, const TYPE *X, const TYPE *Y, TYPE *A, ATL_CINT lda)
{
   ATL_INT i, j;
   const TYPE *x;
   TYPE *A0 = A, *A1 = A + lda, *A2 = A1 + lda, *A3 = A2 + lda;
   ATL_CINT M4 = (M>>2)<<2, N4 = (N>>2)<<2, incAn = (lda<<2) - M4;
   register TYPE x0, x1, x2, x3, y0, y1, y2, y3;

   if (M4)
   {
      for (j=N4; j; j -= 4)
      {
         y0 = *Y;
         y1 = Y[1];
         y2 = Y[2];
         y3 = Y[3];  Y  += 4;
         x = X;
         for (i=M4; i; i -= 4)
         {
            x0 = *x; x1 = x[1]; x2 = x[2]; x3 = x[3];
            *A0 += y0 * x0;
            x += 4;
            *A1 += y1 * x0;
            *A2 += y2 * x0;
            *A3 += y3 * x0;
            A0[1] += y0 * x1;
            A1[1] += y1 * x1;
            A2[1] += y2 * x1;
            A3[1] += y3 * x1;
            A0[2] += y0 * x2;
            A1[2] += y1 * x2;
            A2[2] += y2 * x2;
            A3[2] += y3 * x2;
            A0[3] += y0 * x3; A0 += 4;
            A1[3] += y1 * x3; A1 += 4;
            A2[3] += y2 * x3; A2 += 4;
            A3[3] += y3 * x3; A3 += 4;
         }
         switch(M-M4)
         {
         case 1:
            x0 = *x;
            *A0 += y0 * x0;
            *A1 += y1 * x0;
            *A2 += y2 * x0;
            *A3 += y3 * x0;
            break;
         case 2:
            x0 = *x; x1 = x[1];
            *A0 += y0 * x0;
            *A1 += y1 * x0;
            *A2 += y2 * x0;
            *A3 += y3 * x0;
            A0[1] += y0 * x1;
            A1[1] += y1 * x1;
            A2[1] += y2 * x1;
            A3[1] += y3 * x1;
            break;
         case 3:
            x0 = *x; x1 = x[1]; x2 = x[2];
            *A0 += y0 * x0;
            *A1 += y1 * x0;
            *A2 += y2 * x0;
            *A3 += y3 * x0;
            A0[1] += y0 * x1;
            A1[1] += y1 * x1;
            A2[1] += y2 * x1;
            A3[1] += y3 * x1;
            A0[2] += y0 * x2;
            A1[2] += y1 * x2;
            A2[2] += y2 * x2;
            A3[2] += y3 * x2;
            break;
         }
         A0 += incAn;
         A1 += incAn;
         A2 += incAn;
         A3 += incAn;
      }
      if (N-N4)
         Mjoin(PATL,gerk_axpy)(M, N-N4, ATL_rone, X, 1, Y, 1, A0, lda);
   }
   else
      Mjoin(PATL,gerk_Mlt16)(M, N, ATL_rone, X, 1, Y, 1, A, lda);
}
