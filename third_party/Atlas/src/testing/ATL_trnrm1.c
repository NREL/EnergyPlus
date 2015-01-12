/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                   (C) Copyright 1999 Antoine P. Petitet
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
#include "atlas_tst.h"
#include "atlas_level1.h"

TYPE Mjoin(PATL,trnrm1)(const enum ATLAS_UPLO Uplo, const enum ATLAS_DIAG Diag,
                        const int N, const TYPE *A, const int lda)
/*
 * Calculates the 1-norm of a general rectangular matrix
 */
{
   const int incA = (Uplo == AtlasUpper ? (lda SHIFT) : ((lda+1)SHIFT));
   const int ioff = (Diag == AtlasNonUnit ? 1 : 0);
   int j;
   TYPE max=0.0, t0;

   if (Uplo == AtlasUpper)
   {
      for (j=0; j < N; j++)
      {
         t0 = Mjoin(PATL,asum)(j+ioff, A, 1);
         if (Diag == AtlasUnit) t0 += ATL_rone;
         if (t0 > max) max = t0;
         A += incA;
      }
   }
   else
   {
      if (Diag == AtlasUnit) A += 1 SHIFT;
      for (j=N; j; j--)
      {
         t0 = Mjoin(PATL,asum)(j+ioff-1, A, 1);
         if (Diag == AtlasUnit) t0 += ATL_rone;
         if (t0 > max) max = t0;
         A += incA;
      }
   }
   return(max);
}
