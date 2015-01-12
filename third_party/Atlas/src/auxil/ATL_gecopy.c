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

void Mjoin(PATL,gecopy)
   (const int M0, const int N, const TYPE *A, const int lda,
    TYPE *C, const int ldc)
/*
 * C <- A;  copy backwards, so 1st cols of C are retained in cache for reuse
 */
{
   int i, j;
   #ifdef TREAL
      #define M M0
      const int incA = lda+lda, incC = ldc+ldc;
   #else
      const int M = M0<<1, incA = lda<<2, incC = ldc<<2;
   #endif
   const int n = N>>1;
   const TYPE *A0, *A1;
   TYPE *C0, *C1;

   A0 = A + (lda SHIFT)*(N-2);
   A1 = A0 + (lda SHIFT);
   C0 = C + (ldc SHIFT)*(N-2);
   C1 = C0 + (ldc SHIFT);
   for (j=n; j; j--, A0 -= incA, A1 -= incA, C0 -= incC, C1 -= incC)
   {
      for (i=M-1; i >= 0; i--)
      {
         C0[i] = A0[i];
         C1[i] = A1[i];
      }
   }
   if (N-n-n)
      for (i=M-1; i >= 0; i--)
         C[i] = A[i];
}
