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
#include "atlas_tst.h"

static void my_swap
   (const int N, TYPE *X, const int incx, TYPE *Y, const int incy)
{
   const int incX = incx SHIFT, incY = incy SHIFT;
   int i;
   TYPE t0;
   for (i=N; i; i--, X += incX, Y += incY)
   {
      t0 = *X;
      *X = *Y;
      *Y = t0;
      #ifdef TCPLX
         t0 = X[1];
         X[1] = Y[1];
         Y[1] = t0;
      #endif
   }
}
void Mjoin(PATL,tstsqtran)(const int N, TYPE *A, const int lda0)
/*
 * transposes the square matrix A, easiest (and slowest) algorithm possible
 */
{
   const int lda=(lda0 SHIFT), ldap1=lda0+1;
   int i;

   for (i=1; i < N; i++)
      my_swap(N-i, A+(i SHIFT), ldap1, A+i*lda, ldap1);
}
