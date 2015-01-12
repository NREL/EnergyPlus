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
#include "atlas_tst.h"
#include "atlas_f77blas.h"

#define f77ger Mjoin(PATL,f77gerc)
#define F77GER F77gerc
void f77ger(const int M, const int N, const SCALAR alpha0,
            const TYPE *X, const int incX, const TYPE *Y, const int incY,
            TYPE *A, const int lda)
{
   #ifdef TCPLX
      TYPE alpha[2];
   #else
      TYPE alpha = alpha0;
   #endif
   #ifdef ATL_FunkyInts
      const F77_INTEGER F77M=M, F77N=N, F77lda=lda, F77incx=incX, F77incy=incY;
   #else
      #define F77M    M
      #define F77N    N
      #define F77lda  lda
      #define F77incx incX
      #define F77incy incY
   #endif
   #ifdef TCPLX
      alpha[0] = *alpha0; alpha[1] = alpha0[1];
   #endif
   if (incX < 0) X -= (1-M)*incX SHIFT;
   if (incY < 0) Y -= (1-N)*incY SHIFT;

   F77GER(&F77M, &F77N, SADD alpha, X, &F77incx, Y, &F77incy, A, &F77lda);
}
