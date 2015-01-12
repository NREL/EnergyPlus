/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 *                    (C) Copyright 2007 R. Clint Whaley
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

#ifdef SREAL
void ATL_dsgecollapse(const int M, const int N, double *A,
                      const int lda, const int ldc)
/*
 * Copies double precision array C into a float array in-place.
 */
{
   float *C = (float*)A;
   int i, j;

   ATL_assert(ldc <= 2*lda);
   for (j=0; j < N; j++, A += lda, C += ldc)
   {
      for (i=0; i < M; i++)
         C[i] = A[i];
   }
}
#elif defined(DREAL)
void ATL_qdgecollapse(const int M, const int N, ATL_QTYPE *A,
                      const int lda, const int ldc)
/*
 * Copies double precision array C into a float array in-place.
 */
{
   double *C = (double*)A;
   int i, j;

   ATL_assert(ldc <= 2*lda);
   for (j=0; j < N; j++, A += lda, C += ldc)
   {
      for (i=0; i < M; i++)
         C[i] = A[i];
   }
}
#elif defined(SCPLX)
void ATL_zcgecollapse(const int M, const int N, double *A,
                      const int lda, const int ldc)
{
   ATL_dsgecollapse(M+M, N, A, lda+lda, ldc+ldc);
}
#else
void ATL_ezgecollapse(const int M, const int N, ATL_QTYPE *A,
                      const int lda, const int ldc)
{
   ATL_qdgecollapse(M+M, N, A, lda+lda, ldc+ldc);
}
#endif
