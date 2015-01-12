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
void ATL_dstrcollapse(const enum ATLAS_UPLO Uplo, const enum ATLAS_DIAG Diag,
                      const int N, double *A, const int lda, const int ldc)
#elif defined(SCPLX)
void ATL_zctrcollapse(const enum ATLAS_UPLO Uplo, const enum ATLAS_DIAG Diag,
                      const int N, double *A, const int lda0, const int ldc0)
#elif defined(DCPLX)
void ATL_eztrcollapse(const enum ATLAS_UPLO Uplo, const enum ATLAS_DIAG Diag,
                      const int N, ATL_QTYPE *A, const int lda0, const int ldc0)
#else
void ATL_qdtrcollapse(const enum ATLAS_UPLO Uplo, const enum ATLAS_DIAG Diag,
                      const int N, ATL_QTYPE *A, const int lda, const int ldc)
#endif
/*
 * Translates upper/lower triangle from double to single precision
 */
{
#if defined(SREAL) || defined(SCPLX)
   float *C = (float*)A;
#else
   double *C = (double*)A;
#endif
#ifdef TCPLX
   const int lda = lda0+lda0, ldc = ldc0+ldc0;
#endif
   int i, j, st;

   ATL_assert(ldc <= 2*lda);
   if (Uplo == AtlasUpper)
   {
      for (j=0; j < N; j++, C += ldc, A += lda)
      {
         st = (Diag == AtlasUnit) ? j-1 : j;
         for (i=0; i < st; i++)
         #ifdef TREAL
            C[i] = A[i];
         #else
         {
            C[i+i] = A[i+i];
            C[i+i+1] = A[i+i+1];
         }
         #endif
      }
   }
   else
   {
      for (j=0; j < N; j++, C += ldc, A += lda)
      {
         for (i=(Diag == AtlasUnit) ? j+1 : j; i < N; i++)
         #ifdef TREAL
            C[i] = A[i];
         #else
         {
            C[i+i] = A[i+i];
            C[i+i+1] = A[i+i+1];
         }
         #endif
      }
   }
}
