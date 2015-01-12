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
#include "atlas_lapack.h"
#include "atlas_lvl3.h"

static void trcpzeroU(const int M, const int N, TYPE *U, const int ldu,
                      TYPE *C, const int ldc)
/*
 * Copies an upper row-major array from U, zeroing U, U is unit, so
 * diagonal is not copied
 */
{
   const int ldu2 = ldu SHIFT, ldc2 = ldc SHIFT, N2 = N SHIFT;
   int i, j;

   for (i=0; i != M; i++)
   {
      for (j=(i+1)SHIFT; j < N2; j++)
      {
         C[j] = U[j];
         U[j] = ATL_rzero;
      }
      C += ldc2;
      U += ldu2;
   }
}
int ATL_getriR(const int N, TYPE *A, const int lda, const int *ipiv,
               TYPE *wrk, const int lwrk)
{
   int jb, nb, I, ndown, iret;
   const int lda2 = lda SHIFT;
   #ifdef TREAL
      const TYPE one=ATL_rone, none=ATL_rnone;
   #else
      const TYPE one[2]={ATL_rone,ATL_rzero}, none[2]={ATL_rnone, ATL_rzero};
   #endif

   iret = ATL_trtri(CblasRowMajor, CblasLower, CblasNonUnit, N, A, lda);
   if (!iret && N > 1)
   {
/*
 *    Find largest NB we can use with our provided workspace
 */
    jb = lwrk / N;
    if (jb >= NB) nb = ATL_MulByNB(ATL_DivByNB(jb));
    else if (jb >= ATL_mmMU) nb = (jb/ATL_mmMU)*ATL_mmMU;
    else nb = jb;
    if (!nb) return(-6);  /* need at least 1 row of workspace */
/*
 *    Only first iteration will have partial block, unroll it
 */
      jb = N - (N/nb)*nb;
      if (!jb) jb = nb;
      I = N - jb;
      A += lda2*I;
      trcpzeroU(jb, jb, A+(I SHIFT), lda, wrk, jb);
      cblas_trsm(CblasRowMajor, CblasLeft, CblasUpper, CblasNoTrans, CblasUnit,
                 jb, N, one, wrk, jb, A, lda);
      if (I)
      {
         do
         {
            I -= nb;
            A -= nb*lda2;
            ndown = N-I;
            trcpzeroU(nb, ndown, A+(I SHIFT), lda, wrk, ndown);
            cblas_gemm(CblasRowMajor, CblasNoTrans, CblasNoTrans, nb, N,
                       ndown-nb, none, wrk+(nb SHIFT), ndown, A+nb*lda2, lda,
                       one, A, lda);
            cblas_trsm(CblasRowMajor, CblasLeft, CblasUpper, CblasNoTrans,
                       CblasUnit, nb, N, one, wrk, ndown, A, lda);
         }
         while(I);
      }
/*
 *    Apply row interchanges
 */
      for (I=N-2; I >= 0; I--)
      {
         jb = ipiv[I];
         if (jb != I) cblas_swap(N, A+I*lda2, 1, A+jb*lda2, 1);
      }
   }
   return(iret);
}
