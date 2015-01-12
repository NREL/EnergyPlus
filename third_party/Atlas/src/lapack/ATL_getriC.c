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

static void trcpzeroL(const int M, const int N, TYPE *L, const int ldl,
                      TYPE *C, const int ldc)
/*
 * Copies lower triangle from L, replacing with zeros
 */
{
   const int M2 = M SHIFT, ldl2 = ldl SHIFT, ldc2 = ldc SHIFT;
   int i, j;

   for (j=0; j != N; j++)
   {
      for (i=((j+1)SHIFT); i < M2; i++)
      {
         C[i] = L[i];
         L[i] = ATL_rzero;
      }
      C += ldc2;
      L += ldl2;
   }
}

int ATL_getriC(const int N, TYPE *A, const int lda, const int *ipiv,
               TYPE *wrk, const int lwrk)
{
   const int lda2 = lda SHIFT;
   int J, jb, nb, nright, iret;
   TYPE *A0 = A;
   #ifdef TREAL
      const TYPE one=ATL_rone, none=ATL_rnone;
   #else
      const TYPE one[2]={ATL_rone,ATL_rzero}, none[2]={ATL_rnone, ATL_rzero};
   #endif

   iret = ATL_trtri(CblasColMajor, CblasUpper, CblasNonUnit, N, A, lda);
   if (!iret && N > 1)
   {
/*
 *    Find largest NB we can use with our provided workspace
 */
      jb = lwrk / N;
      if (jb >= NB) nb = ATL_MulByNB(ATL_DivByNB(jb));
      else if (jb >= ATL_mmNU) nb = (jb/ATL_mmNU)*ATL_mmNU;
      else nb = jb;
      if (!nb) return(-6);  /* need at least 1 col of workspace */
/*
 *    Only first iteration will have partial block, unroll it
 */
      jb = N - (N/nb)*nb;
      if (!jb) jb = nb;
      J = N - jb;
      A += lda2*J;
      trcpzeroL(jb, jb, A+(J SHIFT), lda, wrk, jb);
      cblas_trsm(CblasColMajor, CblasRight, CblasLower, CblasNoTrans, CblasUnit,
                 N, jb, one, wrk, jb, A, lda);
      if (J)
      {
         do
         {
            J -= nb;
            A -= nb*lda2;
            nright = N-J;
            trcpzeroL(nright, nb, A+(J SHIFT), lda, wrk, nright);
            cblas_gemm(CblasColMajor, CblasNoTrans, CblasNoTrans, N, nb,
                       nright-nb, none, A+nb*lda2, lda, wrk+(nb SHIFT), nright,
                       one, A, lda);
            cblas_trsm(CblasColMajor, CblasRight, CblasLower, CblasNoTrans,
                       CblasUnit, N, nb, one, wrk, nright, A, lda);
         }
         while(J);
      }
/*
 *    Apply column interchanges
 */
      for (J=N-2; J >= 0; J--)
      {
         jb = ipiv[J];
         if (jb != J) cblas_swap(N, A+J*lda2, 1, A+jb*lda2, 1);
      }
   }
   return(iret);
}
