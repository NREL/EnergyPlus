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

#define SCPLX
#include "atlas_misc.h"
#include "cblas.h"
#ifdef ATL_USEPTHREADS
   #include "atlas_ptalias2.h"
#endif
#include "atlas_level2.h"

void cblas_cgbmv(const enum CBLAS_ORDER Order, const enum CBLAS_TRANSPOSE TA,
                 const int M, const int N, const int KL, const int KU,
                 const void *alpha, const void *A, const int lda,
                 const void *X, const int incX,
                 const void *beta, void *Y, const int incY)
{
   int info = 2000;
   const float *x = X;
   float *y = Y;

#ifndef NoCblasErrorChecks
   if (Order != CblasRowMajor && Order != CblasColMajor)
      info = cblas_errprn(1, info, "Order must be %d or %d, but is set to %d",
                          CblasRowMajor, CblasColMajor, Order);
   if (TA != CblasNoTrans && TA != CblasTrans && TA != CblasConjTrans)
      info = cblas_errprn(2, info,
                          "TransA must be %d, %d or %d, but is set to %d",
                          CblasNoTrans, CblasTrans, CblasConjTrans, TA);
   if (M < 0) info = cblas_errprn(3, info,
                        "M cannot be less than zero; is set to %d.", M);
   if (N < 0) info = cblas_errprn(4, info,
                        "N cannot be less than zero; is set to %d.", N);
   if (KL < 0) info = cblas_errprn(5, info,
                         "KL cannot be less than zero; is set to %d.", KL);
   if (KU < 0) info = cblas_errprn(6, info,
                         "KU cannot be less than zero; is set to %d.", KU);
   if (lda < (KL+KU+1))
      info = cblas_errprn(9, info, "lda must be >= KU+KL+1: lda=%d KU+KL+1=%d",
                          lda, KU+KL+1);
   if (!incX) info = cblas_errprn(11, info,
                                  "incX cannot be zero; is set to %d.", incX);
   if (!incY) info = cblas_errprn(14, info,
                                  "incY cannot be zero; is set to %d.", incY);
   if (info != 2000)
   {
      cblas_xerbla(info, "cblas_cgbmv", "");
      return;
   }
#endif

   if (TA == AtlasNoTrans)
   {
      if (incX < 0) x += (1-N)*incX<<1;
      if (incY < 0) y += (1-M)*incY<<1;
   }
   else
   {
      if (incX < 0) x += (1-M)*incX<<1;
      if (incY < 0) y += (1-N)*incY<<1;
   }
   if (Order == CblasColMajor)
      ATL_cgbmv(TA, M, N, KL, KU, alpha, A, lda, x, incX, beta, y, incY);
   else
   {
      if (TA == CblasNoTrans)
         ATL_cgbmv(CblasTrans, N, M, KU, KL, alpha, A, lda, x, incX,
                   beta, y, incY);
      else if (TA == CblasConjTrans)
         ATL_cgbmv(AtlasConj, N, M, KU, KL, alpha, A, lda, x, incX,
                   beta, y, incY);
      else ATL_cgbmv(CblasNoTrans, N, M, KU, KL, alpha, A, lda, x, incX,
                     beta, y, incY);
   }
}
