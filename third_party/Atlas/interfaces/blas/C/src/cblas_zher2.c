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

#define DCPLX
#include "atlas_misc.h"
#include "cblas.h"
#ifdef ATL_USEPTHREADS
   #include "atlas_ptalias2.h"
#endif
#include "atlas_level2.h"

void cblas_zher2(const enum CBLAS_ORDER Order, const enum CBLAS_UPLO Uplo,
                 const int N, const void *alpha,
                 const void *X, const int incX,
                 const void *Y, const int incY, void *A, const int lda)
{
   int info = 2000;
   void *vx, *vy;
   double *x0, *y0;
   const double *x=X, *y=Y, *alp=alpha;
   const double one[2]={ATL_rone, ATL_rzero};

#ifndef NoCblasErrorChecks
   if (Order != CblasColMajor && Order != CblasRowMajor)
      info = cblas_errprn(1, info, "Order must be %d or %d, but is set to %d",
                          CblasRowMajor, CblasColMajor, Order);
   if (Uplo != CblasUpper && Uplo != CblasLower)
      info = cblas_errprn(2, info, "UPLO must be %d or %d, but is set to %d",
                          CblasUpper, CblasLower, Uplo);
   if (N < 0) info = cblas_errprn(3, info,
                        "N cannot be less than zero; is set to %d.", N);
   if (!incX) info = cblas_errprn(6, info,
                                  "incX cannot be zero; is set to %d.", incX);
   if (!incY) info = cblas_errprn(8, info,
                                  "incY cannot be zero; is set to %d.", incY);
   if (lda < N || lda < 1)
      info = cblas_errprn(10, info, "lda must be >= MAX(N,1): lda=%d N=%d",
                          lda, N);
   if (info != 2000)
   {
      cblas_xerbla(info, "cblas_zher2", "");
      return;
   }
#endif

   if (incX < 0) x += (1-N)*incX<<1;
   if (incY < 0) y += (1-N)*incY<<1;

   if (Order == CblasColMajor)
      ATL_zher2(Uplo, N, alpha, x, incX, y, incY, A, lda);
   else if (alp[0] != ATL_rzero || alp[1] != ATL_rzero)
   {
      vx = malloc(ATL_Cachelen + ATL_MulBySize(N));
      vy = malloc(ATL_Cachelen + ATL_MulBySize(N));
      ATL_assert(vx != NULL && vy != NULL);
      x0 = ATL_AlignPtr(vx);
      y0 = ATL_AlignPtr(vy);
      ATL_zmoveConj(N, alpha, y, incY, y0, 1);
      ATL_zcopyConj(N, x, incX, x0, 1);
      ATL_zher2(( (Uplo == CblasUpper) ? CblasLower : CblasUpper ),
                N, one, y0, 1, x0, 1, A, lda);
      free(vx);
      free(vy);
   }
   else ATL_zher2(( (Uplo == CblasUpper) ? CblasLower : CblasUpper ),
                  N, alpha, y, incY, x, incX, A, lda);
}
