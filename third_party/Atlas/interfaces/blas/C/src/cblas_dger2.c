/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2010 R. Clint Whaley
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

#define DREAL
#include "atlas_misc.h"
#include "cblas.h"
#ifdef ATL_USEPTHREADS
   #include "atlas_ptalias2.h"
#endif
#include "atlas_level2.h"

void cblas_dger2(const enum CBLAS_ORDER Order, ATL_CINT M, ATL_CINT N,
                 const double alpha, const double *X, ATL_CINT incX,
                 const double *Y, ATL_CINT incY, const double beta,
                 const double *W, ATL_CINT incW,
                 const double *Z, ATL_CINT incZ, double *A, ATL_CINT lda)
{
   int info = 2000;
   #define x X
   #define y Y
   #define w W
   #define z Z

#ifndef NoCblasErrorChecks
   if (M < 0) info = cblas_errprn(2, info,
                        "M cannot be less than zero; is set to %d.", M);
   if (N < 0) info = cblas_errprn(3, info,
                        "N cannot be less than zero; is set to %d.", N);
   if (!incX) info = cblas_errprn(6, info,
                                  "incX cannot be zero; is set to %d.", incX);
   if (!incY) info = cblas_errprn(8, info,
                                  "incY cannot be zero; is set to %d.", incY);
   if (!incW) info = cblas_errprn(11, info,
                                  "incW cannot be zero; is set to %d.", incW);
   if (!incZ) info = cblas_errprn(13, info,
                                  "incZ cannot be zero; is set to %d.", incZ);
   if (Order == CblasColMajor)
   {
      if (lda < M || lda < 1)
         info = cblas_errprn(15, info, "lda must be >= MAX(M,1): lda=%d M=%d",
                             lda, M);
   }
   else if (Order == CblasRowMajor)
   {
      if (lda < N || lda < 1)
         info = cblas_errprn(15, info, "lda must be >= MAX(N,1): lda=%d M=%d",
                             lda, N);
   }
   else
      info = cblas_errprn(1, info, "Order must be %d or %d, but is set to %d",
                          CblasRowMajor, CblasColMajor, Order);
   if (info != 2000)
   {
      cblas_xerbla(info, "cblas_dger2", "");
      return;
   }
#endif

   if (incX < 0) x += (1-M)*incX;
   if (incY < 0) y += (1-N)*incY;
   if (incW < 0) w += (1-M)*incW;
   if (incZ < 0) z += (1-N)*incZ;

   if (Order == CblasColMajor)
      ATL_dger2(M, N, alpha, x, incX, y, incY, beta, w, incW, z, incZ, A, lda);
   else
      ATL_dger2(N, M, alpha, y, incY, x, incX, beta, w, incW, z, incZ, A, lda);
}
