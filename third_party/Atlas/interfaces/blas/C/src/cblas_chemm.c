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
#ifdef ATL_USEPTHREADS
   #include "atlas_ptalias3.h"
#endif
#include "cblas.h"

void ATL_chemm(const enum CBLAS_SIDE Side,
               const enum CBLAS_UPLO Uplo, const int M, const int N,
               const float * alpha, const float *A, const int lda,
               const float *B, const int ldb, const float * beta,
               float *C, const int ldc);

void cblas_chemm(const enum CBLAS_ORDER Order, const enum CBLAS_SIDE Side,
                 const enum CBLAS_UPLO Uplo, const int M, const int N,
                 const void * alpha, const void *A, const int lda,
                 const void *B, const int ldb, const void * beta,
                 void *C, const int ldc)
{
   enum CBLAS_SIDE side;
   enum CBLAS_UPLO uplo;
   int info=2000;

#ifndef NoCblasErrorChecks
   if (Order == CblasColMajor)
   {
      if (Side == CblasLeft)
      {
         if ( (lda < M) || (lda < 1) )
            info = cblas_errprn(8, info, "lda must be >= MAX(M,1): lda=%d M=%d",
                                lda, M);
      }
      else if (Side == CblasRight)
      {
         if ( (lda < N) || (lda < 1) )
            info = cblas_errprn(8, info, "lda must be >= MAX(N,1): lda=%d N=%d",
                                lda, N);
      }
      else info = cblas_errprn(2, info,
                               "SIDE must be %d or %d, but is set to %d",
                               CblasRight, CblasLeft, Side);
      if ( (ldb < M) || (ldb < 1) )
         info = cblas_errprn(10, info, "ldb must be >= MAX(M,1): ldb=%d M=%d",
                             ldb, M);
      if ( (ldc < M) || (ldc < 1) )
         info = cblas_errprn(13, info,"ldc must be >= MAX(M,1): ldc=%d M=%d",
                             ldc, M);
   }
   else if (Order == CblasRowMajor)
   {
      if (Side == CblasLeft)
      {
         if ( (lda < M) || (lda < 1) )
            info = cblas_errprn(8, info, "lda must be >= MAX(M,1): lda=%d M=%d",
                                lda, M);
      }
      else if (Side == CblasRight)
      {
         if ( (lda < N) || (lda < 1) )
            info = cblas_errprn(8, info, "lda must be >= MAX(N,1): lda=%d N=%d",
                                lda, N);
      }
      else info = cblas_errprn(2, info,
                               "SIDE must be %d or %d, but is set to %d",
                               CblasRight, CblasLeft, Side);
      if ( (ldb < N) || (ldb < 1) )
         info = cblas_errprn(10, info, "ldb must be >= MAX(N,1): ldb=%d N=%d",
                             ldb, N);
      if ( (ldc < N) || (ldc < 1) )
         info = cblas_errprn(13, info,"ldc must be >= MAX(N,1): ldc=%d N=%d",
                             ldc, N);
   }
   else info = cblas_errprn(1, info, "Order must be %d or %d, but is set to %d",
                            CblasRowMajor, CblasColMajor, Order);

   if (Uplo != CblasUpper && Uplo != CblasLower)
      info = cblas_errprn(3, info, "UPLO must be %d or %d, but is set to %d",
                               CblasUpper, CblasLower, Uplo);

   if (M < 0) info = cblas_errprn(4, info,
                     "M cannot be less than zero; it is set to %d.", M);
   if (N < 0) info = cblas_errprn(5, info,
                     "N cannot be less than zero; it is set to %d.", N);

   if (info != 2000)
   {
      cblas_xerbla(info, "cblas_chemm", "");
      return;
   }
#endif

   if (Order == CblasColMajor)
      ATL_chemm(Side, Uplo, M, N, alpha, A, lda, B, ldb, beta, C, ldc);
   else
   {
      if (Side == CblasLeft) side = CblasRight;
      else side = CblasLeft;
      if (Uplo == CblasUpper) uplo = CblasLower;
      else uplo = CblasUpper;
      ATL_chemm(side, uplo, N, M, alpha, A, lda, B, ldb, beta, C, ldc);
   }
}
