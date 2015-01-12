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
#ifdef ATL_USEPTHREADS
   #include "atlas_ptalias_lapack.h"
#endif
#include "atlas_lapack.h"
#include "clapack.h"

int clapack_cgesv(const enum CBLAS_ORDER Order, const int N, const int NRHS,
                  void *A, const int lda, int *ipiv,
                  void *B, const int ldb)
/*
 * clapack_cgesv computes the solution to a system of linear equations
 *   A * X = B,
 * where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
 * The LU factorization used to factor A is dependent on the Order parameter,
 * as detailed in the leading comments of clapack_cgetrf.
 * The factored form of A is then used solve the system of equations A * X = B.
 * A is overwritten with the appropriate LU factorization, and B, which
 * contains B on input, is overwritten with the solution X on output.
 */
{
   int ierr = 0;

   if (Order != CblasRowMajor && Order != CblasColMajor)
   {
      ierr = -1;
      cblas_xerbla(1, "clapack_cgesv",
                   "Order must be %d or %d, but is set to %d.\n",
                   CblasRowMajor, CblasColMajor, Order);
   }
   if (N < 0)
   {
      ierr = -2;
      cblas_xerbla(2, "clapack_cgesv",
                   "N cannot be less than zero 0,; is set to %d.\n", N);
   }
   if (NRHS < 0)
   {
      ierr = -3;
      cblas_xerbla(3, "clapack_cgesv",
                   "NRHS cannot be less than zero 0,; is set to %d.\n", NRHS);
   }
   if (lda < N || lda < 1)
   {
      ierr = -5;
      cblas_xerbla(5, "clapack_cgesv",
                   "lda must be >= MAX(N,1): lda=%d N=%d\n", lda, N);
   }
   if (ldb < N || ldb < 1)
   {
      ierr = -8;
      cblas_xerbla(8, "clapack_cgesv",
                   "ldb must be >= MAX(N,1): ldb=%d N=%d\n", ldb, N);

   }
   if (!ierr) ierr = ATL_cgetrf(Order, N, N, A, lda, ipiv);
   if (!ierr) ATL_cgetrs(Order, CblasNoTrans, N, NRHS, A, lda, ipiv, B, ldb);
   return(ierr);
}
