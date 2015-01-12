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
#define DREAL
#include "atlas_misc.h"
#ifdef ATL_USEPTHREADS
   #include "atlas_ptalias_lapack.h"
#endif
#include "atlas_lapack.h"
#include "clapack.h"

int clapack_dgetrs
   (const enum CBLAS_ORDER Order, const enum CBLAS_TRANSPOSE Trans,
    const int N, const int NRHS, const double *A, const int lda,
    const int *ipiv, double *B, const int ldb)
{
   int ierr=0;
   if (Order != CblasRowMajor && Order != CblasColMajor)
   {
      ierr = -1;
      cblas_xerbla(1, "clapack_dgetrs",
                   "Order must be %d or %d, but is set to %d\n",
                   CblasRowMajor, CblasColMajor, Order);
   }
   if (Trans != CblasNoTrans && Trans != CblasTrans && Trans != CblasConjTrans)
   {
      ierr = -2;
      cblas_xerbla(2, "clapack_dgetrs",
                   "Trans must be %d, %d, or %d, but is set to %d\n",
                   CblasNoTrans, CblasTrans, CblasConjTrans);
   }
   if (N < 0)
   {
      ierr = -3;
      cblas_xerbla(3, "clapack_dgetrs",
                   "N cannot be less than zero 0,; is set to %d.\n", N);
   }
   if (NRHS < 0)
   {
      ierr = -4;
      cblas_xerbla(4, "clapack_dgetrs",
                   "NRHS cannot be less than zero 0,; is set to %d.\n", NRHS);
   }
   if (lda < N || lda < 1)
   {
      ierr = -6;
      cblas_xerbla(6, "clapack_dgetrs",
                   "lda must be >= MAX(N,1): lda=%d N=%d\n", lda, N);
   }
   if (ldb < N || ldb < 1)
   {
      ierr = -9;
      cblas_xerbla(9, "clapack_dgetrs",
                   "ldb must be >= MAX(N,1): lda=%d N=%d\n", lda, N);
   }
   if (!ierr) ATL_dgetrs(Order, Trans, N, NRHS, A, lda, ipiv, B, ldb);
   return(ierr);
}
