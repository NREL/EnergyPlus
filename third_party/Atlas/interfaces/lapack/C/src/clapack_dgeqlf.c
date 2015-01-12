/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2014, 2009 R. Clint Whaley
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


int clapack_dgeqlf
   (const enum CBLAS_ORDER Order, ATL_CINT M, ATL_CINT N,
    double *A, ATL_CINT lda, double *TAU)
{
   int ierr=0;
   if (Order != CblasRowMajor && Order != CblasColMajor)
   {
      ierr = -1;
      cblas_xerbla(1, "clapack_dgeqlf",
                   "Order must be %d or %d, but is set to %d\n",
                   CblasRowMajor, CblasColMajor, Order);
   }
   if (M < 0)
   {
      ierr = -2;
      cblas_xerbla(2, "clapack_dgeqlf",
                   "M cannot be less than zero 0,; is set to %d.\n", M);
   }
   if (N < 0)
   {
      ierr = -3;
      cblas_xerbla(3, "clapack_dgeqlf",
                   "N cannot be less than zero 0,; is set to %d.\n", N);
   }
   if (Order == CblasColMajor)
   {
      if (lda < M || lda < 1)
      {
         ierr = -5;
         cblas_xerbla(5, "clapack_dgeqlf",
                      "lda must be >= MAX(M,1): lda=%d M=%d\n", lda, M);
      }
   }
   else
   {
      if (lda < N || lda < 1)
      {
         ierr = -5;
         cblas_xerbla(5, "clapack_dgeqlf",
                      "lda must be >= MAX(N,1): lda=%d N=%d\n", lda, N);
      }
   }
   if (ierr)
      return(ierr);
   if (Order == CblasColMajor)
      return(ATL_dgeqlf(M, N, A, lda, TAU, NULL, 0));
   else
      return(ATL_dgerqf(N, M, A, lda, TAU, NULL, 0));
}
