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
#include "atlas_lapack.h"

void ATL_getrs(const enum CBLAS_ORDER Order, const enum CBLAS_TRANSPOSE Trans,
               const int N, const int NRHS, const TYPE *A, const int lda,
               const int *ipiv, TYPE *B, const int ldb)
/*
 * OK, this pivoting crap is tricky.  The trick is, when we pivot columns
 * of the matrix, this effects X but not B, and when we pivot rows, this
 * effects B, but not X.  So, must never attempt to apply a Pr
 * (row permutation matrix) to X or a Pc to B.
 */
{
   enum CBLAS_DIAG Lunit, Uunit;
   #ifdef TREAL
      #define one ATL_rone
   #else
      const TYPE one[2] = {ATL_rone, ATL_rzero};
   #endif

   if (!N || !NRHS) return;

   if (Order == CblasColMajor)
   {
/*
 *    A*X = B.  Since we have pivoted A by Pr (PA=LU), we pivot B by Pr,
 *    **and this does not effect X at all**, so we solve
 *    X = inv(U)*inv(L)*(Pr * B)
 */
      if (Trans == CblasNoTrans)
      {
         ATL_laswp(NRHS, B, ldb, 0, N, ipiv, 1);
         cblas_trsm(Order, CblasLeft, CblasLower, CblasNoTrans, CblasUnit,
                    N, NRHS, one, A, lda, B, ldb);
         cblas_trsm(Order, CblasLeft, CblasUpper, CblasNoTrans, CblasNonUnit,
                    N, NRHS, one, A, lda, B, ldb);
      }
/*
 *    trans(L*U = PA)  ==>  U' L' = A' P, so P is Pc, and does not effect B,
 *    U' L' Pc X = B  ==> Pc X = inv(L') * inv(U') * B, but we want
 *    X, not Pc X, so we apply inv(Pc) after doing these steps.
 */
      else
      {
         cblas_trsm(Order, CblasLeft, CblasUpper, Trans, CblasNonUnit,
                    N, NRHS, one, A, lda, B, ldb);
         cblas_trsm(Order, CblasLeft, CblasLower, Trans, CblasUnit,
                    N, NRHS, one, A, lda, B, ldb);
         ATL_laswp(NRHS, B, ldb, 0, N, ipiv, -1);
      }
   }
/*
 * For row-major arrays, we actually have X^T and B^T, so must tranpose
 * both sides of equation, so what we are solving is:  X' * A' = B'
 */
   else
   {
/*
 *    A = LU*inv(Pc), X' * (LU*inv(Pc))' = B'  ==>  X' * inv(Pc) * U' * L' = B'
 *    X' * inv(Pc) = U' * L' * B', so apply inv(Pc) after solves.
 */
      if (Trans == CblasNoTrans)
      {
         cblas_trsm(Order, CblasRight, CblasLower, CblasTrans, CblasNonUnit,
                    NRHS, N, one, A, lda, B, ldb);
         cblas_trsm(Order, CblasRight, CblasUpper, CblasTrans, CblasUnit,
                    NRHS, N, one, A, lda, B, ldb);
         ATL_laswp(NRHS, B, ldb, 0, N, ipiv, -1);
      }
/*
 *    A' = (LU*inv(Pc))', but Pc is on rows of non-trans matrix, so:
 *    X' * (inv(Pr)*L*U) = B'
 *    X' = (Pr * B') * inv(U) * inv(L)
 *    NOTE: this case is untested
 */
      else
      {
         ATL_laswp(NRHS, B, ldb, 0, N, ipiv, 1);
         cblas_trsm(Order, CblasRight, CblasUpper, CblasNoTrans, CblasUnit,
                    NRHS, N, one, A, lda, B, ldb);
         cblas_trsm(Order, CblasRight, CblasLower, CblasNoTrans, CblasNonUnit,
                    NRHS, N, one, A, lda, B, ldb);
      }
   }
}
