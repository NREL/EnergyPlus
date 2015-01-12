/*
 *             Automatically Tuned Linear Algebra Software v3.10.2
 * Copyright (C) 2009 Siju Samuel
 *
 * Code contributers : Siju Samuel, Anthony M. Castaldo, R. Clint Whaley
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

/*
 * This is the C translation of the standard LAPACK Fortran routine:
 *     SUBROUTINE DGERQ2( M, N, A, LDA, TAU, WORK, INFO )
 *
 * ATL_gerq2.c :
 * int ATL_gerq2( const int M, const int N, TYPE *A, int LDA,
 *                                                      TYPE  *TAU, TYPE *WORK)
 *     NOTE :a)   ATL_gerq2.c will get compiled to four precisions
 *               single precision real,      double precision real
 *               single precision complex,   double precision complex
 *
 *           b) This routine will not validate the input parameters.
 *  Purpose
 *  =======
 *
 *  ATL_gerq2  computes a QR factorization of a real/complex  m by n matrix A:
 *  A = Q * R.
 *
 *  Arguments
 *  =========
 *
 *  M       (input) INTEGER
 *          The number of rows of the matrix A.  M >= 0.
 *
 *  N       (input) INTEGER
 *          The number of columns of the matrix A.  N >= 0.
 *
 *  A       (input/output)  array, dimension (LDA,N)
 *          On entry, the m by n matrix A.
 *          On exit, if m <= n, the upper triangle of the subarray
 *          A(1:m,n-m+1:n) contains the m by m upper triangular matrix R;
 *          if m >= n, the elements on and above the (m-n)-th subdiagonal
 *          contain the m by n upper trapezoidal matrix R; the remaining
 *          elements, with the array TAU, represent the orthogonal matrix
 *          (unitary matrix incase of complex precision )  as a
 *          as a product of elementary reflectors (see Further
 *          Details).
 *
 *  LDA     (input) INTEGER
 *          The leading dimension of the array A.  LDA >= max(1,M).
 *
 *  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
 *          The scalar factors of the elementary reflectors (see Further
 *          Details).
 *
 *  WORK    (workspace) DOUBLE PRECISION array, dimension (M)
 *
 *  INFO    (output) INTEGER
 *          = 0: successful exit
 *          < 0: if INFO = -i, the i-th argument had an illegal value
 *
 *  Further Details
 *  ===============
 *
 *  The matrix Q is represented as a product of elementary reflectors
 *
 *     Q = H(1) H(2) . . . H(k), where k = min(m,n).    (for Real precison)
 *     Q = H(1)' H(2)' . . . H(k)', where k = min(m,n). (for Complex Precison)
 *         (Note : Conjugate Transpose of H is taken above)
 *
 *  Each H(i) has the form
 *
 *     H(i) = I - tau * v * v'                 (for Real Precision)
 *     H(i) = I - tau * v * conjugate(v)'      (for Complex  Precision)
 *
 *  where tau is a real scalar, and v is a real vector with
 *  v(n-k+i+1:n) = 0 and v(n-k+i) = 1; v(1:n-k+i-1) is stored on exit in
 *  A(m-k+i,1:n-k+i-1), and tau in TAU(i).
 */

#include "atlas_misc.h"
#include "cblas.h"
#include "atlas_lapack.h"

int ATL_gerq2(ATL_CINT M, ATL_CINT N, TYPE *A, ATL_CINT LDA,
              TYPE *TAU, TYPE *WORK)
{
   int lda2 = LDA  SHIFT;                   /* for complex LDA*2              */

   #ifdef TREAL
      const TYPE ONE = ATL_rone;
      TYPE AII ;
      TYPE TAUVAL ;
   #else
      const TYPE ONE[2] = {ATL_rone, ATL_rzero};
      TYPE AII[2];
      TYPE TAUVAL[2] ;
   #endif

   int i, K;
   K = (M < N)?M:N;                         /* k is min(M,N)                  */

   for (i=K-1; i>=0 ; i--)
   {
/*
 *    Generate elementary reflector H(i) to annihilate
 *    A(m-k+i,1:n-k+i-1)
 */
      #ifdef TCPLX
/*       Applicable only to Complex Numbers                                   */
         ATL_lacgv(N-K+i+1, (A + ((M-K+i) SHIFT)), LDA);
      #endif

      ATL_larfg((N-K+i+1), (A +((M-K+i) SHIFT) + (N-K+i)*lda2 ),
                (A + ((M-K+i) SHIFT)), LDA, (TAU+(i SHIFT)) );

/*
 *    Apply H(i) to A(1:m-k+i-1,1:n-k+i) from the right
 */

      #ifdef TREAL
         AII = *(A + ((M-K+i) SHIFT)  + (N-K+i)*lda2 );
         *(A + ((M-K+i) SHIFT)  + (N-K+i)*lda2 ) = ONE;
         TAUVAL = TAU[i];
      #else
         AII[0] = *(A + ((M-K+i) SHIFT)  + (N-K+i)*lda2 );
         AII[1] = *(A + ((M-K+i) SHIFT)  + (N-K+i)*lda2 + 1 );

         *(A + ((M-K+i) SHIFT)  + (N-K+i)*lda2 ) = ONE[0];
         *(A + ((M-K+i) SHIFT)  + (N-K+i)*lda2 + 1 ) = ONE[1];

         TAUVAL[0] = TAU[i SHIFT];
         TAUVAL[1] = TAU[(i SHIFT) + 1];
      #endif

      ATL_larf(CblasRight, M-K+i, N-K+i+1, (A+((M-K+i) SHIFT)), LDA, TAUVAL,
               (A), LDA, WORK);

/*    Reassign the values of A[i]                                             */
      #ifdef TREAL
         *(A + ((M-K+i) SHIFT)  + (N-K+i)*lda2 ) = AII;
      #else
         *(A + ((M-K+i) SHIFT)  + (N-K+i)*lda2 ) = AII[0];
         *(A + ((M-K+i) SHIFT)  + (N-K+i)*lda2 + 1 ) = AII[1];
      #endif

      #ifdef TCPLX
         ATL_lacgv(N-K+i, (A + ((M-K+i) SHIFT)), LDA);
      #endif

   }                                        /* end of for                     */

   return(0);
}                                           /* END AL_gerq2                   */
