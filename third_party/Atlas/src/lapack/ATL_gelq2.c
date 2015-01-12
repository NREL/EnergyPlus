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
 *      SUBROUTINE DGELQ2( M, N, A, LDA, TAU, WORK, INFO )
 *
 * ATL_gelq2.c :
 * int ATL_gelq2( const int M, const int N, TYPE *A, int LDA,
 *                                                      TYPE  *TAU, TYPE *WORK)
 *     NOTE :a)   ATL_gelq2.c will get compiled to four precisions
 *               single precision real,      double precision real
 *               single precision complex,   double precision complex
 *
 *           b) This routine will not validate the input parameters.
 *  Purpose
 *  =======
 *
 *  ATL_gelq2  computes an LQ factorization of a real m by n matrix A:
 *  A = L * Q.
 *
 *  Arguments
 *  =========
 *
 *          (unitary matrix incase of complex precision )  as a
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
 *  A       (input/output) array, dimension (LDA,N)
 *          On entry, the m by n matrix A.
 *          On exit, the elements on and below the diagonal of the array
 *          contain the m by min(m,n) lower trapezoidal matrix L (L is
 *          lower triangular if m <= n); the elements above the diagonal,
 *          with the array TAU, represent the orthogonal matrix Q
 *          (unitary matrix incase of complex precision )  as a
 *          product of elementary reflectors (see Further Details).
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
 *     Q = H(k) . . . H(2) H(1), where k = min(m,n).    ( for Real precision)
 *     Q = H(1)' H(2)' . . . H(k)', where k = min(m,n). ( for Complex Precison)
 *         (Note : Conjugate Transpose of H is taken above)
 *
 *
 *  Each H(i) has the form
 *
 *     H(i) = I - tau * v * v'
 *
 *  where tau is a real/complex scalar, and v is a real/complex vector with
 *  v(1:i-1) = 0 and v(i) = 1; v(i+1:n) is stored on exit in A(i,i+1:n),
 *  and tau in TAU(i).
 */
#include "atlas_misc.h"
#include "cblas.h"
#include "atlas_lapack.h"

int ATL_gelq2(ATL_CINT M, ATL_CINT N, TYPE *A, ATL_CINT LDA,
              TYPE  *TAU, TYPE *WORK)
{
   const int lda2 = LDA  SHIFT;             /* for complex LDA*2              */

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

   for (i=0; i<K; i++)
   {
/*
 *    Generate elementary reflector H(i) to annihilate A(i,i+1:n)
 */
      #ifdef TCPLX
/*       Applicable only to Complex Numbers : Make Conjugate                  */
         ATL_lacgv(N-i, (A+(i SHIFT)+i*lda2), LDA);   /* V vextor             */
      #endif
      int t=((i+1)<(N-1))?(i+1):(N-1);      /* t = min(i+1, N-1)              */

      ATL_larfg( (N-i), (A+(i SHIFT) + i*lda2), (A+ (i SHIFT) + (t*lda2) ),
                 LDA, (TAU+(i SHIFT)) );

      if (i < (M-1))                        /* If not last column,            */
      {
/*
 *      Apply H(i) to A(i+1:m,i:n) from the right
 *
 */
         #ifdef TREAL
            AII = A[i+i*lda2];
            A[i+i*lda2] = ONE;
            TAUVAL = TAU[i];
         #else
            AII[0] = A[(i SHIFT)+i*lda2];
            AII[1] = A[(i SHIFT)+i*lda2 + 1];

            A[(i SHIFT)+i*lda2] = ONE[0];
            A[(i SHIFT)+i*lda2 + 1] = ONE[1];

            TAUVAL[0] = TAU[i SHIFT];
            TAUVAL[1] = TAU[(i SHIFT) + 1];
         #endif

         ATL_larf(CblasRight, M-i-1, N-i, (A+(i SHIFT)+i*lda2), LDA,
                  TAUVAL, ( A+ ((i+1) SHIFT) + i*lda2 ) , LDA, WORK);

/*       Reassign the values of A[i]                                          */
         #ifdef TREAL
            A[(i SHIFT)+i*lda2] = AII;
         #else
            A[(i SHIFT) +i*lda2] = AII[0];
            A[(i SHIFT) +i*lda2 + 1] = AII[1];
         #endif
      }
      #ifdef TCPLX
/*       Applicable only to Complex Numbers : Make Conjugate                  */
         ATL_lacgv(N-i, (A+(i SHIFT)+i*lda2), LDA);
      #endif
   }                                        /* end of for                     */

   return(0);
}                                           /* END ATL_gelq2                  */


