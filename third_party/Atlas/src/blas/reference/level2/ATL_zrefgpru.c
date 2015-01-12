/* ---------------------------------------------------------------------
 *
 * -- Automatically Tuned Linear Algebra Software (ATLAS)
 *    (C) Copyright 2000 All Rights Reserved
 *
 * -- ATLAS routine -- Version 3.9.24 -- December 25, 2000
 *
 * Author         : Antoine P. Petitet
 * Originally developed at the University of Tennessee,
 * Innovative Computing Laboratory, Knoxville TN, 37996-1301, USA.
 *
 * ---------------------------------------------------------------------
 *
 * -- Copyright notice and Licensing terms:
 *
 *  Redistribution  and  use in  source and binary forms, with or without
 *  modification, are  permitted provided  that the following  conditions
 *  are met:
 *
 * 1. Redistributions  of  source  code  must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce  the above copyright
 *    notice,  this list of conditions, and the  following disclaimer in
 *    the documentation and/or other materials provided with the distri-
 *    bution.
 * 3. The name of the University,  the ATLAS group,  or the names of its
 *    contributors  may not be used to endorse or promote products deri-
 *    ved from this software without specific written permission.
 *
 * -- Disclaimer:
 *
 * THIS  SOFTWARE  IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,  INCLUDING,  BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,  INDIRECT, INCIDENTAL, SPE-
 * CIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO,  PROCUREMENT  OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
 * OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEO-
 * RY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT  (IN-
 * CLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * ---------------------------------------------------------------------
 */
/*
 * Include files
 */
#include "atlas_refmisc.h"
#include "atlas_reflvl2.h"
#include "atlas_reflevel2.h"

void ATL_zrefgpru
(
   const enum ATLAS_UPLO      UPLO,
   const int                  M,
   const int                  N,
   const double               * ALPHA,
   const double               * X,
   const int                  INCX,
   const double               * Y,
   const int                  INCY,
   double                     * A,
   const int                  LDA
)
{
/*
 * Purpose
 * =======
 *
 * ATL_zrefgpru performs the rank 1 operation
 *
 *    A := alpha * x * y' + A,
 *
 * where alpha is a scalar,  x is an m-element vector, y is an n-element
 * vector and A is an m by n packed matrix.
 *
 * Arguments
 * =========
 *
 * UPLO    (input)                       const enum ATLAS_UPLO
 *         On entry, UPLO  specifies whether the array A contains an up-
 *         per or lower packed submatrix as follows:
 *
 *             UPLO = AtlasUpper   A is an upper-packed submatrix,
 *
 *             UPLO = AtlasLower   A is a  lower-packed submatrix.
 *
 *         Unchanged on exit.
 *
 * M       (input)                       const int
 *         On entry,  M  specifies the number of rows of  the matrix  A.
 *         M must be at least zero. Unchanged on exit.
 *
 * N       (input)                       const int
 *         On entry, N  specifies the number of columns of the matrix A.
 *         N  must be at least zero. Unchanged on exit.
 *
 * ALPHA   (input)                       const double *
 *         On entry, ALPHA specifies the scalar alpha.   When  ALPHA  is
 *         supplied as zero then the arrays X and Y need not be set on
 *         input. Unchanged on exit.
 *
 * X       (input)                       const double *
 *         On entry,  X  points to the  first entry to be accessed of an
 *         incremented array of size equal to or greater than
 *            ( 1 + ( m - 1 ) * abs( INCX ) ) * sizeof( double[2] ),
 *         that contains the vector x. Unchanged on exit.
 *
 * INCX    (input)                       const int
 *         On entry, INCX specifies the increment for the elements of X.
 *         INCX must not be zero. Unchanged on exit.
 *
 * Y       (input)                       const double *
 *         On entry,  Y  points to the  first entry to be accessed of an
 *         incremented array of size equal to or greater than
 *            ( 1 + ( n - 1 ) * abs( INCY ) ) * sizeof( double[2] ),
 *         that contains the vector y. Unchanged on exit.
 *
 * INCY    (input)                       const int
 *         On entry, INCY specifies the increment for the elements of Y.
 *         INCY must not be zero. Unchanged on exit.
 *
 * A       (input/output)                double *
 *         On entry,  A  points  to an array of size equal to or greater
 *         than  ( LDA * n - sum( 1 .. n-1, k ) ) * sizeof( double[2] ).
 *         Before entry with UPLO = AtlasUpper, the array A must contain
 *         the entries of the matrix packed sequentially,  column by co-
 *         lumn, so that A[0] contains a(0,0),  A[1]  and  A[2]  contain
 *         a(1,0) and a(2,0), A[LDA] and A[2*LDA+1]  contain  a(0,1) and
 *         a(0,2) respectively. Before entry with UPLO = AtlasLower, the
 *         array A must contain the entries of the matrix packed sequen-
 *         tially, column by column, so that A[0] contains a(0,0),  A[1]
 *         and  A[2]  contain  a(1,0) and a(2,0),  A[LDA] and A[2*LDA-1]
 *         contain  a(1,1) and a(2,2) respectively, and so on.  On exit,
 *         A is overwritten by the updated matrix.
 *
 * LDA     (input)                       const int
 *         On entry, LDA  specifies the length of the first column of A.
 *         LDA  must be  at least MAX( 1, m ) when  TRANS = AtlasNotrans
 *         or TRANS = AtlasConj, and MAX( 1, n ) otherwise. Unchanged on
 *         exit.
 *
 * ---------------------------------------------------------------------
 */
/* ..
 * .. Executable Statements ..
 *
 */
   if( ( M == 0 ) || ( N == 0 ) || Mdzero( ALPHA[0], ALPHA[1] ) ) return;

   if( UPLO == AtlasLower )
   { ATL_zrefgpruL( M, N, ALPHA, X, INCX, Y, INCY, A, LDA ); }
   else
   { ATL_zrefgpruU( M, N, ALPHA, X, INCX, Y, INCY, A, LDA ); }
/*
 * End of ATL_zrefgpru
 */
}

void ATL_zrefgpruL
(
   const int                  M,
   const int                  N,
   const double               * ALPHA,
   const double               * X,
   const int                  INCX,
   const double               * Y,
   const int                  INCY,
   double                     * A,
   const int                  LDA
)
{
/*
 * .. Local Variables ..
 */
   register double            t0_i, t0_r;
   int                        i, iaij, incx2 = 2 * INCX, incy2 = 2 * INCY,
                              ix, j, jaj, jy, lda2 = ( LDA << 1 );
/* ..
 * .. Executable Statements ..
 *
 */
   for( j = 0, jaj = 0, jy = 0; j < N; j++, jy += incy2 )
   {
      Mmul( ALPHA[0], ALPHA[1], Y[jy],  Y[jy+1], t0_r, t0_i );
      for( i = 0, iaij = jaj, ix = 0; i < M; i++, iaij += 2, ix += incx2 )
      { Mmla( X[ix], X[ix+1], t0_r, t0_i, A[iaij], A[iaij+1] ); }
      lda2 -= 2; jaj += lda2;
   }
/*
 * End of ATL_zrefgpruL
 */
}

void ATL_zrefgpruU
(
   const int                  M,
   const int                  N,
   const double               * ALPHA,
   const double               * X,
   const int                  INCX,
   const double               * Y,
   const int                  INCY,
   double                     * A,
   const int                  LDA
)
{
/*
 * .. Local Variables ..
 */
   register double            t0_i, t0_r;
   int                        i, iaij, incx2 = 2 * INCX, incy2 = 2 * INCY,
                              ix, j, jaj, jy, lda2 = ( LDA << 1 );
/* ..
 * .. Executable Statements ..
 *
 */
   for( j = 0, jaj = 0, jy = 0; j < N; j++, jy += incy2 )
   {
      Mmul( ALPHA[0], ALPHA[1], Y[jy],  Y[jy+1], t0_r, t0_i );
      for( i = 0, iaij = jaj, ix = 0; i < M; i++, iaij += 2, ix += incx2 )
      { Mmla( X[ix], X[ix+1], t0_r, t0_i, A[iaij], A[iaij+1] ); }
      jaj += lda2; lda2 += 2;
   }
/*
 * End of ATL_zrefgpruU
 */
}
