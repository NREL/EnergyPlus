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

void ATL_zrefhemv
(
   const enum ATLAS_UPLO      UPLO,
   const int                  N,
   const double               * ALPHA,
   const double               * A,
   const int                  LDA,
   const double               * X,
   const int                  INCX,
   const double               * BETA,
   double                     * Y,
   const int                  INCY
)
{
/*
 * Purpose
 * =======
 *
 * ATL_zrefhemv performs the matrix-vector operation
 *
 *    y := alpha * A * x + beta * y,
 *
 * where alpha and beta are scalars, x and y are n-element vectors and A
 * is an n by n Hermitian matrix.
 *
 * Arguments
 * =========
 *
 * UPLO    (input)                       const enum ATLAS_UPLO
 *         On entry, UPLO  specifies whether the upper or lower triangu-
 *         lar part of the array A is to be referenced as follows:
 *
 *             UPLO = AtlasUpper   Only the upper triangular part of A
 *                                 is to be referenced.
 *
 *             UPLO = AtlasLower   Only the lower triangular part of A
 *                                 is to be referenced.
 *
 *         Unchanged on exit.
 *
 * N       (input)                       const int
 *         On entry, N specifies the order of the matrix A. N must be at
 *         least zero. Unchanged on exit.
 *
 * ALPHA   (input)                       const double *
 *         On entry, ALPHA specifies the scalar alpha.   When  ALPHA  is
 *         supplied as zero then  A and X  need not be set on input. Un-
 *         changed on exit.
 *
 * A       (input)                       const double *
 *         On entry,  A  points  to an array of size equal to or greater
 *         than   LDA * n * sizeof( double[2] ).   Before   entry   with
 *         UPLO = AtlasUpper, the leading  n by n  upper triangular part
 *         of the array  A must contain the upper triangular part of the
 *         Hermitian matrix  and  the strictly  lower triangular part of
 *         A is not referenced. Before entry with UPLO = AtlasLower, the
 *         leading  n by n  lower  triangular part of the array  A  must
 *         contain the lower triangular part of the Hermitian matrix and
 *         the  strictly upper triangular part of  A  is not referenced.
 *         Unchanged on exit.
 *         Note that the  imaginary parts  of the local entries  corres-
 *         ponding to the diagonal elements of A need not be set and as-
 *         sumed to be zero.
 *
 * LDA     (input)                       const int
 *         On entry, LDA  specifies the leading dimension of A as decla-
 *         red  in  the  calling  (sub) program.  LDA  must be  at least
 *         MAX( 1, n ). Unchanged on exit.
 *
 * X       (input)                       const double *
 *         On entry,  X  points to the  first entry to be accessed of an
 *         incremented array of size equal to or greater than
 *            ( 1 + ( n - 1 ) * abs( INCX ) ) * sizeof( double[2] ),
 *         that contains the vector x. Unchanged on exit.
 *
 * INCX    (input)                       const int
 *         On entry, INCX specifies the increment for the elements of X.
 *         INCX must not be zero. Unchanged on exit.
 *
 * BETA    (input)                       const double *
 *         On entry,  BETA  specifies the scalar  beta.   When  BETA  is
 *         supplied as zero then Y  need not be set on input.  Unchanged
 *         on exit.
 *
 * Y       (input/output)                double *
 *         On entry,  Y  points to the  first entry to be accessed of an
 *         incremented array of size equal to or greater than
 *            ( 1 + ( n - 1 ) * abs( INCY ) ) * sizeof( double[2] ),
 *         that contains the vector y.  Before entry with BETA non-zero,
 *         the incremented array  Y  must contain the vector y. On exit,
 *         Y is overwritten by the updated vector y.
 *
 * INCY    (input)                       const int
 *         On entry, INCY specifies the increment for the elements of Y.
 *         INCY must not be zero. Unchanged on exit.
 *
 * ---------------------------------------------------------------------
 */
/* ..
 * .. Executable Statements ..
 *
 */
   if( ( N == 0 ) ||
       ( Mdzero( ALPHA[0], ALPHA[1] ) && Mdone( BETA[0], BETA[1] ) ) ) return;
   if( Mdzero( ALPHA[0], ALPHA[1] ) )
   { Mzvscal( N, BETA, Y, INCY ); return; }

   if( UPLO == AtlasUpper )
   {
      ATL_zrefhemvU( N,    ALPHA, A, LDA, X, INCX, BETA, Y, INCY );
   }
   else
   {
      ATL_zrefhemvL( N,    ALPHA, A, LDA, X, INCX, BETA, Y, INCY );
   }
/*
 * End of ATL_zrefhemv
 */
}
