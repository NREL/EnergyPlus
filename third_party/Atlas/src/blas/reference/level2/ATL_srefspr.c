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

void ATL_srefspr
(
   const enum ATLAS_UPLO      UPLO,
   const int                  N,
   const float                ALPHA,
   const float                * X,
   const int                  INCX,
   float                      * A
)
{
/*
 * Purpose
 * =======
 *
 * ATL_srefspr performs the symmetric rank 1 operation
 *
 *    A := alpha * x * x' + A,
 *
 * where  alpha is a scalar, x is an n-element vector and A is an n by n
 * symmetric matrix, supplied in packed form.
 *
 * Arguments
 * =========
 *
 * UPLO    (input)                       const enum ATLAS_UPLO
 *         On entry, UPLO  specifies whether the upper or lower triangu-
 *         lar part of the matrix A is supplied in the packed array A
 *         as follows:
 *
 *             UPLO = AtlasUpper   The upper triangular part of A is
 *                                 supplied in A.
 *
 *             UPLO = AtlasLower   The lower triangular part of A is
 *                                 supplied in A.
 *
 *         Unchanged on exit.
 *
 * N       (input)                       const int
 *         On entry, N specifies the order of the matrix A. N must be at
 *         least zero. Unchanged on exit.
 *
 * ALPHA   (input)                       const float
 *         On entry, ALPHA specifies the scalar alpha.   When  ALPHA  is
 *         supplied as  zero then  the array X need not be set on input.
 *         Unchanged on exit.
 *
 * X       (input)                       const float *
 *         On entry,  X  points to the  first entry to be accessed of an
 *         incremented array of size equal to or greater than
 *            ( 1 + ( n - 1 ) * abs( INCX ) ) * sizeof(   float   ),
 *         that contains the vector x. Unchanged on exit.
 *
 * INCX    (input)                       const int
 *         On entry, INCX specifies the increment for the elements of X.
 *         INCX must not be zero. Unchanged on exit.
 *
 * A       (input/output)                float *
 *         On entry,  A  points  to an array of size equal to or greater
 *         than   (( n*(n+1) ) / 2) * sizeof(   float   ). Before  entry
 *         with UPLO = AtlasUpper, the array  A  must  contain the upper
 *         triangular part of the symmetric matrix packed  sequentially,
 *         column  by  column,  so that A[0] contains a(0,0), A[ 1 ] and
 *         A[ 2 ] contain a(0,1) and a(1,1) respectively, and  so on. On
 *         exit, the array A is overwritten by the upper triangular part
 *         of the updated matrix.  Before entry with  UPLO = AtlasLower,
 *         the  array  A  must contain  the lower triangular part of the
 *         symmetric matrix  packed sequentially, column by  column,  so
 *         that A[ 0 ] contains a(0,0), A[ 1 ] and A[ 2 ] contain a(1,0)
 *         and a(2,0) respectively, and so on. On exit, the array  A  is
 *         overwritten by  the  lower triangular part of the updated ma-
 *         trix.
 *
 * ---------------------------------------------------------------------
 */
/* ..
 * .. Executable Statements ..
 *
 */
   if( ( N == 0 ) || ( ALPHA == ATL_sZERO ) ) return;

   if( UPLO == AtlasUpper )
   {
      ATL_srefsprU( N, ALPHA, X, INCX, A, 1   );
   }
   else
   {
      ATL_srefsprL( N, ALPHA, X, INCX, A, N   );
   }
/*
 * End of ATL_srefspr
 */
}
