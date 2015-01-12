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
#include "atlas_reflvl3.h"
#include "atlas_reflevel3.h"

void ATL_drefsymm
(
   const enum ATLAS_SIDE      SIDE,
   const enum ATLAS_UPLO      UPLO,
   const int                  M,
   const int                  N,
   const double               ALPHA,
   const double               * A,
   const int                  LDA,
   const double               * B,
   const int                  LDB,
   const double               BETA,
   double                     * C,
   const int                  LDC
)
{
/*
 * Purpose
 * =======
 *
 * ATL_drefsymm  performs one of the matrix-matrix operations
 *
 *    C := alpha * A * B + beta * C,
 *
 * or
 *
 *    C := alpha * B * A + beta * C,
 *
 * where alpha and beta are scalars,  A is a symmetric matrix and B and
 * C are m by n matrices.
 *
 * Arguments
 * =========
 *
 * SIDE    (input)                       const enum ATLAS_SIDE
 *         On entry,  SIDE  specifies  whether the  symmetric  matrix  A
 *         appears  on  the left or right in the operation as follows:
 *
 *            SIDE = AtlasLeft     C := alpha * A * B + beta * C,
 *
 *            SIDE = AtlasRight    C := alpha * B * A + beta * C.
 *
 *         Unchanged on exit.
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
 * M       (input)                       const int
 *         On entry,  M  specifies  the number  of rows of the matrix C.
 *         M  must be at least zero. Unchanged on exit.
 *
 * N       (input)                       const int
 *         On entry, N  specifies the number of columns of the matrix C.
 *         N must be at least zero. Unchanged on exit.
 *
 * ALPHA   (input)                       const double
 *         On entry, ALPHA specifies the scalar alpha.   When  ALPHA  is
 *         supplied  as  zero  then the elements of the matrices A and B
 *         need not be set on input.
 *
 * A       (input)                       const double *
 *         On entry,  A  points  to an array of size equal to or greater
 *         than   LDA * ka * sizeof(   double  ),  where ka  is  m  when
 *         SIDE = AtlasLeft   and is  n  otherwise.  Before  entry  with
 *         SIDE = AtlasLeft, the  m by m  part of the  array A must con-
 *         tain the symmetric matrix, such that when  UPLO = AtlasUpper,
 *         the leading m by m upper triangular part of the array A  must
 *         contain the upper triangular part of the symmetric matrix and
 *         the  strictly lower triangular part of  A  is not referenced,
 *         and when  UPLO = AtlasLower, the  leading m by m lower trian-
 *         gular part of the array A must contain the  lower  triangular
 *         part of the symmetric matrix and the  strictly upper triangu-
 *         lar part of  A  is not referenced.
 *         Before  entry  with  SIDE = AtlasRight,  the  n by n  part of
 *         the  array  A  must contain the  symmetric matrix,  such that
 *         when  UPLO = AtlasUpper, the  leading n by n upper triangular
 *         part of the array  A  must  contain the upper triangular part
 *         of the  symmetric matrix  and the  strictly  lower triangular
 *         part of  A  is not  referenced,  and when  UPLO = AtlasLower,
 *         the leading  n by n  lower  triangular part  of the  array  A
 *         must  contain  the  lower  triangular part  of the  symmetric
 *         matrix and the  strictly  upper triangular part of  A  is not
 *         referenced. Unchanged on exit.
 *
 * LDA     (input)                       const int
 *         On entry, LDA  specifies the leading dimension of A as decla-
 *         red  in  the  calling  (sub) program.  LDA  must be  at least
 *         MAX( 1, m ) when SIDE = AtlasLeft, and MAX( 1, n ) otherwise.
 *         Unchanged on exit.
 *
 * B       (input)                       const double *
 *         On entry,  B  points  to an array of size equal to or greater
 *         than   LDB * n * sizeof(   double  ).  Before entry, the lea-
 *         ding m by n  part of the array  B  must contain the matrix B.
 *         Unchanged on exit.
 *
 * LDB     (input)                       const int
 *         On entry, LDB  specifies the leading dimension of B as decla-
 *         red  in  the  calling  (sub) program.  LDB  must be  at least
 *         MAX( 1, m ). wise. Unchanged on exit.
 *
 * BETA    (input)                       const double
 *         On entry,  BETA  specifies the scalar  beta.   When  BETA  is
 *         supplied  as  zero  then  the  elements of the matrix C  need
 *         not be set on input. Unchanged on exit.
 *
 * C       (input/output)                double *
 *         On entry,  C  points  to an array of size equal to or greater
 *         than   LDC * n * sizeof(   double  ).  Before entry, the lea-
 *         ding m by n part of the array  C  must contain the matrix  C,
 *         except when beta is zero,  in which case C need not be set on
 *         entry.  On exit, the array C is overwritten by the m by n up-
 *         dated matrix.
 *
 * LDC     (input)                       const int
 *         On entry, LDC  specifies the leading dimension of A as decla-
 *         red  in  the  calling  (sub) program.  LDC  must be  at least
 *         MAX( 1, m ). Unchanged on exit.
 *
 * ---------------------------------------------------------------------
 */
/* ..
 * .. Executable Statements ..
 *
 */
   if( ( M == 0 ) || ( N == 0 ) ||
       ( ( ALPHA == ATL_dZERO ) && ( BETA  == ATL_dONE  ) ) ) return;

   if( ALPHA == ATL_dZERO )
   { Mdgescal( M, N, BETA, C, LDC ); return; }

   if( SIDE == AtlasLeft )
   {
      if( UPLO == AtlasUpper )
      { ATL_drefsymmLU( M, N, ALPHA, A, LDA, B, LDB, BETA, C, LDC ); }
      else
      { ATL_drefsymmLL( M, N, ALPHA, A, LDA, B, LDB, BETA, C, LDC ); }
   }
   else
   {
      if( UPLO == AtlasUpper )
      { ATL_drefsymmRU( M, N, ALPHA, A, LDA, B, LDB, BETA, C, LDC ); }
      else
      { ATL_drefsymmRL( M, N, ALPHA, A, LDA, B, LDB, BETA, C, LDC ); }
   }
/*
 * End of ATL_drefsymm
 */
}
