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

void ATL_sreftrmm
(
   const enum ATLAS_SIDE      SIDE,
   const enum ATLAS_UPLO      UPLO,
   const enum ATLAS_TRANS     TRANS,
   const enum ATLAS_DIAG      DIAG,
   const int                  M,
   const int                  N,
   const float                ALPHA,
   const float                * A,
   const int                  LDA,
   float                      * B,
   const int                  LDB
)
{
/*
 * Purpose
 * =======
 *
 * ATL_sreftrmm  performs one of the matrix-matrix operations
 *
 *    B := alpha * op( A ) * B,   or    B := alpha * B * op( A ),
 *
 * where alpha is a scalar, B is an m by n matrix, A is a unit,  or non-
 * unit, upper or lower triangular matrix and op( X ) is one of
 *
 *    op( X ) = X   or   op( X ) = X'.
 *
 * Arguments
 * =========
 *
 * SIDE    (input)                       const enum ATLAS_SIDE
 *         On entry,  SIDE  specifies whether  op( A ) multiplies B from
 *         the left or right as follows:
 *
 *            SIDE = AtlasLeft   B := alpha * op( A )* B,
 *
 *            SIDE = AtlasRight  B := alpha * B * op( A ).
 *
 *         Unchanged on exit.
 *
 * UPLO    (input)                       const enum ATLAS_UPLO
 *         On entry, UPLO  specifies whether  the  matrix is an upper or
 *         lower triangular matrix as follows:
 *
 *             UPLO = AtlasUpper   A is an upper triangular matrix.
 *
 *             UPLO = AtlasLower   A is a lower triangular matrix.
 *
 *         Unchanged on exit.
 *
 * TRANSA  (input)                       const enum ATLAS_TRANS
 *         On entry, TRANSA  specifies the form of op( A ) to be used in
 *         the matrix multiplication as follows:
 *
 *            TRANSA = AtlasNoTrans    op( A ) = A,
 *
 *            TRANSA = AtlasTrans      op( A ) = A',
 *
 *            TRANSA = AtlasConjTrans  op( A ) = A'.
 *
 *         Unchanged on exit.
 *
 * DIAG    (input)                       const enum ATLAS_DIAG
 *         On entry, DIAG specifies whether or not A is unit triangu-
 *         lar as follows:
 *
 *            DIAG = AtlasUnit       A is assumed to be unit triangular,
 *
 *            DIAG = AtlasNonUnit    A is not assumed to be unit trian-
 *                                   gular.
 *
 *         Unchanged on exit.
 *
 * M       (input)                       const int
 *         On entry,  M  specifies the number of rows of  the  matrix B.
 *         M  must be at least zero. Unchanged on exit.
 *
 * N       (input)                       const int
 *         On entry, N  specifies the number of columns of the matrix B.
 *         N  must be at least zero. Unchanged on exit.
 *
 * ALPHA   (input)                       const float
 *         On entry,  ALPHA  specifies  the scalar  alpha. When ALPHA is
 *         supplied as zero then the elements of the matrix B need   not
 *         be set on input. Unchanged on exit.
 *
 * A       (input)                       const float *
 *         On entry,  A  points  to an array of size equal to or greater
 *         than   LDA * k * sizeof(   float   ),   where  k  is  m  when
 *         SIDE = AtlasLeft  and  is  n  otherwise.  Before  entry  with
 *         UPLO = AtlasUpper,  the  leading k by k upper triangular part
 *         of the array  A  must contain the upper triangular matrix and
 *         the  strictly lower triangular part of  A  is not referenced.
 *         Before entry with UPLO = AtlasLower, the leading k by k lower
 *         triangular part of the array  A must contain the lower trian-
 *         gular  matrix and the strictly upper triangular part of  A is
 *         not referenced.
 *         Note  that when  DIAG = AtlasUnit,   the diagonal elements of
 *         A  are  not referenced either,  but are assumed to be  unity.
 *         Unchanged on exit.
 *
 * LDA     (input)                       const int
 *         On entry, LDA  specifies the leading dimension of A as decla-
 *         red  in  the  calling  (sub) program.  LDA  must be  at least
 *         MAX( 1, m ) when SIDE = AtlasLeft, and MAX( 1, n ) otherwise.
 *         Unchanged on exit.
 *
 * B       (input/output)                float *
 *         On entry,  B  points  to an array of size equal to or greater
 *         than   LDB * n * sizeof(   float   ).  Before entry, the lea-
 *         ding  m by n  part of the array B must contain the matrix  B,
 *         except when beta is zero,  in which case B need not be set on
 *         entry.  On exit, the array B is overwritten by the m by n up-
 *         dated matrix.
 *
 * LDB     (input)                       const int
 *         On entry, LDB  specifies the leading dimension of B as decla-
 *         red  in  the  calling  (sub) program.  LDB  must be  at least
 *         MAX( 1, m ). Unchanged on exit.
 *
 * ---------------------------------------------------------------------
 */
/* ..
 * .. Executable Statements ..
 *
 */
   if( ( M == 0 ) || ( N == 0 ) ) return;

   if( ALPHA == ATL_sZERO )
   { Msgescal( M, N, ALPHA, B, LDB ); return; }

   if( SIDE == AtlasLeft )
   {
      if( UPLO == AtlasUpper )
      {
         if( TRANS == AtlasNoTrans )
         {
            if( DIAG == AtlasNonUnit )
            {      ATL_sreftrmmLUNN( M, N, ALPHA, A, LDA, B, LDB ); }
            else { ATL_sreftrmmLUNU( M, N, ALPHA, A, LDA, B, LDB ); }
         }
         else
         {
            if( DIAG == AtlasNonUnit )
            {      ATL_sreftrmmLUTN( M, N, ALPHA, A, LDA, B, LDB ); }
            else { ATL_sreftrmmLUTU( M, N, ALPHA, A, LDA, B, LDB ); }
         }
      }
      else
      {
         if( TRANS == AtlasNoTrans )
         {
            if( DIAG == AtlasNonUnit )
            {      ATL_sreftrmmLLNN( M, N, ALPHA, A, LDA, B, LDB ); }
            else { ATL_sreftrmmLLNU( M, N, ALPHA, A, LDA, B, LDB ); }
         }
         else
         {
            if( DIAG == AtlasNonUnit )
            {      ATL_sreftrmmLLTN( M, N, ALPHA, A, LDA, B, LDB ); }
            else { ATL_sreftrmmLLTU( M, N, ALPHA, A, LDA, B, LDB ); }
         }
      }
   }
   else
   {
      if( UPLO == AtlasUpper )
      {
         if( TRANS == AtlasNoTrans )
         {
            if( DIAG == AtlasNonUnit )
            {      ATL_sreftrmmRUNN( M, N, ALPHA, A, LDA, B, LDB ); }
            else { ATL_sreftrmmRUNU( M, N, ALPHA, A, LDA, B, LDB ); }
         }
         else
         {
            if( DIAG == AtlasNonUnit )
            {      ATL_sreftrmmRUTN( M, N, ALPHA, A, LDA, B, LDB ); }
            else { ATL_sreftrmmRUTU( M, N, ALPHA, A, LDA, B, LDB ); }
         }
      }
      else
      {
         if( TRANS == AtlasNoTrans )
         {
            if( DIAG == AtlasNonUnit )
            {      ATL_sreftrmmRLNN( M, N, ALPHA, A, LDA, B, LDB ); }
            else { ATL_sreftrmmRLNU( M, N, ALPHA, A, LDA, B, LDB ); }
         }
         else
         {
            if( DIAG == AtlasNonUnit )
            {      ATL_sreftrmmRLTN( M, N, ALPHA, A, LDA, B, LDB ); }
            else { ATL_sreftrmmRLTU( M, N, ALPHA, A, LDA, B, LDB ); }
         }
      }
   }
/*
 * End of ATL_sreftrmm
 */
}
