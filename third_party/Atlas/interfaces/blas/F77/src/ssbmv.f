      SUBROUTINE SSBMV( UPLO, N, K, ALPHA, A, LDA, X, INCX,
     $                  BETA, Y, INCY )
*
*  -- Automatically Tuned Linear Algebra Software (ATLAS)
*     (C) Copyright 2000 All Rights Reserved
*
*  -- ATLAS routine -- F77 Interface -- Version 3.9.24 -- December 25, 2000
*
*  Author         : Antoine P. Petitet
*  Originally developed at the University of Tennessee,
*  Innovative Computing Laboratory,  Knoxville TN, 37996-1301, USA.
*
*  ---------------------------------------------------------------------
*
*  -- Copyright notice and Licensing terms:
*
*  Redistribution  and  use in  source and binary forms, with or without
*  modification, are  permitted provided  that the following  conditions
*  are met:
*
*  1. Redistributions  of  source  code  must retain the above copyright
*     notice, this list of conditions and the following disclaimer.
*  2. Redistributions in binary form must reproduce  the above copyright
*     notice,  this list of conditions, and the  following disclaimer in
*     the documentation and/or other materials provided with the distri-
*     bution.
*  3. The name of the University,  the ATLAS group,  or the names of its
*     contributors  may not be used to endorse or promote products deri-
*     ved from this software without specific written permission.
*
*  -- Disclaimer:
*
*  THIS  SOFTWARE  IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
*  ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,  INCLUDING,  BUT NOT
*  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
*  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE UNIVERSITY
*  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,  INDIRECT, INCIDENTAL, SPE-
*  CIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
*  TO,  PROCUREMENT  OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
*  OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEO-
*  RY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT  (IN-
*  CLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
*  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*
*  ---------------------------------------------------------------------
*
*     .. Scalar Arguments ..
      CHARACTER*1        UPLO
      INTEGER            INCX, INCY, K, LDA, N
      REAL               ALPHA, BETA
*     ..
*     .. Array Arguments ..
      REAL               A( LDA, * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  SSBMV performs the matrix-vector operation
*
*     y := alpha*A*x + beta*y,
*
*  where alpha and beta are scalars, x and y are n-element vectors and A
*  is an n by n symmetric band matrix, with k super-diagonals.
*
*  Arguments
*  =========
*
*  UPLO    (input)                       CHARACTER*1
*          On entry, UPLO  specifies whether the upper or lower triangu-
*          lar part of the band matrix A is being supplied as follows:
*
*              UPLO = 'U' or 'u'   The upper triangular part of A is
*                                  being supplied.
*
*              UPLO = 'L' or 'l'   The lower triangular part of A is
*                                  being supplied.
*
*          Unchanged on exit.
*
*  N       (input)                       INTEGER
*          On entry, N specifies the order of the matrix A. N must be at
*          least zero. Unchanged on exit.
*
*  K       (input)                       INTEGER
*          On entry, K  specifies  the number of  super-diagonals of the
*          matrix A. K must satisfy 0 .le. K. Unchanged on exit.
*
*  ALPHA   (input)                       REAL
*          On entry, ALPHA specifies the scalar alpha.   When  ALPHA  is
*          supplied as zero then  A and X  need not be set on input. Un-
*          changed on exit.
*
*  A       (input)                       REAL array
*          On entry, A is an array of DIMENSION ( LDA, n ). Before entry
*          with UPLO = 'U' or 'u',  the leading (k + 1) by n part of the
*          array  A  must  contain the upper triangular band part of the
*          symmetric matrix, supplied column by column, with the leading
*          diagonal of the matrix in row (k + 1) of the array, the first
*          super-diagonal  starting at position 2 in row k,  and  so on.
*          The  top left k by k  triangle of the array A is not referen-
*          ced.  The following program  segment  will transfer the upper
*          triangular part of a symmetric band  matrix from conventional
*          full matrix storage to band storage:
*
*                DO 20, J = 1, N
*                   M = K + 1 - J
*                   DO 10, I = MAX( 1, J - K ), J
*                      A( M + I, J ) = matrix( I, J )
*             10    CONTINUE
*             20 CONTINUE
*
*          Before entry with  UPLO = 'L' or 'l', the leading (k+1)  by n
*          part of  the array A must contain the lower  triangular  band
*          part of the symmetric matrix, supplied column by column, with
*          the leading diagonal of the matrix in row 1 of the array, the
*          first sub-diagonal starting at position  1  in row  2, and so
*          on.  The bottom right  k by k  triangle of the array A is not
*          referenced.  The  following program segment will transfer the
*          lower triangular part of a symmetric band matrix from conven-
*          tional full matrix storage to band storage:
*
*                DO 20, J = 1, N
*                   M = 1 - J
*                   DO 10, I = J, MIN( N, J + K )
*                      A( M + I, J ) = matrix( I, J )
*             10    CONTINUE
*             20 CONTINUE
*
*          Unchanged on exit.
*
*  LDA     (input)                       INTEGER
*          On entry, LDA  specifies the first dimension of A as declared
*          in the calling (sub) program. LDA must be at least k + 1. Un-
*          changed on exit.
*
*  X       (input)                       REAL array
*          On entry,  X  is an incremented array of dimension  at  least
*          ( 1 + ( n - 1 ) * abs( INCX ) ).  Before entry, the incremen-
*          ted array X must contain the vector x. Unchanged on exit.
*
*  INCX    (input)                       INTEGER
*          On entry, INCX specifies the increment for the elements of X.
*          INCX must not be zero. Unchanged on exit.
*
*  BETA    (input)                       REAL
*          On entry,  BETA  specifies the scalar  beta.   When  BETA  is
*          supplied as zero then Y  need not be set on input.  Unchanged
*          on exit.
*
*  Y       (input/output)                REAL array
*          On entry,  Y  is an incremented array of dimension  at  least
*          ( 1 + ( n - 1 ) * abs( INCY ) ).  Before entry with BETA non-
*          zero, the incremented array  Y  must contain the vector y. On
*          exit, Y is overwritten by the updated vector y.
*
*  INCY    (input)                       INTEGER
*          On entry, INCY specifies the increment for the elements of Y.
*          INCY must not be zero. Unchanged on exit.
*
*  Further Details
*  ===============
*
*  For further information on the Level 1 BLAS specification, see:
*
*  ``A Proposal for Standard Linear Algebra Subprograms''  by R. Hanson,
*  F. Krogh and C. Lawson, ACM SIGNUM Newsl., 8(16), 1973,
*
*  ``Basic Linear Algebra Subprograms for Fortran Usage''  by C. Lawson,
*  R. Hanson, D. Kincaid and F. Krogh,  ACM Transactions on Mathematical
*  Software, 5(3) pp 308-323, 1979.
*
*  For further information on the Level 2 BLAS specification, see:
*
*  ``An  Extended Set of  FORTRAN  Basic Linear Algebra Subprograms'' by
*  J. Dongarra,  J. Du Croz,  S. Hammarling and R. Hanson,  ACM Transac-
*  tions on Mathematical Software, 14(1) pp 1-17, 1988.
*
*  ``Algorithm 656: An extended Set of Basic Linear Algebra Subprograms:
*  Model Implementation and Test Programs''  by J. Dongarra, J. Du Croz,
*  S. Hammarling and R. Hanson,  ACM  Transactions on Mathematical Soft-
*  ware, 14(1) pp 18-32, 1988.
*
*  For further information on the Level 3 BLAS specification, see:
*
*  ``A Set of Level 3 Basic Linear Algebra Subprograms'' by J. Dongarra,
*  J. Du Croz, I. Duff and S. Hammarling, ACM Transactions on Mathemati-
*  cal Software, 16(1), pp 1-17, 1990.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            ILOWER, IUPPER
      PARAMETER          ( IUPPER = 121, ILOWER = 122 )
*     ..
*     .. Local Scalars ..
      INTEGER            INFO, IUPLO
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATL_F77WRAP_SSBMV, XERBLA
*     ..
*     .. External Functions ..
      EXTERNAL           LSAME
      LOGICAL            LSAME
*     ..
*     .. Executable Statements ..
*
      INFO = 0
*
      IF(      LSAME( UPLO  , 'U' ) ) THEN
         IUPLO = IUPPER
      ELSE IF( LSAME( UPLO  , 'L' ) ) THEN
         IUPLO = ILOWER
      ELSE IF( INFO.EQ.0 ) THEN
         INFO  = 1
      END IF
*
      IF( INFO.EQ.0 ) THEN
         IF( N.LT.0 ) THEN
            INFO = 2
         ELSE IF( K.LT.0 ) THEN
            INFO = 3
         ELSE IF( LDA.LT.( K + 1 ) ) THEN
            INFO = 6
         ELSE IF( INCX.EQ.0 ) THEN
            INFO = 8
         ELSE IF( INCY.EQ.0 ) THEN
            INFO = 11
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SSBMV ', INFO )
         RETURN
      END IF
*
      CALL ATL_F77WRAP_SSBMV( IUPLO, N, K, ALPHA, A, LDA, X, INCX,
     $                        BETA, Y, INCY )
*
      RETURN
*
*     End of SSBMV
*
      END
