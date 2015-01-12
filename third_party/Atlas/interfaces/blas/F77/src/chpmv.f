      SUBROUTINE CHPMV( UPLO, N, ALPHA, A, X, INCX, BETA, Y, INCY )
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
      INTEGER            INCX, INCY, N
      COMPLEX            ALPHA, BETA
*     ..
*     .. Array Arguments ..
      COMPLEX            A( * ), X( * ), Y( * )
*     ..
*
*  Purpose
*  =======
*
*  CHPMV performs the matrix-vector operation
*
*     y := alpha*A*x + beta*y,
*
*  where alpha and beta are scalars, x and y are n-element vectors and A
*  is an n by n Hermitian matrix, supplied in packed form.
*
*  Arguments
*  =========
*
*  UPLO    (input)                       CHARACTER*1
*          On entry, UPLO  specifies whether the upper or lower triangu-
*          lar part of the matrix A is supplied in the packed array A
*          as follows:
*
*              UPLO = 'U' or 'u'   The upper triangular part of A is
*                                  supplied in A.
*
*              UPLO = 'L' or 'l'   The lower triangular part of A is
*                                  supplied in A.
*
*          Unchanged on exit.
*
*  N       (input)                       INTEGER
*          On entry, N specifies the order of the matrix A. N must be at
*          least zero. Unchanged on exit.
*
*  ALPHA   (input)                       COMPLEX
*          On entry, ALPHA specifies the scalar alpha.   When  ALPHA  is
*          supplied as zero then  A and X  need not be set on input. Un-
*          changed on exit.
*
*  A       (input)                       COMPLEX array
*          On entry,  A  is an array of DIMENSION ( ( n*( n + 1 ) )/2 ).
*          Before entry with UPLO = 'U' or 'u', the array A must contain
*          the upper triangular  part of the Hermitian matrix packed se-
*          quentially, column by column, so that A( 1 ) contains a(1,1),
*          A( 2 ) and A( 3 ) contain a(1,2) and a(2,2) respectively, and
*          so on.
*          Before entry with UPLO = 'L' or 'l', the array A must contain
*          the lower triangular  part of the Hermitian matrix packed se-
*          quentially, column by column, so that A( 1 ) contains a(1,1),
*          A( 2 ) and A( 3 ) contain a(2,1) and a(3,1) respectively, and
*          so on.
*          Note that the  imaginary parts  of the local entries  corres-
*          ponding to the diagonal elements of A need not be set and as-
*          sumed to be zero. Unchanged on exit.
*
*  X       (input)                       COMPLEX array
*          On entry,  X  is an incremented array of dimension  at  least
*          ( 1 + ( n - 1 ) * abs( INCX ) ).  Before entry, the incremen-
*          ted array X must contain the vector x. Unchanged on exit.
*
*  INCX    (input)                       INTEGER
*          On entry, INCX specifies the increment for the elements of X.
*          INCX must not be zero. Unchanged on exit.
*
*  BETA    (input)                       COMPLEX
*          On entry,  BETA  specifies the scalar  beta.   When  BETA  is
*          supplied as zero then Y  need not be set on input.  Unchanged
*          on exit.
*
*  Y       (input/output)                COMPLEX array
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
      EXTERNAL           ATL_F77WRAP_CHPMV, XERBLA
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
         ELSE IF( INCX.EQ.0 ) THEN
            INFO = 6
         ELSE IF( INCY.EQ.0 ) THEN
            INFO = 9
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'CHPMV ', INFO )
         RETURN
      END IF
*
      CALL ATL_F77WRAP_CHPMV( IUPLO, N, ALPHA, A, X, INCX, BETA,
     $                        Y, INCY )
*
      RETURN
*
*     End of CHPMV
*
      END
