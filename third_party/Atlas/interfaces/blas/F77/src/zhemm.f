      SUBROUTINE ZHEMM( SIDE, UPLO, M, N, ALPHA, A, LDA, B, LDB,
     $                  BETA, C, LDC )
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
      CHARACTER*1        SIDE, UPLO
      INTEGER            M, N, LDA, LDB, LDC
      COMPLEX*16         ALPHA, BETA
*     ..
*     .. Array Arguments ..
      COMPLEX*16         A( LDA, * ), B( LDB, * ), C( LDC, * )
*     ..
*
*  Purpose
*  =======
*
*  ZHEMM  performs one of the matrix-matrix operations
*
*     C := alpha * A * B + beta * C,
*
*  or
*
*     C := alpha * B * A + beta * C,
*
*  where alpha and beta are scalars,  A is a Hermitian matrix and B and
*  C are m by n matrices.
*
*  Arguments
*  =========
*
*  SIDE    (input)                       CHARACTER*1
*          On entry,  SIDE  specifies  whether the  Hermitian  matrix  A
*          appears  on  the left or right in the operation as follows:
*
*             SIDE = 'L' or 'l'    C := alpha * A * B + beta * C,
*
*             SIDE = 'R' or 'r'    C := alpha * B * A + beta * C.
*
*          Unchanged on exit.
*
*  UPLO    (input)                       CHARACTER*1
*          On entry, UPLO  specifies whether the upper or lower triangu-
*          lar part of the array A is to be referenced as follows:
*
*              UPLO = 'U' or 'u'   Only the upper triangular part of A
*                                  is to be referenced.
*
*              UPLO = 'L' or 'l'   Only the lower triangular part of A
*                                  is to be referenced.
*
*          Unchanged on exit.
*
*  M       (input)                       INTEGER
*          On entry,  M  specifies  the number  of rows of the matrix C.
*          M  must be at least zero. Unchanged on exit.
*
*  N       (input)                       INTEGER
*          On entry, N  specifies the number of columns of the matrix C.
*          N must be at least zero. Unchanged on exit.
*
*  ALPHA   (input)                       COMPLEX*16
*          On entry, ALPHA specifies the scalar alpha.   When  ALPHA  is
*          supplied  as  zero  then the elements of the matrices A and B
*          need not be set on input.
*
*  A       (input)                       COMPLEX*16 array
*          On entry, A is an array of DIMENSION ( LDA, ka ), where ka is
*          m  when  SIDE = 'L' or 'l'  and is  n otherwise.
*          Before  entry  with  SIDE = 'L' or 'l',  the  m by m  part of
*          the  array  A  must contain the  Hermitian matrix,  such that
*          when  UPLO = 'U'  or 'u', the leading m by m upper triangular
*          part  of the array  A  must contain the upper triangular part
*          of the  Hermitian  matrix and the  strictly  lower triangular
*          part of  A  is not  referenced,  and when  UPLO = 'L' or 'l',
*          the leading  m by m  lower  triangular part  of the  array  A
*          must  contain  the  lower  triangular part  of the  Hermitian
*          matrix and the  strictly  upper triangular part of  A  is not
*          referenced.
*          Before  entry  with  SIDE = 'R' or 'r',  the  n by n  part of
*          the  array  A  must contain the  Hermitian matrix,  such that
*          when  UPLO = 'U' or 'u', the  leading n by n upper triangular
*          part of the array  A  must  contain the upper triangular part
*          of the  Hermitian matrix  and the  strictly  lower triangular
*          part of  A  is not  referenced,  and when  UPLO = 'L' or 'l',
*          the leading  n by n  lower  triangular part  of the  array  A
*          must  contain  the  lower  triangular part  of the  Hermitian
*          matrix and the  strictly  upper triangular part of  A  is not
*          referenced.
*          Note that the imaginary parts of the local entries correspon-
*          ding to the diagonal  elements of A need not be set and assu-
*          med to be zero. Unchanged on exit.
*
*  LDA     (input)                       INTEGER
*          On entry,  LDA specifies the first dimension of A as declared
*          in the  calling (sub) program.  When  SIDE = 'L' or 'l'  then
*          LDA  must be at least  max( 1, m ), otherwise  LDA must be at
*          least  max( 1, n ). Unchanged on exit.
*
*  B       (input)                       COMPLEX*16 array
*          On entry, B is an array of DIMENSION (LDB, n).  Before entry,
*          the  leading  m by n  part of  the array  B  must contain the
*          matrix B. Unchanged on exit.
*
*  LDB     (input)                       INTEGER
*          On entry, LDB specifies  the first dimension of B as declared
*          in  the  calling  (sub)  program.   LDB  must  be   at  least
*          max( 1, m ). Unchanged on exit.
*
*  BETA    (input)                       COMPLEX*16
*          On entry,  BETA  specifies the scalar  beta.   When  BETA  is
*          supplied  as  zero  then  the  elements of the matrix C  need
*          not be set on input. Unchanged on exit.
*
*  C       (input/output)                COMPLEX*16 array
*          On entry, C is an array of DIMENSION (LDC, n).  Before entry,
*          the leading  m by n  part of the array C must contain the ma-
*          trix C, except when beta is zero, in which case C need not be
*          set on entry. On exit, the array C is overwritten by the m by
*          n updated matrix.
*
*  LDC     (input)                       INTEGER
*          On entry,  LDC specifies the first dimension of C as declared
*          in   the  calling  (sub)  program.   LDC  must  be  at  least
*          max( 1, m ). Unchanged on exit.
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
      INTEGER            ILEFT, IRIGHT
      PARAMETER          ( ILEFT = 141, IRIGHT = 142 )
      INTEGER            ILOWER, IUPPER
      PARAMETER          ( IUPPER = 121, ILOWER = 122 )
*     ..
*     .. Local Scalars ..
      INTEGER            INFO, ISIDE, IUPLO, NROWA
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATL_F77WRAP_ZHEMM, XERBLA
*     ..
*     .. External Functions ..
      EXTERNAL           LSAME
      LOGICAL            LSAME
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
      INFO = 0
*
      IF(      LSAME( SIDE  , 'L' ) ) THEN
         ISIDE = ILEFT
         NROWA = M
      ELSE IF( LSAME( SIDE  , 'R' ) ) THEN
         ISIDE = IRIGHT
         NROWA = N
      ELSE
         ISIDE = ILEFT
         NROWA = 0
         INFO  = 1
      END IF
*
      IF(      LSAME( UPLO  , 'U' ) ) THEN
         IUPLO = IUPPER
      ELSE IF( LSAME( UPLO  , 'L' ) ) THEN
         IUPLO = ILOWER
      ELSE IF( INFO.EQ.0 ) THEN
         IUPLO = IUPPER
         INFO  = 2
      END IF
*
      IF( INFO.EQ.0 ) THEN
         IF(      M  .LT.0               ) THEN
            INFO = 3
         ELSE IF( N  .LT.0               ) THEN
            INFO = 4
         ELSE IF( LDA.LT.MAX( 1, NROWA ) ) THEN
            INFO = 7
         ELSE IF( LDB.LT.MAX( 1, M     ) ) THEN
            INFO = 9
         ELSE IF( LDC.LT.MAX( 1, M     ) ) THEN
            INFO = 12
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'ZHEMM ', INFO )
         RETURN
      END IF
*
      CALL ATL_F77WRAP_ZHEMM( ISIDE, IUPLO, M, N, ALPHA, A, LDA,
     $                        B, LDB, BETA, C, LDC )
*
      RETURN
*
*     End of ZHEMM
*
      END
