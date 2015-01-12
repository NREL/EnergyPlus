      SUBROUTINE CGEMM( TRANSA, TRANSB, M, N, K, ALPHA, A, LDA, B, LDB,
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
      CHARACTER*1        TRANSA, TRANSB
      INTEGER            M, N, K, LDA, LDB, LDC
      COMPLEX            ALPHA, BETA
*     ..
*     .. Array Arguments ..
      COMPLEX            A( LDA, * ), B( LDB, * ), C( LDC, * )
*     ..
*
*  Purpose
*  =======
*
*  CGEMM  performs one of the matrix-matrix operations
*
*     C := alpha * op( A ) * op( B ) + beta * C,
*
*  where op( X ) is one of
*
*     op( X ) = X   or   op( X ) = X'   or   op( X ) = conjg( X' ).
*
*  Alpha and beta are scalars, and A, B and C are matrices, with op( A )
*  an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
*
*  Arguments
*  =========
*
*  TRANSA  (input)                       CHARACTER*1
*          On entry, TRANSA  specifies the form of op( A ) to be used in
*          the matrix multiplication as follows:
*
*             TRANSA = 'N' or 'n'  op( A ) = A,
*
*             TRANSA = 'T' or 't'  op( A ) = A',
*
*             TRANSA = 'C' or 'c'  op( A ) = conjg( A' ).
*
*          Unchanged on exit.
*
*  TRANSB  (input)                       CHARACTER*1
*          On entry, TRANSB  specifies the form of op( A ) to be used in
*          the matrix multiplication as follows:
*
*             TRANSB = 'N' or 'n'  op( B ) = B,
*
*             TRANSB = 'T' or 't'  op( B ) = B',
*
*             TRANSB = 'C' or 'c'  op( B ) = conjg( B' ).
*
*          Unchanged on exit.
*
*  M       (input)                       INTEGER
*          On entry,  M  specifies  the  number  of rows  of the  matrix
*          op( A )  and  of the  matrix  C.  M  must  be at least  zero.
*          Unchanged on exit.
*
*  N       (input)                       INTEGER
*          On entry,  N  specifies  the number  of columns of the matrix
*          op( B )  and the number of columns of the matrix C. N must be
*          at least zero. Unchanged on exit.
*
*  K       (input)                       INTEGER
*          On entry,  K  specifies  the  number of columns of the matrix
*          op( A ) and the number of rows  of the matrix op( B ). K must
*          be at least  zero. Unchanged on exit.
*
*  ALPHA   (input)                       COMPLEX
*          On entry, ALPHA specifies the scalar alpha.   When  ALPHA  is
*          supplied  as  zero  then the elements of the matrices A and B
*          need not be set on input. Unchanged on exit.
*
*  A       (input)                       COMPLEX array
*          On entry, A is an array of DIMENSION ( LDA, ka ), where ka is
*          k  when  TRANSA = 'N' or 'n',  and  is  m  otherwise.  Before
*          entry with  TRANSA = 'N' or 'n',  the leading  m by k part of
*          the array  A must contain the matrix A, otherwise the leading
*          k by m  part of the array  A  must contain the matrix  A. Un-
*          changed on exit.
*
*  LDA     (input)                       INTEGER
*          On entry, LDA  specifies the first dimension of A as declared
*          in the  calling (sub) program. When  TRANSA = 'N' or 'n' then
*          LDA  must be at least  max( 1, m ), otherwise  LDA must be at
*          least  max( 1, k ). Unchanged on exit.
*
*  B       (input)                       COMPLEX array
*          On entry, B is an array of DIMENSION ( LDB, kb ), where kb is
*          n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.   Before
*          entry with  TRANSB = 'N' or 'n',  the leading  k by n part of
*          the array  B must contain the matrix B, otherwise the leading
*          n by k part of the array B must contain the matrix B. Unchan-
*          ged on exit.
*
*  LDB     (input)                       INTEGER
*          On entry, LDB  specifies the first dimension of B as declared
*          in the  calling (sub) program. When  TRANSB = 'N' or 'n' then
*          LDB  must be at least  max( 1, k ), otherwise  LDB must be at
*          least  max( 1, n ). Unchanged on exit.
*
*  BETA    (input)                       COMPLEX
*          On entry,  BETA  specifies the scalar  beta.   When  BETA  is
*          supplied  as  zero  then  the  elements of the matrix C  need
*          not be set on input. Unchanged on exit.
*
*  C       (input/output)                COMPLEX array
*          On entry,  C  is  an  array of  DIMENSION ( LDC, n ).  Before
*          entry, the leading  m by n  part of the array  C must contain
*          the matrix C,  except when beta is zero, in which case C need
*          not be set on entry. On exit, the array  C  is overwritten by
*          the  m by n  matrix ( alpha*op( A )*op( B ) + beta*C ).
*
*  LDC     (input)                       INTEGER
*          On entry, LDC  specifies the first dimension of C as declared
*          in  the   calling  (sub)  program.   LDC  must  be  at  least
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
      INTEGER            ICOTRAN, INOTRAN, ITRAN
      PARAMETER          ( INOTRAN = 111, ITRAN = 112, ICOTRAN = 113 )
*     ..
*     .. Local Scalars ..
      INTEGER            INFO, ITRANSA, ITRANSB, NROWA, NROWB
*     ..
*     .. External Subroutines ..
      EXTERNAL           ATL_F77WRAP_CGEMM, XERBLA
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
      IF(      LSAME( TRANSA, 'N' ) ) THEN
         ITRANSA = INOTRAN
         NROWA   = M
      ELSE IF( LSAME( TRANSA, 'T' ) ) THEN
         ITRANSA = ITRAN
         NROWA   = K
      ELSE IF( LSAME( TRANSA, 'C' ) ) THEN
         ITRANSA = ICOTRAN
         NROWA   = K
      ELSE
         ITRANSA = INOTRAN
         NROWA   = 0
         INFO    = 1
      END IF
*
      IF(      LSAME( TRANSB, 'N' ) ) THEN
         ITRANSB = INOTRAN
         NROWB   = K
      ELSE IF( LSAME( TRANSB, 'T' ) ) THEN
         ITRANSB = ITRAN
         NROWB   = N
      ELSE IF( LSAME( TRANSB, 'C' ) ) THEN
         ITRANSB = ICOTRAN
         NROWB   = N
      ELSE IF( INFO.EQ.0 ) THEN
         ITRANSB = INOTRAN
         NROWB   = 0
         INFO    = 2
      END IF
*
      IF( INFO.EQ.0 ) THEN
         IF(      M  .LT.0               ) THEN
            INFO = 3
         ELSE IF( N  .LT.0               ) THEN
            INFO = 4
         ELSE IF( K  .LT.0               ) THEN
            INFO = 5
         ELSE IF( LDA.LT.MAX( 1, NROWA ) ) THEN
            INFO = 8
         ELSE IF( LDB.LT.MAX( 1, NROWB ) ) THEN
            INFO = 10
         ELSE IF( LDC.LT.MAX( 1, M     ) ) THEN
            INFO = 13
         END IF
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'CGEMM ', INFO )
         RETURN
      END IF
*
      CALL ATL_F77WRAP_CGEMM( ITRANSA, ITRANSB, M, N, K, ALPHA, A, LDA,
     $                        B, LDB, BETA, C, LDC )
*
      RETURN
*
*     End of CGEMM
*
      END
